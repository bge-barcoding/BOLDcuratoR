"""Headless entry point.

Exists so the pipeline can be driven, tested and compared against the R app
without a GUI. The GUI (Phase 3) is a presentation layer over exactly these
calls.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from . import __version__
from .build.fetch_snapshot import add_fetch_args
from .core.pipeline import SizeLimitExceeded, parse_lines, run_search
from .data.snapshot import SnapshotError, SnapshotStore
from .desktop import WINDOW_MODES


def _add_snapshot_arg(p: argparse.ArgumentParser) -> None:
    p.add_argument("--snapshot", required=True, type=Path, help="snapshot .duckdb file")


def cmd_info(args: argparse.Namespace) -> int:
    print(f"boldcurator {__version__}")
    with SnapshotStore(args.snapshot) as store:
        info = store.info()
        print(info.describe())
        print(f"  path             {info.path}")
        print(f"  schema version   {info.schema_version}")
        print(f"  taxa indexed     {info.taxon_count:,}")
        print(f"  sequences        {info.sequence_count:,}")
        print(f"  recordsets       {'yes' if store.has_recordsets else 'no'}")
    return 0


def cmd_resolve(args: argparse.Namespace) -> int:
    from .data.queries import resolve_taxa

    with SnapshotStore(args.snapshot) as store:
        res = resolve_taxa(store, args.name)
        for t in res.resolved:
            print(f"{t.name:<40} {t.rank:<12} {t.n_records:>12,}")
        for name in res.unmatched:
            print(f"{name:<40} {'not found':<12}", file=sys.stderr)
        for name, options in res.ambiguous.items():
            ranks = ", ".join(o.rank for o in options)
            print(f"note: {name!r} is ambiguous ({ranks})", file=sys.stderr)
    return 1 if res.unmatched else 0


def cmd_search(args: argparse.Namespace) -> int:
    taxa_text = args.taxa or ""
    if args.taxa_file:
        taxa_text = Path(args.taxa_file).read_text(encoding="utf-8")

    with SnapshotStore(args.snapshot) as store:
        try:
            result = run_search(
                store,
                taxa_text=taxa_text,
                countries=parse_lines("\n".join(args.country or [])),
                continents=args.continent or [],
                dataset_codes=args.dataset or [],
                project_codes=args.project or [],
                expand_bins=not args.no_bin_expansion,
                limit=args.limit,
            )
        except SizeLimitExceeded as exc:
            print(f"error: {exc}", file=sys.stderr)
            return 2

    for warning in result.warnings:
        print(f"warning: {warning}", file=sys.stderr)

    summary = result.summary()
    print(f"{summary['records']:,} records | {summary['species']} species | "
          f"{summary['bins']} BINs | {summary['countries']} countries | "
          f"snapshot {summary['snapshot_id']}")
    print(f"seed {result.estimate['seed_records']:,} records "
          f"-> BIN expansion -> {result.estimate['expanded_records']:,}")

    if len(result.bags_grades):
        counts = result.bags_grades["bags_grade"].value_counts().sort_index()
        print("BAGS: " + "  ".join(f"{g}={n}" for g, n in counts.items()))
    print("BINs: " + "  ".join(f"{k}={v}" for k, v in
                               result.bin_analysis["summary"].items()))

    if args.out:
        from .io.exports import export_all

        # Reopen the snapshot for the sequence stream the FASTA exports need.
        with SnapshotStore(args.snapshot) as store:
            written = export_all(result, Path(args.out), store=store)
        print()
        print(written.describe())
    return 0


# --------------------------------------------------------------------------
# benchmark
# --------------------------------------------------------------------------


def _peak_rss_mb() -> float | None:
    """Peak resident memory, or None when psutil is not installed.

    Deliberately optional: the benchmark must run on a bare install. `pip
    install -e ".[bench]"` adds it.
    """
    try:
        import psutil
    except ImportError:
        return None
    return psutil.Process().memory_info().rss / 1e6


class _Timer:
    """Records a named step's wall clock, row count and memory."""

    def __init__(self) -> None:
        self.rows: list[dict] = []

    def run(self, name: str, fn, *, detail: str = ""):
        import time

        start = time.monotonic()
        error = ""
        value = None
        try:
            value = fn()
        except Exception as exc:  # noqa: BLE001 - a refusal is a result here
            error = f"{type(exc).__name__}: {exc}"
        elapsed = time.monotonic() - start
        self.rows.append({
            "step": name, "seconds": elapsed, "detail": detail or "",
            "error": error, "rss_mb": _peak_rss_mb(),
        })
        return value

    def annotate(self, detail: str) -> None:
        if self.rows:
            self.rows[-1]["detail"] = detail

    def table(self) -> str:
        width = max(len(r["step"]) for r in self.rows) + 2
        lines = [f"{'step'.ljust(width)}{'seconds':>10}  {'RSS MB':>8}  detail"]
        lines.append("-" * (width + 32))
        for r in self.rows:
            rss = f"{r['rss_mb']:.0f}" if r["rss_mb"] is not None else "-"
            detail = r["error"] or r["detail"]
            lines.append(f"{r['step'].ljust(width)}{r['seconds']:>10.3f}  "
                         f"{rss:>8}  {detail}")
        return "\n".join(lines)


def cmd_benchmark(args: argparse.Namespace) -> int:
    """Measure the real thing.

    Every claim this project rests on -- taxon resolve under a second, BIN
    expansion collapsing to one sub-second query, scoring in seconds rather
    than the R loop's minutes -- is a claim about a 20 M-record snapshot, and
    is worth nothing until measured against one.

    A size-limit refusal is reported as a result rather than raised: whether
    DOWNLOAD_LIMITS is set sensibly for real data is one of the things being
    measured.
    """
    import tempfile

    from .core.pipeline import run_search
    from .data.queries import (
        SearchQuery,
        estimate_search,
        fetch_planned,
        iter_sequences,
        plan_search,
        resolve_taxa,
    )

    if _peak_rss_mb() is None:
        print("note: psutil not installed, so memory is not reported. "
              'Install it with: pip install -e ".[bench]"\n')

    t = _Timer()
    store = t.run("open snapshot", lambda: SnapshotStore(args.snapshot))
    if store is None:
        print(t.table())
        return 1
    # The whole description, not just its first line: the lines after it are
    # the warnings (a partial build, sequences in ingest order), and those are
    # exactly what explains a bad number further down the table.
    t.annotate(" ".join(part.strip() for part in store.info().describe().splitlines()))

    resolved_names: list[str] = []
    try:
        for name in args.taxon:
            res = t.run(f"resolve {name!r}", lambda n=name: resolve_taxa(store, [n]))
            if res is None:
                continue
            if res.resolved:
                resolved_names.append(name)
                t.annotate("; ".join(f"{r.rank} {r.n_records:,} records"
                                     for r in res.resolved))
            else:
                t.annotate("not found in this snapshot")
                continue

            query = SearchQuery(taxa=res.resolved, expand_bins=True)
            est = t.run(f"estimate {name!r}", lambda q=query: estimate_search(store, q))
            if est:
                t.annotate(f"seed {est['seed_records']:,} rows / "
                           f"{est['seed_bins']:,} BINs -> "
                           f"{est['expanded_records']:,} after BIN expansion")

            # Timed as the two halves it actually is. The narrow pass resolves
            # which rows match; the wide one projects them. Reporting a single
            # "search" number hid that essentially all of it was the second
            # half, projecting 70 columns of 20 M rows to return 183.
            plan = t.run(f"plan {name!r}", lambda q=query: plan_search(store, q))
            if plan is not None:
                t.annotate(f"{plan.expanded_records:,} rows resolved")

            if plan is not None and plan.expanded_records > args.max_fetch:
                t.run(f"fetch {name!r}", lambda: None)
                t.annotate(f"skipped: {plan.expanded_records:,} rows is over "
                           f"--max-fetch {args.max_fetch:,}; a frame that size "
                           "should not be materialised at all")
            else:
                frame = t.run(f"fetch {name!r}",
                              lambda p=plan: fetch_planned(store, p))
                if frame is not None:
                    t.annotate(f"{len(frame):,} rows x {len(frame.columns)} columns")

        # The pipeline runs on a taxon that actually resolved, so one bad name
        # on the command line does not lose the measurement entirely.
        pipeline_taxon = args.pipeline_taxon or (
            resolved_names[0] if resolved_names else None
        )
        if pipeline_taxon is None:
            print(t.table())
            print("\nNone of the requested taxa are in this snapshot, so the "
                  "pipeline could not be measured. Check the spelling, or pick "
                  "taxa this snapshot covers.")
            return 1
        result = t.run(
            f"full pipeline {pipeline_taxon!r}",
            lambda: run_search(store, taxa_text=pipeline_taxon,
                               enforce_limits=not args.no_limits),
        )
        if result is not None:
            s = result.summary()
            t.annotate(f"{s['records']:,} records, {s['species']} species, "
                       f"{s['bins']} BINs, {s['selected']} auto-selected")

            # The post-search stages, timed again on their own. They are
            # trivial on a small result and are the whole cost on a large one,
            # and a single pipeline number cannot show which.
            from .core import bags as _bags, bins as _bins, selection as _sel
            from .core.scoring import criterion_flags as _flags

            specimens = result.specimens
            flags = t.run("  score (criterion flags)", lambda: _flags(specimens))
            t.annotate(f"{len(specimens):,} rows x "
                       f"{0 if flags is None else len(flags.columns)} criteria")
            t.run("  BAGS grades", lambda: _bags.calculate_bags_grades(specimens))
            t.annotate(f"{len(result.bags_grades):,} species graded")
            t.run("  BIN analysis", lambda: _bins.analyse_bins(specimens))
            t.annotate(f"{result.bin_analysis['summary']['total_bins']:,} BINs")
            t.run("  auto-selection",
                  lambda: _sel.auto_select_best_specimens(specimens))
            t.annotate(f"{len(result.selections):,} chosen")

            ids = [str(p) for p in result.specimens["processid"][: args.sequences]]
            n = t.run(f"stream {len(ids):,} sequences",
                      lambda: sum(1 for _ in iter_sequences(store, ids)))
            if n is not None:
                t.annotate(f"{n:,} sequences")

            if args.export:
                from .io.exports import export_all

                with tempfile.TemporaryDirectory() as tmp:
                    written = t.run("export all formats",
                                    lambda: export_all(result, tmp, store=store))
                    if written is not None:
                        t.annotate(f"{len(written.written)} files, "
                                   f"{len(written.skipped)} skipped")
    finally:
        store.close()

    print(t.table())
    print()
    failed = [r for r in t.rows if r["error"]]
    limit_refusals = [r for r in failed if "SizeLimitExceeded" in r["error"]]
    if limit_refusals:
        print("Size limits refused a query. That is the guard working, but "
              "check DOWNLOAD_LIMITS is set sensibly for real data; "
              "--no-limits pushes past it.")
    other = [r for r in failed if r not in limit_refusals]
    if other:
        print(f"{len(other)} step(s) failed unexpectedly.")
        return 1
    return 0


def cmd_gui(args: argparse.Namespace) -> int:
    """Launch the Shiny UI.

    Imported here, not at module scope, so the CLI keeps working on an install
    without the ``gui`` extra -- which is most installs, since the CLI and the
    parity harness never need it.
    """
    try:
        from .ui import run
    except ImportError as exc:
        print(f"The GUI needs the optional dependencies: pip install -e \".[gui]\"\n"
              f"  ({exc})")
        return 1
    run(args.snapshot, host=args.host, port=args.port, page_size=args.page_size,
        sessions_path=args.sessions)
    return 0


def cmd_verify(args: argparse.Namespace) -> int:
    from .build.verify import main as verify_main

    return verify_main(["--snapshot", str(args.snapshot)])


def cmd_selftest(args: argparse.Namespace) -> int:
    """Exercise the modules most likely to work unfrozen and break only once
    packaged (PyInstaller) -- no snapshot, no network, no browser needed.

    Added after ``core.phylogeny`` broke on a real Windows build:
    ``import Bio.Phylo.TreeConstruction`` (needed for
    ``DistanceMatrix``/``DistanceTreeConstructor``) works fine unfrozen but
    transitively imports ``Bio.Align``, whose ``DistanceCalculator`` class
    body runs ``substitution_matrices.load()`` -- an ``os.listdir()`` on a
    data directory PyInstaller's import analysis doesn't bundle unless told
    to (``--collect-all Bio``, see ``packaging/README.md``). This
    check builds a real tiny tree, not just an import, so it fails the same
    way a curator's "Build tree" click did rather than passing on the
    import alone and missing the actual failure point.

    Run this against a frozen build directly (``boldcurator.exe selftest``)
    to check a packaging fix without going through Search -> Phylogeny ->
    Build tree in the GUI first.

    ``--network`` adds the one check that needs the internet: resolving the
    default snapshot's Zenodo record over HTTPS, without downloading it.
    It's how a curator whose "Download from Zenodo" fails can tell a
    certificate problem (the V3.3 macOS bug -- see
    ``fetch_snapshot.ssl_context``) from a network that blocks Zenodo.
    """
    print(f"boldcurator {__version__} selftest")

    def check_duckdb() -> None:
        import duckdb

        row = duckdb.connect(":memory:").execute("select 1").fetchone()
        assert row == (1,)

    def check_phylogeny() -> None:
        import random

        from .core import phylogeny as phylo
        from .core import refalign

        # Realistic enough to take every real code path: the aligner (and
        # its NUC.4.4 matrix load), an overhang, a short fragment, K2P.
        rng = random.Random(0)
        core = "".join(rng.choice("ACGT") for _ in range(200))
        mutated = "".join(rng.choice("ACGT") if rng.random() < 0.1 else b
                          for b in core)
        sequences = {"a": core, "b": "TTGACCA" + core[:190], "c": mutated[20:]}
        _, anchored = refalign.anchor_all(
            sequences, target_length=200, min_identity=0.6, min_coverage=0.5)
        names = [a.name for a in anchored]
        distances, _ = phylo.k2p_distance_matrix(
            [a.row for a in anchored], min_shared_sites=100)
        assert distances[0, 1] < distances[0, 2], "aligner gave implausible distances"
        tree = phylo.build_tree(names, distances)
        newick = phylo.to_newick(tree)
        assert newick.endswith(";"), f"unexpected Newick output: {newick!r}"

    def check_trust_store() -> None:
        import truststore

        from .build import fetch_snapshot as fs

        # Importing truststore on macOS/Windows already loads the OS's own
        # certificate API (Security.framework / crypt32) through ctypes,
        # so this fails here, not at a curator's first download.
        ctx = fs.ssl_context()
        assert isinstance(ctx, truststore.SSLContext), type(ctx)

    def check_zenodo() -> str:
        from .build import fetch_snapshot as fs
        from .config.constants import DEFAULT_SNAPSHOT_ZENODO_DOI

        source = fs.resolve_zenodo_record(DEFAULT_SNAPSHOT_ZENODO_DOI)
        # The User-Agent too: what Zenodo sees, if it is the one refusing.
        return f"{source.filename or source.url} (as {fs.USER_AGENT})"

    checks = [
        ("duckdb", check_duckdb),
        ("biopython (Phylogeny tab tree building)", check_phylogeny),
        ("HTTPS certificates (the operating system's own)", check_trust_store),
    ]
    if getattr(args, "network", False):
        checks.append(("Zenodo over HTTPS (--network)", check_zenodo))

    failed = False
    for name, check in checks:
        try:
            detail = check()
            print(f"  [ok]   {name}" + (f": {detail}" if detail else ""))
        except Exception as exc:  # noqa: BLE001 -- reported, not swallowed
            failed = True
            print(f"  [FAIL] {name}: {exc}")

    print("selftest FAILED" if failed else "selftest passed")
    return 1 if failed else 0


def cmd_fetch_snapshot(args: argparse.Namespace) -> int:
    from .build.fetch_snapshot import fetch

    return fetch(args)


def cmd_desktop(args: argparse.Namespace) -> int:
    """Launch the packaged app in a window, not a plain browser tab.

    Imported here, not at module scope, for the same reason ``cmd_gui`` is --
    it needs the ``desktop`` extra (Shiny, uvicorn), which most installs
    (the CLI, the parity harness) never need. ``pywebview`` specifically is
    *not* checked for here even though it is part of the same extra: modes
    other than ``native`` don't need it, and its real history of fragility
    once frozen on Windows (see ``packaging/README.md``) is exactly why
    ``--window`` exists.
    """
    try:
        import shiny  # noqa: F401  -- proves the desktop extra is installed
        import uvicorn  # noqa: F401
        from .desktop import launch
    except ImportError as exc:
        print(f"The desktop app needs the optional dependencies: "
              f"pip install -e \".[desktop]\"\n  ({exc})")
        return 1
    launch(args.snapshot, page_size=args.page_size, window=args.window)
    return 0


_FROZEN_SHORTCUT_NOTE = (
    "This is the installer build of BOLDcurator; its installer already "
    "manages its shortcuts. install-shortcut is for a pip/uv install.")


def cmd_install_shortcut(args: argparse.Namespace) -> int:
    """Start-menu / Applications / app-menu shortcut for a pip/uv install
    (:mod:`.shortcuts`). A no-op in a frozen build, whose installer made
    its own -- a second, differently named set would only confuse."""
    if getattr(sys, "frozen", False):
        print(_FROZEN_SHORTCUT_NOTE)
        return 0
    from importlib.util import find_spec

    from .shortcuts import SHORTCUT_NAME, ShortcutError, install

    # The launcher exists without the desktop extra too; a shortcut to it
    # would then only ever write cmd_desktop's "needs the optional
    # dependencies" message to a log nobody sees.
    missing = [m for m in ("shiny", "uvicorn") if find_spec(m) is None]
    if missing:
        print(f"error: the desktop app's dependencies are not installed "
              f"({', '.join(missing)}). Reinstall with the desktop extra: "
              f"uv tool install \"boldcurator[desktop]\"", file=sys.stderr)
        return 1
    try:
        written = install(desktop=not args.no_desktop)
    except ShortcutError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    print(f"Created the \"{SHORTCUT_NAME}\" shortcut:")
    for path in written:
        print(f"  {path}")
    return 0


def cmd_remove_shortcut(args: argparse.Namespace) -> int:
    if getattr(sys, "frozen", False):
        print(_FROZEN_SHORTCUT_NOTE)
        return 0
    from .shortcuts import ShortcutError, remove

    try:
        removed = remove()
    except ShortcutError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    if not removed:
        print("No shortcuts to remove.")
    for path in removed:
        print(f"Removed {path}")
    return 0


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="boldcurator", description=__doc__)
    p.add_argument("--version", "-V", action="version",
                   version=f"%(prog)s {__version__}")
    sub = p.add_subparsers(dest="command", required=True)

    info = sub.add_parser("info", help="describe a snapshot")
    _add_snapshot_arg(info)
    info.set_defaults(func=cmd_info)

    gui = sub.add_parser("gui", help="launch the Shiny UI")
    _add_snapshot_arg(gui)
    gui.add_argument("--host", default="127.0.0.1")
    gui.add_argument("--port", type=int, default=8000)
    gui.add_argument("--page-size", type=int, default=100)
    gui.add_argument("--sessions", default=None,
                     help="path to the saved-sessions SQLite file "
                          "(default: ~/.boldcurator/sessions.sqlite)")
    gui.set_defaults(func=cmd_gui)

    verify = sub.add_parser("verify", help="verify a snapshot read-only")
    _add_snapshot_arg(verify)
    verify.set_defaults(func=cmd_verify)

    selftest = sub.add_parser(
        "selftest",
        help="exercise modules that break only once frozen -- no snapshot, "
             "no network, no browser")
    selftest.add_argument(
        "--network", action="store_true",
        help="also reach the default snapshot's Zenodo record over HTTPS "
             "(downloads nothing) -- tells a certificate problem apart from "
             "a network that blocks Zenodo")
    selftest.set_defaults(func=cmd_selftest)

    fetch_snapshot = sub.add_parser(
        "fetch-snapshot", help="download a pre-built snapshot (plan 5.1)")
    add_fetch_args(fetch_snapshot)
    fetch_snapshot.set_defaults(func=cmd_fetch_snapshot)

    desktop = sub.add_parser(
        "desktop", help="launch the packaged app in a window, not a browser tab")
    desktop.add_argument("--snapshot", type=Path, default=None,
                         help="snapshot .duckdb file; omit to use the saved "
                              "one, or run the first-run setup screen if "
                              "none is saved yet")
    desktop.add_argument("--page-size", type=int, default=100)
    desktop.add_argument(
        "--window", choices=list(WINDOW_MODES), default="auto",
        help="how to show the app: native (pywebview), browser-app (a "
             "Chromium browser in --app mode -- no tabs/address bar, but "
             "launched as a plain subprocess, so it never touches "
             "pywebview's fragile Windows backend), tab (a plain browser "
             "tab), or auto (try native, then browser-app, then tab, "
             "falling back silently -- the default)")
    desktop.set_defaults(func=cmd_desktop)

    install_shortcut = sub.add_parser(
        "install-shortcut",
        help="add a clickable Start menu / Applications / app menu shortcut "
             "(pip/uv installs; the installers make their own)")
    install_shortcut.add_argument("--no-desktop", action="store_true",
                                  help="skip the Desktop copy")
    install_shortcut.set_defaults(func=cmd_install_shortcut)

    remove_shortcut = sub.add_parser(
        "remove-shortcut", help="remove the shortcuts install-shortcut made")
    remove_shortcut.set_defaults(func=cmd_remove_shortcut)

    resolve = sub.add_parser("resolve", help="resolve taxon names to ranks")
    _add_snapshot_arg(resolve)
    resolve.add_argument("name", nargs="+")
    resolve.set_defaults(func=cmd_resolve)

    search = sub.add_parser("search", help="search, score, rank and grade")
    _add_snapshot_arg(search)
    search.add_argument("--taxa", help="taxa, newline separated, synonyms after commas")
    search.add_argument("--taxa-file", help="file of taxa, one line per group")
    search.add_argument("--country", action="append", help="repeatable")
    search.add_argument("--continent", action="append", help="repeatable")
    search.add_argument("--dataset", action="append", help="BOLD dataset code")
    search.add_argument("--project", action="append", help="BOLD project code")
    search.add_argument("--no-bin-expansion", action="store_true")
    search.add_argument("--limit", type=int)
    search.add_argument("--out", help="directory to write the export set into")
    search.set_defaults(func=cmd_search)

    bench = sub.add_parser(
        "benchmark",
        help="measure resolve, BIN expansion, scoring and export against a "
             "real snapshot",
    )
    _add_snapshot_arg(bench)
    bench.add_argument("--taxon", action="append", default=None,
                       help="taxon to exercise, repeatable. Defaults to a "
                            "species, a family and an order.")
    bench.add_argument("--pipeline-taxon", default=None,
                       help="taxon for the full scoring pipeline "
                            "(default: the first --taxon)")
    bench.add_argument("--sequences", type=int, default=10_000,
                       help="how many sequences to stream (default 10000)")
    bench.add_argument("--max-fetch", type=int, default=250_000,
                       help="skip the wide fetch above this many rows "
                            "(default 250000). The raw fetch has no size "
                            "guard of its own, and 2.1 M rows x 70 columns "
                            "into pandas is not a measurement, it is an OOM.")
    bench.add_argument("--export", action="store_true",
                       help="also time writing every export format")
    bench.add_argument("--no-limits", action="store_true",
                       help="ignore DOWNLOAD_LIMITS, to measure past the guard")
    bench.set_defaults(func=cmd_benchmark)

    return p


#: A species, a family and an order -- three orders of magnitude apart, which
#: is what makes the timings meaningful.
DEFAULT_BENCHMARK_TAXA = ["Danaus plexippus", "Nymphalidae", "Lepidoptera"]


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    if getattr(args, "taxon", None) is None and args.command == "benchmark":
        args.taxon = list(DEFAULT_BENCHMARK_TAXA)
    try:
        return args.func(args)
    except (SnapshotError, ValueError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
