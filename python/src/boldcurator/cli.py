"""Headless entry point.

Exists so the pipeline can be driven, tested and compared against the R app
without a GUI. The GUI (Phase 3) is a presentation layer over exactly these
calls.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from .build.fetch_snapshot import add_fetch_args
from .core.pipeline import SizeLimitExceeded, parse_lines, run_search
from .data.snapshot import SnapshotError, SnapshotStore


def _add_snapshot_arg(p: argparse.ArgumentParser) -> None:
    p.add_argument("--snapshot", required=True, type=Path, help="snapshot .duckdb file")


def cmd_info(args: argparse.Namespace) -> int:
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
        taxa_text = Path(args.taxa_file).read_text()

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


def cmd_fetch_snapshot(args: argparse.Namespace) -> int:
    from .build.fetch_snapshot import fetch

    return fetch(args)


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="boldcurator", description=__doc__)
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

    fetch_snapshot = sub.add_parser(
        "fetch-snapshot", help="download a pre-built snapshot (plan 5.1)")
    add_fetch_args(fetch_snapshot)
    fetch_snapshot.set_defaults(func=cmd_fetch_snapshot)

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
