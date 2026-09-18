"""Headless entry point.

Exists so the pipeline can be driven, tested and compared against the R app
without a GUI. The GUI (Phase 3) is a presentation layer over exactly these
calls.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

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
        out = Path(args.out)
        out.mkdir(parents=True, exist_ok=True)
        result.specimens.to_csv(out / "specimens.tsv", sep="\t", index=False)
        result.bags_grades.to_csv(out / "bags_grades.tsv", sep="\t", index=False)
        result.bin_analysis["content"].to_csv(
            out / "bin_content.tsv", sep="\t", index=False
        )
        print(f"wrote {out}/specimens.tsv, bags_grades.tsv, bin_content.tsv")
    return 0


def cmd_verify(args: argparse.Namespace) -> int:
    from .build.verify import main as verify_main

    return verify_main(["--snapshot", str(args.snapshot)])


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="boldcurator", description=__doc__)
    sub = p.add_subparsers(dest="command", required=True)

    info = sub.add_parser("info", help="describe a snapshot")
    _add_snapshot_arg(info)
    info.set_defaults(func=cmd_info)

    verify = sub.add_parser("verify", help="verify a snapshot read-only")
    _add_snapshot_arg(verify)
    verify.set_defaults(func=cmd_verify)

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
    search.add_argument("--out", help="directory to write result tables into")
    search.set_defaults(func=cmd_search)

    return p


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        return args.func(args)
    except (SnapshotError, ValueError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
