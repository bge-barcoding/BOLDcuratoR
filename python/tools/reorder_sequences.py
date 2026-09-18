#!/usr/bin/env python3
"""Rewrite an existing snapshot's ``sequence`` table in ``specimen`` order.

**Why.** A sequence fetch is always driven by a taxonomic result, so the rows
it wants sit together in the ``specimen`` order and nowhere near each other in
ingest order. A snapshot built before this stores sequences in ingest order, so
every fetch scans the whole ``nuc`` column -- 2.6 s and 4.5 GB of RSS to return
183 sequences. Reordered, and paired with the two-phase fetch in
``iter_sequences``, the same fetch is 0.21 s and 271 MB.

**This does not need the source TSV.** Everything required is already in the
snapshot, which matters because the 20 GB staging file was deleted. A full
re-ingest is about 24 minutes; this is a couple of minutes for a metadata-only
snapshot and well under ten for one carrying 5 GB of sequences.

It writes a **new file** and never touches the input, because DuckDB does not
reclaim space on DROP -- rewriting in place would leave the old copy's bytes in
the file forever.

    python tools/reorder_sequences.py \\
        --snapshot bold_snapshot_2026-09-11.duckdb \\
        --out bold_snapshot_2026-09-11.reordered.duckdb

Verify the result before replacing the original::

    python tools/verify_snapshot.py --snapshot <the new file>
"""

from __future__ import annotations

import argparse
import os
import shutil
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

import duckdb  # noqa: E402

from boldcurator.data import schema as S  # noqa: E402

COPIED_TABLES = ("specimen", "specimen_recordset", "taxon", "bin_species", "_meta")


def _log(message: str) -> None:
    print(message, flush=True)


def reorder(snapshot: Path, out: Path, *, memory_limit: str = "6GB",
            threads: int = 4, temp_dir: Path | None = None) -> None:
    if not snapshot.exists():
        raise SystemExit(f"No snapshot at {snapshot}")
    if out.exists():
        raise SystemExit(f"{out} already exists; refusing to overwrite it")
    for stray in (Path(str(out) + ".wal"), Path(str(snapshot) + ".wal")):
        if stray.exists():
            raise SystemExit(f"A write-ahead log exists beside {stray}")

    # Unique per run. A shared spill directory is silently destructive: two
    # reorders running at once delete each other's temp files and both die
    # part-way through, having already written most of a new snapshot.
    temp_dir = (temp_dir or out.parent) / f".duckdb_tmp_{os.getpid()}"
    temp_dir.mkdir(parents=True, exist_ok=True)

    started = time.time()
    con = duckdb.connect(str(out))
    try:
        con.execute(f"SET memory_limit = '{memory_limit}'")
        con.execute(f"SET threads = {int(threads)}")
        con.execute(f"SET temp_directory = '{temp_dir}'")
        con.execute(f"ATTACH '{snapshot}' AS src (READ_ONLY)")
        # With more than one database attached, an unqualified table name is
        # ambiguous and resolves somewhere surprising. Qualify everything --
        # and quote it: the catalog is named after the output file, so
        # `--out full.duckdb` gives a catalog called `full`, which is a
        # reserved word.
        me = S.quote_ident(con.execute("SELECT current_database()").fetchone()[0])

        meta = dict(con.execute("SELECT key, value FROM src._meta").fetchall())
        if meta.get("sequences_included") != "true":
            raise SystemExit(
                "This snapshot was built without sequences, so there is "
                "nothing to reorder."
            )
        if meta.get("sequence_order") == "specimen":
            raise SystemExit(
                "This snapshot's sequences are already in specimen order."
            )

        for table in COPIED_TABLES:
            step = time.time()
            con.execute(f"CREATE TABLE {me}.{S.quote_ident(table)} AS "
                        f"SELECT * FROM src.{S.quote_ident(table)}")
            _log(f"  copied {table} ({time.time() - step:.1f}s)")

        # Integers only: the build side is specimen's (processid -> rowid) and
        # the probe reads sequence's processid and rowid. `nuc` is never
        # touched here, which is what keeps this pass cheap and flat.
        step = time.time()
        con.execute(f"""
            CREATE TABLE {me}.seq_order AS
            SELECT q.rowid AS seq_rid, s.rowid AS spec_rid
            FROM src.sequence q JOIN src.specimen s ON s.processid = q.processid
        """)
        mapped = con.execute(f"SELECT count(*) FROM {me}.seq_order").fetchone()[0]
        total = con.execute("SELECT count(*) FROM src.sequence").fetchone()[0]
        _log(f"  mapped {mapped:,} of {total:,} sequences to a specimen "
             f"({time.time() - step:.1f}s)")
        if mapped != total:
            raise SystemExit(
                f"{total - mapped:,} sequences have no matching specimen. That "
                "is an orphaned sequence table, not something to reorder -- "
                "run tools/verify_snapshot.py on the input first."
            )

        # The payload move. One sort, with nothing else in flight; this is the
        # step that has to spill, and memory_limit and temp_directory are set
        # above so that it can.
        step = time.time()
        con.execute(f"""
            CREATE TABLE {me}.sequence AS
            SELECT q.processid, q.nuc
            FROM {me}.seq_order m JOIN src.sequence q ON q.rowid = m.seq_rid
            ORDER BY m.spec_rid
        """)
        _log(f"  reordered {mapped:,} sequences ({time.time() - step:.1f}s)")
        con.execute(f"DROP TABLE {me}.seq_order")

        con.execute(f"DELETE FROM {me}._meta WHERE key = 'sequence_order'")
        con.execute(f"INSERT INTO {me}._meta VALUES ('sequence_order', 'specimen')")
        con.execute("CHECKPOINT")

        # The point of the whole exercise: sequence row k belongs to the
        # specimen at or before row k, so a taxonomic result's sequences are
        # contiguous. Checked as a strict ordering, which is what zone maps
        # need, rather than as an equality that the dropped no-sequence rows
        # would break.
        out_of_order = con.execute(f"""
            SELECT count(*) FROM (
                SELECT s.rowid AS spec_rid,
                       lag(s.rowid) OVER (ORDER BY q.rowid) AS previous
                FROM {me}.sequence q JOIN {me}.specimen s
                  ON s.processid = q.processid
            ) WHERE previous IS NOT NULL AND spec_rid <= previous
        """).fetchone()[0]
        if out_of_order:
            raise SystemExit(
                f"{out_of_order:,} sequences are still out of specimen order. "
                "The new file is wrong; do not use it."
            )
        _log("  verified: sequences follow specimen order exactly")
    finally:
        con.close()
        shutil.rmtree(temp_dir, ignore_errors=True)

    _log(f"done in {time.time() - started:.1f}s -- {out} "
         f"({os.path.getsize(out) / 1e9:.2f} GB)")
    _log("Verify it before replacing the original:")
    _log(f"  python tools/verify_snapshot.py --snapshot {out}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--snapshot", required=True, type=Path,
                        help="the snapshot to read (never modified)")
    parser.add_argument("--out", required=True, type=Path,
                        help="the new snapshot to write")
    parser.add_argument("--memory-limit", default="6GB",
                        help="DuckDB memory_limit (default 6GB)")
    parser.add_argument("--threads", type=int, default=4)
    parser.add_argument("--temp-dir", type=Path, default=None,
                        help="parent directory for DuckDB's spill files "
                             "(default: beside --out). A per-process "
                             "subdirectory is created inside it and removed "
                             "afterwards.")
    args = parser.parse_args(argv)
    reorder(args.snapshot, args.out, memory_limit=args.memory_limit,
            threads=args.threads, temp_dir=args.temp_dir)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
