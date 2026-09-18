"""Verify a built snapshot before anything is distributed or queried.

Run from a **fresh process**, opening the file **read-only**.  That is not a
detail: a leftover write-ahead log makes a DuckDB file unopenable read-only, and
the build process that just wrote the file would not notice, because it still
holds a read-write handle.

Exit status is non-zero if any check fails, so this can gate a publish job.
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from pathlib import Path

import duckdb

from ..data import schema as S


@dataclass
class Check:
    """One verification result.

    ``warn`` marks a result that is worth printing but must not fail the job --
    a deliberately partial or taxonomically scoped snapshot is legitimate, and
    treating "this taxon is absent" as an error would block exactly the trial
    builds the ``--limit`` flag exists to support.
    """

    name: str
    ok: bool
    detail: str = ""
    warn: bool = False

    @property
    def failed(self) -> bool:
        return not self.ok and not self.warn

    def line(self) -> str:
        mark = "WARN" if self.warn else ("PASS" if self.ok else "FAIL")
        return f"  [{mark}] {self.name}" + (f" -- {self.detail}" if self.detail else "")


def verify(snapshot: Path, *, previous_rows: int | None = None) -> list[Check]:
    checks: list[Check] = []

    wal = Path(str(snapshot) + ".wal")
    if wal.exists():
        return [Check("no write-ahead log beside the file", False,
                      f"{wal} exists; the file cannot be opened read-only")]

    try:
        con = duckdb.connect(str(snapshot), read_only=True)
    except Exception as exc:  # noqa: BLE001 - we want to report anything here
        return [Check("opens read-only in a fresh process", False, str(exc))]

    checks.append(Check("opens read-only in a fresh process", True))

    try:
        meta = dict(con.execute("SELECT key, value FROM _meta").fetchall())
        checks.append(Check("_meta readable", True,
                            f"snapshot_id={meta.get('snapshot_id')} "
                            f"schema_version={meta.get('schema_version')}"))

        partial = meta.get("partial_build") == "true"
        if partial:
            checks.append(Check(
                "partial build", False,
                f"built with --limit {meta.get('row_limit', '?')}; this is a "
                "trial artefact and must not be distributed or used for "
                "curation",
                warn=True,
            ))

        tables = {r[0] for r in con.execute("SHOW TABLES").fetchall()}
        missing = [t for t in S.ALL_TABLES if t not in tables]
        checks.append(Check("all tables present", not missing,
                            f"missing: {', '.join(missing)}" if missing else
                            ", ".join(sorted(tables))))

        n_rows = con.execute("SELECT count(*) FROM specimen").fetchone()[0]
        checks.append(Check("specimen is non-empty", n_rows > 0, f"{n_rows:,} rows"))

        if previous_rows:
            lo, hi = previous_rows * 0.98, previous_rows * 1.40
            checks.append(Check(
                "row count within -2% / +40% of the previous snapshot",
                lo <= n_rows <= hi,
                f"{n_rows:,} against {previous_rows:,}",
            ))

        cols = {r[0] for r in con.execute("DESCRIBE specimen").fetchall()}
        required_physical = {S.physical_name(c) for c in S.REQUIRED_SOURCE_COLUMNS}
        missing_cols = sorted(required_physical - cols)
        checks.append(Check("all required columns present", not missing_cols,
                            f"missing: {', '.join(missing_cols)}" if missing_cols
                            else f"{len(cols)} columns"))

        bad_pid = con.execute(
            "SELECT count(*) FROM specimen WHERE processid IS NULL OR processid = ''"
        ).fetchone()[0]
        checks.append(Check("no null/empty processid", bad_pid == 0, f"{bad_pid:,} bad"))

        dup = con.execute(
            "SELECT count(*) FROM (SELECT processid FROM specimen "
            "GROUP BY processid HAVING count(*) > 1)"
        ).fetchone()[0]
        checks.append(Check("no duplicate processid", dup == 0, f"{dup:,} duplicated"))

        orphan = con.execute(
            "SELECT count(*) FROM specimen_recordset r "
            "ANTI JOIN specimen s ON s.sid = r.sid"
        ).fetchone()[0]
        checks.append(Check("specimen_recordset has no orphans", orphan == 0,
                            f"{orphan:,} orphans"))

        # Joined on processid: the sequence table carries no sid, because
        # producing one required a 20 M-row join that recovered nothing.
        seq_orphan = con.execute(
            "SELECT count(*) FROM sequence q "
            "ANTI JOIN specimen s ON s.processid = q.processid"
        ).fetchone()[0]
        checks.append(Check("sequence has no orphans", seq_orphan == 0,
                            f"{seq_orphan:,} orphans"))

        n_seq = con.execute("SELECT count(*) FROM sequence").fetchone()[0]
        checks.append(Check("sequence rows <= specimen rows", n_seq <= n_rows,
                            f"{n_seq:,} of {n_rows:,}"))

        if meta.get("marker_filter"):
            off = con.execute(
                "SELECT count(*) FROM specimen WHERE marker_code IS DISTINCT FROM ?",
                [meta["marker_filter"]],
            ).fetchone()[0]
            checks.append(Check(f"every record is {meta['marker_filter']}", off == 0,
                                f"{off:,} other markers"))

        # Null fractions.  These exist to catch a column that parsed into the
        # wrong position -- which otherwise produces a perfectly healthy-looking
        # snapshot -- NOT to judge the data's completeness.
        #
        # Bounds are set from the real 2026-09-11 package (20,164,595 COI-5P
        # records): bin_uri 6.9%, species 67.3%, country_ocean 3.1%,
        # nuc_basecount 0.1%.  Species is high because most BOLD barcode records
        # are BIN-only or identified no finer than genus; an earlier 40% bound
        # was a guess and failed a perfectly good snapshot.
        for col, limit in (("bin_uri", 0.50), ("species", 0.85),
                           ("country_ocean", 0.40), ("nuc_basecount", 0.20)):
            if col not in cols:
                continue
            frac = con.execute(
                f"SELECT count(*) FILTER (WHERE {S.quote_ident(col)} IS NULL "
                f"OR CAST({S.quote_ident(col)} AS VARCHAR) = '') / count(*) FROM specimen"
            ).fetchone()[0]
            checks.append(Check(f"{col} null fraction under {limit:.0%}",
                                frac is not None and frac < limit,
                                f"{frac:.1%}" if frac is not None else "n/a"))

        # Taxon resolution.  What this is actually for is catching a rank
        # column that parsed into the wrong position -- so a name resolving to
        # the WRONG rank is a failure, while a name simply being absent is not.
        # A --limit build, or a taxonomically scoped one, legitimately lacks
        # these.
        for name, expected in (("lepidoptera", "order"),
                               ("nymphalidae", "family"),
                               ("danaus plexippus", "species")):
            rows = con.execute(
                "SELECT taxon_rank, n_records FROM taxon WHERE taxon_lc = ?", [name]
            ).fetchall()
            found = {r[0] for r in rows}
            if not rows:
                note = ("absent -- expected in a full snapshot, normal for a "
                        "partial or scoped build")
                checks.append(Check(f"resolve({name!r})", False, note, warn=True))
            else:
                checks.append(Check(f"resolve({name!r}) -> {expected}",
                                    expected in found,
                                    f"resolved to {', '.join(sorted(found))}"))

        bins = con.execute("SELECT count(DISTINCT bin_uri) FROM bin_species").fetchone()[0]
        checks.append(Check("bin_species is populated", bins > 0, f"{bins:,} BINs"))

    finally:
        con.close()

    return checks


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(
        prog="boldcurator-verify-snapshot",
        description="Verify a snapshot read-only from a fresh process.",
    )
    p.add_argument("--snapshot", required=True, type=Path)
    p.add_argument("--previous-rows", type=int, default=None,
                   help="row count of the snapshot this replaces, to bound the delta")
    args = p.parse_args(argv)

    print(f"Verifying {args.snapshot}")
    checks = verify(args.snapshot, previous_rows=args.previous_rows)
    for c in checks:
        print(c.line())

    failed = [c for c in checks if c.failed]
    warned = [c for c in checks if c.warn]
    print()
    if failed:
        print(f"{len(failed)} of {len(checks)} checks FAILED -- do not publish this file")
        return 1
    if warned:
        print(f"{len(checks) - len(warned)} checks passed, {len(warned)} warning(s)")
        return 0
    print(f"All {len(checks)} checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
