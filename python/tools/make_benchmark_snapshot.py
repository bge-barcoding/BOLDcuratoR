#!/usr/bin/env python3
"""Build a synthetic snapshot with the real one's SHAPE, for benchmarking.

The performance questions this project keeps hitting -- does a predicate prune,
does a projection get pushed down, does a stage scale with rows or with groups
-- are answered by the query PLAN, which depends on row count, column width,
sort order and predicate selectivity.  None of them depend on the actual
strings.  So they can be answered on a machine that does not hold the 8 GB
snapshot, which is what this builds.

By default: 20,164,595 rows, 70 columns, ~418,000 BINs at ~48 records each, in
taxonomic sort order, with a species of 183 records, a family of ~88,000 and an
order of ~2.0 M -- the Danaus plexippus / Nymphalidae / Lepidoptera shapes.

Two details matter and are easy to get wrong:

* ``sid`` is assigned **before** the sort, exactly as ``snapshot_builder`` does
  it, so the surrogate key is uncorrelated with physical position here too.
  Assign it after the sort instead and every fetch-by-sid measurement comes out
  flattering and wrong.
* The leaf index and the per-row noise use independent hashes.  Share a modulus
  family between them and every row of a leaf falls in the same residue class,
  so per-row variation silently vanishes and whole species end up with no BIN.

About six minutes, and ~1.9 GB on disk::

    python tools/make_benchmark_snapshot.py --out bench.duckdb
    python -m boldcurator.cli benchmark --snapshot bench.duckdb

Do **not** curate from it.  The generated taxon names carry digits, which the
unified species rule rejects, so no record is species-level: it exercises the
machinery, not the biology.
"""

from __future__ import annotations

import argparse
import datetime as dt
import time

import duckdb

from boldcurator.data import schema as S

N_LEAF     = 110_000          # taxonomic leaves of ~183 rows -- the "Danaus" size
GENERA_DIV = 8                # 13,750 genera
FAMILY_DIV = 60               # 229 families of ~88,000 rows -- "Nymphalidae"
ORDER_DIV  = 23               # 10 orders of ~2.0 M rows -- "Lepidoptera"
SPLIT_BINS = 15               # BINs per multi-BIN leaf


def build(out: str, n_rows: int) -> None:
    con = duckdb.connect(out)
    con.execute("SET memory_limit='9GB'")
    con.execute("SET preserve_insertion_order=false")

    # marker_code is kept: the builder writes it into `specimen` and the
    # verifier reads it back, so a snapshot without it cannot be verified.
    phys = [S.physical_name(c) for c in S.REQUIRED_SOURCE_COLUMNS]
    phys += [S.physical_name(c) for c in S.OPTIONAL_SOURCE_COLUMNS]
    phys = list(dict.fromkeys(phys))

    # Two INDEPENDENT scatters. The leaf index and the per-row noise must not share
    # a modulus family, or every row of a leaf lands in the same residue class and
    # per-row variation silently disappears.
    L = f"((i * 2654435761) % {N_LEAF})"
    R = "(CAST(hash(i) % 1000003 AS BIGINT))"
    GEN = f"({L} // {GENERA_DIV})"
    FAM = f"({GEN} // {FAMILY_DIV})"
    ORD = f"({FAM} // {ORDER_DIV})"

    # 67.3% of records carry no species-level identification, as measured on the
    # real snapshot. Applied per LEAF: BOLD's unidentified records are whole
    # BIN-only clusters, not a random scatter inside a named species.
    SPECIES_EXPR = (f"CASE WHEN ({L} % 1000) < 673 THEN NULL "
                    f"ELSE 'Genus' || {GEN} || ' species' || {L} END")

    # 80% of leaves hold exactly one BIN of ~183 records (the Danaus shape); the
    # rest split into 15. 0.5% of rows take a BIN belonging to a different leaf,
    # so expansion has to reach rows far away in the taxonomic sort -- the case
    # that makes expansion expensive. Together: ~418 k BINs, ~48 records each.
    BIN_EXPR = (
        f"'BOLD:A' || lpad(CAST(CASE "
        f" WHEN {R} % 200 = 0 THEN ({L} * 7717 + 13) % 418000 "
        f" WHEN {L} % 5 = 0 THEN {N_LEAF} + ({L} // 5) * {SPLIT_BINS} + ({R} % {SPLIT_BINS}) "
        f" ELSE {L} END AS VARCHAR), 6, '0')"
    )

    VALUES = {
        "processid": "'BCPY' || lpad(CAST(i AS VARCHAR), 9, '0')",
        "marker_code": "'COI-5P'",
    "kingdom": "'Animalia'",
        "phylum": f"'Phylum' || ({ORD} // 6)",
        "class": f"'Class' || ({ORD} // 3)",
        "order_": f"'Order' || {ORD}",
        "family": f"'Family' || {FAM}",
        "subfamily": f"CASE WHEN {R} % 3 = 0 THEN NULL ELSE 'Subfamily' || {FAM} END",
        "tribe": "NULL",
        "genus": f"'Genus' || {GEN}",
        "species": SPECIES_EXPR,
        "subspecies": "NULL",
        "bin_uri": f"CASE WHEN {R} % 100 < 2 THEN NULL ELSE {BIN_EXPR} END",
        "country_ocean": f"['Canada','Germany','Brazil','Kenya','China','Peru','Norway','Chile'][({R} % 8) + 1]",
        "identification": SPECIES_EXPR,
        "identified_by": f"'Identifier ' || ({R} % 5000)",
        "identification_method": f"CASE WHEN {R}%4=0 THEN 'Morphology' WHEN {R}%4=1 THEN 'BIN Taxonomy Match' ELSE 'Tree based identification' END",
        "taxonomy_notes": "NULL",
        "voucher_type": f"CASE WHEN {R}%97=0 THEN 'Holotype' ELSE 'Vouchered:Registered Collection' END",
        "notes": f"CASE WHEN {R}%5=0 THEN NULL ELSE 'Specimen collected during survey ' || ({R}%700) END",
        "short_note": "NULL",
        "collection_notes": f"CASE WHEN {R}%7=0 THEN 'Malaise trap sample ' || ({R}%300) ELSE NULL END",
        "nuc_basecount": f"500 + ({R} % 160)",
        "collectors": f"'Collector ' || ({R} % 20000)",
        "collection_date_start": f"'20' || lpad(CAST(5 + ({R}%20) AS VARCHAR),2,'0') || '-' || lpad(CAST(1+({R}%12) AS VARCHAR),2,'0') || '-' || lpad(CAST(1+({R}%28) AS VARCHAR),2,'0')",
        "collection_date_end": "NULL",
        "site": f"'Site ' || ({R} % 90000)",
        "sector": "NULL",
        "region": f"'Region ' || ({R} % 4000)",
        "coord": f"CAST(round(-90 + ({R}%18000)/100.0, 4) AS VARCHAR) || ',' || CAST(round(-180+({R}%36000)/100.0,4) AS VARCHAR)",
        "inst": f"'Institution number ' || ({R} % 1800)",
        "museumid": f"CASE WHEN {R}%3=0 THEN NULL ELSE 'MUS-' || ({R} % 4000000) END",
        "specimenid": "CAST(10000000 + i AS BIGINT)",
        "taxid": f"CAST({L} AS VARCHAR)",
        "bold_recordset_code_arr": f"'DS-SYN' || ({R} % 13706)",
    }

    def expr_for(col: str) -> str:
        if col in VALUES:
            return VALUES[col]
        if col in ("elev", "depth") or col.endswith("_accuracy"):
            return "NULL"
        return f"CASE WHEN {R} % 4 = 0 THEN '{col[:12]} ' || ({R} % 500) ELSE NULL END"

    select = ",\n  ".join(f"{expr_for(c)} AS {S.quote_ident(c)}" for c in phys)
    sort_cols = [S.quote_ident(c) for c in S.SPECIMEN_SORT_ORDER if c in phys]

    t0 = time.time()
    print(f"generating {n_rows:,} rows x {len(phys)} columns ...", flush=True)
    con.execute("CREATE TABLE stage AS SELECT " + select + f" FROM range({n_rows}) t(i)")
    print(f"  stage: {time.time()-t0:.1f}s", flush=True)
    # sid BEFORE the sort -- exactly what snapshot_builder.py does.
    con.execute(
        f"CREATE TABLE specimen AS "
        f"SELECT row_number() OVER () - 1 AS sid, "
        f"{', '.join(S.quote_ident(c) for c in phys)} "
        f"FROM stage ORDER BY {', '.join(sort_cols)}"
    )
    con.execute("DROP TABLE stage")
    print(f"  specimen: {time.time()-t0:.1f}s", flush=True)

    con.execute("CREATE TABLE sequence (processid VARCHAR, nuc VARCHAR)")
    con.execute("CREATE TABLE specimen_recordset AS "
                "SELECT bold_recordset_code_arr AS recordset_code, sid FROM specimen "
                "ORDER BY recordset_code, sid")

    ranks = [r for r in S.TAXON_RANKS if S.physical_name(r) in phys]
    union = "\nUNION ALL\n".join(
        f"SELECT {S.quote_ident(S.physical_name(r))} AS taxon_name, '{r}' AS taxon_rank "
        f"FROM specimen WHERE {S.quote_ident(S.physical_name(r))} IS NOT NULL "
        f"AND {S.quote_ident(S.physical_name(r))} <> ''" for r in ranks)
    con.execute(f"CREATE TABLE taxon AS SELECT lower(taxon_name) AS taxon_lc, "
                f"any_value(taxon_name) AS taxon_name, taxon_rank, count(*) AS n_records "
                f"FROM ({union}) GROUP BY lower(taxon_name), taxon_rank ORDER BY taxon_lc")
    con.execute("CREATE TABLE bin_species AS SELECT bin_uri, species, count(*) AS n_records "
                "FROM specimen WHERE bin_uri IS NOT NULL AND species IS NOT NULL "
                "GROUP BY bin_uri, species ORDER BY bin_uri, species")

    n_bins = con.execute("SELECT count(DISTINCT bin_uri) FROM specimen").fetchone()[0]
    n_tax = con.execute("SELECT count(*) FROM taxon").fetchone()[0]
    con.execute("CREATE TABLE _meta (key VARCHAR, value VARCHAR)")
    for k, v in {
        "snapshot_id": f"synthetic-{n_rows}", "schema_version": S.SCHEMA_VERSION,
        "row_count": str(n_rows), "sequence_count": "0", "taxon_count": str(n_tax),
        "bin_count": str(n_bins), "marker_filter": "COI-5P",
        "sequences_included": "false",
        "sequence_order": "",
        "built_at": dt.datetime.now().isoformat(timespec="seconds"),
    }.items():
        con.execute("INSERT INTO _meta VALUES (?, ?)", [k, v])
    con.execute("CHECKPOINT")
    con.close()
    print(f"done in {time.time()-t0:.1f}s -- {n_bins:,} BINs, {n_tax:,} taxa")


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    ap.add_argument("--out", required=True, help="path to write the .duckdb file to")
    ap.add_argument("--rows", type=int, default=20_164_595,
                    help="row count (default: the real snapshot's 20,164,595)")
    args = ap.parse_args(argv)
    build(args.out, args.rows)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
