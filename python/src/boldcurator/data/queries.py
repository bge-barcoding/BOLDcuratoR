"""The four access points, against a read-only snapshot.

Replaces every BOLD API call the R app makes.  Two things govern the shapes
here:

**Never interpolate user text into an identifier position.**  A resolved rank
picks a column name from a fixed whitelist; every value is a bound parameter.

**Preserve the R fetch semantics exactly.**  In particular the geographic
filter applies to the *seed* and is deliberately **not** re-applied after BIN
expansion (``mod_data_import_server.R:223-277`` then ``:479-534``), so the
result is "records matching the query and the geography" plus "all records
sharing those records' BINs, wherever they are from".  That asymmetry is the
point: the curator needs the full BIN context.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from . import schema as S
from .snapshot import SnapshotStore

#: Only these may appear in an identifier position.
_RANK_WHITELIST = frozenset(S.TAXON_RANKS)


def _in_clause(column: str, values: list[str]) -> tuple[str, list[str]]:
    placeholders = ", ".join("?" for _ in values)
    return f"{column} IN ({placeholders})", list(values)


# --------------------------------------------------------------------------
# Taxon resolution
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedTaxon:
    query: str          # what the user typed
    name: str           # canonical casing from the snapshot
    rank: str           # one of S.TAXON_RANKS
    n_records: int


@dataclass
class Resolution:
    resolved: list[ResolvedTaxon] = field(default_factory=list)
    unmatched: list[str] = field(default_factory=list)
    #: Names that resolved to more than one rank (e.g. both a genus and a
    #: subfamily).  R silently OR-ed across columns; prompting is better.
    ambiguous: dict[str, list[ResolvedTaxon]] = field(default_factory=dict)

    @property
    def total_records(self) -> int:
        return sum(t.n_records for t in self.resolved)


def resolve_taxa(store: SnapshotStore, names: list[str]) -> Resolution:
    """Resolve names to (rank, canonical name, record count).

    Hits the small ``taxon`` table, so it is fast enough to run as the user
    types, and it makes the size of a result knowable **before** anything is
    materialised.  It also removes the title-casing hack at
    ``mod_data_import_server.R:695`` -- the lookup is case-insensitive by
    construction.
    """
    cleaned = [n.strip() for n in names if n and n.strip()]
    if not cleaned:
        return Resolution()

    lowered = [n.lower() for n in cleaned]
    clause, params = _in_clause("taxon_lc", sorted(set(lowered)))
    rows = store.connection.execute(
        f"SELECT taxon_lc, taxon_name, taxon_rank, n_records FROM taxon "
        f"WHERE {clause}",
        params,
    ).fetchall()

    by_lc: dict[str, list[ResolvedTaxon]] = {}
    for taxon_lc, name, rank, n in rows:
        by_lc.setdefault(taxon_lc, []).append(ResolvedTaxon(taxon_lc, name, rank, int(n)))

    result = Resolution()
    seen: set[tuple[str, str]] = set()
    for original, lc in zip(cleaned, lowered):
        matches = by_lc.get(lc)
        if not matches:
            result.unmatched.append(original)
            continue
        if len(matches) > 1:
            result.ambiguous[original] = [
                ResolvedTaxon(original, m.name, m.rank, m.n_records) for m in matches
            ]
        for m in matches:
            key = (m.rank, m.name)
            if key in seen:
                continue
            seen.add(key)
            result.resolved.append(ResolvedTaxon(original, m.name, m.rank, m.n_records))
    return result


# --------------------------------------------------------------------------
# Search
# --------------------------------------------------------------------------


@dataclass
class SearchQuery:
    taxa: list[ResolvedTaxon] = field(default_factory=list)
    countries: list[str] = field(default_factory=list)
    dataset_codes: list[str] = field(default_factory=list)
    project_codes: list[str] = field(default_factory=list)
    expand_bins: bool = True
    limit: int | None = None

    @property
    def recordset_codes(self) -> list[str]:
        return list(dict.fromkeys([*self.dataset_codes, *self.project_codes]))

    def is_empty(self) -> bool:
        return not self.taxa and not self.recordset_codes


def _seed_sql(query: SearchQuery) -> tuple[str, list[object]]:
    """The seed CTE: one single-column equality per resolved rank, UNIONed.

    Not a cross-column disjunction.  ``WHERE genus IN (...) OR family IN (...)``
    forces a scan, because zone maps can only prune one column's predicate at a
    time; a UNION of single-column predicates lets each branch prune.
    """
    geo_clause = ""
    geo_params: list[object] = []
    if query.countries:
        clause, params = _in_clause("country_ocean", sorted(set(query.countries)))
        # SQL IN is false for NULL, which reproduces
        # filter_specimens_by_continent's NA-dropping exactly.
        geo_clause = f" AND {clause}"
        geo_params = params

    branches: list[str] = []
    params: list[object] = []

    by_rank: dict[str, list[str]] = {}
    for taxon in query.taxa:
        if taxon.rank not in _RANK_WHITELIST:
            raise ValueError(f"Refusing to query unknown rank {taxon.rank!r}")
        by_rank.setdefault(taxon.rank, []).append(taxon.name)

    for rank, names in by_rank.items():
        column = S.quote_ident(S.physical_name(rank))
        clause, values = _in_clause(column, sorted(set(names)))
        branches.append(
            f"SELECT sid, bin_uri FROM specimen WHERE {clause}{geo_clause}"
        )
        params.extend(values)
        params.extend(geo_params)

    codes = query.recordset_codes
    if codes:
        clause, values = _in_clause("recordset_code", sorted(set(codes)))
        branches.append(
            f"SELECT s.sid, s.bin_uri FROM specimen s "
            f"WHERE s.sid IN (SELECT sid FROM specimen_recordset WHERE {clause})"
            f"{geo_clause}"
        )
        params.extend(values)
        params.extend(geo_params)

    if not branches:
        raise ValueError("Search has no taxa and no dataset/project codes")

    return "\nUNION\n".join(branches), params


def _seed_counts(store: SnapshotStore, seed_sql: str, params: list[object]
                 ) -> tuple[int, int]:
    seed_records, seed_bins = store.connection.execute(
        f"WITH seed AS ({seed_sql}) "
        "SELECT count(*), count(DISTINCT bin_uri) FILTER "
        "(WHERE bin_uri IS NOT NULL AND bin_uri <> '') FROM seed",
        params,
    ).fetchone()
    return int(seed_records), int(seed_bins or 0)


def _expansion_sql(seed_sql: str, projection: str) -> str:
    """The BIN-expanded row set, projecting whatever ``projection`` asks for.

    The ``sid`` branch keeps seed records that have no BIN, which BIN expansion
    would otherwise drop.
    """
    return (
        f"WITH seed AS ({seed_sql}), "
        "seed_bins AS (SELECT DISTINCT bin_uri FROM seed "
        "              WHERE bin_uri IS NOT NULL AND bin_uri <> '') "
        f"SELECT {projection} FROM specimen s "
        "WHERE s.sid IN (SELECT sid FROM seed) "
        "   OR s.bin_uri IN (SELECT bin_uri FROM seed_bins)"
    )


def estimate_search(store: SnapshotStore, query: SearchQuery) -> dict[str, int]:
    """Row and BIN counts **before** anything is materialised.

    This is what lets the size check fire in front of the fetch instead of
    after it, which is the single cheapest protection against a runaway query.

    Counts only, for the as-you-type pre-check.  A search that is going to run
    anyway should call :func:`plan_search`, which costs the same and hands back
    the rows it counted.
    """
    seed_sql, params = _seed_sql(query)
    seed_records, seed_bins = _seed_counts(store, seed_sql, params)

    expanded = seed_records
    if query.expand_bins and seed_bins:
        expanded = int(store.connection.execute(
            _expansion_sql(seed_sql, "count(*)"), params
        ).fetchone()[0])

    return {
        "seed_records": seed_records,
        "seed_bins": seed_bins,
        "expanded_records": expanded,
    }


# --------------------------------------------------------------------------
# Plan, then fetch
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class SearchPlan:
    """Exactly which rows a search returns, resolved before any is materialised.

    ``row_ids`` holds DuckDB ``rowid`` values -- physical positions in
    ``specimen``.  They are stable because the snapshot is opened read-only and
    never written; nothing else would make them safe to carry between queries.
    """

    row_ids: "np.ndarray"
    seed_records: int
    seed_bins: int

    @property
    def expanded_records(self) -> int:
        return int(len(self.row_ids))

    def as_estimate(self) -> dict[str, int]:
        return {
            "seed_records": self.seed_records,
            "seed_bins": self.seed_bins,
            "expanded_records": self.expanded_records,
        }


def plan_search(store: SnapshotStore, query: SearchQuery) -> SearchPlan:
    """Resolve the result set **narrowly** -- rowids and counts, no payload.

    This is the half of a search that is cheap.  Splitting it out is what makes
    the other half cheap too: see :func:`fetch_planned`.
    """
    seed_sql, params = _seed_sql(query)
    seed_records, seed_bins = _seed_counts(store, seed_sql, params)

    if query.expand_bins and seed_bins:
        sql = _expansion_sql(seed_sql, "s.rowid AS rid")
    else:
        sql = (f"WITH seed AS ({seed_sql}) "
               "SELECT s.rowid AS rid FROM specimen s "
               "WHERE s.sid IN (SELECT sid FROM seed)")
    row_ids = store.connection.execute(sql, params).fetchnumpy()["rid"]
    return SearchPlan(row_ids=np.asarray(row_ids), seed_records=seed_records,
                      seed_bins=seed_bins)


def fetch_planned(store: SnapshotStore, plan: SearchPlan, *,
                  limit: int | None = None) -> pd.DataFrame:
    """Materialise the 70-odd columns for an already-resolved row set.

    **Why a semi-join on ``rowid`` and not the obvious ``WHERE ... OR ...``.**
    The predicate the search really wants -- "in the seed, *or* sharing one of
    the seed's BINs" -- is a disjunction over two subqueries.  DuckDB cannot
    push either half into the table scan, so it projects all 70 columns of all
    20 M rows and filters afterwards: 3.0 s and 2.9 GB of RSS to return 183
    rows.  That, not BIN expansion, was the cost.

    Resolving the rowids first costs 0.17 s, and the fetch that follows joins a
    tiny build side against ``rowid``, which DuckDB *can* push into the scan as
    a zone-map filter.  Measured on a 20 M-row snapshot of the real shape:

    ======================  =========  ==========
    183-row result          current    two-phase
    ======================  =========  ==========
    resolve + fetch          3.03 s     0.22 s
    88,000-row result        3.66 s     1.01 s
    ======================  =========  ==========

    ``rowid`` is the key that works and ``sid`` is not, because ``sid`` is
    assigned before the taxonomic sort and so is uncorrelated with physical
    position -- a semi-join on ``sid`` measures 2.8 s, no better than the
    original.  A literal ``rowid IN (...)`` list is no better either; the
    pushdown comes from the join, not the predicate.
    """
    projection = S.projection(store.physical_columns)
    limit_sql = f" LIMIT {int(limit)}" if limit else ""
    if plan.expanded_records == 0:
        return store.connection.execute(
            f"SELECT {projection} FROM specimen s WHERE false"
        ).df()

    store.connection.register("_planned_rows", pd.DataFrame({"rid": plan.row_ids}))
    try:
        return store.connection.execute(
            f"SELECT {projection} FROM specimen s "
            "SEMI JOIN _planned_rows p ON p.rid = s.rowid "
            f"ORDER BY s.processid{limit_sql}"
        ).df()
    finally:
        store.connection.unregister("_planned_rows")


class ResultTooLarge(RuntimeError):
    """A search resolved to more rows than the caller is willing to hold.

    Raised from the plan, before the wide fetch, so the refusal costs the
    narrow pass and nothing else.
    """

    def __init__(self, records: int, maximum: int):
        self.records, self.maximum = records, maximum
        super().__init__(
            f"This search resolves to {records:,} records, over the "
            f"{maximum:,} this call allows."
        )


def search_specimens(store: SnapshotStore, query: SearchQuery, *,
                     max_records: int | None = None) -> pd.DataFrame:
    """Run the search, with BIN expansion, and return an app-shaped frame.

    Plan then fetch -- see :func:`fetch_planned` for why the two-phase shape is
    worth the extra query.

    ``nuc`` is never projected here -- sequences are fetched on demand by
    ``iter_sequences``.  In the R app ``nuc`` rides through every merge and every
    session serialisation, which is most of why a 50,000-row result costs
    hundreds of megabytes.

    ``max_records`` refuses an oversized result before materialising it.  It is
    opt-in because ``run_search`` already applies ``DOWNLOAD_LIMITS``; anything
    calling this directly is on its own otherwise, which is how the benchmark
    came to materialise 2,095,427 rows into pandas without complaint.
    """
    plan = plan_search(store, query)
    if max_records is not None and plan.expanded_records > max_records:
        raise ResultTooLarge(plan.expanded_records, max_records)
    return fetch_planned(store, plan, limit=query.limit)


def missing_recordset_codes(store: SnapshotStore, codes: list[str]) -> list[str]:
    """Which requested codes matched nothing.

    Offline there is no 401: a private or mistyped code both return zero rows
    and look identical.  Reporting the difference is the only way a user can
    tell them apart.
    """
    codes = [c.strip() for c in codes if c and c.strip()]
    if not codes:
        return []
    clause, params = _in_clause("recordset_code", sorted(set(codes)))
    found = {
        r[0] for r in store.connection.execute(
            f"SELECT DISTINCT recordset_code FROM specimen_recordset WHERE {clause}",
            params,
        ).fetchall()
    }
    return [c for c in codes if c not in found]


# --------------------------------------------------------------------------
# Sequences
# --------------------------------------------------------------------------


def iter_sequences(
    store: SnapshotStore,
    processids: list[str],
    *,
    chunk_size: int = 5000,
):
    """Yield ``(processid, nuc)`` in chunks, at constant memory.

    Used by the FASTA exporters so a large export costs the same as a small
    one.  R's ``download_fasta`` loops over a fully materialised frame.
    """
    processids = [p for p in processids if p]
    if not processids:
        return
    ids = pd.DataFrame({"processid": list(dict.fromkeys(processids))})
    store.connection.register("_wanted_ids", ids)
    try:
        # No ORDER BY. A sort is a blocking operator: it materialises the
        # entire join result before yielding a single row, which defeats the
        # streaming this function exists for and made the docstring's
        # "constant memory" false. Fetching 183 sequences took 8.7 s and
        # pushed RSS to 15.5 GB in the 2026-09-11 benchmark. No caller needs
        # ordered output -- the FASTA writer looks headers up by processid.
        cursor = store.connection.execute(
            "SELECT q.processid, q.nuc FROM sequence q "
            "SEMI JOIN _wanted_ids w ON w.processid = q.processid"
        )
        while True:
            rows = cursor.fetchmany(chunk_size)
            if not rows:
                break
            yield from rows
    finally:
        store.connection.unregister("_wanted_ids")
