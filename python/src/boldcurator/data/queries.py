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


#: Column name carrying the physical ``rowid`` when ``fetch_rows`` is asked for
#: it.  Leading underscore because it is a handle, not data: it is meaningful
#: only against the snapshot it came from and must never reach an export.
ROW_ID_COLUMN = "_rowid"


def fetch_rows(store: SnapshotStore, row_ids, *,
               columns: list[str] | None = None,
               with_row_id: bool = False,
               order_by_processid: bool = False) -> pd.DataFrame:
    """Project ``columns`` for exactly these ``rowid``s.

    The primitive the whole read path is built on.  ``columns`` are *app*
    names; omit it for the full row.  Asking for one column is how the table
    layer gets a sort key without materialising 70 others, and asking for a
    page's worth of rowids is how it renders a page without materialising the
    result.

    The rows come back in no particular order -- a semi-join has no reason to
    preserve one.  ``with_row_id`` adds :data:`ROW_ID_COLUMN` so a caller that
    needs a specific order can restore it, which is what the table layer does
    for every page.
    """
    physical = store.physical_columns
    if columns is not None:
        wanted = set(columns)
        physical = [c for c in physical if S.app_name(c) in wanted]
        missing = wanted - {S.app_name(c) for c in physical}
        if missing:
            raise KeyError(f"No such column(s) in this snapshot: {sorted(missing)}")
    projection = S.projection(physical)
    if with_row_id:
        projection = f"s.rowid AS {S.quote_ident(ROW_ID_COLUMN)}, {projection}"

    row_ids = np.asarray(row_ids)
    if len(row_ids) == 0:
        return store.connection.execute(
            f"SELECT {projection} FROM specimen s WHERE false"
        ).df()

    order = " ORDER BY s.processid" if order_by_processid else ""
    store.connection.register("_wanted_rows", pd.DataFrame({"rid": row_ids}))
    try:
        return store.connection.execute(
            f"SELECT {projection} FROM specimen s "
            "SEMI JOIN _wanted_rows p ON p.rid = s.rowid"
            f"{order}"
        ).df()
    finally:
        store.connection.unregister("_wanted_rows")


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
    row_ids = plan.row_ids
    frame = fetch_rows(store, row_ids, order_by_processid=True)
    if limit:
        frame = frame.head(int(limit))
    return frame


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


def fetch_by_bin(store: SnapshotStore, bin_uris: list[str]) -> pd.DataFrame:
    """Every specimen row for these BINs, straight from the snapshot.

    Unlike :func:`fetch_rows`, this is not scoped to a plan's row set -- it is
    for the rare case where a BIN's full membership matters more than the
    search that found it, which today is exactly one caller: a grade-E group
    whose sharing species did not itself match the search's taxa or geography
    (see ``core.grouping``'s enrichment). A curator asked to see "everything in
    the BIN", not "everything in the BIN that also matches what I typed".
    """
    bin_uris = [b for b in dict.fromkeys(bin_uris) if b]
    projection = S.projection(store.physical_columns)
    if not bin_uris:
        return store.connection.execute(
            f"SELECT {projection} FROM specimen s WHERE false"
        ).df()
    clause, params = _in_clause("bin_uri", bin_uris)
    return store.connection.execute(
        f"SELECT {projection} FROM specimen s WHERE {clause}", params
    ).df()


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

    Plan then fetch, for the same reason the specimen search does -- and the
    reason is worth stating, because the obvious query is the slow one.

    ``SELECT processid, nuc FROM sequence SEMI JOIN wanted`` makes DuckDB
    project ``nuc`` for all 20 M rows before the join can discard them: 2.6 s
    and 4.5 GB of RSS to return 183 sequences. Resolving the rowids first costs
    0.1 s, because that pass never touches ``nuc``, and the fetch that follows
    can push a rowid filter into the scan.

    **This only pays off on a snapshot whose ``sequence`` table is stored in
    ``specimen`` order**, which is what ``sequence_order`` in ``_meta`` records
    and what ``tools/reorder_sequences.py`` retrofits. Older snapshots store it
    in ingest order, where a taxonomic result's sequences are scattered across
    every row group and neither shape can prune. Measured on a 20 M-row
    snapshot with 4.1 GB of sequences, fetching one species' 183 sequences:

    ==========================  ==============  ==============
    ``sequence`` stored in       one query       plan + fetch
    ==========================  ==============  ==============
    ingest order (old)           2.64 s/4.5 GB   2.39 s/4.5 GB
    specimen order (new)         3.06 s/4.6 GB   **0.21 s/271 MB**
    ==========================  ==============  ==============

    Neither half helps alone. The layout without the query shape still projects
    every sequence; the query shape without the layout has nothing contiguous
    to prune to.

    There is deliberately no ``ORDER BY``: a sort is a blocking operator, it
    would materialise the whole result before yielding a row, and no caller
    needs ordered output -- the FASTA writer looks headers up by processid.
    """
    processids = [p for p in processids if p]
    if not processids:
        return
    ids = pd.DataFrame({"processid": list(dict.fromkeys(processids))})
    store.connection.register("_wanted_ids", ids)
    try:
        row_ids = store.connection.execute(
            "SELECT q.rowid AS rid FROM sequence q "
            "SEMI JOIN _wanted_ids w ON w.processid = q.processid"
        ).fetchnumpy()["rid"]
    finally:
        store.connection.unregister("_wanted_ids")

    if len(row_ids) == 0:
        return

    store.connection.register("_wanted_rows", pd.DataFrame({"rid": row_ids}))
    try:
        cursor = store.connection.execute(
            "SELECT q.processid, q.nuc FROM sequence q "
            "SEMI JOIN _wanted_rows r ON r.rid = q.rowid"
        )
        while True:
            rows = cursor.fetchmany(chunk_size)
            if not rows:
                break
            yield from rows
    finally:
        store.connection.unregister("_wanted_rows")
