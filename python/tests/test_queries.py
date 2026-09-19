"""Query-layer tests against a real snapshot built by the real builder."""

import pytest

from boldcurator.data import queries as Q


def test_snapshot_opens_and_describes_itself(store):
    info = store.info()
    assert info.row_count > 0
    assert info.marker_filter == "COI-5P"
    assert info.sequences_included
    assert "records" in info.describe()


def test_resolve_is_case_insensitive_and_reports_rank(store):
    res = Q.resolve_taxa(store, ["DANAUS PLEXIPPUS", "nymphalidae"])
    by_rank = {t.rank: t.name for t in res.resolved}
    assert by_rank["species"] == "Danaus plexippus"
    assert by_rank["family"] == "Nymphalidae"
    assert all(t.n_records > 0 for t in res.resolved)


def test_unmatched_names_are_reported_not_silently_dropped(store):
    res = Q.resolve_taxa(store, ["Danaus plexippus", "Notataxonatall"])
    assert res.unmatched == ["Notataxonatall"]
    assert len(res.resolved) == 1


def test_ambiguous_names_are_surfaced(store):
    """A name that is both a genus and something else must not silently OR."""
    res = Q.resolve_taxa(store, ["Danaus"])
    assert [t.rank for t in res.resolved] == ["genus"]
    # Danainae is a subfamily only; use it to prove single-rank names are clean
    assert not Q.resolve_taxa(store, ["Danainae"]).ambiguous


def test_unknown_rank_is_refused_rather_than_interpolated(store):
    bogus = Q.ResolvedTaxon("x", "x", "'; DROP TABLE specimen; --", 1)
    with pytest.raises(ValueError, match="unknown rank"):
        Q.estimate_search(store, Q.SearchQuery(taxa=[bogus]))


def test_geographic_filter_narrows_the_seed(store):
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    wide = Q.estimate_search(store, Q.SearchQuery(taxa=taxa, expand_bins=False))
    narrow = Q.estimate_search(
        store, Q.SearchQuery(taxa=taxa, countries=["France"], expand_bins=False)
    )
    assert 0 < narrow["seed_records"] < wide["seed_records"]


def test_bin_expansion_deliberately_crosses_the_geographic_filter(store):
    """The R semantic: geography filters the seed, not the expansion.

    The curator needs the full BIN context, so records from other countries
    that share a seed BIN are pulled in on purpose.
    """
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    query = Q.SearchQuery(taxa=taxa, countries=["France"], expand_bins=True)
    frame = Q.search_specimens(store, query)
    countries = set(frame["country.ocean"].dropna().unique())
    assert "France" in countries
    assert len(countries) > 1


def test_expansion_keeps_seed_records_that_have_no_bin(store):
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    seed = Q.search_specimens(store, Q.SearchQuery(taxa=taxa, expand_bins=False))
    expanded = Q.search_specimens(store, Q.SearchQuery(taxa=taxa, expand_bins=True))
    assert set(seed["processid"]) <= set(expanded["processid"])


def test_sequences_are_never_projected_into_a_search(store):
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    frame = Q.search_specimens(store, Q.SearchQuery(taxa=taxa))
    assert "nuc" not in frame.columns
    # and the app-facing names are used, not the physical ones
    assert "country.ocean" in frame.columns and "country_ocean" not in frame.columns
    assert "order" in frame.columns


def test_sequences_stream_on_demand(store):
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    frame = Q.search_specimens(store, Q.SearchQuery(taxa=taxa, limit=20))
    pairs = list(Q.iter_sequences(store, frame["processid"].tolist(), chunk_size=5))
    assert pairs
    assert all(set(seq) <= set("ACGTN") for _, seq in pairs)


def test_missing_recordset_codes_are_reported(store):
    missing = Q.missing_recordset_codes(store, ["DS-TEST1", "DS-DEFINITELYNOTREAL"])
    assert missing == ["DS-DEFINITELYNOTREAL"]


def test_search_with_no_criteria_is_refused(store):
    with pytest.raises(ValueError):
        Q.estimate_search(store, Q.SearchQuery())


# --------------------------------------------------------------------------
# Plan, then fetch
# --------------------------------------------------------------------------


def test_plan_agrees_with_estimate(store):
    """The plan replaces the estimate in the pipeline, so it must count the same."""
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    for expand in (True, False):
        query = Q.SearchQuery(taxa=taxa, expand_bins=expand)
        assert Q.plan_search(store, query).as_estimate() == \
            Q.estimate_search(store, query)


def test_fetch_by_plan_returns_exactly_the_rows_the_plan_resolved(store):
    taxa = Q.resolve_taxa(store, ["Nymphalidae"]).resolved
    query = Q.SearchQuery(taxa=taxa, expand_bins=True)
    plan = Q.plan_search(store, query)
    frame = Q.fetch_planned(store, plan)
    assert len(frame) == plan.expanded_records
    assert frame.equals(Q.search_specimens(store, query))


def test_plan_row_ids_are_unique(store):
    """A duplicated rowid would silently duplicate records in the fetch."""
    taxa = Q.resolve_taxa(store, ["Nymphalidae"]).resolved
    plan = Q.plan_search(store, Q.SearchQuery(taxa=taxa, expand_bins=True))
    assert len(set(plan.row_ids.tolist())) == plan.expanded_records


def test_an_empty_plan_still_gives_a_correctly_shaped_frame(store):
    """Zero rows must not mean zero columns -- callers index by name."""
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    query = Q.SearchQuery(taxa=taxa, countries=["Atlantis"], expand_bins=False)
    plan = Q.plan_search(store, query)
    assert plan.expanded_records == 0
    frame = Q.fetch_planned(store, plan)
    assert len(frame) == 0
    assert list(frame.columns) == store.app_columns


def test_the_fetch_size_guard_refuses_before_materialising(store):
    taxa = Q.resolve_taxa(store, ["Nymphalidae"]).resolved
    query = Q.SearchQuery(taxa=taxa, expand_bins=True)
    n = Q.plan_search(store, query).expanded_records
    with pytest.raises(Q.ResultTooLarge) as caught:
        Q.search_specimens(store, query, max_records=n - 1)
    assert caught.value.records == n
    # and it is opt-in: the same search without the cap still runs
    assert len(Q.search_specimens(store, query)) == n


def test_the_plan_respects_the_limit_on_fetch_not_on_resolution(store):
    """A limit caps what is materialised; the size check still sees the truth."""
    taxa = Q.resolve_taxa(store, ["Nymphalidae"]).resolved
    query = Q.SearchQuery(taxa=taxa, expand_bins=True, limit=5)
    plan = Q.plan_search(store, query)
    assert plan.expanded_records > 5
    assert len(Q.fetch_planned(store, plan, limit=5)) == 5


def test_fetch_by_bin_ignores_the_search_entirely(store):
    """It is not scoped to a plan -- a whole BIN, by bin_uri, nothing else."""
    taxa = Q.resolve_taxa(store, ["Danaus plexippus"]).resolved
    plan = Q.plan_search(store, Q.SearchQuery(taxa=taxa, expand_bins=True))
    seeded = Q.fetch_planned(store, plan)
    a_bin = next(b for b in seeded["bin_uri"] if isinstance(b, str) and b)

    by_bin = Q.fetch_by_bin(store, [a_bin])
    assert len(by_bin) > 0
    assert set(by_bin["bin_uri"].dropna().unique()) == {a_bin}
    # Whatever the search's own BIN expansion already found for this BIN must
    # be a subset of what a direct fetch of the BIN finds.
    assert set(seeded.loc[seeded["bin_uri"] == a_bin, "processid"]) <= \
        set(by_bin["processid"])


def test_fetch_by_bin_of_nothing_is_an_empty_frame_with_the_right_columns(store):
    empty = Q.fetch_by_bin(store, [])
    assert len(empty) == 0
    assert "processid" in empty.columns
    assert "bin_uri" in empty.columns
