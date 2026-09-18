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
