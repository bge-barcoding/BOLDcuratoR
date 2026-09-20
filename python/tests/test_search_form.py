"""The Search form: every field, and the pre-check that sizes a search."""

from __future__ import annotations

import pytest

from boldcurator.config.constants import CONTINENT_COUNTRIES
from boldcurator.ui.state import AppState


@pytest.fixture
def state(store):
    return AppState(store, page_size=10)


def test_taxa_alone_searches(state, store):
    text = state.run_search(taxa_text="Nymphalidae")
    assert state.search is not None
    assert state.search.record_count > 0
    assert "records" in text


def test_the_pre_check_sizes_a_search_without_fetching_anything(state):
    counts = state.estimate(taxa_text="Nymphalidae")
    assert counts["error"] == ""
    assert counts["seed_records"] > 0
    assert counts["expanded_records"] >= counts["seed_records"]
    assert counts["resolved"] == ["Nymphalidae (family)"]
    assert counts["over_limit"] is False


def test_the_pre_check_agrees_with_the_search_it_precedes(state):
    counts = state.estimate(taxa_text="Nymphalidae")
    state.run_search(taxa_text="Nymphalidae")
    assert state.search.record_count == counts["expanded_records"]


def test_a_country_filter_narrows_the_seed(state):
    wide = state.estimate(taxa_text="Nymphalidae")
    narrow = state.estimate(taxa_text="Nymphalidae", countries_text="France")
    assert 0 < narrow["seed_records"] < wide["seed_records"]


def test_continents_expand_to_countries_and_union_with_them(state):
    """A union, not an intersection -- ticking Europe and typing Canada gives
    both, which is what a curator expects and is easy to get backwards."""
    europe = state.estimate(taxa_text="Nymphalidae", continents=["Europe"])
    both = state.estimate(taxa_text="Nymphalidae", continents=["Europe"],
                          countries_text="Canada")
    assert both["seed_records"] >= europe["seed_records"]
    assert "Canada" not in CONTINENT_COUNTRIES["Europe"]


def test_bin_expansion_deliberately_reaches_past_the_geographic_filter(state):
    """The curator needs the whole BIN, wherever its records are from."""
    counts = state.estimate(taxa_text="Nymphalidae", countries_text="France")
    assert counts["expanded_records"] > counts["seed_records"]


def test_a_dataset_code_can_search_on_its_own(state, store):
    code = store.connection.execute(
        "SELECT recordset_code FROM specimen_recordset LIMIT 1").fetchone()[0]
    counts = state.estimate(dataset_text=code)
    assert counts["error"] == ""
    assert counts["seed_records"] > 0
    assert state.run_search(dataset_text=code)
    assert state.search.record_count > 0


def test_an_unknown_code_is_reported_rather_than_silently_empty(state):
    counts = state.estimate(taxa_text="Nymphalidae", dataset_text="DS-NOTREAL")
    assert any("DS-NOTREAL" in w for w in counts["warnings"])


def test_an_unmatched_taxon_is_reported(state):
    counts = state.estimate(taxa_text="Nymphalidae\nNotataxonatall")
    assert any("Notataxonatall" in w for w in counts["warnings"])


def test_an_empty_form_explains_itself_rather_than_erroring(state):
    counts = state.estimate()
    assert "Type a taxon name" in counts["error"]
    assert state.run_search() == counts["error"]
    assert state.search is None


def test_a_form_with_only_unknown_names_says_so(state):
    counts = state.estimate(taxa_text="Notataxonatall")
    assert "None of those names" in counts["error"]


def test_blank_lines_and_stray_whitespace_are_ignored(state):
    counts = state.estimate(taxa_text="  \n Nymphalidae \n\n",
                            countries_text="\n  France  \n\n")
    assert counts["resolved"] == ["Nymphalidae (family)"]
    assert counts["countries"] == 1
