"""Gap analysis: did the search actually find what the curator typed."""

from __future__ import annotations

import pandas as pd

from boldcurator.core.summaries import GAP_ANALYSIS_COLUMNS, gap_analysis

SPECIMENS = pd.DataFrame({
    "processid": ["P1", "P2", "P3"],
    "species": ["Danaus plexippus", "Danaus plexippus", "Pieris rapae"],
})


def test_a_typed_taxon_the_search_found_is_reported_found():
    result = gap_analysis([["Danaus plexippus"]], SPECIMENS).set_index("input_taxon")
    row = result.loc["Danaus plexippus"]
    assert row["status"] == "Found"
    assert row["matched_species"] == "Danaus plexippus"
    assert row["specimen_count"] == 2
    assert row["notes"] == ""


def test_a_typed_taxon_the_search_never_matched_is_reported_missing():
    result = gap_analysis([["Not a real species"]], SPECIMENS).set_index("input_taxon")
    row = result.loc["Not a real species"]
    assert row["status"] == "Missing"
    assert row["matched_species"] == ""
    assert row["specimen_count"] == 0


def test_matching_only_via_a_synonym_says_so():
    """The valid name is tried first; only a synonym match gets the note."""
    groups = [["Danaus plexippus", "Danaus erippus"], ["Nothing here"]]
    result = gap_analysis(groups, SPECIMENS).set_index("input_taxon")

    valid_row = result.loc["Danaus plexippus, Danaus erippus"]
    assert valid_row["status"] == "Found"
    assert valid_row["notes"] == "", "the valid name itself matched -- no synonym note"

    # A search where only the synonym is in the data.
    synonym_only = pd.DataFrame({"processid": ["P1"], "species": ["Danaus erippus"]})
    result2 = gap_analysis(
        [["Danaus plexippus", "Danaus erippus"]], synonym_only
    ).set_index("input_taxon")
    row = result2.loc["Danaus plexippus, Danaus erippus"]
    assert row["status"] == "Found"
    assert row["matched_species"] == "Danaus erippus"
    assert row["notes"] == "Matched via synonym: Danaus erippus"


def test_matching_is_case_insensitive():
    result = gap_analysis([["DANAUS PLEXIPPUS"]], SPECIMENS).set_index("input_taxon")
    row = result.loc["DANAUS PLEXIPPUS"]
    assert row["status"] == "Found"
    assert row["matched_species"] == "Danaus plexippus", \
        "the frame's own spelling is reported, not the curator's"


def test_the_group_label_joins_synonyms_with_a_comma():
    result = gap_analysis([["Valid name", "Synonym one", "Synonym two"]], SPECIMENS)
    assert list(result["input_taxon"]) == ["Valid name, Synonym one, Synonym two"]


def test_an_empty_search_result_reports_every_taxon_missing_with_a_reason():
    empty = SPECIMENS.iloc[:0]
    result = gap_analysis([["Danaus plexippus"], ["Pieris rapae"]], empty)
    assert (result["status"] == "Missing").all()
    assert (result["notes"] == "No specimen data available").all()
    assert (result["specimen_count"] == 0).all()


def test_no_taxa_typed_gives_an_empty_frame_with_the_right_columns():
    result = gap_analysis([], SPECIMENS)
    assert len(result) == 0
    assert list(result.columns) == GAP_ANALYSIS_COLUMNS


def test_a_blank_line_in_a_group_is_skipped_not_matched():
    result = gap_analysis([["", "Danaus plexippus"]], SPECIMENS)
    assert result.iloc[0]["status"] == "Found"
    assert result.iloc[0]["matched_species"] == "Danaus plexippus"
