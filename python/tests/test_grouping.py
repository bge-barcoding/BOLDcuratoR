"""BAGS grouping: one group per problem, and the riders that come with it."""

from __future__ import annotations

import pandas as pd
import pytest

from boldcurator.core.bags import calculate_bags_grades
from boldcurator.core.grouping import (
    GRADE_DESCRIPTIONS,
    GRADES,
    PRIORITY_GRADES,
    group_specimens,
    specimens_for_grade,
)
from boldcurator.core.summaries import build_species_checklist


def _frame(rows):
    frame = pd.DataFrame(rows)
    if "quality_score" not in frame.columns:
        frame["quality_score"] = 5
    return frame


# A shared BIN (two species), a split species (two BINs), and riders in both.
SPECIMENS = _frame([
    # BOLD:A holds two species -> grade E for both
    {"processid": "P1", "species": "Danaus plexippus", "bin_uri": "BOLD:A",
     "quality_score": 9, "country.ocean": "Canada"},
    {"processid": "P2", "species": "Danaus chrysippus", "bin_uri": "BOLD:A",
     "quality_score": 7, "country.ocean": "Kenya"},
    {"processid": "P3", "species": "Danaus sp.", "bin_uri": "BOLD:A",
     "quality_score": 8, "country.ocean": "Kenya"},          # rider, not species-level
    # Pieris rapae is split across two BINs -> grade C
    {"processid": "P4", "species": "Pieris rapae", "bin_uri": "BOLD:B",
     "quality_score": 6, "country.ocean": "France"},
    {"processid": "P5", "species": "Pieris rapae", "bin_uri": "BOLD:C",
     "quality_score": 4, "country.ocean": "France"},
    {"processid": "P6", "species": "Pieris", "bin_uri": "BOLD:C",
     "quality_score": 10, "country.ocean": "Spain"},         # rider in BOLD:C
    # a clean species with a dozen specimens -> grade A
    *[{"processid": f"Q{i}", "species": "Vanessa atalanta", "bin_uri": "BOLD:D",
       "quality_score": i, "country.ocean": "Norway"} for i in range(12)],
])

GRADES_FRAME = calculate_bags_grades(SPECIMENS)


def _grade_of(species: str) -> str:
    row = GRADES_FRAME.loc[GRADES_FRAME["species"] == species, "bags_grade"]
    return row.iloc[0]


def test_the_fixture_grades_the_way_the_test_assumes():
    assert _grade_of("Danaus plexippus") == "E"
    assert _grade_of("Danaus chrysippus") == "E"
    assert _grade_of("Pieris rapae") == "C"
    assert _grade_of("Vanessa atalanta") == "A"


def test_every_grade_has_a_description_and_the_priorities_are_e_and_c():
    assert set(GRADE_DESCRIPTIONS) == set(GRADES)
    assert PRIORITY_GRADES == ("E", "C")


# -- grade E ---------------------------------------------------------------


def test_grade_e_groups_by_shared_bin_not_by_species():
    groups = group_specimens(SPECIMENS, GRADES_FRAME, "E")
    assert [g.bins for g in groups] == [("BOLD:A",)]
    group = groups[0]
    assert group.species == ("Danaus chrysippus", "Danaus plexippus")
    assert "Shared BIN: BOLD:A" in group.caption
    assert "2 species" in group.caption


def test_a_shared_bin_group_carries_its_non_species_level_records():
    """The genus-only record may be the misidentification. It must be visible."""
    group = group_specimens(SPECIMENS, GRADES_FRAME, "E")[0]
    assert set(group.specimens["processid"]) == {"P1", "P2", "P3"}


def test_groups_lead_with_the_best_specimens():
    group = group_specimens(SPECIMENS, GRADES_FRAME, "E")[0]
    assert list(group.specimens["processid"]) == ["P1", "P3", "P2"]
    assert list(group.specimens["quality_score"]) == [9, 8, 7]


# -- grade C ---------------------------------------------------------------


def test_grade_c_makes_one_group_per_bin_so_each_split_is_separate():
    groups = group_specimens(SPECIMENS, GRADES_FRAME, "C")
    assert len(groups) == 2, "a species in two BINs is two problems, not one"
    assert [g.bins for g in groups] == [("BOLD:B",), ("BOLD:C",)]
    assert all(g.species == ("Pieris rapae",) for g in groups)
    assert "Pieris rapae" in groups[0].caption and "BOLD:B" in groups[0].caption


def test_a_grade_c_group_holds_only_its_own_bin_plus_that_bin_s_riders():
    first, second = group_specimens(SPECIMENS, GRADES_FRAME, "C")
    assert set(first.specimens["processid"]) == {"P4"}
    assert set(second.specimens["processid"]) == {"P5", "P6"}


# -- grades A / B / D ------------------------------------------------------


def test_grade_a_groups_by_species():
    groups = group_specimens(SPECIMENS, GRADES_FRAME, "A")
    assert len(groups) == 1
    assert groups[0].species == ("Vanessa atalanta",)
    assert "single BIN" in groups[0].caption
    assert groups[0].specimen_count == 12


# -- the filter ------------------------------------------------------------


def test_specimens_for_grade_excludes_other_grades(): 
    chosen = specimens_for_grade(SPECIMENS, GRADES_FRAME, "C")
    assert set(chosen["species"].dropna()) == {"Pieris rapae", "Pieris"}


def test_a_grade_with_no_species_gives_no_groups():
    assert group_specimens(SPECIMENS, GRADES_FRAME, "B") == []


def test_an_empty_frame_gives_no_groups():
    empty = SPECIMENS.iloc[:0]
    assert group_specimens(empty, GRADES_FRAME, "E") == []
    assert group_specimens(SPECIMENS, GRADES_FRAME.iloc[:0], "E") == []


def test_an_unknown_grade_is_refused():
    with pytest.raises(ValueError, match="Unknown BAGS grade"):
        group_specimens(SPECIMENS, GRADES_FRAME, "Z")


def test_every_grade_group_is_a_subset_of_that_grade_s_specimens():
    for grade in GRADES:
        chosen = set(specimens_for_grade(SPECIMENS, GRADES_FRAME, grade)["processid"])
        for group in group_specimens(SPECIMENS, GRADES_FRAME, grade):
            assert set(group.specimens["processid"]) <= chosen


# -- sharing that is invisible locally -------------------------------------


def test_a_bin_shared_only_in_the_snapshot_still_gets_a_group_and_says_so():
    """Grade E is graded against the whole snapshot, not the download.

    R drops such a BIN because it counts species among downloaded records only,
    which hides the records the grade exists to flag.
    """
    local = _frame([
        {"processid": "R1", "species": "Danaus plexippus", "bin_uri": "BOLD:X",
         "quality_score": 5},
    ])
    snapshot_bins = pd.DataFrame([
        {"bin_uri": "BOLD:X", "species": "Danaus plexippus"},
        {"bin_uri": "BOLD:X", "species": "Danaus chrysippus"},   # not downloaded
    ])
    grades = calculate_bags_grades(local, bin_species=snapshot_bins)
    assert grades["bags_grade"].iloc[0] == "E"

    groups = group_specimens(local, grades, "E")
    assert len(groups) == 1
    assert groups[0].note, "a group that looks innocent must explain itself"
    assert "outside this search" in groups[0].note


# -- the species checklist -------------------------------------------------


def test_the_species_checklist_counts_what_the_r_app_counts():
    checklist = build_species_checklist(SPECIMENS, GRADES_FRAME).set_index("species")
    assert checklist.loc["Pieris rapae", "specimen_count"] == 2
    assert checklist.loc["Pieris rapae", "bin_count"] == 2
    assert checklist.loc["Pieris rapae", "bin_uris"] == "BOLD:B; BOLD:C"
    assert checklist.loc["Pieris rapae", "bags_grade"] == "C"
    assert checklist.loc["Vanessa atalanta", "specimen_count"] == 12
    assert checklist.loc["Danaus plexippus", "countries"] == "Canada"
    assert checklist.loc["Pieris rapae", "mean_quality_score"] == 5.0


def test_the_checklist_is_sorted_and_empty_input_keeps_its_columns():
    checklist = build_species_checklist(SPECIMENS, GRADES_FRAME)
    assert list(checklist["species"]) == sorted(checklist["species"])
    empty = build_species_checklist(SPECIMENS.iloc[:0])
    assert len(empty) == 0
    assert "mean_quality_score" in empty.columns
