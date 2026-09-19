import pandas as pd

from boldcurator.core.bags import calculate_bags_grades, determine_grade


def test_grade_thresholds_follow_the_implementation_not_the_constant():
    """R's BAGS_GRADE_CRITERIA says A at 10; determine_bags_grade says 11."""
    assert determine_grade(11, 1, False) == "A"
    assert determine_grade(10, 1, False) == "B"
    assert determine_grade(3, 1, False) == "B"
    assert determine_grade(2, 1, False) == "D"


def test_multiple_bins_is_c_and_sharing_is_e():
    assert determine_grade(50, 2, False) == "C"
    assert determine_grade(50, 1, True) == "E"
    # sharing wins over bin count
    assert determine_grade(50, 3, True) == "E"


def test_invalid_counts_grade_e():
    assert determine_grade(None, 1, False) == "E"
    assert determine_grade(float("nan"), 1, False) == "E"
    assert determine_grade(-1, 1, False) == "E"
    assert determine_grade("x", 1, False) == "E"


def _frame(rows):
    return pd.DataFrame(rows)


def test_specimen_count_includes_records_without_a_bin():
    rows = [{"species": "Danaus plexippus", "bin_uri": "BOLD:A",
             "identification_rank": "species"} for _ in range(2)]
    rows.append({"species": "Danaus plexippus", "bin_uri": "",
                 "identification_rank": "species"})
    grades = calculate_bags_grades(_frame(rows))
    assert grades["specimen_count"].iloc[0] == 3
    assert grades["bin_count"].iloc[0] == 1
    assert grades["bags_grade"].iloc[0] == "B"   # 3 specimens, not D


def test_shared_bin_gives_both_species_grade_e():
    rows = (
        [{"species": "Danaus plexippus", "bin_uri": "BOLD:A",
          "identification_rank": "species"}] * 20
        + [{"species": "Danaus chrysippus", "bin_uri": "BOLD:A",
            "identification_rank": "species"}] * 20
    )
    grades = calculate_bags_grades(_frame(rows)).set_index("species")
    assert set(grades["bags_grade"]) == {"E"}
    assert grades["shared_bins"].all()


def test_snapshot_scope_detects_sharing_the_local_frame_cannot_see():
    """The scientific improvement: R can only see downloaded records."""
    local = _frame([{"species": "Danaus plexippus", "bin_uri": "BOLD:A",
                     "identification_rank": "species"}] * 20)
    bin_species = _frame([
        {"bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "identification_rank": "species"},
        {"bin_uri": "BOLD:A", "species": "Danaus chrysippus",
         "identification_rank": "species"},
    ])

    local_only = calculate_bags_grades(local)
    assert local_only["bags_grade"].iloc[0] == "A"
    assert local_only["shared_bin_scope"].iloc[0] == "local"

    with_snapshot = calculate_bags_grades(local, bin_species=bin_species)
    assert with_snapshot["bags_grade"].iloc[0] == "E"
    assert with_snapshot["shared_bin_scope"].iloc[0] == "snapshot"


def test_non_species_level_rows_are_not_graded():
    rows = [{"species": "Danaus sp.", "bin_uri": "BOLD:A",
             "identification_rank": "genus"}] * 20
    assert len(calculate_bags_grades(_frame(rows))) == 0
