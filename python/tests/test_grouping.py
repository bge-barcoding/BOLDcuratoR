"""BAGS grouping: one group per problem, and the riders that come with it."""

from __future__ import annotations

import pandas as pd
import pytest

from boldcurator.core.bags import calculate_bags_grades, shared_bins
from boldcurator.core.grouping import (
    GRADE_DESCRIPTIONS,
    GRADES,
    PRIORITY_GRADES,
    SPLIT_SHARED,
    UNNAMED,
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
    assert groups[0].caption == "Species: Vanessa atalanta", \
        "no grade qualifier: the grade header already says it (round 8, 3.1)"
    assert groups[0].specimen_count == 12


def test_a_bin_less_record_does_not_count_toward_or_show_in_any_grade():
    """Round 7, BAGS analysis item 1: a BIN-less record must not move a

    grade one way or the other, and must not appear in that grade's group
    table either -- the project owner's explicit call, a deliberate
    divergence from R (which counts it; round 6 of this port matched that,
    before this request reversed it). 9 BIN-assigned records plus 2
    BIN-less ones must grade B (3-10 specimens), not A, and the group must
    show only the 9.
    """
    frame = _frame([
        *[{"processid": f"B{i}", "species": "Papilio machaon", "bin_uri": "BOLD:E",
           "quality_score": i} for i in range(9)],
        # Two more species-level records of the same species, awaiting BIN
        # assignment -- no bin_uri at all.
        {"processid": "B9", "species": "Papilio machaon", "bin_uri": "",
         "quality_score": 1},
        {"processid": "B10", "species": "Papilio machaon", "bin_uri": pd.NA,
         "quality_score": 1},
    ])
    grades = calculate_bags_grades(frame)
    assert grades.loc[grades["species"] == "Papilio machaon",
                      "bags_grade"].iloc[0] == "B"
    assert grades.loc[grades["species"] == "Papilio machaon",
                      "specimen_count"].iloc[0] == 9

    groups = group_specimens(frame, grades, "B")
    assert len(groups) == 1
    assert groups[0].specimen_count == 9
    assert set(groups[0].specimens["processid"]) == {f"B{i}" for i in range(9)}


def test_a_species_with_no_bin_assigned_records_gets_no_grade_at_all():
    """Round 7: nothing to judge a BIN cohesion grade on -> no grade, not a

    default one -- and it must not show up in any grade's group list.
    """
    frame = _frame([
        {"processid": "N1", "species": "Bombus terrestris", "bin_uri": "",
         "quality_score": 5},
        {"processid": "N2", "species": "Bombus terrestris", "bin_uri": pd.NA,
         "quality_score": 4},
    ])
    grades = calculate_bags_grades(frame)
    assert "Bombus terrestris" not in set(grades["species"])
    for grade in GRADES:
        groups = group_specimens(frame, grades, grade)
        assert all("Bombus terrestris" not in g.species for g in groups)


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

    # The caller passes the snapshot-wide shared set explicitly -- see the
    # BOLD:AAL6477 tests below for why grouping cannot assume the local frame
    # alone tells it which BINs are genuinely shared.
    groups = group_specimens(local, grades, "E",
                             shared_bins=shared_bins(snapshot_bins))
    assert len(groups) == 1
    assert groups[0].note, "a group that looks innocent must explain itself"
    assert "outside this search" in groups[0].note


def test_fetch_bin_fills_in_the_sharing_species_when_given():
    """The point of ``fetch_bin``: don't just say a BIN is shared, show it."""
    local = _frame([
        {"processid": "R1", "species": "Danaus plexippus", "bin_uri": "BOLD:X",
         "quality_score": 5},
    ])
    snapshot_bins = pd.DataFrame([
        {"bin_uri": "BOLD:X", "species": "Danaus plexippus"},
        {"bin_uri": "BOLD:X", "species": "Danaus chrysippus"},
    ])
    grades = calculate_bags_grades(local, bin_species=snapshot_bins)

    extra = _frame([
        {"processid": "R2", "species": "Danaus chrysippus", "bin_uri": "BOLD:X",
         "quality_score": 6},
    ])
    fetched: list[str] = []

    def fetch_bin(bin_uri):
        fetched.append(bin_uri)
        return extra

    groups = group_specimens(local, grades, "E",
                             shared_bins=shared_bins(snapshot_bins),
                             fetch_bin=fetch_bin)
    assert fetched == ["BOLD:X"]
    group = groups[0]
    assert set(group.specimens["processid"]) == {"R1", "R2"}
    assert group.species == ("Danaus chrysippus", "Danaus plexippus")
    assert "2 species" in group.caption
    assert "outside this search" in group.note


# -- a species' OTHER bin must not borrow its grade-E status ----------------


def test_a_species_own_unshared_bin_gets_no_group():
    """The ``BOLD:AAL6477`` bug.

    A species graded E because ONE of its BINs is shared must not turn its
    OTHER, unrelated BIN into a "Shared BIN" group -- that BIN never held more
    than one species, locally or in the snapshot, and grouping it under grade E
    only because the species also happens to sit in a shared BIN elsewhere is
    exactly what R never did.
    """
    local = _frame([
        # BOLD:AAG9765 is genuinely shared -- two species.
        {"processid": "S1", "species": "Sialis concava", "bin_uri": "BOLD:AAG9765",
         "quality_score": 5},
        {"processid": "S2", "species": "Sialis other", "bin_uri": "BOLD:AAG9765",
         "quality_score": 5},
        # BOLD:AAL6477 holds only Sialis concava -- never shared.
        {"processid": "S3", "species": "Sialis concava", "bin_uri": "BOLD:AAL6477",
         "quality_score": 5},
        {"processid": "S4", "species": "Sialis concava", "bin_uri": "BOLD:AAL6477",
         "quality_score": 5},
    ])
    grades = calculate_bags_grades(local)
    assert grades.set_index("species").loc["Sialis concava", "bags_grade"] == "E"

    groups = group_specimens(local, grades, "E", shared_bins=shared_bins(local))
    assert [g.bins for g in groups] == [("BOLD:AAG9765",)], (
        "BOLD:AAL6477 has only one species and must not appear as a shared BIN"
    )


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


# -- interim species, genus conflicts and unnamed BINs ------------------------


def _names_frame():
    from boldcurator.core.pipeline import process_specimen_data

    rows = []

    def add(prefix, name, rank, bin_uri, n, genus="Danaus", ident=None):
        for i in range(n):
            rows.append({"processid": f"{prefix}{i}", "species": name,
                         "identification": ident or name or genus,
                         "identification_rank": rank, "genus": genus,
                         "family": "Nymphalidae", "bin_uri": bin_uri,
                         "quality_score": i, "country.ocean": "Kenya"})

    add("PLX", "Danaus plexippus", "species", "BOLD:A", 3)
    add("CFA", "Danaus cf. plexippus", "species", "BOLD:A", 2)  # interim, same BIN
    add("CHR", "Danaus chrysippus", "species", "BOLD:C", 4)
    add("GNC", None, "genus", "BOLD:C", 1)                      # same genus: no effect
    add("ERI", "Danaus eresimus", "species", "BOLD:D", 4)
    add("PIE", None, "genus", "BOLD:D", 1, genus="Pieris")      # other genus: E
    add("SP1", "Danaus sp. 1", "species", "BOLD:F", 12)         # interim, graded
    add("GSP", "Danaus sp.", "genus", "BOLD:G", 2)              # unnamed, concordant
    add("MIX", None, "genus", "BOLD:H", 1)                      # unnamed, discordant
    add("MXP", None, "genus", "BOLD:H", 1, genus="Pieris")
    add("NOB", "Danaus cf. plexippus", "species", None, 1)      # no BIN
    return process_specimen_data(pd.DataFrame(rows))


def _grades(frame):
    grades = calculate_bags_grades(frame)
    return dict(zip(grades["species"], grades["bags_grade"]))


def test_an_interim_species_in_a_species_bin_makes_it_grade_e():
    """Strict: D. cf. plexippus is a name of its own."""
    grades = _grades(_names_frame())
    assert grades["Danaus plexippus"] == "E"
    assert grades["Danaus cf. plexippus"] == "E"


def test_a_different_genus_makes_a_bin_grade_e_and_the_same_genus_does_not():
    grades = _grades(_names_frame())
    assert grades["Danaus eresimus"] == "E"
    assert grades["Danaus chrysippus"] == "B"


def test_an_interim_species_gets_its_own_grade():
    assert _grades(_names_frame())["Danaus sp. 1"] == "A"


def test_a_genus_rank_name_is_not_graded():
    assert "Danaus sp." not in _grades(_names_frame())


def test_a_genus_conflict_e_group_says_so():
    frame = _names_frame()
    groups = {g.bins[0]: g for g in group_specimens(frame, calculate_bags_grades(frame), "E")}
    assert set(groups) == {"BOLD:A", "BOLD:D"}
    assert groups["BOLD:A"].caption == ("Shared BIN: BOLD:A (2 species) — "
                                        "Danaus cf. plexippus, Danaus plexippus")
    d = groups["BOLD:D"]
    assert d.caption == "Shared BIN: BOLD:D (1 species, 2 genera) — Danaus eresimus"
    assert d.note == "records from more than one genus: Danaus, Pieris"
    assert set(d.specimens["processid"]) == {"ERI0", "ERI1", "ERI2", "ERI3", "PIE0"}


def test_unnamed_bins_get_groups_discordant_first():
    """Every BIN with no species-level record, so none is invisible."""
    groups = group_specimens(_names_frame(), pd.DataFrame(), UNNAMED)

    assert [g.bins[0] for g in groups] == ["BOLD:H", "BOLD:G"]
    h, g = groups
    assert h.discordant and not g.discordant
    assert h.caption == "Discordant BIN: BOLD:H — Danaus, Pieris"
    assert h.note == "records from more than one genus: Danaus, Pieris"
    assert g.caption == "Concordant BIN: BOLD:G — Danaus sp."
    assert set(g.specimens["processid"]) == {"GSP0", "GSP1"}
    assert list(g.specimens["processid"]) == ["GSP1", "GSP0"], "best score first"


def test_unnamed_screen_is_not_a_bags_grade():
    assert UNNAMED not in GRADES


def test_an_unnamed_caption_summarises_many_names():
    frame = pd.DataFrame([
        {"processid": f"P{i}", "species": None, "bin_uri": "BOLD:X",
         "identification_rank": "genus", "genus": "Danaus",
         "identification": f"Danaus {chr(97 + i)}group"} for i in range(5)])
    (group,) = group_specimens(frame, pd.DataFrame(), UNNAMED)
    assert group.caption.endswith("+2 more")


# -- C+E: split and shared ------------------------------------------------------


def _split_shared_frame():
    from boldcurator.core.pipeline import process_specimen_data

    rows = []

    def add(prefix, name, rank, bin_uri, n, genus="Danaus"):
        for i in range(n):
            rows.append({"processid": f"{prefix}{i}", "species": name,
                         "identification": name or genus,
                         "identification_rank": rank, "genus": genus,
                         "family": "Nymphalidae", "bin_uri": bin_uri,
                         "quality_score": i, "country.ocean": "Kenya"})

    add("PLA", "Danaus plexippus", "species", "BOLD:A", 3)   # shared
    add("CHA", "Danaus chrysippus", "species", "BOLD:A", 2)
    add("PLB", "Danaus plexippus", "species", "BOLD:B", 4)   # own
    add("GNB", None, "genus", "BOLD:B", 1)                   # rides along
    add("PLC", "Danaus plexippus", "species", "BOLD:C", 1)   # mixed genera
    add("PIC", None, "genus", "BOLD:C", 1, genus="Pieris")
    add("VAA", "Vanessa atalanta", "species", "BOLD:V", 2, genus="Vanessa")  # C only
    add("VAB", "Vanessa atalanta", "species", "BOLD:W", 2, genus="Vanessa")
    return process_specimen_data(pd.DataFrame(rows))


def test_c_plus_e_groups_every_bin_of_a_split_and_shared_species():
    """Graded E, so the C screen never showed it and E only its shared BIN."""
    frame = _split_shared_frame()
    grades = calculate_bags_grades(frame)
    (group,) = group_specimens(frame, grades, SPLIT_SHARED)

    assert group.caption == ("Species: Danaus plexippus — BOLD:A (shared with "
                             "Danaus chrysippus), BOLD:B (own), BOLD:C (mixed genera)")
    assert group.bins == ("BOLD:A", "BOLD:B", "BOLD:C")
    assert set(group.specimens["processid"]) == {
        "PLA0", "PLA1", "PLA2", "PLB0", "PLB1", "PLB2", "PLB3", "PLC0",
        "CHA0", "CHA1", "GNB0", "PIC0"}, \
        "its own records in every BIN, the sharing species and the riders"
    assert group.note.startswith("Split across 3 BINs, 2 of them shared")
    assert list(group.specimens["processid"]) == [
        "PLA2", "PLA1", "PLA0", "PLB3", "PLB2", "PLB1", "PLB0", "PLC0",
        "CHA1", "CHA0", "GNB0", "PIC0"], \
        "own records BIN by BIN, best first; then the sharing species; riders last"


def test_a_split_species_with_no_shared_bin_is_not_c_plus_e():
    frame = _split_shared_frame()
    grades = calculate_bags_grades(frame)
    assert dict(zip(grades["species"], grades["bags_grade"]))["Vanessa atalanta"] == "C"
    assert all(g.species != ("Vanessa atalanta",)
               for g in group_specimens(frame, grades, SPLIT_SHARED))


def test_c_plus_e_puts_the_species_with_most_bins_first():
    frame = _split_shared_frame()
    extra = frame[frame["processid"].isin(["CHA0", "CHA1"])].copy()
    extra["processid"] = ["CHX0", "CHX1"]
    extra["bin_uri"] = "BOLD:X"                     # chrysippus: 2 BINs, 1 shared
    frame = pd.concat([frame, extra], ignore_index=True)
    grades = calculate_bags_grades(frame)
    groups = group_specimens(frame, grades, SPLIT_SHARED)
    assert [g.species[0] for g in groups] == ["Danaus plexippus", "Danaus chrysippus"]


def test_e_groups_mark_their_c_plus_e_species():
    frame = _split_shared_frame()
    grades = calculate_bags_grades(frame)
    groups = {g.bins[0]: g for g in group_specimens(frame, grades, "E")}
    assert groups["BOLD:A"].caption == ("Shared BIN: BOLD:A (2 species) — "
                                        "Danaus plexippus [C+E], Danaus chrysippus")
    assert "see BAGS C+E" in groups["BOLD:A"].note


def test_the_checklist_marks_c_plus_e_species():
    frame = _split_shared_frame()
    grades = calculate_bags_grades(frame)
    checklist = build_species_checklist(frame, grades).set_index("species")
    assert checklist.loc["Danaus plexippus", "c_plus_e"] == "C+E"
    assert checklist.loc["Danaus chrysippus", "c_plus_e"] == ""
    assert checklist.loc["Vanessa atalanta", "c_plus_e"] == ""


def test_c_plus_e_is_not_a_bags_grade():
    assert SPLIT_SHARED not in GRADES


def _sialis_frame(extra_species=()):
    """Round 8, items 7.1 and 8.1, from a curator's export: Sialis concava
    shares BOLD:AAG9765 with Sialis velata (and 48 genus-level records) and
    has BOLD:AAL6477 to itself -- 68 records in its two BINs."""
    from boldcurator.core.pipeline import process_specimen_data

    rows = []

    def add(prefix, name, rank, bin_uri, n):
        for i in range(n):
            rows.append({"processid": f"{prefix}{i}", "species": name,
                         "identification": name or "Sialis",
                         "identification_rank": rank, "genus": "Sialis",
                         "family": "Sialidae", "order": "Megaloptera",
                         "bin_uri": bin_uri, "quality_score": i,
                         "country.ocean": "Canada"})

    add("GEN", None, "genus", "BOLD:AAG9765", 48)
    add("CON", "Sialis concava", "species", "BOLD:AAG9765", 1)
    add("VEL", "Sialis velata", "species", "BOLD:AAG9765", 15)
    add("VNB", "Sialis velata", "species", None, 8)            # no BIN
    add("COL", "Sialis concava", "species", "BOLD:AAL6477", 4)
    for n, name in enumerate(extra_species):
        add(f"X{n}_", name, "species", "BOLD:AAG9765", 1)
    return process_specimen_data(pd.DataFrame(rows))


def test_c_plus_e_shows_the_species_sharing_its_bin():
    frame = _sialis_frame()
    grades = calculate_bags_grades(frame)
    (group,) = group_specimens(frame, grades, SPLIT_SHARED)
    assert group.caption == ("Species: Sialis concava — BOLD:AAG9765 (shared with "
                             "Sialis velata), BOLD:AAL6477 (own)")
    assert group.specimen_count == 68, "every record in both BINs"
    pids = list(group.specimens["processid"])
    assert sum(p.startswith("VEL") for p in pids) == 15
    assert not any(p.startswith("VNB") for p in pids), "BIN-less velata stays out"
    first_other = min(i for i, p in enumerate(pids) if not p.startswith("CO"))
    assert all(p.startswith("CO") for p in pids[:first_other]) and first_other == 5, \
        "concava's own five records first"


def test_e_caption_names_every_species_in_the_shared_bin():
    frame = _sialis_frame()
    grades = calculate_bags_grades(frame)
    (group,) = group_specimens(frame, grades, "E")
    assert group.caption == ("Shared BIN: BOLD:AAG9765 (2 species) — "
                             "Sialis concava [C+E], Sialis velata")
    assert group.specimen_count == 64


def test_a_long_e_caption_keeps_the_c_plus_e_marker():
    frame = _sialis_frame(["Sialis annae", "Sialis bilobata", "Sialis aequata"])
    grades = calculate_bags_grades(frame)
    (e,) = group_specimens(frame, grades, "E")
    assert e.caption.startswith("Shared BIN: BOLD:AAG9765 (5 species) — "
                                "Sialis concava [C+E], ")
    assert e.caption.endswith("+2 more")
