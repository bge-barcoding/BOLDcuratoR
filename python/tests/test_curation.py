"""Regrading BAGS with the curation applied (``core.curation``)."""

from __future__ import annotations

import pandas as pd

from boldcurator.core.curation import (
    CURATED,
    ORIGINAL,
    UNALTERED,
    curate_frame,
    patch_bin_species,
    regrade,
)
from boldcurator.core.pipeline import Resolution, SearchResult, grade_frame
from boldcurator.core.species import name_status
from boldcurator.io.annotations import Annotations


def _records(name: str, bin_uri: str, n: int, start: int) -> list[dict]:
    genus = name.split()[0]
    return [{"processid": f"P{start + i}", "species": name, "identification": name,
             "identification_rank": "species", "bin_uri": bin_uri,
             "genus": genus, "family": "Baetidae", "order": "Ephemeroptera"}
            for i in range(n)]


def _result(frame: pd.DataFrame, bin_species: pd.DataFrame | None = None) -> SearchResult:
    frame = frame.copy()
    frame["name_status"] = name_status(frame)
    graded, grades, shared = grade_frame(frame, bin_species)
    return SearchResult(specimens=graded, bags_grades=grades, bin_analysis={},
                        selections={}, resolution=Resolution(), estimate={},
                        shared_bins=frozenset(shared), bin_species=bin_species)


def _grades(result: SearchResult) -> dict[str, str]:
    return dict(zip(result.bags_grades["species"], result.bags_grades["bags_grade"]))


#: Aus bus (5) and Aus cus (3) share BIN1: both grade E.
SHARED = pd.DataFrame(_records("Aus bus", "BIN1", 5, 0)
                      + _records("Aus cus", "BIN1", 3, 5))


def _misidentify(a: Annotations, pids, new_id: str, rank: str = "species") -> None:
    for pid in pids:
        a.set_flag(pid, "misidentification")
        a.set_updated_id(pid, new_id, rank=rank, method="Morphology")


def test_the_original_view_is_the_analysis_itself():
    result = _result(SHARED)
    assert regrade(result, Annotations(), ORIGINAL) is result


def test_a_misidentification_moves_records_and_the_grades_follow():
    result = _result(SHARED)
    assert _grades(result) == {"Aus bus": "E", "Aus cus": "E"}
    a = Annotations()
    _misidentify(a, ["P5", "P6", "P7"], "Aus bus")
    curated = regrade(result, a, CURATED)
    assert _grades(curated) == {"Aus bus": "B"}            # 8 records, 1 BIN
    assert curated.curation == {"view": CURATED, "changed": 3, "left_out": 0}
    assert _grades(result) == {"Aus bus": "E", "Aus cus": "E"}   # untouched


def test_a_revised_id_at_genus_rank_is_no_longer_species_level():
    a = Annotations()
    _misidentify(a, ["P5", "P6", "P7"], "Aus", rank="genus")
    frame, changed, _ = curate_frame(_result(SHARED).specimens, a, CURATED)
    revised = frame[frame["processid"].isin(["P5", "P6", "P7"])]
    assert changed == 3
    assert revised["species"].isna().all()
    assert (revised["name_status"] == "higher rank").all()
    assert _grades(regrade(_result(SHARED), a, CURATED)) == {"Aus bus": "B"}


def test_a_new_genus_is_seen_by_the_genus_conflict_test():
    a = Annotations()
    _misidentify(a, ["P5"], "Cus dus")
    frame, _, _ = curate_frame(_result(SHARED).specimens, a, CURATED)
    assert frame.loc[frame["processid"] == "P5", "genus"].iloc[0] == "Cus"


def test_a_taxon_update_renames_the_whole_species():
    frame = pd.DataFrame(_records("Aus bis", "BIN1", 4, 0)
                         + _records("Aus bus", "BIN2", 4, 4))
    a = Annotations()
    a.set_taxon_update("Aus bis", "synonym", "Aus bus", rank="species")
    curated = regrade(_result(frame), a, CURATED)
    assert _grades(curated) == {"Aus bus": "C"}            # one species, two BINs


def test_contaminated_records_are_left_out_of_grading():
    a = Annotations()
    for pid in ("P5", "P6", "P7"):
        a.set_flag(pid, contamination=True, sequence_taxon="Homo sapiens")
    curated = regrade(_result(SHARED), a, CURATED)
    assert _grades(curated) == {"Aus bus": "B"}
    assert curated.curation["left_out"] == 3


def test_unaltered_leaves_out_corrected_flagged_and_renamed_records_only():
    frame = pd.DataFrame(_records("Aus bus", "BIN1", 5, 0)
                         + _records("Aus cus", "BIN1", 3, 5)
                         + _records("Aus dus", "BIN2", 3, 8))
    a = Annotations()
    _misidentify(a, ["P5"], "Aus bus")
    a.set_flag("P6", contamination=True, sequence_taxon="Homo sapiens")
    a.set_flag("P0", other_issue=True)
    a.set_note("P1", "fine")
    a.set_taxon_update("Aus dus", "name_issue", "Aus duus", rank="species")
    viewed, changed, left_out = curate_frame(_result(frame).specimens, a, UNALTERED)
    kept = set(viewed["processid"])
    assert left_out == 5 and changed == 0
    assert {"P0", "P1", "P7"} <= kept
    assert not {"P5", "P6", "P8", "P9", "P10"} & kept


def test_a_corrected_name_stops_counting_against_the_bin_snapshot_wide():
    """The original grades judge shared BINs over the snapshot's
    bin_species; the regrade must take the corrected records' old names
    out of it, or the BIN stays shared."""
    bin_species = pd.DataFrame({
        "bin_uri": ["BIN1", "BIN1"], "species": ["Aus bus", "Aus cus"],
        "identification_rank": ["species", "species"], "n_records": [5, 3]})
    result = _result(SHARED, bin_species)
    a = Annotations()
    _misidentify(a, ["P5", "P6", "P7"], "Aus bus")
    curated = regrade(result, a, CURATED)
    assert curated.bin_species.to_dict("records") == [
        {"bin_uri": "BIN1", "species": "Aus bus", "identification_rank": "species",
         "n_records": 8}]
    assert _grades(curated) == {"Aus bus": "B"}


def test_patching_without_a_snapshot_table_is_a_no_op():
    assert patch_bin_species(None, SHARED, SHARED) is None
