import pandas as pd

from boldcurator.config.constants import MAX_QUALITY_SCORE, SPECIMEN_SCORING_CRITERIA
from boldcurator.core.scoring import criterion_flags, score_specimens


def _row(**kwargs) -> pd.DataFrame:
    return pd.DataFrame([kwargs])


def _passes(name: str, **fields) -> bool:
    return bool(criterion_flags(_row(**fields))[name].iloc[0])


def test_has_image_is_gone_and_max_score_is_15():
    names = [c.name for c in SPECIMEN_SCORING_CRITERIA]
    assert "HAS_IMAGE" not in names
    assert MAX_QUALITY_SCORE == 15 == len(names)


def test_species_id():
    assert _passes("SPECIES_ID", species="Danaus plexippus")
    assert not _passes("SPECIES_ID", species="Danaus sp.")
    assert not _passes("SPECIES_ID", species="")


def test_seq_quality_needs_bin_and_basecount_and_length():
    assert _passes("SEQ_QUALITY", bin_uri="BOLD:AAA1", nuc_basecount="658")
    assert not _passes("SEQ_QUALITY", bin_uri="", nuc_basecount="658")
    assert not _passes("SEQ_QUALITY", bin_uri="BOLD:AAA1", nuc_basecount="499")
    assert not _passes("SEQ_QUALITY", bin_uri="BOLD:AAA1", nuc_basecount="")
    assert _passes("SEQ_QUALITY", bin_uri="BOLD:AAA1", nuc_basecount=658.0)


def test_type_specimen_voucher_shortcut_and_note_patterns():
    # voucher_type containing "type" wins outright
    assert _passes("TYPE_SPECIMEN", voucher_type="Type material")
    assert _passes("TYPE_SPECIMEN", notes="paratype of Smith 1998")
    assert not _passes("TYPE_SPECIMEN", notes="ordinary specimen")


def test_public_voucher_positive_wins_over_negative():
    """"not registered" hits the positive ``registered`` first, so it passes.

    Preserved from R deliberately; it looks wrong but it is the shipped rule.
    """
    assert _passes("PUBLIC_VOUCHER", voucher_type="not registered")
    assert _passes("PUBLIC_VOUCHER", voucher_type="Museum Voucher")
    assert not _passes("PUBLIC_VOUCHER", voucher_type="DNA extract")
    assert not _passes("PUBLIC_VOUCHER", voucher_type="")


def test_identifier_and_id_method_negatives():
    assert _passes("IDENTIFIER", identified_by="A. Smith")
    assert not _passes("IDENTIFIER", identified_by="Kate Perez")
    assert not _passes("IDENTIFIER", identified_by="BOLD")
    assert _passes("ID_METHOD", identification_method="Morphology")
    assert not _passes("ID_METHOD", identification_method="BOLD ID Engine")
    assert not _passes("ID_METHOD", identification_method="None")


def test_institution_negative_pattern():
    assert _passes("INSTITUTION", inst="Natural History Museum")
    assert not _passes("INSTITUTION", inst="Personal collection")
    assert not _passes("INSTITUTION", inst="unknown")


def test_missing_column_is_treated_as_missing_not_an_error():
    flags = criterion_flags(pd.DataFrame([{"species": "Danaus plexippus"}]))
    assert flags["SPECIES_ID"].iloc[0]
    assert not flags["COUNTRY"].iloc[0]


def test_criteria_met_is_declaration_ordered_and_score_counts_it():
    out = score_specimens(_row(
        species="Danaus plexippus",
        bin_uri="BOLD:AAA1",
        nuc_basecount="658",
        country=None,
        site="Pen Ponds",
    ))
    met = out["criteria_met"].iloc[0].split("; ")
    assert met == ["SPECIES_ID", "SEQ_QUALITY", "SITE"]
    assert out["quality_score"].iloc[0] == 3


def test_empty_frame_scores_without_error():
    out = score_specimens(pd.DataFrame(columns=["species"]))
    assert len(out) == 0
