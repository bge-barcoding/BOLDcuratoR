import pandas as pd
import pytest

from boldcurator.core import species as sp


@pytest.mark.parametrize(
    "value,expected",
    [
        ("Danaus plexippus", False),
        ("", True),
        ("   ", True),
        ("None", True),
        ("none", True),
        ("NA", True),
        ("na", True),
        (None, True),
    ],
)
def test_is_empty_matches_r_is_empty_value(value, expected):
    assert bool(sp.is_empty(pd.Series([value])).iloc[0]) is expected


@pytest.mark.parametrize(
    "name,valid",
    [
        ("Danaus plexippus", True),
        ("Pieris rapae", True),
        ("Danaus sp.", False),
        ("Danaus spp.", False),
        ("Danaus cf. plexippus", False),
        ("Danaus aff. plexippus", False),
        ("Apis nr mellifera", False),
        ("Vanessa atalanta 2", False),   # digits
        ("sp", False),
        ("", False),
        ("None", False),
        # Interim names R's pattern let through (no full stop, other
        # qualifiers), all of which BOLD can record at species rank.
        ("Danaus cf plexippus", False),
        ("Danaus Cf. plexippus", False),
        ("Danaus cf.plexippus", False),
        ("Danaus aff plexippus", False),
        ("Danaus nr. plexippus", False),
        ("Danaus sp", False),
        ("Danaus spp", False),
        ("Danaus n.sp.", False),
        ("Danaus gr. plexippus", False),
        ("Danaus plexippus grp", False),
        ("Danaus plexippus agg.", False),
        ("Danaus ?plexippus", False),
        ("Danaus plexippus complex", False),
        ("Danaus indet.", False),
        # Whole words only: real names that merely contain the letters.
        ("Danaus affinis", True),
        ("Grapholita spectrana", True),
        ("Spodoptera exigua", True),
        ("Agriphila straminella", True),
        # "ssp." is a subspecies, not "sp.": R's pattern wrongly caught it.
        ("Danaus plexippus ssp. plexippus", True),
    ],
)
def test_unified_species_rule(name, valid):
    assert bool(sp.is_valid_species_name(pd.Series([name])).iloc[0]) is valid


def test_scoring_uses_the_same_rule():
    """SPECIES_ID and the species rule used to be two copies of one regex."""
    from boldcurator.config.constants import (
        INVALID_SPECIES_PATTERN,
        SPECIMEN_SCORING_CRITERIA,
    )

    species_id = next(c for c in SPECIMEN_SCORING_CRITERIA if c.name == "SPECIES_ID")
    assert species_id.negative_pattern == INVALID_SPECIES_PATTERN == sp.INVALID_SPECIES_PATTERN


def test_danaus_sp_is_invalid_here_unlike_r():
    """The documented divergence, pinned so it cannot regress silently.

    R's destructive pass anchors the pattern as ``^sp\\.``, so "Danaus sp."
    survives it and BAGS then counts it as a species in its own right.
    """
    assert not sp.is_valid_species_name(pd.Series(["Danaus sp."])).iloc[0]


def test_normalise_keeps_invalid_names_and_blanks_only_missing_tokens():
    """The original value is what a curator sees; nothing but the missing
    tokens is blanked."""
    out = sp.normalise_species(
        pd.Series(["  Pieris rapae  ", "Danaus sp.", "None", "NA", ""])
    )
    assert list(out.isna()) == [False, False, True, True, True]
    assert list(out[:2]) == ["Pieris rapae", "Danaus sp."]


@pytest.mark.parametrize("name,rank,genus,status", [
    ("Danaus plexippus", "species", "", sp.NAME_SPECIES),
    ("Danaus plexippus", "subspecies", "", sp.NAME_SPECIES),
    ("Danaus plexippus", "", "", sp.NAME_SPECIES),          # rank not recorded
    ("Danaus plexippus", "genus", "", sp.NAME_HIGHER),      # rank says genus
    ("Danaus cf. plexippus", "species", "", sp.NAME_INTERIM),
    ("Danaus cf plexippus", "Species", "", sp.NAME_INTERIM),
    ("Danaus sp. 1", "species", "", sp.NAME_INTERIM),
    ("Danaus sp.", "species", "", sp.NAME_INTERIM),         # species rank: interim
    ("Danaus sp.", "genus", "", sp.NAME_HIGHER),            # genus rank: higher
    ("Danaus sp. 1", "", "", sp.NAME_HIGHER),               # interim needs the rank
    ("Danaus", "species", "", sp.NAME_HIGHER),              # one word
    ("", "genus", "Danaus", sp.NAME_HIGHER),
    ("", "", "", sp.NAME_NONE),
])
def test_name_status(name, rank, genus, status):
    frame = pd.DataFrame({"species": [name], "identification_rank": [rank],
                          "genus": [genus]})
    assert sp.name_status(frame).iloc[0] == status


def test_species_level_is_species_or_interim_species():
    frame = pd.DataFrame(
        {
            "species": ["Danaus plexippus", "Danaus plexippus", "Danaus",
                        "Danaus sp.", "Danaus cf. plexippus"],
            "identification_rank": ["species", "genus", "species", "genus", "species"],
        }
    )
    assert list(sp.is_species_level(frame)) == [True, False, False, False, True]


def test_species_level_tolerates_missing_rank_column():
    frame = pd.DataFrame({"species": ["Danaus plexippus", "Danaus"]})
    assert list(sp.is_species_level(frame)) == [True, False]


def test_to_text_renders_whole_floats_without_decimal():
    assert list(sp.to_text(pd.Series([658.0, float("nan")]))) == ["658", ""]
