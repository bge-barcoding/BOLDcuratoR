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
    ],
)
def test_unified_species_rule(name, valid):
    assert bool(sp.is_valid_species_name(pd.Series([name])).iloc[0]) is valid


def test_danaus_sp_is_invalid_here_unlike_r():
    """The documented divergence, pinned so it cannot regress silently.

    R's destructive pass anchors the pattern as ``^sp\\.``, so "Danaus sp."
    survives it and BAGS then counts it as a species in its own right.
    """
    assert not sp.is_valid_species_name(pd.Series(["Danaus sp."])).iloc[0]


def test_normalise_blanks_invalid_and_missing_tokens():
    out = sp.normalise_species(
        pd.Series(["  Pieris rapae  ", "Danaus sp.", "None", "NA", ""])
    )
    assert list(out.isna()) == [False, True, True, True, True]
    assert out.iloc[0] == "Pieris rapae"


def test_species_level_requires_binomial_and_rank():
    frame = pd.DataFrame(
        {
            "species": ["Danaus plexippus", "Danaus plexippus", "Danaus", "Danaus sp."],
            "identification_rank": ["species", "genus", "species", "species"],
        }
    )
    assert list(sp.is_species_level(frame)) == [True, False, False, False]


def test_species_level_tolerates_missing_rank_column():
    frame = pd.DataFrame({"species": ["Danaus plexippus", "Danaus"]})
    assert list(sp.is_species_level(frame)) == [True, False]


def test_to_text_renders_whole_floats_without_decimal():
    assert list(sp.to_text(pd.Series([658.0, float("nan")]))) == ["658", ""]
