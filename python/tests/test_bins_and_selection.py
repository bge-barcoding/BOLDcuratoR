import pandas as pd

from boldcurator.core.bins import analyse_bins, check_taxonomic_concordance
from boldcurator.core.selection import auto_select_best_specimens


def _f(rows):
    return pd.DataFrame(rows)


def test_two_valid_species_in_one_bin_is_discordant():
    assert not check_taxonomic_concordance(
        _f([{"species": "Danaus plexippus"}, {"species": "Danaus chrysippus"}])
    )


def test_one_valid_species_is_concordant_even_with_cf_records():
    assert check_taxonomic_concordance(
        _f([{"species": "Danaus plexippus"}, {"species": "Danaus cf. plexippus"}])
    )


def test_falls_back_through_genus_family_order():
    assert not check_taxonomic_concordance(
        _f([{"species": "Danaus sp.", "genus": "Danaus"},
            {"species": "Vanessa sp.", "genus": "Vanessa"}])
    )
    assert check_taxonomic_concordance(
        _f([{"species": "Danaus sp.", "genus": "Danaus"},
            {"species": "Danaus sp.", "genus": "Danaus"}])
    )
    assert not check_taxonomic_concordance(
        _f([{"species": "", "genus": "", "family": "Nymphalidae"},
            {"species": "", "genus": "", "family": "Pieridae"}])
    )


def test_bin_summary_counts():
    frame = _f(
        [{"bin_uri": "BOLD:A", "species": "Danaus plexippus", "country.ocean": "France"},
         {"bin_uri": "BOLD:A", "species": "Danaus chrysippus", "country.ocean": "Kenya"},
         {"bin_uri": "BOLD:B", "species": "Pieris rapae", "country.ocean": "France"},
         {"bin_uri": "", "species": "Pieris rapae", "country.ocean": "France"}]
    )
    result = analyse_bins(frame)
    assert result["summary"] == {
        "total_bins": 2, "concordant_bins": 1,
        "discordant_bins": 1, "shared_bins": 1,
    }
    content = result["content"].set_index("bin_uri")
    assert content.loc["BOLD:A", "unique_species"] == 2
    assert content.loc["BOLD:A", "countries"] == "France; Kenya"


def test_selection_is_per_bin_and_country_not_per_species():
    frame = _f([
        {"processid": "P1", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 5},
        {"processid": "P2", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 9},
        {"processid": "P3", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "Kenya", "quality_score": 4},
    ])
    chosen = auto_select_best_specimens(frame)
    assert set(chosen) == {"P2", "P3"}
    assert chosen["P2"]["auto_selected"] is True


def test_ties_break_on_ascending_processid():
    frame = _f([
        {"processid": "P9", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 7},
        {"processid": "P1", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 7},
    ])
    assert set(auto_select_best_specimens(frame)) == {"P1"}


def test_missing_country_groups_as_unknown():
    frame = _f([
        {"processid": "P1", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": None, "quality_score": 3},
        {"processid": "P2", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 1},
    ])
    assert set(auto_select_best_specimens(frame)) == {"P1", "P2"}


def test_existing_selections_are_never_overwritten():
    frame = _f([{"processid": "P1", "bin_uri": "BOLD:A",
                 "species": "Danaus plexippus", "country.ocean": "France",
                 "quality_score": 9}])
    existing = {"P2": {"user": "curator"}}
    assert auto_select_best_specimens(frame, existing=existing) == existing
