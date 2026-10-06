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


def test_bin_content_has_no_bin_coverage_column():
    frame = _f([{"processid": "P1", "bin_uri": "BOLD:A", "species": "Danaus plexippus"}])
    assert "bin_coverage" not in analyse_bins(frame)["content"].columns


def test_a_bin_with_no_species_name_still_gets_a_representative():
    """Records identified only to genus or family must not take their BIN off
    the Phylogeny tab and "Download Selected"."""
    frame = _f([
        {"processid": "P1", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 5},
        {"processid": "P2", "bin_uri": "BOLD:B", "species": None, "genus": "Danaus",
         "country.ocean": "France", "quality_score": 3},
        {"processid": "P3", "bin_uri": None, "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 9},
    ])
    assert set(auto_select_best_specimens(frame)) == {"P1", "P2"}


def test_fill_gaps_selects_only_groups_with_no_selected_record():
    frame = _f([
        {"processid": "P1", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 9},
        {"processid": "P2", "bin_uri": "BOLD:A", "species": "Danaus plexippus",
         "country.ocean": "France", "quality_score": 1},
        {"processid": "P3", "bin_uri": "BOLD:B", "species": "Danaus chrysippus",
         "country.ocean": "Kenya", "quality_score": 4},
    ])
    existing = {"P2": {"user": "curator"}}
    chosen = auto_select_best_specimens(frame, existing=existing, fill_gaps=True)
    assert set(chosen) == {"P2", "P3"}, "P1 must not join the curator's P2"
    assert chosen["P2"] == {"user": "curator"}
    assert chosen["P3"]["auto_selected"] is True

    covered = {"P2": {}, "P3": {}}
    assert auto_select_best_specimens(frame, existing=covered, fill_gaps=True) is covered


def _bin_rows(*rows):
    """(species, identification_rank, genus) per record, all in BOLD:A."""
    return _f([{"processid": f"P{i}", "bin_uri": "BOLD:A", "species": sp,
                "identification_rank": rank, "genus": genus, "family": "Nymphalidae"}
               for i, (sp, rank, genus) in enumerate(rows)])


def test_an_interim_species_beside_a_species_is_discordant():
    frame = _bin_rows(("Danaus plexippus", "species", "Danaus"),
                      ("Danaus cf. plexippus", "species", "Danaus"))
    assert not check_taxonomic_concordance(frame)
    content = analyse_bins(frame)["content"].iloc[0]
    assert content["concordance"] == "Discordant"
    assert content["species_list"] == "Danaus cf. plexippus; Danaus plexippus"


def test_a_record_of_another_genus_makes_a_bin_discordant():
    frame = _bin_rows(("Danaus plexippus", "species", "Danaus"),
                      (None, "genus", "Pieris"))
    assert not check_taxonomic_concordance(frame)
    assert analyse_bins(frame)["content"].iloc[0]["concordance"] == "Discordant"


def test_a_genus_rank_record_of_the_same_genus_changes_nothing():
    frame = _bin_rows(("Danaus plexippus", "species", "Danaus"),
                      ("Danaus sp.", "genus", "Danaus"))
    assert check_taxonomic_concordance(frame)
    content = analyse_bins(frame)["content"].iloc[0]
    assert content["concordance"] == "Concordant"
    assert content["species_list"] == "Danaus plexippus"
