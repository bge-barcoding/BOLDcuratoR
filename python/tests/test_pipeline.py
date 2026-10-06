import pytest

from boldcurator.core import pipeline as P


def test_taxa_parsing_keeps_synonym_groups():
    groups = P.parse_taxa_input(
        "Danaus plexippus, Danaus archippus\n\n  Nymphalidae  \n"
    )
    assert groups == [["Danaus plexippus", "Danaus archippus"], ["Nymphalidae"]]
    assert P.flatten_taxa(groups) == [
        "Danaus plexippus", "Danaus archippus", "Nymphalidae"
    ]


def test_geographic_filter_is_a_union_not_an_intersection():
    geo = P.geographic_filter(["Canada"], ["Europe"])
    assert "Canada" in geo
    assert "France" in geo


def test_continent_expansion_uses_exact_country_strings():
    north = P.countries_for_continents(["North America"])
    assert "United States" in north
    assert "United States of America" not in north


def test_process_keeps_species_verbatim_classifies_it_and_dedupes(fixture_snapshot):
    """The species field is shown and exported as BOLD has it; name_status
    says what kind of name it is."""
    import pandas as pd

    frame = pd.DataFrame([
        {"processid": "P2", "species": "Danaus sp.", "bin_uri": " BOLD:A ",
         "identification_rank": "genus"},
        {"processid": "P3", "species": "Danaus cf. plexippus", "bin_uri": "BOLD:A",
         "identification_rank": "species"},
        {"processid": "P4", "species": "None", "bin_uri": "BOLD:A",
         "identification_rank": ""},
        {"processid": "P1", "species": "  Pieris rapae ", "bin_uri": "",
         "identification_rank": "species"},
        {"processid": "P1", "species": "Pieris rapae", "bin_uri": "BOLD:B",
         "identification_rank": "species"},
    ])
    out = P.process_specimen_data(frame).set_index("processid")
    assert list(out.index) == ["P1", "P2", "P3", "P4"]      # deduped and sorted
    assert out.loc["P2", "species"] == "Danaus sp."
    assert out.loc["P3", "species"] == "Danaus cf. plexippus"
    assert out.loc["P1", "species"] == "Pieris rapae"         # trimmed
    assert pd.isna(out.loc["P4", "species"])                  # a missing token
    assert list(out["name_status"]) == [
        "species", "higher rank", "interim species", "unidentified"]
    assert pd.isna(out.loc["P1", "bin_uri"])
    assert "data_source" in out.columns and "import_date" in out.columns


def test_end_to_end_search(store):
    result = P.run_search(store, taxa_text="Danaus plexippus", continents=["Europe"])
    summary = result.summary()
    assert summary["records"] > 0
    assert summary["bins"] > 0
    assert summary["snapshot_id"]
    assert {"quality_score", "rank", "criteria_met", "bags_grade"} <= set(
        result.specimens.columns
    )
    assert result.specimens["quality_score"].max() <= 15
    assert set(result.specimens["rank"].unique()) <= set(range(1, 8))
    assert result.bin_analysis["summary"]["total_bins"] > 0
    assert result.selections


def test_shared_bins_are_bins_the_result_actually_holds(store):
    """core.grouping needs this set to tell a species' shared BIN from its

    unrelated other one -- see the BOLD:AAL6477 tests in test_grouping.py.
    """
    result = P.run_search(store, taxa_text="Lepidoptera")
    assert isinstance(result.shared_bins, frozenset)
    assert result.shared_bins, "a family this size should have some sharing"
    present = set(result.specimens["bin_uri"].dropna().astype(str))
    assert result.shared_bins <= present


def test_unmatched_taxa_produce_a_warning_not_a_failure(store):
    result = P.run_search(store, taxa_text="Danaus plexippus\nNotataxonatall")
    assert any("Notataxonatall" in w for w in result.warnings)
    assert result.record_count > 0


def test_search_with_nothing_resolvable_raises(store):
    with pytest.raises(ValueError):
        P.run_search(store, taxa_text="Notataxonatall")


def test_size_limit_can_be_enforced_before_materialising(store, monkeypatch):
    monkeypatch.setitem(P.DOWNLOAD_LIMITS, "MAX_RECORDS", 1)
    with pytest.raises(P.SizeLimitExceeded) as exc:
        P.run_search(store, taxa_text="Nymphalidae")
    assert exc.value.estimate["expanded_records"] > 1


def test_a_search_keeps_species_verbatim_and_grades_only_species_level_records(store):
    """The fixture's genus-rank "Genus sp." records keep that species value,
    say they are higher rank, carry no grade, and export as they are."""
    import pandas as pd

    result = P.run_search(store, taxa_text="Danaus")
    spec = result.specimens
    higher = spec[spec["name_status"] == "higher rank"]
    assert len(higher) and (higher["species"] == "Danaus sp.").all()
    assert higher["bags_grade"].isna().all()
    interim = spec[spec["name_status"] == "interim species"]
    assert len(interim) and interim["species"].str.contains("cf.", regex=False).all()
    assert interim["bags_grade"].notna().all()

    from boldcurator.io import exports
    path = exports.write_csv(spec, store.path.parent / "verbatim.csv")
    exported = pd.read_csv(path, comment="#", dtype=str)
    assert "name_status" in exported.columns
    assert "Danaus sp." in set(exported["species"])
