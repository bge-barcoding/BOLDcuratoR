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


def test_process_blanks_invalid_species_and_dedupes(fixture_snapshot):
    import pandas as pd

    frame = pd.DataFrame([
        {"processid": "P2", "species": "Danaus sp.", "bin_uri": " BOLD:A "},
        {"processid": "P1", "species": "  Pieris rapae ", "bin_uri": ""},
        {"processid": "P1", "species": "Pieris rapae", "bin_uri": "BOLD:B"},
    ])
    out = P.process_specimen_data(frame)
    assert list(out["processid"]) == ["P1", "P2"]           # deduped and sorted
    assert out.loc[out["processid"] == "P2", "species"].isna().all()
    assert out.loc[out["processid"] == "P1", "bin_uri"].isna().all()
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
