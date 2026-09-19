import csv
import io
from pathlib import Path

import pandas as pd
import pytest

from boldcurator.io.annotations import Annotations, merge_annotations
from boldcurator.io import exports


@pytest.fixture
def annotations() -> Annotations:
    a = Annotations()
    a.set_selected("P1", user="curator")
    a.set_flag("P2", "misidentification", user="curator")
    a.set_note("P3", "check the voucher", user="curator")
    return a


def _frame() -> pd.DataFrame:
    return pd.DataFrame([
        {"processid": "P1", "sampleid": "S1", "identification": "Danaus plexippus",
         "species": "Danaus plexippus", "inst": "NHM"},
        {"processid": "P2", "sampleid": "S2", "identification": "Danaus chrysippus",
         "species": "Danaus chrysippus", "inst": "NHM"},
        {"processid": "P3", "sampleid": "S3", "identification": None,
         "species": "Pieris rapae", "inst": ""},
    ])


def test_selection_alone_does_not_count_as_annotated(annotations):
    """R's download_annotated reads flags/updated_ids/notes, not selection.

    Auto-selection would otherwise mark every representative as
    curator-annotated.
    """
    assert annotations.annotated_processids() == {"P2", "P3"}
    assert annotations.selected_processids() == {"P1"}


def test_annotation_columns_lead_the_export(annotations):
    merged = merge_annotations(_frame(), annotations)
    assert list(merged.columns[:6]) == list(
        ["selected", "flag", "updated_id", "curator_notes", "flag_user",
         "flag_timestamp"]
    )
    assert list(merged["selected"]) == [True, False, False]
    assert list(merged["flag"]) == ["", "misidentification", ""]
    assert merged.loc[merged["processid"] == "P3", "curator_notes"].iloc[0] == \
        "check the voucher"


def test_institution_column_survives_export(annotations):
    """R whitelists "institution"; the real column is `inst`, so it was dropped."""
    merged = merge_annotations(_frame(), annotations)
    assert "inst" in merged.columns
    assert merged["inst"].iloc[0] == "NHM"


def test_tsv_is_quoted_so_tabs_and_newlines_survive(tmp_path):
    a = Annotations()
    a.set_note("P1", "line one\nline two\tafter a tab")
    merged = merge_annotations(_frame(), a)
    path = exports.write_tsv(merged, tmp_path / "out.tsv", snapshot_id="test")

    # Parse as CSV over the whole stream, not line by line -- a quoted newline
    # is part of one field, so splitlines() would itself break what this is
    # checking.
    text = path.read_text(encoding="utf-8")
    body = "".join(l for l in text.splitlines(keepends=True) if not l.startswith("#"))
    rows = list(csv.reader(io.StringIO(body), delimiter="\t"))
    header, first = rows[0], rows[1]
    assert first[header.index("curator_notes")] == "line one\nline two\tafter a tab"
    assert len(rows) == 4      # header + 3 records, not split by the embedded newline


def test_exports_carry_provenance_and_the_scoring_warning(tmp_path):
    path = exports.write_tsv(_frame(), tmp_path / "p.tsv", snapshot_id="2026-09-11")
    text = path.read_text(encoding="utf-8")
    assert "snapshot: 2026-09-11" in text
    assert "NOT comparable" in text


def test_exports_carry_the_cc_by_sa_attribution(tmp_path):
    """Plan item 0.3: the source data package is CC BY-SA 4.0, which requires

    attribution on redistribution -- an export from this app is exactly that.
    """
    tsv = exports.write_tsv(_frame(), tmp_path / "p.tsv", snapshot_id="x")
    csv_path = exports.write_csv(_frame(), tmp_path / "p.csv", snapshot_id="x")
    for path in (tsv, csv_path):
        text = path.read_text(encoding="utf-8")
        assert "CC BY-SA 4.0" in text
        assert "Barcode of Life Data System" in text


def test_fasta_header_falls_back_through_identification_then_species():
    assert exports.fasta_header(
        pd.Series({"processid": "P1", "identification": "Danaus plexippus"})
    ) == ">P1|Danaus plexippus"
    assert exports.fasta_header(
        pd.Series({"processid": "P2", "identification": None, "species": "Pieris rapae"})
    ) == ">P2|Pieris rapae"
    assert exports.fasta_header(pd.Series({"processid": "P3"})) == ">P3|Unknown"


def test_fasta_writes_streamed_pairs(tmp_path):
    path, n = exports.write_fasta(
        _frame(), [("P1", "ACGT"), ("P2", "TTTT"), ("P3", "")],
        tmp_path / "s.fasta",
    )
    assert n == 2      # the empty sequence is skipped
    assert path.read_text(encoding="utf-8").splitlines() == [
        ">P1|Danaus plexippus", "ACGT", ">P2|Danaus chrysippus", "TTTT"
    ]


def test_curation_report_has_the_narrow_fixed_column_set(annotations):
    report = exports.curation_report(_frame(), annotations)
    assert list(report.columns) == list(exports.CURATION_REPORT_COLUMNS)
    assert set(report["processid"]) == {"P2", "P3"}


def test_export_all_writes_everything_and_says_what_it_skipped(store, tmp_path):
    from boldcurator.core.pipeline import run_search

    result = run_search(store, taxa_text="Danaus plexippus")
    a = Annotations()
    first = str(result.specimens["processid"].iloc[0])
    a.set_flag(first, "id_uncertain", user="tester")

    out = exports.export_all(result, tmp_path, annotations=a, store=store)
    assert {"all", "annotated", "curation_report", "search_results",
            "bin_analysis", "fasta"} <= set(out.written)
    assert "selected" in out.skipped
    for path in out.written.values():
        assert Path(path).exists() and Path(path).stat().st_size > 0


def test_export_all_skips_fasta_without_a_snapshot(store, tmp_path):
    from boldcurator.core.pipeline import run_search

    result = run_search(store, taxa_text="Danaus plexippus")
    out = exports.export_all(result, tmp_path, store=None)
    assert "fasta" in out.skipped
    assert "sequences cannot be fetched" in out.skipped["fasta"]


def test_bin_analysis_workbook_has_three_populated_sheets(store, tmp_path):
    from boldcurator.core.pipeline import run_search

    result = run_search(store, taxa_text="Danaus plexippus")
    path = exports.write_bin_analysis_xlsx(
        result.bin_analysis, tmp_path / "bins.xlsx", snapshot_id="x"
    )
    sheets = pd.read_excel(path, sheet_name=None)
    assert set(sheets) == {"Summary", "Content", "Statistics"}
    # R's version leaves Summary and Statistics empty; both carry data here.
    assert all(len(frame) > 0 for frame in sheets.values())
    summary_text = sheets["Summary"].to_string()
    assert "CC BY-SA 4.0" in summary_text


def test_unknown_flag_is_refused():
    a = Annotations()
    with pytest.raises(ValueError, match="Unknown flag"):
        a.set_flag("P1", "not_a_real_flag")


def test_auto_selections_reach_the_selected_export(store, tmp_path):
    """R writes auto-selections into the same store download_selected reads."""
    from boldcurator.core.pipeline import run_search

    result = run_search(store, taxa_text="Danaus plexippus")
    assert result.selections, "pipeline should have auto-selected representatives"

    out = exports.export_all(result, tmp_path, store=store)
    assert "selected" in out.written
    assert "selected_fasta" in out.written
    assert "selected" not in out.skipped


def test_explicit_annotations_override_auto_selection(store, tmp_path):
    from boldcurator.core.pipeline import run_search

    result = run_search(store, taxa_text="Danaus plexippus")
    out = exports.export_all(result, tmp_path, annotations=Annotations(), store=store)
    assert "selected" in out.skipped
