"""CLI tests, including the benchmark used to qualify a real snapshot."""

from __future__ import annotations

import pytest

from boldcurator.cli import DEFAULT_BENCHMARK_TAXA, main


def test_info_describes_the_snapshot(fixture_snapshot, capsys):
    assert main(["info", "--snapshot", str(fixture_snapshot)]) == 0
    out = capsys.readouterr().out
    assert "records" in out and "COI-5P" in out


def test_resolve_reports_rank_and_flags_unknown_names(fixture_snapshot, capsys):
    code = main(["resolve", "--snapshot", str(fixture_snapshot),
                 "Danaus plexippus", "Notataxonatall"])
    captured = capsys.readouterr()
    assert "species" in captured.out
    assert "not found" in captured.err
    assert code == 1


def test_search_writes_the_export_set(fixture_snapshot, tmp_path, capsys):
    code = main(["search", "--snapshot", str(fixture_snapshot),
                 "--taxa", "Danaus plexippus", "--out", str(tmp_path)])
    assert code == 0
    assert any(tmp_path.iterdir())
    assert "records" in capsys.readouterr().out


def test_benchmark_times_every_stage(fixture_snapshot, capsys):
    code = main(["benchmark", "--snapshot", str(fixture_snapshot),
                 "--taxon", "Danaus plexippus", "--sequences", "50", "--export"])
    out = capsys.readouterr().out
    assert code == 0
    for step in ("open snapshot", "resolve", "estimate", "plan", "fetch",
                 "full pipeline", "score (criterion flags)", "BAGS grades",
                 "BIN analysis", "auto-selection", "stream",
                 "export all formats"):
        assert step in out, f"{step} missing from the benchmark output"
    assert "seconds" in out


def test_benchmark_defaults_span_three_scales():
    """A species, a family and an order -- orders of magnitude apart."""
    assert DEFAULT_BENCHMARK_TAXA == ["Danaus plexippus", "Nymphalidae",
                                      "Lepidoptera"]


def test_benchmark_skips_a_missing_taxon_but_still_measures_the_rest(
    fixture_snapshot, capsys
):
    """One bad name on the command line must not lose the whole measurement."""
    code = main(["benchmark", "--snapshot", str(fixture_snapshot),
                 "--taxon", "Notataxonatall", "--taxon", "Danaus plexippus",
                 "--sequences", "10"])
    out = capsys.readouterr().out
    assert "not found in this snapshot" in out
    assert "full pipeline 'Danaus plexippus'" in out
    assert code == 0


def test_benchmark_fails_clearly_when_no_taxon_resolves(fixture_snapshot, capsys):
    code = main(["benchmark", "--snapshot", str(fixture_snapshot),
                 "--taxon", "Notataxonatall", "--sequences", "10"])
    out = capsys.readouterr().out
    assert "None of the requested taxa are in this snapshot" in out
    assert code == 1


def test_benchmark_reports_a_size_limit_refusal_as_a_result(
    fixture_snapshot, capsys, monkeypatch
):
    """Whether the guards suit real data is one of the things being measured,
    so a refusal must be reported rather than crash the run."""
    from boldcurator.config import constants

    monkeypatch.setitem(constants.DOWNLOAD_LIMITS, "MAX_RECORDS", 1)
    code = main(["benchmark", "--snapshot", str(fixture_snapshot),
                 "--taxon", "Nymphalidae", "--sequences", "10"])
    out = capsys.readouterr().out
    assert "SizeLimitExceeded" in out
    assert "Size limits refused a query" in out
    assert code == 0, "a guard doing its job is not a benchmark failure"


def test_benchmark_runs_without_psutil(fixture_snapshot, capsys, monkeypatch):
    """Memory reporting is optional; the command must work on a bare install."""
    import builtins

    real_import = builtins.__import__

    def no_psutil(name, *args, **kwargs):
        if name == "psutil":
            raise ImportError("psutil not available")
        return real_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", no_psutil)
    code = main(["benchmark", "--snapshot", str(fixture_snapshot),
                 "--taxon", "Danaus plexippus", "--sequences", "10"])
    out = capsys.readouterr().out
    assert code == 0
    assert "psutil not installed" in out


def test_unknown_snapshot_path_is_a_clean_error(tmp_path, capsys):
    code = main(["info", "--snapshot", str(tmp_path / "nope.duckdb")])
    assert code == 1
    assert "No snapshot at" in capsys.readouterr().err
