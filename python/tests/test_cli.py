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


def test_desktop_threads_the_window_flag_through_to_launch(monkeypatch, tmp_path):
    calls = {}

    def fake_launch(snapshot, *, page_size, window):
        calls["snapshot"] = snapshot
        calls["page_size"] = page_size
        calls["window"] = window

    monkeypatch.setattr("boldcurator.desktop.launch", fake_launch)
    snap = tmp_path / "s.duckdb"

    code = main(["desktop", "--snapshot", str(snap), "--window", "browser-app"])

    assert code == 0
    assert calls == {"snapshot": snap, "page_size": 100, "window": "browser-app"}


def test_desktop_defaults_to_auto_window_mode(monkeypatch):
    calls = {}
    monkeypatch.setattr("boldcurator.desktop.launch",
                        lambda snapshot, **kw: calls.update(kw))

    assert main(["desktop"]) == 0
    assert calls["window"] == "auto"


def test_desktop_rejects_an_unknown_window_mode(capsys):
    with pytest.raises(SystemExit):
        main(["desktop", "--window", "smoke-signal"])
    assert "invalid choice" in capsys.readouterr().err


def test_selftest_needs_no_snapshot_and_passes(capsys):
    """No --snapshot given at all -- this is the point of the command."""
    assert main(["selftest"]) == 0
    out = capsys.readouterr().out
    assert "[ok]" in out and "FAIL" not in out
    assert "selftest passed" in out


def test_selftest_reports_a_broken_check_without_crashing(monkeypatch, capsys):
    """Added after core.phylogeny broke on a real frozen Windows build

    (Bio.Phylo.TreeConstruction's class-body substitution_matrices.load()
    needs a data directory PyInstaller's import analysis doesn't bundle
    unless told to -- packaging/README.md). This doesn't reproduce the real
    packaging failure (that needs an actual frozen build), but proves
    selftest's own error handling: a broken check is reported and fails the
    command, not left to crash it or pass silently.
    """
    import boldcurator.core.phylogeny as phylo

    def broken(*args, **kwargs):
        raise FileNotFoundError("substitution_matrices data directory missing")

    monkeypatch.setattr(phylo, "build_tree", broken)
    assert main(["selftest"]) == 1
    out = capsys.readouterr().out
    assert "[FAIL]" in out and "substitution_matrices" in out
    assert "selftest FAILED" in out
