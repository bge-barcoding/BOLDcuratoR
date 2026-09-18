"""Tests for the parity harness itself.

These run **without R**, against the committed R reference output, so CI can
enforce parity on every push without an R toolchain. Regenerating that
reference needs R and is a separate, optional step.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pandas as pd
import pytest

ROOT = Path(__file__).resolve().parent.parent
PARITY = ROOT / "parity"
FIXTURES = PARITY / "fixtures"

pytestmark = pytest.mark.skipif(
    not (FIXTURES / "r_specimens.csv").exists(),
    reason="R reference output not present",
)


def _run(args: list[str]) -> subprocess.CompletedProcess:
    # PYTHONDONTWRITEBYTECODE, because this module edits a source file in place
    # to prove the harness can fail. The edit ("11" -> "10") leaves the file the
    # same size, and the restore lands in the same second, so a cached .pyc can
    # look valid and the subprocess would reload the injected bug.
    env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1"}
    return subprocess.run([sys.executable, *args], capture_output=True, text=True,
                          cwd=ROOT, env=env)


def test_fixture_generation_is_deterministic(tmp_path):
    """The fixture must not drift between runs, or the reference goes stale."""
    before = (FIXTURES / "parity_input.tsv").read_text(encoding="utf-8")
    _run([str(PARITY / "make_fixture.py")]).check_returncode()
    after = (FIXTURES / "parity_input.tsv").read_text(encoding="utf-8")
    assert before == after


def test_fixture_covers_every_boundary_group():
    frame = pd.read_csv(FIXTURES / "parity_input.tsv", sep="\t", dtype=str,
                        keep_default_na=False)
    groups = {c.split(":")[0] for c in frame["case"]}
    assert {"species", "seq_quality", "public_voucher", "type", "identifier",
            "id_method", "institution", "rank", "bags", "select",
            "bins"} <= groups


def test_fixture_straddles_the_bags_thresholds():
    """2/3/10/11 specimens -- the D/B and B/A edges."""
    frame = pd.read_csv(FIXTURES / "parity_input.tsv", sep="\t", dtype=str,
                        keep_default_na=False)
    counts = frame[frame["genus"] == "Bagsus"]["species"].value_counts()
    assert counts.get("Bagsus duo") == 2
    assert counts.get("Bagsus tres") == 3
    assert counts.get("Bagsus decem") == 10
    assert counts.get("Bagsus undecim") == 11


def test_comparison_passes_against_the_committed_reference():
    result = _run([str(PARITY / "compare.py")])
    assert result.returncode == 0, result.stdout + result.stderr
    assert "PASS" in result.stdout


def test_every_difference_is_explained_and_the_report_says_so():
    _run([str(PARITY / "compare.py")]).check_returncode()
    report = (PARITY / "REPORT.md").read_text(encoding="utf-8")
    assert "**PASS**" in report
    assert "UNEXPLAINED" not in report
    # The four divergences we expect to see, and no others.
    for tag in ("UNIFIED_SPECIES_RULE", "RANK2_IMAGE_REMOVED",
                "CF_AFF_CONCORDANCE", "R_ROW_ERROR_ZEROES_SCORE"):
        assert tag in report


def test_harness_detects_an_injected_divergence(tmp_path, monkeypatch):
    """A harness that cannot fail proves nothing.

    Flipping the BAGS grade-A threshold to the value the dead
    BAGS_GRADE_CRITERIA constant claims must be caught.
    """
    source = ROOT / "src" / "boldcurator" / "core" / "bags.py"
    original = source.read_text(encoding="utf-8")
    assert "if specimen_count >= 11:" in original
    try:
        source.write_text(
            original.replace("if specimen_count >= 11:", "if specimen_count >= 10:"),
            encoding="utf-8",
        )
        result = _run([str(PARITY / "compare.py")])
        assert result.returncode == 1
        assert "UNEXPLAINED" in result.stderr
        assert "Bagsus decem" in result.stderr
    finally:
        source.write_text(original, encoding="utf-8")
        shutil.rmtree(source.parent / "__pycache__", ignore_errors=True)

    assert _run([str(PARITY / "compare.py")]).returncode == 0, \
        "the harness must pass again once the injected bug is reverted"
