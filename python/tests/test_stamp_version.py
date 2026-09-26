"""packaging/stamp_version.py -- the release tag becomes the package version
for every install route, so pyproject.toml is never edited by hand."""

import subprocess
import sys
from pathlib import Path

import pytest

PACKAGING = Path(__file__).resolve().parents[1] / "packaging"
sys.path.insert(0, str(PACKAGING))

import stamp_version as sv  # noqa: E402


@pytest.fixture
def pyproject(tmp_path):
    """A copy of the real pyproject.toml -- the script must handle the file
    it will actually meet in CI, not a toy one."""
    real = Path(__file__).resolve().parents[1] / "pyproject.toml"
    copy = tmp_path / "pyproject.toml"
    copy.write_text(real.read_text(encoding="utf-8"), encoding="utf-8")
    return copy


@pytest.mark.parametrize("tag, expected", [
    ("V3.3", "3.3.0"),
    ("v3.3", "3.3.0"),
    ("v4", "4.0.0"),
    ("v3.2.1", "3.2.1"),
    ("3.5", "3.5.0"),
    ("", sv.DEV_VERSION),
    ("release-candidate", sv.DEV_VERSION),
])
def test_version_for_tag(tag, expected):
    assert sv.version_for_tag(tag) == expected


def test_strict_refuses_a_tag_it_would_have_to_guess_at():
    with pytest.raises(sv.StampError):
        sv.version_for_tag("release-candidate", strict=True)
    with pytest.raises(sv.StampError):
        sv.version_for_tag("v3.3-beta", strict=True)
    assert sv.version_for_tag("", strict=True) == sv.DEV_VERSION


def test_the_placeholder_in_git_is_the_dev_version():
    """An unstamped build should read as plainly not-a-release."""
    real = Path(__file__).resolve().parents[1] / "pyproject.toml"
    assert f'version = "{sv.DEV_VERSION}"' in real.read_text(encoding="utf-8")


def test_stamp_changes_only_the_version_line(pyproject):
    before = pyproject.read_text(encoding="utf-8").splitlines()
    sv.stamp(pyproject, "3.3.0")
    after = pyproject.read_text(encoding="utf-8").splitlines()

    changed = [(a, b) for a, b in zip(before, after) if a != b]
    assert len(before) == len(after)
    assert changed == [(f'version = "{sv.DEV_VERSION}"', 'version = "3.3.0"')]


def test_stamp_refuses_a_file_without_exactly_one_version_line(tmp_path):
    bad = tmp_path / "pyproject.toml"
    bad.write_text('[project]\nname = "x"\n')
    with pytest.raises(sv.StampError, match="found 0"):
        sv.stamp(bad, "3.3.0")


def test_command_line_prints_the_version_and_stamps_the_file(pyproject):
    run = subprocess.run(
        [sys.executable, str(PACKAGING / "stamp_version.py"), "V3.4",
         "--pyproject", str(pyproject)],
        capture_output=True, text=True, check=True)
    assert run.stdout.strip() == "3.4.0"
    assert 'version = "3.4.0"' in pyproject.read_text(encoding="utf-8")


def test_command_line_strict_failure_leaves_the_file_alone(pyproject):
    before = pyproject.read_text(encoding="utf-8")
    run = subprocess.run(
        [sys.executable, str(PACKAGING / "stamp_version.py"), "nightly",
         "--strict", "--pyproject", str(pyproject)],
        capture_output=True, text=True)
    assert run.returncode == 1
    assert "nightly" in run.stderr
    assert pyproject.read_text(encoding="utf-8") == before
