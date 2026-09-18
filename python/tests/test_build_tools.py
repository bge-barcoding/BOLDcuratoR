"""Builder and verifier behaviour, exercised through the real command-line tools.

These cover the failure modes that only show up against real data: a partial
build, a taxonomically scoped build, a header that does not match, and a rank
column that parsed into the wrong position.
"""

from __future__ import annotations

import gzip
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parent.parent
BUILD = [sys.executable, str(ROOT / "tools" / "build_snapshot.py")]
VERIFY = [sys.executable, str(ROOT / "tools" / "verify_snapshot.py")]


def _run(cmd: list[str]) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, capture_output=True, text=True)


@pytest.fixture(scope="module")
def package(tmp_path_factory) -> Path:
    """A fake data package, uncompressed so tests can rewrite it cheaply."""
    work = tmp_path_factory.mktemp("pkg")
    gz = work / "pkg.tsv.gz"
    _run([sys.executable, str(ROOT / "tests" / "make_fake_package.py"),
          "--out", str(gz), "--rows", "1500"]).check_returncode()
    tsv = work / "pkg.tsv"
    tsv.write_text(gzip.open(gz, "rt", encoding="utf-8").read(), encoding="utf-8")
    return tsv


def _rewrite(source: Path, target: Path, transform) -> Path:
    lines = source.read_text(encoding="utf-8").splitlines()
    header = lines[0].split("\t")
    rows = [line.split("\t") for line in lines[1:] if line]
    header, rows = transform(header, rows)
    target.write_text(
        "\n".join(["\t".join(header)] + ["\t".join(r) for r in rows]) + "\n",
        encoding="utf-8",
    )
    return target


# -- dry run ---------------------------------------------------------------


def test_dry_run_reports_a_workable_header_and_creates_nothing(package, tmp_path):
    out = tmp_path / "never.duckdb"
    result = _run(BUILD + ["--tsv", str(package), "--out", str(out), "--dry-run"])
    assert result.returncode == 0, result.stderr
    assert "All required columns are present" in result.stdout
    assert not out.exists()


def test_dry_run_names_the_missing_required_column(package, tmp_path):
    broken = _rewrite(
        package, tmp_path / "broken.tsv",
        lambda h, rows: (
            [c for c in h if c != "country/ocean"],
            [[v for i, v in enumerate(r) if h[i] != "country/ocean"] for r in rows],
        ),
    )
    result = _run(BUILD + ["--tsv", str(broken), "--dry-run"])
    assert result.returncode == 1
    assert "country/ocean" in result.stdout
    assert "would refuse to run" in result.stdout


def test_out_is_required_without_dry_run(package):
    result = _run(BUILD + ["--tsv", str(package)])
    assert result.returncode == 2
    assert "--out is required" in result.stderr


# -- partial builds --------------------------------------------------------


def test_partial_build_is_recorded_and_verifies_clean(package, tmp_path):
    out = tmp_path / "trial.duckdb"
    build = _run(BUILD + ["--tsv", str(package), "--out", str(out),
                          "--limit", "200", "--no-hash"])
    assert build.returncode == 0, build.stderr
    assert "PARTIAL BUILD" in build.stdout

    verify = _run(VERIFY + ["--snapshot", str(out)])
    assert verify.returncode == 0, verify.stdout
    assert "[WARN] partial build" in verify.stdout
    assert "warning" in verify.stdout

    sys.path.insert(0, str(ROOT / "src"))
    from boldcurator.data.snapshot import SnapshotStore

    with SnapshotStore(out) as store:
        info = store.info()
        assert info.partial_build
        assert info.row_limit == "200"
        assert "PARTIAL BUILD" in info.describe()


def test_full_build_is_not_flagged_partial(package, tmp_path):
    out = tmp_path / "full.duckdb"
    _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash"]).check_returncode()
    verify = _run(VERIFY + ["--snapshot", str(out)])
    assert verify.returncode == 0
    assert "partial build" not in verify.stdout
    assert "All" in verify.stdout and "checks passed" in verify.stdout


# -- taxon resolution: absent warns, wrong rank fails ----------------------


def test_scoped_snapshot_warns_about_absent_taxa_but_passes(package, tmp_path):
    """A snapshot with no Lepidoptera is legitimate, not a build failure."""
    scoped = _rewrite(
        package, tmp_path / "scoped.tsv",
        lambda h, rows: (h, [r for r in rows if r[h.index("order")] != "Lepidoptera"]),
    )
    out = tmp_path / "scoped.duckdb"
    _run(BUILD + ["--tsv", str(scoped), "--out", str(out), "--no-hash"]).check_returncode()

    verify = _run(VERIFY + ["--snapshot", str(out)])
    assert verify.returncode == 0, verify.stdout
    assert "[WARN] resolve('lepidoptera')" in verify.stdout
    assert "absent" in verify.stdout
    assert "FAIL" not in verify.stdout


def test_rank_column_parsed_into_the_wrong_position_still_fails(package, tmp_path):
    """What the taxon check is actually for: detecting a mis-mapped rank column."""
    def swap(h, rows):
        oi, fi = h.index("order"), h.index("family")
        for r in rows:
            r[oi], r[fi] = r[fi], r[oi]
        return h, rows

    swapped = _rewrite(package, tmp_path / "swapped.tsv", swap)
    out = tmp_path / "swapped.duckdb"
    _run(BUILD + ["--tsv", str(swapped), "--out", str(out), "--no-hash"]).check_returncode()

    verify = _run(VERIFY + ["--snapshot", str(out)])
    assert verify.returncode == 1, verify.stdout
    assert "[FAIL] resolve('lepidoptera') -> order" in verify.stdout
    assert "do not publish" in verify.stdout


# -- refusals --------------------------------------------------------------


def test_build_refuses_to_overwrite_an_existing_snapshot(package, tmp_path):
    out = tmp_path / "once.duckdb"
    _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash"]).check_returncode()
    again = _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash"])
    assert again.returncode == 1
    assert "Refusing to overwrite" in again.stderr


def test_build_fails_loudly_on_a_missing_required_column(package, tmp_path):
    broken = _rewrite(
        package, tmp_path / "broken2.tsv",
        lambda h, rows: (
            [c for c in h if c != "species"],
            [[v for i, v in enumerate(r) if h[i] != "species"] for r in rows],
        ),
    )
    result = _run(BUILD + ["--tsv", str(broken), "--out", str(tmp_path / "x.duckdb")])
    assert result.returncode == 1
    assert "cannot work without" in result.stderr
    assert not (tmp_path / "x.duckdb").exists()


def test_unknown_marker_yields_a_clear_error(package, tmp_path):
    result = _run(BUILD + ["--tsv", str(package), "--out", str(tmp_path / "y.duckdb"),
                           "--marker", "NOT-A-MARKER", "--no-hash"])
    assert result.returncode == 1
    assert "No rows survived the filter" in result.stderr
