"""Shared fixtures.

The snapshot is generated, not committed: a few seconds of build time keeps a
binary out of git and keeps the fixture in step with the builder.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))


@pytest.fixture(scope="session")
def fixture_snapshot(tmp_path_factory) -> Path:
    """A small real snapshot, built through the real builder."""
    work = tmp_path_factory.mktemp("snapshot")
    tsv = work / "fake_bold.tsv.gz"
    db = work / "fixture.duckdb"

    subprocess.run(
        [sys.executable, str(ROOT / "tests" / "make_fake_package.py"),
         "--out", str(tsv), "--rows", "3000"],
        check=True, capture_output=True,
    )
    subprocess.run(
        [sys.executable, str(ROOT / "tools" / "build_snapshot.py"),
         "--tsv", str(tsv), "--out", str(db),
         "--memory-limit", "1GB", "--threads", "2", "--no-hash"],
        check=True, capture_output=True,
    )
    return db


@pytest.fixture(scope="session")
def store(fixture_snapshot):
    from boldcurator.data.snapshot import SnapshotStore

    with SnapshotStore(fixture_snapshot) as s:
        yield s
