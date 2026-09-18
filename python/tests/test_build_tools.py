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


# -- sequence table --------------------------------------------------------


def test_sequence_table_is_populated_and_orphan_free(package, tmp_path):
    """Built straight from staging, with no join and no sort.

    The join-and-sort version ran a 32 GB machine out of memory on the real
    package; both operations recovered nothing.
    """
    import duckdb

    out = tmp_path / "seq.duckdb"
    _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash"]).check_returncode()

    con = duckdb.connect(str(out), read_only=True)
    try:
        cols = {r[0] for r in con.execute("DESCRIBE sequence").fetchall()}
        assert cols == {"processid", "nuc"}, "sid is gone; nothing reads it"

        n_seq, n_spec = con.execute(
            "SELECT (SELECT count(*) FROM sequence), (SELECT count(*) FROM specimen)"
        ).fetchone()
        assert 0 < n_seq <= n_spec

        orphans = con.execute(
            "SELECT count(*) FROM sequence q "
            "ANTI JOIN specimen s ON s.processid = q.processid"
        ).fetchone()[0]
        assert orphans == 0

        # Every specimen with a basecount over zero should have a sequence.
        assert con.execute(
            "SELECT count(*) FROM specimen s "
            "WHERE s.nuc_basecount > 0 "
            "AND NOT EXISTS (SELECT 1 FROM sequence q WHERE q.processid = s.processid)"
        ).fetchone()[0] == 0
    finally:
        con.close()


def test_no_sequences_build_leaves_an_empty_sequence_table(package, tmp_path):
    import duckdb

    out = tmp_path / "meta.duckdb"
    _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-sequences",
                  "--no-hash"]).check_returncode()
    con = duckdb.connect(str(out), read_only=True)
    try:
        assert con.execute("SELECT count(*) FROM sequence").fetchone()[0] == 0
        assert {r[0] for r in con.execute("DESCRIBE sequence").fetchall()} == \
            {"processid", "nuc"}
    finally:
        con.close()
    assert _run(VERIFY + ["--snapshot", str(out)]).returncode == 0


# -- reuse-staging and overwrite -------------------------------------------


def test_overwrite_is_required_to_replace_an_existing_snapshot(package, tmp_path):
    out = tmp_path / "o.duckdb"
    _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash"]).check_returncode()

    refused = _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash"])
    assert refused.returncode == 1
    assert "--overwrite" in refused.stderr

    allowed = _run(BUILD + ["--tsv", str(package), "--out", str(out), "--no-hash",
                            "--overwrite"])
    assert allowed.returncode == 0, allowed.stderr


def test_reuse_staging_reproduces_the_same_snapshot(package, tmp_path):
    """A retry, or a second build, must not re-read the whole source.

    This is the real workflow: build metadata-only, keep staging, then build
    the full snapshot from the same ingest.
    """
    import duckdb

    def counts(path):
        con = duckdb.connect(str(path), read_only=True)
        try:
            return con.execute(
                "SELECT (SELECT count(*) FROM specimen), "
                "       (SELECT count(*) FROM sequence), "
                "       (SELECT count(*) FROM taxon), "
                "       (SELECT count(*) FROM specimen_recordset)"
            ).fetchone()
        finally:
            con.close()

    direct = tmp_path / "direct.duckdb"
    _run(BUILD + ["--tsv", str(package), "--out", str(direct),
                  "--no-hash"]).check_returncode()

    staged = tmp_path / "staged.duckdb"
    first = _run(BUILD + ["--tsv", str(package), "--out", str(staged),
                          "--no-sequences", "--no-hash", "--keep-staging"])
    first.check_returncode()
    staging = Path(str(staged) + ".staging")
    assert staging.exists(), "‑-keep-staging should leave the staging file"
    assert "Staging kept" in first.stdout

    # Now the full build, reusing that ingest rather than re-reading the source.
    second = _run(BUILD + ["--tsv", str(package), "--out", str(staged),
                           "--no-hash", "--reuse-staging", "--overwrite"])
    second.check_returncode()
    assert "ingest skipped" in second.stdout
    assert "ingest + marker filter" not in second.stdout

    assert counts(direct) == counts(staged)
    assert not staging.exists(), "a successful build without --keep-staging cleans up"


def test_reuse_staging_rejects_something_that_is_not_a_staging_file(package, tmp_path):
    """The guard that stops a wrong file being mistaken for staging."""
    import duckdb

    out = tmp_path / "g.duckdb"
    bogus = Path(str(out) + ".staging")
    con = duckdb.connect(str(bogus))
    con.execute("CREATE TABLE something_else (x INTEGER)")
    con.close()

    result = _run(BUILD + ["--tsv", str(package), "--out", str(out),
                           "--no-hash", "--reuse-staging"])
    assert result.returncode == 1
    assert "no 'stage' table" in result.stderr


def test_reuse_staging_without_a_staging_file_is_refused(package, tmp_path):
    result = _run(BUILD + ["--tsv", str(package), "--out", str(tmp_path / "x.duckdb"),
                           "--reuse-staging", "--no-hash"])
    assert result.returncode == 1
    assert "no staging file" in result.stderr


def test_one_ingest_can_serve_two_different_output_files(package, tmp_path):
    """The real workflow: metadata-only first, then the full snapshot.

    Staging defaults to <out>.staging, so without --staging-path the second
    build writing a different file would re-read the whole source.
    """
    import duckdb

    staging = tmp_path / "shared.staging"
    meta = tmp_path / "meta.duckdb"
    full = tmp_path / "full.duckdb"

    first = _run(BUILD + ["--tsv", str(package), "--out", str(meta),
                          "--no-sequences", "--no-hash",
                          "--staging-path", str(staging), "--keep-staging"])
    first.check_returncode()
    assert staging.exists()

    second = _run(BUILD + ["--tsv", str(package), "--out", str(full), "--no-hash",
                           "--staging-path", str(staging), "--reuse-staging"])
    second.check_returncode()
    assert "ingest skipped" in second.stdout
    assert not staging.exists(), "the last build cleans up"

    def rows(path, table):
        con = duckdb.connect(str(path), read_only=True)
        try:
            return con.execute(f"SELECT count(*) FROM {table}").fetchone()[0]
        finally:
            con.close()

    assert rows(meta, "specimen") == rows(full, "specimen")
    assert rows(meta, "sequence") == 0
    assert rows(full, "sequence") > 0
    assert _run(VERIFY + ["--snapshot", str(meta)]).returncode == 0
    assert _run(VERIFY + ["--snapshot", str(full)]).returncode == 0


def test_benchmark_snapshot_tool_builds_something_the_store_can_open(tmp_path):
    """The benchmark generator is only useful if it produces a real snapshot.

    Small enough to be a test, same code path as the 20 M-row build.
    """
    import importlib.util

    from boldcurator.data.snapshot import SnapshotStore

    tool = Path(__file__).resolve().parents[1] / "tools" / "make_benchmark_snapshot.py"
    spec = importlib.util.spec_from_file_location("make_benchmark_snapshot", tool)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    out = tmp_path / "bench.duckdb"
    module.build(str(out), 5_000)

    with SnapshotStore(out) as store:
        info = store.info()
        assert info.row_count == 5_000
        assert info.bin_count > 0
        assert info.taxon_count > 0
        # every rank the search resolves against must be present
        ranks = {r[0] for r in store.connection.execute(
            "SELECT DISTINCT taxon_rank FROM taxon").fetchall()}
        assert {"species", "genus", "family", "order"} <= ranks
        # sid must NOT track physical position -- that is the whole point
        correlation = store.connection.execute(
            "SELECT abs(corr(rowid, sid)) FROM specimen").fetchone()[0]
        assert correlation < 0.1, (
            "sid tracks physical order, so the generated snapshot no longer "
            "reproduces the real builder and every fetch measurement taken "
            "against it is optimistic"
        )
