"""Plan 5.1: download a pre-built snapshot and verify it.

Runs against a throwaway local HTTP server rather than the real network --
this needs to prove the download/verify/skip-if-unchanged logic works, not
that Zenodo is up. ``resolve_zenodo_record``'s own JSON shape is exercised
directly against a fixed payload, since Zenodo's actual API is out of scope
for a unit test.
"""

from __future__ import annotations

import gzip
import hashlib
import http.server
import json
import threading
from pathlib import Path

import pytest

from boldcurator.build import fetch_snapshot as fs


@pytest.fixture
def http_server(tmp_path):
    """Serves ``tmp_path`` over plain HTTP on a random local port."""
    handler = lambda *a, **kw: http.server.SimpleHTTPRequestHandler(
        *a, directory=str(tmp_path), **kw)
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}", tmp_path
    finally:
        server.shutdown()
        thread.join(timeout=5)


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def test_download_verifies_a_correct_checksum(http_server, tmp_path):
    base_url, served_dir = http_server
    payload = b"a fake snapshot, just bytes for the test" * 1000
    (served_dir / "snap.duckdb").write_bytes(payload)

    source = fs.Source(url=f"{base_url}/snap.duckdb",
                       checksum=f"sha256:{_sha256(payload)}")
    out = tmp_path / "out" / "snap.duckdb"
    fs.download(source, out, progress=lambda *a, **k: None)

    assert out.read_bytes() == payload


def test_download_refuses_a_wrong_checksum_and_cleans_up(http_server, tmp_path):
    base_url, served_dir = http_server
    payload = b"the real bytes"
    (served_dir / "snap.duckdb").write_bytes(payload)

    source = fs.Source(url=f"{base_url}/snap.duckdb",
                       checksum="sha256:" + "0" * 64)
    out = tmp_path / "out" / "snap.duckdb"
    with pytest.raises(fs.FetchError, match="mismatch"):
        fs.download(source, out, progress=lambda *a, **k: None)

    assert not out.exists()
    assert not out.with_suffix(out.suffix + ".part").exists()


def test_download_without_a_checksum_still_writes_the_file(http_server, tmp_path):
    base_url, served_dir = http_server
    (served_dir / "snap.duckdb").write_bytes(b"unverified bytes")

    source = fs.Source(url=f"{base_url}/snap.duckdb")
    out = tmp_path / "out" / "snap.duckdb"
    fs.download(source, out, progress=lambda *a, **k: None)

    assert out.read_bytes() == b"unverified bytes"


def test_resolve_manifest_reads_a_local_file(tmp_path):
    manifest = tmp_path / "manifest.json"
    manifest.write_text(json.dumps({
        "url": "https://example.org/bold_snapshot.duckdb",
        "sha256": "abc123",
        "snapshot_id": "2026-09-11",
        "row_count": 20_164_595,
    }))

    source = fs.resolve_manifest(str(manifest))
    assert source.url == "https://example.org/bold_snapshot.duckdb"
    assert source.checksum == "sha256:abc123"
    assert source.snapshot_id == "2026-09-11"
    assert source.row_count == 20_164_595


def test_resolve_manifest_refuses_a_relative_url(tmp_path):
    manifest = tmp_path / "manifest.json"
    manifest.write_text(json.dumps({"url": "bold_snapshot.duckdb"}))
    with pytest.raises(fs.FetchError, match="absolute"):
        fs.resolve_manifest(str(manifest))


def test_resolve_manifest_over_http(http_server):
    base_url, served_dir = http_server
    (served_dir / "manifest.json").write_text(json.dumps({
        "url": f"{base_url}/snap.duckdb", "sha256": "deadbeef",
        "snapshot_id": "x",
    }))
    source = fs.resolve_manifest(f"{base_url}/manifest.json")
    assert source.url == f"{base_url}/snap.duckdb"
    assert source.checksum == "sha256:deadbeef"


def test_resolve_zenodo_record_picks_the_only_file(monkeypatch):
    payload = {
        "id": 123456,
        "metadata": {"version": "2026-09-11"},
        "files": [{"key": "bold_snapshot.duckdb",
                   "checksum": "md5:deadbeefcafefeed",
                   "links": {"self": "https://zenodo.org/api/files/xyz"}}],
    }
    monkeypatch.setattr(fs, "_get_json", lambda url: payload)

    source = fs.resolve_zenodo_record("123456")
    assert source.url == "https://zenodo.org/api/files/xyz"
    assert source.checksum == "md5:deadbeefcafefeed"
    assert source.snapshot_id == "123456"


def test_resolve_zenodo_record_needs_a_filename_when_ambiguous(monkeypatch):
    payload = {
        "id": 123456,
        "metadata": {},
        "files": [
            {"key": "bold_meta.duckdb", "checksum": "md5:1",
             "links": {"self": "https://x/meta"}},
            {"key": "bold_full.duckdb", "checksum": "md5:2",
             "links": {"self": "https://x/full"}},
            {"key": "readme.txt", "checksum": "md5:3",
             "links": {"self": "https://x/readme"}},
        ],
    }
    monkeypatch.setattr(fs, "_get_json", lambda url: payload)

    with pytest.raises(fs.FetchError, match="pass --filename"):
        fs.resolve_zenodo_record("123456")

    source = fs.resolve_zenodo_record("123456", filename="bold_full.duckdb")
    assert source.url == "https://x/full"


def test_fetch_skips_a_download_already_at_this_snapshot_id(
    http_server, tmp_path, monkeypatch
):
    base_url, served_dir = http_server
    (served_dir / "snap.duckdb").write_bytes(b"new bytes")
    manifest = tmp_path / "manifest.json"
    manifest.write_text(json.dumps({
        "url": f"{base_url}/snap.duckdb", "sha256": "irrelevant-here",
        "snapshot_id": "2026-09-11",
    }))

    out = tmp_path / "existing.duckdb"
    out.write_bytes(b"stale local copy")
    monkeypatch.setattr(fs, "_local_snapshot_id", lambda path: "2026-09-11")

    args = fs.build_parser().parse_args(
        ["--out", str(out), "--manifest", str(manifest)])
    assert fs.fetch(args) == 0
    # Not overwritten -- the manifest's snapshot_id matched what's on disk.
    assert out.read_bytes() == b"stale local copy"


def test_download_decompresses_a_gzipped_source(http_server, tmp_path):
    """Round 4, item 2: a published snapshot may be gzipped
    (``bold_snapshot_2026-09-11.duckdb.gz``) -- ``out`` should end up as the
    plain, already-decompressed .duckdb the app can open directly."""
    base_url, served_dir = http_server
    payload = b"a fake duckdb snapshot" * 5000
    compressed = gzip.compress(payload)
    (served_dir / "bold_snapshot_2026-09-11.duckdb.gz").write_bytes(compressed)

    source = fs.Source(url=f"{base_url}/bold_snapshot_2026-09-11.duckdb.gz",
                       checksum=f"sha256:{_sha256(compressed)}",
                       filename="bold_snapshot_2026-09-11.duckdb.gz")
    out = tmp_path / "out" / "snapshot.duckdb"
    fs.download(source, out, progress=lambda *a, **k: None)

    assert out.read_bytes() == payload
    assert not out.with_suffix(out.suffix + ".gz.part").exists()
    assert not out.with_suffix(out.suffix + ".part").exists()


def test_download_checksum_applies_to_the_compressed_bytes(http_server, tmp_path):
    """Zenodo (and a manifest) publish the checksum of the file as uploaded --
    the gzipped bytes, not what is inside them -- so verification must happen
    before decompression, against the download as-is."""
    base_url, served_dir = http_server
    payload = b"another fake snapshot" * 3000
    compressed = gzip.compress(payload)
    (served_dir / "snap.duckdb.gz").write_bytes(compressed)

    source = fs.Source(url=f"{base_url}/snap.duckdb.gz",
                       checksum="sha256:" + "0" * 64)
    out = tmp_path / "out" / "snapshot.duckdb"
    with pytest.raises(fs.FetchError, match="mismatch"):
        fs.download(source, out, progress=lambda *a, **k: None)
    assert not out.exists()


def test_gzip_detected_from_filename_even_when_the_url_does_not_end_in_it(
    http_server, tmp_path
):
    """Some hosts serve a file from a URL that doesn't end in its real name
    (a redirect, a signed content URL) -- ``source.filename`` (Zenodo's own
    ``key``) is checked first, ``url`` only as a fallback."""
    base_url, served_dir = http_server
    payload = b"yet another fake snapshot" * 2000
    compressed = gzip.compress(payload)
    (served_dir / "content").write_bytes(compressed)

    source = fs.Source(url=f"{base_url}/content",
                       filename="bold_snapshot_2026-09-11.duckdb.gz")
    out = tmp_path / "out" / "snapshot.duckdb"
    fs.download(source, out, progress=lambda *a, **k: None)

    assert out.read_bytes() == payload


@pytest.mark.parametrize("given", [
    "22849516",
    "10.5281/zenodo.22849516",
    "https://doi.org/10.5281/zenodo.22849516",
    "https://zenodo.org/records/22849516",
])
def test_resolve_zenodo_record_accepts_a_doi_or_url_or_bare_id(monkeypatch, given):
    """Round 4, item 2: DEFAULT_SNAPSHOT_ZENODO_DOI is a full DOI, so every
    form a curator (or that constant) might hand in must resolve the same."""
    seen = {}

    def fake_get_json(url):
        seen["url"] = url
        return {
            "id": 22849516,
            "metadata": {},
            "files": [{"key": "bold_snapshot_2026-09-11.duckdb.gz",
                       "checksum": "md5:abc",
                       "links": {"self": "https://zenodo.org/records/22849516/x"}}],
        }

    monkeypatch.setattr(fs, "_get_json", fake_get_json)
    source = fs.resolve_zenodo_record(given)
    assert seen["url"] == fs.ZENODO_API.format(record_id="22849516")
    assert source.filename == "bold_snapshot_2026-09-11.duckdb.gz"


def test_resolve_zenodo_record_picks_a_gzipped_duckdb_among_several_files(
    monkeypatch
):
    payload = {
        "id": 22849516,
        "metadata": {"version": "2026-09-11"},
        "files": [
            {"key": "bold_snapshot_2026-09-11.duckdb.gz", "checksum": "md5:1",
             "links": {"self": "https://x/snapshot"}},
            {"key": "checksums.txt", "checksum": "md5:2",
             "links": {"self": "https://x/checksums"}},
        ],
    }
    monkeypatch.setattr(fs, "_get_json", lambda url: payload)

    source = fs.resolve_zenodo_record("22849516")
    assert source.url == "https://x/snapshot"
    assert source.filename == "bold_snapshot_2026-09-11.duckdb.gz"


def test_fetch_downloads_when_the_snapshot_id_differs(
    http_server, tmp_path, monkeypatch
):
    base_url, served_dir = http_server
    payload = b"the newer bytes"
    (served_dir / "snap.duckdb").write_bytes(payload)
    manifest = tmp_path / "manifest.json"
    manifest.write_text(json.dumps({
        "url": f"{base_url}/snap.duckdb", "sha256": _sha256(payload),
        "snapshot_id": "2026-09-18",
    }))

    out = tmp_path / "existing.duckdb"
    out.write_bytes(b"an older snapshot")
    monkeypatch.setattr(fs, "_local_snapshot_id", lambda path: "2026-09-11")

    args = fs.build_parser().parse_args(
        ["--out", str(out), "--manifest", str(manifest)])
    assert fs.fetch(args) == 0
    assert out.read_bytes() == payload
