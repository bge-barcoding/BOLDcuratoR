"""Get a pre-built snapshot onto disk -- plan 5.1.

Three ways in, all converging on the same download-and-verify core:

* ``--url`` + ``--sha256`` -- a plain link, checked against a known digest.
* ``--manifest`` -- a small JSON file (plan 5.2's shape: ``url``, ``sha256``,
  ``snapshot_id``, and optionally ``row_count``/``schema_version``), fetched
  first so this tool never has to be told the digest by hand. A concept DOI's
  manifest always describes the *latest* release, which is what makes
  re-running this a safe "check for updates" -- the file only re-downloads
  when its content actually changed.
* ``--record`` -- a Zenodo record or concept id, resolved through the public
  REST API (``developers.zenodo.org``) to find the snapshot file and its
  checksum without a manifest at all. A concept id always resolves to the
  newest version, which is the whole point of publishing under one.

Nothing here is BOLD-specific or Zenodo-specific below the resolution step --
any host that can serve a file over HTTP(S) and publish a sha256 works with
``--url``.  Only ``--record`` talks to Zenodo's API.

Uses the standard library's ``urllib`` rather than ``requests``: this project
has stayed dependency-light throughout (``duckdb``, ``pandas``, ``openpyxl``
are the only runtime dependencies), and a one-shot streamed download with a
progress readout does not need more than that.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from urllib.error import HTTPError, URLError

ZENODO_API = "https://zenodo.org/api/records/{record_id}"

#: Chunk size for the streamed download and the running sha256/md5.  A few
#: hundred KB balances syscall overhead against progress-readout granularity
#: for files in the hundreds-of-MB to low-GB range this exists to move.
CHUNK_SIZE = 1024 * 1024


class FetchError(RuntimeError):
    pass


@dataclass
class Source:
    """Where to download from, and how to know the download is right."""

    url: str
    #: ``"sha256:<hex>"`` or ``"md5:<hex>"`` -- Zenodo publishes md5 by
    #: default, this project's own ``manifest.json`` (plan 5.2) publishes
    #: sha256. ``None`` means the download is not checked, which is only ever
    #: allowed for a bare ``--url`` and prints a loud warning either way.
    checksum: str | None = None
    snapshot_id: str = ""
    row_count: int | None = None
    schema_version: str = ""


def _get_json(url: str) -> dict:
    try:
        with urllib.request.urlopen(url, timeout=30) as resp:
            return json.loads(resp.read().decode("utf-8"))
    except (HTTPError, URLError) as exc:
        raise FetchError(f"Could not reach {url}: {exc}") from exc


def resolve_manifest(location: str) -> Source:
    """A manifest is JSON, local or remote -- read either the same way.

    Plan 5.2's shape: ``url`` (absolute), ``sha256``, ``snapshot_id``, and
    optionally ``row_count``/``schema_version``. A relative ``url`` is
    refused rather than guessed at -- the manifest is meant to be the one
    place that has to get this right.
    """
    if location.startswith(("http://", "https://")):
        data = _get_json(location)
    else:
        path = Path(location)
        if not path.exists():
            raise FetchError(f"No manifest at {path}")
        data = json.loads(path.read_text(encoding="utf-8"))

    url = data.get("url", "")
    if not url or "://" not in url:
        raise FetchError(
            f"manifest.json must give an absolute 'url' field, got {url!r}")
    sha256 = data.get("sha256", "")
    return Source(
        url=url,
        checksum=f"sha256:{sha256}" if sha256 else None,
        snapshot_id=data.get("snapshot_id", ""),
        row_count=data.get("row_count"),
        schema_version=data.get("schema_version", ""),
    )


def resolve_zenodo_record(record_id: str, *, filename: str | None = None) -> Source:
    """Resolve a Zenodo record (or concept) id to one file's URL and checksum.

    A **concept** id (the one that does not change between versions) always
    redirects to the record's latest version, which is what makes this the
    right id to hand out for "always get the newest snapshot" -- a specific
    version id pins to that version forever, which is a deliberate choice
    too, just a different one.
    """
    data = _get_json(ZENODO_API.format(record_id=record_id))
    files = data.get("files", [])
    if not files:
        raise FetchError(f"Zenodo record {record_id} lists no files")

    if filename:
        matches = [f for f in files if f.get("key") == filename]
        if not matches:
            available = ", ".join(f.get("key", "?") for f in files)
            raise FetchError(
                f"No file named {filename!r} in record {record_id}. "
                f"Available: {available}")
    elif len(files) == 1:
        matches = files
    else:
        matches = [f for f in files if str(f.get("key", "")).endswith(".duckdb")]
        if len(matches) != 1:
            available = ", ".join(f.get("key", "?") for f in files)
            raise FetchError(
                f"Record {record_id} has {len(files)} files; pass --filename "
                f"to pick one. Available: {available}")

    entry = matches[0]
    checksum = entry.get("checksum", "")  # Zenodo's own form: "md5:<hex>"
    metadata = data.get("metadata", {})
    return Source(
        url=entry["links"]["self"],
        checksum=checksum or None,
        snapshot_id=str(data.get("id", record_id)),
        schema_version=str(metadata.get("version", "")),
    )


def _local_snapshot_id(path: Path) -> str | None:
    """The snapshot id already on disk, or ``None`` if there isn't one yet.

    Failing to open it (partial download, not a DuckDB file, wrong format)
    is treated the same as "nothing here" -- the download proceeds and
    overwrites it, which is the right outcome for a corrupt leftover.
    """
    if not path.exists():
        return None
    try:
        from ..data.snapshot import SnapshotStore

        with SnapshotStore(path) as store:
            return store.info().snapshot_id
    except Exception:
        return None


def _verify(path: Path, checksum: str) -> None:
    algo, _, expected = checksum.partition(":")
    if algo not in ("sha256", "md5"):
        raise FetchError(f"Unsupported checksum kind {algo!r}")
    digest = hashlib.new(algo)
    with open(path, "rb") as fh:
        while chunk := fh.read(CHUNK_SIZE):
            digest.update(chunk)
    actual = digest.hexdigest()
    if actual.lower() != expected.lower():
        raise FetchError(
            f"{algo} mismatch: expected {expected}, got {actual}. The download "
            "is corrupt or the source file changed underneath it -- deleted, "
            "not kept, since a silently wrong snapshot is worse than none.")


def download(source: Source, out: Path, *, progress=print) -> Path:
    """Stream ``source.url`` to a temp file beside ``out``, verify, rename.

    The temp file (not ``out`` itself) is what a failed or interrupted
    download leaves behind, so ``out`` is never observed half-written.
    """
    tmp = out.with_suffix(out.suffix + ".part")
    out.parent.mkdir(parents=True, exist_ok=True)

    try:
        with urllib.request.urlopen(source.url, timeout=30) as resp:
            total = int(resp.headers.get("Content-Length") or 0)
            written = 0
            with open(tmp, "wb") as fh:
                while chunk := resp.read(CHUNK_SIZE):
                    fh.write(chunk)
                    written += len(chunk)
                    if total:
                        progress(f"\r{written / total:.0%} "
                                 f"({written / 1e6:.0f} / {total / 1e6:.0f} MB)",
                                 end="")
                    else:
                        progress(f"\r{written / 1e6:.0f} MB", end="")
        progress("")
    except (HTTPError, URLError) as exc:
        tmp.unlink(missing_ok=True)
        raise FetchError(f"Download failed: {exc}") from exc

    if source.checksum:
        progress("Verifying checksum...")
        try:
            _verify(tmp, source.checksum)
        except FetchError:
            tmp.unlink(missing_ok=True)
            raise
    else:
        progress("No checksum given -- integrity of this download is NOT verified.")

    tmp.replace(out)
    return out


def fetch(args: argparse.Namespace) -> int:
    if args.manifest:
        source = resolve_manifest(args.manifest)
    elif args.record:
        source = resolve_zenodo_record(args.record, filename=args.filename)
    else:
        source = Source(url=args.url, checksum=(f"sha256:{args.sha256}"
                                                 if args.sha256 else None))

    if not args.force and source.snapshot_id:
        current = _local_snapshot_id(args.out)
        if current == source.snapshot_id:
            print(f"{args.out} is already snapshot {source.snapshot_id} -- "
                  "nothing to do. Pass --force to re-download anyway.")
            return 0

    print(f"Fetching {source.url}")
    download(source, args.out)
    print(f"Wrote {args.out}")
    if source.snapshot_id:
        print(f"snapshot_id: {source.snapshot_id}")
    if source.row_count:
        print(f"row_count: {source.row_count:,}")
    print(
        "This data is CC BY-SA 4.0 (Barcode of Life Data System, "
        "boldsystems.org) -- attribute BOLD Systems and share any "
        "redistributed or adapted dataset under the same licence."
    )
    return 0


def add_fetch_args(p: argparse.ArgumentParser) -> None:
    """Shared with ``cli.py``'s ``fetch-snapshot`` subcommand, so the two
    argument sets cannot drift apart."""
    p.add_argument("--out", required=True, type=Path,
                   help="where to write the snapshot .duckdb file")
    source = p.add_mutually_exclusive_group(required=True)
    source.add_argument("--url", help="a direct URL to the snapshot file")
    source.add_argument("--manifest",
                        help="URL or local path to a manifest.json (plan 5.2) "
                             "naming the file, its sha256 and its snapshot_id")
    source.add_argument("--record",
                        help="a Zenodo record or concept id, resolved via the "
                             "REST API -- a concept id always resolves to the "
                             "latest version")
    p.add_argument("--filename",
                   help="which file to fetch when --record's record has more "
                        "than one (default: the only file, or the one .duckdb)")
    p.add_argument("--sha256",
                   help="expected checksum for --url; --manifest and --record "
                        "supply their own")
    p.add_argument("--force", action="store_true",
                   help="download even if --out already reports this "
                        "snapshot_id")


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description=__doc__)
    add_fetch_args(p)
    return p


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        return fetch(args)
    except FetchError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
