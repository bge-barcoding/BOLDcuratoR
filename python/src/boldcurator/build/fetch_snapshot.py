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
has stayed dependency-light throughout, and a streamed download (split into
parallel byte-range requests where the host allows it -- see ``download``)
with a progress readout does not need more than that. The one addition is
``truststore``, for *which certificates to trust* -- see ``ssl_context``.

Every request identifies itself as BOLDcurator (``USER_AGENT``) rather than
urllib's default ``Python-urllib/3.x``: Zenodo asks automated clients for a
clear, identifiable User-Agent and warns that generic ones may be
rate-limited or blocked. A ``429``/``503`` is waited out as its
``Retry-After`` header asks (see ``_retry_delay``), not hammered.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import ssl
import sys
import threading
import time
import urllib.request
import zlib
from concurrent.futures import FIRST_EXCEPTION, ThreadPoolExecutor, wait
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from urllib.error import HTTPError, URLError

import truststore

from .. import __version__

ZENODO_API = "https://zenodo.org/api/records/{record_id}"

#: Sent with every request, in the ``AppName/version (+URL)`` form Zenodo
#: recommends so it can tell this app's traffic from anonymous scripts and
#: knows where to raise a problem (the project's GitHub, rather than a
#: personal address baked into every curator's copy).
USER_AGENT = (f"BOLDcurator/{__version__} "
              "(+https://github.com/bge-barcoding/BOLDcuratoR)")

#: Matches the numeric id out of a full DOI (``10.5281/zenodo.22849516``), a
#: ``doi.org``/``zenodo.org`` URL, or the id on its own -- so a curator (or
#: ``DEFAULT_SNAPSHOT_ZENODO_DOI``) can hand this module whichever form is at
#: hand. Anchored on ``zenodo.<digits>`` specifically so ``zenodo.org`` itself
#: (no digits after the dot) never matches.
_ZENODO_ID_IN_DOI = re.compile(r"zenodo\.(\d+)\b")

#: The date this project's own published snapshots carry in their filename
#: (``bold_snapshot_2026-09-11.duckdb.gz``) -- the same value
#: ``snapshot_builder.build`` stamps into the file itself as ``snapshot_id``
#: (``snapshot_id or date.today().isoformat()``). Used to recover a
#: *comparable* ``Source.snapshot_id`` out of a Zenodo file listing -- see
#: ``resolve_zenodo_record``.
_SNAPSHOT_DATE_IN_FILENAME = re.compile(r"(\d{4}-\d{2}-\d{2})")

#: Chunk size for reading off the network. 1 MB balances syscall overhead
#: against progress-readout granularity for files in the hundreds-of-MB to
#: low-GB range this exists to move.
CHUNK_SIZE = 1024 * 1024

#: Chunk size for the local passes over a finished download (checksum,
#: gunzip) -- disk, not network, so bigger reads just mean fewer of them.
DISK_CHUNK_SIZE = 8 * 1024 * 1024

#: How many byte ranges of one file to fetch at once. Zenodo caps what a
#: single connection gets, well below most curators' own bandwidth, so one
#: stream left a multi-GB snapshot crawling in at a few MB/s; several ranged
#: requests side by side each get their own share of that cap. Kept to a
#: handful, not as many as would go faster still: Zenodo is tightening what
#: it allows automated clients, and a polite client is one it doesn't block.
PARALLEL_CONNECTIONS = 4

#: Below this, a file isn't worth splitting -- the extra requests cost more
#: than they save.
MIN_PARALLEL_BYTES = 32 * 1024 * 1024

#: Attempts per byte range before the whole download is given up on. Each
#: retry resumes from the last byte that range got, so one dropped
#: connection costs a few seconds, not the whole transfer.
RANGE_ATTEMPTS = 4

#: Seconds to wait before retry ``n`` (1-based) is ``RETRY_BACKOFF * 2**(n-1)``,
#: unless the server said how long itself -- see ``_retry_delay``.
RETRY_BACKOFF = 1.0

#: The longest a ``Retry-After`` is honoured for before retrying anyway -- a
#: curator is watching a progress line, not leaving a batch job overnight.
MAX_RETRY_AFTER = 60.0

#: "Too many requests" and "try again shortly": the two answers that mean
#: *slow down*, not *this is broken*.
_RATE_LIMIT_CODES = (429, 503)

_CONTENT_RANGE_TOTAL = re.compile(r"bytes\s+\d+-\d+/(\d+)")


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
    #: The published filename, when known (Zenodo's own ``key``) -- round 4,
    #: item 2's date-named, gzipped published snapshots
    #: (``bold_snapshot_2026-09-11.duckdb.gz``) are told apart from a plain
    #: ``.duckdb`` by this, not by ``url`` alone, which is not guaranteed to
    #: end in the real filename for every host. Falls back to ``url`` itself
    #: when a source (a bare ``--url``, most manifests) doesn't carry one.
    filename: str = ""


def ssl_context() -> ssl.SSLContext:
    """Verify HTTPS against the operating system's own trusted certificates.

    Plain ``urlopen`` verifies against OpenSSL's CA file, found at a path
    compiled into whichever Python built the app. In the frozen macOS build
    that path belongs to the CI runner and doesn't exist on a curator's Mac,
    so every Zenodo request failed with ``CERTIFICATE_VERIFY_FAILED: unable
    to get local issuer certificate`` (a real report, Intel Mac, V3.3).
    ``truststore`` asks the OS instead: the macOS Keychain, the Windows
    certificate store, the usual distro CA bundles on Linux. That also
    picks up an institution's own root certificate if its network inspects
    HTTPS, which a bundled list like ``certifi`` would reject.
    """
    return truststore.SSLContext(ssl.PROTOCOL_TLS_CLIENT)


def _urlopen(url: str, headers: dict[str, str] | None = None):
    request = urllib.request.Request(
        url, headers={"User-Agent": USER_AGENT, **(headers or {})})
    return urllib.request.urlopen(request, timeout=30, context=ssl_context())


def _is_rate_limited(exc: BaseException | None) -> bool:
    return isinstance(exc, HTTPError) and exc.code in _RATE_LIMIT_CODES


def _retry_delay(exc: BaseException | None, attempt: int) -> float:
    """Seconds to wait before retry ``attempt`` (1-based) after ``exc``.

    A 429/503 that says how long to wait (``Retry-After: <seconds>``) gets
    exactly that, up to ``MAX_RETRY_AFTER``; anything else -- a dropped
    connection, or a ``Retry-After`` given as a date -- backs off
    exponentially from ``RETRY_BACKOFF``.
    """
    if _is_rate_limited(exc):
        try:
            asked = float(exc.headers.get("Retry-After", ""))
        except (TypeError, ValueError):
            asked = None
        if asked is not None and asked >= 0:
            return min(asked, MAX_RETRY_AFTER)
    return RETRY_BACKOFF * 2 ** (attempt - 1)


def _rate_limit_message(url: str) -> str:
    return (f"{url} is rate-limiting requests right now (HTTP 429/503). "
            "Wait a few minutes and try again.")


def _urlopen_patiently(url: str, headers: dict[str, str] | None = None):
    """``_urlopen``, but a 429/503 is waited out once before giving up --
    for the one-off requests (the API lookup, the download's first request)
    that ``_fetch_range``'s own retry loop doesn't cover."""
    try:
        return _urlopen(url, headers)
    except HTTPError as exc:
        if not _is_rate_limited(exc):
            raise
        time.sleep(_retry_delay(exc, 1))
    try:
        return _urlopen(url, headers)
    except HTTPError as exc:
        if _is_rate_limited(exc):
            raise FetchError(_rate_limit_message(url)) from exc
        raise


def _get_json(url: str) -> dict:
    try:
        with _urlopen_patiently(url) as resp:
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
        filename=data.get("filename", ""),
    )


def _clean_zenodo_id(record_id: str) -> str:
    """Accept a bare id, a full DOI, or a doi.org/zenodo.org URL alike.

    ``DEFAULT_SNAPSHOT_ZENODO_DOI`` is given as a full DOI (round 4, item 2)
    so it reads the same as the citation on the Zenodo page itself, rather
    than requiring a curator (or this project's own code) to know Zenodo's
    internal numeric id separately.
    """
    match = _ZENODO_ID_IN_DOI.search(record_id)
    if match:
        return match.group(1)
    return record_id.strip().rstrip("/").rsplit("/", 1)[-1]


def resolve_zenodo_record(record_id: str, *, filename: str | None = None) -> Source:
    """Resolve a Zenodo record (or concept) id to one file's URL and checksum.

    A **concept** id (the one that does not change between versions) always
    redirects to the record's latest version, which is what makes this the
    right id to hand out for "always get the newest snapshot" -- a specific
    version id pins to that version forever, which is a deliberate choice
    too, just a different one.

    Republished snapshots are date-named and gzipped (round 4, item 2:
    ``bold_snapshot_2026-09-11.duckdb.gz``) -- matched here the same way a
    plain ``.duckdb`` already was, so a record holding either (or, someday,
    both across versions) resolves the same way with nothing curator-facing
    to change.
    """
    record_id = _clean_zenodo_id(record_id)
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
        matches = [f for f in files
                  if str(f.get("key", "")).endswith((".duckdb", ".duckdb.gz"))]
        if len(matches) != 1:
            available = ", ".join(f.get("key", "?") for f in files)
            raise FetchError(
                f"Record {record_id} has {len(files)} files; pass --filename "
                f"to pick one. Available: {available}")

    entry = matches[0]
    checksum = entry.get("checksum", "")  # Zenodo's own form: "md5:<hex>"
    metadata = data.get("metadata", {})
    filename = str(entry.get("key", ""))

    # ``snapshot_id`` needs to be *comparable* to what's already on disk --
    # ``_local_snapshot_id`` reads the date ``snapshot_builder`` stamped into
    # the file at build time (e.g. "2026-09-11"). Zenodo's own record id
    # (a new one is minted for every version) is never that date, so using
    # it here made ``fetch()``'s "already have this one, skip" check (and
    # this module's own ``check_for_update``) silently never match for a
    # Zenodo-record source -- only a manifest, which supplies its own
    # ``snapshot_id`` field directly, ever actually hit it. Recovered from
    # the published filename instead, which carries the same date by
    # convention (``bold_snapshot_2026-09-11.duckdb.gz``); the record id is
    # kept as a fallback for a file named some other way, so this never
    # raises, just stops being comparable.
    date_match = _SNAPSHOT_DATE_IN_FILENAME.search(filename)
    snapshot_id = date_match.group(1) if date_match else str(data.get("id", record_id))

    return Source(
        url=entry["links"]["self"],
        checksum=checksum or None,
        snapshot_id=snapshot_id,
        schema_version=str(metadata.get("version", "")),
        filename=filename,
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


def _parse_snapshot_date(snapshot_id: str | None) -> date | None:
    """``snapshot_id`` as a real date, or ``None`` when it isn't one.

    Not every ``snapshot_id`` is a date -- ``resolve_zenodo_record`` falls
    back to Zenodo's own record id when a file's name carries no
    ``YYYY-MM-DD`` (its own docstring explains why). Comparing two
    non-dates, or a date against a non-date, has no meaningful direction,
    so callers should treat ``None`` here as "not comparable", not "equal"
    or "different" in either direction.
    """
    if not snapshot_id:
        return None
    try:
        return date.fromisoformat(snapshot_id)
    except ValueError:
        return None


@dataclass
class UpdateCheck:
    """The result of asking Zenodo what's latest, without downloading it."""

    up_to_date: bool
    local_snapshot_id: str | None
    remote_snapshot_id: str
    remote_filename: str

    @property
    def comparison(self) -> str:
        """One of ``"up_to_date"``, ``"remote_newer"``, ``"remote_older"``,
        or ``"different"`` (not equal, but not comparable as dates either --
        e.g. one side is a bare Zenodo record id, not a date-named file).

        ``up_to_date`` (equality) is decided once, in :func:`check_for_update`
        itself -- this only has to work out *which direction* the difference
        goes, for a curator-facing message that shouldn't claim "newer" when
        the local snapshot is actually the more recent one (round found
        during Phylogeny-tab field testing: a local build dated after the
        latest Zenodo publish was reported as having a "newer" one
        available, going backwards in time).
        """
        if self.up_to_date:
            return "up_to_date"
        local_date = _parse_snapshot_date(self.local_snapshot_id)
        remote_date = _parse_snapshot_date(self.remote_snapshot_id)
        if local_date is None or remote_date is None:
            return "different"
        return "remote_newer" if remote_date > local_date else "remote_older"


def check_for_update(record_id: str, local_path: Path) -> UpdateCheck:
    """Ask Zenodo what the latest snapshot is, and compare it to ``local_path``.

    One small API call (``resolve_zenodo_record``), never a download -- for a
    "is a newer snapshot available?" check the app can run any time, not only
    when a curator is already committing to a multi-GB transfer.

    ``record_id`` should be a **concept** id/DOI (``DEFAULT_SNAPSHOT_ZENODO_DOI``)
    so this always compares against the newest published version, not one
    pinned release. Raises :class:`FetchError` on a network failure, the same
    as every other Zenodo-talking function here -- callers already have to
    handle that for the download path, so there is nothing new to catch.

    ``local_path`` not existing, or not being a readable snapshot, reads as
    "no local version to compare" (``local_snapshot_id=None``,
    ``up_to_date=False``) rather than an error -- a curator with no snapshot
    yet still wants to know a snapshot is available, not a crash.
    """
    source = resolve_zenodo_record(record_id)
    local_id = _local_snapshot_id(local_path)
    return UpdateCheck(
        up_to_date=local_id is not None and local_id == source.snapshot_id,
        local_snapshot_id=local_id,
        remote_snapshot_id=source.snapshot_id,
        remote_filename=source.filename,
    )


def _checksum_parts(checksum: str) -> tuple[str, str]:
    algo, _, expected = checksum.partition(":")
    if algo not in ("sha256", "md5"):
        raise FetchError(f"Unsupported checksum kind {algo!r}")
    return algo, expected


def _mismatch(algo: str, expected: str, actual: str) -> FetchError:
    return FetchError(
        f"{algo} mismatch: expected {expected}, got {actual}. The download "
        "is corrupt or the source file changed underneath it -- deleted, "
        "not kept, since a silently wrong snapshot is worse than none.")


def _verify(path: Path, checksum: str) -> None:
    algo, expected = _checksum_parts(checksum)
    digest = hashlib.new(algo)
    with open(path, "rb") as fh:
        while chunk := fh.read(DISK_CHUNK_SIZE):
            digest.update(chunk)
    actual = digest.hexdigest()
    if actual.lower() != expected.lower():
        raise _mismatch(algo, expected, actual)


def _verify_and_decompress(src: Path, checksum: str | None, dst: Path, *,
                           progress=print) -> None:
    """Check ``src`` against ``checksum`` and gunzip it into ``dst``, in one
    read of ``src`` rather than one pass for each -- a snapshot can be
    gigabytes, so every extra pass over it is time a curator sits waiting.

    The checksum is still of the compressed bytes (what Zenodo or a manifest
    publishes) and still decides the outcome: a mismatch deletes ``dst`` even
    though it was written alongside, and a download that is corrupt enough
    to break decompression part-way is reported as the mismatch it really
    is, not as a gzip error, whenever a checksum was given.
    """
    digest = None
    if checksum:
        algo, expected = _checksum_parts(checksum)
        digest = hashlib.new(algo)
    # 16 + MAX_WBITS: expect a gzip header and trailer, not raw zlib.
    inflater = zlib.decompressobj(16 + zlib.MAX_WBITS)
    decompress_error: str | None = None
    written = 0
    try:
        with open(src, "rb") as fh_in, open(dst, "wb") as fh_out:
            while chunk := fh_in.read(DISK_CHUNK_SIZE):
                if digest is not None:
                    digest.update(chunk)
                if decompress_error is not None:
                    continue  # keep hashing, so a mismatch is still reported
                try:
                    while chunk:
                        data = inflater.decompress(chunk)
                        fh_out.write(data)
                        written += len(data)
                        # A gzip file may be several members back to back
                        # (what ``gzip.open`` accepts too): start a fresh
                        # decompressor on whatever follows the one that ended.
                        chunk = inflater.unused_data if inflater.eof else b""
                        if chunk:
                            inflater = zlib.decompressobj(16 + zlib.MAX_WBITS)
                except zlib.error as exc:
                    decompress_error = str(exc)
                    continue
                progress(f"\rVerifying and decompressing... "
                         f"{written / 1e6:.0f} MB", end="")
            if decompress_error is None:
                fh_out.write(inflater.flush())
                if not inflater.eof:
                    decompress_error = "the file ends part-way through (truncated)"
        progress("")

        if digest is not None:
            actual = digest.hexdigest()
            if actual.lower() != expected.lower():
                raise _mismatch(algo, expected, actual)
        if decompress_error is not None:
            raise FetchError(f"Could not decompress the download: {decompress_error}")
    except BaseException:
        dst.unlink(missing_ok=True)
        raise


def _report(progress, written: int, total: int) -> None:
    if total:
        progress(f"\r{written / total:.0%} "
                 f"({written / 1e6:.0f} / {total / 1e6:.0f} MB)", end="")
    else:
        progress(f"\r{written / 1e6:.0f} MB", end="")


def _stream_to(resp, tmp: Path, total: int, progress) -> None:
    """Today's one-connection path: copy ``resp`` into ``tmp`` start to end."""
    written = 0
    with open(tmp, "wb") as fh:
        while chunk := resp.read(CHUNK_SIZE):
            fh.write(chunk)
            written += len(chunk)
            _report(progress, written, total)


def _ranged_total(resp) -> int | None:
    """The full file size from a ``206``'s ``Content-Range``, or ``None``
    when the server didn't honour the range (``200``) or didn't say."""
    if getattr(resp, "status", None) != 206:
        return None
    match = _CONTENT_RANGE_TOTAL.match(resp.headers.get("Content-Range") or "")
    return int(match.group(1)) if match else None


def _fetch_range(url: str, tmp: Path, start: int, end: int, *,
                 on_bytes, abort: threading.Event) -> None:
    """Fetch bytes ``start..end`` (inclusive) of ``url`` into the same
    offsets of ``tmp``, retrying from wherever a failed attempt left off."""
    pos = start
    last_error: Exception | None = None
    for attempt in range(RANGE_ATTEMPTS):
        if abort.is_set():
            return
        if attempt:
            time.sleep(_retry_delay(last_error, attempt))
        try:
            with _urlopen(url, {"Range": f"bytes={pos}-{end}"}) as resp:
                if getattr(resp, "status", None) != 206:
                    raise FetchError(
                        f"server stopped honouring byte ranges "
                        f"(HTTP {getattr(resp, 'status', '?')})")
                with open(tmp, "r+b") as fh:
                    fh.seek(pos)
                    while pos <= end:
                        if abort.is_set():
                            return
                        chunk = resp.read(min(CHUNK_SIZE, end - pos + 1))
                        if not chunk:
                            break
                        fh.write(chunk)
                        pos += len(chunk)
                        on_bytes(len(chunk))
            if pos > end:
                return
            last_error = FetchError(f"connection closed at byte {pos} of {end + 1}")
        except (HTTPError, URLError, OSError, FetchError) as exc:
            last_error = exc
    if _is_rate_limited(last_error):
        raise FetchError(_rate_limit_message(url)) from last_error
    raise FetchError(
        f"bytes {start}-{end} failed after {RANGE_ATTEMPTS} attempts: {last_error}")


def _download_parallel(url: str, tmp: Path, total: int, progress) -> None:
    """Fetch ``url`` as ``PARALLEL_CONNECTIONS`` byte ranges at once, each
    written straight into its place in a pre-sized ``tmp``."""
    with open(tmp, "wb") as fh:
        fh.truncate(total)

    step = -(-total // PARALLEL_CONNECTIONS)  # ceiling division
    ranges = [(start, min(start + step, total) - 1)
              for start in range(0, total, step)]

    lock = threading.Lock()
    written = 0

    def on_bytes(n: int) -> None:
        nonlocal written
        with lock:
            written += n

    abort = threading.Event()
    with ThreadPoolExecutor(max_workers=len(ranges)) as pool:
        futures = [pool.submit(_fetch_range, url, tmp, a, b,
                               on_bytes=on_bytes, abort=abort)
                   for a, b in ranges]
        pending = set(futures)
        try:
            # Progress is reported from this thread only, never a worker --
            # both UIs' ``progress`` callbacks were written for one caller.
            while pending:
                done, pending = wait(pending, timeout=0.25,
                                     return_when=FIRST_EXCEPTION)
                _report(progress, written, total)
                for future in done:
                    future.result()  # re-raises a range that gave up
        except BaseException:
            abort.set()
            raise
    progress("")


def download(source: Source, out: Path, *, progress=print) -> Path:
    """Stream ``source.url`` to a temp file beside ``out``, verify, rename.

    The temp file (not ``out`` itself) is what a failed or interrupted
    download leaves behind, so ``out`` is never observed half-written.

    The first request asks for a single byte. A host that answers with a
    ``206`` and the file's full size (Zenodo does) gets the file fetched as
    several byte ranges in parallel -- see ``PARALLEL_CONNECTIONS``; one
    that ignores ``Range`` just sends the whole file back, which is then
    streamed as before, so nothing is lost to the check either way.

    Round 4, item 2: a published snapshot may be gzipped
    (``bold_snapshot_2026-09-11.duckdb.gz``) -- detected from
    ``source.filename`` (Zenodo's own name for the file) or, failing that,
    ``source.url`` itself. The checksum Zenodo (or a manifest) publishes is
    for the file as uploaded, so it is checked against the *compressed*
    download, in the same pass that decompresses it into ``out``.
    """
    is_gzipped = (source.filename or source.url).split("?")[0].endswith(".gz")
    tmp = out.with_suffix(out.suffix + (".gz.part" if is_gzipped else ".part"))
    out.parent.mkdir(parents=True, exist_ok=True)

    try:
        with _urlopen_patiently(source.url, {"Range": "bytes=0-0"}) as resp:
            total = _ranged_total(resp)
            if total is None:
                # Range ignored: this *is* the whole file, so use it.
                _stream_to(resp, tmp,
                           int(resp.headers.get("Content-Length") or 0), progress)
                progress("")
        if total is not None:
            if total >= MIN_PARALLEL_BYTES:
                _download_parallel(source.url, tmp, total, progress)
            else:
                with _urlopen_patiently(source.url) as resp:
                    _stream_to(resp, tmp, total, progress)
                progress("")
    except (HTTPError, URLError, OSError) as exc:
        tmp.unlink(missing_ok=True)
        raise FetchError(f"Download failed: {exc}") from exc
    except BaseException:
        tmp.unlink(missing_ok=True)
        raise

    if not source.checksum:
        progress("No checksum given -- integrity of this download is NOT verified.")

    if is_gzipped:
        decompressed = out.with_suffix(out.suffix + ".part")
        try:
            _verify_and_decompress(tmp, source.checksum, decompressed,
                                   progress=progress)
        except (FetchError, OSError) as exc:
            tmp.unlink(missing_ok=True)
            if isinstance(exc, FetchError):
                raise
            raise FetchError(f"Could not decompress the download: {exc}") from exc
        tmp.unlink()
        decompressed.replace(out)
    else:
        if source.checksum:
            progress("Verifying checksum...")
            try:
                _verify(tmp, source.checksum)
            except FetchError:
                tmp.unlink(missing_ok=True)
                raise
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
