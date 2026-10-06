"""Is a newer BOLDcurator release out? Asked of Zenodo, the one host the app
already talks to.

The GitHub-Zenodo integration archives every GitHub release under one
concept DOI (``APP_ZENODO_CONCEPT_DOI``), which always resolves to the newest
archived release -- the same mechanism ``DEFAULT_SNAPSHOT_ZENODO_DOI`` uses
for snapshots. That record's ``metadata.version`` is the release tag
(``v3.5.1``), padded to ``x.y.z`` here exactly as ``packaging/stamp_version.py``
pads it into the package version, so the two compare like for like.

Notify only: this never downloads or installs anything. How to update
depends on how the app was installed (``how_to_update``).

* **Automatic** (``check``, run by ``start_background_check`` when the app
  opens): at most once per ``UPDATE_CHECK_INTERVAL_HOURS``, with a short
  timeout and no retry, and silent on any failure -- being offline is
  normal for this app. Off with ``"check_for_updates": false`` in
  ``~/.boldcurator/config.json`` or ``BOLDCURATOR_NO_UPDATE_CHECK=1``.
* **On request** (``manual_check``: the Data tab's button, ``boldcurator
  check-update``): always asks afresh and says what happened, including
  failures. It runs even with automatic checks switched off -- the curator
  asked.

A development build (``0.0.0.dev0``, or ``0.0.0+unknown`` with no package
metadata) never checks on its own: there is no release to compare it with.

What Zenodo sees is one GET with the same ``BOLDcurator/<version>``
User-Agent as the snapshot download, nothing else.
"""

from __future__ import annotations

import json
import os
import re
import sys
import threading
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.error import HTTPError, URLError

from . import __version__
from .build import fetch_snapshot as fs
from .config.constants import (
    APP_RELEASE_NOTES_URL,
    APP_ZENODO_CONCEPT_DOI,
    UPDATE_CHECK_INTERVAL_HOURS,
)
from .desktop import DEFAULT_CONFIG_PATH, load_config, update_config

#: Where the check's cache, the opt-out and the dismissed version live --
#: the desktop app's own config file. A module attribute, not a default
#: argument, so tests can point it somewhere else.
CONFIG_PATH = DEFAULT_CONFIG_PATH

DISABLE_ENV = "BOLDCURATOR_NO_UPDATE_CHECK"

#: Seconds. Short on purpose: the automatic check runs as the app opens, and
#: a slow network is better answered with "no banner" than with a stuck
#: thread. ``fetch_snapshot``'s own 30 s (and its wait on ``Retry-After``)
#: is for requests a curator is actively waiting on.
TIMEOUT = 5.0

#: The same shapes ``packaging/stamp_version.py`` accepts for a release tag.
_VERSION = re.compile(r"^[vV]?(\d+)(?:\.(\d+))?(?:\.(\d+))?$")


def parse_version(text: str | None) -> tuple[int, int, int] | None:
    """``v3.5.1``/``V3.3``/``3.5.1`` -> ``(3, 5, 1)``/``(3, 3, 0)``/...

    ``None`` for anything else, including every development build's version
    (``0.0.0.dev0``, ``0.0.0+unknown``): those are never compared.
    """
    match = _VERSION.match((text or "").strip())
    if not match:
        return None
    parsed = tuple(int(part or 0) for part in match.groups())
    return None if parsed == (0, 0, 0) else parsed  # type: ignore[return-value]


def _fmt(version: tuple[int, int, int]) -> str:
    return ".".join(str(part) for part in version)


@dataclass(frozen=True)
class Latest:
    """The newest release Zenodo has archived."""

    #: Padded ``x.y.z``, comparable to ``__version__``.
    version: str
    #: The tag exactly as released (``v3.5.1``, ``V3.3``) -- for its URL.
    tag: str

    @property
    def notes_url(self) -> str:
        return APP_RELEASE_NOTES_URL.format(tag=self.tag)


@dataclass(frozen=True)
class UpdateStatus:
    #: This app's version, as it reports it.
    current: str
    latest: Latest

    @property
    def newer(self) -> bool:
        """Is the latest release newer than this app? Never true for a
        development build, which has nothing to compare."""
        current = parse_version(self.current)
        latest = parse_version(self.latest.version)
        return current is not None and latest is not None and latest > current


def fetch_latest(concept_doi: str | None = None, *,
                 timeout: float = TIMEOUT) -> Latest:
    """One GET on the concept record. Raises :class:`fetch_snapshot.FetchError`
    for anything short of a usable version -- no retries, no waiting on a
    ``Retry-After``: an update check is never worth holding anything up."""
    concept_doi = concept_doi or APP_ZENODO_CONCEPT_DOI
    if not concept_doi:
        raise fs.FetchError("no Zenodo record is set up for app updates "
                            "in this build")
    url = fs.ZENODO_API.format(record_id=fs._clean_zenodo_id(concept_doi))
    try:
        with fs._urlopen(url, timeout=timeout) as resp:
            data = json.loads(resp.read().decode("utf-8"))
        tag = str(data["metadata"]["version"]).strip()
    except HTTPError as exc:
        if fs._is_rate_limited(exc):
            raise fs.FetchError(fs._rate_limit_message(url)) from exc
        raise fs.FetchError(f"Could not reach {url}: {exc}") from exc
    except (URLError, OSError) as exc:  # includes a timeout
        raise fs.FetchError(f"Could not reach {url}: {exc}") from exc
    except (ValueError, KeyError, TypeError) as exc:  # not JSON / no version
        raise fs.FetchError(f"Unexpected reply from {url}: {exc!r}") from exc
    parsed = parse_version(tag)
    if parsed is None:
        raise fs.FetchError(
            f"Zenodo's latest release is labelled {tag!r}, not vX.Y.Z")
    return Latest(version=_fmt(parsed), tag=tag)


def automatic_checks_enabled(config: dict) -> bool:
    if os.environ.get(DISABLE_ENV, "").strip() not in ("", "0"):
        return False
    return config.get("check_for_updates", True) is not False


def _cached(config: dict, now: datetime) -> Latest | None:
    """The last check's answer, if it was recent enough to reuse."""
    entry = config.get("update_check")
    if not isinstance(entry, dict):
        return None
    try:
        checked_at = datetime.fromisoformat(entry["checked_at"])
        latest = Latest(version=str(entry["latest_version"]),
                        tag=str(entry["latest_tag"]))
        age = now - checked_at
    except (KeyError, TypeError, ValueError):
        return None
    # A negative age (the clock went backwards) re-checks rather than
    # trusting the cache indefinitely.
    if not timedelta(0) <= age < timedelta(hours=UPDATE_CHECK_INTERVAL_HOURS):
        return None
    return latest if parse_version(latest.version) else None


def check(current: str | None = None, *, force: bool = False,
          config_path: Path | None = None,
          now: datetime | None = None) -> UpdateStatus | None:
    """What's the latest release, compared to this app?

    ``None`` when there is nothing to say: no concept DOI in this build, a
    development build, automatic checks switched off (unless ``force``), or
    -- unless ``force`` -- Zenodo couldn't be reached. With ``force``, the
    cache and the opt-out are skipped and a failure raises ``FetchError``.
    """
    current = __version__ if current is None else current
    config_path = config_path or CONFIG_PATH
    if not APP_ZENODO_CONCEPT_DOI or parse_version(current) is None:
        return None
    config = load_config(config_path)
    now = now or datetime.now(timezone.utc)
    if not force:
        if not automatic_checks_enabled(config):
            return None
        cached = _cached(config, now)
        if cached is not None:
            return UpdateStatus(current, cached)
    try:
        latest = fetch_latest()
    except fs.FetchError:
        if force:
            raise
        return None
    try:
        update_config(config_path, update_check={
            "checked_at": now.isoformat(),
            "latest_version": latest.version,
            "latest_tag": latest.tag,
        })
    except OSError:
        pass  # an unwritable config only means asking again next time
    return UpdateStatus(current, latest)


def dismiss(version: str, config_path: Path | None = None) -> None:
    """Stop the banner for this version only; a later release shows again."""
    update_config(config_path or CONFIG_PATH, dismissed_version=version)


def should_notify(status: UpdateStatus | None,
                  config_path: Path | None = None) -> bool:
    if status is None or not status.newer:
        return False
    dismissed = load_config(config_path or CONFIG_PATH).get("dismissed_version")
    return dismissed != status.latest.version


def _installed_by_installer(executable: str | None = None) -> bool:
    """Inno Setup puts its uninstaller next to the app; a portable zip has
    none. (``packaging/windows-installer.iss``)"""
    return (Path(executable or sys.executable).parent / "unins000.exe").exists()


def how_to_update(*, frozen: bool | None = None, platform: str | None = None,
                  executable: str | None = None) -> str:
    """One line on how to get the new release -- for this install."""
    frozen = getattr(sys, "frozen", False) if frozen is None else frozen
    platform = platform or sys.platform
    if not frozen:
        return ("To update, run: uv tool upgrade boldcurator "
                "(or pip install --upgrade boldcurator, if you installed "
                "it with pip).")
    if platform.startswith("win") and _installed_by_installer(executable):
        return ("To update, download and run the new Windows installer; it "
                "replaces this version.")
    if platform == "darwin":
        return ("To update, download the new macOS zip and replace "
                "BOLDcurator.app with the one inside it.")
    return ("To update, download the new zip and extract it into an empty "
            "folder -- not over this copy.")


# -- the automatic check, once per app process -------------------------------
#
# Written by the one background thread, read by any number of Shiny
# sessions' render functions (which poll it -- see ui/app.py). Plain values
# only: reactive.Value.set() from another thread is unsafe.
_background: dict = {"started": False, "done": False, "status": None}
_background_lock = threading.Lock()


def start_background_check() -> None:
    """Run ``check()`` on a daemon thread, once per process however many
    browser sessions open."""
    with _background_lock:
        if _background["started"]:
            return
        _background["started"] = True

    def run() -> None:
        try:
            _background["status"] = check()
        except Exception:  # noqa: BLE001 -- an update check must never break the app
            _background["status"] = None
        finally:
            _background["done"] = True

    threading.Thread(target=run, daemon=True,
                     name="boldcurator-update-check").start()


def background_result() -> tuple[bool, UpdateStatus | None]:
    """``(finished, status)`` of the automatic check, or of a later
    ``manual_check`` that found a release."""
    return _background["done"], _background["status"]


def manual_check(current: str | None = None, *,
                 config_path: Path | None = None) -> tuple[str, UpdateStatus | None]:
    """A curator asked: always ask Zenodo, and always say something."""
    current = __version__ if current is None else current
    if not APP_ZENODO_CONCEPT_DOI:
        return ("Update checks aren't set up in this build of BOLDcurator.",
                None)
    try:
        if parse_version(current) is None:
            latest = fetch_latest()
            return (f"This is a development build ({current}); the latest "
                    f"release is {latest.version}.", None)
        status = check(current, force=True, config_path=config_path)
    except fs.FetchError as exc:
        return f"Could not check for an app update: {exc}", None
    assert status is not None  # force=True with a release version
    _background.update(done=True, status=status)
    latest = status.latest.version
    if status.newer:
        return (f"BOLDcurator {latest} is available (you have {current}). "
                f"{how_to_update()}", status)
    if parse_version(current) == parse_version(latest):
        return f"Up to date: {current} is the latest release.", status
    return (f"This version ({current}) is newer than the latest published "
            f"release ({latest}).", status)
