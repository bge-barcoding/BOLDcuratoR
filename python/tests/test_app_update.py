"""The "is a newer BOLDcurator out?" check (``boldcurator.app_update``).

Zenodo is never reached: ``fetch_snapshot._urlopen`` is replaced with a fake
that serves a concept record's JSON (or fails the way a network does), and
the config file lives in ``tmp_path``.
"""

from __future__ import annotations

import io
import json
import socket
import time
from datetime import datetime, timedelta, timezone
from urllib.error import HTTPError, URLError

import pytest

from boldcurator import app_update, desktop
from boldcurator.build import fetch_snapshot as fs

CONCEPT = "10.5281/zenodo.99999"
NOW = datetime(2026, 10, 1, 12, 0, tzinfo=timezone.utc)


class _Response(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


@pytest.fixture
def zenodo(monkeypatch, tmp_path):
    """A fake concept record whose latest release is ``state["tag"]``;
    ``state["error"]``, when set, is raised instead. Automatic checks are
    back on, against a config in ``tmp_path``."""
    state = {"tag": "v3.6.0", "error": None, "calls": [], "body": None}

    def fake_urlopen(url, headers=None, *, timeout=30):
        state["calls"].append((url, timeout))
        if state["error"] is not None:
            raise state["error"]
        body = state["body"] if state["body"] is not None else json.dumps(
            {"id": 100, "metadata": {"version": state["tag"]}}).encode()
        return _Response(body)

    monkeypatch.setattr(fs, "_urlopen", fake_urlopen)
    monkeypatch.setattr(app_update, "APP_ZENODO_CONCEPT_DOI", CONCEPT)
    monkeypatch.setattr(app_update, "CONFIG_PATH", tmp_path / "config.json")
    monkeypatch.delenv(app_update.DISABLE_ENV, raising=False)
    monkeypatch.setattr(app_update, "_background",
                        {"started": False, "done": False, "status": None})
    return state


@pytest.mark.parametrize("text, expected", [
    ("v3.5.1", (3, 5, 1)),
    ("V3.3", (3, 3, 0)),
    ("3.5.1", (3, 5, 1)),
    ("v4", (4, 0, 0)),
    (" v3.10.2 ", (3, 10, 2)),
    ("0.0.0.dev0", None),
    ("0.0.0+unknown", None),
    ("0.0.0", None),
    ("latest", None),
    ("", None),
    (None, None),
])
def test_parse_version(text, expected):
    assert app_update.parse_version(text) == expected


def test_versions_compare_numerically_not_as_text():
    latest = app_update.Latest(version="3.10.0", tag="v3.10")
    assert app_update.UpdateStatus("3.9.0", latest).newer
    assert not app_update.UpdateStatus("3.10.0", latest).newer
    assert not app_update.UpdateStatus("3.11.0", latest).newer
    # A development build has nothing to compare -- never "update".
    assert not app_update.UpdateStatus("0.0.0.dev0", latest).newer


def test_fetch_latest_reads_the_concept_records_version(zenodo):
    zenodo["tag"] = "V3.7"
    latest = app_update.fetch_latest()
    assert latest == app_update.Latest(version="3.7.0", tag="V3.7")
    # The concept record, by its bare id, with the short timeout.
    assert zenodo["calls"] == [(fs.ZENODO_API.format(record_id="99999"),
                                app_update.TIMEOUT)]
    # The tag keeps its own case: GitHub's tag URLs are case-sensitive.
    assert latest.notes_url.endswith("/releases/tag/V3.7")


@pytest.mark.parametrize("error", [
    URLError("no route to host"),
    socket.timeout("timed out"),
    HTTPError("https://zenodo.org", 429, "Too Many Requests", {}, None),
    HTTPError("https://zenodo.org", 500, "Server Error", {}, None),
])
def test_fetch_latest_turns_network_failures_into_fetch_errors(zenodo, error):
    zenodo["error"] = error
    with pytest.raises(fs.FetchError):
        app_update.fetch_latest()


@pytest.mark.parametrize("body", [b"<html>not json</html>", b'{"metadata": {}}',
                                  b'{"metadata": {"version": "nightly"}}'])
def test_fetch_latest_rejects_a_reply_without_a_usable_version(zenodo, body):
    zenodo["body"] = body
    with pytest.raises(fs.FetchError):
        app_update.fetch_latest()


def test_check_reports_a_newer_release_and_caches_it(zenodo, tmp_path):
    status = app_update.check("3.5.1", now=NOW)
    assert status.newer and status.latest.version == "3.6.0"
    cached = desktop.load_config(tmp_path / "config.json")["update_check"]
    assert cached["latest_version"] == "3.6.0" and cached["latest_tag"] == "v3.6.0"

    # Within the interval: answered from the cache, Zenodo not asked again.
    later = NOW + timedelta(hours=app_update.UPDATE_CHECK_INTERVAL_HOURS - 1)
    assert app_update.check("3.5.1", now=later) == status
    assert len(zenodo["calls"]) == 1

    # Past it, or forced: asked again.
    app_update.check("3.5.1", now=NOW + timedelta(hours=25))
    app_update.check("3.5.1", force=True, now=NOW + timedelta(hours=25))
    assert len(zenodo["calls"]) == 3


def test_a_clock_that_went_backwards_does_not_trust_the_cache(zenodo):
    app_update.check("3.5.1", now=NOW)
    app_update.check("3.5.1", now=NOW - timedelta(days=3))
    assert len(zenodo["calls"]) == 2


def test_check_keeps_the_rest_of_config_json(zenodo, tmp_path):
    config = tmp_path / "config.json"
    desktop.save_snapshot_path(tmp_path / "snap.duckdb", config)
    app_update.check("3.5.1", now=NOW)
    assert desktop.load_config(config)["snapshot_path"] == str(tmp_path / "snap.duckdb")


def test_check_says_nothing_when_up_to_date_or_ahead(zenodo):
    assert not app_update.check("3.6.0", now=NOW).newer
    assert not app_update.check("3.7.0", force=True, now=NOW).newer


@pytest.mark.parametrize("reason", ["no_doi", "dev_build", "env", "config"])
def test_check_is_skipped(zenodo, monkeypatch, tmp_path, reason):
    current = "3.5.1"
    if reason == "no_doi":
        monkeypatch.setattr(app_update, "APP_ZENODO_CONCEPT_DOI", "")
    elif reason == "dev_build":
        current = "0.0.0.dev0"
    elif reason == "env":
        monkeypatch.setenv(app_update.DISABLE_ENV, "1")
    else:
        desktop.update_config(tmp_path / "config.json", check_for_updates=False)
    assert app_update.check(current, now=NOW) is None
    assert zenodo["calls"] == []


def test_the_opt_out_does_not_stop_a_check_the_curator_asked_for(zenodo, monkeypatch):
    monkeypatch.setenv(app_update.DISABLE_ENV, "1")
    message, status = app_update.manual_check("3.5.1")
    assert status is not None and "3.6.0 is available" in message


def test_an_automatic_check_is_silent_offline_but_a_forced_one_says_so(zenodo):
    zenodo["error"] = URLError("offline")
    assert app_update.check("3.5.1", now=NOW) is None
    with pytest.raises(fs.FetchError):
        app_update.check("3.5.1", force=True, now=NOW)
    message, status = app_update.manual_check("3.5.1")
    assert status is None and message.startswith("Could not check")


def test_manual_check_messages(zenodo, monkeypatch):
    assert "Up to date" in app_update.manual_check("3.6.0")[0]
    assert "newer than the latest" in app_update.manual_check("3.7.0")[0]
    assert "development build" in app_update.manual_check("0.0.0.dev0")[0]
    monkeypatch.setattr(app_update, "APP_ZENODO_CONCEPT_DOI", "")
    assert "aren't set up" in app_update.manual_check("3.5.1")[0]


def test_a_manual_check_that_finds_a_release_feeds_the_banner(zenodo):
    assert app_update.background_result() == (False, None)
    app_update.manual_check("3.5.1")
    done, status = app_update.background_result()
    assert done and status.latest.version == "3.6.0"


def test_dismissing_hides_that_version_only(zenodo, tmp_path):
    status = app_update.check("3.5.1", now=NOW)
    assert app_update.should_notify(status)
    app_update.dismiss("3.6.0")
    assert not app_update.should_notify(status)
    newer = app_update.UpdateStatus("3.5.1", app_update.Latest("3.7.0", "v3.7.0"))
    assert app_update.should_notify(newer)
    assert not app_update.should_notify(None)


def test_the_background_check_runs_once_per_process(zenodo, monkeypatch):
    calls = []
    monkeypatch.setattr(app_update, "check", lambda: calls.append(1) or None)
    app_update.start_background_check()
    app_update.start_background_check()
    for _ in range(200):
        if app_update.background_result()[0]:
            break
        time.sleep(0.01)
    assert app_update.background_result() == (True, None)
    assert calls == [1]


def test_the_background_check_survives_anything(zenodo, monkeypatch):
    def boom():
        raise RuntimeError("unexpected")

    monkeypatch.setattr(app_update, "check", boom)
    app_update.start_background_check()
    for _ in range(200):
        if app_update.background_result()[0]:
            break
        time.sleep(0.01)
    assert app_update.background_result() == (True, None)


def test_how_to_update_matches_the_install(tmp_path):
    installed = tmp_path / "installed"
    installed.mkdir()
    (installed / "unins000.exe").write_bytes(b"")
    portable = tmp_path / "portable"
    portable.mkdir()

    assert "uv tool upgrade boldcurator" in app_update.how_to_update(frozen=False)
    assert "installer" in app_update.how_to_update(
        frozen=True, platform="win32", executable=str(installed / "boldcurator.exe"))
    assert "empty folder" in app_update.how_to_update(
        frozen=True, platform="win32", executable=str(portable / "boldcurator.exe"))
    assert "BOLDcurator.app" in app_update.how_to_update(frozen=True, platform="darwin")
    assert "empty folder" in app_update.how_to_update(frozen=True, platform="linux")

    assert app_update.download_url(frozen=True) == app_update.APP_DOWNLOAD_URL
    assert app_update.download_url(frozen=False) is None
