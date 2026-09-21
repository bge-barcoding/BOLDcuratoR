"""The first-run setup screen (plan 4.2) -- round 4 additions.

``create_setup_app``'s server closures are exercised live via
``tools/drive_ui.py``-style manual runs, same as ``ui/app.py``'s (see
``PROGRESS.md``); what is unit-testable here is the plain functions the
server calls into.
"""

from __future__ import annotations

import queue

import pytest

from boldcurator.ui import setup


def test_looks_like_a_manifest_needs_a_json_url():
    assert setup._looks_like_a_manifest("https://example.org/manifest.json")
    assert not setup._looks_like_a_manifest("https://example.org/snapshot.duckdb")
    assert not setup._looks_like_a_manifest("22849516")
    assert not setup._looks_like_a_manifest("manifest.json")  # no scheme


def test_the_setup_app_builds(tmp_path):
    resolved: "queue.Queue" = queue.Queue()
    app = setup.create_setup_app(resolved)
    assert app is not None


def test_the_app_version_is_on_the_setup_page():
    from boldcurator import __version__

    resolved: "queue.Queue" = queue.Queue()
    app = setup.create_setup_app(resolved)
    assert f"v{__version__}" in app.ui["html"]


def test_pick_snapshot_file_returns_none_when_the_helper_process_hangs(
    monkeypatch
):
    """A dialog that never returns (no display, a wedged Tk) must not hang
    the setup screen forever -- it times out and reports "nothing chosen",
    same as a plain Cancel."""

    class HangingProcess:
        def __init__(self, *a, **kw):
            self._alive = True

        def start(self):
            pass

        def join(self, timeout=None):
            pass  # never actually finishes

        def is_alive(self):
            return True

        def terminate(self):
            self._alive = False

    class FakeContext:
        def Queue(self):
            return queue.Queue()

        def Process(self, *a, **kw):
            return HangingProcess()

    monkeypatch.setattr(setup.multiprocessing, "get_context",
                        lambda name: FakeContext())

    assert setup._pick_snapshot_file() is None


def test_pick_snapshot_file_returns_none_when_multiprocessing_is_unavailable(
    monkeypatch
):
    def boom(name):
        raise OSError("no subprocess support here")

    monkeypatch.setattr(setup.multiprocessing, "get_context", boom)
    assert setup._pick_snapshot_file() is None


def test_pick_snapshot_file_returns_the_chosen_path(monkeypatch):
    class FakeProcess:
        def __init__(self, target, args, daemon):
            self._queue = args[0]

        def start(self):
            self._queue.put("/tmp/chosen.duckdb")

        def join(self, timeout=None):
            pass

        def is_alive(self):
            return False

    class FakeContext:
        def Queue(self):
            return queue.Queue()

        def Process(self, target, args, daemon):
            return FakeProcess(target, args, daemon)

    monkeypatch.setattr(setup.multiprocessing, "get_context",
                        lambda name: FakeContext())

    assert setup._pick_snapshot_file() == "/tmp/chosen.duckdb"


def test_default_zenodo_doi_is_wired_into_the_download_panel():
    """Round 4, item 2: the "download one" tab must not require typing a
    Zenodo id -- it names ``DEFAULT_SNAPSHOT_ZENODO_DOI`` on the page."""
    from boldcurator.config.constants import DEFAULT_SNAPSHOT_ZENODO_DOI

    resolved: "queue.Queue" = queue.Queue()
    app = setup.create_setup_app(resolved)
    assert DEFAULT_SNAPSHOT_ZENODO_DOI in app.ui["html"]


def test_the_browse_button_and_file_input_are_on_the_page():
    """Round 4, item 1: a native file browser option, not just a text box."""
    resolved: "queue.Queue" = queue.Queue()
    app = setup.create_setup_app(resolved)
    html = app.ui["html"]
    assert "Browse" in html
    assert 'id="path"' in html
