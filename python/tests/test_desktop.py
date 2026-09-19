"""Plan 4.1a/4.2: the packaged desktop launcher.

``desktop.py`` imports ``pywebview`` lazily, precisely so this file can test
config persistence and server lifecycle without it (or its native backend,
which this sandbox does not have -- WebKitGTK/Qt on Linux, present by
default only on macOS/Windows). The window-opening functions are exercised
by injecting a small fake ``webview`` module into ``sys.modules`` and
asserting the right sequence of calls happens, not by opening a real window.
"""

from __future__ import annotations

import sys
import types
import urllib.request
from pathlib import Path

import pytest

from boldcurator import desktop


def test_load_snapshot_path_with_no_config_file(tmp_path):
    assert desktop.load_snapshot_path(tmp_path / "missing.json") is None


def test_save_then_load_round_trips(tmp_path):
    config = tmp_path / "config.json"
    snapshot = tmp_path / "snap.duckdb"
    snapshot.write_bytes(b"x")

    desktop.save_snapshot_path(snapshot, config)
    assert desktop.load_snapshot_path(config) == snapshot


def test_load_ignores_a_path_that_no_longer_exists(tmp_path):
    config = tmp_path / "config.json"
    desktop.save_snapshot_path(tmp_path / "gone.duckdb", config)
    assert desktop.load_snapshot_path(config) is None


def test_load_ignores_a_corrupt_config_file(tmp_path):
    config = tmp_path / "config.json"
    config.write_text("not json")
    assert desktop.load_snapshot_path(config) is None


def test_run_server_serves_the_app_and_stop_shuts_it_down():
    async def app(scope, receive, send):
        await send({"type": "http.response.start", "status": 200,
                    "headers": [(b"content-type", b"text/plain")]})
        await send({"type": "http.response.body", "body": b"ok"})

    url, stop = desktop.run_server(app)
    try:
        with urllib.request.urlopen(url, timeout=5) as resp:
            assert resp.read() == b"ok"
    finally:
        stop()

    with pytest.raises(Exception):
        urllib.request.urlopen(url, timeout=1)


@pytest.fixture
def fake_webview(monkeypatch):
    """A minimal stand-in for pywebview's public surface.

    Records every window created and every ``start()``/``destroy()`` call so
    a test can assert the launch sequence without a real window.
    """
    calls: list = []

    class FakeEvents:
        def __init__(self):
            self.closed = _EventSlot()

    class _EventSlot:
        def __init__(self):
            self._handlers = []

        def __iadd__(self, handler):
            self._handlers.append(handler)
            return self

        def fire(self):
            for h in self._handlers:
                h()

    class FakeWindow:
        def __init__(self, title, url, **kw):
            self.title = title
            self.url = url
            self.events = FakeEvents()
            calls.append(("create_window", title, url))

        def destroy(self):
            calls.append(("destroy", self.title))

    module = types.ModuleType("webview")
    module.calls = calls
    module.create_window = lambda title, url, **kw: FakeWindow(title, url, **kw)
    module.start = lambda: calls.append(("start",))
    monkeypatch.setitem(sys.modules, "webview", module)
    return module


def test_launch_with_an_explicit_snapshot_skips_setup(
    fake_webview, tmp_path, store, monkeypatch
):
    config = tmp_path / "config.json"
    called_with = {}

    def fake_run_server(app, **kw):
        called_with["app"] = app
        return "http://x/", lambda: None

    monkeypatch.setattr(desktop, "run_server", fake_run_server)

    desktop.launch(store.path, config_path=config)

    assert desktop.load_snapshot_path(config) == store.path
    assert "app" in called_with  # create_app actually ran, not skipped
    assert ("create_window", "BOLDcurator", "http://x/") in fake_webview.calls
    assert ("start",) in fake_webview.calls


def test_launch_uses_the_saved_path_without_reprompting(
    fake_webview, tmp_path, store, monkeypatch
):
    config = tmp_path / "config.json"
    desktop.save_snapshot_path(store.path, config)
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))

    desktop.launch(config_path=config)

    # Only the main window opens -- no setup window was needed.
    windows = [c for c in fake_webview.calls if c[0] == "create_window"]
    assert windows == [("create_window", "BOLDcurator", "http://x/")]


def test_launch_runs_setup_when_nothing_is_configured(
    fake_webview, tmp_path, store, monkeypatch
):
    """``launch()`` itself just has to call ``_run_setup`` when unconfigured
    and use whatever it returns -- ``_run_setup``'s own window-driving logic
    (a real event loop pywebview owns) is out of scope for a fake webview."""
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    monkeypatch.setattr(desktop, "_run_setup", lambda config_path: (
        desktop.save_snapshot_path(store.path, config_path) or store.path))

    desktop.launch(config_path=config)

    assert desktop.load_snapshot_path(config) == store.path
    windows = [c for c in fake_webview.calls if c[0] == "create_window"]
    assert windows == [("create_window", "BOLDcurator", "http://x/")]
