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
    # Real pywebview's own settings dict (module.settings['ALLOW_DOWNLOADS'],
    # default False) -- see desktop._enable_webview_downloads. A bare
    # ModuleType has no such attribute by default, so every native-window
    # test needs this or it fails before even reaching create_window/start.
    module.settings = {"ALLOW_DOWNLOADS": False}
    monkeypatch.setitem(sys.modules, "webview", module)
    return module


def test_launch_enables_pywebview_downloads_before_opening_the_window(
    fake_webview, tmp_path, store, monkeypatch
):
    """A curator reported downloads silently vanishing in a native window --

    pywebview cancels every one by default (``settings['ALLOW_DOWNLOADS']``,
    False in every backend) unless told otherwise before the window opens.
    """
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))

    desktop.launch(store.path, config_path=config)

    assert fake_webview.settings["ALLOW_DOWNLOADS"] is True


@pytest.fixture
def fake_edgechromium(monkeypatch):
    """A minimal stand-in for pywebview's Windows backend module, just

    enough surface for ``_patch_edgechromium_download_extension`` to patch
    and for the patched method to run against: a fake WinForms namespace
    (``SaveFileDialog``, ``DialogResult``), the module's own
    ``webview_settings`` dict, and an ``EdgeChrome`` class with a
    placeholder ``on_download_starting``.
    """
    calls: list = []

    class FakeDialogResult:
        OK = "OK"
        CANCEL = "CANCEL"

    class FakeSaveFileDialog:
        last_instance = None

        def __init__(self):
            self.Filter = None
            self.DefaultExt = None
            self.AddExtension = None
            self.RestoreDirectory = None
            self.InitialDirectory = None
            self.FileName = None
            FakeSaveFileDialog.last_instance = self

        def ShowDialog(self, form):
            calls.append(("ShowDialog",))
            return FakeSaveFileDialog._result

    FakeSaveFileDialog._result = FakeDialogResult.OK

    class FakeWinForms:
        SaveFileDialog = FakeSaveFileDialog
        DialogResult = FakeDialogResult

    class FakeArgs:
        def __init__(self, result_file_path):
            self.ResultFilePath = result_file_path
            self.Cancel = False

    class EdgeChrome:
        def __init__(self):
            self.form = object()

        def on_download_starting(self, sender, args):
            calls.append(("original",))

    edgechromium = types.ModuleType("webview.platforms.edgechromium")
    edgechromium.WinForms = FakeWinForms
    edgechromium.webview_settings = {"ALLOW_DOWNLOADS": True}
    edgechromium.EdgeChrome = EdgeChrome
    edgechromium.calls = calls
    edgechromium.FakeArgs = FakeArgs
    edgechromium.FakeSaveFileDialog = FakeSaveFileDialog

    platforms = types.ModuleType("webview.platforms")
    platforms.edgechromium = edgechromium
    webview_pkg = types.ModuleType("webview")
    webview_pkg.platforms = platforms

    monkeypatch.setitem(sys.modules, "webview", webview_pkg)
    monkeypatch.setitem(sys.modules, "webview.platforms", platforms)
    monkeypatch.setitem(sys.modules, "webview.platforms.edgechromium", edgechromium)
    monkeypatch.setitem(sys.modules, "winreg", types.ModuleType("winreg"))
    monkeypatch.setattr(sys, "platform", "win32")
    return edgechromium


def test_edgechromium_download_extension_patch_sets_default_ext(fake_edgechromium):
    """Round 6, downloads item 1: a native window's own Save As dialog

    dropped every download's file extension. pywebview's own dialog is
    built with an "All files (*.*)" filter and no ``DefaultExt`` -- the
    patch must give it both, derived from the file's own suggested name.
    """
    desktop._patch_edgechromium_download_extension()

    patched = fake_edgechromium.EdgeChrome.on_download_starting
    instance = fake_edgechromium.EdgeChrome()
    args = fake_edgechromium.FakeArgs(r"C:\Users\curator\Downloads\all_specimens_20260101.tsv")

    patched(instance, sender=None, args=args)

    dialog_calls = [c for c in fake_edgechromium.calls if c[0] == "ShowDialog"]
    assert dialog_calls, "the patched method never opened a save dialog"
    assert ("original",) not in fake_edgechromium.calls  # replaced, not wrapped
    assert args.Cancel is False
    assert args.ResultFilePath.endswith(".tsv")

    dialog = fake_edgechromium.FakeSaveFileDialog.last_instance
    assert dialog.DefaultExt == "tsv"
    assert dialog.AddExtension is True
    assert "*.tsv" in dialog.Filter


def test_edgechromium_download_extension_patch_is_noop_off_windows(
    fake_edgechromium, monkeypatch
):
    monkeypatch.setattr(sys, "platform", "linux")
    original = fake_edgechromium.EdgeChrome.on_download_starting

    desktop._patch_edgechromium_download_extension()

    assert fake_edgechromium.EdgeChrome.on_download_starting is original


def test_edgechromium_download_extension_patch_survives_a_missing_module(monkeypatch):
    """A future pywebview version could rename/restructure this module --

    the patch must skip quietly, never crash `launch()`.
    """
    monkeypatch.setattr(sys, "platform", "win32")
    monkeypatch.delitem(sys.modules, "webview.platforms.edgechromium", raising=False)
    monkeypatch.delitem(sys.modules, "webview.platforms", raising=False)

    desktop._patch_edgechromium_download_extension()  # must not raise


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
    monkeypatch.setattr(desktop, "_run_setup", lambda config_path, **kw: (
        desktop.save_snapshot_path(store.path, config_path) or store.path))

    desktop.launch(config_path=config)

    assert desktop.load_snapshot_path(config) == store.path
    windows = [c for c in fake_webview.calls if c[0] == "create_window"]
    assert windows == [("create_window", "BOLDcurator", "http://x/")]


def test_launch_falls_back_to_the_browser_when_the_window_fails_to_start(
    fake_webview, tmp_path, store, monkeypatch
):
    """The real-world case this guards against: a curator on Windows whose

    pythonnet/CLR bridge fails deep inside webview.start() -- native window
    or not, the app must still come up.
    """
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    fake_webview.start = lambda: (_ for _ in ()).throw(
        RuntimeError("Failed to resolve Python.Runtime.Loader.Initialize"))
    # No Chromium browser either, in "auto" mode -- forces the cascade all
    # the way down to a plain tab, which is what this test checks.
    monkeypatch.setattr(desktop, "_launch_browser_app", lambda url, **kw: None)
    opened = {}
    monkeypatch.setattr("webbrowser.open", lambda url: opened.setdefault("url", url))
    monkeypatch.setattr(desktop.time, "sleep",
                        lambda *_: (_ for _ in ()).throw(KeyboardInterrupt()))

    desktop.launch(store.path, config_path=config)

    assert opened["url"] == "http://x/"


def test_launch_uses_a_browser_app_window_when_native_fails(
    fake_webview, tmp_path, store, monkeypatch
):
    """The middle rung of the "auto" cascade: native fails, but a Chromium

    browser is available, so that's what shows -- no plain tab needed.
    """
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    fake_webview.start = lambda: (_ for _ in ()).throw(
        RuntimeError("Failed to resolve Python.Runtime.Loader.Initialize"))
    launched = {}

    class FakeProc:
        def wait(self):
            launched["waited"] = True

    def fake_launch_browser_app(url, **kw):
        launched["url"] = url
        return FakeProc()

    monkeypatch.setattr(desktop, "_launch_browser_app", fake_launch_browser_app)
    monkeypatch.setattr("webbrowser.open",
                        lambda url: pytest.fail("should not fall back to a plain tab"))

    desktop.launch(store.path, config_path=config)

    assert launched["url"] == "http://x/"
    assert launched["waited"] is True


def test_launch_with_window_browser_app_skips_pywebview_entirely(
    tmp_path, store, monkeypatch
):
    """Forcing --window browser-app must not even touch pywebview -- the

    whole point is being usable when pywebview itself is broken.
    """
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    launched = {}

    class FakeProc:
        def wait(self):
            launched["waited"] = True

    def fake_launch_browser_app(url, **kw):
        launched["url"] = url
        return FakeProc()

    monkeypatch.setattr(desktop, "_launch_browser_app", fake_launch_browser_app)
    # A "webview" module that would raise if imported/used at all.
    broken = types.ModuleType("webview")

    def _boom(*a, **k):
        raise AssertionError("native window should never be attempted")
    broken.create_window = _boom
    broken.start = _boom
    monkeypatch.setitem(sys.modules, "webview", broken)

    desktop.launch(store.path, config_path=config, window="browser-app")

    assert launched["url"] == "http://x/"
    assert launched["waited"] is True


def test_launch_with_window_browser_app_raises_when_none_found(tmp_path, store, monkeypatch):
    """Forced modes never silently fall back -- that would defeat the point

    of forcing one to compare it.
    """
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    monkeypatch.setattr(desktop, "_launch_browser_app", lambda url, **kw: None)

    with pytest.raises(RuntimeError, match="no Chromium-based browser"):
        desktop.launch(store.path, config_path=config, window="browser-app")


def test_launch_with_window_native_raises_when_it_fails(fake_webview, tmp_path, store, monkeypatch):
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    fake_webview.start = lambda: (_ for _ in ()).throw(RuntimeError("boom"))

    with pytest.raises(RuntimeError, match="boom"):
        desktop.launch(store.path, config_path=config, window="native")


def test_launch_with_window_tab_never_touches_webview(tmp_path, store, monkeypatch):
    config = tmp_path / "config.json"
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    opened = {}
    monkeypatch.setattr("webbrowser.open", lambda url: opened.setdefault("url", url))
    monkeypatch.setattr(desktop.time, "sleep",
                        lambda *_: (_ for _ in ()).throw(KeyboardInterrupt()))
    broken = types.ModuleType("webview")
    broken.create_window = lambda *a, **k: pytest.fail("native should not be tried")
    monkeypatch.setitem(sys.modules, "webview", broken)

    desktop.launch(store.path, config_path=config, window="tab")

    assert opened["url"] == "http://x/"


def test_show_window_blocking_rejects_an_unknown_mode():
    with pytest.raises(ValueError, match="Unknown window mode"):
        desktop._show_window_blocking("http://x/", window="carrier-pigeon")


def test_run_setup_falls_back_to_the_browser_when_the_window_fails_to_start(
    tmp_path, store, monkeypatch, fake_webview
):
    """Same failure, mid-setup: the watcher thread is already blocked on the

    queue when webview.start() raises, so the fallback must join it rather
    than read the queue a second time (that would race the watcher).
    """
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    fake_webview.start = lambda: (_ for _ in ()).throw(
        RuntimeError("Failed to resolve Python.Runtime.Loader.Initialize"))

    def fake_create_setup_app(resolved):
        # Stands in for a curator completing setup in the fallback browser
        # tab -- the setup app itself would call this once they submit a
        # path or a download finishes.
        resolved.put(store.path)
        return object()

    monkeypatch.setattr("boldcurator.ui.setup.create_setup_app",
                        fake_create_setup_app)
    monkeypatch.setattr(desktop, "_launch_browser_app", lambda url, **kw: None)
    opened = {}
    monkeypatch.setattr("webbrowser.open", lambda url: opened.setdefault("url", url))

    config = tmp_path / "config.json"
    path = desktop._run_setup(config)

    assert path == store.path
    assert opened["url"] == "http://x/"
    assert desktop.load_snapshot_path(config) == store.path


def test_run_setup_falls_back_when_the_window_cannot_even_be_created(
    tmp_path, store, monkeypatch
):
    """No window, no watcher thread -- _run_setup must read the queue

    itself rather than waiting on a watcher that was never started.
    """
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))

    def fake_create_setup_app(resolved):
        resolved.put(store.path)
        return object()

    monkeypatch.setattr("boldcurator.ui.setup.create_setup_app",
                        fake_create_setup_app)
    monkeypatch.setattr(desktop, "_launch_browser_app", lambda url, **kw: None)
    monkeypatch.setattr("webbrowser.open", lambda url: None)
    # No fake_webview fixture here -- "webview" stays whatever real module
    # (if any) is on sys.path; make create_window itself the failure.
    broken = types.ModuleType("webview")
    broken.create_window = lambda *a, **k: (_ for _ in ()).throw(
        ImportError("No module named 'clr'"))
    monkeypatch.setitem(sys.modules, "webview", broken)

    config = tmp_path / "config.json"
    path = desktop._run_setup(config)

    assert path == store.path


def test_run_setup_with_window_browser_app_skips_pywebview_entirely(
    tmp_path, store, monkeypatch
):
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))

    def fake_create_setup_app(resolved):
        resolved.put(store.path)
        return object()

    monkeypatch.setattr("boldcurator.ui.setup.create_setup_app", fake_create_setup_app)
    launched = {}

    def fake_launch_browser_app(url, **kw):
        launched["url"] = url
        return object()

    monkeypatch.setattr(desktop, "_launch_browser_app", fake_launch_browser_app)
    broken = types.ModuleType("webview")
    broken.create_window = lambda *a, **k: pytest.fail("native should not be tried")
    monkeypatch.setitem(sys.modules, "webview", broken)

    config = tmp_path / "config.json"
    path = desktop._run_setup(config, window="browser-app")

    assert path == store.path
    assert launched["url"] == "http://x/"


def test_run_setup_with_window_browser_app_raises_when_none_found(
    tmp_path, store, monkeypatch
):
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    monkeypatch.setattr(desktop, "_launch_browser_app", lambda url, **kw: None)

    with pytest.raises(RuntimeError, match="no Chromium-based browser"):
        desktop._run_setup(tmp_path / "config.json", window="browser-app")


def test_run_setup_with_window_tab(tmp_path, store, monkeypatch):
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))

    def fake_create_setup_app(resolved):
        resolved.put(store.path)
        return object()

    monkeypatch.setattr("boldcurator.ui.setup.create_setup_app", fake_create_setup_app)
    opened = {}
    monkeypatch.setattr("webbrowser.open", lambda url: opened.setdefault("url", url))

    path = desktop._run_setup(tmp_path / "config.json", window="tab")

    assert path == store.path
    assert opened["url"] == "http://x/"


def test_run_setup_rejects_an_unknown_window_mode(tmp_path, monkeypatch):
    monkeypatch.setattr(desktop, "run_server",
                        lambda app, **kw: ("http://x/", lambda: None))
    monkeypatch.setattr("boldcurator.ui.setup.create_setup_app", lambda resolved: object())

    with pytest.raises(ValueError, match="Unknown window mode"):
        desktop._run_setup(tmp_path / "config.json", window="carrier-pigeon")


# -- browser-app discovery/launch, independent of the window-mode plumbing ---


def test_find_chromium_browser_checks_absolute_paths_first(tmp_path, monkeypatch):
    fake_edge = tmp_path / "msedge.exe"
    fake_edge.write_bytes(b"")
    monkeypatch.setattr(desktop, "_chromium_candidates", lambda: [str(fake_edge), "chrome"])

    assert desktop._find_chromium_browser() == str(fake_edge)


def test_find_chromium_browser_falls_back_to_path(monkeypatch):
    monkeypatch.setattr(desktop, "_chromium_candidates",
                        lambda: [r"C:\nowhere\msedge.exe", "definitely-not-a-real-browser-xyz",
                                 "sh"])
    # "sh" is virtually guaranteed to exist on any test runner (Windows
    # runners carry Git for Windows's own sh.exe) and stands in for a
    # browser found via PATH rather than an absolute path. Compared by stem,
    # not a literal suffix -- shutil.which appends .EXE on Windows.
    found = desktop._find_chromium_browser()
    assert found is not None and Path(found).stem.lower() == "sh"


def test_find_chromium_browser_returns_none_when_nothing_matches(monkeypatch):
    monkeypatch.setattr(desktop, "_chromium_candidates",
                        lambda: ["definitely-not-a-real-browser-xyz"])
    assert desktop._find_chromium_browser() is None


def test_launch_browser_app_passes_app_mode_flags(monkeypatch):
    monkeypatch.setattr(desktop, "_find_chromium_browser", lambda: "/usr/bin/fake-edge")
    calls = {}

    class FakePopen:
        def __init__(self, args):
            calls["args"] = args

    monkeypatch.setattr(desktop.subprocess, "Popen", FakePopen)

    result = desktop._launch_browser_app("http://x/", width=800, height=600)

    assert isinstance(result, FakePopen)
    args = calls["args"]
    assert args[0] == "/usr/bin/fake-edge"
    assert "--app=http://x/" in args
    assert "--window-size=800,600" in args
    assert any(a.startswith("--user-data-dir=") for a in args)


def test_launch_browser_app_returns_none_without_a_browser(monkeypatch):
    monkeypatch.setattr(desktop, "_find_chromium_browser", lambda: None)
    assert desktop._launch_browser_app("http://x/") is None


def test_chromium_candidates_are_platform_specific(monkeypatch):
    monkeypatch.setattr(desktop.sys, "platform", "win32")
    windows = desktop._chromium_candidates()
    assert any("msedge" in c.lower() for c in windows)

    monkeypatch.setattr(desktop.sys, "platform", "darwin")
    mac = desktop._chromium_candidates()
    assert any("Microsoft Edge.app" in c for c in mac)

    monkeypatch.setattr(desktop.sys, "platform", "linux")
    linux = desktop._chromium_candidates()
    assert "google-chrome" in linux
