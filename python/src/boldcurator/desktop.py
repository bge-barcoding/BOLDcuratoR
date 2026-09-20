"""The packaged desktop entry point (plan 4.1a/4.2).

``boldcurator gui`` is the developer-facing mode: a fixed host/port, opened
manually in whatever browser the user already has running. A curator should
not need to know there is a server, a port, or a terminal at all -- this
runs the same Shiny app behind a window that looks and behaves like a
regular desktop app, closing which is what stops the server.

Two windows in sequence when no snapshot is configured yet:

1. **Setup** (:mod:`.ui.setup`) -- an ordinary Shiny app asking for an
   existing snapshot's path, or a URL/manifest/Zenodo id to download one via
   :mod:`.build.fetch_snapshot`. It hands the resolved path back through a
   ``queue.Queue`` shared with this module, which is watching it from
   another thread so it knows when to close that window.
2. **The main app** (:mod:`.ui.app`), now that a snapshot path exists,
   saved to ``~/.boldcurator/config.json`` so step 1 only happens once.

**No single window technology is a hard requirement.** `pywebview
<https://pywebview.flowrl.com/>`_ gives a genuinely embedded native window
(WKWebView on macOS, WebView2 via pythonnet/.NET on Windows) but, on
Windows, that pythonnet bridge has a long, still-open history of fragile,
environment-specific failures once frozen by PyInstaller --
``RuntimeError: Failed to resolve Python.Runtime.Loader.Initialize`` is a
real one, hit on a real curator's machine, not a hypothetical
(r0x0r/pywebview#1215, #1292, #1638; see ``packaging/README.md``). Three
window strategies exist, in ``WINDOW_MODES``, and both `pywebview` and any
browser executable are imported/located lazily -- config persistence and
server lifecycle stay importable, and testable, without either:

* ``"native"`` -- pywebview, embedded via pythonnet/.NET. Looks and behaves
  like any other desktop app when it works; the failure mode above when it
  doesn't.
* ``"browser-app"`` -- a Chromium-based browser (Edge, bundled with every
  current Windows install, or Chrome) launched with ``--app=<url>``: no
  tabs, no address bar, its own taskbar entry. Visually just as native as
  pywebview, but launched as a plain subprocess -- it never touches
  pythonnet, so it never hits the failure above at all.
* ``"tab"`` -- a plain browser tab. The simplest possible thing that always
  works, at the cost of looking like a browser, not an app.

``"auto"`` (the default) tries them in that order, falling back silently on
failure -- the shape that should actually reach a curator. Forcing one
directly (``--window native``/``browser-app``/``tab``) is for comparing
them: a forced strategy that fails raises rather than silently falling
back, so the comparison is never masked by an automatic recovery.
"""

from __future__ import annotations

import json
import os
import queue
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

DEFAULT_CONFIG_PATH = Path.home() / ".boldcurator" / "config.json"

WINDOW_MODES = ("auto", "native", "browser-app", "tab")


def load_snapshot_path(config_path: Path = DEFAULT_CONFIG_PATH) -> Path | None:
    """The path saved by a previous run, or ``None`` if there isn't one.

    A path that no longer exists (a moved or deleted file) is treated the
    same as none configured -- back to setup, rather than a confusing
    failure to open it.
    """
    if not config_path.exists():
        return None
    try:
        data = json.loads(config_path.read_text(encoding="utf-8"))
    except (json.JSONDecodeError, OSError):
        return None
    raw = data.get("snapshot_path")
    if not raw:
        return None
    candidate = Path(raw)
    return candidate if candidate.exists() else None


def save_snapshot_path(path: Path, config_path: Path = DEFAULT_CONFIG_PATH) -> None:
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(json.dumps({"snapshot_path": str(path)}), encoding="utf-8")


def _enable_webview_downloads(webview_module) -> None:
    """A curator reported round 5, item 12's own fix (a toast saying "your

    download is going to the Downloads folder") as not actually true in a
    native window: nothing was there. Root cause, found in pywebview's own
    source (every backend -- ``edgechromium.py``/Windows, ``gtk.py``/Linux,
    ``cocoa.py``/macOS, ``qt.py``): ``webview.settings['ALLOW_DOWNLOADS']``
    defaults to ``False``, and every one of them **silently cancels** a
    browser-triggered download (``args.Cancel = True`` on Windows) rather
    than erroring -- indistinguishable, from this app's side, from a
    download that simply never happened. This app's own server-side
    downloads (the snapshot fetch/copy in ``ui/app.py``'s "Snapshot file"
    panel) write straight to disk and were never affected; only the
    ``ui.download_button`` exports (specimens, FASTA, the xlsx reports) go
    through the browser's own download machinery, which is what a *native*
    pywebview window intercepts.

    Must be set before ``create_window``/``start()`` -- pywebview reads it
    when the download event fires, but nothing stops setting it as early as
    right after import. With it on, Windows shows a real native "Save As"
    dialog defaulting to the Downloads folder (via the same registry key
    Explorer itself uses); GTK/Qt/Cocoa save straight to each OS's
    Downloads folder without a prompt. Either way, a download now actually
    happens and lands somewhere findable -- neither silently vanishes.
    """
    webview_module.settings["ALLOW_DOWNLOADS"] = True
    _patch_edgechromium_download_extension()


def _patch_edgechromium_download_extension() -> None:
    """Round 6, downloads item 1: every download in a native Windows window

    landed with no file extension at all (``.tsv``/``.xlsx``/``.fasta``/
    ``.csv`` alike) -- reported right after round 5's item 12 fix made
    downloads work at all in that window. Every one of these already has
    the right extension in Shiny's own ``Content-Disposition`` header
    (``ui/app.py``'s ``filename=`` lambdas), and a *browser* window
    (Playwright's Chromium, this project's own test harness; presumably
    "browser-app" mode too) reads that correctly -- so this is specific to
    pywebview's own Windows glue code, not this app's server side.

    Root cause, in pywebview's own source (``platforms/edgechromium.py``,
    ``EdgeChrome.on_download_starting``): its ``SaveFileDialog`` is built
    with ``Filter = "All files (*.*)|*.*"`` and no ``DefaultExt`` set. This
    is a well-documented WinForms footgun independent of exactly how the
    extension goes missing along the way (in the dialog's own edit box, or
    whatever else the OS does with a wildcard-only filter and no default
    extension to fall back on): with ``DefaultExt`` unset, there is nothing
    for ``SaveFileDialog`` to re-apply if the extension is dropped, and
    with only a ``*.*`` filter, there is no concrete extension tied to the
    dialog's own "save as type" choice either. The fix, replicated here, is
    the one universally recommended for this exact WinForms symptom: give
    the dialog a filter built from the file's own real extension (with
    "All files" still offered second) and set ``DefaultExt`` explicitly.

    Implemented as a monkeypatch of ``EdgeChrome.on_download_starting``
    (reimplemented in full, since the extension has to be set *before*
    ``ShowDialog()`` is called, and that dialog is a local variable inside
    pywebview's own method -- there is no smaller seam to patch) rather
    than a change to pywebview's own package, since this project vendors
    nothing and pywebview is a third-party dependency. Applied defensively:
    if pywebview's internals have moved by the time this runs (a version
    upgrade renaming/restructuring this method), the patch is simply
    skipped -- the original, already-known-broken-for-extensions behaviour
    is what a curator would have seen anyway, never a crash.

    **Not verified on a real Windows/WebView2 machine** -- this sandbox
    cannot install or run pywebview at all (see ``packaging/README.md``'s
    "not verified anywhere yet"); this is reasoned from pywebview 6.2.1's
    published source, not observed live. Confirming this on the curator's
    own machine is the next thing to do.
    """
    if sys.platform != "win32":
        return
    try:
        from webview.platforms import edgechromium
    except Exception:
        return  # this backend isn't in use (or isn't importable) here

    def _patched_on_download_starting(self, sender, args):
        winreg = __import__("winreg")
        WinForms = edgechromium.WinForms
        webview_settings = edgechromium.webview_settings

        if not webview_settings["ALLOW_DOWNLOADS"]:
            args.Cancel = True
            return

        dialog = WinForms.SaveFileDialog()
        try:
            with winreg.OpenKey(
                winreg.HKEY_CURRENT_USER,
                r"Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders",
            ) as windows_key:
                dialog.InitialDirectory = winreg.QueryValueEx(
                    windows_key, "{374DE290-123F-4565-9164-39C4925E467B}"
                )[0]
        except Exception:
            pass

        suggested = os.path.basename(args.ResultFilePath)
        ext = os.path.splitext(suggested)[1].lstrip(".")
        if ext:
            dialog.Filter = (
                f"{ext.upper()} files (*.{ext})|*.{ext}|All files (*.*)|*.*")
            dialog.DefaultExt = ext
        else:
            dialog.Filter = "All files (*.*)|*.*"
        dialog.AddExtension = True
        dialog.RestoreDirectory = True
        dialog.FileName = suggested

        result = dialog.ShowDialog(self.form)
        if result == WinForms.DialogResult.OK:
            args.ResultFilePath = dialog.FileName
        else:
            args.Cancel = True

    try:
        if hasattr(edgechromium, "EdgeChrome") and hasattr(
            edgechromium.EdgeChrome, "on_download_starting"
        ):
            edgechromium.EdgeChrome.on_download_starting = (
                _patched_on_download_starting)
    except Exception:
        pass  # leave pywebview's own (extension-losing) behaviour in place


def _free_port() -> int:
    """An ephemeral local port, free at the moment of asking.

    A desktop launch never needs a fixed port the way ``boldcurator gui``'s
    developer-facing ``--port`` does -- nothing outside this process ever
    needs to know it, since pywebview is handed the URL directly.
    """
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def run_server(app, *, host: str = "127.0.0.1", port: int | None = None):
    """Start an ASGI ``app`` in a background thread. Returns ``(url, stop)``.

    Runs uvicorn directly rather than ``shiny.run_app`` -- that call blocks
    and owns the process's signal handling, neither of which fits a window
    that needs to keep running until pywebview's own event loop returns.
    """
    import uvicorn

    port = port if port is not None else _free_port()
    config = uvicorn.Config(app, host=host, port=port, log_level="warning")
    server = uvicorn.Server(config)
    thread = threading.Thread(target=server.run, daemon=True)
    thread.start()
    while not server.started:
        time.sleep(0.01)

    def stop() -> None:
        server.should_exit = True
        thread.join(timeout=5)

    return f"http://{host}:{port}", stop


#: Where to look for a Chromium-based browser's executable, in preference
#: order (Edge before Chrome -- every current Windows install has Edge,
#: not every one has Chrome). Absolute paths are checked directly; bare
#: names go through ``shutil.which``, since Windows does not reliably put
#: either on ``PATH`` even when installed.
def _chromium_candidates() -> list[str]:
    if sys.platform == "win32":
        roots = [os.environ.get("ProgramFiles", r"C:\Program Files"),
                 os.environ.get("ProgramFiles(x86)", r"C:\Program Files (x86)"),
                 os.environ.get("LocalAppData", "")]
        names = [r"Microsoft\Edge\Application\msedge.exe",
                 r"Google\Chrome\Application\chrome.exe"]
        return ([os.path.join(root, name) for root in roots if root
                for name in names]
                + ["msedge", "chrome", "google-chrome"])
    if sys.platform == "darwin":
        return ["/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"]
    return ["microsoft-edge", "google-chrome", "chromium-browser", "chromium"]


def _find_chromium_browser() -> str | None:
    for candidate in _chromium_candidates():
        if os.path.isabs(candidate):
            if os.path.exists(candidate):
                return candidate
        elif shutil.which(candidate):
            return shutil.which(candidate)
    return None


def _launch_browser_app(url: str, *, width: int = 1400,
                        height: int = 900) -> "subprocess.Popen | None":
    """A Chromium browser in ``--app`` mode: no tabs, no address bar, its
    own taskbar entry -- visually a native window, but launched as a plain
    subprocess rather than embedded, so it never touches the pythonnet/.NET
    bridge pywebview needs on Windows. Returns ``None`` if no suitable
    browser was found, rather than raising -- "not installed" is a normal,
    expected outcome here, not an error.

    A dedicated, throwaway profile directory keeps this from colliding with
    (or borrowing cookies/history from) the user's own browser profile, and
    avoids a launch failure if their browser is already running under its
    default profile.
    """
    browser = _find_chromium_browser()
    if browser is None:
        return None
    profile_dir = tempfile.mkdtemp(prefix="boldcurator-appmode-")
    return subprocess.Popen([
        browser, f"--app={url}", f"--window-size={width},{height}",
        f"--user-data-dir={profile_dir}",
    ])


def _wait_in_browser_tab(url: str) -> None:
    import webbrowser

    print(f"Opening {url} in your default browser. Press Ctrl+C here to quit.")
    webbrowser.open(url)
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        pass


def _run_setup(config_path: Path, *, window: str = "auto") -> Path:
    """Show the setup window until it resolves a snapshot path, or quit.

    ``window`` mirrors :func:`launch`'s own modes. Only ``"native"`` gets an
    OS-level "the user closed this" signal for free (``window.events.closed``),
    which is why it alone runs a watcher thread that owns ``resolved``'s one
    read; every other strategy just shows the URL and blocks directly on
    ``resolved.get()``, with no way to detect a cancel except Ctrl+C in the
    console. That asymmetry is accepted, not an oversight -- closing a
    browser window is not an event Python can see.

    Within the ``"native"``/``"auto"`` branch: if ``webview.start()`` itself
    fails (this project's real-world case: pythonnet fails deep inside it,
    after the window and the watcher already exist), the watcher is still
    the queue's one reader, so recovery *joins* it rather than reading the
    queue a second time, which would race it.
    """
    from .ui.setup import create_setup_app

    resolved: "queue.Queue[Path | None]" = queue.Queue()
    app = create_setup_app(resolved)
    url, stop = run_server(app)

    if window not in WINDOW_MODES:
        raise ValueError(f"Unknown window mode {window!r}")

    if window in ("native", "auto"):
        watcher = None
        box: dict[str, Path | None] = {}
        try:
            import webview

            _enable_webview_downloads(webview)
            win = webview.create_window("BOLDcurator -- set up", url,
                                        width=760, height=640)
            win.events.closed += lambda: resolved.put(None)

            def _watch(win=win) -> None:
                box["path"] = resolved.get()
                try:
                    win.destroy()
                except Exception:
                    pass  # already gone -- webview.start() failed below

            watcher = threading.Thread(target=_watch, daemon=True)
            watcher.start()
            webview.start()
        except Exception as exc:
            if window == "native":
                stop()
                raise
            print(f"Native window unavailable ({exc}); ", end="")
            if _launch_browser_app(url) is not None:
                print("opening a browser app window instead.")
            else:
                import webbrowser

                print(f"opening {url} in your default browser instead.")
                webbrowser.open(url)
            print("Complete setup there, then return to this console.")

        if watcher is not None:
            # The watcher is the queue's one legitimate reader once it
            # exists -- reading it again here too would race it.
            watcher.join()
            path = box.get("path")
        else:
            path = resolved.get()
    elif window == "browser-app":
        if _launch_browser_app(url) is None:
            stop()
            raise RuntimeError("no Chromium-based browser found for app mode")
        path = resolved.get()
    else:  # "tab"
        import webbrowser

        webbrowser.open(url)
        path = resolved.get()

    stop()
    if path is None:
        raise SystemExit("Setup was closed before a snapshot was chosen.")
    save_snapshot_path(path, config_path)
    return path


def _show_window_blocking(url: str, *, window: str) -> None:
    """Show ``url`` in a window and block until the user is done with it.

    ``"native"``, ``"browser-app"`` and ``"tab"`` force exactly one
    strategy -- for comparing them, a forced strategy that fails raises
    rather than silently falling back, so a failure is never masked.
    ``"auto"`` (the default) tries native, then a browser-app window, then
    a plain tab, falling back silently -- the shape that should actually
    reach a curator.
    """
    if window not in WINDOW_MODES:
        raise ValueError(f"Unknown window mode {window!r}")

    if window == "native":
        import webview

        _enable_webview_downloads(webview)
        webview.create_window("BOLDcurator", url, width=1400, height=900)
        webview.start()
        return
    if window == "browser-app":
        proc = _launch_browser_app(url)
        if proc is None:
            raise RuntimeError("no Chromium-based browser found for app mode")
        proc.wait()
        return
    if window == "tab":
        _wait_in_browser_tab(url)
        return

    # "auto"
    try:
        import webview

        _enable_webview_downloads(webview)
        webview.create_window("BOLDcurator", url, width=1400, height=900)
        webview.start()
        return
    except Exception as exc:
        print(f"Native window unavailable ({exc}); trying a browser app window.")
    proc = _launch_browser_app(url)
    if proc is not None:
        proc.wait()
        return
    print("No Chromium-based browser found for app mode either; "
          "falling back to a plain browser tab.")
    _wait_in_browser_tab(url)


def launch(snapshot_path: str | Path | None = None, *,
          page_size: int = 100,
          config_path: Path = DEFAULT_CONFIG_PATH,
          window: str = "auto") -> None:
    """The desktop app's whole lifecycle: resolve a snapshot, then run it.

    ``snapshot_path`` overrides the saved config for this one run (and is
    saved over it) -- useful for switching snapshots without deleting the
    config file by hand. ``window`` picks the window strategy -- see
    ``WINDOW_MODES`` and this module's docstring.
    """
    from .ui.app import create_app

    path = Path(snapshot_path) if snapshot_path else load_snapshot_path(config_path)
    if path is None:
        path = _run_setup(config_path, window=window)
    elif snapshot_path:
        save_snapshot_path(path, config_path)

    app = create_app(path, page_size=page_size)
    url, stop = run_server(app)
    try:
        _show_window_blocking(url, window=window)
    finally:
        stop()
