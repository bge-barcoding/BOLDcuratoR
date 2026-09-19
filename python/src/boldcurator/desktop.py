"""The packaged desktop entry point (plan 4.1a/4.2).

``boldcurator gui`` is the developer-facing mode: a fixed host/port, opened
manually in whatever browser the user already has running. A curator should
not need to know there is a server, a port, or a terminal at all -- this
wraps the same Shiny app in a native window via `pywebview
<https://pywebview.flowrl.com/>`_ (WKWebView on macOS, WebView2 on Windows,
both built in; no extra runtime to install), so closing the window is what
stops the server, the way any other desktop app works.

Two windows in sequence when no snapshot is configured yet:

1. **Setup** (:mod:`.ui.setup`) -- an ordinary Shiny app asking for an
   existing snapshot's path, or a URL/manifest/Zenodo id to download one via
   :mod:`.build.fetch_snapshot`. It hands the resolved path back through a
   ``queue.Queue`` shared with this module, which is watching it from
   another thread so it knows when to close that window.
2. **The main app** (:mod:`.ui.app`), now that a snapshot path exists,
   saved to ``~/.boldcurator/config.json`` so step 1 only happens once.

``pywebview`` is imported lazily, inside the functions that actually open a
window, so config persistence and server lifecycle -- the parts worth
testing -- stay importable without it, and without whatever native webview
backend the platform needs (present by default on macOS/Windows; on Linux
it needs a system WebKitGTK or Qt install this project does not otherwise
require).

**The native window is a best effort, not a hard requirement.** On Windows,
every one of pywebview's backends (winforms, edgechromium, mshtml) bridges
through pythonnet/.NET, and that bridge has a long, still-open history of
fragile, environment-specific failures once frozen by PyInstaller --
``RuntimeError: Failed to resolve Python.Runtime.Loader.Initialize`` is a
real one, hit on a real curator's machine, not a hypothetical
(r0x0r/pywebview#1215, #1292, #1638; see ``packaging/README.md``). A curator
looking at a plain browser tab is a far better outcome than one looking at
a crash, so a native-window failure here is never fatal: both ``launch()``
and the setup screen below fall back to opening the system's default
browser and carrying on from there.
"""

from __future__ import annotations

import json
import queue
import socket
import threading
import time
from pathlib import Path

DEFAULT_CONFIG_PATH = Path.home() / ".boldcurator" / "config.json"


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


def _open_in_browser(url: str, reason: BaseException) -> None:
    import webbrowser

    print(f"Native window unavailable ({reason}); opening {url} in your "
          "default browser instead.")
    webbrowser.open(url)


def _run_setup(config_path: Path) -> Path:
    """Show the setup window until it resolves a snapshot path, or quit.

    ``resolved`` has exactly one reader: either the watcher thread (started
    as soon as a window exists, since it has to be running before
    ``webview.start()`` blocks) or, if a window was never created at all,
    this function directly. Never both -- if ``webview.start()`` itself
    fails (this project's real-world case: pythonnet fails deep inside it,
    after the window and the watcher already exist), the watcher is still
    the one waiting on the queue, so this falls back to the browser and
    *joins* the watcher rather than reading the queue a second time, which
    would race it.
    """
    from .ui.setup import create_setup_app

    resolved: "queue.Queue[Path | None]" = queue.Queue()
    app = create_setup_app(resolved)
    url, stop = run_server(app)

    window = None
    watcher = None
    box: dict[str, Path | None] = {}
    try:
        import webview

        window = webview.create_window("BOLDcurator -- set up", url,
                                       width=760, height=640)
        window.events.closed += lambda: resolved.put(None)

        def _watch(window=window) -> None:
            box["path"] = resolved.get()
            try:
                window.destroy()
            except Exception:
                pass  # already gone -- webview.start() itself failed below

        watcher = threading.Thread(target=_watch, daemon=True)
        watcher.start()
        webview.start()
    except Exception as exc:
        _open_in_browser(url, exc)
        print("Complete setup there, then return to this console.")

    if watcher is not None:
        # The watcher is the queue's one legitimate reader once it exists
        # (started before webview.start(), which needs it running while it
        # blocks) -- reading the queue again here too would race it.
        watcher.join()
        path = box.get("path")
    else:
        # No window was ever created, so nothing else is reading this --
        # block here until the browser-based setup app resolves one.
        path = resolved.get()
    stop()

    if path is None:
        raise SystemExit("Setup was closed before a snapshot was chosen.")
    save_snapshot_path(path, config_path)
    return path


def launch(snapshot_path: str | Path | None = None, *,
          page_size: int = 100,
          config_path: Path = DEFAULT_CONFIG_PATH) -> None:
    """The desktop app's whole lifecycle: resolve a snapshot, then run it.

    ``snapshot_path`` overrides the saved config for this one run (and is
    saved over it) -- useful for switching snapshots without deleting the
    config file by hand.
    """
    from .ui.app import create_app

    path = Path(snapshot_path) if snapshot_path else load_snapshot_path(config_path)
    if path is None:
        path = _run_setup(config_path)
    elif snapshot_path:
        save_snapshot_path(path, config_path)

    app = create_app(path, page_size=page_size)
    url, stop = run_server(app)
    try:
        import webview

        webview.create_window("BOLDcurator", url, width=1400, height=900)
        webview.start()
    except Exception as exc:
        _open_in_browser(url, exc)
        print("Press Ctrl+C here to quit.")
        try:
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            pass
    finally:
        stop()
