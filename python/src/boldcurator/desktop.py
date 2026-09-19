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


def _run_setup(config_path: Path) -> Path:
    """Show the setup window until it resolves a snapshot path, or quit."""
    from .ui.setup import create_setup_app

    resolved: "queue.Queue[Path | None]" = queue.Queue()
    app = create_setup_app(resolved)
    url, stop = run_server(app)

    import webview

    window = webview.create_window("BOLDcurator -- set up", url,
                                   width=760, height=640)
    window.events.closed += lambda: resolved.put(None)

    box: dict[str, Path | None] = {}

    def _watch() -> None:
        box["path"] = resolved.get()
        window.destroy()

    threading.Thread(target=_watch, daemon=True).start()
    webview.start()
    stop()

    path = box.get("path")
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
    finally:
        stop()
