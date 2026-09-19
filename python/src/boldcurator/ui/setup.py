"""The first-run 'get a snapshot onto this machine' screen (plan 4.2).

Shown once, in the window `desktop.py` opens when
``desktop.load_snapshot_path`` finds nothing configured yet. Two ways in,
matching ``docs/python-app-plan.md``'s "Packaging and data loading" design:

* an existing ``.duckdb`` file's path, opened and checked with
  ``SnapshotStore`` before being accepted -- a curator who already has the
  file (a shared drive, a colleague's copy) should not have to download it
  again;
* a URL, a ``manifest.json``, or a Zenodo record/concept id, downloaded via
  :mod:`..build.fetch_snapshot` to a default location.

Building one from a raw BOLD ``.tsv.gz`` package is deliberately **not**
offered here -- that is a maintainer's job (``tools/build_snapshot.py``, a
~24-minute background build needing several GB of disk), not a curator's;
see the plan doc's "Packaging and data loading" section for the reasoning.

Whichever path resolves puts the chosen ``Path`` onto ``resolved`` -- a
``queue.Queue`` that is thread-safe by design, unlike a Shiny
``reactive.Value``, which is why it is what the download's background
thread and ``desktop.py``'s watcher thread hand the result through.
"""

from __future__ import annotations

import multiprocessing
import queue
import threading
from pathlib import Path

from shiny import App, reactive, render, ui

from ..config.constants import (
    BOLD_ATTRIBUTION_TEXT,
    CC_BY_SA_URL,
    DEFAULT_SNAPSHOT_ZENODO_DOI,
)
from ..data.snapshot import SnapshotError, SnapshotStore

#: Where a download lands when the user gives a URL/manifest/record rather
#: than an existing file -- next to the session store and config file this
#: module's caller already uses, not next to whatever the app is installed
#: into (which may not be writable).
DEFAULT_DOWNLOAD_PATH = Path.home() / ".boldcurator" / "snapshot.duckdb"


def _dialog_worker(result_queue: "multiprocessing.Queue[str | None]") -> None:
    """Runs in its own process (see ``_pick_snapshot_file``) -- module-level
    so ``multiprocessing``'s ``spawn`` context can import and pickle it."""
    try:
        import tkinter
        import tkinter.filedialog

        root = tkinter.Tk()
        root.withdraw()
        root.attributes("-topmost", True)
        path = tkinter.filedialog.askopenfilename(
            title="Select a BOLDcurator snapshot",
            filetypes=[("DuckDB snapshot", "*.duckdb"), ("All files", "*.*")],
        )
        result_queue.put(path or None)
    except Exception:
        result_queue.put(None)


def _pick_snapshot_file() -> str | None:
    """Ask the OS for a file with its own native file browser -- round 4,
    item 1: a curator should not have to type a path by hand.

    Runs Tk's own file dialog in a short-lived helper process, via
    ``multiprocessing`` rather than a plain ``subprocess`` re-invoking
    ``sys.executable``: in a PyInstaller-frozen build, ``sys.executable`` is
    this application's own exe, not a general-purpose Python interpreter, so
    ``subprocess.run([sys.executable, "-c", ...])`` would try to hand that
    exe's own CLI a ``-c`` flag it does not understand. ``multiprocessing``'s
    ``spawn`` context re-invokes whatever ``sys.executable`` actually is
    correctly either way -- frozen or not -- given ``freeze_support()`` at
    the entry point (``packaging/entrypoint.py``), which is exactly the
    problem it exists to solve.

    A separate process (not just a thread) also sidesteps Tk's dialogs not
    being guaranteed to work off the process's main thread on every platform
    (macOS's Cocoa is the strict case) -- this server itself runs on a
    background thread (``desktop.py``'s ``run_server``), never the main one.

    ``None`` means no dialog could be shown at all (no Tcl/Tk available, the
    helper process failed to start, or it hung) -- typing the path stays the
    fallback, never a dead end. A plain Cancel in the dialog reports the same
    way (an empty ``askopenfilename`` result), which is also not an error.
    """
    try:
        ctx = multiprocessing.get_context("spawn")
        result_queue: "multiprocessing.Queue[str | None]" = ctx.Queue()
        proc = ctx.Process(target=_dialog_worker, args=(result_queue,), daemon=True)
        proc.start()
    except Exception:
        return None
    proc.join(timeout=300)
    if proc.is_alive():
        proc.terminate()
        proc.join(timeout=5)
        return None
    try:
        return result_queue.get_nowait()
    except queue.Empty:
        return None


def _looks_like_a_manifest(text: str) -> bool:
    """A plain heuristic for one text field covering three input shapes.

    A dedicated manifest.json always ends in ``.json``; anything else that
    starts with a scheme is a direct file URL; anything else again is taken
    as a Zenodo record or concept id. Good enough for a first-run screen --
    getting it wrong just means picking "Download" a second time with the
    right kind of text.
    """
    return text.startswith(("http://", "https://")) and text.rstrip("/").endswith(".json")


def create_setup_app(resolved: "queue.Queue[Path | None]") -> App:
    app_ui = ui.page_fluid(
        ui.tags.h3("Set up BOLDcurator"),
        ui.p("This app works from a local snapshot of BOLD's public data "
             "package -- point it at one, or download one, to get started."),
        ui.navset_tab(
            ui.nav_panel(
                "I already have a snapshot file",
                ui.div(
                    ui.input_text("path", "Path to a .duckdb snapshot file",
                                  width="100%",
                                  placeholder="/path/to/bold_snapshot.duckdb"),
                    ui.input_action_button("browse", "Browse…",
                                           class_="btn-outline-secondary mt-4"),
                    style="display:flex;align-items:start;gap:8px;",
                ),
                ui.input_action_button("use_path", "Use this file",
                                       class_="btn-primary mt-2"),
                ui.output_ui("path_status"),
            ),
            ui.nav_panel(
                "Download one",
                ui.div(
                    ui.tags.strong("The public BOLD snapshot"),
                    ui.p("This project's own rebuilt copy, published on "
                         f"Zenodo ({DEFAULT_SNAPSHOT_ZENODO_DOI}) -- nothing "
                         "to type.", class_="small text-muted mb-2"),
                    ui.input_action_button("download_default",
                                           "Download the latest public "
                                           "BOLD snapshot",
                                           class_="btn-primary"),
                    style="background:#f8f9fa;border:1px solid #dee2e6;"
                          "border-radius:5px;padding:10px 14px;"
                          "margin-bottom:14px;",
                ),
                ui.tags.details(
                    ui.tags.summary("Or provide your own source",
                                    class_="small text-muted"),
                    ui.input_text(
                        "source", "A direct URL, a manifest.json URL, or a "
                        "Zenodo record/concept id/DOI", width="100%"),
                    ui.input_action_button("download", "Download",
                                           class_="btn-primary mt-2"),
                    style="margin-top:8px;",
                ),
                ui.output_ui("download_status"),
            ),
        ),
        ui.div(
            BOLD_ATTRIBUTION_TEXT + " ",
            ui.tags.a("Full licence text.", href=CC_BY_SA_URL,
                     target="_blank", rel="noopener noreferrer"),
            class_="small text-muted", style="margin-top:18px;max-width:820px;",
        ),
    )

    def server(input, output, session):
        path_msg = reactive.Value("")
        #: Written from the download's background thread, so this is a plain
        #: dict (a single-key write is atomic enough under the GIL) rather
        #: than a reactive.Value, which Shiny expects to be touched from its
        #: own reactive context, not an arbitrary thread. ``download_status``
        #: below polls it via ``reactive.invalidate_later``.
        dl_state = {"running": False, "message": ""}
        tick = reactive.Value(0)

        @reactive.effect
        @reactive.event(input.browse)
        def _browse():
            # Runs a helper process and blocks this reactive effect on it --
            # fine here: a curator picking a file expects to wait for the
            # dialog, and nothing else on this one-off setup screen needs
            # this thread meanwhile. A cancelled dialog and "no native file
            # browser available at all" both come back as None -- either
            # way, typing the path above is the fallback, so one message
            # covers both rather than guessing which happened.
            chosen = _pick_snapshot_file()
            if chosen:
                ui.update_text("path", value=chosen)
            else:
                path_msg.set(
                    "No file chosen -- type the path above instead if "
                    "Browse… didn't work.")

        @reactive.effect
        @reactive.event(input.use_path)
        def _use_path():
            candidate = Path((input.path() or "").strip()).expanduser()
            if not candidate.exists():
                path_msg.set(f"No file at {candidate}.")
                return
            try:
                with SnapshotStore(candidate) as store:
                    info = store.info()
            except SnapshotError as exc:
                path_msg.set(f"Not a valid snapshot: {exc}")
                return
            path_msg.set(f"Using {info.describe()}...")
            resolved.put(candidate)

        @output
        @render.ui
        def path_status():
            text = path_msg.get()
            return ui.div(text, class_="small mt-2") if text else ui.div()

        def _run_download(resolve_source):
            """``resolve_source`` is a zero-arg callable returning a
            ``fs.Source`` -- shared by the one-click default download and
            the free-form "provide your own source" field, which differ
            only in how the source gets resolved."""
            from ..build import fetch_snapshot as fs

            try:
                source = resolve_source(fs)

                def progress(text, end="\n"):
                    dl_state["message"] = text.strip("\r")

                DEFAULT_DOWNLOAD_PATH.parent.mkdir(parents=True, exist_ok=True)
                fs.download(source, DEFAULT_DOWNLOAD_PATH, progress=progress)
                dl_state["message"] = "Done."
                resolved.put(DEFAULT_DOWNLOAD_PATH)
            except fs.FetchError as exc:
                dl_state["message"] = f"Failed: {exc}"
            finally:
                dl_state["running"] = False

        def _start(resolve_source) -> None:
            if dl_state["running"]:
                return
            dl_state.update(running=True, message="Starting...")
            threading.Thread(target=_run_download, args=(resolve_source,),
                             daemon=True).start()
            tick.set(tick.get() + 1)

        @reactive.effect
        @reactive.event(input.download_default)
        def _start_default_download():
            _start(lambda fs: fs.resolve_zenodo_record(DEFAULT_SNAPSHOT_ZENODO_DOI))

        @reactive.effect
        @reactive.event(input.download)
        def _start_download():
            source_text = (input.source() or "").strip()
            if not source_text:
                return

            def resolve(fs):
                if _looks_like_a_manifest(source_text):
                    return fs.resolve_manifest(source_text)
                if source_text.startswith(("http://", "https://")):
                    return fs.Source(url=source_text)
                return fs.resolve_zenodo_record(source_text)

            _start(resolve)

        @output
        @render.ui
        def download_status():
            tick.get()  # a dependency so a click re-renders this immediately
            if dl_state["running"]:
                reactive.invalidate_later(0.5)
            text = dl_state["message"]
            return ui.div(text, class_="small mt-2") if text else ui.div()

    return App(app_ui, server)
