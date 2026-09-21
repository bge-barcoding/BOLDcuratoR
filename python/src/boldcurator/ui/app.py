"""The curation screens: data input, species, BINs, the five BAGS grades, specimens.

**Why the BAGS screens are group navigators, not one long table.**
Grade C means "this species is split across more than one BIN" and grade E means
"this BIN holds more than one species". In both, the unit of work is a single
species-BIN problem, and a flat table of every grade-C record mixes dozens of
unrelated problems together. Each grade is therefore split into groups -- one
per species for A, B and D, one per species x BIN for C, one per shared BIN for
E -- and the screen shows a list of groups beside one group's specimens, with
Previous/Next to walk through them.

The R app renders every group as a collapsed accordion panel. A navigator is
used here instead for two reasons: it is what "focus on one problem at a time"
actually looks like, and a grade with several hundred groups would otherwise
render several hundred tables into the DOM at once.

Everything expensive happens below this file. `ui/state.py` owns the session,
`core/table.py` pages the specimen table, `core/grouping.py` builds the groups.
This module lays them out and wires the clicks.
"""

from __future__ import annotations

import datetime as _dt
import json
import tempfile
import threading
from pathlib import Path

import pandas as pd
from shiny import App, reactive, render, ui

from .. import __version__
from ..config.constants import (
    CONTINENT_COUNTRIES,
    DEFAULT_SESSIONS_PATH,
    DEFAULT_SNAPSHOT_DIR,
    DEFAULT_SNAPSHOT_ZENODO_DOI,
    DOWNLOAD_LIMITS,
    FLAG_OPTIONS,
)
from ..core.grouping import (
    GRADE_DESCRIPTIONS,
    GRADES,
    PRIORITY_GRADES,
    SPECIES_GRADES,
)
from ..core.table import DEFAULT_PAGE_SIZE
from ..data.snapshot import SnapshotError, SnapshotStore
from ..io import exports as export_io
from ..io.annotations import merge_annotations
from ..io.session import SessionStore
from .setup import _pick_snapshot_file
from .format import (
    BIN_LABELS,
    BOLD_ATTRIBUTION_SHORT,
    BOLD_ATTRIBUTION_TEXT,
    CC_BY_SA_URL,
    CHECKLIST_LABELS,
    CONCORDANCE_COLOURS,
    GAP_LABELS,
    GAP_STATUS_COLOURS,
    GRADE_COLOURS,
    GROUP_COLUMNS,
    GROUP_LABELS,
    bold_bin_url,
    bold_record_url,
    bold_species_url,
    present,
    value_box,
)
from .state import AppState, ResultTooLargeToAnalyse

PAGE_SIZES = [25, 50, 100, 250, 500]


# --------------------------------------------------------------------------
# Snapshot file management (round 5, file handling items 1-3)
# --------------------------------------------------------------------------
#
# "Which file, downloaded when, what BOLD package version" needs an honest
# answer even for a file nobody downloaded through this app (a colleague's
# copy, a shared drive) -- there is no reliable "download date" for that
# case, so a small sidecar JSON records it accurately for anything this app
# *does* fetch or copy in, and the file's own mtime is the best available
# fallback for anything it didn't.


def _provenance_path(snapshot_path: Path) -> Path:
    return snapshot_path.with_suffix(snapshot_path.suffix + ".meta.json")


def _write_provenance(snapshot_path: Path, *, source: str) -> None:
    meta = {"downloaded_at": _dt.datetime.now().isoformat(timespec="seconds"),
            "source": source}
    _provenance_path(snapshot_path).write_text(json.dumps(meta), encoding="utf-8")


def _read_provenance(snapshot_path: Path) -> dict:
    meta_path = _provenance_path(snapshot_path)
    if meta_path.exists():
        try:
            return json.loads(meta_path.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError):
            pass
    return {}


def _obtained_date(snapshot_path: Path) -> str:
    recorded = _read_provenance(snapshot_path).get("downloaded_at")
    if recorded:
        return recorded
    try:
        mtime = snapshot_path.stat().st_mtime
    except OSError:
        return "unknown"
    return (_dt.datetime.fromtimestamp(mtime).isoformat(timespec="seconds")
            + " (file's own modified date -- not downloaded through this app)")


def _all_columns_ordered(frame: pd.DataFrame) -> list[str]:
    """Every column ``frame`` carries, curated ones first -- nothing dropped.

    Matches the original R app's own choice (`PREFERRED_COLUMNS`/
    `order_columns` in `R/config/constants.R` and `R/utils/annotation_utils.R`):
    the curated/annotation columns lead, the rest of the ~71 BOLD columns
    follow in whatever order the snapshot has them, and the table scrolls
    horizontally (`SCROLL_CLASS`) rather than hiding anything. ``bags_grade``
    is absent from a page until a summary screen has been opened -- see
    ``SearchState.grade_lookup`` -- so it simply isn't in ``frame`` yet, not
    specially excluded here.
    """
    preferred = [c for c in GROUP_COLUMNS if c in frame.columns]
    rest = [c for c in frame.columns if c not in GROUP_COLUMNS]
    return preferred + rest

#: Why each specimen-handling TSV download can come back empty, keyed the same
#: way ``SearchState.export_specimens`` is.
_EMPTY_REASONS = {
    "all": "no records in this result",
    "selected": "no specimens selected",
    "annotated": "no flags, updated IDs or notes recorded",
    "curation_report": "no flags, updated IDs or notes recorded",
}


def _stream_file(path: Path, tmpdir: tempfile.TemporaryDirectory):
    """Yield one file's bytes in chunks, then clean up its temp directory.

    ``@render.download_button`` wants yielded bytes, not a path to a file it
    did not create -- "returning a path to a temp file you created" is exactly
    the mistake that leaves the temp directory behind. Chunking keeps a large
    FASTA export from doubling its own size in memory just to hand it to the
    browser.
    """
    try:
        with open(path, "rb") as fh:
            while True:
                chunk = fh.read(1 << 20)
                if not chunk:
                    break
                yield chunk
    finally:
        tmpdir.cleanup()


def _with_checked(frame: pd.DataFrame, annotations) -> pd.DataFrame:
    """Add the ``checked`` column -- ``Annotations.working``, not ``selected``.

    Not part of ``merge_annotations``: that function's six columns are exactly
    what an export ships, and the working selection is UI scratch space with
    no curatorial meaning to export or persist.
    """
    if frame is None or len(frame) == 0:
        return frame if frame is not None else pd.DataFrame()
    out = frame.copy()
    working = annotations.working
    out["checked"] = [str(p) in working for p in out.get("processid", [])]
    return out


def _annotation_controls(prefix: str) -> list:
    """Flag / note / corrected-ID, plus Apply. Repeated per screen, so shared.

    Explicit widths, because these sit in a flex row: a Shiny input is a block
    element and will otherwise take the full width and stack, turning a
    one-line toolbar into half a screen of form.

    Round 6, curation tools item 6: no per-field label stacked above its own
    input any more -- that was three different heights (a one-word "Flag"
    label next to a much longer "Corrected identification" one) is exactly
    what made the toolbar look uneven, on top of taking a second line
    vertically it didn't need. A compact inline "Flag" tag replaces its
    label (a `<select>`'s own options don't show a placeholder the way a
    text input's greyed-out text can); the two text fields use `placeholder`
    instead of `label` -- same information, without a label row of its own.

    "Apply to checked" acts on ``Annotations.working`` -- the disposable
    bulk-edit selection, not the representative pick. See ``io.annotations``'s
    module docstring.
    """
    return [
        ui.div(
            ui.tags.span("Flag", class_="small text-muted"),
            ui.input_select(f"{prefix}_flag", None,
                            choices=sorted(FLAG_OPTIONS), width="115px"),
            style="display:flex;align-items:center;gap:6px;",
        ),
        ui.input_text(f"{prefix}_note", None, placeholder="Curator note",
                     width="170px"),
        ui.input_text(f"{prefix}_updated_id", None,
                     placeholder="Corrected identification", width="150px"),
        ui.input_action_button(f"{prefix}_apply", "Apply to checked",
                               class_="btn-primary btn-sm"),
    ]


def _grade_panel(grade: str) -> ui.Tag:
    """One grade's screen, with its colour and a mark if it is a priority.

    E and C are where the barcode and the name disagree, so they are where a
    curator's time goes. The nav says so rather than leaving them to be found
    among five identical entries.
    """
    colour = GRADE_COLOURS[grade]
    priority = grade in PRIORITY_GRADES
    title = ui.span(
        ui.span("●", style=f"color:{colour};margin-right:6px;") if priority
        else ui.span("", style="margin-right:6px;"),
        ui.tags.strong(f"BAGS {grade}") if priority else f"BAGS {grade}",
    )
    return ui.nav_panel(
        title,
        ui.div(
            ui.tags.strong(f"BAGS Grade {grade}"),
            ui.tags.span(f" — {GRADE_DESCRIPTIONS[grade]}", style="opacity:.9;"),
            ui.tags.span("  ·  work here first", style="opacity:.85;")
            if priority else ui.span(),
            style=f"background:{colour};color:#fff;padding:8px 14px;"
                  "border-radius:5px;margin-bottom:10px;",
        ),
        ui.div(ui.output_ui(f"grade_{grade}_body"), class_="bc-fill-output"),
        value=f"grade_{grade}",
    )


def _banner_text(warning: str) -> str:
    """A search over a long taxon list can produce a "No records for:
    <hundreds of names>" warning that, rendered in full in the banner --
    above the nav, spanning the whole app width -- squashes everything below
    it into a sliver. The Gap analysis tab already lists every typed taxon's
    Found/Missing status once a search has run (which it has, by the time
    the banner can render at all), so the specifics belong there, not
    repeated as a wall of text here. Matched by prefix, not a blanket
    rewrite: the ambiguous-name and missing-dataset/project-code warnings
    the banner also renders have no Gap analysis equivalent to point to, so
    they keep their own detail. The "Check size" pre-check box
    (``estimate_box``) is a separate warnings list built before a search has
    even run -- it keeps the full unmatched-names detail unchanged, since
    there is no Gap analysis tab yet to point to instead.
    """
    if warning.startswith("No records for: "):
        return (
            "Some of the taxa you typed did not match any records in this "
            "snapshot -- check the spelling, or see the Gap analysis tab "
            "for exactly which ones.")
    return warning


def create_app(snapshot: str | Path, *, page_size: int = DEFAULT_PAGE_SIZE,
               sessions_path: str | Path | None = None) -> App:
    store = SnapshotStore(snapshot)
    info = store.info()
    sessions_path = sessions_path or DEFAULT_SESSIONS_PATH

    app_ui = ui.page_fluid(
        # Round 5, items 6/7/8/10: the page itself must never need its own
        # vertical scrollbar -- only a table (".bc-scroll", see _table) does,
        # and only when the window is too short for it. Without this, a
        # table capped at a fixed viewport-relative height (the old
        # "max-height:62vh") still left the *rest* of a screen's own chrome
        # (toolbars, captions, a BAGS group's note) free to push the whole
        # page past 100vh -- invisible with few rows, visible the moment
        # anything nudges the total over, e.g. the "Showing N of M rows"
        # note that only appears past 500 rows. Fixed with a real flex chain
        # instead: ".bc-app-shell" pins the header/banner/nav row to exactly
        # 100vh, ".bc-fill-output"/".bc-tab-body" carry that height down
        # through Shiny's own output wrapper div into each screen's markup,
        # and only the last child of ".bc-tab-body" (always the table, see
        # each *_body function below) is allowed to grow and scroll -- every
        # row above it (toolbars, captions) keeps its natural height.
        # ".tab-pane.active" also gets its own overflow-y:auto as a fallback
        # for the tabs with no table at all (Data, Search): if their content
        # is ever taller than the window, that tab scrolls on its own rather
        # than the whole page doing it.
        ui.tags.style("""
            html, body { height: 100%; margin: 0; }
            body { overflow: hidden; }
            .bc-app-shell {
                height: 100vh; display: flex; flex-direction: column;
                overflow: hidden;
            }
            .bc-app-shell > * { flex: none; }
            .bc-nav-fill {
                flex: 1 1 auto; min-height: 0;
                display: flex; flex-direction: column;
            }
            .bc-nav-fill > .row {
                flex: 1 1 auto; min-height: 0; flex-wrap: nowrap;
                align-items: stretch;
            }
            .bc-nav-fill .row > .col-sm-2 { overflow-y: auto; }
            .bc-nav-fill .row > .col-sm-10 {
                display: flex; flex-direction: column; min-height: 0;
            }
            .bc-nav-fill .tab-content {
                flex: 1 1 auto; min-height: 0; position: relative;
            }
            .bc-nav-fill .tab-pane.active {
                display: flex !important; flex-direction: column;
                height: 100%; min-height: 0; overflow-y: auto;
                /* Round 7, search tab item 1: overflow-y:auto alone makes a
                   browser compute overflow-x as auto too (the CSS spec's own
                   visible/non-visible interaction rule), so Bootstrap's own
                   row/column gutter (a .row is deliberately slightly wider
                   than its parent via negative margins, self-cancelling
                   against each .col's matching padding) showed up as a real
                   horizontal scrollbar on the Search tab's form -- nothing
                   was actually cut off by hiding it, only that unused gutter
                   sliver. Only the table tabs (".bc-scroll", nested deeper)
                   should ever scroll sideways. */
                overflow-x: hidden;
            }
            /* A grade tab's own coloured banner (_grade_panel) sits above
               ".bc-fill-output" in the same tab-pane -- keep its natural
               height instead of letting flex shrink it. */
            .bc-nav-fill .tab-pane.active > *:not(.bc-fill-output) {
                flex: none;
            }
            .bc-fill-output, .bc-fill-output > .shiny-html-output {
                flex: 1 1 auto; min-height: 0;
                display: flex; flex-direction: column;
            }
            .bc-tab-body {
                flex: 1 1 auto; min-height: 0; height: 100%;
                display: flex; flex-direction: column;
            }
            .bc-tab-body > * { flex: none; }
            .bc-tab-body > *:last-child {
                flex: 1 1 auto; min-height: 0; overflow: auto;
            }
            /* Round 5, item 8: compact rows, not wrapped text -- a wide
               table (item 9's full column set) scrolls horizontally instead
               of every cell wrapping to several lines and inflating row
               height. */
            .bc-scroll td, .bc-scroll th {
                white-space: nowrap; padding-top: 3px; padding-bottom: 3px;
            }
            /* Round 6, all tables item 1: one long value (a free-text notes
               field, say) used to stretch its whole column -- and every row
               with it -- to fit, however long. Capped per cell; a sticky
               column's own inline width (STICKY_COLUMN_WIDTHS) already wins
               over this, being more specific, so this only affects the
               ordinary scrolling columns. The full value is still one hover
               away via each cell's own `title` attribute (_table's default
               cell renderer). */
            .bc-scroll td {
                max-width: 280px; overflow: hidden; text-overflow: ellipsis;
            }
            /* Round 5, item 12: a download click's own visible
               acknowledgement -- see the click listener below. */
            .bc-toast {
                position: fixed; bottom: 20px; right: 20px; z-index: 2000;
                background: #202020; color: #fff; padding: 10px 16px;
                border-radius: 6px; font-size: 13px; opacity: 0;
                transition: opacity 0.3s ease; pointer-events: none;
                max-width: 320px; box-shadow: 0 2px 8px rgba(0,0,0,0.3);
            }
        """),
        ui.tags.div(id="bc-toast", class_="bc-toast"),
        # One delegated listener, attached to the page once. The specimen and
        # group tables are re-rendered as raw HTML on every click (paging,
        # sorting, "next problem"...), which replaces the checkboxes' own
        # elements each time -- a listener attached to *them* would need
        # re-attaching after every render and silently stop working after the
        # first. Delegating to `document` sidesteps that entirely.
        ui.tags.script(f"""
            document.addEventListener('change', function(e) {{
                if (!e.target) return;
                if (e.target.classList.contains('{ROW_CHECK_CLASS}')) {{
                    Shiny.setInputValue('row_check',
                        {{pid: e.target.dataset.pid, checked: e.target.checked}},
                        {{priority: 'event'}});
                }} else if (e.target.classList.contains('{ROW_REP_CLASS}')) {{
                    Shiny.setInputValue('row_rep',
                        {{pid: e.target.dataset.pid, checked: e.target.checked}},
                        {{priority: 'event'}});
                }}
            }});
            document.addEventListener('click', function(e) {{
                var th = e.target.closest && e.target.closest('.{SORT_HEADER_CLASS}');
                if (th) {{
                    Shiny.setInputValue(th.dataset.sortInput, th.dataset.sortCol,
                        {{priority: 'event'}});
                }}
                var del = e.target.closest && e.target.closest('.bc-del-snapshot');
                if (del) {{
                    Shiny.setInputValue('delete_snapshot_click', del.dataset.path,
                        {{priority: 'event'}});
                }}
                // Round 5, item 12: a curator running the packaged desktop
                // build in a chrome-less window (see desktop.py) has no
                // visible browser UI at all -- no toolbar, no
                // download-shelf/bubble a normal browser tab would show --
                // so a real, successful download can look like nothing
                // happened. This toast is that visible acknowledgement.
                // (A native pywebview window's own downloads were actually
                // broken until desktop._enable_webview_downloads -- see
                // that function's docstring -- so this toast could fire on
                // a click that pywebview then silently cancelled. Fixed
                // there, not here; this only makes the click itself
                // visible, whichever window mode is running.)
                var dl = e.target.closest &&
                    e.target.closest('a.shiny-download-link');
                if (dl && !dl.classList.contains('disabled')) {{
                    var toast = document.getElementById('bc-toast');
                    if (toast) {{
                        toast.textContent = 'Downloading -- check your '
                            + "Downloads folder, or a save dialog if one "
                            + "opens.";
                        toast.style.opacity = '1';
                        clearTimeout(toast._bcTimer);
                        toast._bcTimer = setTimeout(function() {{
                            toast.style.opacity = '0';
                        }}, 4000);
                    }}
                }}
            }});
            // A table re-renders as one HTML string on every interaction
            // (paging, checking a row, sorting...), which replaces its
            // scrolling <div> wholesale -- and a browser has no scroll
            // position to carry over to a brand new element. 'shiny:value'
            // fires just *before* Shiny swaps in the new content, which is
            // the last moment the old scroll position can still be read; a
            // one-shot MutationObserver then restores it onto the
            // replacement as soon as it actually lands, however long that
            // takes.
            //
            // 'shiny:value' is a jQuery-only custom event -- Shiny triggers
            // it with a jQuery Event object, which (unlike 'change'/'click')
            // never reaches a plain document.addEventListener. It has to be
            // bound through jQuery, which Shiny already loads globally.
            $(document).on('shiny:value', function(e) {{
                var container = e.target;
                if (!container || !container.querySelector) return;
                var old = container.querySelector('.{SCROLL_CLASS}');
                if (!old) return;
                var saved = {{top: old.scrollTop, left: old.scrollLeft}};
                // jQuery's .html() empties the container and then inserts the
                // new markup as two separate mutations, sometimes delivered
                // as two separate MutationObserver callbacks -- disconnecting
                // on the first (the empty, which finds no .bc-scroll yet)
                // would miss the second, which is the one that matters. Only
                // disconnect once the replacement has actually been found.
                var observer = new MutationObserver(function() {{
                    var el = container.querySelector('.{SCROLL_CLASS}');
                    if (el) {{
                        el.scrollTop = saved.top;
                        el.scrollLeft = saved.left;
                        observer.disconnect();
                    }}
                }});
                observer.observe(container, {{childList: true, subtree: true}});
                setTimeout(function() {{ observer.disconnect(); }}, 3000);
            }});
        """),
        ui.div(
        ui.div(
            ui.tags.h4("BOLDcurator", style="margin:0;"),
            ui.tags.span(
                f"v{__version__} · {info.snapshot_id} · "
                f"{info.row_count:,} records · "
                f"{info.bin_count:,} BINs · offline, no BOLD API",
                class_="text-muted small",
                title="The app version, then the BOLD snapshot's own "
                      "version -- see the Data tab to check the snapshot "
                      "for an update.",
            ),
            ui.tags.a(BOLD_ATTRIBUTION_SHORT, href=CC_BY_SA_URL, target="_blank",
                     rel="noopener noreferrer", class_="small",
                     title=BOLD_ATTRIBUTION_TEXT),
            ui.div(
                ui.input_text("user", None, placeholder="Your name (for annotations)",
                              width="240px"),
                style="margin-left:auto;",
            ),
            style="display:flex;align-items:center;gap:16px;padding:8px 0 10px;"
                  "border-bottom:1px solid #dee2e6;margin-bottom:12px;",
        ),
        ui.output_ui("banner"),
        ui.div(
        ui.navset_pill_list(
            ui.nav_panel(
                "Data",
                # Round 6, data input items 2/3: the snapshot-file panel
                # (round 5, file handling items 1-3) and the session
                # save/load controls used to live at the top of the Search
                # tab, where -- combined with the actual search form -- they
                # pushed that tab past one page's worth of height. Its own
                # tab now, ahead of Search, so every "which data am I
                # working with" concern lives in one place. Reachable from
                # the running app, not only the one-time first-run setup
                # screen (ui/setup.py, unchanged and still what a curator
                # sees before any snapshot is configured at all).
                ui.tags.h5("Snapshot file", style="margin-top:0;"),
                ui.output_ui("snapshot_panel"),
                ui.div(
                    ui.tags.strong("Download a snapshot", class_="small"),
                    ui.div(
                        ui.input_action_button(
                            "snap_download_default",
                            "Download the latest public BOLD snapshot",
                            class_="btn-sm btn-primary"),
                        ui.input_action_button(
                            "snap_check_update", "Check for update",
                            class_="btn-sm btn-outline-secondary"),
                        style="margin:6px 0;display:flex;gap:8px;"
                              "align-items:center;flex-wrap:wrap;",
                    ),
                    ui.tags.details(
                        ui.tags.summary("Or provide your own source",
                                       class_="small text-muted"),
                        ui.div(
                            ui.input_text(
                                "snap_source", None, width="360px",
                                placeholder="A direct URL, a manifest.json "
                                           "URL, or a Zenodo record/DOI"),
                            ui.input_action_button("snap_download",
                                                   "Download",
                                                   class_="btn-sm"),
                            style="display:flex;gap:8px;align-items:center;"
                                  "margin-top:6px;flex-wrap:wrap;",
                        ),
                    ),
                    ui.tags.strong("Use an existing file instead",
                                  class_="small",
                                  style="display:block;margin-top:14px;"),
                    ui.div(
                        ui.input_text(
                            "snap_path", None, width="360px",
                            placeholder="/path/to/a/bold_snapshot.duckdb"),
                        ui.input_action_button("snap_browse", "Browse…",
                                               class_="btn-sm "
                                                     "btn-outline-secondary"),
                        ui.input_action_button(
                            "snap_copy",
                            "Copy into BOLDcurator's data folder",
                            class_="btn-sm"),
                        style="display:flex;gap:8px;align-items:center;"
                              "flex-wrap:wrap;margin-top:4px;",
                    ),
                    ui.tags.span(
                        "A download or copy lands in BOLDcurator's own "
                        "data folder as a new file -- it does not "
                        "replace the file this session is using. "
                        "Restart BOLDcurator to switch to it.",
                        class_="small text-muted",
                        style="display:block;margin-top:6px;"),
                    ui.output_ui("snapshot_mgmt_status"),
                    style="margin-top:10px;padding:10px 14px;"
                          "background:#f8f9fa;border:1px solid #dee2e6;"
                          "border-radius:5px;max-width:900px;",
                ),
                ui.tags.h5("Session", style="margin-top:22px;"),
                ui.div(
                    ui.div(
                        ui.input_text("session_name", None,
                                      placeholder="Session name",
                                      width="220px"),
                        ui.input_action_button("save_session", "Save",
                                               class_="btn-sm"),
                        ui.input_select("load_session_id", None, choices={},
                                        width="320px"),
                        ui.input_action_button("load_session", "Load",
                                               class_="btn-sm"),
                        ui.input_action_button("delete_session", "Delete",
                                               class_="btn-sm btn-outline-danger"),
                        style="display:flex;gap:8px;align-items:center;"
                              "flex-wrap:wrap;",
                    ),
                    ui.tags.span(
                        "Auto-saves every minute, under the name above (or "
                        "\"Auto-save\" if left blank).",
                        class_="small text-muted", style="display:block;"
                              "margin-top:6px;"),
                    ui.output_ui("session_status"),
                    ui.output_ui("session_location"),
                    style="margin-top:6px;padding:10px 14px;"
                          "background:#f8f9fa;border:1px solid #dee2e6;"
                          "border-radius:5px;max-width:900px;",
                ),
                ui.div(
                    BOLD_ATTRIBUTION_TEXT + " ",
                    ui.tags.a("Full licence text.", href=CC_BY_SA_URL,
                             target="_blank", rel="noopener noreferrer"),
                    class_="small text-muted", style="margin-top:18px;"
                          "max-width:900px;",
                ),
                value="data",
            ),
            ui.nav_panel(
                "Search",
                ui.row(
                    ui.column(5,
                        ui.input_text_area(
                            "taxa", "Taxa — one per line, synonyms after commas",
                            value="Nymphalidae", rows=5, width="100%"),
                        ui.input_text_area(
                            "countries", "Countries / oceans — one per line",
                            rows=3, width="100%"),
                    ),
                    ui.column(4,
                        ui.input_checkbox_group(
                            "continents", "Continents",
                            choices=list(CONTINENT_COUNTRIES), inline=False),
                        ui.div(
                            "Continents and countries are combined as a union, "
                            "not an intersection. The filter applies to the "
                            "records your taxa match; records sharing their "
                            "BINs are pulled in wherever they are from, which "
                            "is what gives the BIN its full context.",
                            class_="small text-muted", style="max-width:320px;"),
                    ),
                    ui.column(3,
                        ui.input_text_area("datasets", "Dataset codes (DS-…)",
                                           rows=3, width="100%"),
                        ui.input_text_area("projects", "Project codes",
                                           rows=3, width="100%"),
                    ),
                ),
                ui.div(
                    ui.input_action_button("check", "Check size", class_="btn-sm"),
                    ui.input_action_button("search", "Search",
                                           class_="btn-primary"),
                    style="display:flex;gap:10px;align-items:center;",
                ),
                ui.output_ui("estimate_box"),
                ui.output_ui("search_summary"),
                value="input",
            ),
            ui.nav_panel("Gap analysis",
                        ui.div(ui.output_ui("gap_body"), class_="bc-fill-output"),
                        value="gap"),
            ui.nav_panel("Species",
                        ui.div(ui.output_ui("species_body"), class_="bc-fill-output"),
                        value="species"),
            ui.nav_panel("BINs",
                        ui.div(ui.output_ui("bins_body"), class_="bc-fill-output"),
                        value="bins"),
            *[_grade_panel(g) for g in GRADES],
            ui.nav_panel("Specimens",
                        ui.div(ui.output_ui("specimens_body"), class_="bc-fill-output"),
                        value="specimens"),
            id="nav",
            widths=(2, 10),
        ),
        class_="bc-nav-fill",
        ),
        class_="bc-app-shell",
        ),
    )

    def server(input, output, session):
        state = AppState(store, page_size=page_size)
        #: One connection per browser session/tab -- SQLite handles the
        #: concurrent opens fine at this scale, and it means a tab closing
        #: doesn't affect another tab's saved-session list.
        sessions = SessionStore(sessions_path)
        session.on_ended(sessions.close)
        revision = reactive.Value(0)
        status = reactive.Value("")
        session_msg = reactive.Value("")
        offset = reactive.Value(0)
        estimate: reactive.Value = reactive.Value({})
        group_index: dict[str, reactive.Value] = {
            g: reactive.Value(0) for g in GRADES
        }
        #: Click-a-header sort state for the in-memory tables (species
        #: checklist, gap analysis, BIN dashboard, one BAGS group at a time)
        #: -- (column, descending). The specimen table sorts differently
        #: (server-side, via SpecimenTable.sort_by) because it is never
        #: materialised whole; see _spec_sort_click below.
        checklist_sort = reactive.Value(("", False))
        gap_sort = reactive.Value(("", False))
        bins_sort = reactive.Value(("", False))
        group_sort = reactive.Value(("", False))

        def touch() -> None:
            revision.set(revision.get() + 1)

        # -- snapshot file management (round 5, file handling 1-3) ---------
        #
        # A download or a copy lands as a *new* file, never overwriting the
        # one this session already has an open (read-only) DuckDB handle on
        # -- that handle stays valid for the life of this process regardless
        # of what shows up alongside it. Picking up a new file needs a
        # restart (a fresh `create_app(new_path)`/`SnapshotStore`), which
        # this panel says plainly rather than pretending to hot-swap it.
        snap_msg = reactive.Value("")
        snap_dl_state = {"running": False, "message": ""}
        #: Bumped only from a real Shiny reactive context (a click effect, or
        #: ``_snap_poll`` below) -- never from the download/copy background
        #: thread itself. ``reactive.Value.set()`` from an arbitrary OS
        #: thread is unsafe (see ``ui/setup.py``'s own ``dl_state`` for the
        #: same reasoning); the thread only ever touches ``snap_dl_state``,
        #: a plain dict, and ``_snap_poll`` is what notices it changed.
        snap_tick = reactive.Value(0)
        #: Bumped only by a click that starts a download/copy -- wakes
        #: ``_snap_poll`` up to start (re-)polling ``snap_dl_state``.
        snap_op_seq = reactive.Value(0)
        pending_delete = reactive.Value("")

        @reactive.effect
        def _snap_poll():
            snap_op_seq.get()  # dependency: (re-)start polling on each click
            if snap_dl_state["running"]:
                reactive.invalidate_later(0.4)
            with reactive.isolate():
                snap_tick.set(snap_tick.get() + 1)

        def _other_snapshot_files() -> list[Path]:
            try:
                files = sorted(DEFAULT_SNAPSHOT_DIR.glob("*.duckdb"))
            except OSError:
                return []
            current = store.path.resolve()
            return [f for f in files if f.resolve() != current]

        @output
        @render.ui
        def snapshot_panel():
            snap_tick.get()
            rows = [
                ui.div(ui.tags.strong("File in use: "), str(store.path),
                      class_="small"),
                ui.div(ui.tags.strong("BOLD package version: "),
                      f"{info.snapshot_id} (built {info.built_at})",
                      class_="small"),
                ui.div(ui.tags.strong("Obtained: "), _obtained_date(store.path),
                      class_="small"),
            ]
            others = _other_snapshot_files()
            if others:
                rows.append(ui.tags.strong(
                    "Other snapshot files in BOLDcurator's data folder",
                    class_="small", style="display:block;margin-top:10px;"))
                for f in others:
                    try:
                        size_mb = f.stat().st_size / 1e6
                    except OSError:
                        size_mb = 0.0
                    rows.append(ui.div(
                        ui.tags.span(
                            f"{f.name} -- {size_mb:,.0f} MB, obtained "
                            f"{_obtained_date(f)}", class_="small"),
                        ui.tags.button(
                            "Delete", type="button",
                            class_="btn btn-sm btn-outline-danger "
                                  "bc-del-snapshot",
                            data_path=str(f)),
                        style="display:flex;gap:10px;align-items:center;"
                              "margin-top:4px;",
                    ))
            return ui.div(*rows)

        @reactive.effect
        @reactive.event(input.delete_snapshot_click)
        def _confirm_delete_snapshot():
            path = input.delete_snapshot_click()
            if not path:
                return
            pending_delete.set(path)
            ui.modal_show(ui.modal(
                f"Delete {path}? This cannot be undone.",
                title="Delete snapshot file",
                footer=ui.div(
                    ui.input_action_button("cancel_delete_snapshot", "Cancel",
                                           class_="btn-sm"),
                    ui.input_action_button("confirm_delete_snapshot", "Delete",
                                           class_="btn-sm btn-danger"),
                ),
                easy_close=True,
            ))

        @reactive.effect
        @reactive.event(input.cancel_delete_snapshot)
        def _cancel_delete_snapshot():
            pending_delete.set("")
            ui.modal_remove()

        @reactive.effect
        @reactive.event(input.confirm_delete_snapshot)
        def _do_delete_snapshot():
            target = Path(pending_delete.get())
            pending_delete.set("")
            ui.modal_remove()
            try:
                target.unlink(missing_ok=True)
                _provenance_path(target).unlink(missing_ok=True)
                snap_msg.set(f"Deleted {target}.")
            except OSError as exc:
                snap_msg.set(f"Could not delete {target}: {exc}")
            snap_tick.set(snap_tick.get() + 1)

        def _snap_run_download(resolve_source, out_path: Path) -> None:
            from ..build import fetch_snapshot as fs

            try:
                source = resolve_source(fs)

                def progress(text, end="\n"):
                    snap_dl_state["message"] = text.strip("\r")

                out_path.parent.mkdir(parents=True, exist_ok=True)
                fs.download(source, out_path, progress=progress)
                _write_provenance(out_path, source=source.url)
                snap_dl_state["message"] = (
                    f"Downloaded to {out_path}. Restart BOLDcurator to use it.")
            except fs.FetchError as exc:
                snap_dl_state["message"] = f"Failed: {exc}"
            finally:
                # Only the plain dict, from this background thread -- see
                # ``_snap_poll`` above for why no reactive.Value is touched
                # here.
                snap_dl_state["running"] = False

        def _snap_start_download(resolve_source) -> None:
            if snap_dl_state["running"]:
                return
            out_path = (DEFAULT_SNAPSHOT_DIR
                       / f"snapshot-{export_io.timestamp()}.duckdb")
            snap_dl_state.update(running=True, message="Starting...")
            threading.Thread(target=_snap_run_download,
                             args=(resolve_source, out_path), daemon=True).start()
            snap_op_seq.set(snap_op_seq.get() + 1)

        @reactive.effect
        @reactive.event(input.snap_download_default)
        def _snap_download_default():
            _snap_start_download(
                lambda fs: fs.resolve_zenodo_record(DEFAULT_SNAPSHOT_ZENODO_DOI))

        def _snap_run_check() -> None:
            from ..build import fetch_snapshot as fs

            try:
                result = fs.check_for_update(DEFAULT_SNAPSHOT_ZENODO_DOI, store.path)
                if result.up_to_date:
                    snap_dl_state["message"] = (
                        f"Up to date -- {result.remote_snapshot_id} is the "
                        "latest published snapshot.")
                else:
                    local = result.local_snapshot_id or "unknown"
                    snap_dl_state["message"] = (
                        f"A newer snapshot is available: "
                        f"{result.remote_snapshot_id} (this session is "
                        f"using {local}). Use \"Download the latest public "
                        "BOLD snapshot\" above to get it.")
            except fs.FetchError as exc:
                snap_dl_state["message"] = f"Could not check for an update: {exc}"
            finally:
                # Plain dict only -- see _snap_run_download above.
                snap_dl_state["running"] = False

        @reactive.effect
        @reactive.event(input.snap_check_update)
        def _snap_check_update():
            if snap_dl_state["running"]:
                return
            snap_dl_state.update(running=True, message="Checking for an update...")
            threading.Thread(target=_snap_run_check, daemon=True).start()
            snap_op_seq.set(snap_op_seq.get() + 1)

        @reactive.effect
        @reactive.event(input.snap_download)
        def _snap_download_custom():
            source_text = (input.snap_source() or "").strip()
            if not source_text:
                return

            def resolve(fs):
                looks_like_manifest = (
                    source_text.startswith(("http://", "https://"))
                    and source_text.rstrip("/").endswith(".json"))
                if looks_like_manifest:
                    return fs.resolve_manifest(source_text)
                if source_text.startswith(("http://", "https://")):
                    return fs.Source(url=source_text)
                return fs.resolve_zenodo_record(source_text)

            _snap_start_download(resolve)

        @reactive.effect
        @reactive.event(input.snap_browse)
        def _snap_browse():
            chosen = _pick_snapshot_file()
            if chosen:
                ui.update_text("snap_path", value=chosen)
            else:
                snap_msg.set("No file chosen -- type the path above instead "
                             "if Browse… didn't work.")

        def _snap_run_copy(src: Path, out_path: Path) -> None:
            try:
                out_path.parent.mkdir(parents=True, exist_ok=True)
                tmp = out_path.with_suffix(out_path.suffix + ".part")
                total = src.stat().st_size
                written = 0
                with open(src, "rb") as fin, open(tmp, "wb") as fout:
                    while chunk := fin.read(1 << 20):
                        fout.write(chunk)
                        written += len(chunk)
                        pct = written / total if total else 0
                        snap_dl_state["message"] = (
                            f"Copying... {pct:.0%} "
                            f"({written / 1e6:.0f} / {total / 1e6:.0f} MB)")
                tmp.replace(out_path)
                _write_provenance(out_path, source=str(src))
                snap_dl_state["message"] = (
                    f"Copied to {out_path}. Restart BOLDcurator to use it.")
            except OSError as exc:
                snap_dl_state["message"] = f"Copy failed: {exc}"
            finally:
                # Plain dict only -- see _snap_run_download above.
                snap_dl_state["running"] = False

        @reactive.effect
        @reactive.event(input.snap_copy)
        def _snap_copy():
            if snap_dl_state["running"]:
                return
            candidate = Path((input.snap_path() or "").strip()).expanduser()
            if not candidate.exists():
                snap_msg.set(f"No file at {candidate}.")
                return
            if candidate.resolve().parent == DEFAULT_SNAPSHOT_DIR.resolve():
                snap_msg.set(f"{candidate} is already in BOLDcurator's data "
                             "folder.")
                return
            try:
                with SnapshotStore(candidate) as candidate_store:
                    candidate_store.info()
            except SnapshotError as exc:
                snap_msg.set(f"Not a valid snapshot: {exc}")
                return
            out_path = (DEFAULT_SNAPSHOT_DIR
                       / f"snapshot-{export_io.timestamp()}.duckdb")
            snap_dl_state.update(running=True, message="Starting...")
            threading.Thread(target=_snap_run_copy,
                             args=(candidate, out_path), daemon=True).start()
            snap_op_seq.set(snap_op_seq.get() + 1)

        @output
        @render.ui
        def snapshot_mgmt_status():
            snap_tick.get()
            if snap_dl_state["running"]:
                reactive.invalidate_later(0.5)
            text = snap_dl_state["message"] or snap_msg.get()
            return ui.div(text, class_="small mt-2") if text else ui.div()

        def _register_memory_sort(input_id: str, sort_state: reactive.Value):
            """Click a header: same column flips direction, a new one sorts

            ascending. Shared by every table whose frame already lives in
            memory (the specimens table is the one exception -- it sorts by
            fetching one column server-side instead, see _spec_sort_click).
            """
            @reactive.effect
            @reactive.event(input[input_id])
            def _sort():
                column = input[input_id]()
                if not column:
                    return
                current_column, current_desc = sort_state.get()
                sort_state.set((column, False if column != current_column
                               else not current_desc))
                touch()

        for _input_id, _state in (("checklist_sort_click", checklist_sort),
                                  ("gap_sort_click", gap_sort),
                                  ("bins_sort_click", bins_sort),
                                  ("group_sort_click", group_sort)):
            _register_memory_sort(_input_id, _state)

        def _sorted_by(frame: pd.DataFrame, sort_state: reactive.Value) -> pd.DataFrame:
            column, descending = sort_state.get()
            if not column or frame is None or len(frame) == 0 or column not in frame.columns:
                return frame
            return frame.sort_values(column, ascending=not descending,
                                     kind="stable", na_position="last")

        def current():
            revision.get()
            return state.search

        # -- search --------------------------------------------------------

        def _form() -> dict:
            return {
                "taxa_text": input.taxa() or "",
                "countries_text": input.countries() or "",
                "continents": list(input.continents() or ()),
                "dataset_text": input.datasets() or "",
                "project_text": input.projects() or "",
            }

        @reactive.effect
        @reactive.event(input.check)
        def _check():
            estimate.set(state.estimate(**_form()))

        @reactive.effect
        @reactive.event(input.search)
        def _search():
            state.user = (input.user() or "").strip()
            estimate.set({})
            status.set(state.run_search(**_form()))
            offset.set(0)
            for value in group_index.values():
                value.set(0)
            if state.search is not None:
                ui.update_navset("nav", selected="species")
            touch()

        # -- session save/resume (plan 3.8) ---------------------------------
        #
        # A saved session's identity is its (slugified) name, so saving under
        # a name already used updates that entry in place -- matching
        # SessionStore.save()'s own upsert semantics -- rather than piling up
        # duplicates every time a curator saves their progress.

        def _slugify(name: str) -> str:
            slug = "".join(c if c.isalnum() else "-" for c in name.strip().lower())
            while "--" in slug:
                slug = slug.replace("--", "-")
            return slug.strip("-")

        def _session_choices() -> dict[str, str]:
            return {s.session_id: s.describe() for s in sessions.list_sessions()}

        @reactive.effect
        def _populate_sessions():
            """Runs once at session startup -- it reads no reactive input, so
            Shiny never re-invalidates it. Save/load/delete each refresh the
            list themselves afterwards."""
            ui.update_select("load_session_id", choices=_session_choices())

        def _do_save(name: str, *, default_name: str, quiet_on_no_search: bool
                    ) -> None:
            """Shared by the Save button and the auto-save timer below --

            same slugify-as-identity upsert either way, so a name typed once
            covers both manual and scheduled saves of the same session.
            ``quiet_on_no_search`` skips the "run a search first" message for
            the timer, which fires on a schedule regardless of whether there
            is anything to save yet.
            """
            if state.search is None:
                if not quiet_on_no_search:
                    session_msg.set("Run a search first.")
                return
            session_id = _slugify(name) or _slugify(default_name)
            try:
                saved = state.save_session(sessions, session_id,
                                           name=name or default_name)
            except (ValueError, ResultTooLargeToAnalyse) as exc:
                session_msg.set(str(exc))
                return
            ui.update_select("load_session_id", choices=_session_choices(),
                             selected=session_id)
            session_msg.set(f"Saved {saved.name or saved.session_id!r} -- "
                            f"{saved.record_count:,} records.")

        @reactive.effect
        @reactive.event(input.save_session)
        def _save_session():
            name = (input.session_name() or "").strip()
            _do_save(name, default_name=f"session-{export_io.timestamp()}",
                     quiet_on_no_search=False)

        @reactive.effect
        def _autosave_tick():
            """Every minute, unconditionally -- round 5, item 4: auto-save is

            always on, fixed at one minute, with no checkbox or interval to
            turn it off or change (previously an opt-in checkbox with a
            configurable interval). `reactive.invalidate_later` has to be
            called on every run to keep rescheduling itself.
            `session_name` is read isolated: typing a name should not itself
            trigger a save, only the timer firing should.
            """
            reactive.invalidate_later(60)
            with reactive.isolate():
                name = (input.session_name() or "").strip()
            _do_save(name, default_name="Auto-save", quiet_on_no_search=True)

        @reactive.effect
        @reactive.event(input.load_session)
        def _load_session():
            session_id = input.load_session_id()
            saved = sessions.load(session_id) if session_id else None
            if saved is None:
                session_msg.set("Nothing to load -- save a session first.")
                return
            state.user = (input.user() or "").strip() or state.user
            text, warnings = state.resume_session(saved)
            offset.set(0)
            for value in group_index.values():
                value.set(0)
            ui.update_navset("nav", selected="species")
            session_msg.set(" ".join([text] + warnings))
            touch()

        @reactive.effect
        @reactive.event(input.delete_session)
        def _delete_session():
            session_id = input.load_session_id()
            if session_id and sessions.delete(session_id):
                session_msg.set("Deleted.")
            ui.update_select("load_session_id", choices=_session_choices())

        @output
        @render.ui
        def session_status():
            text = session_msg.get()
            return ui.div(text, class_="small text-muted mt-1") if text else ui.div()

        @output
        @render.ui
        def session_location():
            """Round 5, item 5: a curator asked where sessions are saved and

            what would lose them -- put the real answer on screen instead of
            leaving it to be asked again. Sessions live in one SQLite file
            (`io.session.SessionStore`); they are lost only by deleting that
            file, deleting the session with the Delete button above, or (for
            a specific session) resuming it against a different snapshot's
            worth of retracted records -- never by closing the app or the
            browser tab.
            """
            return ui.div(
                f"Sessions are stored in {sessions_path} -- deleting that "
                "file (or the Delete button above) is the only way to lose "
                "them; closing the app does not.",
                class_="small text-muted mt-1",
            )

        @output
        @render.ui
        def banner():
            search = current()
            if search is None or not search.warnings:
                return ui.div()
            return ui.div(
                *[ui.div(_banner_text(w)) for w in search.warnings],
                class_="alert alert-warning py-2 px-3 small mb-3",
            )

        @output
        @render.ui
        def estimate_box():
            counts = estimate.get()
            if not counts:
                return ui.div()
            if counts.get("error"):
                return ui.div(counts["error"],
                              class_="alert alert-warning py-2 px-3 small mt-3")
            colour = "#dc3545" if counts["over_limit"] else "#2c7fb8"
            return ui.div(
                ui.div(
                    value_box(f"{counts['seed_records']:,}", "Matching records",
                              "#6c757d"),
                    value_box(f"{counts['seed_bins']:,}", "BINs", "#6c757d"),
                    value_box(f"{counts['expanded_records']:,}",
                              "After BIN expansion", colour),
                    style="display:flex;gap:10px;flex-wrap:wrap;margin-top:14px;",
                ),
                ui.div("Resolved: " + ", ".join(counts["resolved"])
                       if counts["resolved"] else "",
                       class_="small text-muted mt-2"),
                ui.div(
                    f"Over the limit of {DOWNLOAD_LIMITS['MAX_RECORDS']:,} records "
                    f"/ {DOWNLOAD_LIMITS['MAX_BINS']:,} BINs. Narrow the taxa or "
                    "add a geographic filter.",
                    class_="alert alert-danger py-2 px-3 small mt-2",
                ) if counts["over_limit"] else ui.div(),
                *[ui.div(w, class_="alert alert-warning py-2 px-3 small mt-2")
                  for w in counts.get("warnings", [])],
            )

        @output
        @render.ui
        def search_summary():
            search = current()
            text = status.get()
            if search is None:
                return ui.div(text, class_="small text-muted pt-3")
            plan = search.plan
            return ui.div(
                ui.div(
                    value_box(f"{plan.expanded_records:,}", "Records", "#2c7fb8"),
                    value_box(f"{plan.seed_bins:,}", "BINs", "#6c757d"),
                    value_box(f"{len(state.annotations.selected):,}",
                              "Representative", "#28a745"),
                    value_box(f"{len(state.annotations.working):,}", "Checked",
                              "#f0ad4e"),
                    value_box(
                        f"{len(state.annotations.annotated_processids()):,}",
                        "Annotated", "#6f42c1"),
                    style="display:flex;gap:10px;flex-wrap:wrap;margin-top:14px;",
                ),
                ui.div(f"Searched: {search.query_label}", class_="small mt-2"),
                ui.div(text, class_="small text-muted"),
                ui.download_button("dl_search_results", "Download CSV",
                                   class_="btn-sm mt-2"),
            )

        # -- the guard every summary screen shares -------------------------

        def _needs_analysis(render_body):
            search = current()
            if search is None:
                return ui.div("Run a search first.", class_="text-muted")
            try:
                was_ready = search.analysis_is_ready
                result = render_body(search)
            except ResultTooLargeToAnalyse as exc:
                return ui.div(str(exc), class_="alert alert-warning py-2 px-3")
            if not was_ready and search.analysis_is_ready:
                # The analysis (and its one-time auto-selection) just ran for
                # the first time. That mutates state.annotations directly, not
                # through a reactive.Value, so search_summary's Representative/
                # Checked/Annotated counts -- which have no dependency of their
                # own on it -- would otherwise sit at their pre-search values
                # until some unrelated click happened to touch() again.
                touch()
            return result

        # -- gap analysis -----------------------------------------------------
        #
        # Round 4, item 5: its own tab, above Species -- it was crowding the
        # checklist, and "did the search find every taxon typed" is a
        # different question from "here is every species found."

        @output
        @render.ui
        def gap_body():
            def body(search):
                gaps = search.gap_analysis(store)
                # Gap analysis only has something to say when taxa were
                # actually typed -- a dataset/project-code-only search has no
                # "did the search find what I typed" question to answer.
                if not len(gaps):
                    return ui.div(
                        "No taxa were typed in this search -- nothing to check "
                        "against.", class_="text-muted")
                found = int((gaps["status"] == "Found").sum())
                missing = int((gaps["status"] == "Missing").sum())
                return ui.div(
                    ui.tags.span(
                        "Every taxon typed, matched against synonyms too, and "
                        "whether the search actually found it.",
                        class_="text-muted"),
                    ui.div(
                        value_box(f"{found:,}", "Found", "#28a745"),
                        value_box(f"{missing:,}", "Missing", "#dc3545"),
                        style="display:flex;gap:10px;margin:10px 0 12px;"
                              "flex-wrap:wrap;",
                    ),
                    # Round 5, item 11: the same workbook the Species tab
                    # downloads (it already carries a Gap analysis sheet
                    # alongside the checklist) -- reachable from here too, so
                    # a curator working this tab doesn't have to switch tabs
                    # for it. A second output id, not a second element bound
                    # to "dl_species_analysis" -- two DOM elements sharing one
                    # Shiny output id is unreliable (duplicate HTML ids), so
                    # this gets its own id wired to the same export below.
                    ui.download_button("dl_gap_analysis",
                                       "Download species analysis (xlsx)",
                                       class_="btn-sm mb-2"),
                    ui.HTML(_gap_html(_sorted_by(gaps, gap_sort),
                                      sort_state=gap_sort.get())),
                    class_="bc-tab-body",
                )
            return _needs_analysis(body)

        # -- species checklist ---------------------------------------------

        @output
        @render.ui
        def species_body():
            def body(search):
                # Round 3, item 4: mean quality isn't something a curator
                # scanning the checklist needs -- dropped from the on-screen
                # table (and the xlsx export, SearchState.export_species_analysis)
                # rather than the underlying build_species_checklist frame,
                # which other callers (tests, a future consumer) may still
                # want it from.
                checklist = _sorted_by(
                    search.checklist(store).drop(columns=["mean_quality_score"],
                                                 errors="ignore"),
                    checklist_sort)
                counts = search.grade_counts(store)
                return ui.div(
                    ui.div(
                        *[value_box(f"{counts.get(g, 0):,}", f"Grade {g}",
                                    GRADE_COLOURS[g]) for g in GRADES],
                        style="display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
                    ),
                    ui.download_button("dl_species_analysis",
                                       "Download species analysis (xlsx)",
                                       class_="btn-sm mb-2"),
                    ui.HTML(_checklist_html(checklist, sort_state=checklist_sort.get())),
                    class_="bc-tab-body",
                )
            return _needs_analysis(body)

        # -- BIN dashboard --------------------------------------------------

        @output
        @render.ui
        def bins_body():
            def body(search):
                analysis = search.analysis(store).bin_analysis
                summary = analysis["summary"]
                # Round 3, item 5: "share of result" isn't something a
                # curator scanning the BIN dashboard needs -- dropped from
                # the on-screen table only; the BIN analysis xlsx download
                # (analysis["content"] itself) is unchanged.
                content = _sorted_by(
                    analysis["content"].drop(columns=["bin_coverage"], errors="ignore"),
                    bins_sort)
                return ui.div(
                    ui.div(
                        value_box(f"{summary['total_bins']:,}", "Total BINs",
                                  "#2c7fb8"),
                        value_box(f"{summary['concordant_bins']:,}",
                                  "Concordant BINs", "#28a745"),
                        value_box(f"{summary['discordant_bins']:,}",
                                  "Discordant BINs", "#dc3545"),
                        style="display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
                    ),
                    ui.download_button("dl_bin_analysis",
                                       "Download BIN analysis (xlsx)",
                                       class_="btn-sm mb-2"),
                    ui.HTML(_bins_html(content, sort_state=bins_sort.get())),
                    class_="bc-tab-body",
                )
            return _needs_analysis(body)

        # -- the BAGS grade screens ----------------------------------------

        def _grade_body(grade: str):
            def body(search):
                groups = search.groups(store, grade)
                if not groups:
                    return ui.div(f"No species graded {grade} in this result.",
                                  class_="text-muted")
                index = min(group_index[grade].get(), len(groups) - 1)
                group = groups[index]
                rows = _sorted_by(
                    _with_checked(
                        merge_annotations(group.specimens, state.annotations),
                        state.annotations),
                    group_sort)
                # "Apply to checked" (and the count below) must act only on
                # this group's own records -- Annotations.working is one
                # global set, and checking rows via the per-row checkbox
                # (rather than "Check this group", which replaces it) could
                # otherwise leave an earlier group's checks live and get them
                # annotated together with this one's.
                group_pids = {str(pid) for pid in group.specimens["processid"]}
                checked_here = state.annotations.working & group_pids
                # A/B/D group one species at a time; C/E group one BIN at a
                # time (species split across BINs, or a BIN shared between
                # species) -- "problem" told a curator neither.
                unit = "species" if grade in SPECIES_GRADES else "BIN"
                plural_unit = "species" if unit == "species" else "BINs"
                # The navigator (which problem) sits in one compact row above
                # the table, not in a sidebar column beside it -- the table is
                # the thing a curator actually works in, and a fixed-width
                # sidebar was taking a third of the tab for it permanently,
                # whatever the screen width. A dropdown loses nothing a tall
                # listbox had: every problem is still one click away.
                return ui.div(
                    ui.div(
                        ui.tags.span(f"{len(groups):,} "
                                     f"{unit if len(groups) == 1 else plural_unit} "
                                     "to work through", class_="small text-muted"),
                        # Round 7, BAGS A/B/D item 1: a fixed 420px cut off a
                        # long caption ("Species: X (>10 specimens, single
                        # BIN)  (404)") -- flexible instead of another fixed
                        # guess, so it actually uses the room a wide window
                        # has rather than truncating regardless of it.
                        ui.div(
                            ui.input_select(
                                f"group_{grade}", None,
                                choices={str(i): f"{g.caption}  ({g.specimen_count})"
                                         for i, g in enumerate(groups)},
                                selected=str(index), width="100%"),
                            style="flex:1 1 auto;min-width:280px;max-width:720px;",
                        ),
                        ui.input_action_button(f"prev_{grade}", "‹ Previous",
                                               class_="btn-sm"),
                        ui.tags.span(f"{index + 1} of {len(groups):,}",
                                     class_="small text-muted"),
                        ui.input_action_button(f"next_{grade}", "Next ›",
                                               class_="btn-sm"),
                        style="display:flex;align-items:center;gap:10px;"
                              "flex-wrap:wrap;margin-bottom:8px;",
                    ),
                    ui.div(ui.tags.strong(group.caption),
                           ui.tags.span(f"  ·  {group.specimen_count} specimens",
                                        class_="text-muted small"),
                           class_="mb-1"),
                    ui.div(group.note, class_="alert alert-info py-1 px-2 small")
                    if group.note else ui.div(),
                    # Round 6, curation tools item 6: one flat row, not a
                    # column of buttons/count beside a separately-aligned
                    # cluster of labelled inputs -- that mix of a
                    # flex-direction:column block and align-items:end is
                    # what made the whole toolbar look uneven and taller
                    # than it needed to be.
                    ui.div(
                        ui.input_action_button(
                            f"selall_{grade}", "Check this group",
                            class_="btn-sm"),
                        ui.input_action_button(f"clear_{grade}",
                                               "Clear checked",
                                               class_="btn-sm"),
                        ui.tags.span(
                            f"{len(checked_here):,} checked here"
                            + (f" ({len(state.annotations.working):,} "
                               "checked in total)"
                               if len(state.annotations.working)
                               > len(checked_here) else ""),
                            class_="small text-muted"),
                        *_annotation_controls(f"g{grade}"),
                        style="display:flex;align-items:center;gap:8px;"
                              "flex-wrap:wrap;margin-bottom:10px;padding:8px;"
                              "background:#f8f9fa;border:1px solid #dee2e6;"
                              "border-radius:5px;",
                    ),
                    # Round 5, item 9: every column, like the Specimens tab --
                    # a curated subset (the old default) hid raw BOLD columns
                    # a curator might need mid-problem. `_table`'s own
                    # horizontal scroll (inherited by `_group_html`) is what
                    # makes that many columns usable.
                    ui.HTML(_group_html(rows, _all_columns_ordered(rows),
                                        sort_input="group_sort_click",
                                        sort_state=group_sort.get())),
                    class_="bc-tab-body",
                )
            return _needs_analysis(body)

        def _register_grade(grade: str):
            @output(id=f"grade_{grade}_body")
            @render.ui
            def _body():
                return _grade_body(grade)

            @reactive.effect
            @reactive.event(input[f"prev_{grade}"])
            def _prev():
                group_index[grade].set(max(0, group_index[grade].get() - 1))
                ui.update_select(f"group_{grade}",
                                 selected=str(group_index[grade].get()))
                touch()

            @reactive.effect
            @reactive.event(input[f"next_{grade}"])
            def _next():
                search = state.search
                if search is None:
                    return
                last = len(search.groups(store, grade)) - 1
                group_index[grade].set(min(last, group_index[grade].get() + 1))
                ui.update_select(f"group_{grade}",
                                 selected=str(group_index[grade].get()))
                touch()

            @reactive.effect
            @reactive.event(input[f"group_{grade}"])
            def _pick():
                chosen = input[f"group_{grade}"]()
                if chosen is not None and str(chosen).isdigit():
                    group_index[grade].set(int(chosen))
                    touch()

            @reactive.effect
            @reactive.event(input[f"selall_{grade}"])
            def _check_group():
                group = _current_group(grade)
                if group is not None:
                    # Replaces the checked set rather than adding to it.
                    # "Apply to checked" acts on whatever is checked, so if
                    # this accumulated, annotating the second problem would
                    # silently re-annotate the first -- which is the opposite
                    # of working through one problem at a time. This never
                    # touches the representative pick (annotations.selected).
                    state.annotations.clear_working()
                    for pid in group.specimens["processid"]:
                        state.annotations.set_working(str(pid))
                    touch()

            @reactive.effect
            @reactive.event(input[f"clear_{grade}"])
            def _clear_group():
                group = _current_group(grade)
                if group is not None:
                    for pid in group.specimens["processid"]:
                        state.annotations.set_working(str(pid), selected=False)
                    touch()

            @reactive.effect
            @reactive.event(input[f"g{grade}_apply"])
            def _apply_group():
                group = _current_group(grade)
                scope = ({str(pid) for pid in group.specimens["processid"]}
                        if group is not None else set())
                _apply(f"g{grade}", scope=scope)

        def _current_group(grade: str):
            search = state.search
            if search is None:
                return None
            groups = search.groups(store, grade)
            if not groups:
                return None
            return groups[min(group_index[grade].get(), len(groups) - 1)]

        for _grade in GRADES:
            _register_grade(_grade)

        # -- annotation, shared by every screen ----------------------------

        def _apply(prefix: str, scope: set[str] | None = None) -> None:
            """Flag/note/update the *checked* records, never the
            representative pick -- see ``io.annotations``'s module docstring.

            ``scope``, when given, restricts this to records that are both
            checked AND in ``scope`` -- the current BAGS group, so a check
            left over from a different group (checked individually, not via
            "Check this group", which replaces the whole set) never gets
            annotated alongside it. The Specimens tab passes no scope: it is
            one continuous table across pages, not a different table per
            page, so a checked-then-paged-away record is still meant to be
            included.
            """
            working = state.annotations.working
            checked = sorted(working if scope is None else working & scope)
            if not checked:
                status.set("Nothing checked.")
                touch()
                return
            user = (input.user() or "").strip()
            flag = input[f"{prefix}_flag"]() or ""
            note = (input[f"{prefix}_note"]() or "").strip()
            updated = (input[f"{prefix}_updated_id"]() or "").strip()
            for pid in checked:
                state.annotations.set_flag(pid, flag, user=user)
                if note:
                    state.annotations.set_note(pid, note, user=user)
                if updated:
                    state.annotations.set_updated_id(pid, updated, user=user)
            status.set(f"Annotated {len(checked):,} records")
            touch()

        # -- the paged specimen table --------------------------------------

        @output
        @render.ui
        def specimens_body():
            search = current()
            if search is None:
                return ui.div("Run a search first.", class_="text-muted")
            table = search.table
            page = table.page(offset.get())
            rows = page.rows  # already carries both "selected" and "checked"
            lookup = search.grade_lookup()
            if lookup:
                rows = rows.copy()
                rows["bags_grade"] = [
                    lookup.get(s, "") if isinstance(s, str) else ""
                    for s in rows["species"].astype(object)
                ]

            sort_label = (
                f"Sorted by {table.sort_column} "
                f"({'desc' if table.sort_descending else 'asc'})"
                if table.sort_column else "Result order (click a column header to sort)"
            )
            return ui.div(
                # Paging/sort in their own row -- kept separate from the
                # curation toolbar below (round 3, item 2) so the whole
                # thing fits the default window width instead of overflowing
                # one long flex row, matching the two-row layout the BAGS
                # C/E screens already use (_grade_body).
                ui.div(
                    ui.tags.span(sort_label, class_="small text-muted"),
                    ui.input_action_button("reset_sort", "Reset order",
                                           class_="btn-sm") if table.sort_column
                    else ui.span(),
                    ui.input_select("page_size", None,
                                    choices=[str(n) for n in PAGE_SIZES],
                                    selected=str(table.page_size), width="90px"),
                    ui.input_action_button("first", "«", class_="btn-sm"),
                    ui.input_action_button("prev", "‹", class_="btn-sm"),
                    ui.tags.span(f" {page.page_number:,} / {page.page_count:,} ",
                                 class_="small text-nowrap"),
                    ui.input_action_button("next_", "›", class_="btn-sm"),
                    ui.input_action_button("last", "»", class_="btn-sm"),
                    style="display:flex;align-items:center;gap:10px;"
                          "flex-wrap:wrap;margin-bottom:8px;",
                ),
                # Round 6, curation tools item 6: one flat row -- see the
                # matching change in _grade_body for why.
                ui.div(
                    ui.input_action_button("select_page", "Check page",
                                           class_="btn-sm"),
                    ui.input_action_button("select_all", "Check all",
                                           class_="btn-sm"),
                    ui.input_action_button("clear_selection", "Clear checked",
                                           class_="btn-sm"),
                    ui.tags.span(f"{len(state.annotations.working):,} checked",
                                class_="small text-muted"),
                    *_annotation_controls("sp"),
                    style="display:flex;align-items:center;gap:8px;flex-wrap:wrap;"
                          "margin-bottom:10px;padding:8px;background:#f8f9fa;"
                          "border:1px solid #dee2e6;border-radius:5px;",
                ),
                ui.div(
                    ui.download_button("dl_all", "Download All", class_="btn-sm"),
                    ui.download_button("dl_selected", "Download Selected",
                                       class_="btn-sm"),
                    ui.download_button("dl_annotated", "Download Annotated Records",
                                       class_="btn-sm"),
                    ui.download_button("dl_curation_report",
                                       "Download BOLD Curation Report",
                                       class_="btn-sm"),
                    ui.download_button("dl_fasta", "Download FASTA", class_="btn-sm"),
                    ui.download_button("dl_selected_fasta", "Download Selected FASTA",
                                       class_="btn-sm"),
                    style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:2px;",
                ),
                ui.tags.span(
                    "Downloads save to your computer's usual Downloads "
                    "folder (a native app window may instead show a save "
                    "dialog, defaulting to Downloads too).",
                    class_="small text-muted",
                    style="display:block;margin-bottom:10px;"),
                ui.HTML(_group_html(
                    rows, columns=_all_columns_ordered(rows), limit=len(rows),
                    sort_input="spec_sort_click",
                    sortable=frozenset(table.sortable_columns),
                    sort_state=(table.sort_column, table.sort_descending))),
                class_="bc-tab-body",
            )

        @reactive.effect
        @reactive.event(input.spec_sort_click)
        def _spec_sort_click():
            """Click a header: same column flips direction, a new one sorts

            ascending -- same rule as the in-memory tables
            (_register_memory_sort), but acting through SpecimenTable.sort_by
            because this table is never materialised whole; see its docstring
            for why that costs one narrow-column fetch instead of nothing.
            """
            search = state.search
            column = input.spec_sort_click()
            if search is None or not column:
                return
            table = search.table
            descending = False if column != table.sort_column else not table.sort_descending
            table.sort_by(column, descending=descending)
            offset.set(0)
            touch()

        @reactive.effect
        @reactive.event(input.reset_sort)
        def _reset_sort():
            if state.search is not None:
                state.search.table.sort_by(None)
                offset.set(0)
                touch()

        @reactive.effect
        def _page_size():
            search = state.search
            size = int(input.page_size() or DEFAULT_PAGE_SIZE)
            if search is None:
                return
            with reactive.isolate():
                search.table.page_size = size
                offset.set(0)
                touch()

        def _step(pages: int) -> None:
            search = state.search
            if search is None:
                return
            table = search.table
            offset.set(max(0, min(offset.get() + pages * table.page_size,
                                  (table.page_count - 1) * table.page_size)))
            touch()

        @reactive.effect
        @reactive.event(input.prev)
        def _prev_page():
            _step(-1)

        @reactive.effect
        @reactive.event(input.next_)
        def _next_page():
            _step(1)

        @reactive.effect
        @reactive.event(input.first)
        def _first_page():
            offset.set(0)
            touch()

        @reactive.effect
        @reactive.event(input.last)
        def _last_page():
            search = state.search
            if search is not None:
                offset.set((search.table.page_count - 1) * search.table.page_size)
                touch()

        @reactive.effect
        @reactive.event(input.select_page)
        def _check_page():
            """"Check page": the working selection, not the representative
            pick -- see io.annotations's module docstring.
            """
            if state.search is not None:
                state.search.table.check_page(offset.get())
                touch()

        @reactive.effect
        @reactive.event(input.select_all)
        def _check_all():
            if state.search is not None:
                n = state.search.table.check_all()
                status.set(f"Checked all {n:,} records")
                touch()

        @reactive.effect
        @reactive.event(input.clear_selection)
        def _clear_checked():
            state.annotations.clear_working()
            touch()

        @reactive.effect
        @reactive.event(input.row_check)
        def _row_check():
            """One record's own "checked" box, on the specimen table or any

            group. This is the disposable bulk-edit selection -- "Apply to
            checked" acts on it, and clearing it never touches the
            representative pick. The bulk buttons (check page / group / all)
            still exist; this is what lets a curator check a handful out of a
            group without pulling in everything else in it.
            """
            payload = input.row_check()
            pid = str((payload or {}).get("pid") or "")
            if not pid or state.search is None:
                return
            state.annotations.set_working(pid, selected=bool(payload.get("checked")))
            touch()

        @reactive.effect
        @reactive.event(input.row_rep)
        def _row_rep():
            """One record's own "representative" box.

            This is the persistent pick -- auto-filled on a fresh search,
            overridable here one record at a time. Nothing bulk ever touches
            it; see io.annotations's module docstring.
            """
            payload = input.row_rep()
            pid = str((payload or {}).get("pid") or "")
            if not pid or state.search is None:
                return
            if payload.get("checked"):
                state.annotations.set_selected(pid, user=(input.user() or "").strip())
            else:
                state.annotations.unset_selected(pid)
            touch()

        @reactive.effect
        @reactive.event(input.sp_apply)
        def _apply_specimens():
            _apply("sp")

        # -- downloads -------------------------------------------------------
        #
        # The six specimen-handling buttons above, plus the search-results CSV
        # (Search) and the BIN-analysis workbook (BINs) below. All eight
        # write through `io.exports`, the same code the CLI and the parity
        # harness already exercise, so a download and `boldcurator export`
        # agree by construction rather than by two implementations staying in
        # sync.

        def _empty_download(reason: str):
            """What a click gets when there is nothing to export.

            A silently empty or missing file is worse than one line saying why
            -- "no specimens selected" is not an error, it is the answer.
            """
            yield f"Nothing to export: {reason}.\n"

        #: Filenames the way ``io.exports.export_all`` already names the same
        #: files, so a download and a CLI/GUI export of the same kind agree.
        _TSV_FILENAME_STEM = {
            "all": "all_specimens", "selected": "selected_specimens",
            "annotated": "annotated_specimens",
            "curation_report": "bold_curation_report",
        }

        def _register_tsv_download(kind: str, output_id: str):
            @output(id=output_id)
            @render.download_button(
                filename=lambda: (f"{_TSV_FILENAME_STEM[kind]}_"
                                  f"{export_io.timestamp()}.tsv"))
            def _handler():
                search = state.search
                if search is None:
                    yield from _empty_download("run a search first")
                    return
                tmpdir = tempfile.TemporaryDirectory()
                path = Path(tmpdir.name) / "export.tsv"
                written = search.export_specimens(store, kind, path)
                if written is None:
                    tmpdir.cleanup()
                    yield from _empty_download(_EMPTY_REASONS[kind])
                    return
                yield from _stream_file(written, tmpdir)

        for _kind, _output_id in (
            ("all", "dl_all"), ("selected", "dl_selected"),
            ("annotated", "dl_annotated"),
            ("curation_report", "dl_curation_report"),
        ):
            _register_tsv_download(_kind, _output_id)

        def _register_fasta_download(output_id: str, *, selected_only: bool):
            @output(id=output_id)
            @render.download_button(
                filename=lambda: (
                    f"{'selected_' if selected_only else ''}"
                    f"sequences_{export_io.timestamp()}.fasta"))
            def _handler():
                search = state.search
                if search is None:
                    yield from _empty_download("run a search first")
                    return
                if not store.has_sequences:
                    yield from _empty_download(
                        "this snapshot was built without sequences")
                    return
                tmpdir = tempfile.TemporaryDirectory()
                path = Path(tmpdir.name) / "export.fasta"
                result = search.export_fasta(store, path, selected_only=selected_only)
                if result is None:
                    tmpdir.cleanup()
                    reason = ("no specimens selected" if selected_only
                              else "no sequences for these records")
                    yield from _empty_download(reason)
                    return
                written, _n = result
                yield from _stream_file(written, tmpdir)

        _register_fasta_download("dl_fasta", selected_only=False)
        _register_fasta_download("dl_selected_fasta", selected_only=True)

        @output(id="dl_search_results")
        @render.download_button(
            filename=lambda: f"bold_search_results_{export_io.timestamp()}.csv")
        def _dl_search_results():
            search = state.search
            if search is None:
                yield from _empty_download("run a search first")
                return
            tmpdir = tempfile.TemporaryDirectory()
            path = Path(tmpdir.name) / "export.csv"
            written = search.export_search_results(store, path)
            if written is None:
                tmpdir.cleanup()
                yield from _empty_download("no records in this result")
                return
            yield from _stream_file(written, tmpdir)

        @output(id="dl_bin_analysis")
        @render.download_button(
            filename=lambda: f"bin_analysis_{export_io.timestamp()}.xlsx",
            media_type="application/vnd.openxmlformats-officedocument"
                       ".spreadsheetml.sheet")
        def _dl_bin_analysis():
            search = state.search
            if search is None:
                yield from _empty_download("run a search first")
                return
            tmpdir = tempfile.TemporaryDirectory()
            path = Path(tmpdir.name) / "export.xlsx"
            written = search.export_bin_analysis(store, path)
            if written is None:
                tmpdir.cleanup()
                yield from _empty_download("no BINs in this result")
                return
            yield from _stream_file(written, tmpdir)

        def _species_analysis_download():
            """The Summary/Species checklist/Gap analysis workbook -- shared by
            the Species tab's own download button and the Gap analysis tab's
            (round 5, item 11), which offer the same export under two output
            ids rather than one element duplicated in the DOM."""
            search = state.search
            if search is None:
                yield from _empty_download("run a search first")
                return
            tmpdir = tempfile.TemporaryDirectory()
            path = Path(tmpdir.name) / "export.xlsx"
            written = search.export_species_analysis(store, path)
            if written is None:
                tmpdir.cleanup()
                yield from _empty_download("no species in this result")
                return
            yield from _stream_file(written, tmpdir)

        def _register_species_analysis_download(output_id: str):
            @output(id=output_id)
            @render.download_button(
                filename=lambda: f"species_analysis_{export_io.timestamp()}.xlsx",
                media_type="application/vnd.openxmlformats-officedocument"
                           ".spreadsheetml.sheet")
            def _handler():
                yield from _species_analysis_download()

        _register_species_analysis_download("dl_species_analysis")
        _register_species_analysis_download("dl_gap_analysis")

    return App(app_ui, server)


# --------------------------------------------------------------------------
# Static table rendering
#
# A DataGrid per group would mean one output id per group, and a grade can hold
# hundreds. These render to HTML instead, which costs nothing per group and
# lets a cell carry the colour that makes a problem visible.
# --------------------------------------------------------------------------


def _is_missing(value: object) -> bool:
    """Missing, for any of the several things pandas means by it.

    ``value != value`` catches float NaN but raises on ``pd.NA``, whose
    comparisons return ``pd.NA`` and whose truthiness is an error --
    "boolean value of NA is ambiguous". ``process_specimen_data`` blanks
    ``bin_uri`` and ``country.ocean`` to ``pd.NA``, so every specimen table
    with a BIN-less record went straight through that.
    """
    if value is None:
        return True
    try:
        return bool(pd.isna(value))
    except (TypeError, ValueError):     # arrays, lists: not missing
        return False


def _escape(value: object) -> str:
    text = "" if _is_missing(value) else str(value)
    return (text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))


def _escape_attr(value: object) -> str:
    """``_escape``, plus the one character that matters inside a
    single-quoted HTML attribute but not in text content: a free-text value
    (a curator note, a collector's name) landing in a ``title='...'``
    (round 6, all tables item 1) can easily contain an apostrophe, which
    would otherwise close the attribute early."""
    return _escape(value).replace("'", "&#39;")


#: The class a clickable column header carries, so the one delegated
#: ``document``-level listener (see the script in ``create_app``) catches a
#: click on it regardless of how many times the table has re-rendered --
#: exactly the reason the row checkboxes are delegated the same way.
SORT_HEADER_CLASS = "bc-sort-th"

#: The class a table's scrolling wrapper carries, so the page-load script can
#: find it again after a re-render and restore the scroll position that
#: re-render would otherwise have thrown away (see the ``shiny:value``
#: listener in ``create_app``).
SCROLL_CLASS = "bc-scroll"


#: Pixel width assigned to each frozen column, so its offset from the left
#: edge (and every later frozen column's) can be computed without a browser
#: -- the annotation columns a curator's eye and mouse live in while everything
#: else scrolls out from under them.
STICKY_COLUMN_WIDTHS: dict[str, int] = {
    "selected": 44, "checked": 44, "flag": 90, "updated_id": 140,
    "curator_notes": 160,
}


def _sticky_offsets(columns, sticky: frozenset[str]) -> dict[str, tuple[int, int]]:
    """``column -> (left_offset_px, width_px)`` for the columns to freeze."""
    offsets: dict[str, tuple[int, int]] = {}
    running = 0
    for column in columns:
        if column in sticky:
            width = STICKY_COLUMN_WIDTHS.get(column, 120)
            offsets[column] = (running, width)
            running += width
    return offsets


def _sticky_style(offset: int, width: int) -> str:
    # z-index 1 for a frozen body cell, above a plain cell scrolling
    # underneath it -- otherwise a wide column's text bleeds through the
    # "frozen" pane while scrolling.
    return (f"position:sticky;left:{offset}px;z-index:1;background:#fff;"
            f"width:{width}px;min-width:{width}px;max-width:{width}px;")


def _header_style(sticky: tuple[int, int] | None) -> str:
    # position:sticky goes on every header cell individually, not on <thead>:
    # <thead> has display:table-header-group, and sticky positioning on that
    # (or on <tr>) is unreliable across browsers -- a <th> is a table cell,
    # exactly what sticky is specified to work on. z-index 3 keeps a frozen
    # (top AND left) corner cell above a plain header cell (z-index 2), which
    # is itself above a frozen body cell (z-index 1) scrolling underneath both.
    if sticky:
        offset, width = sticky
        return (f"position:sticky;top:0;left:{offset}px;z-index:3;background:#fff;"
                f"width:{width}px;min-width:{width}px;max-width:{width}px;")
    return "position:sticky;top:0;z-index:2;background:#fff;"


def _header_cell(column: str, labels: dict[str, str], *, sort_input: str | None,
                 sortable: frozenset[str], sort_state: tuple[str | None, bool],
                 sticky: tuple[int, int] | None) -> str:
    label = _escape(labels.get(column, column))
    can_sort = bool(sort_input) and column in sortable
    style = _header_style(sticky)
    if can_sort:
        style += "cursor:pointer;user-select:none;"
    attrs = f" style='{style}'"
    if can_sort:
        arrow = " ▼" if sort_state[0] == column and sort_state[1] else \
                (" ▲" if sort_state[0] == column else "")
        attrs = (f" class='{SORT_HEADER_CLASS}' data-sort-input='{sort_input}' "
                f"data-sort-col='{_escape(column)}'{attrs} title='Click to sort'")
        label += arrow
    return f"<th{attrs}>{label}</th>"


def _table(frame: pd.DataFrame, labels: dict[str, str] | None = None,
           cell=None, limit: int = 500, *, sort_input: str | None = None,
           sortable: frozenset[str] = frozenset(),
           sort_state: tuple[str | None, bool] = (None, False),
           sticky: frozenset[str] = frozenset()) -> str:
    """Render one table.

    ``sort_input`` names the Shiny input a header click posts its column name
    to (``None`` renders plain, unclickable headers); ``sortable`` is which
    columns accept a click; ``sort_state`` is ``(column, descending)``, so the
    current sort carries its own arrow.

    ``sticky`` freezes those columns (in whatever order they appear) to the
    left edge while the rest of a wide table scrolls underneath -- see
    ``STICKY_COLUMN_WIDTHS``. Every cell renderer in this module happens to
    return a bare ``<td>...`` with no attributes of its own, which is what
    lets this inject the sticky style by string surgery instead of threading
    it through every ``cell()`` callback.
    """
    if frame is None or len(frame) == 0:
        return "<p class='text-muted'>Nothing to show.</p>"
    labels = labels or {}
    shown = frame.head(limit)
    offsets = _sticky_offsets(shown.columns, sticky)
    head = "".join(_header_cell(c, labels, sort_input=sort_input, sortable=sortable,
                                sort_state=sort_state, sticky=offsets.get(c))
                   for c in shown.columns)
    body = []
    for _, row in shown.iterrows():
        cells = []
        for column in shown.columns:
            # ``row`` too, not just the cell's own value -- a checkbox needs
            # the record's processid, which lives in a different column.
            rendered = cell(column, row[column], row) if cell else None
            if rendered is None:
                text = _escape(row[column])
                # Round 6, all tables item 1: a title attribute is the
                # hover-to-read-the-rest for a value the new max-width CSS
                # now truncates -- only worth adding when there is
                # something to truncate.
                rendered = (f"<td title='{_escape_attr(row[column])}'>{text}</td>"
                           if text else f"<td>{text}</td>")
            if column in offsets and rendered.startswith("<td"):
                # Not just the bare "<td>" case any more -- the default
                # renderer above can now also emit "<td title='...'>" (round
                # 6, all tables item 1), so this inserts the sticky style
                # right after "<td" generically rather than assuming nothing
                # else is already there.
                style = _sticky_style(*offsets[column])
                rendered = f"<td style='{style}'" + rendered[len('<td'):]
            cells.append(rendered)
        body.append("<tr>" + "".join(cells) + "</tr>")
    more = ("" if len(frame) <= limit else
            f"<p class='text-muted small mb-0 mt-1'>Showing {limit:,} of "
            f"{len(frame):,} rows.</p>")
    # A single top-level element, not two siblings (the table div and a
    # trailing <p>) -- round 5, items 7/10: the page-level CSS makes *this*
    # element (".bc-scroll") the one that flexes to fill whatever space its
    # container has and scrolls internally (see the ".bc-tab-body" rules in
    # create_app's stylesheet), which only works if it is truly the last DOM
    # child of that container. The "Showing N of M" note lives inside it, not
    # after it, so it is part of the scrolling content instead of extra
    # height tacked on past the fill area.
    return (
        f"<div class='{SCROLL_CLASS}' style='overflow:auto;height:100%;'>"
        "<table class='table table-sm table-hover' style='font-size:13px;"
        "border-collapse:separate;'>"
        f"<thead><tr>{head}</tr></thead>"
        f"<tbody>{''.join(body)}</tbody></table>{more}</div>"
    )


def _chip(value: str, colour: str) -> str:
    return (f"<td><span style='background:{colour};color:#fff;padding:1px 9px;"
            f"border-radius:10px;font-weight:600;'>{_escape(value)}</span></td>")


def _link_cell(value: object, url: str) -> str:
    """A cell whose value opens the matching BOLD portal page in a new tab.

    ``rel="noopener noreferrer"`` because a ``target="_blank"`` link the page
    itself built (not a user-typed URL) should still not hand the opened tab
    a live ``window.opener`` back into the app.
    """
    return (f"<td><a href='{_escape(url)}' target='_blank' "
            f"rel='noopener noreferrer'>{_escape(value)}</a></td>")


def _checklist_html(frame: pd.DataFrame, *,
                    sort_state: tuple[str | None, bool] = (None, False)) -> str:
    def cell(column, value, row):
        if column == "bags_grade" and not _is_missing(value) and value:
            return _chip(value, GRADE_COLOURS.get(str(value), "#adb5bd"))
        if column == "species" and not _is_missing(value) and value:
            return _link_cell(value, bold_species_url(str(value)))
        return None
    return _table(frame, CHECKLIST_LABELS, cell, sort_input="checklist_sort_click",
                 sortable=frozenset(frame.columns) if len(frame) else frozenset(),
                 sort_state=sort_state)


def _bins_html(frame: pd.DataFrame, *,
              sort_state: tuple[str | None, bool] = (None, False)) -> str:
    def cell(column, value, row):
        if column == "concordance" and not _is_missing(value) and value:
            return _chip(value, CONCORDANCE_COLOURS.get(str(value), "#adb5bd"))
        if column == "bin_uri" and not _is_missing(value) and value:
            return _link_cell(value, bold_bin_url(str(value)))
        return None
    return _table(frame, BIN_LABELS, cell, sort_input="bins_sort_click",
                 sortable=frozenset(frame.columns) if len(frame) else frozenset(),
                 sort_state=sort_state)


def _gap_html(frame: pd.DataFrame, *,
             sort_state: tuple[str | None, bool] = (None, False)) -> str:
    def cell(column, value, row):
        if column == "status" and not _is_missing(value) and value:
            return _chip(value, GAP_STATUS_COLOURS.get(str(value), "#adb5bd"))
        if column == "matched_species" and not _is_missing(value) and value:
            return _link_cell(value, bold_species_url(str(value)))
        return None
    return _table(frame, GAP_LABELS, cell, sort_input="gap_sort_click",
                 sortable=frozenset(frame.columns) if len(frame) else frozenset(),
                 sort_state=sort_state)


#: The classes the two per-row checkboxes carry, so one delegated listener
#: (attached once, to ``document`` -- see the script in ``create_app``)
#: catches every row's click regardless of how many times the table around it
#: has re-rendered. **They are not the same checkbox.**
#:
#: ``ROW_REP_CLASS`` toggles ``Annotations.selected`` -- the *representative*
#: pick (auto-filled, best per BIN x country, what "Download Selected"
#: exports). It persists for the life of the result.
#:
#: ``ROW_CHECK_CLASS`` toggles ``Annotations.working`` -- a disposable,
#: session-scratch selection that exists only to gather targets for the
#: "Apply to checked" toolbar. The bulk buttons (check page / group / all,
#: Clear) act on this one, never on the representative pick -- see
#: ``io.annotations``'s module docstring for why conflating the two was a bug.
ROW_REP_CLASS = "bc-row-rep"
ROW_CHECK_CLASS = "bc-row-check"


def _checkbox_cell(pid: object, checked: bool, css_class: str, *,
                   title: str = "") -> str:
    pid = _escape(pid)
    mark = "checked" if checked else ""
    attr = f" title='{_escape(title)}'" if title else ""
    return (f"<td><input type='checkbox' class='{css_class}'{attr} "
            f"data-pid='{pid}' {mark}></td>")


def _group_html(frame: pd.DataFrame, columns: list[str] | None = None,
                limit: int = 500, *, sort_input: str | None = None,
                sortable: frozenset[str] | None = None,
                sort_state: tuple[str | None, bool] = (None, False)) -> str:
    """One group's specimens, with the grade and flag colours carried through.

    ``columns`` defaults to the BAGS group layout. It is a parameter because
    the specimen table shows a different set, and applying the group layout to
    an already-narrowed frame silently dropped the columns the caller picked.

    **Two real checkboxes per record: "Rep." and "Check".** "Rep." is the
    representative pick; "Check" is the disposable bulk-edit selection. They
    used to be the same checkbox, which is why clearing a bulk selection could
    silently wipe out a curator's (or auto-selection's) representative picks.
    ``frame`` must already carry both ``selected`` (representative,
    ``merge_annotations``) and ``checked`` (working, added by the caller from
    ``Annotations.working`` -- it has no curatorial meaning to persist, so it
    is not one of ``merge_annotations``'s six columns).

    ``sortable`` defaults to every shown column, including the two checkbox
    columns (True/False sorts perfectly well) -- fine for a group table,
    which is already fully in memory. The specimen table passes its own set
    (``SpecimenTable.sortable_columns``): sorting a *stored* column there means
    fetching it for the whole result, so a genuinely computed one
    (``quality_score``, ``rank``, ``bags_grade``...) is refused, not silently
    sorted by something else -- annotation columns (``selected``, ``flag``...)
    are cheap there too and are included the same way.
    """
    shown = present(frame, columns or GROUP_COLUMNS)
    has_pid = "processid" in shown.columns
    if sortable is None:
        sortable = frozenset(shown.columns)

    def cell(column, value, row):
        if column == "selected":
            chosen = not _is_missing(value) and bool(value)
            if has_pid:
                return _checkbox_cell(row["processid"], chosen, ROW_REP_CLASS,
                                      title="Representative specimen")
            return f"<td>{'✔' if chosen else ''}</td>"
        if column == "checked":
            chosen = not _is_missing(value) and bool(value)
            if has_pid:
                return _checkbox_cell(row["processid"], chosen, ROW_CHECK_CLASS,
                                      title="Checked for bulk flag/note/update")
            return f"<td>{'✔' if chosen else ''}</td>"
        if column == "bags_grade" and not _is_missing(value) and value:
            return _chip(value, GRADE_COLOURS.get(str(value), "#adb5bd"))
        if column == "flag" and not _is_missing(value) and value:
            return _chip(value, "#6f42c1")
        if column == "processid" and not _is_missing(value) and value:
            return _link_cell(value, bold_record_url(str(value)))
        if column == "bin_uri" and not _is_missing(value) and value:
            return _link_cell(value, bold_bin_url(str(value)))
        if column == "species" and not _is_missing(value) and value:
            return _link_cell(value, bold_species_url(str(value)))
        return None
    return _table(shown, GROUP_LABELS, cell=cell, limit=limit, sort_input=sort_input,
                 sortable=sortable, sort_state=sort_state,
                 sticky=frozenset(STICKY_COLUMN_WIDTHS))


def run(snapshot: str | Path, *, host: str = "127.0.0.1", port: int = 8000,
        page_size: int = DEFAULT_PAGE_SIZE,
        sessions_path: str | Path | None = None) -> None:
    import shiny

    shiny.run_app(
        create_app(snapshot, page_size=page_size, sessions_path=sessions_path),
        host=host, port=port)
