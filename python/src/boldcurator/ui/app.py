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

from pathlib import Path

import pandas as pd
from shiny import App, reactive, render, ui

from ..config.constants import FLAG_OPTIONS
from ..core.grouping import GRADE_DESCRIPTIONS, GRADES, PRIORITY_GRADES
from ..core.table import DEFAULT_PAGE_SIZE
from ..data.snapshot import SnapshotStore
from ..io.annotations import merge_annotations
from .format import (
    BIN_LABELS,
    CHECKLIST_LABELS,
    CONCORDANCE_COLOURS,
    GRADE_COLOURS,
    GROUP_COLUMNS,
    present,
    value_box,
)
from .state import AppState, ResultTooLargeToAnalyse

PAGE_SIZES = [25, 50, 100, 250, 500]

#: Shown on the specimen table. The full 71 columns are available; a grid with
#: 71 columns is unreadable, and the R app shows a chosen subset too.
#: ``bags_grade`` is absent until a summary screen has been opened -- see
#: ``SearchState.grade_lookup``.
PREVIEW_COLUMNS = [
    "selected", "flag", "curator_notes", "updated_id",
    "processid", "species", "bin_uri", "country.ocean",
    "quality_score", "rank", "bags_grade", "inst", "identified_by",
]


def _annotation_controls(prefix: str) -> list:
    """Flag / note / corrected-ID, plus Apply. Repeated per screen, so shared.

    Explicit widths, because these sit in a flex row: a Shiny input is a block
    element and will otherwise take the full width and stack, turning a
    one-line toolbar into half a screen of form.
    """
    return [
        ui.div(ui.input_select(f"{prefix}_flag", "Flag",
                               choices=sorted(FLAG_OPTIONS), width="180px"),
               class_="mb-0"),
        ui.div(ui.input_text(f"{prefix}_note", "Curator note", width="260px"),
               class_="mb-0"),
        ui.div(ui.input_text(f"{prefix}_updated_id", "Corrected identification",
                             width="220px"), class_="mb-0"),
        ui.input_action_button(f"{prefix}_apply", "Apply to selection",
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
        ui.output_ui(f"grade_{grade}_body"),
        value=f"grade_{grade}",
    )


def create_app(snapshot: str | Path, *, page_size: int = DEFAULT_PAGE_SIZE) -> App:
    store = SnapshotStore(snapshot)
    info = store.info()

    app_ui = ui.page_fluid(
        ui.div(
            ui.tags.h4("BOLDcurator", style="margin:0;"),
            ui.tags.span(
                f"{info.snapshot_id} · {info.row_count:,} records · "
                f"{info.bin_count:,} BINs · offline, no BOLD API",
                class_="text-muted small",
            ),
            ui.div(
                ui.input_text("user", None, placeholder="Your name (for annotations)",
                              width="240px"),
                style="margin-left:auto;",
            ),
            style="display:flex;align-items:center;gap:16px;padding:8px 0 10px;"
                  "border-bottom:1px solid #dee2e6;margin-bottom:12px;",
        ),
        ui.output_ui("banner"),
        ui.navset_pill_list(
            ui.nav_panel(
                "Data Input",
                ui.input_text_area("taxa", "Taxa (one per line)",
                                   value="Nymphalidae", rows=4, width="520px"),
                ui.input_action_button("search", "Search", class_="btn-primary"),
                ui.output_ui("search_summary"),
                value="input",
            ),
            ui.nav_panel("Species", ui.output_ui("species_body"), value="species"),
            ui.nav_panel("BINs", ui.output_ui("bins_body"), value="bins"),
            *[_grade_panel(g) for g in GRADES],
            ui.nav_panel("Specimens", ui.output_ui("specimens_body"),
                         value="specimens"),
            id="nav",
            widths=(2, 10),
        ),
    )

    def server(input, output, session):
        state = AppState(store, page_size=page_size)
        revision = reactive.Value(0)
        status = reactive.Value("")
        offset = reactive.Value(0)
        group_index: dict[str, reactive.Value] = {
            g: reactive.Value(0) for g in GRADES
        }

        def touch() -> None:
            revision.set(revision.get() + 1)

        def current():
            revision.get()
            return state.search

        # -- search --------------------------------------------------------

        @reactive.effect
        @reactive.event(input.search)
        def _search():
            state.user = (input.user() or "").strip()
            status.set(state.run_search(input.taxa()))
            offset.set(0)
            for value in group_index.values():
                value.set(0)
            if state.search is not None:
                ui.update_select("sort",
                                 choices=["(result order)"]
                                 + state.search.table.sortable_columns,
                                 selected="(result order)")
                ui.update_navs("nav", selected="species")
            touch()

        @output
        @render.ui
        def banner():
            search = current()
            if search is None or not search.warnings:
                return ui.div()
            return ui.div(
                *[ui.div(w) for w in search.warnings],
                class_="alert alert-warning py-2 px-3 small mb-3",
            )

        @output
        @render.ui
        def search_summary():
            search = current()
            text = status.get()
            if search is None:
                return ui.div(text, class_="small text-muted pt-3")
            return ui.div(
                ui.div(text, class_="small text-muted pt-3"),
                ui.div(f"Searched: {search.query_label}", class_="small"),
                ui.div(f"{len(state.annotations.selected):,} records selected, "
                       f"{len(state.annotations.annotated_processids()):,} annotated",
                       class_="small text-muted"),
            )

        # -- the guard every summary screen shares -------------------------

        def _needs_analysis(render_body):
            search = current()
            if search is None:
                return ui.div("Run a search first.", class_="text-muted")
            try:
                return render_body(search)
            except ResultTooLargeToAnalyse as exc:
                return ui.div(str(exc), class_="alert alert-warning py-2 px-3")

        # -- species checklist ---------------------------------------------

        @output
        @render.ui
        def species_body():
            def body(search):
                checklist = search.checklist(store)
                counts = search.grade_counts(store)
                return ui.div(
                    ui.div(
                        *[value_box(f"{counts.get(g, 0):,}", f"Grade {g}",
                                    GRADE_COLOURS[g]) for g in GRADES],
                        style="display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
                    ),
                    ui.HTML(_checklist_html(checklist)),
                )
            return _needs_analysis(body)

        # -- BIN dashboard --------------------------------------------------

        @output
        @render.ui
        def bins_body():
            def body(search):
                analysis = search.analysis(store).bin_analysis
                summary, content = analysis["summary"], analysis["content"]
                return ui.div(
                    ui.div(
                        value_box(f"{summary['total_bins']:,}", "Total BINs",
                                  "#2c7fb8"),
                        value_box(f"{summary['concordant_bins']:,}",
                                  "Concordant BINs", "#28a745"),
                        value_box(f"{summary['discordant_bins']:,}",
                                  "Discordant BINs", "#dc3545"),
                        value_box(f"{summary['shared_bins']:,}",
                                  "BINs with >1 species", "#f0ad4e"),
                        style="display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
                    ),
                    ui.HTML(_bins_html(content)),
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
                rows = merge_annotations(group.specimens, state.annotations)
                return ui.row(
                    ui.column(4, ui.div(
                        ui.div(f"{len(groups):,} "
                               f"{'problem' if len(groups) == 1 else 'problems'} "
                               "to work through", class_="small text-muted mb-1"),
                        ui.input_select(
                            f"group_{grade}", None,
                            choices={str(i): f"{g.caption}  ({g.specimen_count})"
                                     for i, g in enumerate(groups)},
                            selected=str(index), size=min(20, max(6, len(groups))),
                            width="100%"),
                    )),
                    ui.column(8, ui.div(
                        ui.div(
                            ui.input_action_button(f"prev_{grade}", "‹ Previous",
                                                   class_="btn-sm"),
                            ui.tags.span(f"  {index + 1} of {len(groups):,}  ",
                                         class_="small text-muted"),
                            ui.input_action_button(f"next_{grade}", "Next ›",
                                                   class_="btn-sm"),
                            style="display:flex;align-items:center;gap:8px;"
                                  "margin-bottom:6px;",
                        ),
                        ui.div(ui.tags.strong(group.caption),
                               ui.tags.span(f"  ·  {group.specimen_count} specimens",
                                            class_="text-muted small"),
                               class_="mb-1"),
                        ui.div(group.note, class_="alert alert-info py-1 px-2 small")
                        if group.note else ui.div(),
                        ui.div(
                            ui.div(
                                ui.input_action_button(
                                    f"selall_{grade}", "Select this group",
                                    class_="btn-sm"),
                                ui.input_action_button(f"clear_{grade}", "Clear",
                                                       class_="btn-sm"),
                                ui.div(f"{len(state.annotations.selected):,} "
                                       "selected",
                                       class_="small text-muted pt-1"),
                                style="display:flex;flex-direction:column;gap:4px;",
                            ),
                            *_annotation_controls(f"g{grade}"),
                            style="display:flex;align-items:end;gap:10px;"
                                  "flex-wrap:wrap;margin-bottom:10px;padding:8px;"
                                  "background:#f8f9fa;border:1px solid #dee2e6;"
                                  "border-radius:5px;",
                        ),
                        ui.HTML(_group_html(rows)),
                    )),
                )
            return _needs_analysis(body)

        def _register_grade(grade: str):
            @output(id=f"grade_{grade}_body")
            @render.ui
            def _body(grade=grade):
                return _grade_body(grade)

            @reactive.effect
            @reactive.event(input[f"prev_{grade}"])
            def _prev(grade=grade):
                group_index[grade].set(max(0, group_index[grade].get() - 1))
                ui.update_select(f"group_{grade}",
                                 selected=str(group_index[grade].get()))
                touch()

            @reactive.effect
            @reactive.event(input[f"next_{grade}"])
            def _next(grade=grade):
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
            def _pick(grade=grade):
                chosen = input[f"group_{grade}"]()
                if chosen is not None and str(chosen).isdigit():
                    group_index[grade].set(int(chosen))
                    touch()

            @reactive.effect
            @reactive.event(input[f"selall_{grade}"])
            def _select_group(grade=grade):
                group = _current_group(grade)
                if group is not None:
                    # Replaces the selection rather than adding to it. "Apply
                    # to selection" acts on whatever is selected, so if this
                    # accumulated, annotating the second problem would silently
                    # re-annotate the first -- which is the opposite of working
                    # through one problem at a time.
                    state.annotations.selected.clear()
                    state.annotations_select(group)
                    touch()

            @reactive.effect
            @reactive.event(input[f"clear_{grade}"])
            def _clear_group(grade=grade):
                group = _current_group(grade)
                if group is not None:
                    for pid in group.specimens["processid"]:
                        state.annotations.unset_selected(str(pid))
                    touch()

            @reactive.effect
            @reactive.event(input[f"g{grade}_apply"])
            def _apply_group(grade=grade):
                _apply(f"g{grade}")

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

        def _apply(prefix: str) -> None:
            selected = sorted(state.annotations.selected)
            if not selected:
                status.set("Nothing selected.")
                touch()
                return
            user = (input.user() or "").strip()
            flag = input[f"{prefix}_flag"]() or ""
            note = (input[f"{prefix}_note"]() or "").strip()
            updated = (input[f"{prefix}_updated_id"]() or "").strip()
            for pid in selected:
                state.annotations.set_flag(pid, flag, user=user)
                if note:
                    state.annotations.set_note(pid, note, user=user)
                if updated:
                    state.annotations.set_updated_id(pid, updated, user=user)
            status.set(f"Annotated {len(selected):,} records")
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
            rows = page.rows
            lookup = search.grade_lookup()
            if lookup:
                rows = rows.copy()
                rows["bags_grade"] = [
                    lookup.get(s, "") if isinstance(s, str) else ""
                    for s in rows["species"].astype(object)
                ]

            return ui.div(
                ui.div(
                    ui.input_select("sort", None,
                                    choices=["(result order)"]
                                    + table.sortable_columns,
                                    selected="(result order)", width="200px"),
                    ui.input_checkbox("descending", "Desc"),
                    ui.input_select("page_size", None,
                                    choices=[str(n) for n in PAGE_SIZES],
                                    selected=str(table.page_size), width="90px"),
                    ui.input_action_button("first", "«", class_="btn-sm"),
                    ui.input_action_button("prev", "‹", class_="btn-sm"),
                    ui.tags.span(f" {page.page_number:,} / {page.page_count:,} ",
                                 class_="small text-nowrap"),
                    ui.input_action_button("next_", "›", class_="btn-sm"),
                    ui.input_action_button("last", "»", class_="btn-sm"),
                    ui.input_action_button("select_page", "Select page",
                                           class_="btn-sm"),
                    ui.input_action_button("select_all", "Select all",
                                           class_="btn-sm"),
                    ui.input_action_button("clear_selection", "Clear",
                                           class_="btn-sm"),
                    *_annotation_controls("sp"),
                    style="display:flex;align-items:end;gap:10px;flex-wrap:wrap;"
                          "margin-bottom:10px;padding:8px;background:#f8f9fa;"
                          "border:1px solid #dee2e6;border-radius:5px;",
                ),
                ui.HTML(_group_html(rows, columns=PREVIEW_COLUMNS,
                                    limit=len(rows))),
            )

        @reactive.effect
        def _sort():
            search = state.search
            column, descending = input.sort(), bool(input.descending())
            if search is None or column is None:
                return
            with reactive.isolate():
                search.table.sort_by(
                    None if column == "(result order)" else column,
                    descending=descending)
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
        def _select_page():
            if state.search is not None:
                state.search.table.select_page(offset.get())
                touch()

        @reactive.effect
        @reactive.event(input.select_all)
        def _select_all():
            if state.search is not None:
                n = state.search.table.select_all()
                status.set(f"Selected all {n:,} records")
                touch()

        @reactive.effect
        @reactive.event(input.clear_selection)
        def _clear_selection():
            state.annotations.selected.clear()
            touch()

        @reactive.effect
        @reactive.event(input.sp_apply)
        def _apply_specimens():
            _apply("sp")

        # AppState does not know about groups; give it the one helper it needs
        # rather than letting the UI reach into Annotations row by row.
        def _annotations_select(group) -> None:
            for pid in group.specimens["processid"]:
                state.annotations.set_selected(str(pid),
                                               user=(input.user() or "").strip())
        state.annotations_select = _annotations_select  # type: ignore[attr-defined]

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


def _table(frame: pd.DataFrame, labels: dict[str, str] | None = None,
           cell=None, limit: int = 500) -> str:
    if frame is None or len(frame) == 0:
        return "<p class='text-muted'>Nothing to show.</p>"
    labels = labels or {}
    shown = frame.head(limit)
    head = "".join(f"<th>{_escape(labels.get(c, c))}</th>" for c in shown.columns)
    body = []
    for _, row in shown.iterrows():
        cells = []
        for column in shown.columns:
            rendered = cell(column, row[column]) if cell else None
            cells.append(rendered if rendered is not None
                         else f"<td>{_escape(row[column])}</td>")
        body.append("<tr>" + "".join(cells) + "</tr>")
    more = ("" if len(frame) <= limit else
            f"<p class='text-muted small'>Showing {limit:,} of {len(frame):,} rows.</p>")
    return (
        "<div style='max-height:62vh;overflow:auto;'>"
        "<table class='table table-sm table-hover' style='font-size:13px;'>"
        f"<thead style='position:sticky;top:0;background:#fff;'><tr>{head}</tr></thead>"
        f"<tbody>{''.join(body)}</tbody></table></div>{more}"
    )


def _chip(value: str, colour: str) -> str:
    return (f"<td><span style='background:{colour};color:#fff;padding:1px 9px;"
            f"border-radius:10px;font-weight:600;'>{_escape(value)}</span></td>")


def _checklist_html(frame: pd.DataFrame) -> str:
    def cell(column, value):
        if column == "bags_grade" and not _is_missing(value) and value:
            return _chip(value, GRADE_COLOURS.get(str(value), "#adb5bd"))
        return None
    return _table(frame, CHECKLIST_LABELS, cell)


def _bins_html(frame: pd.DataFrame) -> str:
    def cell(column, value):
        if column == "concordance" and not _is_missing(value) and value:
            return _chip(value, CONCORDANCE_COLOURS.get(str(value), "#adb5bd"))
        if column == "bin_coverage":
            return "<td></td>" if _is_missing(value) else f"<td>{float(value):.1%}</td>"
        return None
    return _table(frame, BIN_LABELS, cell)


def _group_html(frame: pd.DataFrame, columns: list[str] | None = None,
                limit: int = 500) -> str:
    """One group's specimens, with the grade and flag colours carried through.

    ``columns`` defaults to the BAGS group layout. It is a parameter because
    the specimen table shows a different set, and applying the group layout to
    an already-narrowed frame silently dropped the columns the caller picked.
    """
    shown = present(frame, columns or GROUP_COLUMNS)

    def cell(column, value):
        if column == "selected":
            chosen = not _is_missing(value) and bool(value)
            return f"<td>{'✔' if chosen else ''}</td>"
        if column == "bags_grade" and not _is_missing(value) and value:
            return _chip(value, GRADE_COLOURS.get(str(value), "#adb5bd"))
        if column == "flag" and not _is_missing(value) and value:
            return _chip(value, "#6f42c1")
        return None
    return _table(shown, cell=cell, limit=limit)


def run(snapshot: str | Path, *, host: str = "127.0.0.1", port: int = 8000,
        page_size: int = DEFAULT_PAGE_SIZE) -> None:
    import shiny

    shiny.run_app(create_app(snapshot, page_size=page_size), host=host, port=port)
