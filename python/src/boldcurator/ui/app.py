"""Phase 3.1 spike: the specimen table, paged, selectable, bulk-annotatable.

The question 3.1 exists to answer is whether Shiny for Python's ``DataGrid`` can
carry the specimen table. The plan framed it as "render 50,000 rows"; the
benchmark reframed it. A family search returns 89,479 records and an order
2,095,427, so no widget is ever handed the result -- ``core.table.SpecimenTable``
pages it server-side and the grid receives one page. The real questions are
therefore whether the grid does row selection and a bulk-annotation toolbar
well, and whether paging feels immediate.

Everything the screen does is a call into ``core.table``. That is deliberate:
if the answer is "no", ``ui/`` is replaced with NiceGUI + AG Grid and nothing
below it changes.
"""

from __future__ import annotations

from pathlib import Path

from shiny import App, reactive, render, ui

from ..config.constants import DOWNLOAD_LIMITS, FLAG_OPTIONS
from ..core.table import DEFAULT_PAGE_SIZE, SpecimenTable
from ..data.queries import SearchQuery, plan_search, resolve_taxa
from ..data.snapshot import SnapshotStore

#: Shown by default. The full 71 are available, but a grid with 71 columns is
#: unreadable and the R app's specimen table shows a chosen subset too.
PREVIEW_COLUMNS = [
    "selected", "flag", "curator_notes", "updated_id",
    "processid", "species", "bin_uri", "country.ocean",
    "quality_score", "rank", "inst", "identified_by",
]

PAGE_SIZES = [25, 50, 100, 250, 500]


def _panel(store: SnapshotStore) -> ui.Tag:
    info = store.info()
    return ui.div(
        ui.tags.strong(info.snapshot_id),
        ui.tags.span(
            f" · {info.row_count:,} records · {info.bin_count:,} BINs · "
            f"built {info.built_at} · offline, no BOLD API",
            class_="text-muted",
        ),
        class_="small border-bottom pb-2 mb-3",
    )


def create_app(snapshot: str | Path, *, page_size: int = DEFAULT_PAGE_SIZE) -> App:
    store = SnapshotStore(snapshot)

    app_ui = ui.page_fluid(
        ui.tags.h4("BOLDcurator — specimen table"),
        ui.output_ui("snapshot_bar"),
        ui.layout_sidebar(
            ui.sidebar(
                ui.input_text_area(
                    "taxa", "Taxa (one per line)", value="Nymphalidae",
                    rows=3, width="100%",
                ),
                ui.input_action_button("search", "Search", class_="btn-primary"),
                ui.output_ui("search_status"),
                ui.hr(),
                ui.tags.strong("Bulk annotation"),
                ui.output_ui("selection_status"),
                ui.input_select("flag", "Flag", choices=sorted(FLAG_OPTIONS)),
                ui.input_text("note", "Curator note", width="100%"),
                ui.input_text("updated_id", "Corrected identification", width="100%"),
                ui.input_action_button("apply", "Apply to selection"),
                ui.hr(),
                ui.input_action_button("select_page", "Select page"),
                ui.input_action_button("select_all", "Select all matching"),
                ui.input_action_button("clear", "Clear selection"),
                width=320,
            ),
            ui.div(
                ui.row(
                    ui.column(3, ui.input_select(
                        "sort", "Sort by", choices=["(result order)"], selected=None)),
                    ui.column(2, ui.input_checkbox("descending", "Descending")),
                    ui.column(2, ui.input_select(
                        "page_size", "Rows per page",
                        choices=[str(n) for n in PAGE_SIZES], selected=str(page_size))),
                    ui.column(5, ui.div(
                        ui.input_action_button("first", "«"),
                        ui.input_action_button("prev", "‹"),
                        ui.output_ui("pager"),
                        ui.input_action_button("next_", "›"),
                        ui.input_action_button("last", "»"),
                        class_="d-flex align-items-center gap-1 pt-4",
                    )),
                ),
                ui.output_data_frame("grid"),
            ),
        ),
    )

    def server(input, output, session):
        table_rv: reactive.Value = reactive.Value(None)
        offset_rv: reactive.Value = reactive.Value(0)
        status_rv: reactive.Value = reactive.Value("")
        # Bumped whenever an annotation changes, so the grid redraws. Shiny
        # cannot see a mutation inside SpecimenTable, and pretending otherwise
        # is how a bulk edit silently fails to appear.
        revision_rv: reactive.Value = reactive.Value(0)

        def touch() -> None:
            revision_rv.set(revision_rv.get() + 1)

        @output
        @render.ui
        def snapshot_bar():
            return _panel(store)

        @reactive.effect
        @reactive.event(input.search)
        def _run_search():
            names = [n.strip() for n in (input.taxa() or "").splitlines() if n.strip()]
            if not names:
                status_rv.set("Type a taxon name.")
                return
            resolution = resolve_taxa(store, names)
            if not resolution.resolved:
                status_rv.set(f"No records for: {', '.join(resolution.unmatched)}")
                table_rv.set(None)
                return

            plan = plan_search(store, SearchQuery(taxa=resolution.resolved,
                                                  expand_bins=True))
            # The pre-check that makes this safe: the row set is known before
            # a single wide row is read, so an oversized result is refused
            # here rather than discovered by the machine running out of memory.
            if plan.expanded_records > DOWNLOAD_LIMITS["MAX_RECORDS"]:
                status_rv.set(
                    f"{plan.expanded_records:,} records is over the "
                    f"{DOWNLOAD_LIMITS['MAX_RECORDS']:,} limit. Narrow the "
                    "search or add a geographic filter."
                )
                table_rv.set(None)
                return

            table = SpecimenTable(store, plan, page_size=int(input.page_size()))
            table_rv.set(table)
            offset_rv.set(0)
            ui.update_select("sort",
                             choices=["(result order)"] + table.sortable_columns,
                             selected="(result order)")
            status_rv.set(f"{plan.expanded_records:,} records "
                          f"({plan.seed_records:,} seed, {plan.seed_bins:,} BINs)")
            touch()

        # Every input this effect reacts to must be read OUTSIDE the isolate
        # block. Reading one inside takes no reactive dependency on it, so the
        # control renders, accepts clicks and does nothing -- which is exactly
        # how the descending toggle and the page-size select were both dead
        # until the spike was driven in a browser. Only the writes are
        # isolated, because touch() reads the counter it then sets.
        @reactive.effect
        def _apply_sort():
            table = table_rv.get()
            column = input.sort()
            descending = bool(input.descending())
            if table is None:
                return
            with reactive.isolate():
                table.sort_by(None if column in (None, "(result order)") else column,
                              descending=descending)
                offset_rv.set(0)
                touch()

        @reactive.effect
        def _apply_page_size():
            table = table_rv.get()
            page_size = int(input.page_size())
            if table is None:
                return
            with reactive.isolate():
                table.page_size = page_size
                offset_rv.set(0)
                touch()

        def _step(delta_pages: int):
            table = table_rv.get()
            if table is None:
                return
            offset_rv.set(max(0, min(
                offset_rv.get() + delta_pages * table.page_size,
                (table.page_count - 1) * table.page_size)))

        @reactive.effect
        @reactive.event(input.prev)
        def _prev():
            _step(-1)

        @reactive.effect
        @reactive.event(input.next_)
        def _next():
            _step(1)

        @reactive.effect
        @reactive.event(input.first)
        def _first():
            offset_rv.set(0)

        @reactive.effect
        @reactive.event(input.last)
        def _last():
            table = table_rv.get()
            if table is not None:
                offset_rv.set((table.page_count - 1) * table.page_size)

        @reactive.effect
        @reactive.event(input.select_page)
        def _select_page():
            table = table_rv.get()
            if table is not None:
                table.select_page(offset_rv.get())
                touch()

        @reactive.effect
        @reactive.event(input.select_all)
        def _select_all():
            table = table_rv.get()
            if table is not None:
                n = table.select_all()
                status_rv.set(f"Selected all {n:,} matching records")
                touch()

        @reactive.effect
        @reactive.event(input.clear)
        def _clear():
            table = table_rv.get()
            if table is not None:
                table.clear_selection()
                touch()

        @reactive.effect
        @reactive.event(input.apply)
        def _apply():
            table = table_rv.get()
            if table is None or not table.selected_count:
                status_rv.set("Nothing selected.")
                return
            n = table.selected_count
            table.apply_flag(input.flag() or "")
            if (input.note() or "").strip():
                table.apply_note(input.note())
            if (input.updated_id() or "").strip():
                table.apply_updated_id(input.updated_id())
            status_rv.set(f"Annotated {n:,} records")
            touch()

        @output
        @render.ui
        def search_status():
            return ui.div(status_rv.get(), class_="small text-muted pt-2")

        @output
        @render.ui
        def selection_status():
            table = table_rv.get()
            revision_rv.get()
            n = table.selected_count if table else 0
            return ui.div(f"{n:,} selected", class_="small pb-2")

        @output
        @render.ui
        def pager():
            table = table_rv.get()
            # revision_rv, because page_count changes when the page size does
            # and offset_rv does not move when it was already 0. Without this
            # the pager silently keeps reporting the old page count.
            revision_rv.get()
            if table is None:
                return ui.span()
            page = offset_rv.get() // table.page_size + 1
            return ui.span(f" {page:,} / {table.page_count:,} ",
                           class_="small text-nowrap")

        @output
        @render.data_frame
        def grid():
            table = table_rv.get()
            revision_rv.get()
            if table is None:
                return render.DataGrid(__import__("pandas").DataFrame())
            rows = table.page(offset_rv.get()).rows
            shown = [c for c in PREVIEW_COLUMNS if c in rows.columns]
            return render.DataGrid(rows[shown], selection_mode="rows", height="65vh")

        @reactive.effect
        @reactive.event(input.grid_selected_rows)
        def _grid_selection():
            """Clicks in the grid are a selection over the visible page only."""
            table = table_rv.get()
            if table is None:
                return
            indices = input.grid_selected_rows() or ()
            ids = table.page_processids(offset_rv.get())
            chosen = [ids[i] for i in indices if 0 <= i < len(ids)]
            table.set_selected(ids, False)
            table.set_selected(chosen, True)
            touch()

    return App(app_ui, server)


def run(snapshot: str | Path, *, host: str = "127.0.0.1", port: int = 8000,
        page_size: int = DEFAULT_PAGE_SIZE) -> None:
    import shiny

    shiny.run_app(create_app(snapshot, page_size=page_size), host=host, port=port)
