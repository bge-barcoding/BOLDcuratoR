"""A paged, sortable, selectable view over a search result.

**Why this is not in ``ui/``.**  The specimen table is the one screen whose
behaviour is genuinely hard -- paging, sorting, a selection that survives both,
and bulk edits over a selection larger than the page -- and it is also the
screen most likely to force a change of GUI framework.  Keeping the hard part
below ``ui/`` is what makes that swap cost a day instead of a fortnight: Shiny
or NiceGUI, the widget only ever asks for a page and reports clicks.

**Why paging is server-side and not optional.**  A real search returns 183 rows
for a species, 89,479 for a family and 2,095,427 for an order.  Handing any
widget the last of those is an out-of-memory kill, not a slow render.  The read
path is built to avoid it: ``plan_search`` resolves the exact row set up front
without materialising it, so a page is a slice of known ``rowid``s and costs the
same whether the result holds a hundred rows or two million.

Sorting works the same way.  Sorting the result fetches **one** column for the
whole of it -- narrow, so cheap -- orders the rowids by it in memory, and
carries on paging.  The other 70 columns are never touched outside the visible
page.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from ..data.queries import ROW_ID_COLUMN, SearchPlan, fetch_rows
from ..data.snapshot import SnapshotStore
from ..io.annotations import Annotations, merge_annotations
from .pipeline import process_specimen_data
from .ranking import score_and_rank

#: Columns that are computed per page rather than stored.  Sorting by one would
#: mean scoring the whole result, which is the thing paging exists to avoid, so
#: it is refused rather than silently ignored.
DERIVED_COLUMNS = frozenset({
    "quality_score", "criteria_met", "rank", "bags_grade",
    "data_source", "import_date",
})

DEFAULT_PAGE_SIZE = 100


@dataclass(frozen=True)
class Page:
    """One screenful: scored, annotated, in display order."""

    rows: pd.DataFrame
    offset: int
    page_size: int
    total_rows: int

    @property
    def page_number(self) -> int:
        return self.offset // self.page_size + 1 if self.page_size else 1

    @property
    def page_count(self) -> int:
        if not self.page_size:
            return 1
        return max(1, -(-self.total_rows // self.page_size))

    @property
    def has_previous(self) -> bool:
        return self.offset > 0

    @property
    def has_next(self) -> bool:
        return self.offset + self.page_size < self.total_rows


class SpecimenTable:
    """A window onto a planned result set.

    Owns the row set (as ``rowid``s), the display order, and the curator's
    annotations.  Renders nothing and imports no GUI framework.
    """

    def __init__(
        self,
        store: SnapshotStore,
        plan: SearchPlan,
        *,
        page_size: int = DEFAULT_PAGE_SIZE,
        annotations: Annotations | None = None,
        user: str = "",
    ) -> None:
        self.store = store
        self.plan = plan
        self.page_size = max(1, int(page_size))
        self.annotations = annotations if annotations is not None else Annotations()
        self.user = user
        self._order: np.ndarray = np.asarray(plan.row_ids)
        self.sort_column: str | None = None
        self.sort_descending: bool = False
        #: processid per rowid, filled in only if something needs the whole
        #: result's identity -- "select every matching record", for instance.
        self._processids: pd.Series | None = None

    # -- shape -------------------------------------------------------------

    @property
    def total_rows(self) -> int:
        return int(len(self._order))

    @property
    def page_count(self) -> int:
        return max(1, -(-self.total_rows // self.page_size))

    @property
    def sortable_columns(self) -> list[str]:
        return [c for c in self.store.app_columns if c not in DERIVED_COLUMNS]

    # -- ordering ----------------------------------------------------------

    def sort_by(self, column: str | None, *, descending: bool = False) -> None:
        """Order the whole result by one stored column.

        ``None`` restores the plan's order, which is the snapshot's physical
        (taxonomic) order and needs no fetch at all.
        """
        if column is None:
            self._order = np.asarray(self.plan.row_ids)
            self.sort_column, self.sort_descending = None, False
            return
        if column in DERIVED_COLUMNS:
            raise ValueError(
                f"{column!r} is computed per page, not stored. Sorting by it "
                "would mean scoring the whole result, which is what paging "
                "exists to avoid."
            )
        keys = fetch_rows(self.store, self.plan.row_ids, columns=[column],
                          with_row_id=True)
        ordered = keys.sort_values(
            column, ascending=not descending, kind="stable", na_position="last"
        )
        self._order = ordered[ROW_ID_COLUMN].to_numpy()
        self.sort_column, self.sort_descending = column, bool(descending)

    # -- paging ------------------------------------------------------------

    def page(self, offset: int = 0) -> Page:
        """Fetch, score and annotate exactly one page."""
        offset = max(0, min(int(offset), max(0, self.total_rows - 1)))
        wanted = self._order[offset:offset + self.page_size]
        frame = fetch_rows(self.store, wanted, with_row_id=True)

        # A semi-join returns rows in whatever order suits it, so put the page
        # back into the order the curator asked for before anything else.
        if len(frame):
            frame = frame.set_index(ROW_ID_COLUMN).reindex(wanted).reset_index()
        frame = frame.drop(columns=[ROW_ID_COLUMN])

        frame = process_specimen_data(frame, sort=False)
        frame = score_and_rank(frame)
        frame = merge_annotations(frame, self.annotations)
        return Page(rows=frame, offset=offset, page_size=self.page_size,
                    total_rows=self.total_rows)

    def page_processids(self, offset: int = 0) -> list[str]:
        """The processids on one page, without materialising the page."""
        wanted = self._order[offset:offset + self.page_size]
        frame = fetch_rows(self.store, wanted, columns=["processid"],
                           with_row_id=True)
        if not len(frame):
            return []
        frame = frame.set_index(ROW_ID_COLUMN).reindex(wanted)
        return [str(p) for p in frame["processid"]]

    def all_processids(self) -> list[str]:
        """Every processid in the result, in display order.

        One narrow column for the whole result -- about 30 MB even for the
        2.1 M-row case, against the gigabytes the full rows would cost. Cached,
        because "select all" and "export" both want it.
        """
        if self._processids is None:
            frame = fetch_rows(self.store, self.plan.row_ids,
                               columns=["processid"], with_row_id=True)
            self._processids = frame.set_index(ROW_ID_COLUMN)["processid"]
        ordered = self._processids.reindex(self._order)
        return [str(p) for p in ordered]

    # -- selection ---------------------------------------------------------

    @property
    def selected_count(self) -> int:
        return len(self.annotations.selected)

    def set_selected(self, processids, selected: bool = True) -> None:
        for processid in processids:
            processid = str(processid)
            if selected:
                self.annotations.set_selected(processid, user=self.user)
            else:
                self.annotations.unset_selected(processid)

    def select_page(self, offset: int = 0, *, selected: bool = True) -> int:
        """Select (or clear) every record on one page.  Returns how many."""
        ids = self.page_processids(offset)
        self.set_selected(ids, selected)
        return len(ids)

    def select_all(self, *, selected: bool = True) -> int:
        """Select (or clear) every record in the result.  Returns how many."""
        ids = self.all_processids()
        self.set_selected(ids, selected)
        return len(ids)

    def clear_selection(self) -> None:
        self.annotations.selected.clear()

    # -- bulk annotation ---------------------------------------------------

    def apply_flag(self, flag: str, processids=None) -> int:
        """Flag the selection (or a given set).  Returns how many changed.

        An empty flag clears, which is what the R dropdown's "None" does.
        """
        targets = self._targets(processids)
        for processid in targets:
            self.annotations.set_flag(processid, flag, user=self.user)
        return len(targets)

    def apply_note(self, note: str, processids=None) -> int:
        """Set a curator note on the selection.  Empty text clears it."""
        targets = self._targets(processids)
        for processid in targets:
            self.annotations.set_note(processid, note, user=self.user)
        return len(targets)

    def apply_updated_id(self, text: str, processids=None) -> int:
        """Set a corrected identification on the selection."""
        targets = self._targets(processids)
        for processid in targets:
            self.annotations.set_updated_id(processid, text, user=self.user)
        return len(targets)

    def _targets(self, processids) -> list[str]:
        if processids is None:
            return sorted(self.annotations.selected)
        return [str(p) for p in processids]
