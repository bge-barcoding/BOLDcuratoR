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

#: Columns that live only in ``Annotations``, not the snapshot. Sorting by one
#: doesn't touch the database at all: every processid the plan resolves to is
#: already known (``all_processids``), and each annotation store is a small,
#: sparse dict already held in memory -- unlike ``DERIVED_COLUMNS``, this is
#: cheap regardless of result size.
ANNOTATION_SORT_COLUMNS = frozenset({
    "selected", "checked", "flag", "updated_id", "curator_notes",
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
        return [c for c in self.store.app_columns if c not in DERIVED_COLUMNS] \
            + list(ANNOTATION_SORT_COLUMNS)

    # -- ordering ----------------------------------------------------------

    def sort_by(self, column: str | None, *, descending: bool = False) -> None:
        """Order the whole result by one stored column.

        ``None`` restores the plan's order, which is the snapshot's physical
        (taxonomic) order and needs no fetch at all. A column in
        ``ANNOTATION_SORT_COLUMNS`` sorts by ``self.annotations`` instead of
        the snapshot -- see :meth:`_sort_by_annotation`.
        """
        if column is None:
            self._order = np.asarray(self.plan.row_ids)
            self.sort_column, self.sort_descending = None, False
            return
        if column in ANNOTATION_SORT_COLUMNS:
            self._sort_by_annotation(column, descending=descending)
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

    def _sort_by_annotation(self, column: str, *, descending: bool) -> None:
        """Order the whole result by a curator annotation, not a snapshot column.

        No database fetch: ``all_processids`` (cached) already has every
        processid in the plan, keyed by rowid, and each annotation store is a
        small dict already in memory. ``selected``/``checked`` sort by
        membership (bool); the rest reuse ``Annotations.to_frame``, the exact
        columns ``merge_annotations`` renders, so a curator sorting by "Flag"
        gets the same values they see in the table.
        """
        self.all_processids()  # ensures self._processids is populated
        ids_by_rowid = self._processids
        processids = [str(p) for p in ids_by_rowid]
        if column == "selected":
            key = pd.Series([p in self.annotations.selected for p in processids])
        elif column == "checked":
            key = pd.Series([p in self.annotations.working for p in processids])
        else:
            key = self.annotations.to_frame(processids)[column]
        order = pd.DataFrame({"key": key.to_numpy(), "rid": ids_by_rowid.index.to_numpy()})
        order = order.sort_values(
            "key", ascending=not descending, kind="stable", na_position="last"
        )
        self._order = order["rid"].to_numpy()
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
        # Not one of merge_annotations's six columns -- "checked" (the working
        # selection) has no curatorial meaning to export, but the table still
        # needs to render it. See io.annotations's module docstring for why it
        # is not the same thing as "selected".
        if len(frame):
            working = self.annotations.working
            frame["checked"] = [str(p) in working for p in frame["processid"]]
        else:
            frame["checked"] = pd.Series(dtype=bool)
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

    # -- the representative pick --------------------------------------------
    #
    # Persistent: auto-filled (core.selection, best per BIN x country) and
    # curator-overridable, one record at a time via its own checkbox
    # (ui/app.py's ROW_REP_CLASS). Nothing below ever bulk-clears it -- see
    # io.annotations's module docstring for why representative and working
    # selection must not share a store.

    @property
    def selected_count(self) -> int:
        return len(self.annotations.selected)

    # -- the working selection -----------------------------------------------
    #
    # Disposable: exists only to gather targets for apply_flag/apply_note/
    # apply_updated_id. "Check page/all", "Clear checked" and the per-row
    # ROW_CHECK_CLASS checkbox all act here, never on the representative pick.

    @property
    def checked_count(self) -> int:
        return len(self.annotations.working)

    def set_checked(self, processids, checked: bool = True) -> None:
        for processid in processids:
            self.annotations.set_working(str(processid), selected=checked)

    def check_page(self, offset: int = 0, *, checked: bool = True) -> int:
        """Check (or uncheck) every record on one page.  Returns how many."""
        ids = self.page_processids(offset)
        self.set_checked(ids, checked)
        return len(ids)

    def check_all(self, *, checked: bool = True) -> int:
        """Check (or uncheck) every record in the result.  Returns how many."""
        ids = self.all_processids()
        self.set_checked(ids, checked)
        return len(ids)

    def clear_checked(self) -> None:
        self.annotations.clear_working()

    # -- bulk annotation ---------------------------------------------------
    #
    # Acts on the working selection (self.annotations.working) by default, or
    # on an explicit processid list -- never on the representative pick.

    def apply_flag(self, flag: str, processids=None) -> int:
        """Flag the checked records (or a given set).  Returns how many changed.

        An empty flag clears, which is what the R dropdown's "None" does.
        """
        targets = self._targets(processids)
        for processid in targets:
            self.annotations.set_flag(processid, flag, user=self.user)
        return len(targets)

    def apply_note(self, note: str, processids=None) -> int:
        """Set a curator note on the checked records.  Empty text clears it."""
        targets = self._targets(processids)
        for processid in targets:
            self.annotations.set_note(processid, note, user=self.user)
        return len(targets)

    def apply_updated_id(self, text: str, processids=None) -> int:
        """Set a corrected identification on the checked records."""
        targets = self._targets(processids)
        for processid in targets:
            self.annotations.set_updated_id(processid, text, user=self.user)
        return len(targets)

    def _targets(self, processids) -> list[str]:
        if processids is None:
            return sorted(self.annotations.working)
        return [str(p) for p in processids]
