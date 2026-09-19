"""The paged table: the logic the GUI must not own."""

from __future__ import annotations

import pytest

from boldcurator.core.table import DEFAULT_PAGE_SIZE, SpecimenTable
from boldcurator.data.queries import (
    ROW_ID_COLUMN,
    SearchQuery,
    fetch_planned,
    fetch_rows,
    plan_search,
    resolve_taxa,
)


@pytest.fixture
def table(store):
    plan = plan_search(store, SearchQuery(
        taxa=resolve_taxa(store, ["Nymphalidae"]).resolved, expand_bins=True))
    return SpecimenTable(store, plan, page_size=10, user="tester")


def test_paging_covers_every_row_exactly_once(table):
    seen: list[str] = []
    for offset in range(0, table.total_rows, table.page_size):
        page = table.page(offset)
        assert len(page.rows) <= table.page_size
        seen.extend(str(p) for p in page.rows["processid"])
    assert len(seen) == table.total_rows
    assert len(set(seen)) == table.total_rows


def test_a_page_never_holds_more_than_a_page(table):
    page = table.page(0)
    assert len(page.rows) == min(table.page_size, table.total_rows)
    assert page.total_rows == table.total_rows
    assert page.page_count == table.page_count


def test_pages_are_scored_and_annotated_like_the_pipeline(table, store):
    page = table.page(0)
    for column in ("quality_score", "rank", "criteria_met", "selected", "checked",
                   "flag"):
        assert column in page.rows.columns
    # the same rows through the whole-result path must score identically
    full = fetch_planned(store, table.plan)
    from boldcurator.core.pipeline import process_specimen_data
    from boldcurator.core.ranking import score_and_rank

    scored = score_and_rank(process_specimen_data(full)).set_index("processid")
    for _, row in page.rows.iterrows():
        reference = scored.loc[str(row["processid"])]
        assert row["quality_score"] == reference["quality_score"]
        assert row["rank"] == reference["rank"]
        assert row["criteria_met"] == reference["criteria_met"]


def test_the_row_id_handle_never_reaches_the_caller(table):
    assert ROW_ID_COLUMN not in table.page(0).rows.columns


def test_sorting_orders_the_whole_result_not_just_the_page(table):
    table.sort_by("processid")
    first = [str(p) for p in table.page(0).rows["processid"]]
    assert first == sorted(first)

    last_offset = (table.page_count - 1) * table.page_size
    last = [str(p) for p in table.page(last_offset).rows["processid"]]
    assert min(last) > max(first), "later pages must sort after earlier ones"

    table.sort_by("processid", descending=True)
    descending = [str(p) for p in table.page(0).rows["processid"]]
    assert descending == sorted(descending, reverse=True)
    assert descending[0] == last[-1]


def test_sorting_by_none_restores_the_plan_order(table):
    original = [str(p) for p in table.page(0).rows["processid"]]
    table.sort_by("processid", descending=True)
    table.sort_by(None)
    assert [str(p) for p in table.page(0).rows["processid"]] == original
    assert table.sort_column is None


def test_sorting_by_a_computed_column_is_refused_not_ignored(table):
    """Silently ignoring it would show a table that lies about its order."""
    with pytest.raises(ValueError, match="computed per page"):
        table.sort_by("quality_score")
    assert "quality_score" not in table.sortable_columns
    assert "processid" in table.sortable_columns


# -- sorting by an annotation column: no database fetch involved -----------


def test_annotation_columns_are_sortable_without_touching_the_snapshot(table):
    for column in ("selected", "checked", "flag", "updated_id", "curator_notes"):
        assert column in table.sortable_columns


def test_sorting_by_selected_puts_the_representative_picks_first(table):
    ids = table.page_processids(0)[:2]
    table.annotations.set_selected(ids[0])
    table.sort_by("selected", descending=True)
    top = table.page(0).rows
    assert bool(top["selected"].iloc[0]) is True
    assert str(top["processid"].iloc[0]) == ids[0]


def test_sorting_by_checked_reaches_rows_never_rendered(table):
    ids = table.page_processids(0)[:1]
    table.set_checked(ids)
    table.sort_by("checked", descending=True)
    assert str(table.page(0).rows["processid"].iloc[0]) == ids[0]
    assert bool(table.page(0).rows["checked"].iloc[0]) is True


def test_sorting_by_flag_groups_the_flagged_records_together(table):
    ids = table.page_processids(0)[:3]
    table.apply_flag("id_uncertain", ids)
    table.sort_by("flag", descending=True)  # non-empty strings sort after ""
    top = {str(p) for p in table.page(0).rows["processid"].iloc[:3]}
    assert top == set(ids)


def test_sorting_by_updated_id_and_notes_uses_what_the_table_shows(table):
    ids = table.page_processids(0)[:2]
    table.set_checked(ids)
    table.apply_note("checked against the type series")
    table.apply_updated_id("Danaus plexippus")
    for column in ("updated_id", "curator_notes"):
        table.sort_by(column, descending=True)
        top = {str(p) for p in table.page(0).rows["processid"].iloc[:2]}
        assert top == set(ids), f"sorting by {column} should surface the edited rows"


def test_sorting_reaches_rows_that_are_not_on_the_first_page(table, store):
    """The sort key comes from the snapshot, not from the visible page."""
    table.sort_by("processid", descending=True)
    highest = str(table.page(0).rows["processid"].iloc[0])
    every = fetch_rows(store, table.plan.row_ids, columns=["processid"])
    assert highest == max(str(p) for p in every["processid"])


def test_checked_selection_survives_paging(table):
    table.check_page(0)
    assert table.checked_count == min(table.page_size, table.total_rows)
    first_page_ids = set(table.page_processids(0))
    # move away and back
    table.page(table.page_size)
    page = table.page(0)
    assert set(page.rows.loc[page.rows["checked"], "processid"].astype(str)) == \
        first_page_ids


def test_check_all_covers_rows_never_rendered(table):
    n = table.check_all()
    assert n == table.total_rows
    assert table.checked_count == table.total_rows
    table.clear_checked()
    assert table.checked_count == 0


def test_clearing_checked_leaves_the_representative_pick_alone(table):
    """The bug report: "Clear" used to wipe out representative picks too.

    Checking and the representative pick are different stores -- see
    io.annotations's module docstring. Nothing that acts on "checked" may
    ever touch "selected" (the representative pick).
    """
    ids = table.page_processids(0)[:3]
    table.annotations.set_selected(ids[0], user="auto", auto_selected=True)
    table.check_all()
    assert table.checked_count == table.total_rows
    assert table.annotations.selected_processids() == {ids[0]}

    table.clear_checked()
    assert table.checked_count == 0
    assert table.annotations.selected_processids() == {ids[0]}, (
        "clearing the checked/working selection must not touch the "
        "representative pick"
    )


def test_bulk_flag_applies_to_the_checked_set_not_the_page(table):
    table.check_all()
    changed = table.apply_flag("id_uncertain")
    assert changed == table.total_rows
    page = table.page(table.page_size)      # a page never rendered before
    assert (page.rows["flag"] == "id_uncertain").all()


def test_an_empty_flag_clears_it(table):
    table.check_page(0)
    table.apply_flag("id_uncertain")
    table.apply_flag("")
    assert (table.page(0).rows["flag"] == "").all()


def test_bulk_note_and_updated_id_land_on_the_checked_rows(table):
    ids = table.page_processids(0)[:3]
    table.set_checked(ids)
    table.apply_note("checked against the type series")
    table.apply_updated_id("Danaus plexippus")
    page = table.page(0)
    touched = page.rows[page.rows["processid"].astype(str).isin(ids)]
    untouched = page.rows[~page.rows["processid"].astype(str).isin(ids)]
    assert (touched["curator_notes"] == "checked against the type series").all()
    assert (touched["updated_id"] == "Danaus plexippus").all()
    assert (untouched["curator_notes"] == "").all()


def test_an_unknown_flag_is_refused(table):
    table.check_page(0)
    with pytest.raises(ValueError, match="Unknown flag"):
        table.apply_flag("definitely not a flag")


def test_an_empty_result_pages_without_blowing_up(store):
    plan = plan_search(store, SearchQuery(
        taxa=resolve_taxa(store, ["Danaus plexippus"]).resolved,
        countries=["Atlantis"], expand_bins=False))
    table = SpecimenTable(store, plan)
    assert table.total_rows == 0
    assert table.page_count == 1
    page = table.page(0)
    assert len(page.rows) == 0
    assert list(page.rows.columns), "an empty page still needs its columns"
    assert table.check_all() == 0


def test_offsets_past_the_end_clamp_rather_than_erroring(table):
    page = table.page(10 ** 9)
    assert len(page.rows) >= 1
    assert page.offset < table.total_rows


def test_the_default_page_size_is_something_a_widget_can_hold():
    assert 20 <= DEFAULT_PAGE_SIZE <= 500
