"""The UI layer, to the extent it can be tested without a browser.

The browser half is `tools/drive_ui.py`, which is where the reactive-dependency
bugs actually surface -- two dead controls in this spike were invisible to
every test here and obvious on the first click.
"""

from __future__ import annotations

import pytest

shiny = pytest.importorskip("shiny", reason="the gui extra is not installed")


def test_the_app_object_builds(fixture_snapshot):
    from boldcurator.ui import create_app

    app = create_app(fixture_snapshot)
    assert isinstance(app, shiny.App)


def test_the_preview_columns_all_exist_in_a_real_page(store):
    """A preview column no page carries renders as a silently missing one."""
    from boldcurator.core.table import SpecimenTable
    from boldcurator.data.queries import SearchQuery, plan_search, resolve_taxa
    from boldcurator.ui.app import PREVIEW_COLUMNS

    plan = plan_search(store, SearchQuery(
        taxa=resolve_taxa(store, ["Nymphalidae"]).resolved, expand_bins=True))
    rows = SpecimenTable(store, plan, page_size=5).page(0).rows
    missing = [c for c in PREVIEW_COLUMNS if c not in rows.columns]
    assert not missing, f"preview columns absent from a page: {missing}"


def test_every_offered_sort_column_can_actually_be_sorted(store):
    """The select is populated from sortable_columns, so they must all work."""
    from boldcurator.core.table import SpecimenTable
    from boldcurator.data.queries import SearchQuery, plan_search, resolve_taxa

    plan = plan_search(store, SearchQuery(
        taxa=resolve_taxa(store, ["Danaus plexippus"]).resolved, expand_bins=True))
    table = SpecimenTable(store, plan, page_size=5)
    for column in table.sortable_columns:
        table.sort_by(column)
        assert table.sort_column == column
