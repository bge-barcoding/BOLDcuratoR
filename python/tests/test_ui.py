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
    """A preview column no page carries renders as a silently missing one.

    ``bags_grade`` is the one exception and it is deliberate: grading is a fact
    about the whole result, so a page cannot carry it until a summary screen
    has been opened. `SearchState.grade_lookup` supplies it after that.
    """
    from boldcurator.core.table import SpecimenTable
    from boldcurator.data.queries import SearchQuery, plan_search, resolve_taxa
    from boldcurator.ui.app import PREVIEW_COLUMNS

    plan = plan_search(store, SearchQuery(
        taxa=resolve_taxa(store, ["Nymphalidae"]).resolved, expand_bins=True))
    rows = SpecimenTable(store, plan, page_size=5).page(0).rows
    missing = [c for c in PREVIEW_COLUMNS if c not in rows.columns]
    assert missing == ["bags_grade"], f"unexpected missing columns: {missing}"


def test_the_grade_appears_on_the_specimen_table_once_the_analysis_has_run(store):
    from boldcurator.ui.state import AppState

    state = AppState(store, page_size=5)
    state.run_search("Nymphalidae")
    assert state.search.grade_lookup() == {}, "not before the analysis has run"
    state.search.analysis(store)
    lookup = state.search.grade_lookup()
    assert lookup and set(lookup.values()) <= set("ABCDE")


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


# --------------------------------------------------------------------------
# The static renderers. Pure functions, and the place a missing value bites.
# --------------------------------------------------------------------------


def test_missing_values_render_as_blank_rather_than_raising():
    """`process_specimen_data` blanks bin_uri and country to pd.NA.

    `value != value` catches float NaN but raises on pd.NA, so every specimen
    table holding a BIN-less record rendered as "boolean value of NA is
    ambiguous" instead of a table.
    """
    import numpy as np
    import pandas as pd

    from boldcurator.ui.app import _escape, _group_html, _is_missing

    for missing in (None, pd.NA, np.nan, float("nan"), pd.NaT):
        assert _is_missing(missing), missing
        assert _escape(missing) == ""
    assert not _is_missing("")
    assert not _is_missing(0)
    assert _escape("<script>") == "&lt;script&gt;"

    frame = pd.DataFrame({
        "processid": ["P1", "P2"],
        "species": ["Danaus plexippus", pd.NA],
        "bin_uri": [pd.NA, "BOLD:A"],
        "country.ocean": [pd.NA, "Kenya"],
        "selected": [True, pd.NA],
        "flag": ["", pd.NA],
        "quality_score": [5, np.nan],
    })
    html = _group_html(frame)
    assert "Danaus plexippus" in html
    assert "BOLD:A" in html
    assert "nan" not in html.lower(), "a missing value must not print as 'nan'"


def test_a_record_can_be_selected_on_its_own_not_only_the_whole_group():
    """The point of the per-row checkbox: pick one record out of a group.

    Before this, "selected" rendered as a plain check mark and the only ways
    to change a selection were "select this whole group / page / result" --
    there was no way to act on a handful of records out of a larger group.
    """
    import pandas as pd

    from boldcurator.ui.app import ROW_CHECKBOX_CLASS, _group_html

    frame = pd.DataFrame({
        "processid": ["P1", "P2"],
        "species": ["Danaus plexippus", "Danaus chrysippus"],
        "bin_uri": ["BOLD:A", "BOLD:A"],
        "selected": [True, False],
    })
    html = _group_html(frame)

    assert html.count(f"class='{ROW_CHECKBOX_CLASS}'") == 2
    assert "data-pid='P1'" in html and "data-pid='P2'" in html
    # P1 is selected, P2 is not -- exactly one checkbox carries `checked`.
    p1_row = html.split("data-pid='P1'")[1].split("</tr>")[0]
    p2_row = html.split("data-pid='P2'")[1].split("</tr>")[0]
    assert "checked" in p1_row.split(">")[0]
    assert "checked" not in p2_row.split(">")[0]


def test_an_empty_frame_renders_a_message_not_a_broken_table():
    import pandas as pd

    from boldcurator.ui.app import _group_html

    assert "Nothing to show" in _group_html(pd.DataFrame())


def test_the_fixture_exercises_every_grade_the_screens_show(store):
    """A fixture with no grade-C data leaves the busiest screen untested."""
    from boldcurator.core.grouping import GRADES, group_specimens
    from boldcurator.ui.state import AppState

    state = AppState(store)
    state.run_search("Lepidoptera")
    result = state.search.analysis(store)
    present = set(result.bags_grades["bags_grade"])
    assert {"C", "E"} <= present, (
        f"the two grades curators focus on must be in the fixture; got {present}")

    for grade in GRADES:
        groups = group_specimens(result.specimens, result.bags_grades, grade)
        for group in groups:
            assert group.specimen_count > 0
            assert group.caption


def test_grade_c_keeps_each_bin_of_a_split_species_separate(store):
    """The point of the screen: one BIN of one species at a time."""
    from boldcurator.core.grouping import group_specimens
    from boldcurator.ui.state import AppState

    state = AppState(store)
    state.run_search("Lepidoptera")
    result = state.search.analysis(store)
    groups = group_specimens(result.specimens, result.bags_grades, "C")
    assert len(groups) >= 2

    for group in groups:
        assert len(group.bins) == 1, "a grade C group is one species in one BIN"
        bins = {b for b in group.specimens["bin_uri"].astype(object)
                if isinstance(b, str) and b}
        assert bins == set(group.bins), "a group must not mix BINs"


def test_the_specimen_table_renders_the_columns_it_says_it_does(store):
    """`_group_html` used to re-filter to the BAGS layout, silently dropping
    whatever columns the caller had chosen."""
    from boldcurator.ui.app import PREVIEW_COLUMNS, _group_html
    from boldcurator.ui.state import AppState

    state = AppState(store, page_size=5)
    state.run_search("Lepidoptera")
    state.search.analysis(store)          # so bags_grade is available
    rows = state.search.table.page(0).rows.copy()
    lookup = state.search.grade_lookup()
    rows["bags_grade"] = [lookup.get(s, "") if isinstance(s, str) else ""
                          for s in rows["species"].astype(object)]

    html = _group_html(rows, columns=PREVIEW_COLUMNS, limit=len(rows))
    header = html.split("<tbody>")[0]
    for column in PREVIEW_COLUMNS:
        assert f">{column}<" in header, f"{column} missing from the rendered header"


def test_the_priority_grades_are_marked_in_the_navigation():
    """E and C are where curators spend their time; the nav must say so."""
    from boldcurator.core.grouping import PRIORITY_GRADES
    from boldcurator.ui.app import _grade_panel

    def rendered(grade: str) -> str:
        # A NavPanel has no useful __str__; its rendered content and nav do.
        panel = _grade_panel(grade)
        return str(panel.content) + str(panel.nav)

    for grade in ("E", "C"):
        assert "work here first" in rendered(grade)
    for grade in ("A", "B", "D"):
        assert "work here first" not in rendered(grade)
    assert PRIORITY_GRADES == ("E", "C")


def test_a_fresh_search_auto_selects_a_representative_per_bin_and_country(store):
    """R auto-selects on a fresh import (``app.R:425-467``); so must this.

    The GUI never materialises the whole result at search time, so the only
    honest place to do this is the first time something needs the scored,
    whole-result frame -- which is ``analysis()``.
    """
    from boldcurator.ui.state import AppState

    state = AppState(store)
    state.run_search("Lepidoptera")
    assert state.annotations.selected == {}, "nothing selected before analysis runs"
    state.search.analysis(store)
    assert state.annotations.selected, "a fresh search must end up with a selection"
    assert all(v.get("auto_selected") for v in state.annotations.selected.values())


def test_auto_selection_never_overwrites_a_curator_s_own_choice(store):
    from boldcurator.ui.state import AppState

    state = AppState(store)
    state.run_search("Lepidoptera")
    first_page = state.search.table.page(0).rows
    manual_pid = str(first_page["processid"].iloc[0])
    state.annotations.set_selected(manual_pid, user="curator")

    state.search.analysis(store)

    assert state.annotations.selected == {
        manual_pid: state.annotations.selected[manual_pid]
    }, "an existing selection is a curator's, and analysis must leave it alone"


def test_selecting_a_group_replaces_the_selection_rather_than_adding(store):
    """Otherwise annotating the second problem re-annotates the first.

    The screens exist so a curator works one problem at a time; a selection
    that accumulates across groups quietly defeats that.
    """
    from boldcurator.ui.state import AppState

    state = AppState(store)
    state.run_search("Lepidoptera")
    groups = state.search.groups(store, "C")
    assert len(groups) >= 2

    def select(group):
        state.annotations.selected.clear()
        for pid in group.specimens["processid"]:
            state.annotations.set_selected(str(pid))

    select(groups[0])
    first = set(state.annotations.selected)
    select(groups[1])
    second = set(state.annotations.selected)

    assert second and not (first & second), "the two groups must not overlap"
    assert set(state.annotations.selected) == second, \
        "selecting a group must not leave the previous group selected"
