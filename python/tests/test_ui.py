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


def test_the_cc_by_sa_attribution_is_on_the_page(fixture_snapshot):
    """Plan item 0.3: the CC BY-SA 4.0 requirement goes in the app's about

    text, not only in exports (see test_exports.py for those).
    """
    from boldcurator.ui.app import create_app
    from boldcurator.ui.format import CC_BY_SA_URL

    app = create_app(fixture_snapshot)
    html = app.ui["html"]
    assert "CC BY-SA 4.0" in html
    assert "Barcode of Life Data System" in html
    assert CC_BY_SA_URL in html


def test_all_columns_ordered_drops_nothing_and_leads_with_the_curated_ones(store):
    """Plan round 3, item 1: the specimen table shows every column, not a

    curated subset -- matching the original R app's own `order_columns`/
    `PREFERRED_COLUMNS` (curated columns first, everything else after, with
    a horizontal scroll rather than anything hidden).
    """
    from boldcurator.core.table import SpecimenTable
    from boldcurator.data.queries import SearchQuery, plan_search, resolve_taxa
    from boldcurator.ui.app import _all_columns_ordered
    from boldcurator.ui.format import GROUP_COLUMNS

    plan = plan_search(store, SearchQuery(
        taxa=resolve_taxa(store, ["Nymphalidae"]).resolved, expand_bins=True))
    rows = SpecimenTable(store, plan, page_size=5).page(0).rows
    ordered = _all_columns_ordered(rows)

    assert set(ordered) == set(rows.columns), "a column was dropped or invented"
    assert len(ordered) == len(rows.columns), "no duplicates either"
    curated_present = [c for c in GROUP_COLUMNS if c in rows.columns]
    assert ordered[:len(curated_present)] == curated_present
    # A real BOLD column that is deliberately not curated, to prove "the rest"
    # isn't empty -- this port's schema always carries it.
    assert "sampleid" in ordered[len(curated_present):]


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
    """The point of the per-row checkboxes: pick one record out of a group.

    Before this, "selected" rendered as a plain check mark and the only ways
    to change it were "select this whole group / page / result" -- there was
    no way to act on a handful of records out of a larger group. There are now
    two independent checkboxes per row -- "Rep." (the representative pick,
    ``ROW_REP_CLASS``) and "Check" (the disposable bulk-edit selection,
    ``ROW_CHECK_CLASS``) -- see io.annotations's module docstring for why they
    must not be the same one.
    """
    import pandas as pd

    from boldcurator.ui.app import ROW_CHECK_CLASS, ROW_REP_CLASS, _group_html

    frame = pd.DataFrame({
        "processid": ["P1", "P2"],
        "species": ["Danaus plexippus", "Danaus chrysippus"],
        "bin_uri": ["BOLD:A", "BOLD:A"],
        "selected": [True, False],   # representative: P1 only
        "checked": [False, True],    # working: P2 only
    })
    html = _group_html(frame)

    assert html.count(f"class='{ROW_REP_CLASS}'") == 2
    assert html.count(f"class='{ROW_CHECK_CLASS}'") == 2

    def row(pid: str) -> str:
        marker = f"data-pid='{pid}'"
        start = html.index(marker)
        # walk back/forward to the enclosing <tr>...</tr>
        tr_start = html.rindex("<tr>", 0, start)
        tr_end = html.index("</tr>", start)
        return html[tr_start:tr_end]

    p1, p2 = row("P1"), row("P2")
    # P1 is the representative but not checked.
    assert f"class='{ROW_REP_CLASS}'" in p1.split(f"class='{ROW_CHECK_CLASS}'")[0]
    rep_cell_p1 = p1.split(f"class='{ROW_REP_CLASS}'")[1].split(">")[0]
    check_cell_p1 = p1.split(f"class='{ROW_CHECK_CLASS}'")[1].split(">")[0]
    assert "checked" in rep_cell_p1
    assert "checked" not in check_cell_p1
    # P2 is checked but not the representative.
    rep_cell_p2 = p2.split(f"class='{ROW_REP_CLASS}'")[1].split(">")[0]
    check_cell_p2 = p2.split(f"class='{ROW_CHECK_CLASS}'")[1].split(">")[0]
    assert "checked" not in rep_cell_p2
    assert "checked" in check_cell_p2


def test_processid_bin_and_species_link_out_to_the_bold_portal():
    """A curator working offline from a snapshot still wants to look a record

    up on BOLD itself -- the portal is public and needs no API key, so a link
    costs nothing this app depends on.
    """
    import pandas as pd

    from boldcurator.ui.app import _group_html
    from boldcurator.ui.format import bold_bin_url, bold_record_url, bold_species_url

    frame = pd.DataFrame({
        "processid": ["GBMHO3680-19"],
        "species": ["Cordulegaster heros"],
        "bin_uri": ["BOLD:AAJ5773"],
    })
    html = _group_html(frame)

    assert f"href='{bold_record_url('GBMHO3680-19')}'" in html
    assert f"href='{bold_bin_url('BOLD:AAJ5773')}'" in html
    assert f"href='{bold_species_url('Cordulegaster heros')}'" in html
    assert html.count("target='_blank'") == 3
    assert "GBMHO3680-19</a>" in html


def test_sticky_columns_freeze_to_the_left_edge_in_column_order():
    """selected/checked/flag/updated_id/curator_notes stay put while the rest

    of a wide table scrolls sideways underneath them.
    """
    import pandas as pd

    from boldcurator.ui.app import STICKY_COLUMN_WIDTHS, _group_html

    frame = pd.DataFrame({
        "selected": [True], "checked": [False], "flag": ["misidentification"],
        "updated_id": ["Danaus plexippus"], "curator_notes": ["checked"],
        "processid": ["P1"], "species": ["Danaus plexippus"],
    })
    html = _group_html(frame)
    header = html.split("<tbody>")[0]

    running = 0
    for column in ("selected", "checked", "flag", "updated_id", "curator_notes"):
        width = STICKY_COLUMN_WIDTHS[column]
        assert f"left:{running}px" in html, f"{column} should sit at {running}px"
        running += width
    # every header cell stays pinned to the top (see test below), but a
    # column that isn't one of the five never gets a *left* offset
    other_header_cell = header.split("Process ID")[1].split("</th>")[0]
    assert "left:" not in other_header_cell


def test_every_header_cell_stays_pinned_to_the_top_not_just_the_thead():
    """The bug: only the five frozen-left headers had their own

    ``position:sticky``; the rest relied on ``<thead>``'s, which browsers do
    not reliably honour (``<thead>`` is ``display:table-header-group``, not a
    table cell). Every ``<th>`` now carries its own top-sticky style.
    """
    import pandas as pd

    from boldcurator.ui.app import _group_html

    frame = pd.DataFrame({"processid": ["P1"], "species": ["Danaus plexippus"]})
    html = _group_html(frame)
    header = html.split("<tbody>")[0]
    assert "<thead><tr>" in header, "sticky belongs on the cells, not <thead>"
    for column_label in ("Process ID", "Species"):
        cell = header.split(column_label)[0].split("<th")[-1]
        assert "position:sticky" in cell and "top:0" in cell


def test_an_empty_frame_renders_a_message_not_a_broken_table():
    import pandas as pd

    from boldcurator.ui.app import _group_html

    assert "Nothing to show" in _group_html(pd.DataFrame())


def test_column_headers_are_clickable_and_carry_the_sort_arrow():
    """Click-a-header sorting: the point of this issue.

    A sortable column's header carries the class the delegated JS listener
    watches for, the Shiny input name it should post to, and an arrow on
    whichever column is currently the sort key -- so a curator can tell what
    they are looking at without a separate dropdown.
    """
    import pandas as pd

    from boldcurator.ui.app import SORT_HEADER_CLASS, _table

    frame = pd.DataFrame({"species": ["B", "A"], "count": [2, 1]})
    html = _table(frame, sort_input="my_sort", sortable=frozenset(frame.columns),
                 sort_state=("species", False))
    header = html.split("<tbody>")[0]

    assert header.count(f"class='{SORT_HEADER_CLASS}'") == 2
    assert "data-sort-input='my_sort'" in header
    assert "data-sort-col='species'" in header and "data-sort-col='count'" in header
    # only the current sort column carries an arrow, and ascending is "up"
    species_th = header.split("data-sort-col='species'")[1].split("</th>")[0]
    count_th = header.split("data-sort-col='count'")[1].split("</th>")[0]
    assert "▲" in species_th
    assert "▲" not in count_th and "▼" not in count_th


def test_a_column_left_out_of_sortable_renders_a_plain_header():
    """A caller can still restrict which columns are clickable (the specimen

    table does, for columns SpecimenTable.sort_by can't fetch server-side);
    _table must honour that rather than making everything clickable.
    """
    import pandas as pd

    from boldcurator.ui.app import SORT_HEADER_CLASS, _table

    frame = pd.DataFrame({"selected": [True], "species": ["A"]})
    html = _table(frame, sort_input="my_sort", sortable=frozenset({"species"}))
    header = html.split("<tbody>")[0]
    excluded_cell = header.split("selected")[0].split("<th")[-1]
    assert "cursor:pointer" not in excluded_cell, "excluded column must not be clickable"


def test_group_tables_default_to_sorting_by_rep_and_check_too():
    """Flag/Updated ID/Notes were already sortable in a BAGS group table;

    Rep./Check (selected/checked) were the two left out. A group table is
    already fully in memory, so there is no reason a boolean column can't
    sort like any other.
    """
    import pandas as pd

    from boldcurator.ui.app import SORT_HEADER_CLASS, _group_html

    frame = pd.DataFrame({
        "selected": [True, False], "checked": [False, True],
        "processid": ["P1", "P2"], "species": ["A", "B"],
    })
    html = _group_html(frame, sort_input="group_sort_click")
    header = html.split("<tbody>")[0]
    assert header.count(f"class='{SORT_HEADER_CLASS}'") == 4


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
    from boldcurator.ui.app import _all_columns_ordered, _group_html
    from boldcurator.ui.format import GROUP_LABELS
    from boldcurator.ui.state import AppState

    state = AppState(store, page_size=5)
    state.run_search("Lepidoptera")
    state.search.analysis(store)          # so bags_grade is available
    rows = state.search.table.page(0).rows.copy()
    lookup = state.search.grade_lookup()
    rows["bags_grade"] = [lookup.get(s, "") if isinstance(s, str) else ""
                          for s in rows["species"].astype(object)]

    columns = _all_columns_ordered(rows)
    html = _group_html(rows, columns=columns, limit=len(rows))
    header = html.split("<tbody>")[0]
    for column in columns:
        label = GROUP_LABELS.get(column, column)
        assert f">{label}<" in header, f"{column} missing from the rendered header"


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


def test_checking_a_group_replaces_the_checked_set_rather_than_adding(store):
    """Otherwise annotating the second problem re-annotates the first.

    The screens exist so a curator works one problem at a time; a checked set
    that accumulates across groups quietly defeats that. This is the working
    selection ("Check this group" / "Apply to checked"), not the
    representative pick -- see io.annotations's module docstring.
    """
    from boldcurator.ui.state import AppState

    state = AppState(store)
    state.run_search("Lepidoptera")
    groups = state.search.groups(store, "C")
    assert len(groups) >= 2

    def check(group):
        state.annotations.clear_working()
        for pid in group.specimens["processid"]:
            state.annotations.set_working(str(pid))

    check(groups[0])
    first = set(state.annotations.working)
    check(groups[1])
    second = set(state.annotations.working)

    assert second and not (first & second), "the two groups must not overlap"
    assert set(state.annotations.working) == second, \
        "checking a group must not leave the previous group checked"
