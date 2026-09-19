"""Plan 3.8: save the current search, then rebuild it against the snapshot.

``tests/test_session.py`` already covers ``io/session.py`` (SQLite storage,
JSON round-tripping, the raw ``resume()`` rehydration) in isolation. This
file covers the GUI-facing half: ``AppState.save_session``/``resume_session``,
which route a saved session back through the normal search machinery --
``SearchPlan``, ``SpecimenTable``, ``analyse_plan`` -- rather than a
one-off rehydration path, so a resumed session pages, sorts and groups
exactly like a fresh search.
"""

from __future__ import annotations

import pytest

from boldcurator.data.queries import plan_from_processids
from boldcurator.io.session import SessionStore
from boldcurator.ui import state as state_module
from boldcurator.ui.state import AppState, ResultTooLargeToAnalyse


@pytest.fixture
def app_state(store) -> AppState:
    return AppState(store, page_size=25, user="curator")


def test_plan_from_processids_finds_every_row(store):
    ids = store.connection.execute(
        "SELECT processid FROM specimen LIMIT 5"
    ).fetchnumpy()["processid"].tolist()
    plan, missing = plan_from_processids(store, ids)
    assert plan.expanded_records == 5
    assert missing == []


def test_plan_from_processids_reports_what_it_could_not_find(store):
    real = store.connection.execute(
        "SELECT processid FROM specimen LIMIT 2"
    ).fetchnumpy()["processid"].tolist()
    plan, missing = plan_from_processids(store, real + ["BCTST-DOES-NOT-EXIST"])
    assert plan.expanded_records == 2
    assert missing == ["BCTST-DOES-NOT-EXIST"]


def test_plan_from_processids_handles_an_empty_list(store):
    plan, missing = plan_from_processids(store, [])
    assert plan.expanded_records == 0
    assert missing == []


def test_save_session_refuses_without_a_search(app_state, tmp_path):
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        with pytest.raises(ValueError, match="Run a search first"):
            app_state.save_session(sessions, "s1")


def test_save_then_resume_round_trips_the_search_and_annotations(app_state, tmp_path):
    app_state.run_search(taxa_text="Danaus plexippus")
    original_result = app_state.search.analysis(app_state.store)
    original_pids = set(original_result.specimens["processid"].astype(str))

    pid = str(original_result.specimens["processid"].iloc[0])
    app_state.annotations.set_flag(pid, "misidentification", user="curator")
    app_state.annotations.set_selected(pid, user="curator")

    with SessionStore(tmp_path / "s.sqlite") as sessions:
        record = app_state.save_session(sessions, "danaus-review", name="Danaus review")
        assert record.record_count == len(original_pids)

        # A fresh AppState -- as if the app were restarted -- resuming from
        # the saved session id should reconstruct the same result.
        fresh = AppState(app_state.store, page_size=25, user="someone else")
        loaded = sessions.load("danaus-review")
        assert loaded is not None
        status, warnings = fresh.resume_session(loaded)

    assert "restored" in status
    assert warnings == []
    resumed_result = fresh.search.analysis(fresh.store)
    assert set(resumed_result.specimens["processid"].astype(str)) == original_pids
    assert fresh.annotations.flags[pid]["flag"] == "misidentification"
    assert pid in fresh.annotations.selected
    # The specimen table itself -- not just the analysed frame -- pages off
    # the resumed plan, exactly like a live search's.
    page = fresh.search.table.page(0)
    assert len(page.rows) > 0


def test_resume_reports_processids_missing_from_the_current_snapshot(
    app_state, tmp_path
):
    app_state.run_search(taxa_text="Danaus plexippus")
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        saved = app_state.save_session(sessions, "s1")
        saved.processids = saved.processids[:3] + ["BCTST-DOES-NOT-EXIST"]

        fresh = AppState(app_state.store, page_size=25)
        status, warnings = fresh.resume_session(saved)

    assert fresh.search.plan.expanded_records == 3
    assert any("not in the current snapshot" in w for w in warnings)


def test_resume_warns_when_the_snapshot_id_changed(app_state, tmp_path):
    app_state.run_search(taxa_text="Danaus plexippus")
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        saved = app_state.save_session(sessions, "s1")
        saved.snapshot_id = "some-older-snapshot"

        fresh = AppState(app_state.store, page_size=25)
        _status, warnings = fresh.resume_session(saved)

    assert any("BIN membership" in w for w in warnings)


def test_save_session_refuses_a_result_too_large_to_analyse(
    app_state, tmp_path, monkeypatch
):
    app_state.run_search(taxa_text="Danaus plexippus")
    monkeypatch.setattr(state_module, "ANALYSIS_LIMIT", 0)
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        with pytest.raises(ResultTooLargeToAnalyse):
            app_state.save_session(sessions, "s1")


def test_saving_under_the_same_name_updates_the_session_in_place(app_state, tmp_path):
    app_state.run_search(taxa_text="Danaus plexippus")
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        first = app_state.save_session(sessions, "s1", name="My review")
        second = app_state.save_session(sessions, "s1", name="My review")
        assert second.created_at == first.created_at
        assert len(sessions.list_sessions()) == 1
