import pytest

from boldcurator.io.annotations import Annotations
from boldcurator.io.session import SessionStore, resume


@pytest.fixture
def result(store):
    from boldcurator.core.pipeline import run_search

    return run_search(store, taxa_text="Danaus plexippus")


def test_session_stores_the_query_not_the_frame(tmp_path, result):
    """The whole point: a session is kilobytes, not ~125 MB."""
    with SessionStore(tmp_path / "sessions.sqlite") as sessions:
        sessions.save("s1", result=result, name="test", user_email="a@example.org")

    size = (tmp_path / "sessions.sqlite").stat().st_size
    assert size < 2_000_000
    assert size < result.specimens.memory_usage(deep=True).sum()


def test_round_trip_preserves_annotations(tmp_path, result):
    a = Annotations()
    first = str(result.specimens["processid"].iloc[0])
    a.set_flag(first, "misidentification", contamination=True,
               sequence_taxon="Homo sapiens", user="curator")
    a.set_updated_id(first, "Danaus erippus", rank="species",
                     method="Morphology", user="curator")
    a.set_note(first, "note text", user="curator")
    a.set_selected(first, user="curator")
    a.set_taxon_update("Danaus plexippus", "synonym", "Danaus archippus",
                       rank="species", citation="Smith 2019", user="curator")

    path = tmp_path / "sessions.sqlite"
    with SessionStore(path) as sessions:
        sessions.save("s1", result=result, annotations=a, user_email="a@example.org")

    with SessionStore(path) as sessions:
        loaded = sessions.load("s1")

    assert loaded is not None
    assert loaded.snapshot_id == result.snapshot_id
    assert loaded.record_count == len(result.specimens)
    assert loaded.annotations.flags[first]["flag"] == "misidentification"
    assert loaded.annotations.flags[first]["contamination"] is True
    assert loaded.annotations.flags[first]["sequence_taxon"] == "Homo sapiens"
    assert loaded.annotations.updated_ids[first]["method"] == "Morphology"
    assert loaded.annotations.curator_notes[first]["text"] == "note text"
    assert first in loaded.annotations.selected
    assert loaded.annotations.taxon_updates == a.taxon_updates
    assert loaded.annotations.converted == 0


def test_retired_flags_in_an_old_session_are_converted_on_load(tmp_path, result):
    """Agreed: Data Issue becomes the Other issue tick, ID Uncertain becomes
    ID unfounded, and Contamination (a flag before it was a tick) the
    Contamination tick -- counted, so the app can say so, except the last,
    whose meaning didn't change. Notes, IDs and who flagged it stay."""
    from boldcurator.config.constants import ID_FLAGS, LEGACY_FLAG_MAP

    assert not set(LEGACY_FLAG_MAP) & set(ID_FLAGS) - {"contamination"}
    pids = [str(p) for p in result.specimens["processid"].iloc[:4]]
    a = Annotations()
    stamp = {"timestamp": "2026-09-01T00:00:00", "user": "curator", "species": ""}
    a.flags[pids[0]] = {"flag": "id_uncertain", **stamp}
    a.flags[pids[1]] = {"flag": "data_issue", **stamp}
    a.flags[pids[2]] = {"flag": "contamination", **stamp}
    a.flags[pids[3]] = "misidentification"      # the oldest shape: a bare string
    a.updated_ids[pids[3]] = {"text": "Danaus erippus", "timestamp": "", "user": ""}
    path = tmp_path / "sessions.sqlite"
    with SessionStore(path) as sessions:
        sessions.save("old", result=result, annotations=a, user_email="a@example.org")
    with SessionStore(path) as sessions:
        loaded = sessions.load("old").annotations
    assert loaded.converted == 2
    assert loaded.flags[pids[0]]["flag"] == "id_unfounded"
    assert loaded.flags[pids[0]]["user"] == "curator"
    assert loaded.flags[pids[1]]["flag"] == "" and loaded.flags[pids[1]]["other_issue"]
    assert loaded.flags[pids[2]]["flag"] == "" and loaded.flags[pids[2]]["contamination"]
    assert loaded.flags[pids[3]]["flag"] == "misidentification"
    assert loaded.resolve(pids[3], "Danaus plexippus").updated_id == "Danaus erippus"


def test_resuming_an_old_session_says_what_was_converted(tmp_path, store):
    from boldcurator.ui.state import AppState

    state = AppState(store, page_size=25)
    state.run_search("Danaus plexippus")
    first = state.search.table.all_processids()[0]
    state.annotations.flags[first] = {"flag": "data_issue"}
    path = tmp_path / "sessions.sqlite"
    with SessionStore(path) as sessions:
        state.save_session(sessions, "old")
        _, warnings = state.resume_session(sessions.load("old"))
    assert any("converted" in w and "Other issue" in w for w in warnings)
    assert state.annotations.flags[first]["other_issue"] is True


def test_saving_twice_updates_in_place_and_keeps_created_at(tmp_path, result):
    path = tmp_path / "sessions.sqlite"
    with SessionStore(path) as sessions:
        first = sessions.save("s1", result=result, name="one")
        second = sessions.save("s1", result=result, name="two")
        assert second.created_at == first.created_at
        assert second.name == "two"
        assert len(sessions.list_sessions()) == 1


def test_list_can_filter_by_user(tmp_path, result):
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        sessions.save("a", result=result, user_email="a@example.org")
        sessions.save("b", result=result, user_email="b@example.org")
        assert len(sessions.list_sessions()) == 2
        mine = sessions.list_sessions(user_email="A@EXAMPLE.ORG")
        assert [s.session_id for s in mine] == ["a"]


def test_delete(tmp_path, result):
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        sessions.save("a", result=result)
        assert sessions.delete("a") is True
        assert sessions.delete("a") is False
        assert sessions.load("a") is None


def test_resume_rehydrates_and_rescores(tmp_path, result, store):
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        saved = sessions.save("s1", result=result)

    resumed = resume(saved, store)
    assert len(resumed.specimens) == len(result.specimens)
    assert not resumed.missing_processids
    assert {"quality_score", "rank", "criteria_met"} <= set(resumed.specimens.columns)


def test_resume_reports_records_missing_from_the_snapshot(tmp_path, result, store):
    """Records get retracted; a curator needs to know, not to silently lose them."""
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        saved = sessions.save("s1", result=result)
    saved.processids = saved.processids[:5] + ["BCTST-DOES-NOT-EXIST"]

    resumed = resume(saved, store)
    assert resumed.missing_processids == ["BCTST-DOES-NOT-EXIST"]
    assert any("not in the current snapshot" in w for w in resumed.warnings)
    assert len(resumed.specimens) == 5


def test_resume_warns_when_the_snapshot_changed(tmp_path, result, store):
    with SessionStore(tmp_path / "s.sqlite") as sessions:
        saved = sessions.save("s1", result=result)
    saved.snapshot_id = "some-older-snapshot"

    resumed = resume(saved, store)
    assert any("BIN membership" in w for w in resumed.warnings)


def test_annotations_are_plain_json_not_language_specific_blobs(tmp_path, result):
    """R stores serialize() blobs that nothing else can read."""
    import json
    import sqlite3

    path = tmp_path / "s.sqlite"
    a = Annotations()
    a.set_flag("P1", "id_unfounded")
    with SessionStore(path) as sessions:
        sessions.save("s1", result=result, annotations=a)

    con = sqlite3.connect(path)
    raw = con.execute("SELECT annotations_json FROM sessions").fetchone()[0]
    con.close()
    assert json.loads(raw)["flags"]["P1"]["flag"] == "id_unfounded"
