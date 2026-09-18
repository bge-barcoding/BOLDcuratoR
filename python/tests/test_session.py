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
    a.set_flag(first, "synonym", user="curator")
    a.set_note(first, "note text", user="curator")
    a.set_selected(first, user="curator")

    path = tmp_path / "sessions.sqlite"
    with SessionStore(path) as sessions:
        sessions.save("s1", result=result, annotations=a, user_email="a@example.org")

    with SessionStore(path) as sessions:
        loaded = sessions.load("s1")

    assert loaded is not None
    assert loaded.snapshot_id == result.snapshot_id
    assert loaded.record_count == len(result.specimens)
    assert loaded.annotations.flags[first]["flag"] == "synonym"
    assert loaded.annotations.curator_notes[first]["text"] == "note text"
    assert first in loaded.annotations.selected


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
    a.set_flag("P1", "data_issue")
    with SessionStore(path) as sessions:
        sessions.save("s1", result=result, annotations=a)

    con = sqlite3.connect(path)
    raw = con.execute("SELECT annotations_json FROM sessions").fetchone()[0]
    con.close()
    assert json.loads(raw)["flags"]["P1"]["flag"] == "data_issue"
