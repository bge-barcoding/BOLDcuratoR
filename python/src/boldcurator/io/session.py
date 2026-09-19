"""Session persistence.

Replaces ``R/utils/session_persistence.R``, which serialises the **entire**
specimen frame into a SQLite BLOB every 60 seconds per session
(``app.R:660-675``) -- roughly 125 MB for a 50,000-row result with sequences.

Here a session stores the *query* plus the processid list plus the annotations,
which is a few kilobytes. That is only safe because a named snapshot makes a
result reproducible: re-running the same query against the same ``snapshot_id``
returns the same records. The snapshot id is therefore stored and checked on
resume -- BIN membership and identifications genuinely change between releases,
so silently rehydrating against a different snapshot would be wrong.

Format is plain SQLite with JSON columns: readable by anything, unlike R's
``serialize()`` blobs, which no other language can open.
"""

from __future__ import annotations

import datetime as _dt
import json
import sqlite3
from dataclasses import dataclass, field
from pathlib import Path

from .annotations import Annotations

SCHEMA_VERSION = 1

_SCHEMA = """
CREATE TABLE IF NOT EXISTS sessions (
    session_id     TEXT PRIMARY KEY,
    name           TEXT NOT NULL DEFAULT '',
    created_at     TEXT NOT NULL,
    updated_at     TEXT NOT NULL,
    user_name      TEXT NOT NULL DEFAULT '',
    user_email     TEXT NOT NULL DEFAULT '',
    snapshot_id    TEXT NOT NULL DEFAULT '',
    query_json     TEXT NOT NULL DEFAULT '{}',
    processids_json TEXT NOT NULL DEFAULT '[]',
    annotations_json TEXT NOT NULL DEFAULT '{}',
    record_count   INTEGER NOT NULL DEFAULT 0,
    schema_version INTEGER NOT NULL DEFAULT 1
);
"""


@dataclass
class Session:
    session_id: str
    name: str = ""
    created_at: str = ""
    updated_at: str = ""
    user_name: str = ""
    user_email: str = ""
    snapshot_id: str = ""
    query: dict = field(default_factory=dict)
    processids: list[str] = field(default_factory=list)
    annotations: Annotations = field(default_factory=Annotations)
    record_count: int = 0

    def describe(self) -> str:
        return (
            f"{self.name or self.session_id} -- {self.record_count:,} records, "
            f"snapshot {self.snapshot_id or 'unknown'}, updated {self.updated_at}"
        )


class SessionStore:
    """A SQLite file of saved sessions."""

    def __init__(self, path: str | Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._con = sqlite3.connect(str(self.path))
        self._con.row_factory = sqlite3.Row
        self._con.executescript(_SCHEMA)
        self._con.commit()

    def close(self) -> None:
        self._con.close()

    def __enter__(self) -> "SessionStore":
        return self

    def __exit__(self, *exc: object) -> None:
        self.close()

    # -- write -------------------------------------------------------------

    def save(
        self,
        session_id: str,
        *,
        result=None,
        annotations: Annotations | None = None,
        name: str = "",
        user_name: str = "",
        user_email: str = "",
        query: dict | None = None,
    ) -> Session:
        now = _dt.datetime.now().isoformat(timespec="seconds")
        annotations = annotations or Annotations()

        if result is not None:
            processids = [str(p) for p in result.specimens.get("processid", [])]
            snapshot_id = getattr(result, "snapshot_id", "") or ""
            query = query if query is not None else {
                "taxonomy_groups": result.taxonomy_groups,
                "countries": result.geographic_filter,
                "missing_codes": result.missing_codes,
            }
        else:
            processids = []
            snapshot_id = ""
            query = query or {}

        existing = self.load(session_id)
        created = existing.created_at if existing else now

        self._con.execute(
            "INSERT INTO sessions (session_id, name, created_at, updated_at, "
            " user_name, user_email, snapshot_id, query_json, processids_json, "
            " annotations_json, record_count, schema_version) "
            "VALUES (?,?,?,?,?,?,?,?,?,?,?,?) "
            "ON CONFLICT(session_id) DO UPDATE SET "
            " name=excluded.name, updated_at=excluded.updated_at, "
            " user_name=excluded.user_name, user_email=excluded.user_email, "
            " snapshot_id=excluded.snapshot_id, query_json=excluded.query_json, "
            " processids_json=excluded.processids_json, "
            " annotations_json=excluded.annotations_json, "
            " record_count=excluded.record_count",
            (
                session_id, name, created, now, user_name, user_email, snapshot_id,
                json.dumps(query), json.dumps(processids),
                json.dumps(annotations.to_dict()), len(processids), SCHEMA_VERSION,
            ),
        )
        self._con.commit()
        return self.load(session_id)  # type: ignore[return-value]

    def delete(self, session_id: str) -> bool:
        cur = self._con.execute(
            "DELETE FROM sessions WHERE session_id = ?", (session_id,)
        )
        self._con.commit()
        return cur.rowcount > 0

    # -- read --------------------------------------------------------------

    def load(self, session_id: str) -> Session | None:
        row = self._con.execute(
            "SELECT * FROM sessions WHERE session_id = ?", (session_id,)
        ).fetchone()
        return self._row_to_session(row) if row else None

    def list_sessions(self, *, user_email: str = "") -> list[Session]:
        if user_email:
            rows = self._con.execute(
                "SELECT * FROM sessions WHERE lower(user_email) = lower(?) "
                "ORDER BY updated_at DESC",
                (user_email,),
            ).fetchall()
        else:
            rows = self._con.execute(
                "SELECT * FROM sessions ORDER BY updated_at DESC"
            ).fetchall()
        return [self._row_to_session(r) for r in rows]

    @staticmethod
    def _row_to_session(row: sqlite3.Row) -> Session:
        return Session(
            session_id=row["session_id"],
            name=row["name"],
            created_at=row["created_at"],
            updated_at=row["updated_at"],
            user_name=row["user_name"],
            user_email=row["user_email"],
            snapshot_id=row["snapshot_id"],
            query=json.loads(row["query_json"] or "{}"),
            processids=json.loads(row["processids_json"] or "[]"),
            annotations=Annotations.from_dict(json.loads(row["annotations_json"] or "{}")),
            record_count=row["record_count"],
        )


# --------------------------------------------------------------------------
# Resume
# --------------------------------------------------------------------------


@dataclass
class ResumeResult:
    session: Session
    specimens: "object"
    missing_processids: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)


def resume(session: Session, store) -> ResumeResult:
    """Rehydrate a session's records from the snapshot.

    Records are occasionally retracted, so a ``processid`` saved last month may
    be absent today. Those are **reported, never dropped silently** -- a curator
    who selected 200 representatives needs to know if they now have 197.
    """
    import pandas as pd

    from ..core.pipeline import process_specimen_data
    from ..core.ranking import score_and_rank
    from ..data import schema as S

    warnings: list[str] = []
    current = store.info().snapshot_id
    if session.snapshot_id and current != session.snapshot_id:
        warnings.append(
            f"This session was saved against snapshot {session.snapshot_id} but "
            f"the current snapshot is {current}. BIN membership and "
            "identifications may have changed since; scores and BAGS grades are "
            "recomputed against the current data."
        )

    if not session.processids:
        return ResumeResult(session=session, specimens=pd.DataFrame(),
                            warnings=warnings)

    ids = pd.DataFrame({"processid": session.processids})
    store.connection.register("_resume_ids", ids)
    try:
        frame = store.connection.execute(
            f"SELECT {S.projection(store.physical_columns)} FROM specimen s "
            "SEMI JOIN _resume_ids r ON r.processid = s.processid "
            "ORDER BY s.processid"
        ).df()
    finally:
        store.connection.unregister("_resume_ids")

    frame = score_and_rank(process_specimen_data(frame))

    found = set(frame["processid"].astype(str)) if len(frame) else set()
    missing = [p for p in session.processids if p not in found]
    if missing:
        warnings.append(
            f"{len(missing)} of {len(session.processids)} saved records are not in "
            "the current snapshot. They may have been retracted or reassigned. "
            "They are listed rather than dropped silently."
        )

    return ResumeResult(
        session=session,
        specimens=frame,
        missing_processids=missing,
        warnings=warnings,
    )
