"""Opening and describing a snapshot.

The snapshot is opened **read-only**, always.  Nothing may ever open the
serving file read-write: DuckDB takes an exclusive cross-process lock for a
read-write handle, so a single stray writer locks every reader out.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import duckdb

from . import schema as S


class SnapshotError(RuntimeError):
    pass


@dataclass(frozen=True)
class SnapshotInfo:
    path: Path
    snapshot_id: str
    schema_version: str
    row_count: int
    sequence_count: int
    taxon_count: int
    bin_count: int
    marker_filter: str
    sequences_included: bool
    built_at: str

    def describe(self) -> str:
        marker = self.marker_filter or "all markers"
        seq = "with sequences" if self.sequences_included else "no sequences"
        return (
            f"snapshot {self.snapshot_id} -- {self.row_count:,} records "
            f"({marker}, {seq}), {self.bin_count:,} BINs, built {self.built_at}"
        )


class SnapshotStore:
    """A read-only handle on one snapshot file."""

    def __init__(self, path: str | Path):
        self.path = Path(path)
        if not self.path.exists():
            raise SnapshotError(f"No snapshot at {self.path}")
        wal = Path(str(self.path) + ".wal")
        if wal.exists():
            raise SnapshotError(
                f"A write-ahead log exists beside {self.path}. DuckDB cannot "
                "replay a WAL without write access, so the file cannot be "
                "opened read-only. Rebuild or checkpoint it."
            )
        try:
            self._con = duckdb.connect(str(self.path), read_only=True)
        except Exception as exc:  # noqa: BLE001
            raise SnapshotError(f"Cannot open {self.path} read-only: {exc}") from exc
        self._meta: dict[str, str] | None = None
        self._columns: list[str] | None = None

    # -- lifecycle ---------------------------------------------------------

    @property
    def connection(self) -> duckdb.DuckDBPyConnection:
        return self._con

    def close(self) -> None:
        self._con.close()

    def __enter__(self) -> "SnapshotStore":
        return self

    def __exit__(self, *exc: object) -> None:
        self.close()

    # -- description -------------------------------------------------------

    @property
    def meta(self) -> dict[str, str]:
        if self._meta is None:
            self._meta = dict(self._con.execute("SELECT key, value FROM _meta").fetchall())
        return self._meta

    @property
    def physical_columns(self) -> list[str]:
        """Physical ``specimen`` columns, excluding the surrogate key."""
        if self._columns is None:
            self._columns = [
                r[0] for r in self._con.execute("DESCRIBE specimen").fetchall()
                if r[0] != "sid"
            ]
        return self._columns

    @property
    def app_columns(self) -> list[str]:
        return [S.app_name(c) for c in self.physical_columns]

    def info(self) -> SnapshotInfo:
        m = self.meta

        def _int(key: str) -> int:
            try:
                return int(m.get(key, 0) or 0)
            except ValueError:
                return 0

        return SnapshotInfo(
            path=self.path,
            snapshot_id=m.get("snapshot_id", "unknown"),
            schema_version=m.get("schema_version", "unknown"),
            row_count=_int("row_count"),
            sequence_count=_int("sequence_count"),
            taxon_count=_int("taxon_count"),
            bin_count=_int("bin_count"),
            marker_filter=m.get("marker_filter", ""),
            sequences_included=m.get("sequences_included", "false") == "true",
            built_at=m.get("built_at", ""),
        )

    @property
    def has_sequences(self) -> bool:
        return self.info().sequences_included and self.info().sequence_count > 0

    @property
    def has_recordsets(self) -> bool:
        return (
            self._con.execute(
                "SELECT count(*) FROM specimen_recordset"
            ).fetchone()[0]
            > 0
        )
