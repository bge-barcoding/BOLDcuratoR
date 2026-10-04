"""A hostile snapshot file is harmless (SECURITY_FINDINGS.md, P5).

A ``.duckdb`` file is downloaded from Zenodo or picked from disk, so it is
untrusted input. It must not be able to read local files, reach the network
or load an extension through SQL it carries, and values inside it must not
be able to inject markup into the app's tables.
"""

from __future__ import annotations

import shutil
from html.parser import HTMLParser

import duckdb
import pytest

from boldcurator.data.snapshot import SnapshotError, SnapshotStore


def _tampered(fixture_snapshot, tmp_path, *statements: str):
    path = tmp_path / "tampered.duckdb"
    shutil.copy(fixture_snapshot, path)
    con = duckdb.connect(str(path))
    for sql in statements:
        con.execute(sql)
    con.close()
    return path


@pytest.mark.parametrize("statement, kind", [
    ("CREATE VIEW sneaky AS SELECT 1 AS a", "view main.sneaky"),
    ("CREATE MACRO sneaky(x) AS x + 1", "macro main.sneaky"),
    ("CREATE MACRO sneaky() AS TABLE SELECT 1 AS a", "table macro main.sneaky"),
])
def test_a_snapshot_with_a_view_or_macro_is_refused(fixture_snapshot, tmp_path,
                                                    statement, kind):
    path = _tampered(fixture_snapshot, tmp_path, statement)
    with pytest.raises(SnapshotError, match=kind):
        SnapshotStore(path)


def test_verify_fails_a_snapshot_with_a_view(fixture_snapshot, tmp_path):
    from boldcurator.build.verify import verify

    path = _tampered(fixture_snapshot, tmp_path,
                     "CREATE VIEW sneaky AS SELECT 1 AS a")
    checks = verify(path)
    assert len(checks) == 1 and checks[0].failed
    assert "view main.sneaky" in checks[0].detail


def test_the_real_snapshot_still_opens_and_reads(store):
    assert store.info().row_count > 0
    assert store.connection.execute("SELECT count(*) FROM specimen").fetchone()[0] > 0


def test_sql_cannot_read_local_files(store, tmp_path):
    secret = tmp_path / "secret.txt"
    secret.write_text("do not read me")
    with pytest.raises(duckdb.Error, match="disabled by configuration"):
        store.connection.execute(
            "SELECT * FROM read_text(?)", [str(secret)]).fetchall()


@pytest.mark.parametrize("setting", [
    "enable_external_access = true",
    "autoload_known_extensions = true",
    "autoinstall_known_extensions = true",
    "lock_configuration = false",
])
def test_the_lockdown_cannot_be_undone_from_inside_the_session(store, setting):
    with pytest.raises(duckdb.Error, match="locked"):
        store.connection.execute(f"SET {setting}")


def test_crafted_values_cannot_break_out_of_html_attributes():
    import pandas as pd

    from boldcurator.ui.app import _group_html

    evil = "x' onmouseover='alert(1)"
    frame = pd.DataFrame({
        "selected": [False], "checked": [False],
        "processid": [evil], "bin_uri": [evil], "species": [evil],
        evil: ["v"],
    })
    html = _group_html(frame, list(frame.columns), sort_input="s")

    names: set[str] = set()

    class Attributes(HTMLParser):
        def handle_starttag(self, tag, attrs):
            names.update(name for name, _ in attrs)

    Attributes().feed(html)
    assert "onmouseover" not in names
    assert {"href", "title", "data-pid", "data-sort-col"} <= names
    # The BIN is percent-encoded in the link, and still lands on the portal.
    assert "query=x%27%20onmouseover%3D%27alert%281%29[bin]" in html
