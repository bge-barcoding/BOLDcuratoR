"""Build a queryable DuckDB snapshot from the BOLD public data package.

Input is the package as published: a tab-separated file, optionally gzipped,
with a BCDM header row.  Output is a single DuckDB file that the application
opens **read-only**.

Run it with ``python tools/build_snapshot.py --tsv <file> --out <file>`` or, once
installed, ``boldcurator-build-snapshot``.

Several choices here are failure modes already diagnosed in
``docs/archive/static-datapackage-plan.md``; each is commented where it
appears rather than left to look like a preference.
"""

from __future__ import annotations

import argparse
import datetime as _dt
import gzip
import hashlib
import io
import os
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path

import duckdb

from ..data import schema as S


# --------------------------------------------------------------------------
# small helpers
# --------------------------------------------------------------------------


def _log(msg: str) -> None:
    print(msg, flush=True)


class BuildError(RuntimeError):
    """A condition that must stop the build rather than publish bad data."""


@dataclass
class _Step:
    name: str
    started: float = field(default_factory=time.monotonic)

    def done(self) -> float:
        elapsed = time.monotonic() - self.started
        _log(f"  {self.name}: {elapsed:.1f}s")
        return elapsed


def _open_text(path: Path) -> io.TextIOBase:
    if path.suffix == ".gz":
        return gzip.open(path, "rt", encoding="utf-8", errors="replace")
    return open(path, "r", encoding="utf-8", errors="replace")


def read_header(path: Path) -> list[str]:
    """Read just the header row, without decompressing the whole file."""
    with _open_text(path) as fh:
        line = fh.readline()
    if not line:
        raise BuildError(f"{path} is empty -- no header row")
    return [c.strip() for c in line.rstrip("\r\n").split("\t")]


def sha256_file(path: Path, chunk: int = 8 << 20) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for block in iter(lambda: fh.read(chunk), b""):
            h.update(block)
    return h.hexdigest()


def _sql_str(value: str) -> str:
    """A SQL single-quoted string literal."""
    return "'" + value.replace("'", "''") + "'"


# --------------------------------------------------------------------------
# column planning
# --------------------------------------------------------------------------


@dataclass
class ColumnPlan:
    header: list[str]
    kept: list[str]            # source names, in header order
    missing_optional: list[str]
    sequence_present: bool
    recordset_present: bool

    @property
    def physical(self) -> list[str]:
        return [S.physical_name(c) for c in self.kept]


def plan_columns(header: list[str], *, include_sequences: bool) -> ColumnPlan:
    """Decide which source columns to keep, failing loudly on a missing required one.

    Keeps **every** header column except :data:`S.EXCLUDED_SOURCE_COLUMNS`
    (privacy-sensitive fields) and the sequence column (handled separately) --
    not just the ones named in :data:`S.REQUIRED_SOURCE_COLUMNS`/
    :data:`S.OPTIONAL_SOURCE_COLUMNS`. A curated allowlist silently drops any
    BCDM field this project hasn't been told about yet, which is exactly what
    a curator reported as "lost most columns" against the original R app,
    whose own column handling (`PREFERRED_COLUMNS` in `R/config/constants.R`)
    only ever reorders what it already has, never narrows it.
    """
    present = set(header)

    missing_required = [c for c in S.REQUIRED_SOURCE_COLUMNS if c not in present]
    if missing_required:
        raise BuildError(
            "The data package is missing columns the application cannot work "
            "without:\n  " + "\n  ".join(missing_required) + "\n\n"
            "This usually means the BCDM schema changed. Reconcile "
            "boldcurator/data/schema.py with the new header before rebuilding; "
            "do not drop the requirement silently -- a missing scoring field "
            "would score every record lower with no error.\n"
            f"Header has {len(header)} columns: {', '.join(sorted(present))}"
        )

    missing_optional = [c for c in S.OPTIONAL_SOURCE_COLUMNS if c not in present]

    wanted = present - set(S.EXCLUDED_SOURCE_COLUMNS) - {S.SEQUENCE_SOURCE_COLUMN}

    # Header order, so the physical layout follows the source layout.
    kept = [c for c in header if c in wanted]

    sequence_present = S.SEQUENCE_SOURCE_COLUMN in present
    if include_sequences and not sequence_present:
        raise BuildError(
            f"--with-sequences was requested but the package has no "
            f"'{S.SEQUENCE_SOURCE_COLUMN}' column. Pass --no-sequences to build "
            "a metadata-only snapshot."
        )

    return ColumnPlan(
        header=header,
        kept=kept,
        missing_optional=missing_optional,
        sequence_present=sequence_present,
        recordset_present="bold_recordset_code_arr" in present,
    )


def read_csv_clause(path: Path, header: list[str]) -> str:
    """The ``read_csv`` call, with every option that matters spelled out.

    ``auto_detect=false`` with an explicit ``columns`` map is not optional on a
    ``.gz``: a gzip stream is unseekable, so DuckDB cannot rewind to re-sniff
    types and buffers the whole file instead, exhausting RAM.

    ``quote=''`` / ``escape=''`` matter just as much: with the default ``"``, a
    single unbalanced double quote in a ``notes`` field swallows the rest of the
    file into one value.
    """
    cols = ", ".join(f"{_sql_str(c)}: 'VARCHAR'" for c in header)
    compression = "gzip" if path.suffix == ".gz" else "none"
    return (
        f"read_csv({_sql_str(str(path))}, "
        f"delim='\\t', header=true, auto_detect=false, "
        f"compression='{compression}', "
        f"columns={{{cols}}}, "
        f"quote='', escape='', "
        f"nullstr=['', 'None', 'NA'], "
        f"ignore_errors=false)"
    )


def staging_select(plan: ColumnPlan, *, include_sequences: bool) -> str:
    """Projection from the raw VARCHAR read into staging's physical columns."""
    parts = []
    for src in plan.kept:
        phys = S.physical_name(src)
        cast = S.NUMERIC_SOURCE_COLUMNS.get(src)
        ref = S.quote_ident(src)
        if cast:
            # TRY_CAST, not CAST: a malformed number becomes NULL rather than
            # failing a 30 GB ingest at hour two.
            parts.append(f"TRY_CAST({ref} AS {cast}) AS {S.quote_ident(phys)}")
        else:
            parts.append(f"{ref} AS {S.quote_ident(phys)}")
    if include_sequences:
        parts.append(
            f"{S.quote_ident(S.SEQUENCE_SOURCE_COLUMN)} AS "
            f"{S.quote_ident(S.SEQUENCE_SOURCE_COLUMN)}"
        )
    return ",\n       ".join(parts)


# --------------------------------------------------------------------------
# build
# --------------------------------------------------------------------------


def describe_columns(tsv: Path, *, include_sequences: bool = True) -> int:
    """Report what a build would keep, reading only the header.

    A fraction of a second even on a 30 GB file, so a column mismatch is found
    before an hour of ingest rather than after it. Returns a process exit code.
    """
    if not tsv.exists():
        raise BuildError(f"No such file: {tsv}")

    header = read_header(tsv)
    _log(f"Source : {tsv}  ({tsv.stat().st_size / 1e9:.2f} GB)")
    _log(f"Header : {len(header)} columns")
    _log("")

    present = set(header)
    missing_required = [c for c in S.REQUIRED_SOURCE_COLUMNS if c not in present]
    missing_optional = [c for c in S.OPTIONAL_SOURCE_COLUMNS if c not in present]
    excluded = [c for c in S.EXCLUDED_SOURCE_COLUMNS if c in present]
    unrecognized = sorted(
        present
        - set(S.REQUIRED_SOURCE_COLUMNS)
        - set(S.OPTIONAL_SOURCE_COLUMNS)
        - set(S.EXCLUDED_SOURCE_COLUMNS)
        - {S.SEQUENCE_SOURCE_COLUMN}
    )

    kept = [c for c in header
            if c not in set(S.EXCLUDED_SOURCE_COLUMNS)
            and c != S.SEQUENCE_SOURCE_COLUMN]

    _log(f"Would keep ({len(kept)}):")
    _log("  " + ", ".join(kept))
    if include_sequences:
        seq = "yes" if S.SEQUENCE_SOURCE_COLUMN in present else "NO -- no sequence column!"
        _log(f"Sequences ({S.SEQUENCE_SOURCE_COLUMN}): {seq}")
    if excluded:
        _log(f"Excluded on purpose: {', '.join(excluded)}")
    if missing_optional:
        _log(f"Optional, absent ({len(missing_optional)}): {', '.join(missing_optional)}")
    if unrecognized:
        # Kept anyway -- every header column not excluded is kept, named or
        # not (see plan_columns's docstring). This is purely "you may want to
        # name these in schema.py so 'optional, absent' can warn about them
        # on a future header that's missing them."
        _log(f"In the file, not yet named in schema.py, kept anyway "
             f"({len(unrecognized)}): {', '.join(unrecognized)}")

    _log("")
    if "marker_code" in present:
        _log("marker_code present -- the COI-5P filter will work.")
    if missing_required:
        _log(f"MISSING {len(missing_required)} REQUIRED column(s):")
        for c in missing_required:
            _log(f"  {c}")
        _log("")
        _log("A build would refuse to run. Reconcile "
             "boldcurator/data/schema.py with this header first.")
        return 1

    _log("All required columns are present -- a build would run.")
    return 0


def build(
    tsv: Path,
    out: Path,
    *,
    marker: str | None = "COI-5P",
    include_sequences: bool = True,
    memory_limit: str = "8GB",
    threads: int = 4,
    temp_dir: Path | None = None,
    max_temp_size: str = "80GB",
    snapshot_id: str | None = None,
    hash_source: bool = True,
    limit: int | None = None,
    allow_recordset_drift: bool = False,
    progress: bool = True,
    reuse_staging: bool = False,
    overwrite: bool = False,
    keep_staging: bool = False,
    staging_path: Path | None = None,
) -> dict[str, str]:
    if not tsv.exists():
        raise BuildError(f"No such file: {tsv}")
    if out.exists():
        if not overwrite:
            raise BuildError(
                f"Refusing to overwrite an existing snapshot: {out}\n"
                "Pass --overwrite to replace it (useful after a failed run left "
                "a partial file behind)."
            )
        out.unlink()
        Path(str(out) + ".wal").unlink(missing_ok=True)

    out.parent.mkdir(parents=True, exist_ok=True)
    # Staging depends on the source and the marker filter, not on the output, so
    # it can be shared between builds that write different snapshots -- e.g.
    # metadata-only first, then the full one, from a single ingest.
    staging = Path(staging_path) if staging_path else out.with_suffix(
        out.suffix + ".staging"
    )
    staging.parent.mkdir(parents=True, exist_ok=True)

    # A failed build leaves staging behind on purpose -- it holds the whole
    # ingested package, and re-reading 30+ GB to retry a later step is a waste
    # of ~10 minutes.
    have_staging = staging.exists()
    if reuse_staging and not have_staging:
        raise BuildError(
            f"--reuse-staging was given but there is no staging file at "
            f"{staging}.\n"
            "Staging defaults to <out>.staging, so a build writing to a "
            "different --out will not find one left by an earlier build. Pass "
            "--staging-path to point both builds at the same file, or run "
            "without --reuse-staging to ingest from the source."
        )
    if not reuse_staging:
        for stale in (staging, Path(str(staging) + ".wal")):
            if stale.exists():
                stale.unlink()

    header = read_header(tsv)
    plan = plan_columns(header, include_sequences=include_sequences)

    _log(f"Source      : {tsv}  ({tsv.stat().st_size / 1e9:.2f} GB)")
    _log(f"Header      : {len(header)} columns")
    _log(f"Keeping     : {len(plan.kept)} columns" + (" + nuc" if include_sequences else ""))
    if plan.missing_optional:
        _log(f"Not present : {', '.join(plan.missing_optional)}")
    if not plan.recordset_present:
        _log("WARNING     : no bold_recordset_code_arr column -- dataset/project "
             "code search will be unavailable in this snapshot")
    _log(f"Marker      : {marker or '(no filter -- all markers)'}")
    _log(f"Sequences   : {'included' if include_sequences else 'excluded'}")
    if limit:
        _log(f"Row limit   : {limit:,}  -- PARTIAL BUILD, for testing only")
    _log("")
    _log("The ingest step reads the whole file and is the long one "
         "(30-60 min on a full package). Progress is reported below.")
    _log("")

    snapshot_id = snapshot_id or _dt.date.today().isoformat()
    source_sha = ""
    if hash_source:
        st = _Step("sha256 of source")
        source_sha = sha256_file(tsv)
        st.done()

    con = duckdb.connect(str(staging))
    try:
        con.execute(f"SET memory_limit = '{memory_limit}'")
        con.execute(f"SET threads = {int(threads)}")
        # Insertion order is meaningless here -- we impose our own sort -- and
        # preserving it costs a great deal of memory on a 30 GB ingest.
        con.execute("SET preserve_insertion_order = false")
        if temp_dir is not None:
            temp_dir.mkdir(parents=True, exist_ok=True)
            con.execute(f"SET temp_directory = {_sql_str(str(temp_dir))}")
        con.execute(f"SET max_temp_directory_size = '{max_temp_size}'")
        if progress:
            # The ingest is the long step and otherwise prints nothing for
            # 30-60 minutes, which reads as a hang. DuckDB's own progress bar
            # is the only thing that can report progress from inside it.
            try:
                con.execute("SET enable_progress_bar = true")
                con.execute("SET enable_progress_bar_print = true")
            except duckdb.Error:  # pragma: no cover - older DuckDB builds
                pass

        # ---------------------------------------------------------------- ingest
        where = []
        if marker:
            where.append(f"marker_code = {_sql_str(marker)}")
        where.append("processid IS NOT NULL AND processid <> ''")
        where_sql = " WHERE " + " AND ".join(where)
        limit_sql = f" LIMIT {int(limit)}" if limit else ""

        if reuse_staging:
            tables = {r[0] for r in con.execute("SHOW TABLES").fetchall()}
            if "stage" not in tables:
                raise BuildError(
                    f"{staging} has no 'stage' table -- it is not a usable "
                    "staging file. Run without --reuse-staging."
                )
            staged_columns = {
                r[0] for r in con.execute("DESCRIBE stage").fetchall()
            }
            wanted = set(plan.physical)
            if include_sequences:
                wanted.add(S.SEQUENCE_SOURCE_COLUMN)  # output needs it
            missing = sorted(wanted - staged_columns)
            if missing:
                raise BuildError(
                    "The staging file does not carry the columns this build "
                    f"needs ({', '.join(missing)}). It was probably built with "
                    "different options -- run without --reuse-staging."
                )
            _log("  reusing the existing staging file (ingest skipped)")
        else:
            st = _Step("ingest + marker filter")
            # Stage `nuc` whenever the source has it, even for a --no-sequences
            # build. What goes in the OUTPUT is decided later; staging without
            # it would mean a metadata-only build could not hand its staging to
            # a subsequent full build, forcing a second read of the whole
            # source -- which is exactly the workflow --keep-staging exists for.
            con.execute(
                f"CREATE TABLE stage AS\n"
                f"SELECT {staging_select(plan, include_sequences=plan.sequence_present)}\n"
                f"FROM {read_csv_clause(tsv, header)}\n"
                f"{where_sql}{limit_sql}"
            )
            st.done()

        n_stage = con.execute("SELECT count(*) FROM stage").fetchone()[0]
        if n_stage == 0:
            raise BuildError(
                f"No rows survived the filter (marker={marker!r}). Check the "
                "marker code spelling against the package."
            )
        _log(f"  rows kept: {n_stage:,}")

        # ------------------------------------------------------- sorted output
        # ATTACH the final file and write straight into it.  Staging then
        # dropping inside one file would keep the staging bytes forever --
        # VACUUM does not reclaim space in DuckDB.
        con.execute(f"ATTACH {_sql_str(str(out))} AS out")

        phys = plan.physical
        st = _Step("sort + write specimen")
        con.execute(
            f"CREATE TABLE out.specimen AS\n"
            f"SELECT row_number() OVER () - 1 AS sid,\n"
            f"       {', '.join(S.quote_ident(c) for c in phys)}\n"
            f"FROM stage\n"
            f"ORDER BY {', '.join(S.quote_ident(c) for c in S.SPECIMEN_SORT_ORDER if c in phys)}"
        )
        st.done()

        if include_sequences:
            # Sorted the same way specimen is, straight from staging.
            #
            # An earlier version joined out.specimen back to stage and sorted
            # by processid, which put a 20 M-row hash join and a global sort
            # over ~13 GB of sequence strings in flight at once and ran a 32 GB
            # machine out of memory. The join was pure waste -- out.specimen is
            # `SELECT ... FROM stage ORDER BY ...` with no extra WHERE, so it
            # recovered nothing stage did not already have -- and sorting by
            # processid was the wrong key.
            #
            # Removing the sort as well was a step too far. A sequence fetch is
            # driven by a taxonomic result, so the rows it wants are contiguous
            # in the SPECIMEN order and nowhere near each other in ingest
            # order. Storing sequences in ingest order made every fetch scan
            # the whole nuc column: 2.6 s and 4.5 GB of RSS for 183 sequences,
            # measured on the real snapshot. Sorted by the same key as
            # specimen, and paired with the two-phase fetch in iter_sequences,
            # the same fetch is 0.21 s and 271 MB.
            #
            # That leaves one sort of ~13 GB. It is a sort and nothing else,
            # with no join beside it, and memory_limit and temp_directory are
            # both set above, so DuckDB spills rather than dying.
            st = _Step("sort + write sequence")
            con.execute(
                "CREATE TABLE out.sequence AS\n"
                "SELECT processid, nuc FROM stage\n"
                "WHERE nuc IS NOT NULL AND nuc <> ''\n"
                f"ORDER BY {', '.join(S.quote_ident(c) for c in S.SPECIMEN_SORT_ORDER if c in phys)}"
            )
            st.done()
        else:
            con.execute("CREATE TABLE out.sequence (processid VARCHAR, nuc VARCHAR)")

        # --------------------------------------------------------- recordsets
        if plan.recordset_present:
            st = _Step("explode recordsets")
            _build_recordsets(con, allow_drift=allow_recordset_drift)
            st.done()
        else:
            con.execute(
                "CREATE TABLE out.specimen_recordset (recordset_code VARCHAR, sid BIGINT)"
            )

        # -------------------------------------------------------------- taxon
        st = _Step("build taxon lookup")
        _build_taxon(con, phys)
        st.done()

        # -------------------------------------------------------- bin_species
        st = _Step("build bin_species")
        _build_bin_species(con, phys)
        st.done()

        # --------------------------------------------------------------- meta
        meta = {
            "schema_version": S.SCHEMA_VERSION,
            "snapshot_id": snapshot_id,
            "source_file": tsv.name,
            "source_sha256": source_sha,
            "source_bytes": str(tsv.stat().st_size),
            "marker_filter": marker or "",
            "sequences_included": "true" if include_sequences else "false",
            # Which physical order the sequence table is in. "specimen" means a
            # taxonomic result's sequences are contiguous, which is what makes
            # iter_sequences cheap; snapshots built before this, or retrofitted
            # by tools/reorder_sequences.py, say so here.
            "sequence_order": "specimen" if include_sequences else "",
            # A trial build must be identifiable once it is on disk, or a
            # 10,000-row file gets used for real work by mistake.
            "partial_build": "true" if limit else "false",
            "row_limit": str(limit) if limit else "",
            "built_at": _dt.datetime.now(_dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "builder_version": S.SCHEMA_VERSION,
            "duckdb_version": duckdb.__version__,
            "columns": ",".join(S.app_name(c) for c in phys),
        }
        con.execute("CREATE TABLE out._meta (key VARCHAR PRIMARY KEY, value VARCHAR)")
        con.executemany(
            "INSERT INTO out._meta VALUES (?, ?)", list(meta.items())
        )
        for key, sql in (
            ("row_count", "SELECT count(*) FROM out.specimen"),
            ("sequence_count", "SELECT count(*) FROM out.sequence"),
            ("taxon_count", "SELECT count(*) FROM out.taxon"),
            ("bin_count", "SELECT count(DISTINCT bin_uri) FROM out.bin_species"),
            ("recordset_count", "SELECT count(DISTINCT recordset_code) FROM out.specimen_recordset"),
        ):
            value = str(con.execute(sql).fetchone()[0])
            con.execute("INSERT INTO out._meta VALUES (?, ?)", [key, value])
            meta[key] = value

        # A DuckDB file with a leftover .wal beside it CANNOT be opened
        # read-only -- DuckDB cannot replay a WAL without write access, so
        # every reader would refuse it.  DETACH + CHECKPOINT, then verify from
        # a fresh process.
        con.execute("DETACH out")
        con.execute("CHECKPOINT")
    finally:
        con.close()

    # Only now that the build has succeeded is staging disposable -- unless the
    # caller wants to build again from the same ingest, which is the whole
    # point of building metadata-only first and then the full snapshot.
    if keep_staging:
        _log(f"\nStaging kept at {staging} -- reuse it with --reuse-staging")
    else:
        for tmp in (staging, Path(str(staging) + ".wal")):
            if tmp.exists():
                tmp.unlink()

    stray_wal = Path(str(out) + ".wal")
    if stray_wal.exists():
        raise BuildError(
            f"A write-ahead log was left beside the snapshot ({stray_wal}). "
            "The file cannot be opened read-only in this state."
        )

    meta["file_bytes"] = str(out.stat().st_size)
    _log("")
    _log(f"Wrote {out}  ({out.stat().st_size / 1e9:.2f} GB)")
    for key in ("snapshot_id", "row_count", "sequence_count", "taxon_count",
                "bin_count", "recordset_count"):
        _log(f"  {key:16s} {meta.get(key, '')}")
    return meta


def _build_recordsets(con: duckdb.DuckDBPyConnection, *, allow_drift: bool) -> None:
    """Explode ``bold_recordset_code_arr`` (``['AANIC','DS-ANIC2A']``) to rows.

    Asserts the parse, because a silently wrong split produces a table that
    looks fine and matches nothing.
    """
    total, bracketed = con.execute(
        r"""
        SELECT count(*),
               count(*) FILTER (WHERE regexp_matches(trim(bold_recordset_code_arr), '^\[.*\]$'))
        FROM out.specimen
        WHERE bold_recordset_code_arr IS NOT NULL AND bold_recordset_code_arr <> ''
        """
    ).fetchone()

    if total == 0:
        _log("  WARNING: bold_recordset_code_arr is empty for every record")
    else:
        bad = 1.0 - (bracketed / total)
        if bad > 0.01 and not allow_drift:
            raise BuildError(
                f"{bad:.1%} of non-empty bold_recordset_code_arr values are not "
                r"of the expected ['CODE','CODE'] form. The split would produce "
                "a wrong specimen_recordset table. Inspect the column and update "
                "the parser, or pass --allow-recordset-drift to continue anyway."
            )

    strip = (
        r"regexp_replace(raw, '^[\s''\"]+|[\s''\"]+$', '', 'g')"
    )
    con.execute(
        rf"""
        CREATE TABLE out.specimen_recordset AS
        WITH ex AS (
            SELECT sid,
                   unnest(string_split(
                       regexp_replace(bold_recordset_code_arr, '^\s*\[|\]\s*$', '', 'g'),
                       ',')) AS raw
            FROM out.specimen
            WHERE bold_recordset_code_arr IS NOT NULL AND bold_recordset_code_arr <> ''
        )
        SELECT {strip} AS recordset_code, sid
        FROM ex
        WHERE {strip} <> ''
        ORDER BY recordset_code, sid
        """
    )

    distinct = con.execute(
        "SELECT count(DISTINCT recordset_code) FROM out.specimen_recordset"
    ).fetchone()[0]
    if total > 0 and distinct <= 1:
        raise BuildError(
            f"specimen_recordset has {distinct} distinct code(s) from {total:,} "
            "non-empty values -- the split is wrong."
        )
    if total > 0 and not (1_000 <= distinct <= 5_000_000):
        _log(
            f"  WARNING: {distinct:,} distinct recordset codes is outside the "
            "expected range; check the parse before relying on dataset search"
        )

    # mod_data_import_utils.R:50 validates dataset codes against ^DS-[A-Z0-9]+$.
    # If BOLD ships lowercase or hyphenated codes, that validator rejects
    # legitimate input before any query runs -- worth knowing at build time.
    ds_total, ds_ok = con.execute(
        r"""
        SELECT count(*), count(*) FILTER (WHERE regexp_matches(recordset_code, '^DS-[A-Z0-9]+$'))
        FROM (SELECT DISTINCT recordset_code FROM out.specimen_recordset
              WHERE recordset_code LIKE 'DS-%')
        """
    ).fetchone()
    if ds_total and ds_ok < ds_total:
        _log(
            f"  NOTE: {ds_total - ds_ok:,} of {ds_total:,} DS- codes do not match "
            r"^DS-[A-Z0-9]+$ -- the R app's dataset-code validator would reject them"
        )


def _build_taxon(con: duckdb.DuckDBPyConnection, physical: list[str]) -> None:
    """name -> rank lookup.  This is what replaces ``bold.public.search``.

    It makes the size of a result knowable *before* anything is materialised,
    gives a typeahead source, resolves case without a title-casing hack, and
    surfaces ambiguity (a name that is both a genus and a subfamily returns two
    rows) instead of silently OR-ing across columns.
    """
    ranks = [r for r in S.TAXON_RANKS if S.physical_name(r) in physical]
    unpivot = " UNION ALL ".join(
        f"SELECT {_sql_str(rank)} AS taxon_rank, "
        f"{S.quote_ident(S.physical_name(rank))} AS taxon_name FROM out.specimen"
        for rank in ranks
    )
    con.execute(
        f"""
        CREATE TABLE out.taxon AS
        WITH u AS ({unpivot})
        SELECT lower(taxon_name) AS taxon_lc,
               any_value(taxon_name) AS taxon_name,
               taxon_rank,
               count(*) AS n_records
        FROM u
        WHERE taxon_name IS NOT NULL AND taxon_name <> ''
        GROUP BY lower(taxon_name), taxon_rank
        ORDER BY taxon_lc
        """
    )


def _build_bin_species(con: duckdb.DuckDBPyConnection, physical: list[str]) -> None:
    """BIN <-> species counts across every record in the snapshot.

    This is what makes BAGS grade E correct.  ``check_shared_bins``
    (R/utils/bags_grading.R:85-115) can only see BINs among the records the user
    happened to download, so "this BIN is shared with another species" is
    systematically under-detected today.

    No species-level rule is applied here: ``identification_rank`` is carried
    through so the rule lives in one place, ``boldcurator.core.species``.
    """
    has_rank = "identification_rank" in physical
    rank_col = "identification_rank" if has_rank else "NULL"
    con.execute(
        f"""
        CREATE TABLE out.bin_species AS
        SELECT bin_uri,
               species,
               CAST({rank_col} AS VARCHAR) AS identification_rank,
               count(*) AS n_records
        FROM out.specimen
        WHERE bin_uri IS NOT NULL AND bin_uri <> ''
          AND species IS NOT NULL AND species <> ''
        GROUP BY bin_uri, species, CAST({rank_col} AS VARCHAR)
        ORDER BY bin_uri
        """
    )


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(
        prog="boldcurator-build-snapshot",
        description="Build a DuckDB snapshot from the BOLD public data package.",
    )
    p.add_argument("--tsv", required=True, type=Path,
                   help="BOLD_Public.<date>.tsv or .tsv.gz")
    p.add_argument("--out", type=Path,
                   help="snapshot file to create (must not exist). "
                        "Not needed with --dry-run.")
    p.add_argument("--dry-run", action="store_true",
                   help="read only the header and report which columns would "
                        "be kept, which are missing, and whether a build would "
                        "run. Takes seconds even on a 30 GB file -- run this "
                        "first.")
    p.add_argument("--marker", default="COI-5P",
                   help="marker_code to keep (default: COI-5P). "
                        "Pass --marker '' to keep all markers.")
    p.add_argument("--no-sequences", action="store_true",
                   help="build a metadata-only snapshot (no FASTA export)")
    p.add_argument("--memory-limit", default="8GB", help="DuckDB memory_limit (6GB on a 16GB box)")
    p.add_argument("--threads", type=int, default=4, help="DuckDB threads (2 on a 16GB box)")
    p.add_argument("--temp-dir", type=Path, default=None,
                   help="scratch directory on LOCAL disk; expect ~35 GB peak")
    p.add_argument("--max-temp-size", default="80GB")
    p.add_argument("--snapshot-id", default=None, help="default: today's date")
    p.add_argument("--no-hash", action="store_true",
                   help="skip the source sha256 (saves a pass over the file)")
    p.add_argument("--limit", type=int, default=None,
                   help="stop after N rows -- for testing the pipeline only")
    p.add_argument("--allow-recordset-drift", action="store_true",
                   help="continue even if bold_recordset_code_arr is not in the "
                        "expected ['CODE',...] form")
    p.add_argument("--no-progress", action="store_true",
                   help="suppress DuckDB's progress bar")
    p.add_argument("--reuse-staging", action="store_true",
                   help="reuse the staging database a previous run left behind "
                        "instead of re-reading the source. A failed build keeps "
                        "it, so a retry skips the ingest entirely.")
    p.add_argument("--overwrite", action="store_true",
                   help="replace an existing output file (e.g. the partial one "
                        "a failed run left behind)")
    p.add_argument("--staging-path", type=Path, default=None,
                   help="where to put the staging database (default: "
                        "<out>.staging). Set it explicitly to share one ingest "
                        "between builds writing different output files.")
    p.add_argument("--keep-staging", action="store_true",
                   help="keep the staging database after a successful build so "
                        "another build can --reuse-staging. Use it when "
                        "building metadata-only first and the full snapshot "
                        "after, to ingest the source only once.")
    args = p.parse_args(argv)

    if args.dry_run:
        try:
            return describe_columns(args.tsv,
                                    include_sequences=not args.no_sequences)
        except BuildError as exc:
            print(f"\nFAILED: {exc}", file=sys.stderr)
            return 1

    if args.out is None:
        p.error("--out is required unless --dry-run is given")

    started = time.monotonic()
    try:
        build(
            args.tsv,
            args.out,
            marker=args.marker or None,
            include_sequences=not args.no_sequences,
            memory_limit=args.memory_limit,
            threads=args.threads,
            temp_dir=args.temp_dir,
            max_temp_size=args.max_temp_size,
            snapshot_id=args.snapshot_id,
            hash_source=not args.no_hash,
            limit=args.limit,
            allow_recordset_drift=args.allow_recordset_drift,
            progress=not args.no_progress,
            reuse_staging=args.reuse_staging,
            overwrite=args.overwrite,
            keep_staging=args.keep_staging,
            staging_path=args.staging_path,
        )
    except BuildError as exc:
        print(f"\nBUILD FAILED: {exc}", file=sys.stderr)
        return 1
    except Exception as exc:  # noqa: BLE001 - report, then advise on the retry
        staging = args.staging_path or args.out.with_suffix(
            args.out.suffix + ".staging"
        )
        print(f"\nBUILD FAILED: {type(exc).__name__}: {exc}", file=sys.stderr)
        if staging.exists():
            print(
                f"\nThe staging database was kept at:\n  {staging}\n"
                "Retry without re-reading the source:\n"
                f"  --reuse-staging --overwrite",
                file=sys.stderr,
            )
        return 1
    _log(f"\nTotal {time.monotonic() - started:.1f}s")
    if args.limit:
        _log("\nThis is a PARTIAL build (--limit) -- a trial artefact. "
             "Do not use it for curation.")
    _log("\nNow verify it from a fresh process:")
    _log(f"  python tools/verify_snapshot.py --snapshot {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
