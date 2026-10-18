"""Export formats.

Ported from the **live** R download handlers in
``mod_specimen_handling_server.R`` and ``mod_data_import_server.R``.

Deliberately *not* ported from ``R/modules/export/mod_export.R``: that
``ExportManager`` is constructed at ``app.R:323`` and never called, so its
21-column list and its five-field FASTA header are not shipped behaviour,
however well tested they are. Same for ``R/modules/export_history/``, which is
never even sourced.

Two R bugs are fixed rather than reproduced:

* R writes every TSV with ``quote = FALSE``, so a curator note containing a tab
  or a newline silently corrupts the file. Python quotes properly.
* ``mod_export.R:229`` whitelists ``"institution"``, but the BCDM column is
  ``inst``, so an ``intersect()`` drops it and the institution is missing from
  every export today. The correct name is used here.
"""

from __future__ import annotations

import csv
import datetime as _dt
import math
import re
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator

import numpy as np
import pandas as pd

from ..config.constants import BOLD_ATTRIBUTION_TEXT
from ..core.species import column_or_missing, is_empty, to_text
from .annotations import Annotations, merge_annotations

#: ``bold_curation_report`` is the one export with a fixed, narrow column list
#: (``mod_specimen_handling_server.R:498-499``). Everything else exports all
#: columns.
CURATION_REPORT_COLUMNS = (
    "sampleid",
    "processid",
    "identification",
    "flag",
    "updated_id",
    "flag_user",
    "curator_notes",
)


def timestamp() -> str:
    """``YYYYmmdd_HHMM``, matching the R filename convention."""
    return _dt.datetime.now().strftime("%Y%m%d_%H%M")


def _provenance(snapshot_id: str) -> list[str]:
    """Header lines stamped onto text exports.

    A curated spreadsheet with no provenance is a data-integrity problem six
    months later, and these scores are **not** comparable with the Shiny app's:
    the maximum is 15 rather than 16 because the image criterion is gone, and
    BAGS grade E is evaluated against the whole snapshot rather than only the
    records that happened to be downloaded.

    The licence line is not decoration: the BOLD data package this snapshot
    is built from is CC BY-SA 4.0, which requires attribution *and* that a
    redistributed or adapted dataset carry the same licence -- this export
    is exactly that redistribution, so it says so on its way out the door,
    not only in the app a curator downloaded it from.
    """
    return [
        f"# BOLDcuratoR (Python) export -- {_dt.datetime.now().isoformat(timespec='seconds')}",
        f"# snapshot: {snapshot_id}",
        "# scoring: 15 criteria, no HAS_IMAGE -- scores are NOT comparable with "
        "the R Shiny app (max 16)",
        "# BAGS grade E evaluated against the full snapshot, not only these records",
        f"# {BOLD_ATTRIBUTION_TEXT}",
    ]


def write_tsv(
    frame: pd.DataFrame,
    path: Path,
    *,
    snapshot_id: str = "",
    provenance: bool = True,
) -> Path:
    """Write a TSV, quoted so embedded tabs and newlines survive."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as fh:
        if provenance:
            for line in _provenance(snapshot_id):
                fh.write(line + "\n")
        frame.to_csv(fh, sep="\t", index=False, quoting=csv.QUOTE_MINIMAL,
                     lineterminator="\n")
    return path


def write_csv(frame: pd.DataFrame, path: Path, *, snapshot_id: str = "") -> Path:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as fh:
        for line in _provenance(snapshot_id):
            fh.write(line + "\n")
        frame.to_csv(fh, index=False, quoting=csv.QUOTE_MINIMAL, lineterminator="\n")
    return path


def write_processid_list(processids: Iterable[str], path: Path) -> Path:
    """One processid per line and nothing else (round 9, item 3): what BOLD
    takes when adding records to a dataset, so no provenance header to
    delete first."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(f"{p}\n" for p in processids), encoding="utf-8")
    return path


#: Excel's own limit on one cell's text.
_XLSX_MAX_CELL = 32_767

#: Characters XML 1.0 can't hold (openpyxl's own list), dropped from text.
_ILLEGAL_XML = re.compile(r"[\000-\010]|[\013-\014]|[\016-\037]")

#: Rows between progress reports and between writes into the zip.
_XLSX_CHUNK_ROWS = 2_000

_XLSX_NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
_XLSX_REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
_XLSX_PKG_REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
_XLSX_TYPE = "application/vnd.openxmlformats-officedocument.spreadsheetml"

#: Style 0 plain, style 1 bold (the header row).
_XLSX_STYLES = (
    f'<styleSheet xmlns="{_XLSX_NS}">'
    '<fonts count="2"><font><sz val="11"/><name val="Calibri"/></font>'
    '<font><b/><sz val="11"/><name val="Calibri"/></font></fonts>'
    '<fills count="2"><fill><patternFill patternType="none"/></fill>'
    '<fill><patternFill patternType="gray125"/></fill></fills>'
    '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/>'
    '</border></borders>'
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" '
    'borderId="0"/></cellStyleXfs>'
    '<cellXfs count="2"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" '
    'xfId="0"/><xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" '
    'applyFont="1"/></cellXfs>'
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/>'
    '</cellStyles></styleSheet>'
)


def _xml_attr(text: str) -> str:
    return (text.replace("&", "&amp;").replace("<", "&lt;")
            .replace(">", "&gt;").replace('"', "&quot;"))


def _xlsx_column_letters(n: int) -> list[str]:
    letters = []
    for i in range(1, n + 1):
        name = ""
        while i:
            i, rem = divmod(i - 1, 26)
            name = chr(65 + rem) + name
        letters.append(name)
    return letters


def _xlsx_cell(ref: str, value, style: str = "") -> str:
    """One ``<c>`` element, or ``""`` for an empty cell.

    Text is an inline string, which a spreadsheet never evaluates: a value
    starting with ``=`` stays text, not a formula (the values come from the
    snapshot and from curators). Control characters a worksheet can't hold
    are dropped, and text is cut at Excel's 32,767-character limit.
    """
    if type(value) is str:                       # most cells: the fast path
        return _xlsx_text(ref, value, style) if value else ""
    if value is None:
        return ""
    if isinstance(value, (bool, np.bool_)):
        return f'<c r="{ref}"{style} t="b"><v>{int(value)}</v></c>'
    if isinstance(value, (int, np.integer)):
        return f'<c r="{ref}"{style}><v>{int(value)}</v></c>'
    if isinstance(value, (float, np.floating)):
        if not math.isfinite(value):
            return ""
        return f'<c r="{ref}"{style}><v>{float(value)!r}</v></c>'
    try:
        if pd.isna(value):
            return ""
    except (TypeError, ValueError):      # a list or array: written as text
        pass
    return _xlsx_text(ref, str(value), style)


def _xlsx_text(ref: str, text: str, style: str) -> str:
    if _ILLEGAL_XML.search(text):
        text = _ILLEGAL_XML.sub("", text)
    text = (text[:_XLSX_MAX_CELL].replace("&", "&amp;").replace("<", "&lt;")
            .replace(">", "&gt;"))
    return (f'<c r="{ref}"{style} t="inlineStr"><is><t xml:space="preserve">'
            f"{text}</t></is></c>")


def _xlsx_sheet(book, number: int, header: list, rows, *, freeze: bool,
                columns: int, total: int = 0, progress=None) -> None:
    """Stream one worksheet's XML into the zip, ``_XLSX_CHUNK_ROWS`` at a time."""
    letters = _xlsx_column_letters(max(columns, 1))
    view = ('<sheetViews><sheetView workbookViewId="0"><pane ySplit="1" '
            'topLeftCell="A2" activePane="bottomLeft" state="frozen"/>'
            '<selection pane="bottomLeft"/></sheetView></sheetViews>'
            if freeze else "")
    with book.open(f"xl/worksheets/sheet{number}.xml", "w",
                   force_zip64=True) as out:
        out.write((f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                   f'<worksheet xmlns="{_XLSX_NS}">{view}<sheetData>').encode())
        if header:
            out.write(('<row r="1">' + "".join(
                _xlsx_cell(f"{c}1", h, ' s="1"') for c, h in zip(letters, header))
                + "</row>").encode())
        first = 2 if header else 1
        chunk: list[str] = []
        done = 0
        for r, row in enumerate(rows, start=first):
            chunk.append(f'<row r="{r}">' + "".join(
                _xlsx_cell(f"{c}{r}", v) for c, v in zip(letters, row)) + "</row>")
            if len(chunk) == _XLSX_CHUNK_ROWS:
                out.write("".join(chunk).encode())
                done += len(chunk)
                chunk = []
                if progress is not None:
                    progress(done, total)
        out.write("".join(chunk).encode())
        if progress is not None:
            progress(done + len(chunk), total)
        out.write(b"</sheetData></worksheet>")


def write_xlsx(frame: pd.DataFrame, path: Path, *, snapshot_id: str = "",
               sheet_name: str = "Records", progress=None) -> Path:
    """One table as a workbook: the rows on the first sheet, header bold and
    frozen, and the provenance ``write_tsv`` puts in its comment lines on an
    "About" sheet (round 9, item 4).

    Round 10, item 3: written directly as spreadsheet XML, streamed into the
    zip a chunk of rows at a time, instead of through openpyxl's cell
    objects -- about ten times faster (85,000 records x 83 columns: 50 s
    with openpyxl, 5 s this way) for the same file. ``progress(rows_done,
    rows_total)`` is called after each chunk.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    sheets = [sheet_name, "About"]
    names = "".join(
        f'<sheet name="{_xml_attr(n)}" sheetId="{i}" r:id="rId{i}"/>'
        for i, n in enumerate(sheets, start=1))
    rels = "".join(
        f'<Relationship Id="rId{i}" Type="{_XLSX_REL_NS}/worksheet" '
        f'Target="worksheets/sheet{i}.xml"/>' for i in range(1, len(sheets) + 1))
    rels += (f'<Relationship Id="rId{len(sheets) + 1}" Type="{_XLSX_REL_NS}/styles" '
             'Target="styles.xml"/>')
    sheet_types = "".join(
        f'<Override PartName="/xl/worksheets/sheet{i}.xml" '
        f'ContentType="{_XLSX_TYPE}.worksheet+xml"/>'
        for i in range(1, len(sheets) + 1))
    head = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
    tmp = path.with_name(path.name + ".part")
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED, compresslevel=1) as book:
        book.writestr("[Content_Types].xml", (
            f'{head}<Types xmlns="http://schemas.openxmlformats.org/package/2006/'
            'content-types"><Default Extension="rels" ContentType="application/'
            'vnd.openxmlformats-package.relationships+xml"/><Default '
            'Extension="xml" ContentType="application/xml"/><Override '
            f'PartName="/xl/workbook.xml" ContentType="{_XLSX_TYPE}.sheet.main+xml"/>'
            f'<Override PartName="/xl/styles.xml" ContentType="{_XLSX_TYPE}.styles+xml"/>'
            f'{sheet_types}</Types>'))
        book.writestr("_rels/.rels", (
            f'{head}<Relationships xmlns="{_XLSX_PKG_REL_NS}"><Relationship '
            f'Id="rId1" Type="{_XLSX_REL_NS}/officeDocument" '
            'Target="xl/workbook.xml"/></Relationships>'))
        book.writestr("xl/workbook.xml", (
            f'{head}<workbook xmlns="{_XLSX_NS}" xmlns:r="{_XLSX_REL_NS}">'
            f"<sheets>{names}</sheets></workbook>"))
        book.writestr("xl/_rels/workbook.xml.rels", (
            f'{head}<Relationships xmlns="{_XLSX_PKG_REL_NS}">{rels}'
            "</Relationships>"))
        book.writestr("xl/styles.xml", head + _XLSX_STYLES)
        _xlsx_sheet(book, 1, [str(c) for c in frame.columns],
                    frame.itertuples(index=False, name=None), freeze=True,
                    columns=frame.shape[1], total=len(frame), progress=progress)
        _xlsx_sheet(book, 2, [], ([line.lstrip("# ")]
                                  for line in _provenance(snapshot_id)),
                    freeze=False, columns=1)
    tmp.replace(path)
    return path


# --------------------------------------------------------------------------
# Row selection
# --------------------------------------------------------------------------


def _subset(frame: pd.DataFrame, processids: Iterable[str]) -> pd.DataFrame:
    wanted = set(processids)
    if "processid" not in frame.columns:
        return frame.iloc[0:0]
    mask = frame["processid"].astype(str).isin(wanted)
    return frame[mask]


def selected_rows(frame: pd.DataFrame, annotations: Annotations) -> pd.DataFrame:
    return _subset(frame, annotations.selected_processids())


def annotated_rows(frame: pd.DataFrame, annotations: Annotations) -> pd.DataFrame:
    return _subset(frame, annotations.annotated_processids())


# --------------------------------------------------------------------------
# FASTA
# --------------------------------------------------------------------------


def fasta_header(row: pd.Series) -> str:
    """``>{processid}|{identification or species or "Unknown"}``.

    Two fields, as all three live R FASTA handlers emit. The five-field header
    in ``mod_export.R`` is unreachable code and is not the shipped format.
    """
    processid = str(row.get("processid", "")).strip()
    for key in ("identification", "species"):
        value = row.get(key)
        if value is not None and not pd.isna(value) and str(value).strip():
            return f">{processid}|{str(value).strip()}"
    return f">{processid}|Unknown"


def write_fasta(
    frame: pd.DataFrame,
    sequences: Iterable[tuple[str, str]],
    path: Path,
    *,
    wrap: int | None = None,
) -> tuple[Path, int]:
    """Stream sequences to a FASTA file at constant memory.

    ``sequences`` is the ``(processid, nuc)`` iterator from
    ``data.queries.iter_sequences``, so a large export costs the same as a small
    one. R's ``download_fasta`` loops over a fully materialised frame instead,
    which is why it is one of the places a big result set hurts.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    if "processid" in frame.columns:
        headers = {
            str(row["processid"]): fasta_header(row)
            for _, row in frame.iterrows()
        }
    else:
        headers = {}

    written = 0
    with path.open("w", encoding="utf-8", newline="") as fh:
        for processid, nuc in sequences:
            if not nuc:
                continue
            header = headers.get(str(processid), f">{processid}|Unknown")
            fh.write(header + "\n")
            if wrap:
                for i in range(0, len(nuc), wrap):
                    fh.write(nuc[i:i + wrap] + "\n")
            else:
                fh.write(nuc + "\n")
            written += 1
    return path, written


# --------------------------------------------------------------------------
# Curation report
# --------------------------------------------------------------------------


def curation_report(frame: pd.DataFrame, annotations: Annotations) -> pd.DataFrame:
    """The narrow report BOLD curators are sent."""
    rows = annotated_rows(frame, annotations)
    merged = merge_annotations(rows, annotations)
    present = [c for c in CURATION_REPORT_COLUMNS if c in merged.columns]
    return merged[present]


# --------------------------------------------------------------------------
# BIN analysis workbook
# --------------------------------------------------------------------------


def write_bin_analysis_xlsx(
    analysis: dict,
    path: Path,
    *,
    snapshot_id: str = "",
) -> Path:
    """Summary / Content / Statistics, as the R BIN download intends.

    R's ``analyze_bin_data`` only ever returns ``content``, so the server's
    references to ``results$summary`` and ``results$stats`` are always NULL and
    two of the three sheets come out empty. ``core.bins.analyse_bins``
    populates the summary, so all three carry data here.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    content = analysis.get("content", pd.DataFrame())
    summary = analysis.get("summary", {})

    summary_frame = pd.DataFrame(
        [{"metric": k, "value": v} for k, v in summary.items()]
        + [{"metric": "snapshot_id", "value": snapshot_id}]
        + [{"metric": "data licence", "value": BOLD_ATTRIBUTION_TEXT}]
    )
    if len(content):
        stats = pd.DataFrame(
            [
                {"metric": "mean records per BIN",
                 "value": round(float(content["total_records"].mean()), 2)},
                {"metric": "max records in one BIN",
                 "value": int(content["total_records"].max())},
                {"metric": "BINs with >1 species",
                 "value": int((content["unique_species"] > 1).sum())},
                {"metric": "mean species per BIN",
                 "value": round(float(content["unique_species"].mean()), 2)},
            ]
        )
    else:
        stats = pd.DataFrame(columns=["metric", "value"])

    with pd.ExcelWriter(path, engine="openpyxl") as writer:
        summary_frame.to_excel(writer, sheet_name="Summary", index=False)
        content.to_excel(writer, sheet_name="Content", index=False)
        stats.to_excel(writer, sheet_name="Statistics", index=False)
    return path


def write_species_analysis_xlsx(
    checklist: pd.DataFrame,
    gaps: pd.DataFrame,
    path: Path,
    *,
    snapshot_id: str = "",
) -> Path:
    """The species checklist and gap analysis, one workbook (plan round 3,

    items 3-4) -- both are whole-result summaries of the Species screen, so
    a curator downloading one is likely to want the other alongside it.
    ``mean_quality_score`` is dropped from the checklist sheet: it is not
    shown on screen any more (round 3, item 4), and a fresh export's column
    set has no download-format compatibility to preserve the way the older,
    already-shipped TSV/BIN exports do.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    checklist_out = checklist.drop(columns=["mean_quality_score"], errors="ignore")
    summary_frame = pd.DataFrame([
        {"metric": "species", "value": len(checklist)},
        {"metric": "taxa typed", "value": len(gaps)},
        {"metric": "snapshot_id", "value": snapshot_id},
        {"metric": "data licence", "value": BOLD_ATTRIBUTION_TEXT},
    ])

    with pd.ExcelWriter(path, engine="openpyxl") as writer:
        summary_frame.to_excel(writer, sheet_name="Summary", index=False)
        checklist_out.to_excel(writer, sheet_name="Species checklist", index=False)
        gaps.to_excel(writer, sheet_name="Gap analysis", index=False)
    return path


# --------------------------------------------------------------------------
# The whole set
# --------------------------------------------------------------------------


@dataclass
class ExportResult:
    written: dict[str, Path]
    skipped: dict[str, str]

    def describe(self) -> str:
        lines = [f"{name}: {path}" for name, path in self.written.items()]
        lines += [f"{name}: skipped -- {why}" for name, why in self.skipped.items()]
        return "\n".join(lines)


def export_all(
    result,
    directory: Path,
    *,
    annotations: Annotations | None = None,
    store=None,
    stamp: str | None = None,
) -> ExportResult:
    """Write every output the Shiny app offers.

    ``store`` is a ``SnapshotStore``; without it the two FASTA outputs are
    skipped rather than silently omitted, because sequences live in the
    snapshot and never in the result frame.

    With no ``annotations``, the pipeline's auto-selections are used. R writes
    those into the same ``selected_specimens`` store that ``download_selected``
    reads (``app.R:456``), so a search with no curation still exports one
    representative per BIN per country.
    """
    if annotations is None:
        annotations = Annotations(selected=dict(getattr(result, "selections", {}) or {}))
    directory = Path(directory)
    stamp = stamp or timestamp()
    snapshot_id = getattr(result, "snapshot_id", "") or ""
    frame = result.specimens

    written: dict[str, Path] = {}
    skipped: dict[str, str] = {}

    merged = merge_annotations(frame, annotations)
    written["all"] = write_tsv(
        merged, directory / f"all_specimens_{stamp}.tsv", snapshot_id=snapshot_id
    )

    selected = merge_annotations(selected_rows(frame, annotations), annotations)
    if len(selected):
        written["selected"] = write_tsv(
            selected, directory / f"selected_specimens_{stamp}.tsv",
            snapshot_id=snapshot_id,
        )
    else:
        skipped["selected"] = "no specimens selected"

    annotated = merge_annotations(annotated_rows(frame, annotations), annotations)
    if len(annotated):
        written["annotated"] = write_tsv(
            annotated, directory / f"annotated_specimens_{stamp}.tsv",
            snapshot_id=snapshot_id,
        )
        written["curation_report"] = write_tsv(
            curation_report(frame, annotations),
            directory / f"bold_curation_report_{stamp}.tsv",
            snapshot_id=snapshot_id,
        )
    else:
        skipped["annotated"] = "no flags, updated IDs or notes recorded"
        skipped["curation_report"] = "no flags, updated IDs or notes recorded"

    written["search_results"] = write_csv(
        frame, directory / f"bold_search_results_{stamp}.csv", snapshot_id=snapshot_id
    )

    if result.bin_analysis and len(result.bin_analysis.get("content", [])):
        written["bin_analysis"] = write_bin_analysis_xlsx(
            result.bin_analysis, directory / f"bin_analysis_{stamp}.xlsx",
            snapshot_id=snapshot_id,
        )
    else:
        skipped["bin_analysis"] = "no BINs in the result"

    if store is None:
        skipped["fasta"] = "no snapshot given, so sequences cannot be fetched"
        skipped["selected_fasta"] = "no snapshot given, so sequences cannot be fetched"
    elif not store.has_sequences:
        skipped["fasta"] = "this snapshot was built without sequences"
        skipped["selected_fasta"] = "this snapshot was built without sequences"
    else:
        from ..data.queries import iter_sequences

        path, n = write_fasta(
            frame,
            iter_sequences(store, [str(p) for p in frame.get("processid", [])]),
            directory / f"specimens_sequences_{stamp}.fasta",
        )
        if n:
            written["fasta"] = path
        else:
            path.unlink(missing_ok=True)
            skipped["fasta"] = "no sequences for these records"

        selected_frame = selected_rows(frame, annotations)
        if len(selected_frame):
            path, n = write_fasta(
                selected_frame,
                iter_sequences(store, [str(p) for p in selected_frame["processid"]]),
                directory / f"selected_sequences_{stamp}.fasta",
            )
            if n:
                written["selected_fasta"] = path
            else:
                path.unlink(missing_ok=True)
                skipped["selected_fasta"] = "no sequences for the selected records"
        else:
            skipped["selected_fasta"] = "no specimens selected"

    return ExportResult(written=written, skipped=skipped)
