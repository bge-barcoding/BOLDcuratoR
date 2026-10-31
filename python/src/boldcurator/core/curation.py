"""BAGS grades with the curation applied -- or with the altered records left out.

A search is graded once, from the snapshot as BOLD has it
(:func:`core.pipeline.analyse_plan`). Curation changes what that grading
should see: a misidentified record belongs to another species, a name issue or
synonym renames a whole species, a contaminated record's sequence says
nothing about its voucher. :func:`regrade` re-runs the grading on a copy of
the analysed result in one of two views:

``curated``
    Each record carries its effective new identification
    (``Annotations.resolve``: its own corrected ID, then any taxon update), at
    the rank the curator gave it; contaminated records are left out.

``unaltered``
    Only the records nobody has corrected or flagged: no ID flag, no
    corrected ID, no contamination, not reached by a taxon update. A note or
    an Other issue tick doesn't count -- neither questions the ID.

Scores aren't recomputed. The BAGS grades, the groups built from them and
the BIN dashboard's concordance are.
"""

from __future__ import annotations

import dataclasses

import pandas as pd

from ..config.constants import ID_RANKS
from ..io.annotations import Annotations, guess_rank
from . import bins
from .pipeline import SearchResult, grade_frame
from .species import column_or_missing, name_status, to_text

ORIGINAL = "original"
CURATED = "curated"
UNALTERED = "unaltered"

#: The views a curator can grade by, in the order the switch offers them.
GRADE_VIEWS: dict[str, str] = {
    ORIGINAL: "Original IDs",
    CURATED: "With curated IDs",
    UNALTERED: "Excluding altered records",
}

#: The export column holding each non-original view's grade.
VIEW_GRADE_COLUMNS = {CURATED: "bags_grade_curated",
                      UNALTERED: "bags_grade_unaltered"}

#: The BIN table column holding each non-original view's concordance.
VIEW_CONCORDANCE_COLUMNS = {CURATED: "concordance_curated",
                            UNALTERED: "concordance_unaltered"}

#: The taxonomy columns a new identification sets, highest first: the one for
#: its rank gets the name, every lower one is cleared.
_TAXONOMY = ("kingdom", "phylum", "class", "order", "family", "subfamily",
             "tribe", "genus")
_SPECIES_RANKS = ("species", "subspecies")


def curate_frame(frame: pd.DataFrame, annotations: Annotations,
                 view: str) -> tuple[pd.DataFrame, int, int]:
    """``frame`` as the ``view`` grades it, with how many records it changed
    and how many it left out. ``name_status`` is recomputed, since
    ``is_species_level`` reads the stored column."""
    if view == ORIGINAL or frame is None or len(frame) == 0:
        return frame, 0, 0
    if view not in GRADE_VIEWS:
        raise ValueError(f"Unknown grade view {view!r}")
    pids = [str(p) for p in column_or_missing(frame, "processid")]
    idents = [v if isinstance(v, str) else ""
              for v in column_or_missing(frame, "identification")]
    ann = annotations.to_frame(pids, idents)
    ann.index = frame.index
    contaminated = ann["contamination"].astype(bool)
    updated = ann["update_source"].astype(str) != ""

    if view == UNALTERED:
        altered = (ann["id_flag"].astype(str) != "") | contaminated | updated
        out = frame[~altered.to_numpy()].copy()
        return out, 0, int(altered.sum())

    out = frame[~contaminated.to_numpy()].copy()
    changed = ann.loc[out.index][updated.loc[out.index]]
    for column in ("species", "identification_rank", *_TAXONOMY, "subspecies"):
        if column in out.columns:
            out[column] = out[column].astype(object)
    # One assignment per distinct new identification, not per record: a
    # synonym can reach tens of thousands of records.
    pairs = changed.groupby(["updated_id", "updated_id_rank"]).groups
    for (name, rank), index in pairs.items():
        _reidentify(out, index, str(name), str(rank).strip().lower())
    out["name_status"] = name_status(out)
    return out, len(changed), int(contaminated.sum())


def _reidentify(frame: pd.DataFrame, index, name: str, rank: str) -> None:
    """Give these records a new identification at ``rank``, as BOLD's
    columns would carry it."""
    rank = rank if rank in ID_RANKS else guess_rank(name)
    words = name.split()
    if rank in _SPECIES_RANKS:
        frame.loc[index, "species"] = (name if rank == "species"
                                       else " ".join(words[:2]))
        lowest = "genus"
        value = words[0] if words else pd.NA
    else:
        frame.loc[index, "species"] = pd.NA
        lowest, value = rank, name
    if "subspecies" in frame.columns:
        frame.loc[index, "subspecies"] = name if rank == "subspecies" else pd.NA
    if "identification_rank" in frame.columns:
        frame.loc[index, "identification_rank"] = rank
    if lowest in _TAXONOMY:
        position = _TAXONOMY.index(lowest)
        if lowest in frame.columns:
            frame.loc[index, lowest] = value
        for column in _TAXONOMY[position + 1:]:
            if column in frame.columns:
                frame.loc[index, column] = pd.NA


def _contributions(frame: pd.DataFrame) -> pd.DataFrame:
    """Each record's row in the snapshot's ``bin_species`` count: (BIN,
    species, rank), for records with both a BIN and a species."""
    bins = to_text(column_or_missing(frame, "bin_uri")).str.strip()
    species = to_text(column_or_missing(frame, "species")).str.strip()
    rank = to_text(column_or_missing(frame, "identification_rank")).str.strip()
    keep = (bins != "") & (species != "")
    return pd.DataFrame({"bin_uri": bins[keep], "species": species[keep],
                         "identification_rank": rank[keep]})


def patch_bin_species(bin_species: pd.DataFrame | None, before: pd.DataFrame,
                      after: pd.DataFrame) -> pd.DataFrame | None:
    """The snapshot's species-per-BIN counts with these records' original
    contributions (``before``) taken away and their new ones (``after``)
    added -- so a corrected name stops counting against its BIN, at the same
    snapshot-wide scope the original grades used."""
    if bin_species is None:
        return None
    key = ["bin_uri", "species", "identification_rank"]
    table = bin_species.copy()
    for column in key:
        table[column] = to_text(column_or_missing(table, column)).str.strip()
    delta = pd.concat([
        _contributions(before).assign(n_records=-1),
        _contributions(after).assign(n_records=1),
    ])
    if not len(delta):
        return bin_species
    delta = delta.groupby(key, as_index=False)["n_records"].sum()
    merged = pd.concat([table[key + ["n_records"]], delta]) \
        .groupby(key, as_index=False)["n_records"].sum()
    return merged[merged["n_records"] > 0].reset_index(drop=True)


def regrade(result: SearchResult, annotations: Annotations,
            view: str) -> SearchResult:
    """A copy of ``result`` graded in ``view``. ``curation`` on the copy says
    which view, and how many records it changed or left out."""
    if view == ORIGINAL:
        return result
    original = result.specimens.drop(columns=["bags_grade"], errors="ignore")
    frame, changed, left_out = curate_frame(original, annotations, view)
    moved = original.index.difference(frame.index).union(
        frame.index[_differs(original.loc[frame.index], frame)])
    bin_species = patch_bin_species(result.bin_species, original.loc[moved],
                                    frame.loc[frame.index.intersection(moved)])
    graded, grades, shared = grade_frame(frame, bin_species)
    return dataclasses.replace(
        result, specimens=graded, bags_grades=grades,
        shared_bins=frozenset(shared), bin_species=bin_species,
        bin_analysis=bins.analyse_bins(graded),
        curation={"view": view, "changed": changed, "left_out": left_out})


def _differs(before: pd.DataFrame, after: pd.DataFrame) -> pd.Series:
    """Rows whose species or rank the view changed."""
    out = pd.Series(False, index=after.index)
    for column in ("species", "identification_rank"):
        if column in after.columns:
            out |= (to_text(before[column]).str.strip()
                    != to_text(after[column]).str.strip())
    return out.to_numpy()
