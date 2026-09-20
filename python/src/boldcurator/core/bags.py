"""BAGS grading (Barcode, Audit & Grade System).

Ported from the **implementation**, ``determine_bags_grade``
(``R/utils/bags_grading.R:123-151``), not from ``BAGS_GRADE_CRITERIA`` in
``constants.R``.  The two disagree: the constant says grade A at 10 specimens,
the code and the R tests both say 11.  The constant is dead -- nothing reads it.

**Grade E is computed differently here, and better.**  R's ``check_shared_bins``
(``bags_grading.R:84-115``) can only see the records the user happened to
download, so "this BIN is shared with another species" is systematically
under-detected.  Given the snapshot's ``bin_species`` table, sharing is
evaluated against every record in the snapshot.  That is a scientific
improvement rather than a speedup, and it means grade E will not match the
Shiny app's; ``shared_bin_scope`` records which was used.

**A BIN-less record is excluded entirely (round 7), unlike R.**  R's own
``calculate_bags_grade`` counts every species-level record toward
``specimen_count`` whether or not it has a BIN yet.  This port matched
that faithfully through round 6 -- until the project owner asked for the
opposite: a record with no BIN cannot be judged on "single BIN, N
specimens" at all, so it should not count toward a grade, and should not
appear in that grade's group table either.  ``calculate_bags_grades``
drops it before counting anything; a species with *no* BIN-assigned
records at all gets no grade rather than one computed from nothing it can
be judged on.  The record itself is untouched -- still in the specimen
table, just outside BAGS.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from .frames import distinct_by_group, reindex_counts, reindex_joined
from .species import column_or_missing, is_empty, is_species_level, to_text

GRADES = ("A", "B", "C", "D", "E")


def determine_grade(
    specimen_count: int | float | None,
    bin_count: int | float | None,
    has_shared_bins: bool | None,
) -> str:
    """The rules, in the order R applies them."""
    try:
        if specimen_count is None or bin_count is None:
            return "E"
        if pd.isna(specimen_count) or pd.isna(bin_count):
            return "E"
        specimen_count = int(specimen_count)
        bin_count = int(bin_count)
    except (TypeError, ValueError):
        return "E"

    if has_shared_bins is None or pd.isna(has_shared_bins):
        has_shared_bins = False

    if has_shared_bins or specimen_count < 0 or bin_count < 0:
        return "E"
    if bin_count > 1:
        return "C"
    if specimen_count < 3:
        return "D"
    if specimen_count >= 11:
        return "A"
    return "B"


def shared_bins(
    frame: pd.DataFrame,
    *,
    species_column: str = "species",
    bin_column: str = "bin_uri",
) -> set[str]:
    """BINs holding more than one distinct species-level name.

    Works for both the local frame and the snapshot's ``bin_species`` table --
    the same species-level rule applies to each.
    """
    if len(frame) == 0:
        return set()
    eligible = frame[is_species_level(frame, species_column=species_column)]
    if len(eligible) == 0:
        return set()
    bins = to_text(column_or_missing(eligible, bin_column)).str.strip()
    species = to_text(column_or_missing(eligible, species_column)).str.strip()
    keep = (bins != "") & (species != "")
    counts = (
        pd.DataFrame({"bin_uri": bins[keep], "species": species[keep]})
        .drop_duplicates()
        .groupby("bin_uri")
        .size()
    )
    return set(counts[counts > 1].index)


def calculate_bags_grades(
    specimens: pd.DataFrame,
    *,
    bin_species: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """One row per species-level species: grade, counts and BIN sharing.

    ``bin_species`` is the snapshot table.  Pass it to evaluate grade E against
    every public record; omit it to reproduce R's local-only behaviour, which
    is what the parity harness does.
    """
    empty = pd.DataFrame(
        columns=["species", "bags_grade", "specimen_count", "bin_count",
                 "shared_bins", "bin_uris", "shared_bin_scope"]
    )
    if specimens is None or len(specimens) == 0:
        return empty

    eligible = specimens[is_species_level(specimens)].copy()
    if len(eligible) == 0:
        return empty

    eligible["_species"] = to_text(eligible["species"]).str.strip()
    bin_text = to_text(column_or_missing(eligible, "bin_uri")).str.strip()
    eligible["_bin"] = bin_text.mask(is_empty(column_or_missing(eligible, "bin_uri")), "")

    # Round 7: a record with no BIN yet cannot be assessed for "single BIN,
    # N specimens" at all, so it must not move a species' grade one way or
    # the other -- drop it before anything is counted. This is a deliberate
    # divergence from the original R app (``calculate_bags_grade``,
    # ``R/utils/bags_grading.R``, counts every species-level record
    # regardless of BIN, and round 6 of this port matched that on purpose)
    # -- the project owner's own explicit call, not a parity target any
    # more for this one behaviour. A species with no BIN-assigned records
    # at all now gets no grade rather than one computed from nothing it can
    # actually be judged on; the records themselves are untouched and still
    # show up in the specimen table, just not in any BAGS grade or group.
    eligible = eligible[eligible["_bin"] != ""]
    if len(eligible) == 0:
        return empty

    scope = "snapshot" if bin_species is not None else "local"
    shared = shared_bins(bin_species if bin_species is not None else eligible)

    # Vectorised for the same reason BIN analysis is: the loop this replaces
    # ran once per species, and a family-sized result holds thousands.
    # Measured on 88,000 rows: 1.1 s at 5,000 species, 2.6 s at 20,000.
    species_key = eligible["_species"]
    per_bin = distinct_by_group(species_key, eligible["_bin"])

    counts = species_key.groupby(species_key, sort=True).size()
    index = counts.index
    bin_count = reindex_counts(per_bin, index)
    bin_uris = reindex_joined(per_bin, index)

    in_shared = eligible["_bin"].isin(shared) & (eligible["_bin"] != "")
    has_shared = (
        in_shared.groupby(species_key, sort=True).any().reindex(index).fillna(False)
    )

    # The grade still comes from determine_grade, and only from there. A
    # second, vectorised copy of the ladder would be a rule in two places --
    # and the parity harness proved the point by catching exactly that.
    # Grading depends on nothing but the (specimens, bins, shared) triple, and
    # thousands of species share a few hundred distinct triples, so one call
    # per distinct triple is both faithful and cheap.
    specimen_count = counts.to_numpy()
    shared_arr = has_shared.to_numpy(dtype=bool)
    bins_arr = bin_count.to_numpy()
    triples = list(zip(specimen_count.tolist(), bins_arr.tolist(),
                       shared_arr.tolist()))
    ladder = {t: determine_grade(*t) for t in set(triples)}
    grade = np.array([ladder[t] for t in triples], dtype=object)

    return pd.DataFrame(
        {
            "species": index.to_numpy(),
            "bags_grade": grade,
            "specimen_count": specimen_count,
            "bin_count": bins_arr,
            "shared_bins": shared_arr,
            "bin_uris": bin_uris.to_numpy(),
            "shared_bin_scope": scope,
        },
        columns=empty.columns,
    )



def grade_lookup(grades: pd.DataFrame) -> dict[str, str]:
    """species -> grade, for joining onto a specimen frame."""
    if grades is None or len(grades) == 0:
        return {}
    return dict(zip(grades["species"], grades["bags_grade"]))
