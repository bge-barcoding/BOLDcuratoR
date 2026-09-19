"""Result-level summaries: the species checklist and the BIN dashboard.

These are **whole-result** aggregates, not pages.  Everything else the app
shows can be answered a page at a time; a checklist cannot, because a species'
specimen count is a fact about every record in the result.  That is why the
screens built on this carry a size cap and the specimen table does not.

``build_species_checklist`` is ``build_species_checklist``
(``mod_species_analysis_utils.R:8-47``) with two changes.  It is vectorised --
R rebuilds a one-row data frame per species and ``rbind``s them, which is
O(species) allocations -- and the species rule is the unified one, so names R
would have kept (``Danaus sp.``) are not counted as species here.
"""

from __future__ import annotations

import pandas as pd

from .frames import distinct_by_group, reindex_counts, reindex_joined
from .species import column_or_missing, to_text

CHECKLIST_COLUMNS = [
    "species", "specimen_count", "bin_count", "bin_uris",
    "bags_grade", "countries", "mean_quality_score",
]

GAP_ANALYSIS_COLUMNS = [
    "input_taxon", "status", "matched_species", "specimen_count", "notes",
]


def build_species_checklist(
    specimens: pd.DataFrame,
    grades: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """One row per species: counts, BINs, grade, countries, mean quality.

    ``grades`` is the frame from :func:`core.bags.calculate_bags_grades`; without
    it the grade column is blank, which is what R does when grading has not run.
    """
    if specimens is None or len(specimens) == 0:
        return pd.DataFrame(columns=CHECKLIST_COLUMNS)

    species = to_text(column_or_missing(specimens, "species")).str.strip()
    keep = (species != "").to_numpy()
    frame = specimens[keep]
    if len(frame) == 0:
        return pd.DataFrame(columns=CHECKLIST_COLUMNS)
    key = species[keep]

    counts = key.groupby(key, sort=True).size()
    index = counts.index

    per_bin = distinct_by_group(
        key, to_text(column_or_missing(frame, "bin_uri")).str.strip())
    per_country = distinct_by_group(
        key, to_text(column_or_missing(frame, "country.ocean")).str.strip())

    quality = pd.to_numeric(column_or_missing(frame, "quality_score"),
                            errors="coerce")
    mean_quality = quality.groupby(key.to_numpy(), sort=True).mean().reindex(index)

    grade_by_species: dict[str, str] = {}
    if grades is not None and len(grades):
        grade_by_species = dict(zip(grades["species"], grades["bags_grade"]))

    return pd.DataFrame(
        {
            "species": index.to_numpy(),
            "specimen_count": counts.to_numpy(),
            "bin_count": reindex_counts(per_bin, index).to_numpy(),
            "bin_uris": reindex_joined(per_bin, index).to_numpy(),
            "bags_grade": [grade_by_species.get(s, "") for s in index],
            "countries": reindex_joined(per_country, index).to_numpy(),
            "mean_quality_score": mean_quality.round(2).to_numpy(),
        },
        columns=CHECKLIST_COLUMNS,
    )


def gap_analysis(
    taxonomy_groups: list[list[str]],
    specimens: pd.DataFrame,
) -> pd.DataFrame:
    """One row per taxon the curator typed: did the search actually find it?

    ``perform_gap_analysis`` (``mod_species_analysis_utils.R:58-104``), with
    the same synonym handling: ``taxonomy_groups`` is one group per line of
    the taxa textarea, first name is the valid one and the rest are synonyms
    (``core.pipeline.parse_taxa_input``). A group matches on **any** name in
    it -- the valid name is tried first, so "matched via synonym" only shows
    when a synonym is what actually found it.

    Species-level lookup only: a curator typed a species (or a synonym of
    one), and this answers "is that name, under any of its synonyms, among
    the species this search actually turned up" -- a family or order in the
    group would never appear as a ``species`` value to match against, which
    mirrors R exactly (it does the same case-insensitive equality against
    ``specimen_data$species`` regardless of rank).

    The R loop is over BOTH input taxa and, for a miss, effectively the whole
    specimen frame (`` == `` scans it per name). Groups are few -- a curator
    types dozens of taxa at most -- so only that loop is Python; the count and
    "first original-case spelling" per species are computed once, vectorised,
    before it.
    """
    labels = [", ".join(group) for group in taxonomy_groups]
    if not taxonomy_groups:
        return pd.DataFrame(columns=GAP_ANALYSIS_COLUMNS)

    if specimens is None or len(specimens) == 0:
        return pd.DataFrame(
            {
                "input_taxon": labels,
                "status": "Missing",
                "matched_species": "",
                "specimen_count": 0,
                "notes": "No specimen data available",
            },
            columns=GAP_ANALYSIS_COLUMNS,
        )

    species = to_text(column_or_missing(specimens, "species")).str.strip()
    named = species[species != ""]
    lowered = named.str.lower()

    counts = lowered.value_counts()
    # R takes the first row's spelling per exact match; groupby here
    # preserves the specimens' own row order, so ".first()" agrees with it.
    first_spelling = named.groupby(lowered).first()

    rows = []
    for group, label in zip(taxonomy_groups, labels):
        status, matched, count, note = "Missing", "", 0, ""
        for name in group:
            name = name.strip()
            if not name:
                continue
            key = name.lower()
            if key in counts.index:
                status = "Found"
                matched = first_spelling[key]
                count = int(counts[key])
                if group and name != group[0]:
                    note = f"Matched via synonym: {name}"
                break
        rows.append((label, status, matched, count, note))

    return pd.DataFrame(rows, columns=GAP_ANALYSIS_COLUMNS)
