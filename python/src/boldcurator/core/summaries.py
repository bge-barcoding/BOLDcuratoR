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
