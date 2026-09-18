"""BIN content and taxonomic concordance.

Ported from ``R/modules/bin_analysis/mod_bin_analysis_utils.R``, with the
species rule unified (see ``core.species``).  Two R behaviours therefore change,
both reported by the parity harness:

* the ``cf.``/``aff.`` branch at ``bin_analysis_utils.R:63-70`` is gone.  Its
  constructed pattern ``cf\\.|aff\\.<Reference>`` meant "contains ``cf.``" OR
  "contains ``aff.<Reference>``" by alternation precedence, so every ``cf.``
  record passed it regardless of the reference species.
* ``species_list`` used a looser filter than concordance did (``sp.``/``spp.``
  only, keeping ``cf.``/``aff.``), so the count in the table could disagree with
  the concordance verdict on the same BIN.  One rule now drives both.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from .frames import distinct_by_group, reindex_counts, reindex_joined
from .species import (
    column_or_missing,
    is_valid_species_name,
    to_text,
)

CONCORDANT = "Concordant"
DISCORDANT = "Discordant"


def _distinct(frame: pd.DataFrame, column: str) -> list[str]:
    values = to_text(column_or_missing(frame, column)).str.strip()
    return sorted({v for v in values if v})


def check_taxonomic_concordance(bin_specimens: pd.DataFrame) -> bool:
    """``True`` when a BIN's records do not conflict taxonomically.

    Hierarchical fallback: more than one valid species name is discordant;
    with no valid species name, fall back to genus, then family, then order,
    each "more than one distinct value is discordant".
    """
    if bin_specimens is None or len(bin_specimens) == 0:
        return True

    species = column_or_missing(bin_specimens, "species")
    valid = to_text(species).str.strip()[is_valid_species_name(species)]
    valid_species = sorted({v for v in valid if v})

    if len(valid_species) > 1:
        return False
    if len(valid_species) == 1:
        return True

    for rank in ("genus", "family", "order"):
        values = _distinct(bin_specimens, rank)
        if len(values) > 1:
            return False
        if len(values) == 1:
            return True
    return True


_CONTENT_COLUMNS = ["bin_uri", "total_records", "unique_species", "species_list",
                    "countries", "concordance", "bin_coverage"]


def process_bin_content(specimens: pd.DataFrame) -> pd.DataFrame:
    """One row per BIN: counts, species list, countries, concordance.

    **Vectorised.**  The obvious shape -- ``for bin_uri, group in
    frame.groupby(...)`` with ``check_taxonomic_concordance(group)`` inside --
    runs four distinct-value passes over a small frame per BIN, so an 88,000-row
    result with ~5,000 BINs cost 8.1 s, more than the search that produced it.
    Every quantity it needs is a distinct-count per BIN, so one pass per rank
    computes them all.  ``check_taxonomic_concordance`` is kept for a single
    BIN's records and is the specification this reproduces.
    """
    if specimens is None or len(specimens) == 0:
        return pd.DataFrame(columns=_CONTENT_COLUMNS)

    bins = to_text(column_or_missing(specimens, "bin_uri")).str.strip()
    keep = (bins != "").to_numpy()
    frame = specimens[keep]
    bin_key = bins[keep]
    if len(frame) == 0:
        return pd.DataFrame(columns=_CONTENT_COLUMNS)

    total = len(specimens)
    species_raw = column_or_missing(frame, "species")
    species = to_text(species_raw).str.strip().where(
        is_valid_species_name(species_raw), ""
    )

    per_species = distinct_by_group(bin_key, species)
    per_country = distinct_by_group(
        bin_key, to_text(column_or_missing(frame, "country.ocean")).str.strip()
    )
    per_rank = {
        rank: distinct_by_group(
            bin_key, to_text(column_or_missing(frame, rank)).str.strip()
        )
        for rank in ("genus", "family", "order")
    }

    counts = bin_key.groupby(bin_key, sort=True).size()
    index = counts.index

    n_species = reindex_counts(per_species, index)
    n_genus, n_family, n_order = (
        reindex_counts(per_rank[r], index) for r in ("genus", "family", "order")
    )

    # The hierarchical fallback, as a single selection: the first rank with any
    # distinct value decides, and more than one value there is discordant.
    decided_by = np.select(
        [n_species > 0, n_genus > 0, n_family > 0],
        [n_species, n_genus, n_family],
        default=n_order,
    )
    concordance = np.where(decided_by > 1, DISCORDANT, CONCORDANT)

    return pd.DataFrame(
        {
            "bin_uri": index.to_numpy(),
            "total_records": counts.to_numpy(),
            "unique_species": n_species.to_numpy(),
            "species_list": reindex_joined(per_species, index).to_numpy(),
            "countries": reindex_joined(per_country, index).to_numpy(),
            "concordance": concordance,
            "bin_coverage": counts.to_numpy() / total if total else 0.0,
        },
        columns=_CONTENT_COLUMNS,
    )


def analyse_bins(specimens: pd.DataFrame) -> dict[str, object]:
    """BIN analysis payload: the content table plus the summary counters.

    R's ``analyze_bin_data`` returns only ``content`` while its server reads
    ``results$summary`` and ``results$stats``, which are therefore always NULL
    and the BIN download is dead. Both are populated here.
    """
    content = process_bin_content(specimens)
    return {
        "content": content,
        "summary": {
            "total_bins": int(len(content)),
            "concordant_bins": int((content["concordance"] == CONCORDANT).sum())
            if len(content) else 0,
            "discordant_bins": int((content["concordance"] == DISCORDANT).sum())
            if len(content) else 0,
            "shared_bins": int((content["unique_species"] > 1).sum())
            if len(content) else 0,
        },
    }
