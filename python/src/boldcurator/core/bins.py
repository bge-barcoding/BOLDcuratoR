"""BIN content and taxonomic concordance.

Ported from ``R/modules/bin_analysis/mod_bin_analysis_utils.R``, with the
species rule unified (see ``core.species``).  R behaviours that change, all
reported by the parity harness:

* the ``cf.``/``aff.`` branch at ``bin_analysis_utils.R:63-70`` is gone.  Its
  constructed pattern ``cf\\.|aff\\.<Reference>`` meant "contains ``cf.``" OR
  "contains ``aff.<Reference>``" by alternation precedence, so every ``cf.``
  record passed it regardless of the reference species.
* ``species_list`` used a looser filter than concordance did (``sp.``/``spp.``
  only, keeping ``cf.``/``aff.``), so the count in the table could disagree with
  the concordance verdict on the same BIN.  One rule now drives both.
* Concordance is the same test grade E uses (``core.bags.discordant_bins``),
  not R's "the first rank with any value decides": a BIN is discordant when it
  holds more than one species-level name (interim species names included, each
  as written -- ``core.species.name_status``) **or** records from more than
  one genus, family or order. R let one species name decide, so a BIN with one
  species and a record identified only to a different genus was concordant.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from .bags import CONFLICT_RANKS
from .frames import distinct_by_group, reindex_counts, reindex_joined
from .species import (
    column_or_missing,
    is_empty_text,
    is_species_level,
    to_text,
)

CONCORDANT = "Concordant"
DISCORDANT = "Discordant"


def _distinct(frame: pd.DataFrame, column: str) -> list[str]:
    values = to_text(column_or_missing(frame, column)).str.strip()
    return sorted({v for v in values if v})


def check_taxonomic_concordance(bin_specimens: pd.DataFrame) -> bool:
    """``True`` when a BIN's records do not conflict taxonomically.

    Discordant when there is more than one species-level name, or more than
    one genus, family or order among the records (whatever rank each record
    was identified to). The single-BIN specification
    :func:`process_bin_content` reproduces, vectorised.
    """
    if bin_specimens is None or len(bin_specimens) == 0:
        return True

    species = to_text(column_or_missing(bin_specimens, "species")).str.strip()
    names = {v for v in species[is_species_level(bin_specimens)] if v}
    if len(names) > 1:
        return False
    return all(len(_distinct(bin_specimens, rank)) <= 1 for rank in CONFLICT_RANKS)


_CONTENT_COLUMNS = ["bin_uri", "total_records", "unique_species", "species_list",
                    "countries", "concordance"]


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

    species = to_text(column_or_missing(frame, "species")).str.strip().where(
        is_species_level(frame), ""
    )

    per_species = distinct_by_group(bin_key, species)
    per_country = distinct_by_group(
        bin_key, to_text(column_or_missing(frame, "country.ocean")).str.strip()
    )
    def _rank_values(rank: str) -> pd.Series:
        # "None"/"NA" are missing, not a genus -- as in
        # bags.taxonomic_conflict_bins, so the two never disagree.
        values = to_text(column_or_missing(frame, rank)).str.strip()
        return values.where(~is_empty_text(values), "")

    per_rank = {rank: distinct_by_group(bin_key, _rank_values(rank))
                for rank in CONFLICT_RANKS}

    counts = bin_key.groupby(bin_key, sort=True).size()
    index = counts.index

    n_species = reindex_counts(per_species, index)
    conflict = n_species > 1
    for rank in CONFLICT_RANKS:
        conflict |= reindex_counts(per_rank[rank], index) > 1
    concordance = np.where(conflict, DISCORDANT, CONCORDANT)

    return pd.DataFrame(
        {
            "bin_uri": index.to_numpy(),
            "total_records": counts.to_numpy(),
            "unique_species": n_species.to_numpy(),
            "species_list": reindex_joined(per_species, index).to_numpy(),
            "countries": reindex_joined(per_country, index).to_numpy(),
            "concordance": concordance,
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
