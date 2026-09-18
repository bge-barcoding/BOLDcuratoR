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

import pandas as pd

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


def process_bin_content(specimens: pd.DataFrame) -> pd.DataFrame:
    """One row per BIN: counts, species list, countries, concordance."""
    columns = ["bin_uri", "total_records", "unique_species", "species_list",
               "countries", "concordance", "bin_coverage"]
    if specimens is None or len(specimens) == 0:
        return pd.DataFrame(columns=columns)

    bins = to_text(column_or_missing(specimens, "bin_uri")).str.strip()
    frame = specimens[bins != ""].copy()
    frame["_bin"] = bins[bins != ""]
    if len(frame) == 0:
        return pd.DataFrame(columns=columns)

    total = len(specimens)
    rows = []
    for bin_uri, group in frame.groupby("_bin", sort=True):
        species = column_or_missing(group, "species")
        names = sorted(
            {v for v in to_text(species).str.strip()[is_valid_species_name(species)] if v}
        )
        countries = _distinct(group, "country.ocean")
        rows.append(
            {
                "bin_uri": bin_uri,
                "total_records": len(group),
                "unique_species": len(names),
                "species_list": "; ".join(names),
                "countries": "; ".join(countries),
                "concordance": CONCORDANT if check_taxonomic_concordance(group)
                else DISCORDANT,
                "bin_coverage": len(group) / total if total else 0.0,
            }
        )
    return pd.DataFrame(rows, columns=columns)


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
