"""Automatic selection of representative specimens.

Ported from ``auto_select_best_specimens`` (``app.R:425-467``).  The granularity
is **one representative per (BIN x country)**, not per species -- easy to
misread from the function name.

R re-filters the whole frame once per unique combination, which is O(n x combos).
A single grouped selection does the same work in one pass.
"""

from __future__ import annotations

import datetime as _dt

import pandas as pd

from .species import column_or_missing, is_empty, to_text

UNKNOWN_COUNTRY = "Unknown"


def auto_select_best_specimens(
    specimens: pd.DataFrame,
    *,
    user: str = "auto",
    existing: dict[str, dict] | None = None,
) -> dict[str, dict]:
    """Pick the best specimen per (BIN x country).

    Returns a mapping ``processid -> annotation``.  Returns ``existing``
    untouched when it is non-empty: R only auto-selects on a fresh import, so a
    curator's manual choices are never overwritten.
    """
    if existing:
        return existing
    if specimens is None or len(specimens) == 0:
        return {}

    bin_uri = column_or_missing(specimens, "bin_uri")
    species = column_or_missing(specimens, "species")
    candidates = specimens[~is_empty(bin_uri) & ~is_empty(species)].copy()
    if len(candidates) == 0:
        return {}

    country = column_or_missing(candidates, "country.ocean")
    candidates["_country"] = to_text(country).str.strip().mask(
        is_empty(country), UNKNOWN_COUNTRY
    ).replace("", UNKNOWN_COUNTRY)
    candidates["_bin"] = to_text(candidates["bin_uri"]).str.strip()
    candidates["_score"] = pd.to_numeric(
        column_or_missing(candidates, "quality_score"), errors="coerce"
    ).fillna(0)
    candidates["_pid"] = to_text(column_or_missing(candidates, "processid"))

    # Highest quality score, ties broken by ascending processid.
    best = (
        candidates.sort_values(["_bin", "_country", "_score", "_pid"],
                               ascending=[True, True, False, True])
        .groupby(["_bin", "_country"], sort=False)
        .head(1)
    )

    timestamp = _dt.datetime.now().isoformat(timespec="seconds")
    return {
        row["_pid"]: {
            "timestamp": timestamp,
            "species": to_text(pd.Series([row.get("species")])).iloc[0],
            "quality_score": float(row["_score"]),
            "user": user,
            "selected": True,
            "auto_selected": True,
        }
        for _, row in best.iterrows()
    }
