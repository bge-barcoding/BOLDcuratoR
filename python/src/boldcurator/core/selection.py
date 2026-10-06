"""Automatic selection of representative specimens.

Ported from ``auto_select_best_specimens`` (``app.R:425-467``).  The granularity
is **one representative per (BIN x country)**, not per species -- easy to
misread from the function name.

R re-filters the whole frame once per unique combination, which is O(n x combos).
A single grouped selection does the same work in one pass.

Two deliberate differences from R, both so that **every** BIN in a result gets
a representative (the Phylogeny tab and "Download Selected" are built from
this set, so a BIN without one silently vanishes from both):

- A record needs a BIN, not a species name. R also required ``species``, so a
  BIN whose records are identified only to genus or family got no
  representative at all.
- ``fill_gaps``: with a non-empty ``existing`` selection, R's rule leaves the
  whole result alone. That starved a broader second search (Pieris, then
  Pieridae) of representatives for every BIN the first one had not covered.
  With ``fill_gaps`` only the (BIN x country) groups that hold no selected
  record are filled; an existing pick is still never replaced.
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
    fill_gaps: bool = False,
) -> dict[str, dict]:
    """Pick the best specimen per (BIN x country).

    Returns a mapping ``processid -> annotation``.  With a non-empty
    ``existing`` and ``fill_gaps`` off, returns ``existing`` untouched: R only
    auto-selects on a fresh import, so a curator's manual choices are never
    overwritten. With ``fill_gaps`` on, returns ``existing`` plus a pick for
    each group none of whose records is in ``existing`` (see the module
    docstring); ``existing`` itself is returned when there is nothing to add.
    """
    if existing and not fill_gaps:
        return existing
    if specimens is None or len(specimens) == 0:
        return existing or {}

    bin_uri = column_or_missing(specimens, "bin_uri")
    candidates = specimens[~is_empty(bin_uri)].copy()
    if len(candidates) == 0:
        return existing or {}

    country = column_or_missing(candidates, "country.ocean")
    candidates["_country"] = to_text(country).str.strip().mask(
        is_empty(country), UNKNOWN_COUNTRY
    ).replace("", UNKNOWN_COUNTRY)
    candidates["_bin"] = to_text(candidates["bin_uri"]).str.strip()
    candidates["_score"] = pd.to_numeric(
        column_or_missing(candidates, "quality_score"), errors="coerce"
    ).fillna(0)
    candidates["_pid"] = to_text(column_or_missing(candidates, "processid"))

    if existing:
        selected = candidates["_pid"].isin(set(existing))
        covered = set(zip(candidates.loc[selected, "_bin"],
                          candidates.loc[selected, "_country"]))
        open_group = [key not in covered
                      for key in zip(candidates["_bin"], candidates["_country"])]
        candidates = candidates[open_group]
        if len(candidates) == 0:
            return existing

    # Highest quality score, ties broken by ascending processid.
    best = (
        candidates.sort_values(["_bin", "_country", "_score", "_pid"],
                               ascending=[True, True, False, True])
        .groupby(["_bin", "_country"], sort=False)
        .head(1)
    )

    timestamp = _dt.datetime.now().isoformat(timespec="seconds")
    picks = {
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
    return {**(existing or {}), **picks}
