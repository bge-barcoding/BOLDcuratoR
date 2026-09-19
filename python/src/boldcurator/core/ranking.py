"""The rank ladder.

Ranks are evaluated **in order 1 -> 6 and the first match wins**
(``specimen_processor.R:182-227``); anything unmatched is rank 7.  Rank 1 is
therefore not a superset of rank 2 -- a type specimen with a clean species name
is rank 1 even if it lacks everything else.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from ..config.constants import DEFAULT_RANK, SPECIMEN_RANK_CRITERIA


def rank_from_flags(flags: pd.DataFrame) -> pd.Series:
    """Assign a rank per row from the per-criterion boolean frame."""
    if len(flags) == 0:
        return pd.Series(dtype="int64", index=flags.index)

    conditions = []
    for requirements in SPECIMEN_RANK_CRITERIA:
        satisfied = np.ones(len(flags), dtype=bool)
        for alternatives in requirements:
            # A requirement is satisfied by ANY of its alternatives; a
            # one-element tuple is a plain AND condition.
            any_of = np.zeros(len(flags), dtype=bool)
            for name in alternatives:
                if name in flags.columns:
                    any_of |= flags[name].to_numpy(dtype=bool)
            satisfied &= any_of
        conditions.append(satisfied)

    ranks = np.arange(1, len(SPECIMEN_RANK_CRITERIA) + 1)
    return pd.Series(
        np.select(conditions, ranks, default=DEFAULT_RANK).astype("int64"),
        index=flags.index,
    )


def rank_specimens(frame: pd.DataFrame, flags: pd.DataFrame | None = None) -> pd.DataFrame:
    """Add a ``rank`` column.  Returns a new frame."""
    if flags is None:
        from .scoring import criterion_flags, flags_from_criteria_met

        if "criteria_met" in frame.columns:
            flags = flags_from_criteria_met(frame["criteria_met"])
        else:
            flags = criterion_flags(frame)
    out = frame.copy()
    out["rank"] = rank_from_flags(flags)
    return out


def score_and_rank(frame: pd.DataFrame) -> pd.DataFrame:
    """Score and rank in one pass, reusing the flags between the two steps."""
    from .scoring import criteria_met_strings, criterion_flags

    flags = criterion_flags(frame)
    out = frame.copy()
    out["quality_score"] = flags.sum(axis=1).astype("int64")
    out["criteria_met"] = criteria_met_strings(flags)
    out["rank"] = rank_from_flags(flags)
    return out
