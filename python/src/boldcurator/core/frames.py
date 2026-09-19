"""Frame primitives shared by the per-group summaries.

BIN analysis and BAGS grading both reduce to the same question -- *what
distinct values does each group hold?* -- and both originally answered it with
a Python loop over ``groupby``.  That is the R row-loop in a different costume:
it scales with the number of groups, which on real results is thousands.  One
de-duplication and one sort answers it for every group at once.
"""

from __future__ import annotations

import pandas as pd


def distinct_by_group(keys: pd.Series, values: pd.Series) -> pd.DataFrame:
    """Per group: how many distinct non-empty ``values``, and them joined.

    ``keys`` and ``values`` are positionally aligned Series of equal length.
    The result is indexed by group key, sorted, with columns ``n`` and
    ``joined`` -- ``joined`` being ``"; "``-separated values in sorted order,
    which is what both callers' exports print.
    """
    pairs = pd.DataFrame({"_k": keys.to_numpy(), "_v": values.to_numpy()})
    pairs = pairs[pairs["_v"] != ""].drop_duplicates()
    pairs = pairs.sort_values(["_k", "_v"], kind="stable")
    grouped = pairs.groupby("_k", sort=True)["_v"]
    return pd.DataFrame({"n": grouped.size(), "joined": grouped.agg("; ".join)})


def reindex_counts(table: pd.DataFrame, index: pd.Index) -> pd.Series:
    """``distinct_by_group`` counts, as int64, for every key in ``index``.

    Groups whose values were all empty are absent from the table and must come
    back as 0, not as missing.
    """
    return table["n"].reindex(index).fillna(0).astype("int64")


def reindex_joined(table: pd.DataFrame, index: pd.Index) -> pd.Series:
    """``distinct_by_group`` joined strings, as ``""`` where absent."""
    return table["joined"].reindex(index).fillna("")
