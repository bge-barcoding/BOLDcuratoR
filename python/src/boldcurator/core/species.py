"""Species-name handling, and the emptiness test the whole scorer rests on.

**The unified rule.**  The R codebase applies five *different* regexes for
"invalid species name", none identical:

===================================================  ======================================================  ==========
Location                                             Pattern                                                 Ignore case
===================================================  ======================================================  ==========
``constants.R:52`` (``SPECIES_ID``)                  ``sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.| nr ``           yes
``specimen_validator.R:32`` (``valid_species``)      ``sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.``                no
``mod_data_import_utils.R:180`` (destructive NA)     ``^sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.``               no
``bin_analysis_utils.R:52`` (concordance)            ``sp\\.|spp\\.|cf\\.|aff\\.``                           no
``bin_analysis_utils.R:114`` (species list)          ``sp\\.|spp\\.``                                        no
===================================================  ======================================================  ==========

This module defines **one** rule, used everywhere, taken from the most complete
and most carefully specified of the five (``SPECIES_ID``).

Two consequences are worth stating plainly, because the parity harness will
report them and they are not bugs in this code:

1. ``"Danaus sp."`` is **invalid here but valid in R's BAGS grading**.  R's
   destructive pass anchors the pattern as ``^sp\\.``, which only matches a name
   *beginning* ``sp.`` -- so ``"Danaus sp."`` survives it and is then counted by
   ``calculate_bags_grade`` as a species in its own right.  That is a bug in the
   anchor, and unifying removes it.
2. The ``cf.``/``aff.`` branch of ``check_taxonomic_concordance``
   (``bin_analysis_utils.R:63-70``) disappears.  It built the pattern
   ``cf\\.|aff\\.<Reference>``, which by alternation precedence means "contains
   ``cf.``" OR "contains ``aff.<Reference>``" -- so *any* ``cf.`` record passed.
   Under one rule those records are simply not species-level.
"""

from __future__ import annotations

import re

import numpy as np
import pandas as pd

#: The unified invalid-species-name pattern.  Case-insensitive, as
#: ``SPECIES_ID`` is in R.
INVALID_SPECIES_PATTERN = r"sp\.|spp\.|[0-9]|^sp$|aff\.|cf\.| nr "
_INVALID_SPECIES_RE = re.compile(INVALID_SPECIES_PATTERN, re.IGNORECASE)

#: A name must look like a binomial to be species-level.  Ported unchanged from
#: ``bags_grading.R:15``.
BINOMIAL_PATTERN = r"^\S+\s+\S+"
_BINOMIAL_RE = re.compile(BINOMIAL_PATTERN)

#: Identification ranks that count as species-level, where the column exists.
SPECIES_LEVEL_RANKS = frozenset({"species", "subspecies"})

#: Strings that count as missing in addition to NA and "".  From
#: ``is_empty_value`` (``specimen_scorer.R:77-81``), which upper-cases after
#: trimming -- so "none", "None", "na" and "NA" are all missing.
EMPTY_TOKENS = frozenset({"NONE", "NA"})


def to_text(values: pd.Series) -> pd.Series:
    """Coerce any column to a plain string Series, with missing as ``""``.

    Mirrors R's ``as.character()`` for the types that actually occur, without
    turning missing values into the string ``"nan"``.
    """
    if values.dtype == object or isinstance(values.dtype, pd.StringDtype):
        out = values.astype(object).where(values.notna(), "")
        return pd.Series([x if isinstance(x, str) else ("" if x is None else str(x))
                          for x in out], index=values.index, dtype=object)
    if pd.api.types.is_float_dtype(values.dtype):
        # 658.0 must render as "658", the way R's as.character does for a
        # whole number, or a pattern anchored with ^...$ would stop matching.
        def _fmt(x: object) -> str:
            if pd.isna(x):
                return ""
            f = float(x)  # type: ignore[arg-type]
            return str(int(f)) if f.is_integer() else str(f)

        return pd.Series([_fmt(x) for x in values], index=values.index, dtype=object)
    return values.astype(object).where(values.notna(), "").astype(str)


def is_empty(values: pd.Series) -> pd.Series:
    """Vectorised ``is_empty_value`` (``specimen_scorer.R:77-81``)."""
    text = to_text(values).str.strip()
    return (text == "") | text.str.upper().isin(EMPTY_TOKENS)


def column_or_missing(frame: pd.DataFrame, name: str) -> pd.Series:
    """Return a column, or an all-missing Series if the frame lacks it.

    R's scorer treats an absent column as ``NA`` for every row rather than
    erroring.  Keeping that behaviour matters for snapshots built without an
    optional column.
    """
    if name in frame.columns:
        return frame[name]
    return pd.Series([np.nan] * len(frame), index=frame.index, dtype=object)


def matches(values: pd.Series, pattern: re.Pattern[str] | None) -> pd.Series:
    """Vectorised ``grepl``: ``False`` where the value is missing."""
    if pattern is None:
        return pd.Series(False, index=values.index)
    text = to_text(values)
    return pd.Series(
        [bool(pattern.search(v)) for v in text], index=values.index, dtype=bool
    )


def is_valid_species_name(values: pd.Series) -> pd.Series:
    """The single rule: non-empty and free of the invalid-name markers."""
    return ~is_empty(values) & ~matches(values, _INVALID_SPECIES_RE)


def is_binomial(values: pd.Series) -> pd.Series:
    """At least two whitespace-separated tokens (``bags_grading.R:15``)."""
    return matches(values, _BINOMIAL_RE)


def is_species_level(
    frame: pd.DataFrame,
    *,
    species_column: str = "species",
    rank_column: str = "identification_rank",
) -> pd.Series:
    """Rows usable as a species-level identification.

    Requires a valid binomial species name and, **where the column exists**, an
    ``identification_rank`` of species or subspecies.  R tolerates the column's
    absence in ``calculate_bags_grade`` but requires it in the grade tabs; the
    tolerant reading is used here so a snapshot without the column still grades.
    """
    species = column_or_missing(frame, species_column)
    ok = is_valid_species_name(species) & is_binomial(species)
    if rank_column in frame.columns:
        rank = to_text(frame[rank_column]).str.strip().str.lower()
        # A missing rank is not evidence against; only a present, non-species
        # rank disqualifies.
        present = ~is_empty(frame[rank_column])
        ok = ok & (~present | rank.isin(SPECIES_LEVEL_RANKS))
    return ok


def normalise_species(values: pd.Series) -> pd.Series:
    """Trim, and blank out invalid names -- R's destructive pass, unified.

    ``process_specimen_data`` (``mod_data_import_utils.R:152-214``) does this to
    ``species`` before anything downstream sees it, so an invalid name cannot be
    counted as a species by BAGS or by auto-selection.
    """
    text = to_text(values).str.strip()
    # The full emptiness test, not a bare == "" -- otherwise the literal
    # strings "None" and "NA" survive as species names and are counted by BAGS
    # as species in their own right, which is what R does today.
    invalid = is_empty(values) | matches(text, _INVALID_SPECIES_RE)
    return text.mask(invalid, pd.NA)
