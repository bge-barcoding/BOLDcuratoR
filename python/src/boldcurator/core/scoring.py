"""Quality scoring.

Every criterion is worth exactly 1 point, so ``quality_score`` is a count and
``criteria_met`` is the ``"; "``-joined names of the criteria that passed, in
declaration order.  Maximum is 15 -- see ``config.constants`` for why that is
not 16.

**Vectorised on purpose.**  ``SpecimenScorer$score_specimens``
(``specimen_scorer.R:38-52``) loops ``for (i in 1:nrow())`` and extracts
``specimens[i, ]`` each time: about 15 s for 10,000 rows and 75 s for 50,000,
blocking the whole R process.  The criteria are per-row regex and emptiness
tests over whole columns, so they vectorise directly.
"""

from __future__ import annotations

import re

import numpy as np
import pandas as pd

from ..config.constants import (
    CRITERIA_SEPARATOR,
    SPECIMEN_SCORING_CRITERIA,
    Criterion,
)
from .species import (
    column_or_missing,
    is_empty,
    is_empty_text,
    matches_text,
    to_text,
)

#: ``check_type_specimen`` short-circuits on a voucher_type containing "type",
#: independently of the criterion's own positive pattern.
_TYPE_RE = re.compile("type", re.IGNORECASE)


def _field(frame: pd.DataFrame, name: str) -> tuple[pd.Series, pd.Series]:
    """A field as text, with its "has a value" mask.

    Both halves are needed by every check, and computing them together is what
    lets the pattern skip the empty rows -- and stops ``to_text`` running twice
    over the same column, once inside ``is_empty`` and once inside ``matches``.
    """
    text = to_text(column_or_missing(frame, name))
    return text, ~is_empty_text(text)


def _fields(frame: pd.DataFrame, criterion: Criterion
            ) -> list[tuple[pd.Series, pd.Series]]:
    return [_field(frame, name) for name in criterion.fields]


def _check_general(frame: pd.DataFrame, criterion: Criterion) -> pd.Series:
    """``check_general_criterion`` (``specimen_scorer.R:232-251``).

    Each non-empty value contributes: ``False`` if it hits the negative
    pattern, otherwise the positive pattern's result if one exists, otherwise
    ``True``.  The criterion passes if **any** value contributes ``True``.

    The all-fields-empty short-circuit at ``specimen_scorer.R:139`` is implied:
    if every value is empty, every contribution is ``False``.
    """
    neg, pos = criterion.negative_re, criterion.positive_re
    result = pd.Series(False, index=frame.index)
    for text, present in _fields(frame, criterion):
        contributes = present & ~matches_text(text, neg, present)
        if pos is not None:
            contributes &= matches_text(text, pos, present)
        result |= contributes
    return result


def _check_species_id(frame: pd.DataFrame, criterion: Criterion) -> pd.Series:
    """``check_species_id`` -- the first field only, non-empty and clean."""
    text, present = _field(frame, criterion.fields[0])
    return present & ~matches_text(text, criterion.negative_re, present)


def _check_type_specimen(frame: pd.DataFrame, criterion: Criterion) -> pd.Series:
    """``check_type_specimen`` (``specimen_scorer.R:161-175``).

    A ``voucher_type`` containing "type" wins outright, before the positive
    pattern is tried against the five note fields.
    """
    voucher, has_voucher = _field(frame, "voucher_type")
    result = has_voucher & matches_text(voucher, _TYPE_RE, has_voucher)
    pos = criterion.positive_re
    for text, present in _fields(frame, criterion):
        result |= present & matches_text(text, pos, present)
    return result


def _check_sequence_quality(frame: pd.DataFrame, criterion: Criterion) -> pd.Series:
    """``check_sequence_quality`` -- BIN **and** basecount **and** >= min_length.

    All three are required; a long sequence with no BIN does not pass.
    """
    basecount = column_or_missing(frame, "nuc_basecount")
    bin_uri = column_or_missing(frame, "bin_uri")
    numeric = pd.to_numeric(to_text(basecount).str.strip(), errors="coerce")
    minimum = criterion.min_length if criterion.min_length is not None else 0
    return (
        ~is_empty(bin_uri)
        & ~is_empty(basecount)
        & numeric.notna()
        & (numeric >= minimum)
    )


def _check_public_voucher(frame: pd.DataFrame, criterion: Criterion) -> pd.Series:
    """``check_public_voucher`` (``specimen_scorer.R:191-206``) -- positive wins.

    The positive pattern is tested *first*, so a value is accepted if it
    matches, and only otherwise rejected on the negative pattern.  One
    consequence is deliberate and preserved: the positive pattern contains
    ``registered`` and the negative contains ``not registered``, so
    "not registered" matches the positive first and **passes**.
    """
    text, present = _field(frame, criterion.fields[0])
    return present & (
        matches_text(text, criterion.positive_re, present)
        | ~matches_text(text, criterion.negative_re, present)
    )


def _check_id_method(frame: pd.DataFrame, criterion: Criterion) -> pd.Series:
    """``check_id_method`` -- non-empty and free of the negative pattern."""
    result = pd.Series(False, index=frame.index)
    for text, present in _fields(frame, criterion):
        result |= present & ~matches_text(text, criterion.negative_re, present)
    return result


_SPECIAL = {
    "SPECIES_ID": _check_species_id,
    "TYPE_SPECIMEN": _check_type_specimen,
    "SEQ_QUALITY": _check_sequence_quality,
    "PUBLIC_VOUCHER": _check_public_voucher,
    "ID_METHOD": _check_id_method,
}


def criterion_flags(frame: pd.DataFrame) -> pd.DataFrame:
    """One boolean column per criterion, in declaration order."""
    if len(frame) == 0:
        return pd.DataFrame(
            {c.name: pd.Series(dtype=bool) for c in SPECIMEN_SCORING_CRITERIA},
            index=frame.index,
        )
    checks = {}
    for criterion in SPECIMEN_SCORING_CRITERIA:
        check = _SPECIAL.get(criterion.name, _check_general)
        checks[criterion.name] = check(frame, criterion).fillna(False).astype(bool)
    return pd.DataFrame(checks, index=frame.index)


def criteria_met_strings(flags: pd.DataFrame) -> pd.Series:
    """``"; "``-joined criterion names, in declaration order, per row."""
    names = np.array(flags.columns, dtype=object)
    arr = flags.to_numpy(dtype=bool)
    return pd.Series(
        [CRITERIA_SEPARATOR.join(names[row]) for row in arr],
        index=flags.index,
        dtype=object,
    )


def score_specimens(frame: pd.DataFrame) -> pd.DataFrame:
    """Add ``quality_score`` and ``criteria_met``.

    Returns a new frame; the input is not modified.
    """
    flags = criterion_flags(frame)
    out = frame.copy()
    out["quality_score"] = flags.sum(axis=1).astype("int64")
    out["criteria_met"] = criteria_met_strings(flags)
    return out


def flags_from_criteria_met(criteria_met: pd.Series) -> pd.DataFrame:
    """Rebuild the boolean frame from a stored ``criteria_met`` string.

    Needed when re-ranking data loaded from a saved session, where the flags
    themselves were never persisted.
    """
    names = [c.name for c in SPECIMEN_SCORING_CRITERIA]
    text = to_text(criteria_met)
    sets = [
        frozenset(part.strip() for part in value.split(";") if part.strip())
        for value in text
    ]
    return pd.DataFrame(
        {name: [name in s for s in sets] for name in names},
        index=criteria_met.index,
        dtype=bool,
    )
