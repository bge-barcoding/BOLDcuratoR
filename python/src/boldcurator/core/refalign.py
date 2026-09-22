"""Anchor every representative sequence to one in-data reference, so the
Phylogeny tab's distances compare the same COI positions.

Why this exists: a k-mer profile of a sequence depends on *which part* of
COI it covers, not only on how divergent it is, so an overhanging long read
and a short partial read of the same species end up looking unrelated, and
two short reads of different species covering the same stretch end up
looking close. Comparing position-by-position fixes that, and it needs the
sequences in a shared coordinate frame.

That frame comes from **one** reference, chosen from the representatives
themselves (the one closest to the Folmer region's 658 bp, preferring clean
ACGT). Each sequence is aligned to it once -- O(n) alignments, not the
O(n^2) all-vs-all a real multiple alignment would need -- with free end gaps
on both sides (semi-global), so overhangs and uncovered reference ends cost
nothing. The alignment is then *kept*, not just used to trim: the query's
bases are written into reference coordinates, giving a reference-anchored
alignment that :func:`core.phylogeny.k2p_distance_matrix` compares site by
site over whatever each pair actually shares.

Insertions in a query relative to the reference are dropped (they have no
reference column; COI-5P indels are rare and codon-sized). A sequence that
aligns badly is **kept and flagged**, not dropped -- see
:class:`AnchoredSequence` -- so the tree shows it, marked, and a curator can
decide.

Pure Python + Biopython's C ``PairwiseAligner``; ~5 ms per 658 bp alignment,
so the 400-tip cap (config.constants.PHYLOGENY_LIMITS) costs ~2 s here,
well under the NJ step it feeds.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable

import numpy as np

#: Row code for a site with no comparable base: outside the aligned span, an
#: ambiguity code, or a reference position the query has a deletion at.
MISSING = 4

_IUPAC = set("ACGTRYSWKMBDHVN")
_GAP_CHARS = set("-. \t\r\n")
_COMPLEMENT = str.maketrans("ACGTRYSWKMBDHVN", "TGCAYRSWMKVHDBN")

#: byte -> row code (A=0, C=1, G=2, T=3, anything else MISSING).
_CODE_TABLE = np.full(256, MISSING, dtype=np.uint8)
for _code, _base in enumerate("ACGT"):
    _CODE_TABLE[ord(_base)] = _code


def clean_sequence(seq: str | None) -> str:
    """Upper-case, drop gap characters (BOLD ``nuc`` can carry ``-``, which
    the aligner's NUC.4.4 alphabet has no letter for), read ``U`` as ``T``,
    and turn anything else outside IUPAC into ``N``."""
    out = []
    for ch in (seq or "").upper():
        if ch in _GAP_CHARS:
            continue
        if ch == "U":
            ch = "T"
        out.append(ch if ch in _IUPAC else "N")
    return "".join(out)


def encode(seq: str) -> np.ndarray:
    """Row codes for an already-cleaned sequence."""
    return _CODE_TABLE[np.frombuffer(seq.encode("ascii"), dtype=np.uint8)]


def reverse_complement(seq: str) -> str:
    return seq.translate(_COMPLEMENT)[::-1]


def _ambiguous_count(seq: str) -> int:
    return sum(1 for ch in seq if ch not in "ACGT")


def select_reference(sequences: dict[str, str], target_length: int) -> str:
    """Name of the sequence to anchor everything else to: closest in length
    to ``target_length``, among those with at most 1% non-ACGT (falling back
    to all of them if none qualify), ties broken by fewest non-ACGT, then
    insertion order. ``sequences`` must already be cleaned."""
    if not sequences:
        raise ValueError("No sequences to select a reference from.")
    items = list(sequences.items())
    clean = [(name, seq) for name, seq in items
             if seq and _ambiguous_count(seq) <= 0.01 * len(seq)]
    pool = clean or items
    order = {name: i for i, (name, _) in enumerate(items)}
    best = min(pool, key=lambda item: (abs(len(item[1]) - target_length),
                                       _ambiguous_count(item[1]),
                                       order[item[0]]))
    return best[0]


def build_aligner():
    """Semi-global aligner: global mode, NUC.4.4, internal gaps cost
    (open -10, extend -0.5), end gaps on either sequence free.

    The end-gap score is set *last* on purpose: Biopython's
    ``open_gap_score``/``extend_gap_score`` setters overwrite the end-gap
    scores too, so setting them after ``end_gap_score`` silently turns the
    free end gaps back into -10/-0.5 ones.
    """
    from Bio.Align import PairwiseAligner, substitution_matrices

    aligner = PairwiseAligner()
    aligner.mode = "global"
    aligner.substitution_matrix = substitution_matrices.load("NUC.4.4")
    aligner.open_gap_score = -10
    aligner.extend_gap_score = -0.5
    aligner.end_gap_score = 0.0
    return aligner


@dataclass
class AnchoredSequence:
    name: str
    #: One code per reference position (A=0 C=1 G=2 T=3, MISSING=4).
    row: np.ndarray
    #: Reference span the alignment covers, 0-based, end exclusive.
    ref_start: int = 0
    ref_end: int = 0
    #: Matching bases / reference span covered -- the script-era definition,
    #: which counts the mismatched and skipped positions between aligned
    #: blocks against the score, so scattered chance matches from an
    #: unrelated sequence don't read as a good alignment.
    identity: float = 0.0
    #: Reference span covered / reference length.
    coverage: float = 0.0
    reverse_complemented: bool = False
    #: Human-readable reasons this sequence's placement deserves a second
    #: look. Empty for a clean alignment.
    flags: list[str] = field(default_factory=list)


def _anchor_once(seq: str, ref_codes: np.ndarray, ref: str, aligner
                 ) -> tuple[np.ndarray, int, int, float]:
    row = np.full(len(ref), MISSING, dtype=np.uint8)
    if not seq:
        return row, 0, 0, 0.0
    alignment = aligner.align(ref, seq)[0]
    ref_blocks, query_blocks = alignment.aligned
    if len(ref_blocks) == 0:
        return row, 0, 0, 0.0
    query_codes = encode(seq)
    matches = 0
    for (r_start, r_end), (q_start, q_end) in zip(ref_blocks, query_blocks):
        block = query_codes[q_start:q_end]
        row[r_start:r_end] = block
        matches += int(np.count_nonzero((block == ref_codes[r_start:r_end])
                                        & (block != MISSING)))
    ref_start, ref_end = int(ref_blocks[0][0]), int(ref_blocks[-1][1])
    span = ref_end - ref_start
    return row, ref_start, ref_end, (matches / span if span else 0.0)


def anchor_to_reference(name: str, seq: str, ref: str, aligner, *,
                        min_identity: float, min_coverage: float,
                        ref_codes: np.ndarray | None = None) -> AnchoredSequence:
    """Align ``seq`` (cleaned) to ``ref`` and lay its bases out in reference
    coordinates. A forward alignment below ``min_identity`` is retried as the
    reverse complement, which is kept only if it clears ``min_identity`` --
    an unrelated sequence scores ~50% either way, and "flipped" would be a
    misleading thing to say about it."""
    if ref_codes is None:
        ref_codes = encode(ref)
    row, start, end, identity = _anchor_once(seq, ref_codes, ref, aligner)
    flipped = False
    if seq and identity < min_identity:
        rc = _anchor_once(reverse_complement(seq), ref_codes, ref, aligner)
        if rc[3] >= min_identity:
            row, start, end, identity = rc
            flipped = True

    coverage = (end - start) / len(ref) if len(ref) else 0.0
    flags = []
    if not seq:
        flags.append("no alignable bases")
    else:
        if identity < min_identity:
            flags.append(f"low identity to the reference ({identity:.0%})")
        if flipped:
            flags.append("reverse-complemented to match the reference")
        if coverage < min_coverage:
            flags.append(f"short: spans {end - start} of {len(ref)} reference bp")
    return AnchoredSequence(name=name, row=row, ref_start=start, ref_end=end,
                            identity=identity, coverage=coverage,
                            reverse_complemented=flipped, flags=flags)


def anchor_all(
    sequences: dict[str, str],
    *,
    target_length: int,
    min_identity: float,
    min_coverage: float,
    progress: "Callable[[str], None] | None" = None,
) -> tuple[str, list[AnchoredSequence]]:
    """Clean every sequence, pick the reference, anchor everything to it.

    Returns ``(reference_name, anchored)``, ``anchored`` in the insertion
    order of ``sequences``. The reference gets its own bases as its row.
    """
    cleaned = {name: clean_sequence(seq) for name, seq in sequences.items()}
    ref_name = select_reference(cleaned, target_length)
    ref = cleaned[ref_name]
    ref_codes = encode(ref)
    aligner = build_aligner()

    anchored = []
    total = len(cleaned)
    for i, (name, seq) in enumerate(cleaned.items(), start=1):
        if name == ref_name:
            anchored.append(AnchoredSequence(
                name=name, row=ref_codes.copy(), ref_start=0, ref_end=len(ref),
                identity=1.0, coverage=1.0))
        else:
            anchored.append(anchor_to_reference(
                name, seq, ref, aligner, min_identity=min_identity,
                min_coverage=min_coverage, ref_codes=ref_codes))
        if progress is not None and (i % 25 == 0 or i == total):
            progress(f"Aligning to reference... {i:,}/{total:,}")
    return ref_name, anchored
