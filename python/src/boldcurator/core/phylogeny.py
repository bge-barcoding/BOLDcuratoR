"""A quick, alignment-free overview tree for the Phylogeny tab.

The tree is deliberately **not** built from every specimen in a search
result -- it is built from the specimens the app has *already* selected as
each (BIN x country) group's best representative
(:func:`core.selection.auto_select_best_specimens`, surfaced via
:func:`io.exports.selected_rows`). This module does not re-derive which
specimens count as representative; it only turns whatever representative set
it is handed into a tree. That keeps a curator's manual reselection
(:mod:`io.annotations`) in charge of what appears on the tree, exactly as it
is already in charge of "Download Selected".

No external binary (no aligner, no ML tree tool) is used, so nothing new
needs bundling per OS. Two consequences of that:

1. The tree is neighbor-joining, not maximum-likelihood. NJ from a
   k-mer-based distance is what "estimated very quickly" buys once likelihood
   and cross-platform binaries are both off the table.
2. Distances come from tetranucleotide frequency vectors (cosine distance),
   not a multiple sequence alignment -- COI-5P barcodes are never aligned
   anywhere else in this app, and full pairwise alignment is far too slow at
   any tip count worth showing.

The real speed ceiling is not this module's own arithmetic (a single
vectorised distance-matrix computation) but Biopython's
``DistanceTreeConstructor.nj()``, a plain-Python, unvectorised O(n^3) loop.
Measured on this project's own hardware: ~0.4s at 100 tips, ~6s at 250,
~54s at 500 -- a clean cubic (`t = k * n**3`, `k ~= 4.3e-7`). PHYLOGENY_LIMITS
(config.constants) is set from that fit, not a guess.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Iterable

import numpy as np
import pandas as pd

from .selection import UNKNOWN_COUNTRY
from .species import column_or_missing, to_text

#: ACGT-only k-mers are counted; anything containing an ambiguity code (N, R,
#: Y, ...) or a gap is skipped rather than expanded, since COI-5P barcodes are
#: >99% clean ACGT and expanding ambiguity codes would cost far more than the
#: handful of skipped windows are worth.
_BASE_CODE = {"A": 0, "C": 1, "G": 2, "T": 3}


class PhylogenyTooLargeToBuild(RuntimeError):
    """The representative set is bigger than this pure-Python engine can
    turn into a tree in a reasonable time. See module docstring for the
    measured NJ scaling this is based on."""

    def __init__(self, tip_count: int, max_tips: int):
        self.tip_count = tip_count
        self.max_tips = max_tips
        super().__init__(
            f"{tip_count:,} representative specimens is over the {max_tips:,} "
            "this tab's pure-Python tree builder can handle quickly. Narrow "
            "the search, or curate down the selected representatives, and "
            "try again."
        )


def tip_label(row: pd.Series) -> str:
    """``{processid}-{species}-{country}``.

    ``species`` already holds the full binomial (BOLD's own convention for
    the species-rank taxonomy field -- nothing else in this app concatenates
    a separate ``genus`` column onto it). Falls back to ``identification``
    then ``"Unknown"`` for species, and to ``"Unknown"`` for country -- the
    same fallback :func:`core.selection.auto_select_best_specimens` already
    uses for a blank country, so the convention matches the rest of the app.
    ``processid`` is always present and unique, so the label is unique even
    when both fallbacks fire.
    """
    processid = str(row.get("processid", "")).strip()
    species = row.get("species")
    if species is None or pd.isna(species) or not str(species).strip():
        species = row.get("identification")
    if species is None or pd.isna(species) or not str(species).strip():
        species = "Unknown"
    else:
        species = str(species).strip()
    country = row.get("country.ocean")
    if country is None or pd.isna(country) or not str(country).strip():
        country = UNKNOWN_COUNTRY
    else:
        country = str(country).strip()
    return f"{processid}-{species}-{country}"


def fetch_representative_sequences(store, representatives: pd.DataFrame
                                   ) -> dict[str, str]:
    """``processid -> nuc`` for exactly the representative set -- never the
    whole search result. ``core.selection`` has already done the work of
    keeping this set small."""
    from ..data.queries import iter_sequences

    if representatives is None or len(representatives) == 0:
        return {}
    processids = [str(p) for p in representatives.get("processid", [])]
    return dict(iter_sequences(store, processids))


def kmer_frequency_matrix(sequences: dict[str, str], k: int = 4
                          ) -> tuple[list[str], np.ndarray]:
    """Tip names (insertion order of ``sequences``) and an ``(M, 4**k)``
    row-normalised k-mer frequency matrix.

    A window containing anything other than A/C/G/T (an ambiguity code, a
    gap, lowercase is upper-cased first) is simply skipped, not expanded --
    see the module docstring.
    """
    names = list(sequences.keys())
    n_bins = 4 ** k
    freqs = np.zeros((len(names), n_bins), dtype=np.float64)

    for row, name in enumerate(names):
        seq = (sequences[name] or "").upper()
        counts = np.zeros(n_bins, dtype=np.float64)
        total = 0
        run: list[int] = []  # the current unbroken stretch of valid bases
        for base in seq:
            code = _BASE_CODE.get(base, -1)
            if code < 0:
                run.clear()
                continue
            run.append(code)
            if len(run) > k:
                run.pop(0)
            if len(run) == k:
                index = 0
                for c in run:
                    index = index * 4 + c
                counts[index] += 1
                total += 1
        if total:
            freqs[row] = counts / total

    return names, freqs


def cosine_distance_matrix(freqs: np.ndarray) -> np.ndarray:
    """Symmetric ``(M, M)`` distance matrix, ``1 - cosine similarity``.

    A single vectorised matrix multiply -- the one part of tree-building
    that scales well; see the module docstring for what does not.
    """
    norms = np.linalg.norm(freqs, axis=1, keepdims=True)
    safe_norms = np.where(norms == 0, 1.0, norms)
    normalised = freqs / safe_norms
    similarity = normalised @ normalised.T
    distance = 1.0 - similarity
    np.fill_diagonal(distance, 0.0)
    # Floating-point noise can push a same-sequence pair fractionally below
    # zero or a zero-vector row (an unreadable sequence) to a stray negative.
    distance = np.clip(distance, 0.0, None)
    return (distance + distance.T) / 2.0


def build_tree(names: list[str], distances: np.ndarray):
    """Neighbor-joining tree (``Bio.Phylo.BaseTree.Tree``), midpoint-rooted.

    ``Bio.Phylo.TreeConstruction.DistanceCalculator`` is not used -- it
    computes distances from an aligned ``MultipleSeqAlignment``, which this
    module deliberately never builds (see module docstring). The NumPy
    matrix from :func:`cosine_distance_matrix` is converted directly to
    Biopython's own lower-triangular container instead.
    """
    from Bio.Phylo.TreeConstruction import DistanceMatrix, DistanceTreeConstructor

    lower = [row[: i + 1].tolist() for i, row in enumerate(distances)]
    dm = DistanceMatrix(names, lower)
    tree = DistanceTreeConstructor().nj(dm)
    if tree.count_terminals() > 2:
        tree.root_at_midpoint()
    return tree


def to_newick(tree) -> str:
    from io import StringIO

    from Bio import Phylo

    buf = StringIO()
    Phylo.write(tree, buf, "newick")
    return buf.getvalue().strip()


def check_monophyly(tree, representatives: pd.DataFrame, bags_grades: pd.DataFrame
                    ) -> dict[str, bool]:
    """``species -> is_monophyletic``, for every grade-C species with >=2
    tips on this tree.

    Every grade-C species already contributes at least one tip per BIN by
    construction of the representative pool (see module docstring) -- a
    species with only one tip here has nothing to ask about.
    """
    if bags_grades is None or len(bags_grades) == 0:
        return {}
    if "_tip_label" not in representatives.columns:
        return {}

    grade_c = set(bags_grades.loc[bags_grades["bags_grade"] == "C", "species"])
    if not grade_c:
        return {}

    terminal_by_name = {t.name: t for t in tree.get_terminals()}
    species_col = to_text(column_or_missing(representatives, "species")).str.strip()

    out: dict[str, bool] = {}
    for species in sorted(grade_c):
        group = representatives[species_col == species]
        tips = [terminal_by_name[label] for label in group["_tip_label"]
                if label in terminal_by_name]
        if len(tips) < 2:
            continue
        out[species] = bool(tree.is_monophyletic(tips))
    return out


def reroot_at(tree, target_name: str) -> bool:
    """Re-root ``tree`` in place using ``target_name`` as the new outgroup.
    Returns ``True`` on success, ``False`` (never raises) if ``target_name``
    isn't a real tip or internal clade on this tree.

    Deliberately accepts *either* a tip or an internal clade's own name --
    not just a tip. Biopython's ``root_with_outgroup`` already treats both
    the same way (confirmed against a real NJ tree: internal clades come
    out of ``DistanceTreeConstructor.nj()`` reliably named, e.g. ``Inner1``,
    ``Inner2``, and the Newick writer/JS parser both already round-trip
    those names like any other). Restricting this to tips only would be the
    wrong default: rooting at a single tip of a (BIN x country) group that
    contributed more than one representative (see module docstring) would
    visually split that tip away from its own group's other tips in the
    redrawn layout, even though nothing about the underlying topology
    actually changed -- purely a rooting/display artefact. Letting a
    curator root on the common ancestor of a whole clade instead avoids
    that.

    A blank ``target_name`` (the tree's own root has no name) or a name
    from a different, stale tree both fail the same way Biopython's own
    lookup fails on a name it can't find -- ``ValueError`` -- which is
    exactly the "nothing to do" case this returns ``False`` for.
    """
    if not target_name:
        return False
    try:
        tree.root_with_outgroup(target_name)
        return True
    except ValueError:
        return False


@dataclass
class PhylogenyResult:
    representatives: pd.DataFrame
    newick: str
    monophyly: dict[str, bool] = field(default_factory=dict)
    tip_count: int = 0
    warnings: list[str] = field(default_factory=list)
    #: The live tree object, not just its Newick string -- kept so a
    #: reroot (see reroot_at()) can mutate it in place and regenerate both
    #: the Newick and the monophyly verdicts against the new root, rather
    #: than needing to re-parse Newick back into a tree first. None until a
    #: tree has actually been built (e.g. the empty/too-few-sequences cases
    #: below, which return before one exists).
    tree: object = None
    #: Kept alongside the tree so a reroot can recompute monophyly without
    #: re-fetching session state it has no access to from here.
    bags_grades: pd.DataFrame = field(default_factory=pd.DataFrame)


def build_phylogeny(
    representatives: pd.DataFrame,
    bags_grades: pd.DataFrame,
    store,
    *,
    k: int = 4,
    max_tips: int,
    progress: "Callable[[str], None] | None" = None,
) -> PhylogenyResult:
    """Orchestrates label -> fetch -> distance -> tree -> monophyly.

    ``representatives`` is already ``io.exports.selected_rows(result.specimens,
    annotations)`` -- the app's existing curated (BIN x country) selection.
    This function does not re-derive which specimens count as representative;
    see the module docstring.
    """
    def report(text: str) -> None:
        if progress is not None:
            progress(text)

    tip_count = len(representatives) if representatives is not None else 0
    if tip_count > max_tips:
        raise PhylogenyTooLargeToBuild(tip_count, max_tips)
    if tip_count == 0:
        return PhylogenyResult(representatives=representatives, newick="",
                               tip_count=0,
                               warnings=["No representative specimens to build a tree from."])

    reps = representatives.copy()
    reps["_tip_label"] = reps.apply(tip_label, axis=1)
    # processid is unique, but guard against a duplicate tip label anyway
    # (e.g. two blank-everything rows) -- Biopython's tree needs unique names.
    if reps["_tip_label"].duplicated().any():
        reps["_tip_label"] = reps["_tip_label"] + "-" + reps["processid"].astype(str)

    report("Fetching sequences...")
    sequences_by_pid = fetch_representative_sequences(store, reps)
    label_by_pid = dict(zip(reps["processid"].astype(str), reps["_tip_label"]))
    sequences = {
        label_by_pid[pid]: seq
        for pid, seq in sequences_by_pid.items()
        if pid in label_by_pid and seq
    }
    warnings = []
    missing = tip_count - len(sequences)
    if missing:
        warnings.append(
            f"{missing:,} representative specimen(s) had no usable sequence "
            "and were left off the tree."
        )
    reps = reps[reps["_tip_label"].isin(sequences)]
    if len(reps) < 2:
        return PhylogenyResult(representatives=reps, newick="", tip_count=len(reps),
                               warnings=warnings + [
                                   "Fewer than two representatives had usable "
                                   "sequences; a tree needs at least two."])

    report("Computing distances...")
    names, freqs = kmer_frequency_matrix(sequences, k=k)
    distances = cosine_distance_matrix(freqs)

    report("Building tree...")
    tree = build_tree(names, distances)
    newick = to_newick(tree)

    report("Checking grade-C monophyly...")
    monophyly = check_monophyly(tree, reps, bags_grades)

    return PhylogenyResult(representatives=reps, newick=newick, monophyly=monophyly,
                           tip_count=len(reps), warnings=warnings,
                           tree=tree, bags_grades=bags_grades)
