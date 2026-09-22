"""A quick overview tree for the Phylogeny tab.

The tree is deliberately **not** built from every specimen in a search
result -- it is built from the specimens the app has *already* selected as
each (BIN x country) group's best representative
(:func:`core.selection.auto_select_best_specimens`, surfaced via
:func:`io.exports.selected_rows`). This module does not re-derive which
specimens count as representative; it only turns whatever representative set
it is handed into a tree. That keeps a curator's manual reselection
(:mod:`io.annotations`) in charge of what appears on the tree, exactly as it
is already in charge of "Download Selected".

No external binary (no ML tree tool, no multiple aligner) is used, so
nothing new needs bundling per OS. Two consequences of that:

1. The tree is neighbor-joining, not maximum-likelihood. NJ is what
   "estimated very quickly" buys once likelihood and cross-platform
   binaries are both off the table.
2. Distances are Kimura 2-parameter (K2P, the distance BOLD's own trees
   use) over a **reference-anchored** alignment, not a true multiple
   alignment: :mod:`core.refalign` aligns every representative once to a
   single in-data reference (O(n) pairwise alignments, ~2s at the tip cap)
   and lays its bases out in reference coordinates. Each pair's distance is
   then computed only over the sites *both* cover (pairwise deletion).

   This replaced an alignment-free tetranucleotide (k-mer) cosine distance,
   which was fast but wrong in a way that mattered here: a k-mer profile
   depends on which part of COI a sequence covers, so long reads with
   overhangs and short partial reads were placed by length and region
   rather than by divergence. Comparing shared sites removes that bias.

   Sequences that align poorly, cover little of the reference, or share too
   few sites with some other tip are kept on the tree and flagged (see
   :attr:`PhylogenyResult.flags`), not silently dropped.

The real speed ceiling is not this module's own arithmetic (a handful of
vectorised matrix multiplies) nor the alignment step, but Biopython's
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

from ..config.constants import PHYLOGENY_ALIGNMENT
from . import refalign
from .selection import UNKNOWN_COUNTRY
from .species import column_or_missing, to_text


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


def k2p_distance_matrix(rows: np.ndarray, *, min_shared_sites: int
                        ) -> tuple[np.ndarray, np.ndarray]:
    """Kimura 2-parameter distances between the reference-anchored rows of
    :func:`core.refalign.anchor_all`, each pair over only the sites both
    have a base at (pairwise deletion).

    Returns ``(distances, imputed)``: a symmetric ``(M, M)`` matrix and a
    boolean mask of the pairs whose distance could not be computed directly
    -- fewer than ``min_shared_sites`` shared sites, or so divergent that
    K2P's logs are undefined (saturation) -- and were estimated instead as
    the shortest two-step path ``min_k d(i,k) + d(k,j)`` through a third tip
    with both distances defined (an upper bound, and additive on a tree), or
    failing that the largest defined distance. NJ needs a full matrix, and
    this keeps such a tip on the tree near the tips it does overlap.

    All counts come from one-hot matrix multiplies, so this is vectorised
    across every pair at once.
    """
    rows = np.asarray(rows, dtype=np.uint8)
    n = rows.shape[0]
    valid = (rows != refalign.MISSING).astype(np.float32)
    shared = valid @ valid.T
    same = np.zeros((n, n), dtype=np.float32)
    for base in range(4):
        onehot = (rows == base).astype(np.float32)
        same += onehot @ onehot.T
    purine = ((rows == 0) | (rows == 2)).astype(np.float32)      # A, G
    pyrimidine = ((rows == 1) | (rows == 3)).astype(np.float32)  # C, T
    transversions = purine @ pyrimidine.T + pyrimidine @ purine.T
    transitions = shared - same - transversions

    shared = shared.astype(np.float64)
    with np.errstate(divide="ignore", invalid="ignore"):
        p = transitions / shared
        q = transversions / shared
        a = 1.0 - 2.0 * p - q
        b = 1.0 - 2.0 * q
        defined = (shared >= min_shared_sites) & (a > 0) & (b > 0)
        dist = np.where(defined, -0.5 * np.log(np.where(defined, a, 1.0))
                        - 0.25 * np.log(np.where(defined, b, 1.0)), np.nan)
    np.fill_diagonal(dist, 0.0)
    np.fill_diagonal(defined, True)
    dist = np.clip(dist, 0.0, None)  # -0.0 / float noise on identical pairs

    imputed = ~defined
    if imputed.any():
        known = np.where(defined, dist, np.inf)
        fallback = dist[defined & ~np.eye(n, dtype=bool)]
        fallback = float(fallback.max()) if fallback.size else 1.0
        for i in np.flatnonzero(imputed.any(axis=1)):
            via = (known[i][:, None] + known).min(axis=0)
            cols = imputed[i]
            dist[i, cols] = np.where(np.isfinite(via[cols]), via[cols], fallback)
        dist = (dist + dist.T) / 2.0
    return dist, imputed


def build_tree(names: list[str], distances: np.ndarray):
    """Neighbor-joining tree (``Bio.Phylo.BaseTree.Tree``), midpoint-rooted.

    ``Bio.Phylo.TreeConstruction.DistanceCalculator`` is not used -- it
    computes distances from an aligned ``MultipleSeqAlignment``, which this
    module deliberately never builds (see module docstring). The NumPy
    matrix from :func:`k2p_distance_matrix` is converted directly to
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
    #: ``tip label -> reasons`` for every tip whose placement deserves a
    #: second look (poor or reverse-complemented alignment to the reference,
    #: short coverage, estimated distances). Flagged tips stay on the tree;
    #: the tab marks them. Tips with nothing to report are absent.
    flags: dict[str, list[str]] = field(default_factory=dict)
    #: Tip label of the representative every sequence was aligned to, and
    #: its length in bp (see core.refalign).
    reference: str = ""
    reference_length: int = 0


def build_phylogeny(
    representatives: pd.DataFrame,
    bags_grades: pd.DataFrame,
    store,
    *,
    max_tips: int,
    target_length: int = PHYLOGENY_ALIGNMENT["TARGET_LENGTH"],
    min_identity: float = PHYLOGENY_ALIGNMENT["MIN_IDENTITY"],
    min_coverage: float = PHYLOGENY_ALIGNMENT["MIN_COVERAGE"],
    min_shared_sites: int = PHYLOGENY_ALIGNMENT["MIN_SHARED_SITES"],
    progress: "Callable[[str], None] | None" = None,
) -> PhylogenyResult:
    """Orchestrates label -> fetch -> align to reference -> K2P distance ->
    tree -> monophyly.

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
        if pid in label_by_pid and refalign.clean_sequence(seq)
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

    report("Aligning to reference...")
    reference, anchored = refalign.anchor_all(
        sequences, target_length=target_length, min_identity=min_identity,
        min_coverage=min_coverage, progress=progress)
    names = [a.name for a in anchored]

    report("Computing distances...")
    distances, imputed = k2p_distance_matrix(
        np.vstack([a.row for a in anchored]), min_shared_sites=min_shared_sites)

    flags: dict[str, list[str]] = {}
    for i, a in enumerate(anchored):
        reasons = list(a.flags)
        n_imputed = int(imputed[i].sum())
        if n_imputed:
            reasons.append(
                f"shares under {min_shared_sites} comparable sites with "
                f"{n_imputed:,} other tip(s); those distances are estimated")
        if reasons:
            flags[a.name] = reasons
    if flags:
        warnings.append(
            f"{len(flags):,} tip(s) marked \u26a0 may be misplaced; hover a "
            "marked tip to see why.")

    report("Building tree...")
    tree = build_tree(names, distances)
    newick = to_newick(tree)

    report("Checking grade-C monophyly...")
    monophyly = check_monophyly(tree, reps, bags_grades)

    return PhylogenyResult(representatives=reps, newick=newick, monophyly=monophyly,
                           tip_count=len(reps), warnings=warnings,
                           tree=tree, bags_grades=bags_grades, flags=flags,
                           reference=reference,
                           reference_length=len(anchored[names.index(reference)].row))
