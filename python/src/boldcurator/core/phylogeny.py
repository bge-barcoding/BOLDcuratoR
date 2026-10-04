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

A representative whose own record has no usable sequence would otherwise
take its whole (BIN x country) group off the tree. Given the full result
(``specimens``), :func:`build_phylogeny` stands in the group's best-scoring
record that does have one, and flags the tip as a stand-in.

Neighbor-joining is :func:`neighbor_joining`, a NumPy port of Biopython's
``DistanceTreeConstructor.nj()`` that gives the same tree. Biopython's own is
a plain-Python O(n^3) loop (~54 s at 500 tips, ~7 min at 1,000); vectorising
each step brings 1,000 tips to a few seconds, so the alignment to the
reference is now the slowest step. PHYLOGENY_LIMITS (config.constants) is set
from those measurements.
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


#: How many of a group's best-scoring records :func:`find_stand_ins` fetches
#: sequences for, looking for one that has a usable sequence. Bounds the fetch
#: for a big BIN whose records mostly have none.
STAND_IN_CANDIDATES = 25


def group_keys(frame: pd.DataFrame) -> pd.Series:
    """``(bin_uri, country)`` per row, normalised the way
    :func:`core.selection.auto_select_best_specimens` groups them (a blank
    country is ``UNKNOWN_COUNTRY``)."""
    bins = to_text(column_or_missing(frame, "bin_uri")).str.strip()
    country = to_text(column_or_missing(frame, "country.ocean")).str.strip()
    country = country.mask(country == "", UNKNOWN_COUNTRY)
    return pd.Series(list(zip(bins, country)), index=frame.index, dtype=object)


def find_stand_ins(store, specimens: pd.DataFrame, missing: pd.DataFrame,
                   taken: set[str], *, per_group: int = STAND_IN_CANDIDATES
                   ) -> list[tuple[str, pd.Series, str]]:
    """For each row of ``missing`` (representatives with no usable sequence),
    the best-scoring other record of the same (BIN x country) group in
    ``specimens`` that has one: ``[(missing processid, stand-in row, nuc)]``.

    Records in ``taken`` (already on the tree) are skipped, as is a
    representative with no BIN, which has no group to draw from.
    """
    if specimens is None or len(specimens) == 0 or len(missing) == 0:
        return []
    missing_keys = group_keys(missing)
    wanted = {key for key in missing_keys if key[0]}
    if not wanted:
        return []

    keys = group_keys(specimens)
    pids = to_text(column_or_missing(specimens, "processid"))
    pool = specimens[keys.isin(wanted) & ~pids.isin(taken)].copy()
    if len(pool) == 0:
        return []
    pool["_key"] = keys[pool.index]
    pool["_pid"] = pids[pool.index]
    pool["_score"] = pd.to_numeric(column_or_missing(pool, "quality_score"),
                                   errors="coerce").fillna(0)
    pool = (pool.sort_values(["_score", "_pid"], ascending=[False, True])
            .groupby("_key", sort=False).head(per_group))
    fetched = fetch_representative_sequences(store, pool)

    used: set[str] = set()
    out = []
    for missing_pid, key in zip(to_text(missing["processid"]), missing_keys):
        for _, row in pool[pool["_key"] == key].iterrows():
            seq = fetched.get(row["_pid"])
            if row["_pid"] not in used and refalign.clean_sequence(seq):
                used.add(row["_pid"])
                out.append((missing_pid,
                            row.drop(labels=["_key", "_pid", "_score"]), seq))
                break
    return out


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


def neighbor_joining(names: list[str], distances: np.ndarray):
    """Unrooted neighbor-joining tree (``Bio.Phylo.BaseTree.Tree``).

    Step for step the algorithm of Biopython's
    ``DistanceTreeConstructor.nj()`` -- the same pair choice (ties go to the
    first pair in its scan order), branch lengths, ``Inner<k>`` clade names
    and final join -- so it yields the same tree. Each step is vectorised
    over the whole matrix instead of looped in Python, which is the
    difference between seconds and minutes at 1,000 tips.
    """
    from Bio.Phylo import BaseTree

    d = np.array(distances, dtype=np.float64)
    clades = [BaseTree.Clade(None, name) for name in names]
    n = len(clades)
    if n == 1:
        return BaseTree.Tree(clades[0], rooted=False)
    if n == 2:
        clades[1].branch_length = d[1, 0] / 2.0
        clades[0].branch_length = d[1, 0] - clades[1].branch_length
        inner = BaseTree.Clade(None, "Inner")
        inner.clades.extend([clades[1], clades[0]])
        return BaseTree.Tree(inner, rooted=False)

    # Biopython scans i = 1.., j < i and keeps the first strict minimum: the
    # strict lower triangle in row-major order, which is what argmin over it
    # returns with everything else masked out.
    lower = np.tri(n, k=-1, dtype=bool)
    inner_count = 0
    inner = None
    while len(clades) > 2:
        m = len(clades)
        # Summed left to right (cumsum), as Biopython's loop does: NumPy's
        # pairwise sum rounds differently, and on tied distances that last-bit
        # difference picks a different, equally valid, pair.
        node_dist = np.cumsum(d, axis=1)[:, -1] / (m - 2)
        q = np.where(lower[:m, :m],
                     d - node_dist[:, None] - node_dist[None, :], np.inf)
        min_i, min_j = divmod(int(np.argmin(q)), m)
        if (min_i, min_j) == (1, 0):
            # Biopython seeds its scan with min_i=0, min_j=1, so when that
            # first pair wins the roles are the other way round.
            min_i, min_j = 0, 1

        inner_count += 1
        inner = BaseTree.Clade(None, f"Inner{inner_count}")
        clade1, clade2 = clades[min_i], clades[min_j]
        inner.clades.extend([clade1, clade2])
        clade1.branch_length = (d[min_i, min_j] + node_dist[min_i]
                                - node_dist[min_j]) / 2.0
        clade2.branch_length = d[min_i, min_j] - clade1.branch_length

        joined = (d[min_i] + d[min_j] - d[min_i, min_j]) / 2.0
        joined[min_j] = 0.0
        d[min_j, :] = joined
        d[:, min_j] = joined
        clades[min_j] = inner
        del clades[min_i]
        d = np.delete(np.delete(d, min_i, axis=0), min_i, axis=1)

    if clades[0] is inner:
        clades[0].branch_length = 0
        clades[1].branch_length = d[1, 0]
        clades[0].clades.append(clades[1])
        root = clades[0]
    else:
        clades[0].branch_length = d[1, 0]
        clades[1].branch_length = 0
        clades[1].clades.append(clades[0])
        root = clades[1]
    return BaseTree.Tree(root, rooted=False)


def build_tree(names: list[str], distances: np.ndarray):
    """Neighbor-joining tree (``Bio.Phylo.BaseTree.Tree``), midpoint-rooted.

    ``Bio.Phylo.TreeConstruction.DistanceCalculator`` is not used -- it
    computes distances from an aligned ``MultipleSeqAlignment``, which this
    module deliberately never builds (see module docstring). The NumPy
    matrix from :func:`k2p_distance_matrix` goes straight to
    :func:`neighbor_joining`.
    """
    tree = neighbor_joining(names, distances)
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

    tip_sets = clade_tip_sets(tree)
    tip_names = {name for s in tip_sets for name in s}
    species_col = to_text(column_or_missing(representatives, "species")).str.strip()

    out: dict[str, bool] = {}
    for species in sorted(grade_c):
        group = representatives[species_col == species]
        tips = frozenset(label for label in group["_tip_label"] if label in tip_names)
        if len(tips) < 2:
            continue
        out[species] = tips in tip_sets
    return out


def clade_tip_sets(tree) -> set[frozenset[str]]:
    """The set of tip names under each clade of ``tree``.

    A species is monophyletic exactly when its tips are one of these sets --
    the test Biopython's ``is_monophyletic`` makes, but that walks the tree
    again for every species (8 s for 250 grade-C species on a 1,000-tip
    tree). Built once, iteratively, so a deep tree cannot hit the recursion
    limit.
    """
    order, stack = [], [tree.root]
    while stack:
        clade = stack.pop()
        order.append(clade)
        stack.extend(clade.clades)
    tips: dict[int, frozenset[str]] = {}
    for clade in reversed(order):
        tips[id(clade)] = (frozenset().union(*(tips[id(c)] for c in clade.clades))
                           if clade.clades else frozenset([clade.name]))
    return set(tips.values())


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
    specimens: pd.DataFrame | None = None,
) -> PhylogenyResult:
    """Orchestrates label -> fetch -> align to reference -> K2P distance ->
    tree -> monophyly.

    ``representatives`` is already ``io.exports.selected_rows(result.specimens,
    annotations)`` -- the app's existing curated (BIN x country) selection.
    This function does not re-derive which specimens count as representative;
    see the module docstring. ``specimens`` (the whole result) is only where a
    stand-in comes from when a representative has no usable sequence (see
    :func:`find_stand_ins`); without it such a group is left off the tree.
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
    flags: dict[str, list[str]] = {}

    unsequenced = reps[~reps["_tip_label"].isin(sequences)]
    stand_ins = find_stand_ins(store, specimens, unsequenced,
                               set(to_text(reps["processid"])))
    if stand_ins:
        rows = []
        for missing_pid, row, seq in stand_ins:
            label = tip_label(row)
            if label in sequences:
                label = f"{label}-{row.get('processid')}"
            row["_tip_label"] = label
            rows.append(row)
            sequences[label] = seq
            flags[label] = [
                f"stand-in for {missing_pid}, the selected representative of "
                "this BIN x country, which has no usable sequence"]
        reps = pd.concat([reps, pd.DataFrame(rows)], ignore_index=True)

    reps = reps[reps["_tip_label"].isin(sequences)]
    left_off = unsequenced[~unsequenced["processid"].astype(str).isin(
        {pid for pid, _, _ in stand_ins})]
    if len(left_off):
        bins = sorted({b for b in to_text(column_or_missing(left_off, "bin_uri"))
                       .str.strip() if b})
        shown = ", ".join(bins[:10]) + (f" and {len(bins) - 10:,} more"
                                        if len(bins) > 10 else "")
        warnings.append(
            f"{len(left_off):,} representative specimen(s) had no usable "
            "sequence, and no other record of the same BIN x country has one, "
            "so they were left off the tree"
            + (f" (BINs: {shown})." if bins else "."))
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

    for i, a in enumerate(anchored):
        reasons = flags.get(a.name, []) + list(a.flags)
        n_imputed = int(imputed[i].sum())
        if n_imputed:
            reasons.append(
                f"shares under {min_shared_sites} comparable sites with "
                f"{n_imputed:,} other tip(s); those distances are estimated")
        if reasons:
            flags[a.name] = reasons
    if flags:
        warnings.append(
            f"{len(flags):,} tip(s) marked \u26a0 need a second look (a "
            "stand-in record, or a sequence that may be misplaced); hover a "
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
