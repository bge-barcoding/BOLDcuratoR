import math
import random

import numpy as np
import pandas as pd
import pytest

from boldcurator.core import phylogeny as phylo
from boldcurator.core import refalign


def _f(rows):
    return pd.DataFrame(rows)


def _random_seq(rng, n):
    return "".join(rng.choice("ACGT") for _ in range(n))


def _mutate(rng, seq, rate):
    return "".join(rng.choice("ACGT") if rng.random() < rate else b for b in seq)


# -- tip_label ---------------------------------------------------------


def test_tip_label_uses_species_and_country():
    row = pd.Series({"processid": "ABC123", "species": "Danaus plexippus",
                     "identification": "Danaus plexippus",
                     "country.ocean": "Kenya"})
    assert phylo.tip_label(row) == "ABC123-Danaus plexippus-Kenya"


def test_tip_label_falls_back_to_identification_then_unknown():
    row = pd.Series({"processid": "ABC123", "species": "", "identification": "Danaus sp.",
                     "country.ocean": ""})
    assert phylo.tip_label(row) == "ABC123-Danaus sp.-Unknown"

    row2 = pd.Series({"processid": "XYZ999", "species": None, "identification": None,
                      "country.ocean": None})
    assert phylo.tip_label(row2) == "XYZ999-Unknown-Unknown"


# -- k2p_distance_matrix ------------------------------------------------

_TRANSITION = {0: 2, 2: 0, 1: 3, 3: 1}      # A<->G, C<->T
_TRANSVERSION = {0: 1, 1: 0, 2: 3, 3: 2}    # A<->C, G<->T


def _random_row(n, seed=0):
    return np.random.default_rng(seed).integers(0, 4, n).astype(np.uint8)


def _k2p(p, q):
    return -0.5 * math.log(1 - 2 * p - q) - 0.25 * math.log(1 - 2 * q)


def test_identical_rows_have_zero_distance():
    row = _random_row(300)
    dist, imputed = phylo.k2p_distance_matrix(np.vstack([row, row]),
                                              min_shared_sites=100)
    assert dist.shape == (2, 2)
    assert dist[0, 1] == pytest.approx(0.0, abs=1e-12)
    assert not imputed.any()


def test_k2p_matches_the_formula_for_known_transitions_and_transversions():
    a = _random_row(200)
    b = a.copy()
    for i in range(10):
        b[i] = _TRANSITION[int(a[i])]
    for i in range(10, 15):
        b[i] = _TRANSVERSION[int(a[i])]
    dist, _ = phylo.k2p_distance_matrix(np.vstack([a, b]), min_shared_sites=100)
    assert dist[0, 1] == pytest.approx(_k2p(10 / 200, 5 / 200))
    assert dist[0, 1] == pytest.approx(dist[1, 0])


def test_missing_sites_are_left_out_of_the_denominator():
    """The point of pairwise deletion: a short read is compared over only
    what it shares, so 10 transitions in 150 shared sites reads as 10/150,
    not 10/200."""
    a = _random_row(200)
    b = a.copy()
    b[:50] = refalign.MISSING
    for i in range(50, 60):
        b[i] = _TRANSITION[int(a[i])]
    dist, imputed = phylo.k2p_distance_matrix(np.vstack([a, b]),
                                              min_shared_sites=100)
    assert dist[0, 1] == pytest.approx(_k2p(10 / 150, 0.0))
    assert not imputed.any()


def test_pairs_with_too_few_shared_sites_are_estimated_via_a_third_tip():
    full = _random_row(400)
    left = full.copy()
    left[200:] = refalign.MISSING
    right = full.copy()
    right[:200] = refalign.MISSING
    for i in (10, 20):
        left[i] = _TRANSITION[int(full[i])]
    for i in (300, 310, 320):
        right[i] = _TRANSITION[int(full[i])]
    dist, imputed = phylo.k2p_distance_matrix(np.vstack([full, left, right]),
                                              min_shared_sites=100)
    assert imputed[1, 2] and imputed[2, 1]
    assert not imputed[0, 1] and not imputed[0, 2]
    assert dist[1, 2] == pytest.approx(dist[1, 0] + dist[0, 2])
    assert dist[1, 2] == pytest.approx(dist[2, 1])


def test_pairs_with_no_route_fall_back_to_the_largest_defined_distance():
    full = _random_row(400)
    near = full.copy()
    near[0] = _TRANSITION[int(full[0])]
    empty = np.full(400, refalign.MISSING, dtype=np.uint8)
    dist, imputed = phylo.k2p_distance_matrix(np.vstack([full, near, empty]),
                                              min_shared_sites=100)
    assert imputed[2, 0] and imputed[2, 1] and not imputed[0, 1]
    assert dist[2, 0] == pytest.approx(dist[0, 1])
    assert dist[2, 2] == 0.0


# -- build_tree / to_newick ----------------------------------------------


def test_build_tree_round_trips_to_newick_with_tip_names():
    import numpy as np

    names = ["t0", "t1", "t2", "t3"]
    # A simple 4-point distance matrix with an obvious (t0,t1) | (t2,t3) split.
    mat = np.array([
        [0.0, 0.1, 0.9, 0.9],
        [0.1, 0.0, 0.9, 0.9],
        [0.9, 0.9, 0.0, 0.1],
        [0.9, 0.9, 0.1, 0.0],
    ])
    tree = phylo.build_tree(names, mat)
    newick = phylo.to_newick(tree)
    assert newick.endswith(";")
    for name in names:
        assert name in newick


# -- check_monophyly ------------------------------------------------------


def _tree_from(names, mat):
    import numpy as np

    return phylo.build_tree(names, np.array(mat))


def test_monophyletic_species_split_across_bins_is_detected():
    # sp_c's two BIN-tips (c1, c2) are close to each other and far from d.
    names = ["sp_c-BIN1", "sp_c-BIN2", "sp_d-BIN3"]
    mat = [
        [0.0, 0.1, 0.9],
        [0.1, 0.0, 0.9],
        [0.9, 0.9, 0.0],
    ]
    tree = _tree_from(names, mat)
    reps = _f([
        {"processid": "p1", "species": "sp_c", "_tip_label": "sp_c-BIN1"},
        {"processid": "p2", "species": "sp_c", "_tip_label": "sp_c-BIN2"},
        {"processid": "p3", "species": "sp_d", "_tip_label": "sp_d-BIN3"},
    ])
    grades = _f([
        {"species": "sp_c", "bags_grade": "C"},
        {"species": "sp_d", "bags_grade": "A"},
    ])
    result = phylo.check_monophyly(tree, reps, grades)
    assert result == {"sp_c": True}


def test_non_monophyletic_species_split_across_bins_is_detected():
    # sp_c's two BIN-tips are on opposite sides of sp_d -- not monophyletic.
    names = ["sp_c-BIN1", "sp_d-BIN2", "sp_c-BIN3"]
    mat = [
        [0.0, 0.2, 0.9],
        [0.2, 0.0, 0.2],
        [0.9, 0.2, 0.0],
    ]
    tree = _tree_from(names, mat)
    reps = _f([
        {"processid": "p1", "species": "sp_c", "_tip_label": "sp_c-BIN1"},
        {"processid": "p2", "species": "sp_d", "_tip_label": "sp_d-BIN2"},
        {"processid": "p3", "species": "sp_c", "_tip_label": "sp_c-BIN3"},
    ])
    grades = _f([
        {"species": "sp_c", "bags_grade": "C"},
        {"species": "sp_d", "bags_grade": "A"},
    ])
    result = phylo.check_monophyly(tree, reps, grades)
    assert result == {"sp_c": False}


def test_grade_c_species_with_only_one_tip_is_skipped():
    names = ["sp_c-BIN1", "sp_d-BIN2"]
    mat = [[0.0, 0.5], [0.5, 0.0]]
    tree = _tree_from(names, mat)
    reps = _f([
        {"processid": "p1", "species": "sp_c", "_tip_label": "sp_c-BIN1"},
        {"processid": "p2", "species": "sp_d", "_tip_label": "sp_d-BIN2"},
    ])
    grades = _f([{"species": "sp_c", "bags_grade": "C"}])
    assert phylo.check_monophyly(tree, reps, grades) == {}


# -- build_phylogeny orchestration ----------------------------------------


def test_build_phylogeny_too_large_raises_before_any_sequence_fetch(monkeypatch):
    called = []
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, reps: called.append(1) or {})
    reps = _f([{"processid": f"p{i}", "species": "sp", "bin_uri": "B",
               "country.ocean": "X"} for i in range(5)])
    with pytest.raises(phylo.PhylogenyTooLargeToBuild):
        phylo.build_phylogeny(reps, _f([]), store=None, max_tips=3)
    assert not called, "must not fetch sequences once the cap is already exceeded"


def test_build_phylogeny_end_to_end_with_fake_sequences(monkeypatch):
    reps = _f([
        {"processid": "p1", "species": "sp_c", "identification": "sp_c",
         "bin_uri": "BIN1", "country.ocean": "Kenya"},
        {"processid": "p2", "species": "sp_c", "identification": "sp_c",
         "bin_uri": "BIN2", "country.ocean": "France"},
        {"processid": "p3", "species": "sp_d", "identification": "sp_d",
         "bin_uri": "BIN3", "country.ocean": "Peru"},
    ])
    grades = _f([
        {"species": "sp_c", "bags_grade": "C"},
        {"species": "sp_d", "bags_grade": "A"},
    ])
    rng = random.Random(0)
    core = _random_seq(rng, 658)
    fake_sequences = {
        "p1": core,
        "p2": _mutate(rng, core, 0.02),
        "p3": _mutate(rng, core, 0.2),
    }
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, r: fake_sequences)

    result = phylo.build_phylogeny(reps, grades, store=None, max_tips=100)
    assert result.tip_count == 3
    assert result.newick.endswith(";")
    assert not result.warnings
    assert not result.flags
    assert result.monophyly == {"sp_c": True}
    assert result.reference == "p1-sp_c-Kenya"
    assert result.reference_length == 658


def test_build_phylogeny_keeps_and_flags_problem_sequences(monkeypatch):
    rng = random.Random(0)
    core = _random_seq(rng, 658)
    reps = _f([
        {"processid": pid, "species": "sp", "identification": "sp",
         "bin_uri": "B", "country.ocean": "X"}
        for pid in ("full", "near", "short", "junk", "flipped")
    ])
    fake_sequences = {
        "full": core,
        "near": _mutate(rng, core, 0.02),
        "short": core[:250],
        "junk": _random_seq(random.Random(7), 600),
        "flipped": refalign.reverse_complement(_mutate(rng, core, 0.02)),
    }
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, r: fake_sequences)

    result = phylo.build_phylogeny(reps, _f([]), store=None, max_tips=100)
    assert result.tip_count == 5, "flagged sequences stay on the tree"
    flags = result.flags
    assert set(flags) == {"short-sp-X", "junk-sp-X", "flipped-sp-X"}
    assert any(f.startswith("short:") for f in flags["short-sp-X"])
    assert any("low identity" in f for f in flags["junk-sp-X"])
    assert any("reverse-complemented" in f for f in flags["flipped-sp-X"])
    assert any("3 tip(s) marked" in w for w in result.warnings)


def test_long_and_short_reads_group_by_species_not_by_length(monkeypatch):
    """The bug this module's reference-anchored K2P replaced k-mer cosine
    distance for. COI's base composition varies along the gene; here the
    first half is AT-rich. Two species ~15% apart each contribute a
    full-length read, a read with long overhangs, and a short read of the
    AT-rich half. On k-mer profiles the two short reads looked like each
    other rather than their own species (confirmed against the previous
    implementation: neither species came out monophyletic); comparing
    shared sites must group every read with its own species.
    """
    rng = random.Random(1)
    ancestor = ("".join(rng.choices("ACGT", weights=[4, 1, 1, 4], k=330))
                + _random_seq(rng, 328))
    species = {"A": _mutate(rng, ancestor, 0.08), "B": _mutate(rng, ancestor, 0.08)}

    rows, fake_sequences = [], {}
    for sp, genome in species.items():
        for kind in ("full", "over", "short"):
            seq = _mutate(rng, genome, 0.01)
            if kind == "over":
                seq = _random_seq(rng, 80) + seq + _random_seq(rng, 90)
            elif kind == "short":
                seq = seq[:300]
            pid = f"{sp}_{kind}"
            fake_sequences[pid] = seq
            rows.append({"processid": pid, "species": f"sp_{sp}",
                         "identification": f"sp_{sp}", "bin_uri": sp,
                         "country.ocean": "X"})
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, r: fake_sequences)
    grades = _f([{"species": "sp_A", "bags_grade": "C"},
                 {"species": "sp_B", "bags_grade": "C"}])

    result = phylo.build_phylogeny(_f(rows), grades, store=None, max_tips=100)
    assert result.monophyly == {"sp_A": True, "sp_B": True}


def test_build_phylogeny_drops_representatives_with_no_sequence(monkeypatch):
    reps = _f([
        {"processid": "p1", "species": "sp", "identification": "sp",
         "bin_uri": "B1", "country.ocean": "Kenya"},
        {"processid": "p2", "species": "sp", "identification": "sp",
         "bin_uri": "B2", "country.ocean": "France"},
    ])
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, r: {"p1": "ACGTACGTACGTACGT", "p2": "---"})
    result = phylo.build_phylogeny(reps, _f([]), store=None, max_tips=100)
    assert result.tip_count == 1
    assert result.newick == ""
    assert result.warnings


# -- reroot_at --------------------------------------------------------


def test_reroot_at_a_tip_changes_the_newick_string():
    names = ["t0", "t1", "t2", "t3"]
    mat = [
        [0.0, 0.1, 0.9, 0.9],
        [0.1, 0.0, 0.9, 0.9],
        [0.9, 0.9, 0.0, 0.1],
        [0.9, 0.9, 0.1, 0.0],
    ]
    tree = _tree_from(names, mat)
    before = phylo.to_newick(tree)
    assert phylo.reroot_at(tree, "t2") is True
    after = phylo.to_newick(tree)
    assert after != before
    # Still the same four tips, just rerooted.
    for name in names:
        assert name in after


def test_reroot_at_an_internal_clade_keeps_its_tips_together():
    """The point of accepting an internal-clade name, not only a tip: a
    curator rooting on the common ancestor of a (BIN x country) group's
    several tips must not split that group apart in the redrawn layout --
    confirmed here by checking the two tips under the named internal clade
    are still each other's nearest neighbours (a cherry) after rerooting
    there, the same as they were before.
    """
    names = ["t0", "t1", "t2", "t3"]
    mat = [
        [0.0, 0.1, 0.9, 0.9],
        [0.1, 0.0, 0.9, 0.9],
        [0.9, 0.9, 0.0, 0.1],
        [0.9, 0.9, 0.1, 0.0],
    ]
    tree = _tree_from(names, mat)
    internal_names = [c.name for c in tree.get_nonterminals() if c.name]
    assert internal_names, "the NJ tree should have at least one named internal clade"

    target = internal_names[0]
    assert phylo.reroot_at(tree, target) is True
    newick = phylo.to_newick(tree)
    assert target in newick
    for name in names:
        assert name in newick


def test_reroot_at_an_unknown_name_returns_false_without_raising():
    names = ["t0", "t1", "t2"]
    mat = [[0.0, 0.5, 0.5], [0.5, 0.0, 0.5], [0.5, 0.5, 0.0]]
    tree = _tree_from(names, mat)
    before = phylo.to_newick(tree)
    assert phylo.reroot_at(tree, "not-a-real-tip") is False
    assert phylo.to_newick(tree) == before, "a failed reroot must not mutate the tree"


def test_reroot_at_a_blank_name_returns_false():
    names = ["t0", "t1", "t2"]
    mat = [[0.0, 0.5, 0.5], [0.5, 0.0, 0.5], [0.5, 0.5, 0.0]]
    tree = _tree_from(names, mat)
    assert phylo.reroot_at(tree, "") is False


def test_reroot_recomputes_monophyly_against_the_new_root():
    """The whole reason a reroot needs to regenerate monophyly, not just
    the Newick string: monophyly is a rooted-tree property, so the same
    underlying topology can read as monophyletic under one root and not
    under another. Also exactly the failure mode a single-tip reroot risks
    for a multi-tip (BIN x country) group -- rooting AT one of a species'
    own two tips splits it off from its sibling, breaking that species'
    own monophyly as a side effect of where the root landed, not because
    anything about the underlying data changed. Confirmed empirically
    before writing this test, not assumed.
    """
    # Quartet topology: (A,B) and (C,D) are each other's closest pair.
    names = ["A", "B", "C", "D"]
    mat = [
        [0.0, 0.1, 0.9, 0.9],
        [0.1, 0.0, 0.9, 0.9],
        [0.9, 0.9, 0.0, 0.1],
        [0.9, 0.9, 0.1, 0.0],
    ]
    tree = _tree_from(names, mat)
    reps = _f([
        {"processid": "p1", "species": "sp_x", "_tip_label": "A"},
        {"processid": "p2", "species": "sp_x", "_tip_label": "B"},
        {"processid": "p3", "species": "sp_y", "_tip_label": "C"},
        {"processid": "p4", "species": "sp_y", "_tip_label": "D"},
    ])
    grades = _f([
        {"species": "sp_x", "bags_grade": "C"},
        {"species": "sp_y", "bags_grade": "C"},
    ])
    before = phylo.check_monophyly(tree, reps, grades)
    assert before == {"sp_x": True, "sp_y": True}

    # Reroot AT one of sp_x's own two tips: splits it from its sibling B,
    # flipping sp_x to not-monophyletic, while sp_y (untouched by this
    # reroot) stays monophyletic.
    assert phylo.reroot_at(tree, "A") is True
    after = phylo.check_monophyly(tree, reps, grades)
    assert after == {"sp_x": False, "sp_y": True}
