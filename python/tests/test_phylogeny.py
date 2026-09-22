import pandas as pd
import pytest

from boldcurator.core import phylogeny as phylo


def _f(rows):
    return pd.DataFrame(rows)


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


# -- kmer_frequency_matrix / cosine_distance_matrix ---------------------


def test_identical_sequences_have_zero_distance():
    seqs = {"a": "ACGTACGTACGT", "b": "ACGTACGTACGT"}
    names, freqs = phylo.kmer_frequency_matrix(seqs, k=4)
    dist = phylo.cosine_distance_matrix(freqs)
    assert dist.shape == (2, 2)
    assert dist[0, 1] == pytest.approx(0.0, abs=1e-9)
    assert dist[0, 0] == 0.0 and dist[1, 1] == 0.0


def test_divergent_sequences_have_positive_symmetric_distance():
    seqs = {"a": "ACGTACGTACGTACGT", "b": "TTTTAAAACCCCGGGG"}
    names, freqs = phylo.kmer_frequency_matrix(seqs, k=4)
    dist = phylo.cosine_distance_matrix(freqs)
    assert dist[0, 1] > 0
    assert dist[0, 1] == pytest.approx(dist[1, 0])


def test_ambiguity_codes_are_skipped_not_expanded():
    # A single N should not crash and should simply drop the windows that
    # touch it, not enumerate every ACGT expansion.
    seqs = {"a": "ACGTNACGTACGT"}
    names, freqs = phylo.kmer_frequency_matrix(seqs, k=4)
    assert freqs.shape == (1, 256)
    assert freqs[0].sum() == pytest.approx(1.0)


def test_short_or_empty_sequence_gives_all_zero_row():
    seqs = {"a": "AC", "b": ""}
    names, freqs = phylo.kmer_frequency_matrix(seqs, k=4)
    assert (freqs == 0).all()
    dist = phylo.cosine_distance_matrix(freqs)
    # Cosine similarity is undefined between two all-zero (unreadable)
    # vectors; treated as maximally dissimilar rather than identical, so an
    # unreadable sequence never masquerades as evidence of a match.
    assert dist[0, 1] == 1.0
    assert dist[0, 0] == 0.0 and dist[1, 1] == 0.0


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
    fake_sequences = {
        "p1": "ACGTACGTACGTACGTACGT",
        "p2": "ACGTACGTACGTACGTACGA",
        "p3": "TTTTAAAACCCCGGGGTTTT",
    }
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, r: fake_sequences)

    result = phylo.build_phylogeny(reps, grades, store=None, max_tips=100)
    assert result.tip_count == 3
    assert result.newick.endswith(";")
    assert not result.warnings
    assert "sp_c" in result.monophyly


def test_build_phylogeny_drops_representatives_with_no_sequence(monkeypatch):
    reps = _f([
        {"processid": "p1", "species": "sp", "identification": "sp",
         "bin_uri": "B1", "country.ocean": "Kenya"},
        {"processid": "p2", "species": "sp", "identification": "sp",
         "bin_uri": "B2", "country.ocean": "France"},
    ])
    monkeypatch.setattr(phylo, "fetch_representative_sequences",
                        lambda store, r: {"p1": "ACGTACGTACGTACGT"})
    result = phylo.build_phylogeny(reps, _f([]), store=None, max_tips=100)
    assert result.tip_count == 1
    assert result.newick == ""
    assert result.warnings
