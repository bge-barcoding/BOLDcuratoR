import random

import numpy as np
import pytest

from boldcurator.core import refalign

MIN = dict(min_identity=0.6, min_coverage=0.5)


def _rng():
    return random.Random(0)


def _random_seq(rng, n):
    return "".join(rng.choice("ACGT") for _ in range(n))


def _mutate(rng, seq, rate):
    return "".join(rng.choice("ACGT") if rng.random() < rate else b for b in seq)


@pytest.fixture
def core():
    return _random_seq(_rng(), 658)


def _anchor(seq, ref):
    return refalign.anchor_to_reference("q", seq, ref, refalign.build_aligner(), **MIN)


# -- clean_sequence / select_reference ------------------------------------


def test_clean_sequence_strips_gaps_and_normalises_letters():
    assert refalign.clean_sequence("--acg-t..u\nNX?") == "ACGTTNNN"
    assert refalign.clean_sequence(None) == ""


def test_select_reference_prefers_a_clean_sequence_over_a_closer_length_one():
    seqs = {
        "closest_but_dirty": "A" * 300 + "N" * 358,  # exactly 658, 54% N
        "clean": "ACGT" * 160,                        # 640
        "far": "ACGT" * 50,
    }
    assert refalign.select_reference(seqs, 658) == "clean"


def test_select_reference_falls_back_when_nothing_is_clean():
    seqs = {"a": "N" * 600, "b": "N" * 650}
    assert refalign.select_reference(seqs, 658) == "b"


# -- aligner --------------------------------------------------------------


def test_aligner_end_gaps_are_free_and_internal_gaps_are_not():
    """Biopython's open/extend gap setters also overwrite the end-gap scores,
    so the order they are set in matters -- this guards against that
    regressing (it was wrong in the prototype script this module came
    from)."""
    aligner = refalign.build_aligner()
    for side in ("left", "right"):
        for kind in ("insertion", "deletion"):
            assert getattr(aligner, f"open_{side}_{kind}_score") == 0.0
            assert getattr(aligner, f"extend_{side}_{kind}_score") == 0.0
    assert aligner.open_internal_insertion_score == -10
    assert aligner.open_internal_deletion_score == -10


# -- anchor_to_reference --------------------------------------------------


@pytest.mark.parametrize("left,right", [(15, 0), (0, 20), (10, 12), (150, 200)])
def test_overhangs_fall_outside_the_reference_row(core, left, right):
    rng = _rng()
    seq = _random_seq(rng, left) + _mutate(rng, core, 0.02) + _random_seq(rng, right)
    a = _anchor(seq, core)
    assert (a.ref_start, a.ref_end) == (0, 658)
    assert len(a.row) == 658
    assert a.identity > 0.95
    assert not a.flags


def test_short_fragment_keeps_its_own_positions_only(core):
    seq = core[50:350]
    a = _anchor(seq, core)
    assert (a.ref_start, a.ref_end) == (50, 350)
    assert (a.row[:50] == refalign.MISSING).all()
    assert (a.row[350:] == refalign.MISSING).all()
    assert (a.row[50:350] == refalign.encode(seq)).all()
    assert a.identity == pytest.approx(1.0)
    assert any(f.startswith("short:") for f in a.flags)


def test_unrelated_sequence_is_flagged_but_still_returned(core):
    a = _anchor(_random_seq(random.Random(99), 500), core)
    assert a.identity < 0.6
    assert any("low identity" in f for f in a.flags)
    assert not a.reverse_complemented, "junk fails both ways; don't call it flipped"
    assert len(a.row) == 658


def test_reverse_complement_is_recovered(core):
    seq = refalign.reverse_complement(_mutate(_rng(), core, 0.02))
    a = _anchor(seq, core)
    assert a.reverse_complemented
    assert a.identity > 0.95
    assert any("reverse-complemented" in f for f in a.flags)
    assert not any("low identity" in f for f in a.flags)


def test_ambiguity_codes_become_missing_sites(core):
    seq = core[:100] + "N" + core[101:]
    a = _anchor(seq, core)
    assert a.row[100] == refalign.MISSING
    assert (a.row[:100] == refalign.encode(core[:100])).all()


def test_empty_sequence_is_flagged_not_crashed(core):
    a = _anchor("", core)
    assert (a.row == refalign.MISSING).all()
    assert a.flags == ["no alignable bases"]


# -- anchor_all -----------------------------------------------------------


def test_anchor_all_keeps_insertion_order_and_the_reference_row(core):
    rng = _rng()
    seqs = {
        "short": core[:300],
        "gappy_ref": "--" + core + "--",
        "long": _random_seq(rng, 40) + core + _random_seq(rng, 40),
    }
    messages = []
    ref, anchored = refalign.anchor_all(seqs, target_length=658,
                                        progress=messages.append, **MIN)
    assert ref == "gappy_ref"
    assert [a.name for a in anchored] == list(seqs)
    ref_row = anchored[1].row
    assert (ref_row == refalign.encode(core)).all()
    assert (anchored[2].row == ref_row).all()
    assert messages and messages[-1].endswith("3/3")
