"""The flag scheme (BOLDcuration_flag_options.docx): what each flag needs,
how taxon updates reach records, and that a record's own update wins."""

from __future__ import annotations

import pytest

from boldcurator.config.constants import ID_FLAGS, REQUIRED_FIELDS, TAXON_FLAGS
from boldcurator.io.annotations import AnnotationForm, Annotations, guess_rank

CURATOR = "Ann Curator"
RECORDS = {"P1": "Aus bis", "P2": "Aus bis", "P3": "Cus dus"}


def _complete(flag: str) -> AnnotationForm:
    """A form with everything ``flag`` could need filled in."""
    return AnnotationForm(id_flag=flag, new_id="Aus bus", rank="species",
                          method="Morphology", citation="Smith 2019",
                          note="why", curator=CURATOR)


@pytest.mark.parametrize("flag", sorted(ID_FLAGS))
def test_each_required_field_is_enforced(flag):
    a = Annotations()
    records = {"P1": "Aus bis"}
    assert a.validate(_complete(flag), records) == []
    for name in REQUIRED_FIELDS[flag]:
        form = _complete(flag)
        setattr(form, name, "")
        errors = a.validate(form, records)
        assert len(errors) == 1 and "required" in errors[0], (flag, name)


def test_the_method_is_not_asked_for_a_taxon_update():
    for flag in TAXON_FLAGS:
        assert "method" not in REQUIRED_FIELDS[flag]
    form = _complete("synonym")
    form.method = ""
    assert Annotations().validate(form, {"P1": "Aus bis"}) == []


def test_a_note_is_required_for_misidentification_id_unfounded_and_other_issue():
    a = Annotations()
    for form in (_complete("misidentification"), _complete("id_unfounded"),
                 AnnotationForm(other_issue=True, curator=CURATOR)):
        form.note = ""
        assert any("Curator note is required" in e
                   for e in a.validate(form, {"P1": "Aus bis"}))
    for flag in TAXON_FLAGS:
        form = _complete(flag)
        form.note = ""
        assert a.validate(form, {"P1": "Aus bis"}) == []


def test_contamination_needs_the_sequence_taxon():
    a = Annotations()
    form = AnnotationForm(contamination=True, curator=CURATOR)
    assert a.validate(form, {"P1": "Aus bis"}) == [
        "Sequence taxon is required for contamination."]
    form.sequence_taxon = "Homo sapiens"
    assert a.validate(form, {"P1": "Aus bis"}) == []


def test_the_curator_gives_a_full_name():
    a = Annotations()
    form = AnnotationForm(other_issue=True, note="lat/long swapped")
    assert a.validate(form, {"P1": "x"}) == ["Enter your full name as the curator."]
    form.curator = "Ann"
    assert "first and last" in a.validate(form, {"P1": "x"})[0]


def test_unknown_ranks_and_methods_are_refused():
    form = _complete("misidentification")
    form.rank, form.method = "clade", "Guesswork"
    errors = Annotations().validate(form, {"P1": "Aus bis"})
    assert "Unknown rank 'clade'." in errors
    assert "Unknown identification method 'Guesswork'." in errors


def test_a_taxon_update_corrects_one_name_to_another():
    a = Annotations()
    errors = a.validate(_complete("name_issue"), RECORDS)
    assert any("2 different identifications" in e for e in errors)
    same = _complete("name_issue")
    same.new_id = "Aus bis"
    assert any("same as the current name" in e
               for e in a.validate(same, {"P1": "Aus bis"}))


def test_a_taxon_update_is_stored_once_not_on_the_records():
    a = Annotations()
    a.apply(_complete("synonym"), {"P1": "Aus bis", "P2": "Aus bis"})
    assert set(a.taxon_updates) == {"Aus bis"}
    assert a.taxon_updates["Aus bis"]["citation"] == "Smith 2019"
    assert a.taxon_updates["Aus bis"]["user"] == CURATOR
    assert not a.flags and not a.updated_ids
    # ... and reaches records nobody checked.
    assert a.resolve("P9", "Aus bis").updated_id == "Aus bus"
    assert a.resolve("P9", "Aus bis").source == "taxon (synonym)"
    assert a.resolve("P9", "Cus dus").updated_id == ""


@pytest.mark.parametrize("own_first", [True, False])
def test_a_records_own_update_wins_over_a_taxon_update(own_first):
    """Requirement 5: sp. A → sp. B as a synonym must not overwrite a
    misidentification of sp. A to taxon C -- whichever came first."""
    a = Annotations()
    misid = _complete("misidentification")
    misid.new_id = "Cus dus"
    steps = [lambda: a.apply(misid, {"P1": "Aus bis"}),
             lambda: a.apply(_complete("synonym"), {"P2": "Aus bis"})]
    for step in (steps if own_first else reversed(steps)):
        step()
    own = a.resolve("P1", "Aus bis")
    assert (own.updated_id, own.source, own.flag) == \
        ("Cus dus", "record", "misidentification")
    assert a.resolve("P2", "Aus bis").updated_id == "Aus bus"


def test_a_record_corrected_to_a_junior_name_follows_its_taxon_update():
    """Agreed: chained, and shown as both steps."""
    a = Annotations()
    misid = _complete("misidentification")
    misid.new_id = "Aus bis"
    a.apply(misid, {"P3": "Cus dus"})
    a.apply(_complete("synonym"), {"P1": "Aus bis"})
    resolved = a.resolve("P3", "Cus dus")
    assert resolved.updated_id == "Aus bus"
    assert resolved.source == "record → taxon (synonym)"
    assert resolved.flag == "misidentification"
    assert resolved.method == "Morphology" and resolved.citation == "Smith 2019"


def test_taxon_updates_chain_and_a_loop_ends():
    a = Annotations()
    a.set_taxon_update("Aus bis", "name_issue", "Aus bus")
    a.set_taxon_update("Aus bus", "synonym", "Aus cus")
    assert a.resolve("P1", "Aus bis").updated_id == "Aus cus"
    assert a.resolve("P1", "Aus bis").source == \
        "taxon (name_issue) → taxon (synonym)"
    a.set_taxon_update("Aus cus", "synonym", "Aus bis")
    assert a.resolve("P1", "Aus bis").updated_id == "Aus bis"   # stops, no hang


def test_annotated_records_include_those_a_taxon_update_reaches():
    a = Annotations()
    a.set_taxon_update("Aus bis", "synonym", "Aus bus")
    a.set_note("P3", "check")
    assert a.annotated_processids() == {"P3"}
    assert a.annotated_processids(RECORDS) == {"P1", "P2", "P3"}


def test_records_that_differ_keep_their_own_unless_the_form_sets_it():
    a = Annotations()
    a.apply(_complete("misidentification"), {"P1": "Aus bis"})
    a.apply(AnnotationForm(other_issue=True, note="lat/long", curator=CURATOR),
            {"P2": "Aus bis"})
    form, uniform = a.form_for({"P1": "Aus bis", "P2": "Aus bis"})
    assert not uniform and form == AnnotationForm()
    # Tick contamination on both: nothing else changes.
    a.apply(AnnotationForm(contamination=True, sequence_taxon="Homo sapiens",
                           curator=CURATOR), {"P1": "Aus bis", "P2": "Aus bis"})
    assert a.flags["P1"]["flag"] == "misidentification"
    assert a.updated_ids["P1"]["text"] == "Aus bus"
    assert a.flags["P2"]["other_issue"] is True
    assert a.curator_notes["P2"]["text"] == "lat/long"
    assert a.flags["P1"]["contamination"] and a.flags["P2"]["contamination"]


def test_the_form_for_one_record_starts_from_what_it_has():
    a = Annotations()
    a.apply(_complete("misidentification"), {"P1": "Aus bis"})
    form, uniform = a.form_for({"P1": "Aus bis"})
    assert uniform
    assert (form.id_flag, form.new_id, form.rank, form.method, form.note) == \
        ("misidentification", "Aus bus", "species", "Morphology", "why")
    a.set_taxon_update("Cus dus", "synonym", "Cus eus", citation="X 2001")
    form, _ = a.form_for({"P3": "Cus dus"})
    assert (form.id_flag, form.new_id, form.citation) == ("synonym", "Cus eus",
                                                          "X 2001")


def test_choosing_none_removes_the_records_own_id_issue():
    a = Annotations()
    a.apply(_complete("misidentification"), {"P1": "Aus bis"})
    a.apply(AnnotationForm(id_flag="", contamination=False, other_issue=False,
                           note="", curator=CURATOR), {"P1": "Aus bis"})
    assert not a.flags and not a.updated_ids and not a.curator_notes


@pytest.mark.parametrize("name, rank", [
    ("Aus bus", "species"), ("Aus", "genus"), ("Nymphalidae", "family"),
    ("Danainae", "subfamily"), ("Danaini", "tribe"), ("Aus bus cus", "subspecies"),
    ("Aus sp. 3", "species"), ("Aus cf. bus", "species"), ("", ""),
    ("lowercase", ""),
])
def test_guess_rank(name, rank):
    assert guess_rank(name) == rank


def test_a_taxon_update_leaves_who_flagged_a_record_alone():
    a = Annotations()
    a.apply(_complete("misidentification"), {"P1": "Aus bis"})
    stamped = dict(a.flags["P1"])
    other = _complete("synonym")
    other.curator = "Bob Other"
    a.apply(other, {"P1": "Aus bis"})
    assert a.flags["P1"] == stamped


def test_a_form_that_changes_nothing_is_refused():
    errors = Annotations().validate(AnnotationForm(curator=CURATOR), {"P1": "x"})
    assert errors == ["Nothing to apply -- choose an ID issue, tick an issue or "
                      "write a note."]


def test_every_change_bumps_the_revision():
    """Regraded views are cached per revision, so anything that can change a
    grade or a group's marks must bump it."""
    a = Annotations()
    steps = [
        lambda: a.apply(_complete("misidentification"), {"P1": "Aus bis"}),
        lambda: a.set_note("P2", "x"),
        lambda: a.set_taxon_update("Cus dus", "synonym", "Cus eus"),
        lambda: a.remove_taxon_update("Cus dus"),
        lambda: a.clear_record("P1"),
        lambda: a.set_reviewed("Aus bis"),
    ]
    for step in steps:
        before = a.revision
        step()
        assert a.revision > before


def test_reviewed_marks_are_saved_and_copied():
    from boldcurator.io.annotations import group_unit

    a = Annotations()
    a.set_reviewed(group_unit("CE|Aus bis"), user=CURATOR, screen="CE")
    a.set_reviewed(group_unit("E|BOLD:AAA1000"), user=CURATOR, screen="E")
    assert set(a.reviewed) == {"Aus bis", "BOLD:AAA1000"}
    assert a.reviewed["Aus bis"]["user"] == CURATOR
    assert Annotations.from_dict(a.to_dict()).reviewed == a.reviewed
    copy = a.snapshot()
    a.set_reviewed("Aus bis", False)
    assert "Aus bis" in copy.reviewed and "Aus bis" not in a.reviewed


def test_a_group_unit_survives_a_change_of_screen():
    from boldcurator.io.annotations import group_unit

    assert group_unit("CE|Aus bus") == group_unit("B|Aus bus") == "Aus bus"
    assert group_unit("C|Aus bus|BOLD:AAA1") == "Aus bus|BOLD:AAA1"
