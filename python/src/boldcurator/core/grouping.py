"""Splitting a BAGS grade into the units a curator actually works through.

A grade is not one table.  Grade C is "this species has more than one BIN" and
grade E is "this BIN holds more than one species" -- in both, the *problem* is a
single species-BIN combination, and a flat table of every grade-C record mixes
dozens of unrelated problems together.  So each grade is split into groups, one
per problem, and the curator takes them one at a time:

=======  ==========================  ==============================================
Grade    One group per               Caption
=======  ==========================  ==============================================
A, B, D  species                     ``Species: X (>10 specimens, single BIN)``
C        species x BIN               ``Species: X - BIN: Y``
E        shared BIN                  ``Shared BIN: Y (2 species)``
=======  ==========================  ==============================================

**Grades E and C are the ones that matter**, because they are the ones where
the barcode and the name disagree; A, B and D are mostly confirmation.

**Non-species-level records ride along by BIN membership.**  A record identified
only to genus, or as ``Danaus cf. plexippus``, carries no BAGS grade of its own,
but if it sits in a grade-E BIN it is part of the problem and the curator has to
see it -- it may be the misidentification, or the evidence that the BIN is fine.
Ported from ``organize_grade_specimens`` (``mod_bags_grading_utils.R:32-149``)
and the filter at ``mod_bags_grading_server.R:54-87``, with the unified species
rule (``core.species``) in place of R's five.

**What counts as a species here** is ``core.species.name_status``: a resolved
name, or an interim species name (``Danaus cf. plexippus``, ``Danaus sp. 1``)
recorded at species rank, which is graded as a species of its own. Records
identified to genus or higher ride along.

**Unnamed BINs (screen ``UNNAMED``, not a BAGS grade).**  Riding along needs
a graded BIN to ride in, so a BIN with no species-level record at all
appeared on no BAGS screen. :func:`unnamed_bin_groups` gives each its own
group, discordant ones (more than one genus, family or order) first.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable

import pandas as pd

from .bags import CONFLICT_RANKS, discordant_bins
from .species import (
    column_or_missing,
    is_empty_text,
    is_species_level,
    to_text,
)

GRADES = ("A", "B", "C", "D", "E")

#: The screen key for unnamed BINs. Shares the grade screens' machinery
#: (``group_specimens``, the app's group navigator) but is not a BAGS grade,
#: so it is deliberately not in ``GRADES``.
UNNAMED = "U"

UNNAMED_DESCRIPTION = ("BINs with no species-level name (every record "
                       "identified to genus or higher), discordant ones first")

#: How many distinct identifications an unnamed-BIN caption lists before
#: summarising the rest as "+N more".
_CAPTION_NAMES = 3

#: What each grade means, in the words the screen shows.  A, B and D read as
#: descriptions; C and E read as problems, which is what they are.
GRADE_DESCRIPTIONS: dict[str, str] = {
    "A": "More than 10 specimens, all in one BIN",
    "B": "Three to ten specimens, all in one BIN",
    "C": "Species split across more than one BIN",
    "D": "Fewer than three specimens",
    "E": "Species sharing a BIN with another species",
}

#: The parenthetical on a group caption, so a group carries its own criterion
#: without the curator having to remember which tab they are on.
_GRADE_QUALIFIER: dict[str, str] = {
    "A": ">10 specimens, single BIN",
    "B": "3-10 specimens, single BIN",
    "D": "<3 specimens, single BIN",
}

#: The grades where the barcode and the name disagree, so the curator's
#: attention belongs here first.
PRIORITY_GRADES = ("E", "C")

#: Group by species, not by BIN.
SPECIES_GRADES = ("A", "B", "D")


@dataclass(frozen=True)
class SpecimenGroup:
    """One problem: a species, or a BIN, or a species within a BIN."""

    key: str
    caption: str
    specimens: pd.DataFrame
    species: tuple[str, ...] = ()
    bins: tuple[str, ...] = ()
    #: Set when the grade says this BIN is shared but the sharing species is
    #: not in this search's results, so the group looks innocent on screen,
    #: or to name the genera (families, orders) a discordant BIN mixes.
    note: str = ""
    #: An unnamed BIN whose records come from more than one genus, family or
    #: order. (Grade E groups are discordant by definition and leave it unset.)
    discordant: bool = False

    @property
    def specimen_count(self) -> int:
        return len(self.specimens)

    @property
    def species_count(self) -> int:
        return len(self.species)


def _text(frame: pd.DataFrame, column: str) -> pd.Series:
    return to_text(column_or_missing(frame, column)).str.strip()


def specimens_for_grade(specimens: pd.DataFrame, grades: pd.DataFrame,
                        grade: str) -> pd.DataFrame:
    """Every record a grade's screen should show.

    Every species-level, **BIN-assigned** record of the grade's species, plus
    every non-species-level record sharing one of those species' BINs. The
    second half is the point: those records have no grade of their own but
    they are part of the problem being looked at.

    Round 7, BAGS analysis item 1: a BIN-less record must not count toward a
    grade at all -- ``core.bags.calculate_bags_grades`` now drops it before
    counting anything, on the project owner's explicit instruction (a
    deliberate divergence from R, which counts it; see that module's own
    docstring). This function has to agree, or the same "graded on records
    the curator never actually sees" mismatch round 6 fixed the other
    direction would reappear here, just flipped: a BIN-less record excluded
    from the *count* but still shown in the *group* would inflate what looks
    like the group's own size past what actually earned the grade.
    ``has_bin`` is therefore required for a record to be a "core" member of
    its species' group, same as it always was for finding **riders**: a
    BIN-less record has no BIN to share with anything, so it can only ever
    be judged as part of its own species (and now, not even that).
    """
    if specimens is None or len(specimens) == 0 or grades is None or not len(grades):
        return specimens.iloc[:0] if specimens is not None else pd.DataFrame()

    wanted = set(grades.loc[grades["bags_grade"] == grade, "species"])
    if not wanted:
        return specimens.iloc[:0]

    species = _text(specimens, "species")
    bins = _text(specimens, "bin_uri")
    species_level = is_species_level(specimens).to_numpy()
    has_bin = (bins != "").to_numpy()

    is_grade = species.isin(wanted).to_numpy() & species_level & has_bin
    grade_bins = set(bins[is_grade])

    riders = bins.isin(grade_bins).to_numpy() & has_bin & ~species_level
    chosen = specimens[is_grade | riders]
    return chosen.drop_duplicates(subset=["processid"], keep="first") \
        if "processid" in chosen.columns else chosen


def group_specimens(specimens: pd.DataFrame, grades: pd.DataFrame,
                    grade: str, *, shared_bins: frozenset[str] | set[str] | None = None,
                    fetch_bin: Callable[[str], pd.DataFrame] | None = None,
                    ) -> list[SpecimenGroup]:
    """Split a grade into one group per problem, best specimens first.

    ``shared_bins`` and ``fetch_bin`` matter only for grade E -- see
    :func:`_shared_bin_groups`.
    """
    if grade == UNNAMED:
        return unnamed_bin_groups(specimens)
    if grade not in GRADES:
        raise ValueError(f"Unknown BAGS grade {grade!r}; expected one of {GRADES}")

    frame = specimens_for_grade(specimens, grades, grade)
    if len(frame) == 0:
        return []
    frame = frame.reset_index(drop=True)

    species = _text(frame, "species")
    bins = _text(frame, "bin_uri")
    species_level = pd.Series(is_species_level(frame).to_numpy(), index=frame.index)
    named = species_level & (species != "")

    if grade in SPECIES_GRADES:
        return _species_groups(frame, species, bins, named, grade)
    if grade == "C":
        return _species_bin_groups(frame, species, bins, named)
    return _shared_bin_groups(frame, species, bins, named, grades,
                              shared_bins=shared_bins, fetch_bin=fetch_bin)


def _sorted(frame: pd.DataFrame) -> pd.DataFrame:
    score = pd.to_numeric(column_or_missing(frame, "quality_score"),
                          errors="coerce").fillna(-1)
    pid = _text(frame, "processid")
    order = pd.DataFrame({"s": score.to_numpy(), "p": pid.to_numpy()},
                         index=frame.index)
    order = order.sort_values(["s", "p"], ascending=[False, True], kind="stable")
    return frame.loc[order.index].reset_index(drop=True)


_RANK_PLURALS = {"genus": "genera", "family": "families", "order": "orders"}


def rank_conflicts(frame: pd.DataFrame) -> dict[str, list[str]]:
    """``rank -> distinct values`` for each of genus, family and order that has
    more than one value among ``frame``'s records -- the per-group view of
    ``core.bags.taxonomic_conflict_bins``."""
    out: dict[str, list[str]] = {}
    for rank in CONFLICT_RANKS:
        values = _text(frame, rank)
        distinct = sorted({v for v, empty in zip(values, is_empty_text(values))
                           if not empty})
        if len(distinct) > 1:
            out[rank] = distinct
    return out


def conflict_note(conflicts: dict[str, list[str]]) -> str:
    """"records from more than one genus: Danaus, Pieris" -- the lowest
    conflicting rank, which is the most specific thing to say."""
    rank, values = next(iter(conflicts.items()))
    return f"records from more than one {rank}: {', '.join(values)}"


def conflict_counts(n_species: int, conflicts: dict[str, list[str]]) -> str:
    """"2 species" or "1 species, 2 genera" for a caption."""
    text = f"{n_species} species"
    if conflicts:
        rank, values = next(iter(conflicts.items()))
        text += f", {len(values)} {_RANK_PLURALS[rank]}"
    return text


def unnamed_bin_groups(specimens: pd.DataFrame) -> list[SpecimenGroup]:
    """One group per BIN with no species-level record -- every record in it is
    identified to genus or higher (``core.species.NAME_HIGHER``) -- with all
    of that BIN's records.

    Discordant BINs (records from more than one genus, family or order) come
    first, then concordant ones, each in BIN order. A BIN with any
    species-level record is left out: that name is graded, so the BIN is on a
    BAGS screen already, discordant ones on grade E.
    """
    if specimens is None or len(specimens) == 0:
        return []
    frame = specimens.reset_index(drop=True)
    bins = _text(frame, "bin_uri")
    has_bin = bins != ""
    level = pd.Series(is_species_level(frame).to_numpy(), index=frame.index)
    wanted = set(bins[has_bin]) - set(bins[level & has_bin])
    if not wanted:
        return []

    ident = _text(frame, "identification")
    in_wanted = bins.isin(wanted)
    discordant, concordant = [], []
    for bin_uri, members in frame[in_wanted].groupby(bins[in_wanted], sort=True):
        conflicts = rank_conflicts(members)
        names = sorted(set(ident[members.index]) - {""})
        shown = ", ".join(names[:_CAPTION_NAMES]) or "unidentified"
        if len(names) > _CAPTION_NAMES:
            shown += f" +{len(names) - _CAPTION_NAMES} more"
        status = "Discordant" if conflicts else "Concordant"
        group = SpecimenGroup(
            key=f"{UNNAMED}|{bin_uri}",
            caption=f"{status} BIN: {bin_uri} — {shown}",
            specimens=_sorted(members),
            bins=(bin_uri,),
            note=conflict_note(conflicts) if conflicts else "",
            discordant=bool(conflicts),
        )
        (discordant if conflicts else concordant).append(group)
    return discordant + concordant


def _species_groups(frame, species, bins, named, grade) -> list[SpecimenGroup]:
    groups: list[SpecimenGroup] = []
    for name in sorted(set(species[named])):
        core = named & (species == name)
        own_bins = set(bins[core]) - {""}
        riders = ~named & bins.isin(own_bins) & (bins != "")
        members = _sorted(frame[core | riders])
        qualifier = _GRADE_QUALIFIER.get(grade, "")
        groups.append(SpecimenGroup(
            key=f"{grade}|{name}",
            caption=f"Species: {name}" + (f" ({qualifier})" if qualifier else ""),
            specimens=members,
            species=(name,),
            bins=tuple(sorted(own_bins)),
        ))
    return groups


def _species_bin_groups(frame, species, bins, named) -> list[SpecimenGroup]:
    groups: list[SpecimenGroup] = []
    for name in sorted(set(species[named])):
        core = named & (species == name)
        for bin_uri in sorted(set(bins[core]) - {""}):
            in_bin = bins == bin_uri
            members = _sorted(frame[(core & in_bin) | (~named & in_bin)])
            groups.append(SpecimenGroup(
                key=f"C|{name}|{bin_uri}",
                caption=f"Species: {name} — BIN: {bin_uri}",
                specimens=members,
                species=(name,),
                bins=(bin_uri,),
            ))
    return groups


def _shared_bin_groups(frame, species, bins, named, grades, *,
                       shared_bins=None, fetch_bin=None) -> list[SpecimenGroup]:
    """One group per BIN that is *itself* shared -- not per grade-E species.

    A species is graded E if **any** of its BINs is shared, so the frame this
    receives can contain a species' other, unshared BIN too (that is the
    ``BOLD:AAL6477`` bug: a single-species BIN showing up as "Shared" only
    because the same species also sits in a genuinely shared BIN elsewhere).
    ``shared_bins`` -- the same set ``bags.calculate_bags_grades`` used for
    ``has_shared_bins`` -- is what tells the two apart; a BIN not in it gets no
    group at all, however many grade-E species happen to pass through it.

    R keeps a BIN only when more than one species-level name appears *in the
    downloaded records*.  Grade E here is evaluated against the whole snapshot
    (``core.bags``), so a BIN can be genuinely shared while the other species is
    absent from this search.  Dropping those would hide the very records the
    grade exists to flag, so ``fetch_bin`` -- when given -- pulls the rest of
    the BIN's specimens straight from the snapshot instead of hiding them
    behind a note.
    """
    if shared_bins is None:
        shared_bins = discordant_bins(frame)
    shared_bins = set(shared_bins)

    groups: list[SpecimenGroup] = []
    for bin_uri in sorted(set(bins[bins != ""]) & shared_bins):
        in_bin = bins == bin_uri
        members = frame[in_bin]
        note = ""

        local_names = set(species[in_bin & named]) - {""}
        if (len(local_names) < 2 and not rank_conflicts(members)
                and fetch_bin is not None):
            extra = fetch_bin(bin_uri)
            if extra is not None and len(extra) and "processid" in members.columns:
                known = set(_text(members, "processid"))
                extra = extra[~_text(extra, "processid").isin(known)]
            if extra is not None and len(extra):
                members = pd.concat([members, extra], ignore_index=True)
                note = ("includes records outside this search's taxa or "
                        "geography, because this BIN is shared with another "
                        "species in the snapshot")

        member_species = _text(members, "species")
        member_level = pd.Series(is_species_level(members).to_numpy(),
                                 index=members.index)
        names = tuple(sorted(set(member_species[member_level & (member_species != "")])
                             - {""}))
        conflicts = rank_conflicts(members)
        outside = bool(note)
        if len(names) < 2 and not conflicts and not note:
            outside = True
            note = ("shared with a species outside this search — BAGS grades "
                    "sharing against the whole snapshot, not just these records")
        if conflicts:
            note = "; ".join(filter(None, [note, conflict_note(conflicts)]))
        label = (f"Shared BIN: {bin_uri} ({conflict_counts(len(names), conflicts)}"
                 f"{' here' if outside else ''})")
        groups.append(SpecimenGroup(
            key=f"E|{bin_uri}",
            caption=label,
            specimens=_sorted(members),
            species=names,
            bins=(bin_uri,),
            note=note,
        ))
    return groups
