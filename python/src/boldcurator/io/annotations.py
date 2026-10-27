"""Curator annotations: selection, flags, updated IDs, notes and taxon updates.

Ported from ``R/utils/annotation_utils.R`` and the four stores in
``R/modules/state/state_manager.R:171-174``.  Each is a mapping from
``processid`` to a small record carrying the value plus who set it and when;
deleting an annotation removes the key, exactly as assigning ``NULL`` does in R.

**``selected`` vs ``working`` -- two different things share the word
"selected" and must not share a store.**  ``selected`` is the *representative*
pick: what :mod:`core.selection` auto-fills (best specimen per BIN x country)
and what "Download Selected" exports, meant to persist for the life of the
result.  R's own per-row checkbox (``mod_bags_grading_server.R``'s
``input$specimen_select``) *is* this, and R never had a second kind --  its
flag/note/updated-ID are separate per-row inline inputs, committed
individually, not gathered by selecting rows first. This app's bulk "apply to
selection" toolbar is a substitute for that (a static HTML table cannot easily
carry live per-cell inputs the way a DT widget can), and it needs its own,
disposable selection -- ``working`` -- so that clearing it, or checking a
different handful of rows to flag next, can never silently drop a curator's
representative picks. ``working`` is UI scratch space: it is not exported and
not written to a saved session (:mod:`io.session`).

**Record flags and taxon updates** (BOLDcuration_flag_options.docx). A record
carries at most one *ID flag* (``config.constants.ID_FLAGS``) plus two
independent yes/no issues, contamination and other issue. Of the ID flags,
misidentification and id_unfounded change one record, so they are stored on it
(``flags`` and ``updated_ids``). Name issue and synonym change the *taxon*
record in BOLD, so they are stored once, keyed by the name being corrected
(``taxon_updates``), and reach every record carrying that name when the
annotations are read (:meth:`Annotations.resolve`) -- never copied onto the
records. That is what keeps a taxon update from overwriting a record's own:
a record's own corrected ID always wins, made before the taxon update or
after, and if the record's own ID is itself a name with a taxon update, that
update is followed too ("record → taxon (synonym)").
"""

from __future__ import annotations

import datetime as _dt
import re
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from typing import Any

import pandas as pd

from ..config.constants import (
    FIELD_LABELS,
    ID_FLAGS,
    ID_METHODS,
    ID_RANKS,
    ISSUE_TICKS,
    LEGACY_FLAG_MAP,
    REQUIRED_FIELDS,
    TAXON_FLAGS,
)

#: The columns annotation merging contributes to an export, in order.
#: ``updated_id`` is the record's *effective* new identification: its own,
#: or a taxon update's, or its own followed by a taxon update -- and
#: ``update_source`` says which. ``updated_id_method`` is named apart from the
#: snapshot's own ``identification_method``, which says how the *current* ID
#: was made.
ANNOTATION_COLUMNS = (
    "selected",
    "id_flag",
    "contamination",
    "other_issue",
    "updated_id",
    "updated_id_rank",
    "updated_id_method",
    "update_source",
    "sequence_taxon",
    "citation",
    "curator_notes",
    "curator",
    "annotation_timestamp",
)

_HTML = re.compile(r"<[^>]+>")

#: Words that mark an interim or uncertain name ("Aus sp. 3", "Aus cf. bus").
_INTERIM = {"sp", "spp", "cf", "aff", "nr", "gr", "complex", "group"}


def _now() -> str:
    return _dt.datetime.now().isoformat(timespec="seconds")


def _value(entry: Any, *fields: str, default: str = "") -> str:
    """``extract_annotation`` (``annotation_utils.R:28``).

    Entries are normally dicts but may be bare strings from older sessions, so
    both shapes are accepted.
    """
    if entry is None:
        return default
    if isinstance(entry, str):
        return entry
    if isinstance(entry, dict):
        for name in fields:
            if name in entry and entry[name] is not None:
                return str(entry[name])
    return default


def _text(value) -> str:
    return (value or "").strip() if isinstance(value, str) or value is None \
        else str(value).strip()


def guess_rank(name: str) -> str:
    """The rank a name most likely has, for the form to suggest: the
    curator still confirms it. ``""`` when there is no good guess."""
    words = (name or "").split()
    if not words:
        return ""
    if any(w.lower().rstrip(".") in _INTERIM for w in words[1:]):
        return "species"
    if len(words) == 1:
        word = words[0]
        if word.endswith("idae"):
            return "family"
        if word.endswith("inae"):
            return "subfamily"
        if word.endswith("ini"):
            return "tribe"
        return "genus" if word[:1].isupper() else ""
    if len(words) == 3 and words[1].islower() and words[2].islower():
        return "subspecies"
    return "species"


@dataclass
class AnnotationForm:
    """What the annotation form asks to change, for one or many records.

    ``None`` means *keep each record's own* -- what a form over records with
    different annotations uses for the parts the curator leaves alone. A form
    over one record (or records that all agree) is explicit throughout, so
    ``""`` or ``False`` there removes.
    """

    #: An ``ID_FLAGS`` key, ``""`` for no ID issue, or ``None`` to keep.
    id_flag: str | None = None
    new_id: str = ""
    rank: str = ""
    method: str = ""
    citation: str = ""
    contamination: bool | None = None
    sequence_taxon: str = ""
    other_issue: bool | None = None
    note: str | None = None
    curator: str = ""


@dataclass(frozen=True)
class Resolution:
    """A record's effective new identification (see ``Annotations.resolve``)."""

    updated_id: str = ""
    rank: str = ""
    method: str = ""
    citation: str = ""
    #: ``record``, ``taxon (synonym)``, ``record → taxon (synonym)``...
    source: str = ""
    #: The ID flag behind it: the record's own, else the first taxon update's.
    flag: str = ""
    #: The taxon updates followed, in order (their keys).
    taxa: tuple[str, ...] = ()


@dataclass
class Annotations:
    """The annotation stores: four keyed by ``processid``, one by name."""

    selected: dict[str, dict] = field(default_factory=dict)
    #: ``{"flag", "contamination", "other_issue", "sequence_taxon", "user",
    #: "timestamp"}`` -- ``flag`` is a record-level ID flag or ``""``.
    flags: dict[str, dict] = field(default_factory=dict)
    #: ``{"text", "rank", "method", "user", "timestamp"}``.
    updated_ids: dict[str, dict] = field(default_factory=dict)
    curator_notes: dict[str, dict] = field(default_factory=dict)
    #: Keyed by the name being corrected: ``{"flag", "new_name", "rank",
    #: "citation", "note", "user", "timestamp"}``.
    taxon_updates: dict[str, dict] = field(default_factory=dict)
    #: The temporary "checked, to flag/note/update in bulk" set. See the
    #: module docstring -- this is deliberately not ``selected``.
    working: set[str] = field(default_factory=set)
    #: Records whose retired flag was converted on load (``from_dict``), so
    #: the app can say so once. Not saved.
    converted: int = field(default=0, compare=False)

    # -- mutation ----------------------------------------------------------

    def set_selected(self, processid: str, *, user: str = "", **extra) -> None:
        self.selected[processid] = {
            "timestamp": _now(), "user": user, "selected": True, **extra
        }

    def unset_selected(self, processid: str) -> None:
        self.selected.pop(processid, None)

    def set_working(self, processid: str, *, selected: bool = True) -> None:
        """Check or uncheck a record for the bulk-edit toolbar."""
        if selected:
            self.working.add(processid)
        else:
            self.working.discard(processid)

    def clear_working(self) -> None:
        self.working.clear()

    def set_flag(self, processid: str, flag: str = "", *, user: str = "",
                 contamination: bool = False, other_issue: bool = False,
                 sequence_taxon: str = "") -> None:
        """Set a record's ID flag and issues outright; all empty removes them.

        ``flag`` must be a record-level ID flag (or ``""``): name issue and
        synonym belong to the name, via :meth:`set_taxon_update`.
        """
        if flag and (flag not in ID_FLAGS or flag in TAXON_FLAGS):
            raise ValueError(
                f"Unknown record flag {flag!r}; expected one of "
                f"{sorted(set(ID_FLAGS) - TAXON_FLAGS)} -- name issue and "
                "synonym are taxon updates")
        if not (flag or contamination or other_issue):
            self.flags.pop(processid, None)
            return
        self.flags[processid] = {
            "flag": flag, "contamination": bool(contamination),
            "other_issue": bool(other_issue),
            "sequence_taxon": _text(sequence_taxon) if contamination else "",
            "timestamp": _now(), "user": user,
        }

    def set_updated_id(self, processid: str, text: str, *, rank: str = "",
                       method: str = "", user: str = "") -> None:
        if not _text(text):
            self.updated_ids.pop(processid, None)
            return
        self.updated_ids[processid] = {
            "text": _text(text), "rank": _text(rank), "method": _text(method),
            "timestamp": _now(), "user": user,
        }

    def set_note(self, processid: str, text: str, *, user: str = "") -> None:
        if not _text(text):
            self.curator_notes.pop(processid, None)
            return
        self.curator_notes[processid] = {
            "text": _text(text), "timestamp": _now(), "user": user
        }

    def set_taxon_update(self, name: str, flag: str, new_name: str, *,
                         rank: str = "", citation: str = "", note: str = "",
                         user: str = "") -> None:
        if flag not in TAXON_FLAGS:
            raise ValueError(f"{flag!r} is not a taxon flag; expected one of "
                             f"{sorted(TAXON_FLAGS)}")
        self.taxon_updates[_text(name)] = {
            "flag": flag, "new_name": _text(new_name), "rank": _text(rank),
            "citation": _text(citation), "note": _text(note),
            "timestamp": _now(), "user": user,
        }

    def remove_taxon_update(self, name: str) -> None:
        self.taxon_updates.pop(name, None)

    def clear_record(self, processid: str) -> None:
        """Remove a record's own flag, issues, corrected ID and note. A taxon
        update for its name is not the record's, so stays."""
        self.flags.pop(processid, None)
        self.updated_ids.pop(processid, None)
        self.curator_notes.pop(processid, None)

    # -- the annotation form -----------------------------------------------

    def validate(self, form: AnnotationForm,
                 identifications: Mapping[str, str]) -> list[str]:
        """Why ``form`` can't be applied to these records (processid ->
        current identification), in plain English; empty when it can."""
        errors: list[str] = []
        curator = _text(form.curator)
        if not curator:
            errors.append("Enter your full name as the curator.")
        elif len(curator.split()) < 2:
            errors.append("Enter your full name (first and last) as the "
                          "curator.")
        if not identifications:
            errors.append("No records checked.")
        if (form.id_flag is None and form.contamination is None
                and form.other_issue is None and form.note is None):
            errors.append("Nothing to apply -- choose an ID issue, tick an "
                          "issue or write a note.")
        if form.id_flag and form.id_flag not in ID_FLAGS:
            errors.append(f"Unknown ID issue {form.id_flag!r}.")
            return errors

        needed: list[tuple[str, str]] = []   # (field, what it is for)
        if form.id_flag:
            needed += [(f, form.id_flag) for f in REQUIRED_FIELDS[form.id_flag]]
        if form.contamination:
            needed += [(f, "contamination") for f in REQUIRED_FIELDS["contamination"]]
        if form.other_issue:
            needed += [(f, "other_issue") for f in REQUIRED_FIELDS["other_issue"]]
        for name, why in needed:
            if not _text(getattr(form, name)):
                label = (ID_FLAGS[form.id_flag]["new_id"] if name == "new_id"
                         else FIELD_LABELS[name])
                reason = (ID_FLAGS.get(why) or ISSUE_TICKS[why])["label"]
                errors.append(f"{label} is required for {reason.lower()}.")
        if form.id_flag and form.rank and form.rank not in ID_RANKS:
            errors.append(f"Unknown rank {form.rank!r}.")
        if (form.id_flag and form.id_flag not in TAXON_FLAGS and form.method
                and form.method not in ID_METHODS):
            errors.append(f"Unknown identification method {form.method!r}.")

        if form.id_flag in TAXON_FLAGS and identifications:
            names = sorted({_text(n) for n in identifications.values()})
            if len(names) > 1:
                errors.append(
                    f"{ID_FLAGS[form.id_flag]['label']} corrects one name, but "
                    f"the checked records have {len(names)} different "
                    "identifications. Check only records with the same name.")
            elif not names[0]:
                errors.append("The checked records have no identification "
                              "to correct.")
            elif _text(form.new_id) and _text(form.new_id) == names[0]:
                errors.append(f"{ID_FLAGS[form.id_flag]['new_id']} is the "
                              "same as the current name.")
        return errors

    def apply(self, form: AnnotationForm,
              identifications: Mapping[str, str]) -> None:
        """Apply a validated ``form`` to these records (processid -> current
        identification). Call :meth:`validate` first."""
        user = _text(form.curator)
        if form.id_flag in TAXON_FLAGS:
            name = _text(next(iter(identifications.values()), ""))
            self.set_taxon_update(name, form.id_flag, form.new_id,
                                  rank=form.rank, citation=form.citation,
                                  note=form.note or "", user=user)
        for pid in identifications:
            entry = self.flags.get(pid) or {}
            flag = entry.get("flag", "") if isinstance(entry, dict) else ""
            contamination = bool(entry.get("contamination")) \
                if isinstance(entry, dict) else False
            other_issue = bool(entry.get("other_issue")) \
                if isinstance(entry, dict) else False
            sequence_taxon = entry.get("sequence_taxon", "") \
                if isinstance(entry, dict) else ""

            # A taxon update leaves a record's own ID flag and corrected ID
            # alone: they win over it, which is the point.
            if form.id_flag is not None and form.id_flag not in TAXON_FLAGS:
                flag = form.id_flag
                if flag:
                    self.set_updated_id(pid, form.new_id, rank=form.rank,
                                        method=form.method, user=user)
                else:
                    self.updated_ids.pop(pid, None)
            if form.contamination is not None:
                contamination = form.contamination
                sequence_taxon = form.sequence_taxon if contamination else ""
            if form.other_issue is not None:
                other_issue = form.other_issue
            # Only what changed is re-stamped with who and when: a taxon
            # update, or a note, leaves the record's flags as they were set.
            before = (entry.get("flag", ""), bool(entry.get("contamination")),
                      bool(entry.get("other_issue")),
                      entry.get("sequence_taxon", "")) \
                if isinstance(entry, dict) and entry else None
            after = (flag, bool(contamination), bool(other_issue),
                     _text(sequence_taxon) if contamination else "")
            if after != before:
                self.set_flag(pid, flag, user=user, contamination=contamination,
                              other_issue=other_issue,
                              sequence_taxon=sequence_taxon)
            if form.note is not None:
                self.set_note(pid, form.note, user=user)

    def form_for(self, identifications: Mapping[str, str]) -> tuple[AnnotationForm, bool]:
        """What the form should start with for these records, and whether
        they all agree. Records that differ get a form that keeps each
        record's own (``None`` throughout)."""
        forms: list[AnnotationForm] = []
        for pid, ident in identifications.items():
            form = self._record_form(pid, ident)
            if form not in forms:
                forms.append(form)
            if len(forms) > 1:
                return AnnotationForm(), False
        return (forms[0] if forms else AnnotationForm(id_flag="",
                contamination=False, other_issue=False, note="")), True

    def _record_form(self, pid: str, identification: str) -> AnnotationForm:
        entry = self.flags.get(pid)
        entry = entry if isinstance(entry, dict) else {}
        own = self.updated_ids.get(pid)
        own = own if isinstance(own, dict) else {}
        form = AnnotationForm(
            id_flag=entry.get("flag", "") or "",
            new_id=_value(own, "text", "value"), rank=own.get("rank", ""),
            method=own.get("method", ""),
            contamination=bool(entry.get("contamination")),
            sequence_taxon=entry.get("sequence_taxon", ""),
            other_issue=bool(entry.get("other_issue")),
            note=_value(self.curator_notes.get(pid), "text", "note", "value"),
        )
        rule = self.taxon_updates.get(_text(identification))
        if not form.id_flag and not form.new_id and rule:
            form.id_flag = rule["flag"]
            form.new_id = rule.get("new_name", "")
            form.rank = rule.get("rank", "")
            form.citation = rule.get("citation", "")
        return form

    # -- queries -----------------------------------------------------------

    @property
    def is_empty(self) -> bool:
        return not (self.selected or self.flags or self.updated_ids
                    or self.curator_notes or self.taxon_updates)

    def annotated_processids(
            self, identifications: Mapping[str, str] | None = None) -> set[str]:
        """Records carrying a flag, an updated ID or a note -- and, given
        their identifications (processid -> name), those a taxon update
        reaches.

        Selection alone does **not** count as annotated -- R's
        ``download_annotated`` uses exactly these three stores
        (``mod_specimen_handling_server.R:420``), because auto-selection would
        otherwise mark every representative as curator-annotated.
        """
        own = set(self.flags) | set(self.updated_ids) | set(self.curator_notes)
        if identifications and self.taxon_updates:
            own |= {str(p) for p, name in identifications.items()
                    if _text(name) in self.taxon_updates}
        return own

    def selected_processids(self) -> set[str]:
        return set(self.selected)

    def working_processids(self) -> set[str]:
        return set(self.working)

    def manual_selections(self) -> dict[str, dict]:
        return {k: v for k, v in self.selected.items() if not v.get("auto_selected")}

    def resolve(self, processid: str, identification: str = "") -> Resolution:
        """A record's effective new identification: its own corrected ID if
        it has one, else its current identification -- then any taxon
        updates for that name, followed in turn. Empty when nothing changes
        it."""
        own = self.updated_ids.get(processid)
        entry = self.flags.get(processid)
        own_flag = entry.get("flag", "") if isinstance(entry, dict) else (
            entry if isinstance(entry, str) else "")
        steps: list[str] = []
        taxa: list[str] = []
        name, rank, method, citation, flag = _text(identification), "", "", "", ""
        if own is not None and _value(own, "text", "value"):
            name = _value(own, "text", "value")
            if isinstance(own, dict):
                rank, method = own.get("rank", ""), own.get("method", "")
            steps.append("record")
            flag = own_flag
        while name in self.taxon_updates and name not in taxa:
            rule = self.taxon_updates[name]
            taxa.append(name)
            steps.append(f"taxon ({rule['flag']})")
            flag = flag or rule["flag"]
            name, rank = rule.get("new_name", ""), rule.get("rank", "")
            citation = rule.get("citation", "") or citation
        if not steps:
            return Resolution(flag=own_flag)
        return Resolution(updated_id=name, rank=rank, method=method,
                          citation=citation, source=" → ".join(steps),
                          flag=flag, taxa=tuple(taxa))

    # -- projection --------------------------------------------------------

    def to_frame(self, processids: list[str],
                 identifications: Iterable[str] | None = None) -> pd.DataFrame:
        """``ANNOTATION_COLUMNS``, aligned to the given processids.

        ``identifications`` (each record's current identification, aligned
        the same way) is what lets taxon updates reach records; without it
        only each record's own annotations show.
        """
        idents = list(identifications) if identifications is not None \
            else [""] * len(processids)
        rows = []
        for pid, ident in zip(processids, idents):
            ident = _text(ident) if isinstance(ident, str) else ""
            entry = self.flags.get(pid)
            entry = entry if isinstance(entry, dict) else (
                {"flag": entry} if isinstance(entry, str) else {})
            resolved = self.resolve(pid, ident)
            contamination = bool(entry.get("contamination"))
            updated = resolved.updated_id or (ident if contamination else "")
            note = self.curator_notes.get(pid)
            stamped = [e for e in (entry, self.updated_ids.get(pid), note,
                                   *(self.taxon_updates[t] for t in resolved.taxa))
                       if isinstance(e, dict) and e.get("timestamp")]
            latest = max(stamped, key=lambda e: e["timestamp"], default={})
            rows.append((
                pid in self.selected, resolved.flag, contamination,
                bool(entry.get("other_issue")), updated, resolved.rank,
                resolved.method, resolved.source,
                entry.get("sequence_taxon", "") or "", resolved.citation,
                _value(note, "text", "note", "value"),
                latest.get("user", ""), latest.get("timestamp", ""),
            ))
        frame = pd.DataFrame(rows, columns=list(ANNOTATION_COLUMNS))
        frame.insert(0, "processid", list(processids))
        return frame

    def taxon_frame(self, identifications: Iterable[str] | None = None) -> pd.DataFrame:
        """One row per taxon update, with how many of these records (their
        current identifications) it reaches."""
        counts = pd.Series([_text(n) for n in identifications or []
                            if isinstance(n, str)]).value_counts()
        rows = [
            {"old_name": name, "flag": rule["flag"],
             "new_name": rule.get("new_name", ""), "rank": rule.get("rank", ""),
             "citation": rule.get("citation", ""), "note": rule.get("note", ""),
             "curator": rule.get("user", ""), "timestamp": rule.get("timestamp", ""),
             "records_affected": int(counts.get(name, 0))}
            for name, rule in sorted(self.taxon_updates.items())
        ]
        return pd.DataFrame(rows, columns=list(TAXON_UPDATE_COLUMNS))

    def snapshot(self) -> "Annotations":
        """A copy to read on another thread while this one keeps changing
        (round 10, item 3: a download prepared in the background). Each
        ``dict()`` copy is one atomic step under the GIL."""
        return Annotations(selected=dict(self.selected), flags=dict(self.flags),
                           updated_ids=dict(self.updated_ids),
                           curator_notes=dict(self.curator_notes),
                           taxon_updates=dict(self.taxon_updates),
                           working=set(self.working))

    # -- serialisation -----------------------------------------------------

    def to_dict(self) -> dict[str, dict]:
        return {
            "selected": self.selected,
            "flags": self.flags,
            "updated_ids": self.updated_ids,
            "curator_notes": self.curator_notes,
            "taxon_updates": self.taxon_updates,
        }

    @classmethod
    def from_dict(cls, data: dict[str, dict] | None) -> "Annotations":
        """Load saved stores, converting flags from earlier versions
        (``LEGACY_FLAG_MAP``) to today's shape and counting the ones whose
        meaning changed in ``converted``."""
        data = data or {}
        flags: dict[str, dict] = {}
        converted = 0
        for pid, entry in (data.get("flags") or {}).items():
            entry = dict(entry) if isinstance(entry, dict) else {"flag": entry}
            old = _value(entry, "flag", "value")
            entry.setdefault("contamination", False)
            entry.setdefault("other_issue", False)
            entry.setdefault("sequence_taxon", "")
            target = LEGACY_FLAG_MAP.get(old)
            if target in ISSUE_TICKS:
                entry["flag"] = ""
                entry[target] = True
                converted += old != "contamination"
            elif target:
                entry["flag"] = target
                converted += 1
            else:
                entry["flag"] = old
            entry.pop("value", None)
            flags[pid] = entry
        return cls(
            selected=dict(data.get("selected") or {}),
            flags=flags,
            updated_ids=dict(data.get("updated_ids") or {}),
            curator_notes=dict(data.get("curator_notes") or {}),
            taxon_updates=dict(data.get("taxon_updates") or {}),
            converted=converted,
        )


#: The taxon updates report (``Annotations.taxon_frame``).
TAXON_UPDATE_COLUMNS = (
    "old_name", "flag", "new_name", "rank", "citation", "note", "curator",
    "timestamp", "records_affected",
)


def merge_annotations(frame: pd.DataFrame, annotations: Annotations) -> pd.DataFrame:
    """Attach the annotation columns and move them to the front.

    Also collapses list-valued cells with ``"; "`` and strips HTML from text
    columns, both of which ``merge_annotations_for_export``
    (``annotation_utils.R:181-238``) does -- the HTML strip is a safety net
    against the R table renderers' ``<div class="cell-content">`` wrappers
    leaking into an export.
    """
    if frame is None or len(frame) == 0:
        return frame if frame is not None else pd.DataFrame()

    out = frame.copy()
    for column in out.columns:
        if out[column].dtype == object:
            out[column] = [
                "; ".join(str(v) for v in value)
                if isinstance(value, (list, tuple, set))
                else (_HTML.sub("", value) if isinstance(value, str) else value)
                for value in out[column]
            ]

    processids = [str(p) for p in out.get("processid", pd.Series(dtype=object))]
    idents = (out["identification"].tolist() if "identification" in out.columns
              else None)
    annotation_frame = annotations.to_frame(processids, idents) \
        .drop(columns=["processid"])
    for column in ANNOTATION_COLUMNS:
        out[column] = annotation_frame[column].to_numpy()

    ordered = list(ANNOTATION_COLUMNS) + [
        c for c in out.columns if c not in ANNOTATION_COLUMNS
    ]
    return out[ordered]
