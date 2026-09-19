"""Curator annotations: selection, flags, updated IDs and notes.

Ported from ``R/utils/annotation_utils.R`` and the four stores in
``R/modules/state/state_manager.R:171-174``.  Each is a mapping from
``processid`` to a small record carrying the value plus who set it and when;
deleting an annotation removes the key, exactly as assigning ``NULL`` does in R.
"""

from __future__ import annotations

import datetime as _dt
import re
from dataclasses import dataclass, field
from typing import Any

import pandas as pd

from ..config.constants import FLAG_OPTIONS

#: The six columns annotation merging contributes to an export, in the order R
#: puts them (``merge_annotations_for_export``, ``annotation_utils.R:235-238``).
ANNOTATION_COLUMNS = (
    "selected",
    "flag",
    "updated_id",
    "curator_notes",
    "flag_user",
    "flag_timestamp",
)

_HTML = re.compile(r"<[^>]+>")


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


@dataclass
class Annotations:
    """The four annotation stores, keyed by ``processid``."""

    selected: dict[str, dict] = field(default_factory=dict)
    flags: dict[str, dict] = field(default_factory=dict)
    updated_ids: dict[str, dict] = field(default_factory=dict)
    curator_notes: dict[str, dict] = field(default_factory=dict)

    # -- mutation ----------------------------------------------------------

    def set_selected(self, processid: str, *, user: str = "", **extra) -> None:
        self.selected[processid] = {
            "timestamp": _now(), "user": user, "selected": True, **extra
        }

    def unset_selected(self, processid: str) -> None:
        self.selected.pop(processid, None)

    def set_flag(self, processid: str, flag: str, *, user: str = "",
                 species: str = "") -> None:
        if flag not in FLAG_OPTIONS:
            raise ValueError(
                f"Unknown flag {flag!r}; expected one of {sorted(FLAG_OPTIONS)}"
            )
        # An empty flag clears the annotation, as the R dropdown's "None" does.
        if not flag:
            self.flags.pop(processid, None)
            return
        self.flags[processid] = {
            "flag": flag, "timestamp": _now(), "user": user, "species": species
        }

    def set_updated_id(self, processid: str, text: str, *, user: str = "") -> None:
        if not (text or "").strip():
            self.updated_ids.pop(processid, None)
            return
        self.updated_ids[processid] = {
            "text": text.strip(), "timestamp": _now(), "user": user
        }

    def set_note(self, processid: str, text: str, *, user: str = "") -> None:
        if not (text or "").strip():
            self.curator_notes.pop(processid, None)
            return
        self.curator_notes[processid] = {
            "text": text.strip(), "timestamp": _now(), "user": user
        }

    # -- queries -----------------------------------------------------------

    @property
    def is_empty(self) -> bool:
        return not (self.selected or self.flags or self.updated_ids
                    or self.curator_notes)

    def annotated_processids(self) -> set[str]:
        """Records carrying a flag, an updated ID or a note.

        Selection alone does **not** count as annotated -- R's
        ``download_annotated`` uses exactly these three stores
        (``mod_specimen_handling_server.R:420``), because auto-selection would
        otherwise mark every representative as curator-annotated.
        """
        return set(self.flags) | set(self.updated_ids) | set(self.curator_notes)

    def selected_processids(self) -> set[str]:
        return set(self.selected)

    def manual_selections(self) -> dict[str, dict]:
        return {k: v for k, v in self.selected.items() if not v.get("auto_selected")}

    # -- projection --------------------------------------------------------

    def to_frame(self, processids: list[str]) -> pd.DataFrame:
        """The six annotation columns, aligned to the given processids."""
        return pd.DataFrame(
            {
                "processid": processids,
                "selected": [p in self.selected for p in processids],
                "flag": [_value(self.flags.get(p), "flag", "value")
                         for p in processids],
                "updated_id": [_value(self.updated_ids.get(p), "text", "value")
                               for p in processids],
                "curator_notes": [_value(self.curator_notes.get(p), "text", "note",
                                         "value") for p in processids],
                "flag_user": [_value(self.flags.get(p), "user") for p in processids],
                "flag_timestamp": [_value(self.flags.get(p), "timestamp")
                                   for p in processids],
            }
        )

    # -- serialisation -----------------------------------------------------

    def to_dict(self) -> dict[str, dict]:
        return {
            "selected": self.selected,
            "flags": self.flags,
            "updated_ids": self.updated_ids,
            "curator_notes": self.curator_notes,
        }

    @classmethod
    def from_dict(cls, data: dict[str, dict] | None) -> "Annotations":
        data = data or {}
        return cls(
            selected=dict(data.get("selected") or {}),
            flags=dict(data.get("flags") or {}),
            updated_ids=dict(data.get("updated_ids") or {}),
            curator_notes=dict(data.get("curator_notes") or {}),
        )


def merge_annotations(frame: pd.DataFrame, annotations: Annotations) -> pd.DataFrame:
    """Attach the six annotation columns and move them to the front.

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
    annotation_frame = annotations.to_frame(processids).drop(columns=["processid"])
    for column in ANNOTATION_COLUMNS:
        out[column] = annotation_frame[column].to_numpy()

    ordered = list(ANNOTATION_COLUMNS) + [
        c for c in out.columns if c not in ANNOTATION_COLUMNS
    ]
    return out[ordered]
