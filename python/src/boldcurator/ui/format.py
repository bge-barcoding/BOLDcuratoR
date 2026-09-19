"""Presentation: the colours and column choices that make a problem visible.

The R app colour-codes BAGS grades and concordance, and that is not decoration
-- a curator scanning a checklist is looking for the red rows. The palette is
kept here, in one place, so every screen agrees.
"""

from __future__ import annotations

import pandas as pd

from shiny import ui

#: BAGS grade colours, matching the R app so the two are readable side by side.
#: E and C are the ones that need work, so they are the ones that shout.
GRADE_COLOURS: dict[str, str] = {
    "A": "#28a745",   # green  -- well supported
    "B": "#17a2b8",   # teal   -- supported, fewer specimens
    "C": "#f0ad4e",   # amber  -- species split across BINs
    "D": "#868e96",   # grey   -- too few specimens to say
    "E": "#dc3545",   # red    -- BIN shared with another species
}

CONCORDANCE_COLOURS = {"Concordant": "#28a745", "Discordant": "#dc3545"}

#: The columns a curator works with, in the order the R app shows them:
#: annotations first, because that is what they are here to change.
#: ``selected`` and ``checked`` are two different checkboxes -- see
#: ``io.annotations``'s module docstring -- so both are always shown together.
GROUP_COLUMNS = [
    "selected", "checked", "flag", "updated_id", "curator_notes",
    "rank", "quality_score", "processid", "bin_uri",
    "species", "bags_grade", "identification", "identified_by", "country.ocean",
]

#: Headers for ``GROUP_COLUMNS`` (and the specimen table's ``PREVIEW_COLUMNS``,
#: a subset of the same names). "Rep." and "Check" carry the distinction
#: ``GROUP_COLUMNS`` documents -- one is the persistent representative pick,
#: the other a disposable bulk-edit selection.
GROUP_LABELS = {
    "selected": "Rep.", "checked": "Check", "flag": "Flag",
    "updated_id": "Updated ID", "curator_notes": "Notes", "rank": "Rank",
    "quality_score": "Score", "processid": "Process ID", "bin_uri": "BIN",
    "species": "Species", "bags_grade": "BAGS", "identification": "ID",
    "identified_by": "Identified by", "country.ocean": "Country/Ocean",
    "inst": "Institution",
}

CHECKLIST_LABELS = {
    "species": "Species", "specimen_count": "Specimens", "bin_count": "BINs",
    "bin_uris": "BIN URIs", "bags_grade": "BAGS", "countries": "Countries",
    "mean_quality_score": "Mean quality",
}

BIN_LABELS = {
    "bin_uri": "BIN", "total_records": "Records", "unique_species": "Species",
    "species_list": "Species list", "countries": "Countries",
    "concordance": "Concordance", "bin_coverage": "Share of result",
}


def value_box(value: str, label: str, colour: str) -> ui.Tag:
    return ui.div(
        ui.div(value, style="font-size:30px;font-weight:700;line-height:1.1;"),
        ui.div(label, style="font-size:13px;opacity:.9;"),
        style=f"background:{colour};color:#fff;border-radius:6px;padding:12px 16px;"
              "min-width:150px;",
    )


def present(frame: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    """The columns that exist, in the order asked for.

    Silently dropping an absent column beats erroring: a snapshot built without
    an optional column should still render every screen.
    """
    return frame[[c for c in columns if c in frame.columns]]
