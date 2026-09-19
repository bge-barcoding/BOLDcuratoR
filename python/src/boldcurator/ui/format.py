"""Presentation: the colours and column choices that make a problem visible.

The R app colour-codes BAGS grades and concordance, and that is not decoration
-- a curator scanning a checklist is looking for the red rows. The palette is
kept here, in one place, so every screen agrees.
"""

from __future__ import annotations

from urllib.parse import quote

import pandas as pd

from shiny import ui

from ..config.constants import BOLD_ATTRIBUTION_TEXT, CC_BY_SA_URL

__all__ = [
    "BOLD_ATTRIBUTION_TEXT", "CC_BY_SA_URL", "BOLD_ATTRIBUTION_SHORT",
]

#: The public BOLD portal -- no API key, no login, works from an offline
#: snapshot's data because it is just a link, not a fetch.
BOLD_PORTAL = "https://portal.boldsystems.org"

#: The short form for the header bar; ``BOLD_ATTRIBUTION_TEXT`` (the full
#: wording) lives in ``config.constants`` so ``io.exports`` can stamp it onto
#: exports without depending on the GUI layer.
BOLD_ATTRIBUTION_SHORT = "Data: BOLD Systems, CC BY-SA 4.0"


def bold_record_url(processid: str) -> str:
    """One specimen record, e.g. ``GBMHO3680-19``."""
    return f"{BOLD_PORTAL}/record/{quote(str(processid))}"


def bold_bin_url(bin_uri: str) -> str:
    """Every record BOLD holds in this BIN, e.g. ``BOLD:AAJ5773``.

    The query syntax itself (``:``, ``[bin]``) is not percent-encoded --
    matching the portal's own URLs exactly, which do not encode it either.
    """
    return f"{BOLD_PORTAL}/result?query={bin_uri}[bin]"


def bold_species_url(species: str) -> str:
    """Every record BOLD holds for this species, quoted for an exact match.

    The quotes and the name are encoded (``%22``, ``%20``...); the trailing
    ``[tax]`` is not, again matching the portal's own URLs.
    """
    quoted_name = quote('"' + species + '"')
    return f"{BOLD_PORTAL}/result?query={quoted_name}[tax]"

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

GAP_STATUS_COLOURS = {"Found": "#28a745", "Missing": "#dc3545"}

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

GAP_LABELS = {
    "input_taxon": "Taxon typed", "status": "Status",
    "matched_species": "Matched species", "specimen_count": "Specimens",
    "notes": "Notes",
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
