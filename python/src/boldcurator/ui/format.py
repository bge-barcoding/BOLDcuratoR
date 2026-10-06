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
    Everything else in the BIN is: it comes from the snapshot file, and an
    unencoded quote or space would let a crafted value break out of the
    link's ``href``. A real BIN (``BOLD:`` plus letters and digits) comes
    out unchanged.
    """
    return f"{BOLD_PORTAL}/result?query={quote(str(bin_uri), safe=':')}[bin]"


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

#: The unnamed-BINs screen (``core.grouping.UNNAMED``): not a grade, so not
#: one of the grade colours.
UNNAMED_COLOUR = "#5a6f8a"

#: The columns a curator works with, in the order the R app shows them:
#: annotations first, because that is what they are here to change.
#: ``selected`` and ``checked`` are two different checkboxes -- see
#: ``io.annotations``'s module docstring -- so both are always shown together.
GROUP_COLUMNS = [
    "selected", "checked", "flag", "updated_id", "curator_notes",
    "rank", "quality_score", "processid", "bin_uri",
    "species", "name_status", "identification_rank", "bags_grade",
    "identification", "identified_by", "country.ocean",
]

#: Headers for ``GROUP_COLUMNS`` (and the columns of the same names among the
#: specimen table's full column set -- see ``ui.app._all_columns_ordered``).
#: "Rep." and "Check" carry the distinction
#: ``GROUP_COLUMNS`` documents -- one is the persistent representative pick,
#: the other a disposable bulk-edit selection.
GROUP_LABELS = {
    "selected": "Rep.", "checked": "Check", "flag": "Flag",
    "updated_id": "Updated ID", "curator_notes": "Notes", "rank": "Rank",
    "quality_score": "Score", "processid": "Process ID", "bin_uri": "BIN",
    "species": "Species", "name_status": "Name status",
    "identification_rank": "ID rank", "bags_grade": "BAGS", "identification": "ID",
    "identified_by": "Identified by", "country.ocean": "Country/Ocean",
    "inst": "Institution",
}

#: ``mean_quality_score`` is a real column of ``build_species_checklist``'s
#: output but has no entry here -- round 3, item 4 dropped it from the
#: on-screen checklist and the xlsx export as noise nobody asked to see, not
#: from the underlying data (other callers, e.g. tests, still get it).
CHECKLIST_LABELS = {
    "species": "Species", "name_status": "Name", "specimen_count": "Specimens",
    "bin_count": "BINs",
    "bin_uris": "BIN URIs", "bags_grade": "BAGS", "c_plus_e": "C+E",
    "countries": "Countries",
}

BIN_LABELS = {
    "bin_uri": "BIN", "total_records": "Records", "unique_species": "Species",
    "species_list": "Species list", "countries": "Countries",
    "concordance": "Concordance",
}

GAP_LABELS = {
    "input_taxon": "Taxon typed", "status": "Status",
    "matched_species": "Matched species", "specimen_count": "Specimens",
    "notes": "Notes",
}


def value_box(value: str, label: str, colour: str, *, compact: bool = False) -> ui.Tag:
    """A coloured count tile. ``compact`` tiles share their row equally and
    shrink to fit it -- round 8, item 2.1: the Species tab's eight tiles on
    one row -- wrapping a long label onto a second line rather than widening."""
    if compact:
        return ui.div(
            ui.div(value, style="font-size:22px;font-weight:700;line-height:1.1;"),
            ui.div(label, style="font-size:12px;opacity:.9;line-height:1.2;"),
            style=f"background:{colour};color:#fff;border-radius:6px;"
                  "padding:6px 10px;flex:1 1 0;min-width:0;max-width:170px;",
        )
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


#: Round 8, items 1.2 / 4.1, shared by the app and the first-run setup
#: screen: label-less (toolbar) inputs line up with the buttons beside them.
INLINE_INPUT_CSS = """
/* Round 8, items 1.2 / 4.1: toolbar inputs sat about half a
   rem above the buttons beside them. Shiny wraps every input in
   a .form-group with margin-bottom:1rem, and a flex row's
   align-items:center centres the box *with* that margin; the
   inputs were also full-height next to btn-sm buttons. Every
   label-less input is a toolbar one (a labelled input stacks
   its label above it and keeps the spacing), so: no margin, and
   btn-sm's height. The attribute selector is the fallback for a
   webview without :has(). */
.shiny-input-container:has(> .shiny-label-null),
[style*="display:flex"] > .shiny-input-container {
    margin-bottom: 0;
}
.shiny-input-container:has(> .shiny-label-null) .form-control,
.shiny-input-container:has(> .shiny-label-null) .form-select,
[style*="display:flex"] > .shiny-input-container .form-control,
[style*="display:flex"] > .shiny-input-container .form-select {
    padding-top: .25rem; padding-bottom: .25rem;
    padding-left: .5rem; font-size: .875rem;
    min-height: calc(1.5em + .5rem + 2px);
    border-radius: var(--bs-border-radius-sm, .25rem);
}
"""
