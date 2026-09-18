"""The end-to-end search pipeline.

search -> BIN expansion -> geographic filter -> process -> score -> rank ->
BAGS -> BIN analysis -> auto-selection.

Mirrors the order in ``mod_data_import_server.R`` and the pipeline observer at
``app.R:378-419``.  The ordering is not arbitrary: the geographic filter runs on
the seed, *before* expansion, and is not re-applied afterwards.
"""

from __future__ import annotations

import datetime as _dt
from dataclasses import dataclass, field

import pandas as pd

from ..config.constants import CONTINENT_COUNTRIES, DOWNLOAD_LIMITS
from ..data.queries import (
    Resolution,
    SearchQuery,
    estimate_search,
    missing_recordset_codes,
    resolve_taxa,
    search_specimens,
)
from ..data.snapshot import SnapshotStore
from . import bags, bins, selection
from .ranking import score_and_rank
from .species import column_or_missing, is_empty, to_text


# --------------------------------------------------------------------------
# Input parsing
# --------------------------------------------------------------------------


def parse_taxa_input(text: str) -> list[list[str]]:
    """Parse the taxa textarea into synonym groups.

    One group per line; within a line, comma-separated names where the **first**
    is the valid name and the rest are synonyms.  The groups are kept because
    gap analysis reports against the name the user actually typed.
    """
    groups: list[list[str]] = []
    for line in (text or "").splitlines():
        names = [n.strip() for n in line.split(",") if n.strip()]
        if names:
            groups.append(names)
    return groups


def flatten_taxa(groups: list[list[str]]) -> list[str]:
    """The flat, de-duplicated list of names actually searched."""
    return list(dict.fromkeys(name for group in groups for name in group))


def parse_lines(text: str) -> list[str]:
    """One value per line, trimmed, blanks dropped, de-duplicated."""
    return list(dict.fromkeys(
        line.strip() for line in (text or "").splitlines() if line.strip()
    ))


def countries_for_continents(continents: list[str]) -> list[str]:
    """Expand continent names to their country lists."""
    out: list[str] = []
    for continent in continents or []:
        out.extend(CONTINENT_COUNTRIES.get(continent, []))
    return list(dict.fromkeys(out))


def geographic_filter(countries: list[str], continents: list[str]) -> list[str]:
    """The **union** of the continent expansion and the explicit country list.

    A union, not an intersection -- ``mod_data_import_server.R:227``.  Ticking
    Europe and also typing "Canada" gives you both, which is what users expect
    and is easy to get backwards.
    """
    return list(dict.fromkeys([*countries_for_continents(continents), *(countries or [])]))


# --------------------------------------------------------------------------
# Processing
# --------------------------------------------------------------------------


def process_specimen_data(frame: pd.DataFrame) -> pd.DataFrame:
    """``process_specimen_data`` (``mod_data_import_utils.R:152-214``), unified.

    Trims and blanks the three fields the app keys on, stamps provenance, then
    de-duplicates on ``processid`` and sorts.  The species rule applied here is
    the unified one -- see ``core.species`` for what that changes.
    """
    from .species import normalise_species

    if frame is None or len(frame) == 0:
        return frame if frame is not None else pd.DataFrame()

    out = frame.copy()
    if "species" in out.columns:
        out["species"] = normalise_species(out["species"])
    for column in ("bin_uri", "country.ocean"):
        if column in out.columns:
            values = out[column]
            out[column] = to_text(values).str.strip().mask(is_empty(values), pd.NA)

    out["data_source"] = "BOLD snapshot"
    out["import_date"] = _dt.datetime.now().isoformat(timespec="seconds")

    if "processid" in out.columns:
        out = out.drop_duplicates(subset=["processid"], keep="first")
        out = out.sort_values("processid", kind="stable")
    return out.reset_index(drop=True)


# --------------------------------------------------------------------------
# Result
# --------------------------------------------------------------------------


@dataclass
class SearchResult:
    specimens: pd.DataFrame
    bags_grades: pd.DataFrame
    bin_analysis: dict
    selections: dict
    resolution: Resolution
    estimate: dict
    taxonomy_groups: list[list[str]] = field(default_factory=list)
    missing_codes: list[str] = field(default_factory=list)
    geographic_filter: list[str] = field(default_factory=list)
    snapshot_id: str = ""
    warnings: list[str] = field(default_factory=list)

    @property
    def record_count(self) -> int:
        return len(self.specimens)

    def summary(self) -> dict[str, object]:
        df = self.specimens
        bin_uri = column_or_missing(df, "bin_uri")
        species = column_or_missing(df, "species")
        country = column_or_missing(df, "country.ocean")
        return {
            "records": len(df),
            "species": int(species[~is_empty(species)].nunique()) if len(df) else 0,
            "bins": int(bin_uri[~is_empty(bin_uri)].nunique()) if len(df) else 0,
            "countries": int(country[~is_empty(country)].nunique()) if len(df) else 0,
            "selected": len(self.selections),
            "snapshot_id": self.snapshot_id,
        }


class SizeLimitExceeded(RuntimeError):
    """Raised when a search would exceed the hard cap.

    With the API gone these limits are memory guards, not rate guards: the cost
    of materialising and scoring the frame in this process is now the binding
    constraint.
    """

    def __init__(self, estimate: dict[str, int]):
        self.estimate = estimate
        super().__init__(
            f"This search would return {estimate['expanded_records']:,} records "
            f"across {estimate['seed_bins']:,} BINs, over the limit of "
            f"{DOWNLOAD_LIMITS['MAX_RECORDS']:,} records / "
            f"{DOWNLOAD_LIMITS['MAX_BINS']:,} BINs. Narrow the taxa or add a "
            "geographic filter."
        )


# --------------------------------------------------------------------------
# Run
# --------------------------------------------------------------------------


def run_search(
    store: SnapshotStore,
    *,
    taxa_text: str = "",
    countries: list[str] | None = None,
    continents: list[str] | None = None,
    dataset_codes: list[str] | None = None,
    project_codes: list[str] | None = None,
    expand_bins: bool = True,
    limit: int | None = None,
    enforce_limits: bool = True,
    auto_select: bool = True,
) -> SearchResult:
    groups = parse_taxa_input(taxa_text)
    names = flatten_taxa(groups)
    geo = geographic_filter(countries or [], continents or [])
    datasets = [c for c in (dataset_codes or []) if c]
    projects = [c for c in (project_codes or []) if c]

    warnings: list[str] = []
    resolution = resolve_taxa(store, names) if names else Resolution()
    if resolution.unmatched:
        warnings.append(
            "No records for: " + ", ".join(resolution.unmatched)
            + ". These names are absent from the snapshot -- check the spelling, "
            "or they may only exist on unpublished records, which a public "
            "snapshot cannot contain."
        )
    for name, options in resolution.ambiguous.items():
        warnings.append(
            f"{name!r} is ambiguous: "
            + ", ".join(f"{o.rank} ({o.n_records:,} records)" for o in options)
            + ". All of them were searched."
        )

    query = SearchQuery(
        taxa=resolution.resolved,
        countries=geo,
        dataset_codes=datasets,
        project_codes=projects,
        expand_bins=expand_bins,
        limit=limit,
    )
    if query.is_empty():
        raise ValueError(
            "Nothing to search: no taxon resolved and no dataset/project code given."
        )

    missing = missing_recordset_codes(store, datasets + projects)
    if missing:
        warnings.append(
            "No records for these dataset/project codes: " + ", ".join(missing)
            + ". Offline there is no authentication error to distinguish a typo "
            "from a code covering private records -- both return nothing."
        )

    estimate = estimate_search(store, query)
    if enforce_limits and limit is None:
        if (estimate["expanded_records"] > DOWNLOAD_LIMITS["MAX_RECORDS"]
                or estimate["seed_bins"] > DOWNLOAD_LIMITS["MAX_BINS"]):
            raise SizeLimitExceeded(estimate)
    if estimate["expanded_records"] > DOWNLOAD_LIMITS["WARN_RECORDS"]:
        warnings.append(
            f"Large result: {estimate['expanded_records']:,} records. "
            "Scoring and the results table will be slow."
        )

    frame = search_specimens(store, query)
    frame = process_specimen_data(frame)
    frame = score_and_rank(frame)

    # Only the BINs this result actually touches. Loading the whole
    # bin_species table cost seconds and gigabytes on EVERY search, however
    # small -- it dominated a 183-record search in the 2026-09-11 benchmark.
    # bin_species is sorted by bin_uri, so an IN predicate prunes well.
    result_bins = sorted({
        b for b in to_text(column_or_missing(frame, "bin_uri")).str.strip() if b
    })
    if result_bins:
        placeholders = ", ".join("?" for _ in result_bins)
        bin_species = store.connection.execute(
            f"SELECT * FROM bin_species WHERE bin_uri IN ({placeholders})",
            result_bins,
        ).df()
    else:
        bin_species = None
    grades = bags.calculate_bags_grades(frame, bin_species=bin_species)
    if len(grades):
        frame = frame.merge(
            grades[["species", "bags_grade"]], on="species", how="left"
        )

    analysis = bins.analyse_bins(frame)
    selections = selection.auto_select_best_specimens(frame) if auto_select else {}

    return SearchResult(
        specimens=frame,
        bags_grades=grades,
        bin_analysis=analysis,
        selections=selections,
        resolution=resolution,
        estimate=estimate,
        taxonomy_groups=groups,
        missing_codes=missing,
        geographic_filter=geo,
        snapshot_id=store.info().snapshot_id,
        warnings=warnings,
    )
