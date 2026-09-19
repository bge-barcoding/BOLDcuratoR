"""The app's session state, and the size policy that keeps it honest.

Two kinds of screen, with different costs:

* The **specimen table** is paged. It asks the snapshot for one page at a time
  and costs the same whether the result holds 183 rows or two million.
* The **species, BIN and BAGS screens** are whole-result aggregates. A species'
  specimen count is a fact about every record in the result, so there is no
  paging its way out of materialising and scoring the lot.

So the analysis is computed **lazily and once**, the first time a screen needs
it, and refused above :data:`ANALYSIS_LIMIT` with an explanation rather than
attempted and survived. Searching stays instant either way.

No Shiny here. This is the state a GUI drives, not the GUI.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from ..config.constants import DOWNLOAD_LIMITS
from ..core.grouping import SpecimenGroup, group_specimens
from ..core.pipeline import (
    SearchResult,
    analyse_plan,
    geographic_filter,
    parse_lines,
    parse_taxa_input,
    process_specimen_data,
    flatten_taxa,
)
from ..core import selection
from ..core.ranking import score_and_rank
from ..core.summaries import build_species_checklist
from ..core.table import SpecimenTable
from ..data.queries import (
    SearchPlan,
    SearchQuery,
    estimate_search,
    fetch_by_bin,
    missing_recordset_codes,
    plan_search,
    resolve_taxa,
)
from ..data.snapshot import SnapshotStore
from ..io.annotations import Annotations

#: Above this, the aggregate screens refuse rather than grind. The specimen
#: table has no such limit -- it never materialises the result.
ANALYSIS_LIMIT = DOWNLOAD_LIMITS["MAX_RECORDS"]

#: Above this the analysis is worth a warning before it runs.
ANALYSIS_WARN = DOWNLOAD_LIMITS["WARN_RECORDS"]


class ResultTooLargeToAnalyse(RuntimeError):
    def __init__(self, records: int):
        self.records = records
        super().__init__(
            f"{records:,} records is over the {ANALYSIS_LIMIT:,} this screen can "
            "summarise. The specimen table still works at any size; to get the "
            "species, BIN and BAGS screens, narrow the taxa or add a "
            "geographic filter."
        )


@dataclass
class SearchState:
    """One search, and everything derived from it."""

    plan: SearchPlan
    table: SpecimenTable
    query_label: str = ""
    warnings: list[str] = field(default_factory=list)
    #: The session's one annotation store, so a fresh search's auto-selection
    #: (see :meth:`analysis`) lands where the specimen table and every BAGS
    #: group already look for it.
    annotations: Annotations | None = field(default=None, repr=False)
    _analysis: SearchResult | None = field(default=None, repr=False)
    _groups: dict[str, list[SpecimenGroup]] = field(default_factory=dict, repr=False)

    @property
    def record_count(self) -> int:
        return self.plan.expanded_records

    @property
    def can_analyse(self) -> bool:
        return 0 < self.record_count <= ANALYSIS_LIMIT

    @property
    def analysis_is_ready(self) -> bool:
        return self._analysis is not None

    def analysis(self, store: SnapshotStore) -> SearchResult:
        """The whole-result summaries. Computed once, on first use.

        Auto-selects a best specimen per (BIN x country) the first time this
        result is analysed -- ``auto_select_best_specimens`` already refuses to
        touch a non-empty selection, so this only ever fires on a fresh search,
        the way R's own auto-selection does (``app.R:425-467``). It happens
        here rather than at search time because this is the first point that
        actually holds the scored, whole-result frame the selection needs --
        the search itself only plans, and the paged table never materialises
        more than one page.
        """
        if self._analysis is None:
            if not self.can_analyse:
                raise ResultTooLargeToAnalyse(self.record_count)
            # auto_select=False: analyse_plan's own auto-select can't see the
            # session's Annotations object, so it is done just below instead,
            # against the one store the rest of the app reads and writes.
            self._analysis = analyse_plan(store, self.plan, auto_select=False)
            if self.annotations is not None:
                chosen = selection.auto_select_best_specimens(
                    self._analysis.specimens, existing=self.annotations.selected)
                if chosen is not self.annotations.selected:
                    self.annotations.selected.update(chosen)
        return self._analysis

    def checklist(self, store: SnapshotStore):
        result = self.analysis(store)
        return build_species_checklist(result.specimens, result.bags_grades)

    def groups(self, store: SnapshotStore, grade: str) -> list[SpecimenGroup]:
        """The groups for one grade, cached -- a curator revisits a tab often."""
        if grade not in self._groups:
            result = self.analysis(store)

            def fetch_bin(bin_uri: str, store=store):
                """Everything in this BIN, straight from the snapshot.

                Used only when a grade-E group's own search didn't happen to
                capture the species it shares the BIN with -- see
                ``core.grouping``. Scored the same way every other specimen is,
                so it sorts and displays like the rest of the group.
                """
                extra = fetch_by_bin(store, [bin_uri])
                if len(extra) == 0:
                    return extra
                return score_and_rank(process_specimen_data(extra))

            self._groups[grade] = group_specimens(
                result.specimens, result.bags_grades, grade,
                shared_bins=result.shared_bins, fetch_bin=fetch_bin)
        return self._groups[grade]

    def grade_lookup(self) -> dict[str, str]:
        """species -> grade, but only if the analysis has already been computed.

        A page of the specimen table cannot know a BAGS grade: grading is a
        fact about every record in the result, not about the page. So the
        column appears once a summary screen has been opened, and is honestly
        absent before that rather than blank or wrong.
        """
        if self._analysis is None or not len(self._analysis.bags_grades):
            return {}
        grades = self._analysis.bags_grades
        return dict(zip(grades["species"], grades["bags_grade"]))

    def grade_counts(self, store: SnapshotStore) -> dict[str, int]:
        """Species per grade, for the badges on the navigation."""
        grades = self.analysis(store).bags_grades
        if not len(grades):
            return {}
        return grades["bags_grade"].value_counts().to_dict()

    # -- exports -------------------------------------------------------
    #
    # Six of these mirror ``mod_specimen_handling_ui.R``'s six download
    # buttons exactly: All, Selected, Annotated, Curation Report, FASTA,
    # Selected FASTA. ``search_results`` and ``bin_analysis`` are the two
    # other live R download handlers (``mod_data_import_server.R:932`` and
    # ``mod_bin_analysis_server.R:200``), placed on the screens that already
    # show what they export. Each writes to ``path`` and hands back what it
    # wrote, or ``None`` when there is nothing to write -- a curator should
    # not be able to click "Download Selected" into an empty file.

    def export_specimens(self, store: SnapshotStore, kind: str, path) -> "Path | None":
        from ..io import exports as export_io

        result = self.analysis(store)
        frame = result.specimens
        ann = self.annotations if self.annotations is not None else Annotations()

        if kind == "all":
            rows = export_io.merge_annotations(frame, ann)
        elif kind == "selected":
            rows = export_io.merge_annotations(
                export_io.selected_rows(frame, ann), ann)
        elif kind == "annotated":
            rows = export_io.merge_annotations(
                export_io.annotated_rows(frame, ann), ann)
        elif kind == "curation_report":
            rows = export_io.curation_report(frame, ann)
        else:
            raise ValueError(f"Unknown export {kind!r}")

        if len(rows) == 0:
            return None
        return export_io.write_tsv(rows, path, snapshot_id=result.snapshot_id)

    def export_search_results(self, store: SnapshotStore, path) -> "Path | None":
        from ..io import exports as export_io

        result = self.analysis(store)
        if len(result.specimens) == 0:
            return None
        return export_io.write_csv(result.specimens, path,
                                   snapshot_id=result.snapshot_id)

    def export_fasta(self, store: SnapshotStore, path, *,
                     selected_only: bool) -> "tuple[Path, int] | None":
        if not store.has_sequences:
            return None
        from ..data.queries import iter_sequences
        from ..io import exports as export_io

        result = self.analysis(store)
        frame = result.specimens
        if selected_only:
            ann = self.annotations if self.annotations is not None else Annotations()
            frame = export_io.selected_rows(frame, ann)
        if len(frame) == 0:
            return None

        written, n = export_io.write_fasta(
            frame, iter_sequences(store, [str(p) for p in frame["processid"]]), path)
        if n == 0:
            written.unlink(missing_ok=True)
            return None
        return written, n

    def export_bin_analysis(self, store: SnapshotStore, path) -> "Path | None":
        from ..io import exports as export_io

        result = self.analysis(store)
        analysis = result.bin_analysis
        if not analysis or not len(analysis.get("content", [])):
            return None
        return export_io.write_bin_analysis_xlsx(analysis, path,
                                                  snapshot_id=result.snapshot_id)


class AppState:
    """Everything the session holds between clicks."""

    def __init__(self, store: SnapshotStore, *, page_size: int = 100,
                 user: str = "") -> None:
        self.store = store
        self.page_size = page_size
        self.user = user
        #: One store of annotations for the whole session. The specimen table
        #: and every BAGS group read and write the same one, so a flag set on
        #: the grade E screen is already there on the specimen table.
        self.annotations = Annotations()
        self.search: SearchState | None = None

    # -- the search form ---------------------------------------------------

    def _build(self, taxa_text: str, countries_text: str, continents: list[str],
               dataset_text: str, project_text: str):
        """Turn the form into a query, or explain why it cannot be one.

        Returns ``(query, resolution, warnings, error)``. Every field is parsed
        the way the CLI parses it -- one value per line, blanks dropped -- so
        the two cannot drift.
        """
        groups = parse_taxa_input(taxa_text)
        names = flatten_taxa(groups)
        geo = geographic_filter(parse_lines(countries_text), list(continents or []))
        datasets = parse_lines(dataset_text)
        projects = parse_lines(project_text)

        warnings: list[str] = []
        resolution = resolve_taxa(self.store, names) if names else None
        if resolution is not None and resolution.unmatched:
            warnings.append(
                "No records for: " + ", ".join(resolution.unmatched)
                + ". Check the spelling, or they may exist only on unpublished "
                "records, which a public snapshot cannot contain.")
        if resolution is not None:
            for name, options in resolution.ambiguous.items():
                warnings.append(
                    f"{name!r} is ambiguous: "
                    + ", ".join(f"{o.rank} ({o.n_records:,} records)"
                                for o in options)
                    + ". All of them were searched.")

        missing = missing_recordset_codes(self.store, datasets + projects)
        if missing:
            warnings.append(
                "No records for these dataset/project codes: "
                + ", ".join(missing)
                + ". Offline there is no authentication error to tell a typo "
                "from a code covering private records -- both return nothing.")

        query = SearchQuery(
            taxa=resolution.resolved if resolution else [],
            countries=geo,
            dataset_codes=datasets,
            project_codes=projects,
            expand_bins=True,
        )
        if query.is_empty():
            if names:
                return None, resolution, warnings, (
                    "None of those names are in this snapshot.")
            return None, resolution, warnings, (
                "Type a taxon name, or a dataset or project code.")
        return query, resolution, warnings, ""

    def estimate(self, taxa_text: str = "", countries_text: str = "",
                 continents: list[str] | None = None, dataset_text: str = "",
                 project_text: str = "") -> dict:
        """Size the search without fetching a single record.

        Counts only -- this is the pre-check the whole design rests on, and it
        costs a fraction of a second even for an order of two million records.
        """
        query, resolution, warnings, error = self._build(
            taxa_text, countries_text, continents or [], dataset_text, project_text)
        if query is None:
            return {"error": error, "warnings": warnings}
        counts = estimate_search(self.store, query)
        counts["warnings"] = warnings
        counts["error"] = ""
        counts["resolved"] = [f"{t.name} ({t.rank})" for t in query.taxa]
        counts["countries"] = len(query.countries)
        counts["over_limit"] = (
            counts["expanded_records"] > DOWNLOAD_LIMITS["MAX_RECORDS"]
            or counts["seed_bins"] > DOWNLOAD_LIMITS["MAX_BINS"])
        return counts

    def run_search(self, taxa_text: str = "", countries_text: str = "",
                   continents: list[str] | None = None, dataset_text: str = "",
                   project_text: str = "") -> str:
        """Plan a search from the form. Returns a human-readable status line."""
        query, resolution, warnings, error = self._build(
            taxa_text, countries_text, continents or [], dataset_text, project_text)
        if query is None:
            self.search = None
            return error

        plan = plan_search(self.store, query)
        if plan.expanded_records > ANALYSIS_LIMIT:
            warnings.append(
                f"{plan.expanded_records:,} records: the specimen table works, "
                "but the species, BIN and BAGS screens need a narrower search.")
        elif plan.expanded_records > ANALYSIS_WARN:
            warnings.append(
                f"{plan.expanded_records:,} records: the summary screens will "
                "take a few seconds the first time.")

        label = ", ".join(t.name for t in query.taxa) or "dataset/project codes"
        if query.countries:
            label += f" · {len(query.countries)} countries"
        self.search = SearchState(
            plan=plan,
            table=SpecimenTable(self.store, plan, page_size=self.page_size,
                                annotations=self.annotations, user=self.user),
            query_label=label,
            warnings=warnings,
            annotations=self.annotations,
        )
        return (f"{plan.expanded_records:,} records "
                f"({plan.seed_records:,} seed, {plan.seed_bins:,} BINs)")
