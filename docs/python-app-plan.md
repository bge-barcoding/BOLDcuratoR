# BOLDcuratoR — Python app (offline snapshot backend)

## Context

BOLDcuratoR is an R Shiny app (~5,900 lines, `app.R` + `R/modules/*`) that curates BOLD
specimen records by fetching them live from the BOLD API via BOLDconnectR, scoring and
ranking them, grading species with BAGS, and letting a curator annotate and select
representatives for export.

`docs/static-datapackage-plan.md` establishes why that shape has to change: ~20 students on
a course share one API key and serialise behind it, and BIN expansion issues one HTTP round
trip per 50 BINs (minutes to tens of minutes per search). BOLD publishes a static public
data package (~3 GB `.gz` → ~30 GB TSV, BCDM schema, CC-BY-SA 4.0), which turns BIN
expansion into a single sub-second local query and removes the API key entirely.

That plan offers several delivery targets. **4C — a Python rewrite — is the one being built
here**, chosen because it has the fewest external dependencies: no IT decision, no BOLD API,
no CAOS image API, no Zenodo dependency for development.

**Be clear about why, because the reason changed.** 4C was originally scoped as the fallback
for 4B (shinylive in the browser) failing. The 4B spike has since been run and **passed** —
WORKERFS adds 0 MB of wasm linear memory at both 180 MB and 441 MB, queries land under
200 ms, memory is stable across repeated runs. So 4C is no longer justified by 4B being
unworkable. What justifies it now:

- **Data ceiling.** 4B's binding constraint moved to the ~3× transient memory peak while
  mounting (1.3 GB for a 441 MB image) and the per-visit download, giving a recommended
  budget of ~500 MB and forcing taxonomic partitioning. A native Python app has no such
  ceiling — DuckDB reads only the pages a query touches, against a file of any size. This is
  what makes "COI-5P with sequences" affordable here and not in the browser.
- **Install story.** The remaining alternative with the same data ceiling is 4D — the
  existing Shiny app run natively against a local DuckDB file, which costs almost nothing but
  requires the user to install R. 4C exists precisely to avoid that for people who have never
  seen R.
- **Consolidation.** One Python codebase can serve both a laptop and (later) a container,
  whereas 4A-in-R plus something-else-for-local implements the scoring and BAGS logic twice.

This is a real cost decision (3–6 weeks against near-zero for 4D), taken deliberately. If the
offline users can install R, 4D ships in days and 4C's case rests on consolidation alone.

Intended outcome: `pip install boldcurator` (plus a double-click installer) gives a
cross-platform desktop app that opens in the browser, works fully offline against a
downloaded DuckDB snapshot, and reproduces the Shiny app's search → BIN expansion → filter →
score → rank → BAGS → annotate → export workflow.

Built in a `python/` folder **on this repo** so the R code stays available as the reference
implementation; moved to its own repo once stable, at which point the pip packaging is
finished.

### Decisions taken with the user

| Decision | Choice |
|---|---|
| Snapshot contents | **COI-5P only, sequences included** |
| Snapshot source (M1) | **Local file path / manual download.** Zenodo + auto-update later, behind the same resolver |
| Build order | **Core library + CLI first, verified against R; GUI second** |
| GUI stack | **Shiny for Python** — see *GUI stack* below; decision is deliberately reversible |
| Species-name rule | **One unified rule**, replacing the five divergent R regexes; divergence quantified by the parity harness |
| `HAS_IMAGE` | **Removed**, and the image requirement dropped from `RANK_2` so rank 2 stays reachable. Max quality score 16 → 15 |
| Dataset/project codes | **Implemented properly** — offline via an exploded `specimen_recordset` table, with UI inputs |

---

## What the R code actually does (the spec to port)

Verified by reading the source; several points contradict the config constants, and the
**implementation wins** in every case.

### Scoring — `R/config/constants.R:49-121`, `R/modules/specimen_handling/specimen_scorer.R`

16 criteria, **each worth exactly 1 point** (no weights); `quality_score` is a plain count,
`criteria_met` is the `"; "`-joined names in declaration order. All `grepl` use
`ignore.case = TRUE` → port with `re.IGNORECASE`.

Emptiness test, applied everywhere (`specimen_scorer.R:77-81`): a value is empty if NA, or
`strip() == ""`, or `strip().upper() in {"NONE", "NA"}`.

Global short-circuit (`specimen_scorer.R:139`): if *all* of a criterion's field values are
empty → criterion fails, before any pattern runs.

Six criteria have bespoke logic, the rest fall to `check_general_criterion`
(`specimen_scorer.R:232-251`: any non-empty value that doesn't hit the negative pattern —
and does hit the positive pattern if one exists — passes the criterion):

- `SPECIES_ID` (`:153`) — `species` non-empty AND not matching the invalid-name pattern.
- `TYPE_SPECIMEN` (`:161`) — `voucher_type` containing `"type"` wins outright; else the
  positive pattern against any of five note fields.
- `SEQ_QUALITY` (`:178`) — `bin_uri` non-empty **AND** `nuc_basecount` non-empty **AND**
  `float(nuc_basecount) >= 500`. All three.
- `PUBLIC_VOUCHER` (`:191`) — **positive wins first**, then negative. Consequence to
  preserve: `"not registered"` matches the positive `registered` and *passes*.
- `HAS_IMAGE` (`:209`) — **being deleted.**
- `ID_METHOD` (`:219`) — non-empty AND no negative match.

### Ranking — `R/config/constants.R:158-172`, `specimen_processor.R:182-227`

Evaluated **in order 1→6, first match wins**; no match → 7. A scalar in a rank definition is
an AND condition, a vector is an OR group. Rank 1 is *not* a superset of rank 2 — a type
specimen with a clean name gets rank 1 regardless of everything else.

With `HAS_IMAGE` removed from `RANK_2`, the ladder is:

1. `SPECIES_ID` ∧ `TYPE_SPECIMEN`
2. `SPECIES_ID` ∧ `SEQ_QUALITY` ∧ `COLLECTORS` ∧ `COLLECTION_DATE` ∧ `COUNTRY` ∧ (`SITE` ∨ `SECTOR` ∨ `REGION` ∨ `COORD`) ∧ `IDENTIFIER` ∧ `ID_METHOD` ∧ (`INSTITUTION` ∨ `PUBLIC_VOUCHER` ∨ `MUSEUM_ID`)
3. `SPECIES_ID` ∧ `SEQ_QUALITY` ∧ `COUNTRY` ∧ `IDENTIFIER` ∧ `ID_METHOD` ∧ (`INSTITUTION` ∨ `PUBLIC_VOUCHER` ∨ `MUSEUM_ID`)
4. `SPECIES_ID` ∧ `SEQ_QUALITY` ∧ `COUNTRY`
5. `SPECIES_ID` ∧ `SEQ_QUALITY`
6. `SPECIES_ID`
7. default

### BAGS — `R/utils/bags_grading.R:123-151` is authoritative

`BAGS_GRADE_CRITERIA` (`constants.R:175-181`) is **dead config and disagrees with the code**
(it says A at 10 specimens; the code says 11). Port the code, in this order:

1. non-numeric or NA `specimen_count`/`bin_count` → `E`
2. `has_shared_bins` or either count < 0 → `E`
3. `bin_count > 1` → `C`
4. `specimen_count < 3` → `D`
5. `specimen_count >= 11` → `A`
6. else → `B`

Eligibility (`bags_grading.R:13-23`): `species` non-empty, matching the binomial test
`^\S+\s+\S+`, and — where `identification_rank` exists — `identification_rank ∈
{species, subspecies}`. `specimen_count` counts rows *including* those with no BIN.

`check_shared_bins` (`:84-115`) currently only sees BINs among the records the user happened
to download, so grade E is systematically under-detected. **The snapshot fixes this**: a
`bin_species` table evaluates BIN sharing against all public COI-5P records. This is a
scientific improvement, and the strongest single argument for the whole project — so it must
be flagged in the UI and in exports as a deliberate difference from the Shiny app.

`ANALYSIS_CONSTANTS` (`constants.R:124-153`) is also dead — nothing reads `HAPLOTYPE`,
`CONCORDANCE_THRESHOLD` or `MAX_SPECIES_PER_BIN`. Do not reimplement them.

### BIN concordance — `R/modules/bin_analysis/mod_bin_analysis_utils.R:49-92`

Hierarchical fallback: >1 valid species → Discordant; else fall back to genus, then family,
then order, each "more than one distinct value → Discordant". Note `check_taxonomic_concordance`'s
`cf.`/`aff.` branch (`:63-70`) builds the pattern `cf\.|aff\.<Reference>`, which by alternation
precedence means *any* `cf.` record passes — a latent bug. Under the unified species rule this
branch disappears; record the behaviour change in the parity report.

### Fetch semantics — `R/modules/data_import/mod_data_import_server.R`

The one semantic that is easy to get wrong: **the geographic filter runs *before* BIN
expansion and is never re-applied afterwards** (`:223-277` then `:479-534`). The result set
is therefore "records matching taxa ∧ geography" **plus all global records sharing those
records' BINs" — deliberately, so the curator sees full BIN context. Reproduce exactly.

Other details worth carrying:
- Taxa textarea: split on `\n` → each line split on `,` → group of (valid name, synonyms…).
  `taxonomy` = flat unique list (searched); `taxonomy_groups` = kept for gap analysis.
- Geography is a **union** of the continent checkbox expansion and the countries textarea
  (`:227`), not an intersection. Rows with NA country are **dropped** when any filter is
  active (`mod_data_import_utils.R:242-253`).
- `CONTINENT_COUNTRIES` (`constants.R:4-46`) is 7 exact-string lists. Copy verbatim,
  including `"Ivory Coast"`, `"Swaziland"`, `"United States"` (not "United States of
  America"). **There are no Caribbean entries at all** — a real gap; keep the behaviour,
  raise it separately rather than silently fixing it here.
- `merge_specimens` (`:767-775`) is a left-biased union keyed on `processid` — first seen wins.
- `process_specimen_data` (`mod_data_import_utils.R:152-214`) destructively NAs out invalid
  species names, adds `data_source`/`import_date`, dedups on `processid`, sorts by `processid`.

### Auto-selection — `app.R:425-467`

Granularity is **one representative per (BIN × country)**, *not* per species. Candidates need
non-empty `bin_uri` and `species`; NA/empty country maps to `"Unknown"`; sort by
`-quality_score` then ascending `processid`, take the first. Runs **only** when there are zero
existing selections, so manual picks are never overwritten.

### Exports — the live handlers are the spec

`R/modules/export/mod_export.R` (`ExportManager`) is **instantiated at `app.R:323` and never
called** — fully tested dead code. Its 21-column list and its 5-field FASTA header are *not*
shipped behaviour. The same is true of the whole `R/modules/export_history/` module (never
sourced). Port the live handlers in `mod_specimen_handling_server.R` and
`mod_data_import_server.R`:

| Output | File | Rows | Columns |
|---|---|---|---|
| Download All | `all_specimens_<ts>.tsv` | all | all, annotations merged |
| Download Selected | `selected_specimens_<ts>.tsv` | selected | all |
| Download Annotated | `annotated_specimens_<ts>.tsv` | any flag/note/updated-id | all |
| BOLD Curation Report | `bold_curation_report_<ts>.tsv` | annotated set | `sampleid, processid, identification, flag, updated_id, flag_user, curator_notes` |
| FASTA / Selected FASTA | `*_sequences_<ts>.fasta` | has `nuc` | header `>{processid}\|{identification or species or "Unknown"}` |
| Search results | `bold_search_results_<ts>.csv` | raw, **no annotations** | all |
| BIN analysis | `bin_analysis_<ts>.xlsx` | — | sheets Summary / Content / Statistics |

Two fixes to take while porting, both one-liners in Python:
- R writes TSVs **unquoted** (`quote=FALSE`), so a curator note containing a tab or newline
  corrupts the file. Python must quote properly.
- `mod_export.R:229` whitelists `"institution"` but the BCDM/app column is `inst`, so the
  institution column is silently dropped from every export. Use `inst`.

---

## Snapshot design

One DuckDB file, opened read-only. Built by `python/tools/build_snapshot.py` from the BOLD
data package `.tsv.gz`.

| Table | Sorted by | Purpose |
|---|---|---|
| `specimen` | `kingdom, phylum, class, "order", family, subfamily, genus, species, processid` | main table, **no `nuc`** |
| `sequence` | `processid` | `nuc` only, joined on demand |
| `specimen_recordset` | `recordset_code, sid` | exploded `bold_recordset_code_arr` |
| `taxon` | `taxon_lc` | name → rank lookup with `n_records`; replaces `bold.public.search` |
| `bin_species` | `bin_uri` | BIN ↔ species counts across **all** public COI-5P records — this is what makes BAGS grade E correct |
| `_meta` | — | `snapshot_id`, `source_sha256`, `row_count`, `schema_version`, `marker_filter` |

Non-negotiable build details, from `docs/static-datapackage-plan.md` §1.2–1.3 (each of these
is a failure mode already diagnosed there, not a preference):

- `read_csv` with `auto_detect=false` and an explicit 79-column `VARCHAR` map. Auto-detect on
  a `.gz` buffers the whole unseekable stream and exhausts RAM.
- `quote=''`, `escape=''` — one unbalanced `"` in a `notes` field otherwise swallows the rest
  of the file into a single value.
- **No indexes.** DuckDB ART indexes must fit in RAM at build time and are not buffer-managed.
  The taxonomic sort order plus zone maps prune every rank's equality predicate from one
  physical layout; they are alternatives to indexes, not complements.
- Write the sorted result directly into a freshly `ATTACH`ed target file, then delete staging.
  `VACUUM` does not reclaim space in DuckDB.
- Finish with `DETACH; CHECKPOINT; close`. **A leftover `.wal` makes the file unopenable
  read-only** — verification must open it read-only in a fresh process to catch this.
- Physical names `country_ocean` / `province_state`, aliased at projection time to
  `"country.ocean"`. DuckDB parses a dotted identifier as `table.column`. `order` is a SQL
  keyword — quote it everywhere.
- **Drop `identifier_email`** — shipping millions of personal email addresses to student
  laptops is a data-protection problem, not a size one.
- Filter to `marker_code = 'COI-5P'` at build time; record the filter in `_meta`.

### Size — measure, don't assume

The user's target is "close to 3 GB". The existing plan estimates ~1.5 GB for `specimen` and
~6 GB for `sequence` **with all markers**; COI-5P-only cuts both substantially, but the
sequence table is still the dominant term and **the total may land above 3 GB**. Measure on
the first real build and record it in `python/tools/README.md` before committing to a
distribution shape. Levers, in the order to reach for them:

1. Column pruning beyond the ~57 already kept.
2. Ship `specimen` + side tables as the core download, `sequence` as a separate optional file
   the app fetches on first FASTA export (the `sequence` split already makes this a
   configuration change, not a rewrite).
3. Taxonomic scoping (Class or Order — Phylum is a poor split for BOLD, Arthropoda dominates).

Note the ~500 MB budget in the main plan's spike verdict is a **browser** constraint and does
not apply here; it is the ceiling for 4B, not 4C. If one snapshot is later required to serve
both targets, that is a scoping decision to take explicitly rather than by default.

---

## Query layer

Four access points, all against the read-only snapshot.

**Taxon resolve then query.** Step 1 hits the small `taxon` table (`WHERE taxon_lc IN (?)`,
<50 ms) and returns the exact rank column to filter on, canonical casing, `n_records`, and
ambiguity (a name that is both a genus and a subfamily returns two rows → prompt, don't
silently OR). Step 2 is a single-column equality per resolved rank, with the rank name chosen
from a **fixed whitelist of the ten BCDM rank columns** — never interpolate user text into an
identifier position; values go through bound parameters.

This retires the title-casing hack (`mod_data_import_server.R:695`), gives a typeahead source,
and lets the size check fire *before* anything is materialised.

**Country filter** pushes into the seed CTE as `country_ocean IN (?)`, which reproduces
`filter_specimens_by_continent` exactly (SQL `IN` is false for NULL, matching its NA-dropping)
and retires the HTTP-422 workaround at `mod_data_import_server.R:674-676`.

**Dataset / project codes** resolve through `specimen_recordset`, sorted by `recordset_code`.
Keep a per-code loop or diff returned codes against input, so you can report *which* codes
matched nothing — offline there is no 401, and a private code returns zero rows looking
exactly like a typo.

**BIN expansion** — the whole 50-BIN HTTP loop collapses to one statement:

```sql
WITH seed AS (
  SELECT sid, bin_uri FROM specimen
  WHERE <resolved rank predicate> AND country_ocean IN (?)
),
seed_bins AS (
  SELECT DISTINCT bin_uri FROM seed WHERE bin_uri IS NOT NULL AND bin_uri <> ''
)
SELECT <projection> FROM specimen s
WHERE s.sid IN (SELECT sid FROM seed)
   OR s.bin_uri IN (SELECT bin_uri FROM seed_bins);
```

The `sid IN seed` branch preserves the current behaviour where BIN-less seed records survive
expansion; the absence of a country predicate on the outer query preserves the
geo-filter-before-expansion semantic above.

---

## GUI stack

**Shiny for Python**, because the module/reactive structure of the R app translates rather
than gets redesigned, the maintainer already thinks in that model, and the same codebase can
later serve the Docker/K8s target unchanged. Install story is identical to NiceGUI's: `pip
install`, launch a local server, open the browser — no native webview dependency to break on
a Linux student laptop.

Two deliberate simplifications, both of which *reduce* risk versus a literal port:

- **Row selection + a bulk-annotation toolbar replaces per-cell widgets.** `R/utils/table_utils.R`
  is 1,282 lines, roughly a third of it hand-written JS to make DT render checkboxes,
  dropdowns and text inputs in cells, plus a `CustomEvent` broadcast so sibling tables stay in
  sync (`:473-649`). Shiny for Python's `DataGrid` gives multi-row selection natively; flag /
  updated-ID / notes become a toolbar applied to the selection. That deletes the JS layer
  outright and is arguably better UX for bulk curation.
- **BAGS tabs render one grouped table, not N tables.** The R version renders a separate DT
  per species group — hundreds of DT instances in one pane. Use one table with a group filter.

The UI is isolated behind a plain view-model layer (`core/` returns DataFrames and dicts; `ui/`
does nothing but present them), so if the half-day UI spike in Phase 3 says `DataGrid` can't
carry the specimen table, swapping to NiceGUI + AG Grid costs days, not weeks.

---

## Layout

```
python/
  pyproject.toml              # hatchling, console_script `boldcurator`
  README.md
  PROGRESS.md                 # the live checklist + session handoff notes
  src/boldcurator/
    config/constants.py       # criteria, rank defs, CONTINENT_COUNTRIES, limits
    config/columns.py         # COLUMN_DEFINITIONS, PREFERRED_COLUMNS
    data/snapshot.py          # SnapshotStore: resolve, open read-only, _meta
    data/queries.py           # resolve_taxa, search, recordsets, bin_expand
    data/schema.py            # table + column definitions, shared SQL
    core/species.py           # the single is_valid_species_name()
    core/scoring.py           # vectorised scorer
    core/ranking.py
    core/bags.py
    core/bins.py              # concordance, BIN content
    core/selection.py         # auto_select_best_specimens
    core/pipeline.py          # search -> expand -> filter -> process -> score -> rank -> bags
    io/exports.py             # tsv / xlsx / fasta / curation report
    io/session.py             # sqlite persistence + annotation stores
    cli.py
    ui/                       # Phase 3
  tools/
    build_snapshot.py
    verify_snapshot.py
    fetch_snapshot.py         # Phase 5: URL/Zenodo + checksum
  tests/
    fixtures/                 # tiny generated snapshot, committed or built in conftest
  parity/
    export_r_reference.R      # run the R logic over the shared fixture -> CSV
    compare.py                # row-by-row diff, writes parity/REPORT.md
```

Core logic is pandas + duckdb only — no Shiny import below `ui/`, enforced by a test. That is
what keeps the GUI decision reversible and the CLI usable headless.

---

## Checklist

Tick items in `python/PROGRESS.md` as they land; each phase ends in a commit on
`claude/intelligent-dijkstra-g66cbr`. Phases 1–2 are independent of Phase 0 and can start
immediately against a synthetic fixture.

### Phase 0 — Snapshot build (blocked only on obtaining the `.tsv.gz`)

- [ ] 0.1 Confirm download mechanics for `bench.boldsystems.org/…/datapackage` (API key header vs form-login cookie; stable URL vs one-shot signed link). If interactive, that step stays manual — one person, one download. Do not contort the design around it.
- [ ] 0.2 Confirm the course's records are public. Private/unpublished records are simply absent from the public package and there is no API fallback any more. If not, scope an overlay DuckDB file in the same schema, `ATTACH`ed and `UNION ALL`ed.
- [ ] 0.3 Note the CC-BY-SA 4.0 attribution requirement in writing; it goes in the app's about text and any redistribution.
- [ ] 0.4 `tools/build_snapshot.py` — ingest per the rules above, COI-5P filter, `TRY_CAST` the four numerics (`specimenid`, `elev`, `depth`, `nuc_basecount`), leave partial BCDM dates as text.
- [ ] 0.5 Explode `bold_recordset_code_arr` (`['AANIC','DS-ANIC2A']` form) and **assert the parse**: every non-empty value matches `^\[.*\]$`, `count(DISTINCT recordset_code)` lands in the 10⁵ range. 1 or 10⁷ means the split is wrong. Check real codes against `^DS-[A-Z0-9]+$` (`mod_data_import_utils.R:50`) — if BOLD uses lowercase, that validator rejects legitimate input.
- [ ] 0.6 Build `taxon` and `bin_species`.
- [ ] 0.7 `tools/verify_snapshot.py` — opens the file **read-only in a fresh process** (catches a leftover `.wal`); asserts required columns, no null/empty/duplicate `processid`, `specimen_recordset ANTI JOIN specimen` empty, `sequence ANTI JOIN specimen` empty, sane null fractions, and that `resolve("lepidoptera") → order`, `resolve("nymphalidae") → family`, `resolve("danaus plexippus") → species`.
- [ ] 0.8 **Record measured sizes and build times** in `python/tools/README.md`. Decide the distribution shape against the 3 GB target using the levers above.
- [ ] 0.9 `tests/fixtures/` — a few-thousand-row synthetic snapshot in the same schema, generated by `conftest.py`, so CI never needs the real file.

### Phase 1 — Core library

- [ ] 1.1 `config/constants.py` — port `SPECIMEN_SCORING_CRITERIA` (15, no `HAS_IMAGE`), the rank ladder above, `CONTINENT_COUNTRIES` verbatim, `DOWNLOAD_LIMITS`. Do **not** port `ANALYSIS_CONSTANTS` or `BAGS_GRADE_CRITERIA` (dead and wrong).
- [ ] 1.2 `core/species.py` — the single `is_valid_species_name()`. Document what each of the five R variants did and where results will differ.
- [ ] 1.3 `core/scoring.py` — vectorised over columns, not `for i in range(nrow)`. The R loop takes ~15 s for 10,000 rows. Preserve the emptiness test, the all-fields-empty short-circuit, and `PUBLIC_VOUCHER`'s positive-wins order.
- [ ] 1.4 `core/ranking.py` — first-match-wins 1→6, default 7.
- [ ] 1.5 `core/bags.py` — implementation rules, not the constants. Grade E uses `bin_species` for global BIN sharing; expose a flag so the parity harness can also run in "local records only" mode to match R.
- [ ] 1.6 `core/bins.py` — concordance with the hierarchical fallback; `process_bin_content`.
- [ ] 1.7 `core/selection.py` — auto-select per (BIN × country), `-quality_score` then `+processid`, only when no selections exist.
- [ ] 1.8 `data/snapshot.py` + `data/queries.py` — the four access points, whitelisted rank identifiers, bound parameters, streamed sequence fetch (`fetchmany`, never materialise `nuc` in a search).
- [ ] 1.9 `core/pipeline.py` — wire the whole flow including the size pre-check from `taxon.n_records`, which now fires *before* anything is materialised.
- [ ] 1.10 Unit tests per module against the fixture.

### Phase 2 — CLI, exports, parity

- [ ] 2.1 `cli.py` — `boldcurator search --taxa-file … --countries … --continents … --datasets … --out …`, plus `snapshot info` and `snapshot verify`.
- [ ] 2.2 `io/exports.py` — the seven live outputs above. Quote TSVs properly; use `inst` not `institution`.
- [ ] 2.3 `io/session.py` — SQLite annotation store (`selected`, `flag`, `updated_id`, `curator_notes`, each keyed by `processid` with timestamp + user). **Store the query + processid list + annotations, not the whole frame** — a named snapshot makes the result reproducible, so session blobs drop from ~125 MB to a few KB. Stamp `snapshot_id` on the session row.
- [ ] 2.4 `parity/export_r_reference.R` — run the R scorer/ranker/BAGS/selection over the shared fixture and dump a CSV.
- [ ] 2.5 `parity/compare.py` — row-by-row diff of `quality_score`, `criteria_met`, `rank`, BAGS grade, BIN concordance, auto-selection. **Every difference must be explained by a decision in this plan** (the unified species rule, the `HAS_IMAGE` removal, global grade E); anything unexplained is a bug. Output `parity/REPORT.md`.
- [ ] 2.6 CI workflow running the Python tests and the parity harness on Linux/macOS/Windows.

**Gate: do not start Phase 3 until `parity/REPORT.md` is clean.**

### Phase 3 — GUI

- [ ] 3.1 Half-day spike: render 50,000 rows in a Shiny for Python `DataGrid` with multi-row selection and a bulk-annotation toolbar. If it can't carry it, switch `ui/` to NiceGUI + AG Grid — nothing below `ui/` changes.
- [ ] 3.2 Shell: snapshot status bar (snapshot id, date, record count, "offline — no BOLD API"), replacing the API-key/user bar. Keep name/email for annotation attribution.
- [ ] 3.3 Data Input page: taxa textarea, countries textarea, continent checkboxes, **dataset and project code inputs**, size-check modal driven by `taxon.n_records`, result value boxes.
- [ ] 3.4 Species focus: checklist (species, counts, BINs, BAGS grade, countries, mean score), gap analysis against `taxonomy_groups` with synonym matching, summary stats by family.
- [ ] 3.5 BIN focus: BIN content table, concordant/discordant counts. Wire up the shared-BINs box and the xlsx download — both exist server-side in R with no UI slot.
- [ ] 3.6 BAGS A–E: one grouped table per grade, with the R grouping semantics (A/B/D by species; C by species × BIN; E by shared BIN, only where >1 species-level name) including pulling non-species-level records in by BIN membership.
- [ ] 3.7 Specimen table: read-only mirror, the six download buttons.
- [ ] 3.8 Session save/resume; warn on resume if the snapshot id changed, since BIN membership and identifications may have.

### Phase 4 — Packaging

- [ ] 4.1 `pyproject.toml`, console script, `python -m boldcurator`.
- [ ] 4.2 First-run flow: no snapshot configured → prompt for a path or a download URL; verify checksum; store the pointer in a platform config dir.
- [ ] 4.3 PyInstaller/Briefcase builds on a GitHub Actions matrix (Windows, macOS x86_64 + arm64, Linux), attached to releases.
- [ ] 4.4 **Signing is a real cost, decide explicitly**: unsigned builds hit Gatekeeper on macOS and SmartScreen on Windows. A genuine double-click needs Apple Developer (~$99/yr) plus Windows code signing. Document the workaround if not paying.
- [ ] 4.5 Smoke test each installer on a clean VM.

### Phase 5 — Distribution and hand-off

- [ ] 5.1 `tools/fetch_snapshot.py` — Zenodo concept DOI (always resolves to latest) or plain URL + sha256; no-op when `snapshot_id` is unchanged.
- [ ] 5.2 Publish to Zenodo with a `manifest.json` (`snapshot_id`, `sha256`, `row_count`, `schema_version`, source package id) and CC-BY-SA attribution.
- [ ] 5.3 In-app "check for new snapshot".
- [ ] 5.4 Move `python/` to its own repo, finish PyPI publishing, leave a pointer in this repo.

---

## What the snapshot cannot do — state plainly in the README and in-app

- Private / unpublished / early-release records are **absent**. No API fallback exists.
- Dataset/project codes covering private records return zero rows; report which codes matched nothing.
- Non-COI-5P markers are absent by build-time choice.
- Freshness is bounded by BOLD's release cadence plus how often the user updates.
- Records are occasionally retracted — a `processid` in a saved session may be missing from a newer snapshot. Don't drop rows silently on resume.
- **Scores are not numerically comparable with the Shiny app's** (max 15, not 16), and grade E is *more* complete. Stamp `snapshot_id` and a scoring-version marker into every export.

---

## Verification

- **Unit**: per-module tests against the generated fixture; a test asserting nothing under `core/`, `data/` or `io/` imports the UI framework.
- **Parity** (the one that matters): `parity/compare.py` diffs R vs Python over the same fixture for `quality_score`, `criteria_met`, `rank`, BAGS grade, BIN concordance and auto-selection. Every diff must map to a decision recorded here.
- **Snapshot**: `tools/verify_snapshot.py` must pass from a fresh process, read-only, before any snapshot is distributed.
- **End-to-end**: `boldcurator search --taxa-file tests/fixtures/taxa.txt --continents Europe --out /tmp/out` on the real snapshot; assert non-empty, record latency for taxon resolve (<1 s), a family-level query, and BIN expansion (target sub-second).
- **Scale**: run the full pipeline over a ~50,000-row result and record wall clock and peak RSS. The R scorer's row loop is ~75 s at that size; the vectorised version should be seconds.
- **Cross-platform**: CI matrix on Linux, macOS and Windows for tests; manual installer smoke test per platform in Phase 4.

---

## Noted, not in scope here

- `mod_export.R` (`ExportManager`) and the whole `R/modules/export_history/` module are unreachable dead code in the R app, as are several value boxes and download handlers with no UI slot. They are not being ported. Whether to delete them from the R app is a separate call.
- `CONTINENT_COUNTRIES` has no Caribbean entries. Ported verbatim to preserve behaviour; worth fixing in both apps separately.
- The `cf.`/`aff.` alternation bug in `check_taxonomic_concordance` disappears under the unified species rule; that is a behaviour change, reported by the parity harness.
