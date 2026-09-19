# Progress and session handover

Branch: `claude/wonderful-newton-qw7llz`. Plan:
[`../docs/python-app-plan.md`](../docs/python-app-plan.md).

**State: Phases 0–2 complete and fast. Phase 3 has all six screens working,
the eight downloads are wired, record selection is per-row (not just
whole-group), auto-selection runs on a fresh search, and a real grade-E
grouping bug is fixed. 233 tests pass, the parity gate is green.**

---

# START HERE TOMORROW

Phases 0-2 are closed and performance is closed. Phase 3's six screens now
have their downloads wired too; what remains is listed below in priority
order. Everything after the second horizontal rule is reference — why things
are the way they are — and does not need reading to get going.

## What this session did

A curator using the real app (not the fixture) reported three things, all
now fixed -- see the commit for the detail, this is the summary:

1. **A real BAGS grade-E bug**: a species graded E because ONE of its BINs is
   shared could turn its OTHER, unrelated BIN into a spurious "Shared BIN"
   group. Real example: `BOLD:AAL6477` (one species, four records) showed up
   as shared only because *Sialis concava* also sits in `BOLD:AAG9765`, which
   really is shared. `core/grouping.py`'s `_shared_bin_groups` now filters to
   BINs that are themselves in the shared set (`SearchResult.shared_bins`,
   new on `core/pipeline.py`), not every BIN a grade-E species happens to
   touch. It also now enriches a group from the snapshot
   (`data/queries.fetch_by_bin`) when the sharing species wasn't captured by
   the search's own taxa/geography, instead of only a note saying so.
   `tests/test_grouping.py` has the regression tests, named after the real
   BIN.
2. **Selection was whole-group-or-nothing.** The specimen table and every
   BAGS group now render a real checkbox per record
   (`ui/app.py::ROW_CHECKBOX_CLASS`, one delegated `document`-level listener
   so it survives every table re-render) alongside the existing bulk buttons
   (select page / group / all), wired through a `row_select` Shiny input to
   `Annotations.set_selected`/`unset_selected`.
3. **Auto-selection (best record per BIN x country) was never wired to the
   GUI.** `core/selection.auto_select_best_specimens` existed and was tested,
   but `SearchState.analysis` passed `auto_select=False` and nothing ever
   called it. It now runs the first time a result is analysed (species/BIN/
   BAGS tab), straight into the session's `Annotations`, and only when
   nothing is selected yet -- matching R's "auto-select on a fresh import"
   and never overwriting a curator's own choice.

Also: the six specimen-handling downloads (All / Selected / Annotated /
Curation Report / FASTA / Selected FASTA), plus the search-results CSV and
the BIN-analysis workbook, are wired to real `ui.download_button`s
(`SearchState.export_*` in `ui/state.py`, `@render.download_button` handlers
in `ui/app.py`). All eight were driven in a real browser and produce
non-empty files; a click with nothing to export gets one line saying why
instead of a blank or missing file.

All of the above was checked in a real browser
(`tools/drive_ui.py` plus ad-hoc Playwright scripts for the checkbox and the
downloads), not just unit tests -- see "the rules this session cost the most
to learn" below for why that matters here specifically.

## First, 60 seconds of setup

```powershell
cd C:\GitHub\BOLDcurator\python
git pull
pip install -e ".[dev,gui]"
python -m pytest tests/ -q          # 233 passing
python parity/compare.py            # PASS

python -m boldcurator.cli gui --snapshot "<the reordered snapshot>"
```

The snapshot to use is the **reordered** one (`sequence_order = specimen`).
`info` and `benchmark` warn on the snapshot line if you point at the old
layout, and `verify` fails it outright.

## What to build next, in order

### 1. Gap analysis (plan 3.4)

`perform_gap_analysis` (`mod_species_analysis_utils.R:57+`) is **not ported**.
It compares the taxa the user typed -- as synonym groups, first name is the
valid one -- against what the search found, and reports Found / Missing per
group. `core/pipeline.parse_taxa_input` already returns the groups, and
`SearchResult.taxonomy_groups` carries them. It belongs in `core/summaries.py`
beside `build_species_checklist`, then on the Species screen.

### 2. Session save/resume (plan 3.8)

`io/session.py` exists and is tested; it saves the query, processids and
annotations rather than the whole frame. Wire Save/Load to `AppState`, and
**warn on resume if the snapshot id changed** -- BIN membership and
identifications may have moved under the saved work.

### 3. CI (plan 2.6) -- still not done

233 tests and the parity harness run only when someone remembers. A GitHub
Actions matrix (Linux/macOS/Windows) that installs, runs pytest and runs
`parity/compare.py` needs no real snapshot: `conftest.py` builds the fixture,
and `tests/make_fake_package.py` generates the source. Worth doing before the
GUI grows further.

### 5. Two Phase 0 items only you can close

* **0.2** confirm the course's records are public. If some are not, they are
  simply absent from a public snapshot and we need an overlay DuckDB file in
  the same schema, `ATTACH`ed and `UNION ALL`ed -- worth knowing early because
  it changes the query layer.
* **0.3** the CC-BY-SA 4.0 attribution requirement, in writing, for the about
  text and any redistribution.

### Also open, not urgent

* **The R app rejects 4% of real BOLD dataset codes.** `mod_data_import_utils.R:50`
  validates against `^DS-[A-Z0-9]+$` and sets `results$valid <- FALSE`, a hard
  gate before any query runs; the builder found 544 of 13,706 real DS- codes do
  not match. That is a live bug in the *shipped R app*, not in this rewrite.
  The SQL to list examples is further down this file.
* `export_all` streams the sequences twice, once for all specimens and once for
  the selected subset. Two passes where one would do, at ~0.4 s a pass.

## The rules this session cost the most to learn

1. **Drive the UI in a browser before believing it.** Every UI bug so far has
   been invisible to the unit tests and obvious on the first click. Run
   `tools/drive_ui.py` -- it checks seventeen things across all six screens and
   exits non-zero.
2. **In a Shiny effect, read every input you react to OUTSIDE
   `reactive.isolate()`**, and isolate only the writes. Two controls rendered
   perfectly, accepted clicks and did nothing because of this.
3. **When checking the UI, match against the table, not the panel.** The
   annotation toolbar holds a flag `<select>` listing every flag name, so
   `"synonym" in panel.inner_text()` passes for an annotation that never
   rendered.
4. **`ui.navset_pill_list` keeps every tab's DOM mounted, just hidden.** A
   Playwright selector like `.bc-row-select` matches the specimen table's
   checkboxes *and* whatever BAGS group table is behind another tab, and
   Playwright correctly refuses to click the hidden ones. Scope selectors to
   the active panel's own output id (`#specimens_body .bc-row-select`), not
   the class alone. Also: a `<select>`'s value change is a websocket
   round-trip -- clicking Apply immediately after `select_option` with no wait
   between them races the server and can apply the *previous* value.
5. **A raw HTML table (`ui.HTML(...)`) can still hold real inputs.** A
   `<script>` tag inside that HTML will not run -- browsers do not execute
   scripts inserted via `innerHTML` -- so a per-row control that needs to
   survive the table being re-rendered on every click (paging, sorting,
   "next problem") needs one listener attached once, at `document` level, via
   event delegation, not a listener attached to the row elements themselves.
   That is what the per-record selection checkboxes do
   (`ui/app.py::ROW_CHECKBOX_CLASS`).

And the one that predates the GUI: **measure before optimising**. Two sessions
of guessing at performance cost more than the fixes did, and both real causes
(a projection that could not be pushed down, a table stored in the wrong
physical order) were invisible to reasoning and obvious to a benchmark.

---

## Where the real data stands — Phase 0 is CLOSED

Both snapshots are built and verified from `BOLD_Public.11-Sep-2026.tsv`.
**Do not rebuild** — the full one needs `tools/reorder_sequences.py` run on it
(two minutes, no TSV), not a 24-minute re-ingest. Figures are in `README.md`.

| Snapshot | Size | Zipped |
|---|---|---|
| `bold_meta_2026-09-11.duckdb` | 2.89 GB | ~750 MB |
| `bold_snapshot_2026-09-11.duckdb` (full) | 7.95 GB | 1.9 GB |

20,164,595 COI-5P records, 20,096,366 with sequences, 546,861 taxa,
412,637 BINs. All 19 verification checks passed on both when they were built.
The verifier has since gained a twentieth, on sequence ordering, which the full
snapshot will fail until it is reordered — that is the check doing its job, not
a corrupt file.

**The staging file can be deleted** — `C:\Users\benjp\Downloads\bold.staging`,
about 20 GB back. It has done its job.

**Sequences stay in one file.** 4.2x compression makes a 1.9 GB download
comfortable, so the metadata/sequence split is not needed and the plan's size
levers are closed.

---

## Performance — done and measured on the real snapshot

**State: done and confirmed on the real snapshot, reordered. Every output
byte-identical, parity green, 164 tests pass. Nothing here is outstanding.**

All figures below are the real `bold_snapshot_2026-09-11.duckdb`, Windows,
32 GB — no stand-ins:

| Step | Before | After |
|---|---|---|
| search `Danaus plexippus` (183 rows) | 4.657 s | **0.219 s** |
| **full pipeline `Danaus plexippus`** | **8.500 s** | **0.312 s** |
| stream 183 sequences | 7.375 s, RSS +6,088 MB | **0.437 s, RSS +169 MB** |
| export all formats | 12.234 s | **1.328 s** |

Sequence streaming is 17x faster and grows memory 36x less. The whole
benchmark run went from about 76 s of measured steps to about 17 s.

**Read the numbers on a freshly reordered file with some care.** The run that
produced them was the first touch of a newly written 8 GB file, so every
disk-bound step is cold: `plan Lepidoptera` measured 1.859 s against 0.359 s on
the warm original, and `fetch Nymphalidae` 7.938 s against 2.015 s. Nothing
changed in those paths. Run the benchmark twice and take the second if the
absolute numbers matter.

`README.md` has the full tables and the reasoning. The short version is that
the same mistake was in two places.

### What it actually was, twice

**Not BIN expansion**, which was the previous session's suspect. Counting the
BIN-expanded row set costs 0.1-0.4 s at every scale. The cost was the
*projection*: "in the seed **or** sharing its BINs" is a disjunction over two
subqueries, which DuckDB cannot push into the scan, so it projected all 71
columns of all 20 M rows and filtered afterwards.

Fixed by planning then fetching: `plan_search` resolves `rowid`s narrowly,
`fetch_planned` semi-joins them back, and DuckDB pushes *that* into the scan.
`run_search` uses the one plan as both the size pre-check and the row set.

**The `bin_index` table proposed here is not needed, and neither is a rebuild.**
`rowid` is already the physical position. Worth recording why `sid` cannot do
the job: `snapshot_builder` assigns it with `row_number() OVER ()` *before* the
`ORDER BY`, so it is uncorrelated with physical position.

**Then the same thing in `sequence`.** With the pipeline at 0.3 s, fetching 183
sequences took 7.4 s and 6 GB. `sequence` was stored in ingest order while
every fetch is driven by a taxonomic result, so nothing could prune and all
5 GB of `nuc` was projected. Two changes, and **neither works alone**:

* `iter_sequences` resolves rowids in a pass that never touches `nuc`, then
  fetches by rowid.
* the builder sorts `sequence` by the same key as `specimen`.

On the old layout the new query shape is worth nothing (2.39 s vs 2.64 s); on
the new layout the old query shape is worth nothing (3.06 s). Together:
0.21 s and 271 MB.

**Two more per-group Python loops**, the same defect the R port already fixed in
scoring and selection, found by timing the stages separately:

* `bins.process_bin_content` ran four distinct-value passes per BIN -- 8.1 s for
  an 88,000-row result, more than the search. Now 0.48 s.
* `bags.calculate_bags_grades` ran once per species -- 2.6 s at 20,000 species.
  Now 0.94 s. The grade still comes from `determine_grade` and only from there;
  a vectorised second copy of the ladder was written first and the parity
  harness caught it, which is the mutation test earning its keep.

Both now share `core/frames.distinct_by_group`.

### Performance is closed

Curators download at most ~10,000 sequences at a time and rarely that, so the
remaining levers are not worth their cost. **Do not reopen this without a
measurement showing a real user waiting.** The two candidates, both declined:

* Arrow-backed string dtype, which would take scoring an 88,000-row result from
  1.9 s to perhaps a third of that. It is a dtype change across the query layer
  and every `.str` call, for a stage that costs 16 ms on a realistic result.
* `export_all` streams the sequences twice, once for all specimens and once for
  the selected subset. Two passes where one would do, at 0.4 s a pass.

### Parallel scoring: asked, measured, no

Scoring 88,000 rows takes 1.9 s and is the largest remaining stage. Threads are
**slower** than serial (2.13 s vs 2.08 s) -- the work is `re.Pattern.search` in
a Python loop, holding the GIL. Four processes give 1.4x on 4 cores after
pickling the frame, which is not worth the complexity. On the result size that
matters -- 183 rows -- the whole scoring stage is 15 ms.

Reducing the work beat parallelising it: skipping the regex on already-empty
values took it 2.30 s -> 1.93 s. **The next real lever is an Arrow-backed
string dtype.** What is left is not regex, it is `to_text` and `is_empty_text`
walking an object-dtype column in Python, once per field. DuckDB can hand
pandas Arrow-backed strings, which would move that into C -- but it is a dtype
change across the query layer and every `.str` call, so it is its own task.

### The snapshot has been reordered — done

`bold_snapshot_2026-09-11.duckdb` was built before the sequence ordering fix
and has been retrofitted with `tools/reorder_sequences.py`. No re-ingest was
needed, which is what mattered once the 20 GB staging file was gone.

If a snapshot ever needs it again, that is the whole procedure:

```powershell
python tools/reorder_sequences.py `
    --snapshot "...\bold_snapshot_2026-09-11.duckdb" `
    --out "...\bold_snapshot_2026-09-11.reordered.duckdb"

python tools/verify_snapshot.py --snapshot "...reordered.duckdb"
```

It writes a new file and never touches the input, because DuckDB does not
reclaim space on `DROP`. `verify` **fails** a snapshot still in ingest order,
and `info` and `benchmark` say so on the snapshot line, so a stale one cannot
go unnoticed. The metadata-only snapshot has no sequences and needs nothing.

### The one constraint Phase 3 inherits

**`Lepidoptera` must never be materialised.** 2,095,427 rows resolve in under
two seconds and the guard fires before the fetch, but 2.1 M rows x 71 columns
into pandas is an OOM, not a slow query. The GUI needs server-side paging or a
hard display cap **from the start**, not retrofitted — and `plan_search` is
built for exactly that: it hands back the exact row set in advance, so a page
can be fetched by its `rowid`s without re-running the search.

`search_specimens` also takes an opt-in `max_records` now, and
`benchmark --max-fetch` (default 250,000) skips the wide fetch rather than
measuring an OOM kill. `run_search` is unchanged: it still enforces
`DOWNLOAD_LIMITS`.

## Phase 3 — the GUI. 3.1 is done and the answer is yes

**Shiny for Python carries the specimen table. No swap to NiceGUI + AG Grid.**

```sh
pip install -e ".[gui]"
python -m boldcurator.cli gui --snapshot bold_snapshot_2026-09-11.duckdb
```

### What 3.1 actually asked, once the benchmark had spoken

The plan framed it as "render 50,000 rows in a `DataGrid`". That is the wrong
question: a family search returns 89,479 records and an order 2,095,427, so
**no widget is ever handed the result**. `core/table.py` pages it server-side
and the grid receives one page. The right questions were whether the grid does
selection and a bulk-annotation toolbar, and whether paging feels immediate.

Measured on the 20 M-row stand-in, page size 100:

| Result | Rows | Pages | Page fetch | Sort whole result | Select all |
|---|---|---|---|---|---|
| species | 183 | 2 | 74 ms | 7 ms | 0.2 ms |
| family | 87,991 | 880 | **49 ms** | 18 ms | 21 ms |
| order | 2,023,789 | 20,238 | **47 ms** | 547 ms | 565 ms |

**A page costs the same at 183 rows and at two million**, and RSS stays at
708 MB for the largest because the result is never materialised. In the app
the 2 M case is refused by `DOWNLOAD_LIMITS`, so the real worst case is
250,000 rows.

### The design decision worth not relitigating

**The table's behaviour is in `core/table.py`, not `ui/`.** Paging, sorting,
selection that survives both, and bulk edits over a selection larger than the
page. It is the hardest screen and the one most likely to force a framework
change, so it is the one that must not depend on a framework. `ui/app.py` is a
thin shell: every control calls into `SpecimenTable`.

Sorting follows the same rule as the search — fetch **one** column for the whole
result, order the rowids in memory, carry on paging. Sorting by a computed
column (`quality_score`, `rank`, `bags_grade`) raises rather than silently
sorting by something else: it would mean scoring the whole result, which is
what paging exists to avoid. If a curator genuinely needs it, that is a
decision to make deliberately, not a thing to let happen by accident.

### Drive the UI in a browser. This is not optional

```sh
python -m boldcurator.cli gui --snapshot fixture.duckdb --port 8765 &
pip install playwright && playwright install chromium
python tools/drive_ui.py --out /tmp/shots
```

**Every UI bug so far has been invisible to the unit tests and obvious on the
first click:**

* the descending toggle and the rows-per-page select rendered perfectly,
  accepted clicks and did nothing -- their inputs were read inside
  `reactive.isolate()`, so the effects took no reactive dependency on them;
* the pager kept reporting the old page count, same cause;
* every specimen table holding a BIN-less record rendered as *"boolean value of
  NA is ambiguous"* -- `value != value` catches float NaN but **raises** on
  `pd.NA`, which is what `process_specimen_data` blanks `bin_uri` to;
* the specimen table silently ignored its own column list, because the renderer
  re-filtered to the BAGS layout.

**Two rules follow.** In a Shiny effect, read every input you want to react to
OUTSIDE `reactive.isolate()` and isolate only the writes. And when writing a
check for `drive_ui.py`, match against the **table**, not the panel: the
annotation toolbar holds a flag `<select>` whose options include every flag
name, so `"synonym" in panel.inner_text()` passes for an annotation that never
rendered. That false pass cost a round.

Run `tools/drive_ui.py` before believing any UI change works. It checks
seventeen things across all six screens and exits non-zero.

## Phase 3.2-3.6 — the six screens are in

Data Input, Species, BINs, BAGS A-E, Specimens. `tools/drive_ui.py` checks
fourteen things across all of them in a real browser and exits non-zero.

### The BAGS screens, and why they are navigators

**Each problem is kept separate, which is the whole point.** Grade C is "this
species is split across more than one BIN"; grade E is "this BIN holds more
than one species". In both the unit of work is a single species-BIN problem, so
a flat table of every grade-C record mixes dozens of unrelated problems.

| Grade | One group per | Caption |
|---|---|---|
| A, B, D | species | `Species: X (>10 specimens, single BIN)` |
| **C** | species x BIN | `Species: X - BIN: Y` |
| **E** | shared BIN | `Shared BIN: Y (2 species)` |

**E and C are marked in the navigation** and their banners say "work here
first". The screen shows the list of problems beside one problem's specimens,
with Previous/Next to walk through them.

The R app renders every group as a collapsed accordion. A navigator is used
instead because it is what "one problem at a time" actually looks like, and
because a grade with several hundred groups would otherwise put several
hundred tables in the DOM at once.

**"Select this group" replaces the selection rather than adding to it.** "Apply
to selection" acts on whatever is selected, so an accumulating selection would
mean annotating the second problem silently re-annotates the first.

Non-species-level records ride along by BIN membership -- a genus-only record
in a grade-E BIN may be the misidentification, or the evidence the BIN is fine.

**One deliberate divergence from R.** R keeps a shared-BIN group only when more
than one species-level name appears *in the downloaded records*. Grade E here
grades against the whole snapshot, so a BIN can be genuinely shared while the
other species is absent from the search. Dropping those hides the records the
grade exists to flag, so they are kept and the group says why it looks
innocent.

### The size policy, which is the thing to get right

The specimen table is paged and works at any size. The species, BIN and BAGS
screens are whole-result aggregates and cannot be paged -- a species' specimen
count is a fact about every record. They are computed **lazily and once**, on
first use, and refused above `MAX_RECORDS` with an explanation rather than
attempted and survived. Searching stays instant either way.
`core/pipeline.analyse_plan` is the entry point: everything after planning,
for a caller that already holds a plan.

### Data Input is in

Taxa, countries, continent tick-boxes, dataset and project codes, parsed
exactly as the CLI parses them so the two cannot drift. **Check size** runs
`estimate_search` and reports matching records / BINs / after-expansion
**without fetching a record** -- the pre-check the whole design rests on.

Two behaviours are stated on the screen because they are easy to get backwards:
continents and countries are a **union**, not an intersection; and the
geographic filter applies to the seed while **BIN expansion deliberately
reaches past it**, so a BIN arrives with its full context.

## Findings from the real build, worth acting on

**The R app rejects 4% of real BOLD dataset codes.** The builder reported *544
of 13,706 `DS-` codes do not match `^DS-[A-Z0-9]+$`* — the pattern
`mod_data_import_utils.R:50` validates against. Those are legitimate codes the
Shiny app would refuse before any query ran. The Python side does not validate
codes this way so it is unaffected, but the R app has a live bug here. Next
session: query the snapshot for examples and decide whether to relax the R
pattern.

```sql
SELECT DISTINCT recordset_code FROM specimen_recordset
WHERE recordset_code LIKE 'DS-%'
  AND NOT regexp_matches(recordset_code, '^DS-[A-Z0-9]+$') LIMIT 30;
```

**67.3% of COI-5P records have no species-level identification.** Normal for
BOLD — most barcode records are BIN-only or genus-level. It failed verification
only because the threshold was a guess; bounds are now set from measured data.
It does mean **BAGS grades apply to a third of the data**, which is worth
surfacing in the UI rather than letting a curator assume otherwise.

---

## Checklist

### Phase 0 — snapshot build
- [x] 0.4 `tools/build_snapshot.py`
- [x] 0.5 recordset explode with parse assertions
- [x] 0.6 `taxon` and `bin_species`
- [x] 0.7 `tools/verify_snapshot.py`
- [x] 0.8 measured figures recorded — both snapshots built and verified
- [x] 0.9 generated test fixture
- [x] 0.1 download mechanics — still manual, fine
- [ ] 0.2 confirm the course's records are public
- [ ] 0.3 CC-BY-SA attribution noted in writing

### Phase 1 — core library
- [x] 1.1–1.10 all complete

### Phase 2 — CLI, exports, parity
- [x] 2.1 `cli.py`
- [x] 2.2 `io/exports.py` — seven live formats, quoted TSVs, `inst` not `institution`
- [x] 2.3 `io/session.py` — query + processids + annotations, not the whole frame
- [x] 2.4 `parity/export_r_reference.R`
- [x] 2.5 `parity/compare.py` — **green**
- [ ] 2.6 CI workflow (Linux/macOS/Windows)

### Phase 2.5 — performance
- [x] plan-then-fetch, so a search projects only the rows it returns
- [x] one BIN-expansion pass per search, not two
- [x] vectorised BIN analysis and BAGS grading
- [x] `tools/make_benchmark_snapshot.py`, so this is measurable without the 8 GB file
- [x] re-measured on the real snapshot — 8.5 s pipeline is 0.31 s
- [x] sequences stored in specimen order, and fetched in two phases
- [x] `tools/reorder_sequences.py`, so no snapshot needs re-ingesting
- [x] ran the reorder on `bold_snapshot_2026-09-11.duckdb`
- [ ] ~~Arrow-backed strings, for the scoring stage~~ — **not needed**; see below

### Phase 3 — GUI
- [x] 3.1 spike — **Shiny for Python carries it**; paging is flat at ~47 ms
- [x] `core/table.py` — paged, sortable, selectable, bulk-annotatable
- [x] `core/grouping.py` — BAGS split into one group per problem
- [x] `core/summaries.py` — the species checklist
- [x] `tools/drive_ui.py` — browser checks, because unit tests missed dead controls
- [x] 3.2 shell — snapshot bar, name for annotation attribution
- [x] 3.4 species checklist  [x] 3.5 BIN dashboard  [x] 3.6 BAGS A–E
- [x] 3.3 Data Input — taxa, countries, continents, codes, size pre-check
- [x] `core/table.py` / `ui/app.py` — per-record selection, not only whole-group
- [x] auto-selection (best per BIN x country) wired into `SearchState.analysis`
- [x] grade-E grouping bug fixed — a species' *other*, unshared BIN no longer
      shows up as a "Shared BIN" group (see `BOLD:AAL6477` in `PROGRESS.md`
      above and `tests/test_grouping.py`)
- [x] 3.7 the six download buttons, plus the search-results CSV and the
      BIN-analysis workbook (eight downloads total, all driven in a browser)
- [ ] 3.4 gap analysis against the taxa typed in
- [ ] 3.8 session save/resume

### Phases 4–5 — packaging and distribution
- [ ] not started; compression (above) lands in 5.1

---

## Decisions already taken — do not relitigate

| Decision | Choice |
|---|---|
| Snapshot | COI-5P only, sequences included in the full build |
| Species-name rule | One unified rule replacing R's five divergent regexes |
| `HAS_IMAGE` | Removed; image requirement dropped from `RANK_2` so rank 2 stays reachable. Max score 15, not 16 |
| Dataset/project codes | Implemented properly via `specimen_recordset` |
| GUI | Shiny for Python — spiked and confirmed at 3.1, still reversible |
| Specimen table | Server-side paged from `core/table.py`; no widget ever receives a whole result |
| BAGS grade E | Evaluated against the whole snapshot, not just downloaded records |

## Known divergences from the R app — all deliberate, all tested

`parity/REPORT.md` is the authority. Four categories, each with a registered
explanation: `UNIFIED_SPECIES_RULE`, `RANK2_IMAGE_REMOVED`,
`CF_AFF_CONCORDANCE`, `R_ROW_ERROR_ZEROES_SCORE`. Adding a fifth means adding it
to `EXPLANATIONS` in `compare.py` *and* recording it in the plan.

## Process notes for the next session

- **Verify from a clean clone in a fresh virtualenv before pushing.** Three
  setup failures in this session (a `.gitignore` pattern that excluded the
  `build` package, undocumented dependencies, a stale `.pyc`) all came from
  testing where the environment already had what a new one would not.
- R is not installed by default here. `apt-get update && apt-get install -y
  --no-install-recommends r-base-core r-cran-r6` is all the parity harness
  needs, and `Rscript --vanilla` is required or the repo's `.Rprofile`
  bootstraps renv and tries to reach CRAN.
