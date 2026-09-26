# BOLDcuratoR (Python)

Offline rewrite of the BOLDcuratoR Shiny app. Curates BOLD specimen records
against a local DuckDB snapshot of the BOLD public data package — no BOLD API,
no API key, no network at query time.

Developed in this repository alongside the R app (the reference
implementation) rather than a separate one — the project owner's call, so
work on either app can reference the other and both stay in sync. See the
curator-facing project website (`../website/`) for downloads and setup
instructions, or the plan and checklist:
[`../docs/python-app-plan.md`](../docs/python-app-plan.md).

## Status

All six screens (Data/Search, Species, BINs, the five BAGS grades,
Specimens) are built and working, with installers for Windows, macOS and
Linux built automatically on every tagged release
(`.github/workflows/python-release.yml`). Seven rounds of curator-reported
feedback have been fixed; see `PROGRESS.md` for the full history and
current state.

## Installing (curators)

There are two ways to install, and a machine can have both:

- **Downloadable installers** for Windows, macOS and Linux, from the
  [project website](https://bge-barcoding.github.io/BOLDcuratoR/#download)
  (built by `.github/workflows/python-release.yml`; see
  `packaging/README.md`).
- **One pasted command** that installs from PyPI with
  [uv](https://docs.astral.sh/uv/) and adds a "BOLDcurator (Python)"
  shortcut to the Start menu, Applications folder or app menu. There is no
  "unidentified developer" warning on a Mac with this route.

  ```sh
  # macOS / Linux
  curl -LsSf https://bge-barcoding.github.io/BOLDcuratoR/install.sh | sh
  ```
  ```powershell
  # Windows (PowerShell)
  powershell -ExecutionPolicy ByPass -c "irm https://bge-barcoding.github.io/BOLDcuratoR/install.ps1 | iex"
  ```

  Or run the steps yourself:
  `uv tool install --python 3.11 "boldcurator[desktop]"` then
  `boldcurator install-shortcut`. Update with `uv tool upgrade boldcurator`.
  To uninstall, run `boldcurator remove-shortcut` then
  `uv tool uninstall boldcurator`. `pip`/`pipx` work the same way.

Every route reports the same version, taken from the GitHub release tag
(`V3.4` -> `3.4.0`); `pyproject.toml`'s own `0.0.0.dev0` is a placeholder
that is never edited by hand -- see `packaging/README.md`, "Releases".

Both routes use the same `~/.boldcurator/` folder for the snapshot, config
and saved sessions.

## Setup (development)

Python 3.10 or newer. From this `python/` directory:

```sh
pip install -e .
```

That pulls in DuckDB, pandas and openpyxl, and puts `boldcurator`,
`boldcurator-build-snapshot` and `boldcurator-verify-snapshot` on your PATH.
The `-e` (editable) install means a `git pull` takes effect without
reinstalling.

On Windows, if `pip` is not found, use `python -m pip install -e .`.

A virtual environment is recommended but not required:

```sh
python -m venv .venv
# Windows:  .venv\Scripts\activate
# macOS/Linux:  source .venv/bin/activate
pip install -e .
```

Add the test dependencies with `pip install -e ".[dev]"`, then `pytest`.

Once installed, either invocation style works — `boldcurator-build-snapshot ...`
or `python tools/build_snapshot.py ...`. The examples below use the second so
they work from a checkout whether or not you installed.

## Building a snapshot

You need the BOLD public data package (`BOLD_Public.<date>.tsv.gz`, ~3 GB
compressed, login-gated at `bench.boldsystems.org/index.php/datapackage`).
Either the `.gz` or the extracted `.tsv` works; the extracted one ingests 3-4x
faster, because a gzip stream cannot be read in parallel.

### 1. Check the columns first (seconds)

```sh
python tools/build_snapshot.py --tsv /path/to/BOLD_Public.2026-09-11.tsv --dry-run
```

This reads only the header and reports what would be kept, what is absent, and
whether a build would run at all. It costs a fraction of a second even on a
30 GB file, so run it before committing to an hour of ingest. Exit status is 1
if a required column is missing.

### 2. Trial build (optional, minutes)

```sh
python tools/build_snapshot.py --tsv <file> --out trial.duckdb --limit 100000
python tools/verify_snapshot.py --snapshot trial.duckdb
```

`--limit` stops after N rows. The result is recorded in the snapshot as a
**partial build**, so `verify` and `boldcurator info` both say so and it cannot
quietly be mistaken for the real thing. Verification passes with warnings --
reference taxa that a partial or taxonomically scoped snapshot legitimately
lacks are warnings, not failures.

### 3. Full build

Because the ingest is the expensive part, do it **once** and build both
snapshots from it:

```powershell
# metadata only -- usable immediately, and keeps the ingest for the next build
python tools/build_snapshot.py `
    --tsv "C:\path\BOLD_Public.2026-09-11.tsv" `
    --out "C:\path\bold_snapshot_meta_2026-09-11.duckdb" `
    --staging-path "C:\path\bold.staging" --keep-staging `
    --no-sequences --temp-dir "C:\path\duckdb_tmp" `
    --memory-limit 12GB --threads 4

# full snapshot, reusing that ingest instead of re-reading the source
python tools/build_snapshot.py `
    --tsv "C:\path\BOLD_Public.2026-09-11.tsv" `
    --out "C:\path\bold_snapshot_2026-09-11.duckdb" `
    --staging-path "C:\path\bold.staging" --reuse-staging `
    --temp-dir "C:\path\duckdb_tmp" --memory-limit 12GB --threads 4

python tools/verify_snapshot.py --snapshot "C:\path\bold_snapshot_2026-09-11.duckdb"
```

Staging defaults to `<out>.staging`, so two builds writing **different** output
files need an explicit `--staging-path` to share one ingest.

> **On Windows / PowerShell**, quote each path and close every quote. An
> unclosed quote makes PowerShell wait silently for more input (the `>>`
> prompt) rather than run anything, which looks exactly like a hang.

Defaults keep **COI-5P only, with sequences**. BIN, BAGS and the 500 bp
`SEQ_QUALITY` threshold all assume COI-5P, so other markers add size without
serving the scoring logic. `--marker ''` keeps everything; `--no-sequences`
builds a metadata-only file (smaller, but no FASTA export).

Useful options:

| Option | Why |
|---|---|
| `--dry-run` | Header check only; no output file. Run this first |
| `--limit N` | Stop after N rows. Marks the result as a partial build |
| `--no-sequences` | Metadata only. Staging still keeps sequences, so a later full build can reuse it |
| `--staging-path` | Share one ingest between builds writing different outputs |
| `--keep-staging` | Keep staging after a successful build, for the build after it |
| `--reuse-staging` | Skip the ingest and use an existing staging database |
| `--overwrite` | Replace an existing output file, e.g. a partial one from a failed run |
| `--memory-limit`, `--threads` | `12GB` / `4` suits a 32 GB box; `6GB` / `2` on 16 GB |
| `--temp-dir` | Put DuckDB's scratch on **local** disk -- expect ~35 GB peak |
| `--no-hash` | Skip the source sha256, saving one pass over the file |
| `--no-progress` | Suppress DuckDB's progress bar |

**A failed build keeps its staging database**, and the error says so. Retry with
`--reuse-staging --overwrite` to skip the ingest entirely.

**Verify from a fresh process before distributing anything.** A leftover
write-ahead log makes a DuckDB file unopenable read-only, and the process that
wrote it cannot detect that, because it still holds a read-write handle.

### Measured build figures — both snapshots are built; do not rebuild

Source: `BOLD_Public.11-Sep-2026.tsv`, 33.57 GB extracted, 76 columns, 71 kept.
**20,164,595 records are COI-5P.** Windows, 32 GB RAM,
`--memory-limit 12GB --threads 4`.

| Snapshot | Rows | File size | Zipped | Build time |
|---|---|---|---|---|
| metadata (`--no-sequences`) | 20,164,595 | 2.89 GB | ~750 MB | 1037.6 s from source |
| **full (with sequences)** | 20,164,595 | **7.95 GB** | **1.9 GB** | 439.8 s reusing staging |

20,096,366 records carry a sequence. 546,861 taxa, 412,637 BINs, 32,146
recordset codes.

Per step, from source: sha256 28.4 s, ingest + COI-5P filter 344.5 s, sort +
write `specimen` 627.3 s, explode recordsets 31.8 s, `taxon` 2.3 s,
`bin_species` 1.2 s. Reusing staging skips the ingest, which is why the second
build took 7 minutes rather than 24 — that is what `--keep-staging` is for.

Null fractions in the real data, which is what the verifier's bounds are set
from: `bin_uri` 6.9%, `species` **67.3%**, `country_ocean` 3.1%,
`nuc_basecount` 0.1%. Species is high because most BOLD barcode records are
BIN-only or identified no finer than genus — normal, not a defect. It does mean
**BAGS grades apply to about a third of the data**.

**Sequences stay in the single file.** 7.95 GB compresses to 1.9 GB (4.2x — DNA
over a four-letter alphabet compresses hard), so the download is comfortable and
the metadata/sequence split is not needed.

**The full snapshot has since been reordered** with
`tools/reorder_sequences.py`, which retrofits the fast sequence layout from the
snapshot itself in about two minutes — no re-ingest. A build from source now
also sorts the sequence table, which adds one ~13 GB sort to the figures above.

A benchmark run against a freshly written file measures cold disk on every
step: on the first run after reordering, `plan Lepidoptera` took 1.859 s against
0.359 s warm. Run it twice if the absolute numbers matter.

## Benchmark — where the time went, and where it goes now

Measured on the full 7.95 GB snapshot (20,164,595 records), Windows, 32 GB.

| Step | Before | After | |
|---|---|---|---|
| `estimate` `Danaus plexippus` | 0.359 | 0.406 | pre-check, unchanged |
| search `Danaus plexippus` (183 rows) | **4.657** | **0.219** | plan 0.094 + fetch 0.125 |
| search `Nymphalidae` (89,479 rows) | 8.203 | 2.109 | plan 0.094 + fetch 2.015 |
| `Lepidoptera` (2,095,427 rows) | 18.235 | 0.359 | resolved, then refused |
| **full pipeline `Danaus plexippus`** | **8.500** | **0.312** | **27×** |

Resolve and estimate were always fine. One query was 90% of everything else:
for the 183-record pipeline, *all* of processing, scoring, ranking, BAGS, BIN
analysis and auto-selection together came to 0.03 s.

### Why one query cost 4.7 s to return 183 rows

The predicate a BIN-expanded search needs — "in the seed, **or** sharing one of
the seed's BINs" — is a disjunction over two subqueries. DuckDB cannot push
either half into the table scan, so it projects all 71 columns of all 20 M rows
and filters afterwards. The 2.9 GB of RSS for a 183-row result was that whole
projection, and it is why the cost barely moved between 183 rows and 89,479.

BIN expansion itself was never expensive: counting the same row set, which
touches only `sid` and `bin_uri`, takes 0.1–0.4 s at every scale.

**So resolve the rows first, then fetch them.** `plan_search` runs the narrow
pass and returns a `rowid` per matching row; `fetch_planned` joins that small
set back against `specimen.rowid`, which DuckDB *can* push into the scan as a
zone-map filter. `run_search` calls `plan_search` once and uses it as both the
size pre-check and the row set, instead of running the expansion twice.

`rowid` is the key that works. `sid` is not: it is assigned before the
taxonomic sort, so it is uncorrelated with physical position, and a semi-join
on it measures no better than the original. A literal `rowid IN (…)` list is no
better either — the pushdown comes from the join, not from the predicate.
**No schema change and no rebuild**, which supersedes the `bin_index` table
proposed earlier.

### Then the sequences, which are the same story in a different table

With the pipeline at 0.3 s, fetching 183 sequences took **7.4 s and pushed RSS
from 1.5 GB to 7.6 GB**, and the export set 12.2 s — `export_all` writes two
FASTA files, so it pays that cost twice.

The cause is not the query. `sequence` is stored in **ingest order**, while
every sequence fetch is driven by a taxonomic result, whose rows are contiguous
in `specimen` order and scattered across every row group in ingest order. So
nothing can prune, and `nuc` — 5 GB of it — is projected in full.

Both halves are needed, and neither helps alone. Fetching one species' 183
sequences, each in a fresh process, on a 20 M-row snapshot with a 4.1 GB `nuc`
column:

| `sequence` stored in | one query | plan + fetch |
|---|---|---|
| ingest order (old) | 2.64 s / 4.5 GB | 2.39 s / 4.5 GB |
| **specimen order (new)** | 3.06 s / 4.6 GB | **0.21 s / 271 MB** |

End to end through `benchmark`, on the real 7.95 GB snapshot:

| Step | Ingest order | Specimen order | |
|---|---|---|---|
| stream 183 sequences | 7.375 s, RSS +6,088 MB | **0.437 s, RSS +169 MB** | 17× |
| export all formats | 12.234 s | **1.328 s** | 9.2× |

`iter_sequences` now resolves rowids in a pass that never touches `nuc`, then
fetches by rowid. The builder sorts `sequence` by the same key as `specimen`.
That adds one ~13 GB sort to a build — a sort with nothing else beside it, with
`memory_limit` and `temp_directory` already set, so it spills rather than dying
the way the earlier join-plus-sort did.

**An existing snapshot does not need rebuilding from the TSV**, which matters
because the 20 GB staging file was deleted. Everything needed is already in the
snapshot:

```sh
python tools/reorder_sequences.py \
    --snapshot bold_snapshot_2026-09-11.duckdb \
    --out bold_snapshot_2026-09-11.reordered.duckdb
python tools/verify_snapshot.py --snapshot bold_snapshot_2026-09-11.reordered.duckdb
```

103 seconds for a 6.12 GB snapshot, peak RSS 1.4 GB, against about 24 minutes
for a full re-ingest. It writes a new file and never touches the input, because
DuckDB does not reclaim space on `DROP`. `verify` now fails a snapshot still in
ingest order, and `info` and `benchmark` say so on the snapshot line.

### The rest of the pipeline, measured on a 20 M-row stand-in

Built by `tools/make_benchmark_snapshot.py` — 20,164,595 rows, 70 columns,
417,999 BINs, taxonomic sort order, a species of 183 records, a family of
87,991 and an order of 2,023,789. Synthetic data, real query plans.

| Full pipeline | Before | After | |
|---|---|---|---|
| 183 records | 3.83 s | **0.37 s** | 10.4× |
| 87,991 records | 15.12 s | **4.04 s** | 3.7× |
| RSS, 183-record search | 2,158 MB | **371 MB** | |

Every stage output — specimens, BAGS grades, BIN content, selections, summary —
is byte-identical before and after, at all three scales, and the R parity
harness stays green.

| Change | Where | 183 rows | 87,991 rows |
|---|---|---|---|
| plan-then-fetch on `rowid` | `data/queries.py` | 3.49 → 0.22 s | 3.71 → 1.02 s |
| one expansion pass, not two | `core/pipeline.py` | −0.22 s | −0.24 s |
| vectorised BIN analysis | `core/bins.py` | 0.061 → 0.021 s | 8.08 → 0.48 s |
| vectorised BAGS grading | `core/bags.py` | — | 2.61 → 0.94 s (20k species) |

### Can the scoring run in parallel? No — measured

Scoring 88,000 rows takes 1.9 s, and it is the largest remaining stage. It does
not want threads:

| | Seconds |
|---|---|
| serial | 2.08 |
| `ThreadPoolExecutor(2)` | 2.13 |
| `ThreadPoolExecutor(4)` | 2.09 |
| `ProcessPoolExecutor(4)`, incl. pickling | 1.52 |

Threads are **slower** — the work is `re.Pattern.search` inside a Python loop,
which holds the GIL throughout. Four processes buy 1.4× on 4 cores after paying
to pickle the frame, which is not worth the complexity. And on the result size
that actually matters — 183 rows — the entire scoring stage is 15 ms.

The single-threaded work is reducible instead. Skipping the regex on values
already known to be empty took it from 2.30 s to 1.93 s. What remains is not
the regex: it is that `to_text` and `is_empty_text` are Python-level walks of an
**object-dtype** column, paid once per field. An Arrow-backed string dtype would
move that into C.

**That lever is deliberately not pulled.** Curators download at most ~10,000
sequences at a time and rarely that, so the stage costs 16 ms on a realistic
result; the change touches the dtype of every column and every `.str` call in
the query layer. Performance is closed — reopen it only with a measurement
showing a real user waiting.

### Still open

`Lepidoptera` resolves to 2,095,427 rows in 0.36 s and the size guard fires
before anything is materialised, but **no table widget should ever be handed
that frame** — 2.1 M rows × 71 columns into pandas is an OOM, not a slow query.
The GUI needs server-side paging or a hard display cap from the start, driven by
the pre-check. `benchmark --max-fetch` refuses the wide fetch above 250,000 rows
rather than measuring an out-of-memory kill.

| Measure | Target | Now |
|---|---|---|
| taxon resolve | < 1 s | **0.000 s** ✓ |
| size pre-check | < 1 s | **0.09–0.41 s** ✓ |
| BIN-expanded search, small | sub-second | **0.22 s** ✓ |
| full pipeline, small result | ~1 s | **0.31 s** ✓ |
| sequence fetch, small result | sub-second | **0.44 s** ✓ (reordered snapshot) |
| full pipeline, 88 k result | — | 4.0 s, half of it scoring |

## The GUI

```sh
pip install -e ".[gui]"
python -m boldcurator.cli gui --snapshot bold_snapshot_2026-09-11.duckdb
```

Only `ui/` may import a GUI framework; `tests/test_no_gui_dependency.py`
enforces it over `config/`, `data/`, `core/`, `io/` and `build/`. Importing
`boldcurator.ui` does not import Shiny either, so the CLI keeps working on an
install without the `gui` extra.

### Phase 3.1 — the spike, and its answer

The plan framed 3.1 as "can a `DataGrid` render 50,000 rows?". The benchmark
reframed the question: a family search returns 89,479 records and an order
2,095,427, so **no widget is ever handed the result**. `core.table.SpecimenTable`
pages it server-side and the grid receives one page.

That is why the hard part lives in `core/`, not `ui/`: paging, sorting,
selection that survives both, and bulk edits over a selection larger than the
page. Shiny or NiceGUI, the widget only asks for a page and reports clicks — so
the framework stays swappable for a day's work rather than a fortnight's.

Sorting works the same way. Sorting the result fetches **one** column for the
whole of it, orders the rowids in memory, and carries on paging; the other 70
columns are never touched outside the visible page. Sorting by a *computed*
column (`quality_score`, `rank`, `bags_grade`) is refused rather than silently
ignored — it would mean scoring the whole result, which is what paging exists to
avoid.

**Measured on the 20 M-row stand-in, page size 100:**

| Result | Rows | Pages | Page fetch | Sort whole result | Select all |
|---|---|---|---|---|---|
| species | 183 | 2 | 74 ms | 7 ms | 0.2 ms |
| family | 87,991 | 880 | **49 ms** | 18 ms | 21 ms |
| order | 2,023,789 | 20,238 | **47 ms** | 547 ms | 565 ms |

**A page costs the same whether the result holds 183 rows or two million**, and
RSS stays at 708 MB for the largest because the result is never materialised.
In the app the 2 M case is refused by `DOWNLOAD_LIMITS` anyway, so the real
worst case is 250,000 rows.

**Verdict: Shiny for Python carries it. No swap to NiceGUI + AG Grid.** The
grid does row selection and virtualises its own DOM; everything expensive
happens below it.

### The six screens

| Screen | What it is | Cost |
|---|---|---|
| Data Input | taxa, countries, continents, dataset and project codes, and a size pre-check | instant |
| Species | one row per species: counts, BINs, grade, countries, mean quality | whole-result |
| BINs | total / concordant / discordant / shared, then the BIN table | whole-result |
| BAGS A–E | one screen per grade, split into groups | whole-result |
| Specimens | every record, paged | any size |

**The BAGS screens are the point of the app**, and they are group navigators
rather than one long table. Grade C means "this species is split across more
than one BIN" and grade E means "this BIN holds more than one species" — in
both, the unit of work is a single species-BIN problem, and a flat table of
every grade-C record mixes dozens of unrelated problems together. So:

| Grade | One group per | Caption |
|---|---|---|
| A, B, D | species | `Species: X (>10 specimens, single BIN)` |
| **C** | species × BIN | `Species: X — BIN: Y` |
| **E** | shared BIN | `Shared BIN: Y (2 species)` |

**E and C are marked in the navigation** and say "work here first" on the
banner, because they are the grades where the barcode and the name disagree.
The screen shows a list of problems beside one problem's specimens, with
Previous/Next to walk through them, and "Select this group" **replaces** the
selection rather than adding to it — otherwise annotating the second problem
would silently re-annotate the first.

Non-species-level records ride along by BIN membership. A record identified
only to genus carries no BAGS grade of its own, but if it sits in a grade-E BIN
it is part of the problem: it may be the misidentification, or the evidence the
BIN is fine. Ported from `organize_grade_specimens`
(`mod_bags_grading_utils.R:32-149`).

One divergence from the R app, and it is deliberate. R keeps a shared-BIN group
only when more than one species-level name appears **in the downloaded
records**. Grade E here is graded against the whole snapshot, so a BIN can be
genuinely shared while the other species is absent from this search — dropping
those would hide the records the grade exists to flag. They are kept, and the
group says why it looks innocent.

### Data Input, and the pre-check

Taxa (one per line, synonyms after commas), countries, continent tick-boxes,
and dataset/project codes — parsed exactly as the CLI parses them, so the two
cannot drift. **Check size** runs `estimate_search` and reports matching
records, BINs, and the count after BIN expansion **without fetching a single
record**; that is the pre-check the whole design rests on, and it costs a
fraction of a second even for an order of two million.

Two behaviours are easy to get backwards and are stated on the screen itself:

- continents and countries are a **union**, not an intersection — ticking
  Europe *and* typing Canada gives you both;
- the geographic filter applies to the records your taxa match, and **BIN
  expansion deliberately reaches past it** — records sharing those BINs are
  pulled in wherever they are from, which is what gives a BIN its full context.

### The size policy

The specimen table is paged and works at any size. The species, BIN and BAGS
screens are whole-result aggregates — a species' specimen count is a fact about
every record in the result, so there is no paging around it. They are computed
**lazily and once**, on first use, and refused above `DOWNLOAD_LIMITS`
`MAX_RECORDS` with an explanation rather than attempted and survived. Searching
stays instant either way.

### Drive the UI in a browser before believing it

```sh
python -m boldcurator.cli gui --snapshot fixture.duckdb --port 8765 &
pip install playwright && playwright install chromium
python tools/drive_ui.py --out /tmp/shots
```

Not optional colour. Every UI bug so far has been invisible to the unit tests
and obvious on the first click:

- the descending toggle and the rows-per-page select rendered perfectly,
  accepted clicks and **did nothing** — their inputs were read inside
  `reactive.isolate()`, so the effects took no reactive dependency on them;
- the pager kept reporting the old page count, for the same reason;
- every specimen table holding a BIN-less record rendered as *"boolean value of
  NA is ambiguous"* — `value != value` catches float NaN but **raises** on
  `pd.NA`, which is what `process_specimen_data` blanks `bin_uri` to;
- the specimen table silently ignored its own column list, because the renderer
  re-filtered to the BAGS layout.

`tools/drive_ui.py` checks fourteen things across all six screens and exits
non-zero. A note on writing checks for it: match against the **table**, not the
panel. The annotation toolbar holds a flag `<select>` whose options include
every flag name, so `"synonym" in panel.inner_text()` passes for an annotation
that never rendered — a false pass that took a round to notice.

## Testing

```sh
pip install -e ".[dev]"

python -m pytest tests/ -q        # correctness, against a generated fixture
python parity/compare.py          # R-vs-Python parity; exit 1 on any surprise

# the one that needs the real snapshot
python -m boldcurator.cli benchmark --snapshot /path/to/bold_snapshot_2026-09-11.duckdb --export
```

`benchmark` times each stage separately and reports rows and peak RSS. It
splits the search into its two halves (`plan` then `fetch`) and times the
post-search stages individually, because a single "search" number hid that
essentially all of it was one query. Install `psutil`
(`pip install -e ".[bench]"`) for the memory column; without it the command
still runs and says so.

**You do not need the real snapshot to benchmark.**
`tools/make_benchmark_snapshot.py` builds a 20 M-row stand-in with the same
shape in about six minutes — see the benchmark section above for what it does
and does not reproduce. It carries no sequences, so it measures everything up
to the FASTA exports.

```sh
python tools/make_benchmark_snapshot.py --out bench.duckdb
python -m boldcurator.cli benchmark --snapshot bench.duckdb
```

A `SizeLimitExceeded` refusal is **reported, not raised** — whether
`DOWNLOAD_LIMITS` is set sensibly for real data is one of the things being
measured. `search_specimens` now takes an opt-in `max_records`, and the
benchmark's own `--max-fetch` (default 250,000) skips the wide fetch rather
than materialising 2.1 M rows × 70 columns into pandas and being killed.

## Testing without the real package

`tests/make_fake_package.py` generates a small stand-in with the same header
shape, a shared BIN, genus-level records, and free text containing unbalanced
quotes — enough to exercise every branch of the builder in seconds.

```sh
python tests/make_fake_package.py --out /tmp/fake.tsv.gz --rows 5000
python tools/build_snapshot.py --tsv /tmp/fake.tsv.gz --out /tmp/fake.duckdb
python tools/verify_snapshot.py --snapshot /tmp/fake.duckdb
```

## Layout

```
src/boldcurator/
  config/     scoring criteria, rank ladder, continents, limits
  data/       snapshot schema, connection handling, queries
  core/       species rule, scoring, ranking, BAGS, BINs, selection,
              pipeline, the paged table, the BAGS grouping, the summaries
  io/         exports and session persistence
  build/      snapshot builder and verifier
  ui/         Shiny app — the ONLY place a GUI framework is imported
tools/        build, verify, reorder, benchmark-snapshot, drive-ui
tests/        unit tests and the fixture generator
parity/       R-vs-Python comparison harness
```

Nothing under `config/`, `data/`, `core/`, `io/` or `build/` may import a GUI
framework — that is what keeps the core testable headless and the GUI choice
reversible. The specimen table's behaviour lives in `core/table.py` for the
same reason: it is the part most likely to force a framework change, so it is
the part that must not depend on one.
