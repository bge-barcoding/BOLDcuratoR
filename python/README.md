# BOLDcuratoR (Python)

Offline rewrite of the BOLDcuratoR Shiny app. Curates BOLD specimen records
against a local DuckDB snapshot of the BOLD public data package — no BOLD API,
no API key, no network at query time.

Being developed in this repository alongside the R app so the R code stays
available as the reference implementation; it moves to its own repository once
stable. Plan and checklist: [`../docs/python-app-plan.md`](../docs/python-app-plan.md).

## Status

Phase 0 (snapshot build) and Phase 1 (core library) are in progress. There is no
GUI yet — that is Phase 3, gated on the R-vs-Python parity harness passing.

## Setup

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

## Benchmark — where the time went, and where it goes now

Run against the full 7.95 GB snapshot (20,164,595 records), Windows, 32 GB.

| Step | Seconds | RSS MB | Result |
|---|---|---|---|
| open snapshot | 0.031 | 107 | |
| resolve `Danaus plexippus` | 0.000 | 112 | species, 182 records |
| estimate `Danaus plexippus` | 0.359 | 366 | seed 182 → 183 expanded |
| search+expand `Danaus plexippus` | **4.657** | 2923 | 183 rows |
| search+expand `Nymphalidae` | 8.203 | 3180 | 89,479 rows |
| search+expand `Lepidoptera` | 18.235 | 9781 | 2,095,427 rows |
| full pipeline `Danaus plexippus` | **8.500** | 9590 | 183 records |
| stream 183 sequences | 8.109 | 15492 | 183 sequences |
| export all formats | 13.687 | 15530 | 6 files |

**Resolve and estimate were always fine. One query was 90% of everything
else.** For the 183-record pipeline the split is: the search 3.46 s, the size
pre-check 0.22 s, and *all* of processing, scoring, ranking, BAGS, BIN analysis
and auto-selection together 0.13 s. Nothing downstream was ever the problem.

### Why the search cost 3.5 s to return 183 rows

The predicate a BIN-expanded search needs — "in the seed, **or** sharing one of
the seed's BINs" — is a disjunction over two subqueries. DuckDB cannot push
either half into the table scan, so it projects all 70 columns of all 20 M rows
and filters afterwards. The 2.9 GB of RSS for a 183-row result is that whole
projection, and it is why the cost barely moved between 183 rows and 89,479.

BIN expansion itself was never expensive: counting the same row set, which
touches only `sid` and `bin_uri`, takes 0.2 s.

**So resolve the rows first, then fetch them.** `plan_search` runs the narrow
pass and returns a `rowid` per matching row; `fetch_planned` joins that small
set back against `specimen.rowid`, which DuckDB *can* push into the scan as a
zone-map filter. `run_search` now calls `plan_search` once and uses it as both
the size pre-check and the row set, instead of running the expansion twice and
throwing the first answer away.

`rowid` is the key that works. `sid` is not: it is assigned before the
taxonomic sort, so it is uncorrelated with physical position and a semi-join on
it measures no better than the original. A literal `rowid IN (…)` list is no
better either — the pushdown comes from the join, not from the predicate.
**No schema change and no rebuild is needed**, which supersedes the `bin_index`
table proposed in `PROGRESS.md`.

### Measured, on a 20 M-row snapshot of the same shape

Built by `python/tools/make_benchmark_snapshot.py`: 20,164,595 rows, 70
columns, 417,999 BINs, taxonomic sort order, a species of 183 records, a family
of 87,991 and an order of 2,023,789 — i.e. the *Danaus* / *Nymphalidae* /
*Lepidoptera* shapes. Synthetic data, real query plans. Linux, 4 cores, 15 GB.

| Full pipeline | Before | After | |
|---|---|---|---|
| 183 records | 3.83 s | **0.37 s** | 10.4× |
| 87,991 records | 15.12 s | **4.04 s** | 3.7× |
| RSS, 183-record search | 2,158 MB | **371 MB** | |

Every stage output — specimens, BAGS grades, BIN content, selections, summary
— is byte-identical before and after, at all three scales, and the R parity
harness stays green.

Four changes, each measured separately:

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
which holds the GIL throughout. Four processes buy 1.4× on 4 cores after
paying to pickle the frame, which is not worth the complexity. And on the
result size that actually matters — 183 rows — the entire scoring stage is
15 ms, so there is nothing to parallelise.

The single-threaded work is reducible instead. Skipping the regex on values
already known to be empty took it from 2.30 s to 1.93 s (a third of scored
values are empty; `short_note` and `taxonomy_notes` are empty throughout). What
remains is not the regex: it is that `to_text` and `is_empty_text` are
Python-level walks of an **object-dtype** column, paid once per field. An
Arrow-backed string dtype would move that into C; that is the next real lever,
and it is a dtype change across the query layer rather than a tweak.

### Still open

`Lepidoptera` resolves to 2,095,427 rows. `plan_search` resolves them in 0.7 s
and the size guard now fires before anything is materialised, but **no table
widget should ever be handed that frame** — 2.1 M rows × 70 columns into pandas
is an OOM, not a slow query. The GUI needs server-side paging or a hard display
cap from the start, driven by the pre-check that already runs in under a
second. `benchmark --max-fetch` now refuses the wide fetch above 250,000 rows
rather than measuring an out-of-memory kill.

| Measure | Target | Now |
|---|---|---|
| taxon resolve | < 1 s | **0.003 s** ✓ |
| size pre-check | < 1 s | **0.22–0.71 s** ✓ |
| BIN-expanded search, small | sub-second | **0.22 s** ✓ |
| full pipeline, small result | ~1 s | **0.37 s** ✓ |
| full pipeline, 88 k result | — | 4.0 s, half of it scoring |
| sequence streaming | flat memory | flat ✓ |

## Testing before the GUI

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
and does not reproduce.

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
  core/       species rule, scoring, ranking, BAGS, BINs, selection, pipeline
  io/         exports and session persistence
  build/      snapshot builder and verifier
tools/        run the build/verify tools without installing
tests/        unit tests and the fixture generator
parity/       R-vs-Python comparison harness
```

Nothing under `config/`, `data/`, `core/` or `io/` may import a GUI framework —
that is what keeps the core testable headless and the GUI choice reversible.
