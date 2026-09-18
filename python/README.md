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

## Benchmark — measured 2026-09-18, and the verdict

Run against the full 7.95 GB snapshot (20,164,595 records), Windows, 32 GB.

| Step | Seconds | RSS MB | Result |
|---|---|---|---|
| open snapshot | 0.016 | 108 | |
| resolve `Danaus plexippus` | 0.000 | 113 | species, 182 records |
| estimate `Danaus plexippus` | 0.125 | 354 | seed 182 → 183 expanded |
| search+expand `Danaus plexippus` | **2.671** | 2894 | 183 rows |
| resolve `Nymphalidae` | 0.000 | 2896 | family, 87,949 records |
| estimate `Nymphalidae` | 0.172 | 2925 | seed 87,949 / 5,347 BINs → 89,479 |
| search+expand `Nymphalidae` | 9.516 | 3180 | 89,479 rows |
| resolve `Lepidoptera` | 0.016 | 3181 | order, 2,094,602 records |
| estimate `Lepidoptera` | 0.484 | 3283 | → 2,095,427 expanded |
| search+expand `Lepidoptera` | 23.891 | 9782 | 2,095,427 rows |
| full pipeline `Danaus plexippus` | **13.704** | 9614 | 183 records |
| stream 183 sequences | **8.734** | **15506** | 183 sequences |
| export all formats | 18.593 | 15541 | 6 files |

**Verdict: resolve and estimate are excellent; the rest was not ready.** Taxon
resolution is effectively instant (0–16 ms against a 546,861-row lookup) and the
size pre-check costs 0.1–0.5 s, so a search's cost is known before anything is
materialised — both design goals met.

Everything after that was too slow, and two causes were outright bugs:

1. **`SELECT * FROM bin_species` ran on every search** — the whole table into
   pandas regardless of result size. That is most of 13.7 s for a *183-record*
   search. Now fetches only the BINs the result touches. **Fixed.**
2. **`iter_sequences` had an `ORDER BY`** on the output of a semi-join over
   20 M rows. A sort is a blocking operator: it materialises the entire result
   before yielding a row, which defeats the streaming the function exists for
   and made its "constant memory" docstring false — 8.7 s and 15.5 GB to fetch
   183 sequences. No caller needs ordered output. **Fixed.**
3. **BIN expansion is a full table scan** — ~2.7 s floor, because `specimen` is
   sorted taxonomically so `bin_uri IN (…)` cannot use zone maps. Still open;
   see `PROGRESS.md`. It is a ~100× improvement on the R app's HTTP loop but
   misses the sub-second target.

**Re-run the benchmark after pulling** — the two fixes are unmeasured against
real data, and the numbers decide whether a schema change is needed.

## Testing before the GUI

```sh
pip install -e ".[dev]"

python -m pytest tests/ -q        # correctness, against a generated fixture
python parity/compare.py          # R-vs-Python parity; exit 1 on any surprise

# the one that needs the real snapshot
python -m boldcurator.cli benchmark --snapshot /path/to/bold_snapshot_2026-09-11.duckdb --export
```

`benchmark` times each stage separately and reports rows and peak RSS. Install
`psutil` (`pip install -e ".[bench]"`) for the memory column; without it the
command still runs and says so.

| Measure | Target | 2026-09-18 |
|---|---|---|
| taxon resolve | < 1 s | **0.000–0.016 s** ✓ |
| size pre-check | < 1 s | **0.125–0.484 s** ✓ |
| BIN-expanded search | sub-second | 2.7 s (183 rows) ✗ |
| full pipeline, small result | ~1 s | 13.7 s ✗ — fixed, unmeasured |
| sequence streaming | flat memory | 15.5 GB ✗ — fixed, unmeasured |

A `SizeLimitExceeded` refusal is **reported, not raised**. Note `Lepidoptera`
did *not* trip the guard: 2,095,427 expanded records against a `MAX_RECORDS` of
250,000, because `--no-limits` is off only for `run_search` — the raw
`search_specimens` path the benchmark uses has no guard. That is worth
revisiting.

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
