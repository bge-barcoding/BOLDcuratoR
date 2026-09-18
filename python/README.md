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

### Measured build figures

Record real numbers here after the first full build — the size estimates in the
plan are estimates, and the distribution shape depends on what this turns out to
be.

Source: `BOLD_Public.11-Sep-2026.tsv`, 33.57 GB extracted, 76 columns, of which
71 are kept. **20,164,595 records are COI-5P.**

| Step | Time | Machine |
|---|---|---|
| sha256 of source | 28.5 s | Windows, 32 GB, `--memory-limit 12GB --threads 4` |
| ingest + COI-5P filter | 362 s | " |
| sort + write `specimen` | 669 s | " |

| Snapshot | Rows | File size |
|---|---|---|
| metadata only (`--no-sequences`) | 20,164,595 | _(to be filled in)_ |
| full (with sequences) | 20,164,595 | _(to be filled in)_ |

At ~658 bp per record, 20.16 M sequences are roughly 13 GB of raw text, so the
full snapshot is expected well above the 3 GB target. That is what makes the
metadata/sequence split worth measuring rather than assuming.

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
