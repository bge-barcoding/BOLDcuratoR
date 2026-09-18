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

## Building a snapshot

You need the BOLD public data package (`BOLD_Public.<date>.tsv.gz`, ~3 GB
compressed, login-gated at
`bench.boldsystems.org/index.php/datapackage`).

```sh
python tools/build_snapshot.py \
    --tsv /path/to/BOLD_Public.2026-09-01.tsv.gz \
    --out /path/to/bold_snapshot_2026-09-01.duckdb \
    --temp-dir /path/to/fast/local/scratch

python tools/verify_snapshot.py --snapshot /path/to/bold_snapshot_2026-09-01.duckdb
```

Defaults keep **COI-5P only, with sequences**. BIN, BAGS and the 500 bp
`SEQ_QUALITY` threshold all assume COI-5P, so other markers add size without
serving the scoring logic. `--marker ''` keeps everything; `--no-sequences`
builds a metadata-only file (smaller, but no FASTA export).

Useful options:

| Option | Why |
|---|---|
| `--memory-limit`, `--threads` | `8GB` / `4` suits a 16 GB box; drop to `6GB` / `2` if it is also doing other work |
| `--temp-dir` | Put DuckDB's scratch on **local** disk — expect ~35 GB peak. On network storage the build takes 3–5× longer |
| `--limit N` | Stop after N rows, to test the pipeline without a full build |
| `--no-hash` | Skip the source sha256, saving one pass over the file |

Expect roughly 30–60 min, ~11 GB peak RSS and ~60 GB free disk on 16 GB /
8 vCPU / NVMe. If the builder has the disk, `gunzip` the package first — a gzip
stream cannot be read in parallel, so a plain TSV ingests 3–4× faster.

**Verify from a fresh process before distributing anything.** A leftover
write-ahead log makes a DuckDB file unopenable read-only, and the process that
wrote it cannot detect that, because it still holds a read-write handle.

### Measured build figures

Record real numbers here after the first full build — the size estimates in the
plan are estimates, and the distribution shape depends on what this turns out to
be.

| Snapshot | Rows | File size | Build time | Machine |
|---|---|---|---|---|
| _(to be filled in)_ | | | | |

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
