# BOLDcurator (Python)

Offline curation of BOLD specimen records. BOLDcurator searches, scores, ranks
and BAGS-grades COI-5P barcode records against a local DuckDB snapshot of the
[BOLD](https://boldsystems.org) public data package -- no BOLD API, no API
key, no network once you have the snapshot. It is the offline counterpart of
the [BOLDcuratoR R Shiny app](https://github.com/bge-barcoding/BOLDcuratoR),
developed in the same repository.

**Curators:** downloads, setup and an FAQ are on the
[project website](https://bge-barcoding.github.io/BOLDcuratoR/). Or install
with [uv](https://docs.astral.sh/uv/) (`pip`/`pipx` work the same way):

```sh
uv tool install --python 3.11 "boldcurator[desktop]"
boldcurator install-shortcut        # Start menu / Applications / app menu
```

The rest of this README is for developers. Current state, open items and the
decisions not to relitigate are in
[`PROGRESS.md`](https://github.com/bge-barcoding/BOLDcuratoR/blob/main/python/PROGRESS.md).

BOLD data is CC BY-SA 4.0 (Barcode of Life Data System, boldsystems.org); the
app and every export carry that attribution.

## What the app does

The desktop app (`boldcurator-desktop`, or `boldcurator desktop`) opens on a
first-run setup screen until it has a snapshot, then opens on the Guide:

| Tab | What it is |
|---|---|
| **Guide** | The first screen: how to use the app in ten steps, the BAGS grades, and a link to the BAGS paper |
| **Data** | The snapshot in use and others in the data folder; download the latest from Zenodo, check for an update, or add your own file/URL. Sessions: save, load, delete, autosave every 60 s |
| **Search** | Taxa (one per line, synonyms after commas), countries, continents (a union with countries), dataset and project codes. **Check size** estimates records and BINs without fetching anything; results are BIN-expanded past the geographic filter |
| **Gap analysis** | Each typed taxon, found or missing |
| **Species** | Grade counts, the C+E count, unnamed-BIN counts (discordant / concordant) and a species checklist, whose C+E column marks grade-E species also split across BINs |
| **BINs** | Concordant / discordant / shared BINs and a BIN content table |
| **BAGS A-E** | One screen per grade, worked one problem at a time: per species (A, B, D), per species x BIN (C), per shared BIN (E). E and C are marked "work here first" |
| **BAGS C+E** | Grade-E species that are also split across BINs (BAGS ranks E above C, so the C screen never shows them and E shows only their shared BIN): one group per species with every one of its BINs, each labelled shared, mixed genera or own; most BINs first. Grade E groups name the C+E species they hold |
| **Unnamed BINs** | One group per BIN with no species-level name (every record identified to genus or higher), so no BAGS grade covers it. Discordant BINs (more than one genus, family or order) first |

**How names are read.** The species field is shown and exported exactly as
BOLD has it; a `name_status` column says what kind of name it is, using
BOLD's `identification_rank`:

| `name_status` | When | In BAGS |
|---|---|---|
| species | a binomial passing the species rule (`config.constants.INVALID_SPECIES_PATTERN`) | graded |
| interim species | a binomial failing it (`Genus cf. species`, `Genus sp. 1`, a bare `Genus sp.`) at species or subspecies rank | graded as a species of its own |
| higher rank | rank genus or above, a one-word name, or an interim-looking name with no rank recorded | not graded; rides along with the BIN's species |
| unidentified | no name at any rank | not graded |

A BIN is discordant (grade E for every species in it; Discordant on the
BINs tab) when it holds two different species-level names -- strictly, so
*D. cf. plexippus* beside *D. plexippus* counts -- or records from more than
one genus, family or order, whatever rank they were identified to. A
record identified only to the BIN's own genus changes nothing.
| **Phylogeny** | Neighbour-joining tree (K2P, reference-anchored alignment) of one representative per selected BIN x country, with monophyly badges for grade-C species; warns above 500 tips, refuses above 1,000 |
| **Specimens** | Every record, server-side paged and sortable; Find records narrows it to one column's (or any column's) text |

Across the tables: annotations (below), "Find records" on Specimens and
every BAGS group (check all it found, then annotate them), "checked"
records and a separate representative pick (auto-selected per BIN x country
on a fresh search). Downloads: all / selected / annotated records (xlsx,
prepared in the background with a progress panel, then saved by themselves;
the annotated workbook carries the taxon updates as a second sheet), the
selected process IDs (one per line, for a BOLD dataset), the BOLD curation
report and the taxon updates (TSV), FASTA, CSV, xlsx species and BIN
analyses, and the tree as Newick.

### Working through the BAGS screens

Each screen shows one species or BIN at a time; the list above it can be
searched by typing. **Mark reviewed ✓** records that a curator has finished
with it (✓ in the list, "N reviewed" above it, **Next unreviewed ›** skips
it); ✎ marks one holding annotated records. Marks are saved with the
session, and follow the species or BIN if regrading moves it to another
screen.

**Grades from** regrades the search without touching the snapshot:
*Original IDs*; *With curated IDs* (each record's updated ID and rank, taxon
updates included; contaminated records left out); or *Excluding altered
records* (no record with an ID flag, corrected ID, contamination or taxon
update). Counts read "before → after", the checklist and downloads add the
view's grade beside the original (`bags_grade_curated` /
`bags_grade_unaltered`), and group entries note "was X". The BIN dashboard
and the Specimens table keep the original grades. A search over 20,000
records regrades in the background, with a progress panel.

While **Find records** narrows a table, an amber bar above it says how many
of how many records it shows, with **Clear filter**, and the table is
outlined.

### Annotations

Check records, then **Annotate checked…** opens a form that shows each
flag's definition, asks only for what the chosen flag needs, and won't apply
until the required fields are filled in. A record has at most one **ID
issue**, because each gives one new identification, plus two yes/no issues
that combine with it and with each other:

| | Level | Required |
|---|---|---|
| Misidentification -- the voucher is incorrectly identified | record | correct ID, rank, identification method, note |
| ID unfounded -- the ID can't be validated; strip it back to a supported rank | record | revised ID, rank, identification method, note |
| Name issue -- the name is misspelt or malformed | taxon | corrected name, rank |
| Synonym -- the name is a junior synonym | taxon | senior synonym, rank, citation |
| Contamination (tick) -- the sequence isn't from the voucher | record | the taxon the sequence comes from |
| Other issue (tick) -- a data problem that isn't the ID, e.g. lat/long | record | note |

The curator's full name is always required. The rank is suggested from the
name; the identification method is one of the iBOL accepted terms.

**Taxon updates.** Name issue and synonym fix the taxon record in BOLD, so
they are saved once, for the name, and reach every record identified as it
(listed, downloadable and removable under *Taxon updates* on the Specimens
tab). A record's own corrected ID always wins over a taxon update, whichever
came first; if that corrected ID is itself a name with a taxon update, the
update is followed too, and *Update from* shows both steps
("record → taxon (synonym)"). Sessions saved by earlier versions convert on
load: Data Issue becomes the Other issue tick, ID Uncertain becomes ID
unfounded.

The window is a native pywebview window where that works, else a Chromium
`--app` window, else a browser tab (`--window auto|native|browser-app|tab`).
Everything the app keeps is in `~/.boldcurator/`: snapshots (each with a
`.meta.json` provenance sidecar), `config.json`, `sessions.sqlite` and
`boldcurator.log`.

## Development setup

Python 3.10 or newer. From this `python/` directory:

```sh
python -m venv .venv
source .venv/bin/activate          # Windows: .venv\Scripts\activate
pip install -e ".[dev,desktop]"
python -m pytest tests/ -q
```

Or, with [uv](https://docs.astral.sh/uv/), exactly the versions pinned in
`uv.lock` (what CI tests, pip-audit checks and the desktop builds ship):
`uv sync --locked --extra dev --extra desktop`, then
`uv run python -m pytest tests/ -q`. After changing dependencies in
`pyproject.toml`, run `uv lock` and commit `uv.lock` -- CI fails on a stale
lock.

Extras: `gui` (Shiny), `desktop` (Shiny, uvicorn, pywebview), `bench`
(psutil, for `benchmark`'s memory column), `dev` (pytest, psutil).

## Command line

`boldcurator <command>`:

| Command | What it does |
|---|---|
| `desktop` | The packaged app: first-run setup, then the app in a window |
| `gui --snapshot FILE` | The Shiny app in a browser, for development (`--port`, `--sessions`); open the URL it prints, which carries a one-time launch token |
| `search --snapshot FILE --taxa ...` | Search, score, rank and grade; `--out DIR` writes the full export set |
| `resolve --snapshot FILE NAME...` | Resolve taxon names to ranks |
| `info` / `verify --snapshot FILE` | Describe / verify a snapshot (read-only) |
| `fetch-snapshot --out FILE --record/--url/--manifest ...` | Download and verify a pre-built snapshot |
| `benchmark --snapshot FILE` | Time every pipeline stage, with rows and peak RSS |
| `check-update` | Ask Zenodo whether a newer release is out (installs nothing) |
| `selftest [--network]` | Check the modules that only break once frozen; `--network` also reaches Zenodo |
| `install-shortcut` / `remove-shortcut` | Desktop shortcut for a `uv`/`pip` install |

Also installed: `boldcurator-desktop`, `boldcurator-build-snapshot`,
`boldcurator-verify-snapshot` and `boldcurator-fetch-snapshot`. Each
`tools/*.py` script runs the same code from a checkout without installing.

## Snapshots

### Getting one

The published snapshot is on Zenodo under the concept DOI
`10.5281/zenodo.22849515`, which always resolves to the latest version
(`bold_snapshot_<date>.duckdb.gz`, about 1.9 GB):

```sh
boldcurator fetch-snapshot --record 10.5281/zenodo.22849515 --out bold_snapshot.duckdb
```

The download runs as 4 parallel byte-range requests where the host allows it,
identifies itself as `BOLDcurator/<version>`, waits out `429`/`503` as
`Retry-After` asks, and verifies the checksum in the same pass that
decompresses it. Re-running only downloads when the snapshot has changed.

### Building one

You need the BOLD public data package (`BOLD_Public.<date>.tsv.gz`, ~3 GB
compressed, login-gated at `bench.boldsystems.org/index.php/datapackage`).
The extracted `.tsv` ingests 3-4x faster than the `.gz`.

```sh
# 1. header check only, in a fraction of a second -- run it first
python tools/build_snapshot.py --tsv BOLD_Public.2026-09-11.tsv --dry-run

# 2. optional trial build; recorded in the file as a partial build
python tools/build_snapshot.py --tsv BOLD_Public.2026-09-11.tsv --out trial.duckdb --limit 100000

# 3. the real build, then verify it from a fresh process
python tools/build_snapshot.py --tsv BOLD_Public.2026-09-11.tsv \
    --out bold_snapshot_2026-09-11.duckdb \
    --temp-dir /fast/local/disk --memory-limit 12GB --threads 4
python tools/verify_snapshot.py --snapshot bold_snapshot_2026-09-11.duckdb
```

Defaults keep **COI-5P only, with sequences**, and every source column except
`identifier_email`. BIN, BAGS and the 500 bp quality threshold all assume
COI-5P; `--marker ''` keeps every marker.

| Option | Why |
|---|---|
| `--dry-run` | Header check only; exit 1 if a required column is missing |
| `--limit N` | Stop after N rows; marks the result as a partial build |
| `--no-sequences` | Metadata only, no FASTA export (2.9 GB instead of 7.95 GB) |
| `--staging-path`, `--keep-staging`, `--reuse-staging` | Keep the expensive ingest and build again from it -- two builds writing different outputs need an explicit shared `--staging-path` |
| `--overwrite` | Replace an existing output, e.g. a failed partial one |
| `--memory-limit`, `--threads` | `12GB` / `4` suits a 32 GB machine; `6GB` / `2` on 16 GB |
| `--temp-dir` | DuckDB scratch on **local** disk; expect ~35 GB peak |
| `--no-hash`, `--no-progress` | Skip the source sha256 / DuckDB's progress bar |

A failed build keeps its staging database, and the error says so: retry with
`--reuse-staging --overwrite` to skip the ingest. On PowerShell, quote every
path and close every quote -- an unclosed one leaves PowerShell silently
waiting at a `>>` prompt, which looks exactly like a hang.

The 2026-09-11 build, for scale (Windows, 32 GB RAM): 33.6 GB extracted TSV,
**20,164,595 COI-5P records**, 546,861 taxa, 412,637 BINs; 7.95 GB file
(1.9 GB gzipped); about 24 minutes from source, 7 reusing staging. 67% of
records have no species-level name, so BAGS grades apply to about a third of
the data.

`tools/reorder_sequences.py` rewrites an older snapshot's `sequence` table
into specimen order (about two minutes, no re-ingest); `verify` fails a
snapshot still in ingest order.

## Testing

```sh
python -m pytest tests/ -q          # against a generated fixture snapshot
python parity/compare.py            # R-vs-Python parity; exit 1 on any unexplained difference
```

`parity/compare.py` runs the committed R reference outputs against the Python
pipeline and writes `parity/REPORT.md`; every difference must map to a
recorded divergence (see `parity/README.md`).

**Drive the UI in a real browser before believing a UI change works** --
every UI bug so far was invisible to the unit tests:

```sh
python tests/make_fake_package.py --out /tmp/fake.tsv.gz --rows 5000
python tools/build_snapshot.py --tsv /tmp/fake.tsv.gz --out /tmp/fake.duckdb
python -m boldcurator.cli gui --snapshot /tmp/fake.duckdb --port 8765 &
pip install playwright && playwright install chromium
python tools/drive_ui.py --out /tmp/shots --url "<the URL gui printed>"
```

The server only lets in the client that opened its launch URL, which
carries a one-time token (`localguard.py`), so `gui` prints it ("Open
BOLDcurator at ...") and each `drive_ui.py` run needs a fresh server.

`drive_ui.py` makes 66 checks across every tab and exits non-zero on a
failure. When adding one, match against the table, not the panel -- the
annotation form's `<select>` contains every flag name, so a panel-wide text
match passes for an annotation that never rendered.

**Benchmarking** needs no real snapshot: `tools/make_benchmark_snapshot.py`
builds a 20 M-row synthetic stand-in with the real shape in about six
minutes (no sequences). `boldcurator benchmark --snapshot FILE` times each
stage; a size-limit refusal is reported, not raised, and `--max-fetch`
(default 250,000) skips a fetch too wide to hold in memory.

## Performance

Measured on the real 7.95 GB snapshot:

| Step | Before | Now |
|---|---|---|
| Full pipeline, `Danaus plexippus` (183 records) | 8.5 s | **0.31 s** |
| Search, `Nymphalidae` (89,479 records) | 8.2 s | 2.1 s |
| Fetch 183 sequences | 7.4 s, +6 GB RSS | **0.44 s**, +169 MB |
| One page of the specimen table, any result size | -- | ~50 ms |
| Download All as xlsx, 85,067 records (fake-package snapshot) | 50 s | 5.5 s |

Three rules came out of it; keep them:

- **Resolve rows first, then fetch them.** A BIN-expanded search is a
  disjunction DuckDB can't push into the scan, so `plan_search` finds matching
  `rowid`s through narrow columns and `fetch_planned` joins that small set
  back -- which DuckDB *can* push down. `sid` doesn't work for this; `rowid`
  does.
- **`sequence` is stored in specimen order**, so a taxonomic result's
  sequences sit in a few row groups instead of all of them.
- **No parallel scoring.** Threads are slower (the regex loop holds the GIL);
  processes buy 1.4x for real complexity. On a realistic result, scoring is
  15 ms.

Performance is closed: reopen it only with a measurement showing a curator
waiting. (Round 10 did: large searches and Excel downloads. Anything slow
now runs on a background thread with visible progress -- Shiny serves every
render, click and download from one event loop, so slow work there freezes
the whole app.)

## Architecture

```
src/boldcurator/
  config/      scoring criteria, rank ladder, continents, limits, paths, Zenodo DOI
  data/        snapshot schema, read-only connection, queries (plan/fetch, BIN expansion)
  core/        species rule, scoring, ranking, BAGS, BINs, selection, pipeline,
               paged table, BAGS grouping, summaries, phylogeny, reference alignment
  io/          annotations, exports, session persistence (SQLite)
  build/       snapshot builder, verifier, Zenodo/URL fetcher
  ui/          Shiny app and first-run setup -- the ONLY place a GUI framework is imported
  cli.py       the `boldcurator` command
  desktop.py   window strategies, config, local server
  localguard.py  the local server's host check and one-time launch token
  launcher.py  `boldcurator-desktop` entry point
  shortcuts.py desktop shortcuts for uv/pip installs
tools/         build, verify, fetch, reorder, benchmark snapshot, drive-ui
tests/         pytest suite and the fake data-package generator
parity/        R-vs-Python comparison harness
packaging/     PyInstaller entry point, Windows installer, version stamping
```

- **No GUI imports outside `ui/`** (`tests/test_no_gui_dependency.py`
  enforces it), so the core is testable headless and the CLI works without
  the `gui` extra.
- **No widget ever receives a whole result.** `core/table.py` pages, sorts
  and holds selections server-side; a page costs the same for 183 rows or two
  million. Sorting by a computed column (score, rank, grade) is refused, since
  it would mean scoring the whole result.
- **Size policy.** The specimen table works at any size. Species, BIN and
  BAGS screens are whole-result aggregates, computed lazily once and refused
  above `DOWNLOAD_LIMITS["MAX_RECORDS"]` (350,000) with an explanation.
- **BAGS screens are problem navigators, not flat tables.** Grade C (a
  species split across BINs) and E (a BIN shared by species) are each a set
  of separate species-BIN problems, so the screen walks them one at a time,
  and "Check this group" replaces the selection rather than adding to it.
  Records identified only to genus ride along by BIN membership, since they
  may be the misidentification.

## Releases

Publish a GitHub release; CI builds the Windows, macOS and Linux downloads
and publishes to PyPI, with the version taken from the tag. Never edit the
version in `pyproject.toml` (it stays `0.0.0.dev0`). Zenodo archives each
release too (`/.zenodo.json`), and that archive is what the app's "a new
version is available" check reads. How the builds work, the update check,
and why each packaging flag is there:
[`packaging/README.md`](https://github.com/bge-barcoding/BOLDcuratoR/blob/main/python/packaging/README.md).

Every release also carries a CycloneDX SBOM, a `SHA256SUMS.txt` and signed
build-provenance attestations; PyPI files carry PEP 740 attestations from
trusted publishing. How to check a download:
[Verifying your download](https://github.com/bge-barcoding/BOLDcuratoR#verifying-your-download).
For IT reviewers -- network use, files, privileges:
[SECURITY_OVERVIEW.md](https://github.com/bge-barcoding/BOLDcuratoR/blob/main/docs/SECURITY_OVERVIEW.md).
Reporting a vulnerability: [SECURITY.md](https://github.com/bge-barcoding/BOLDcuratoR/blob/main/SECURITY.md).
