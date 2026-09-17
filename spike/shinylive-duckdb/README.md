# Spike: shinylive + webR + duckdb

**Timeboxed to one day.** Throwaway code. Not wired into the app, and not a
prototype of BOLDcuratoR.

## The question

Can a shinylive (webR) app query a few-hundred-megabyte DuckDB snapshot fast
enough to be usable, inside the 4 GB wasm memory ceiling?

The answer decides the local/offline delivery target in
`docs/static-datapackage-plan.md`:

- **Yes** → target 4B (shinylive). Zero install, hosted as static files, no
  rewrite. A fraction of the cost of 4C.
- **No** → target 4C (Python rewrite, ~3–6 weeks) is proven necessary rather
  than assumed, at a cost of one day.

The Docker target (4A) proceeds in R regardless and is not blocked by this.

## Why this is not obvious either way

`duckdb` is available as a webR binary, so the app can run. The open question is
**memory**, not capability. wasm32 caps a browser tab at 4 GB of linear memory,
shared between R's heap, DuckDB's buffers and result frames.

Everything turns on whether **WORKERFS is lazy for DuckDB's access pattern**.
webR's docs say WORKERFS "avoids memory copies with the archive files until they
are actually opened and read". If that holds for random-access reads into a
DuckDB file, a 400 MB snapshot costs almost nothing until queried. If instead the
file lands in linear memory on mount, the snapshot budget collapses to roughly
"what fits in 4 GB alongside R", and 4B is capped far below a useful dataset.

**That single measurement is the point of the spike.** Everything else is
supporting evidence.

## Run it

Needs `DBI`, `duckdb`, `shiny`, `shinylive`, `httpuv` in R, and — from step 2
onwards — emsdk on PATH and Chrome to test in.

### On Windows

Use **PowerShell**, not `cmd` — `cmd` mishandles the `'single quotes'` below.
Everything works natively; WSL is not needed. Five differences:

0. **Install the packages as binaries**, or the install fails:

   ```powershell
   Rscript -e "install.packages(c('DBI','duckdb','shiny','shinylive','httpuv'), repos='https://cloud.r-project.org', type='binary')"
   ```

   CRAN's source versions of `duckdb`, `archive` and `shinylive` are ahead of its
   Windows binaries, and `Rscript` — unlike RStudio, which asks — silently
   compiles from source. That needs a working Rtools, and Rtools' cygwin fork
   (`cygheap read copy failed`, `exit code 0xC0000142`, typically antivirus
   interference) is a fight worth skipping: nothing here needs a newer version.
   `type='binary'` uses no compiler at all. It also means a slightly older DuckDB
   than the one the sizes below were calibrated on, so trust the sizes
   `build_fixture.R` prints over the ones in this file.

   **Note the quote style throughout**: double quotes outside, single quotes
   inside. Windows PowerShell 5.1 mangles embedded double quotes when passing
   arguments to a program, and `Rscript -e '...("app")...'` fails as a confusing
   R syntax error.

1. **`Rscript` is not on PATH** if you installed R via RStudio. Get the folder
   with `file.path(R.home("bin"), "Rscript.exe")` in the RStudio console, then add
   it under Start → "Edit environment variables for your account" → `Path` → New.
   No admin rights needed. Reopen PowerShell afterwards.
2. **Environment variables** are set separately, not inline:
   `$env:SPIKE_LOCAL_DB = "fixtures/bold_spike_01.duckdb"`
3. **emsdk** uses `.\emsdk.bat install latest`, `.\emsdk.bat activate latest`,
   `.\emsdk_env.ps1`.
4. **Use `package_fixture.ps1`**, not the `.sh`. Same arguments, same outputs. If
   PowerShell refuses to run it, allow scripts for that terminal only:
   `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass`

`package_fixture.ps1` has been exercised against a stub packager on PowerShell
7.4, not against a real emsdk on Windows. If it fails, the message it prints is
the useful part.

### 0. Sanity check without a browser

Outside webR the app opens the fixture straight off disk (`SPIKE_LOCAL_DB`), so
this exercises the fixture and the queries with no emsdk and no wasm. It proves
nothing about memory, but it catches a broken fixture in seconds rather than
after a 15-minute build.

```bash
Rscript build_fixture.R --synthetic --rows 200000
SPIKE_LOCAL_DB=fixtures/bold_spike_01.duckdb Rscript -e 'shiny::runApp("app", port = 8080)'
```

### 1. Fixtures at three sizes

Three sizes, to find WHERE it breaks rather than just whether it works at one
size. `--synthetic` needs no BOLD data, so it runs today without the login-gated
download; the two commands are **alternatives**, and the `--out` differs because
both write `bold_spike_01/02/03.duckdb` and would otherwise overwrite each other.

```bash
Rscript build_fixture.R --synthetic                       # ~49 / 180 / 441 MB
Rscript build_fixture.R --tsv /path/to/BOLD_Public.tsv.gz --out fixtures-real
```

### 2. Wrap each fixture as a WORKERFS image

Needs emsdk, for Emscripten's `file_packager.py`:

```bash
git clone https://github.com/emscripten-core/emsdk && (cd emsdk && \
  ./emsdk install latest && ./emsdk activate latest) && . emsdk/emsdk_env.sh
```

```bash
./package_fixture.sh fixtures/bold_spike_01.duckdb
mkdir -p app/fixtures && cp fixtures/bold_spike_01.{data,js.metadata} app/fixtures/
```

One fixture at a time: all three together are ~670 MB, and `export.R` copies
whatever is in `app/fixtures/` into `site/` on every run. Repeat steps 2–4 for
`_02` and `_03`.

### 3. Point the app at that fixture

Edit the two defaults at the top of `app/app.R`:

```r
FIXTURE_IMAGE <- Sys.getenv("SPIKE_FIXTURE_IMAGE", "fixtures/bold_spike_01.data")
FIXTURE_DB    <- Sys.getenv("SPIKE_FIXTURE_DB",    "/bold/bold_spike_01.duckdb")
```

The environment variables only work for step 0. `app.R` runs inside webR in the
browser, where your shell environment does not exist, so the exported app always
uses the defaults baked into the file — edit them and re-export.

### 4. Export and serve locally

```bash
Rscript export.R
```

Open <http://localhost:8080> **in Chrome**, with Task Manager (Shift+Esc) visible.

Test locally before deploying. Localhost removes network variability, and GitHub
Pages has a 100 MB per-file limit that the 200 MB and 400 MB fixtures breach —
deploy only the small fixture there, to measure the real cold-load path.

## What to measure

Run each fixture through: **Mount → Run query → Run benchmark**.

| Measure | Pass | 49 MB | 180 MB | 441 MB |
|---|---|---|---|---|
| Mount time | — | | | |
| **wasm memory after mount, before any query** | **≈ unchanged** | | | |
| Cold load (first visit, app + fixture) | < 60 s | | | |
| Taxon resolve | < 1 s | | | |
| Family query, ~5,000 rows | < 5 s | | | |
| Peak tab memory (Chrome Task Manager) | < 3 GB | | | |
| 20 consecutive queries | no crash, memory stable | | | |

The bolded row is the one that decides it. If wasm memory jumps by roughly the
fixture size on mount, WORKERFS is not lazy for this workload and 4B is capped —
record the number and stop; the remaining rows are then academic.

The in-app memory readout uses `performance.memory`, which is Chrome-only and does
not reliably account for wasm linear memory. **Chrome's Task Manager is ground
truth.** Use the in-app number for trend, the Task Manager for level.

## If WORKERFS fails

A failure is a result, not a dead end. Record the exact error from the mount
status box, then fall back: fetch the fixture into MEMFS instead and find the
largest size that survives. **That number is 4B's real budget.** If it lands below
a useful snapshot size, 4C is proven and the day was well spent.

## Known unknowns in this code

- **`webr::mount()`'s signature has moved between webR versions.** `app.R` tries
  the documented shapes in turn and reports which one worked, so a mismatch costs
  seconds rather than the day. Check against
  <https://docs.r-wasm.org/webr/latest/mounting.html> if all three fail.
- **IDBFS persistence is not implemented here.** If WORKERFS passes, test next
  whether the fixture can be cached across reloads — that is what makes the real
  thing download once rather than every visit.
- **The synthetic generator is calibrated, not real.** Measured on DuckDB 1.5.5:

  | rows | file | bytes/row | distinct taxa |
  |---|---|---|---|
  | 1,000,000 | 49.0 MB | 51.4 | 29,082 |
  | 4,200,000 | 180.5 MB | 45.1 | 121,989 |
  | 8,500,000 | 441.3 MB | 54.4 | 246,832 |

  Real BOLD data has more varied free text and will run higher — the Phase 1
  estimate implies ~75 bytes/row — so a real snapshot will be noticeably larger for
  the same row count. **Re-measure with `--tsv` before trusting any size
  conclusion**, and treat these fixtures as a proxy for browser behaviour at a given
  *file size*, not for how many BOLD records fit.

## Fixture schema

A trimmed version of Phase 1 in `docs/static-datapackage-plan.md`:

- `specimen` — 34 columns, no `nuc`, sorted
  `kingdom, phylum, class, order_, family, subfamily, genus, species, processid`.
  Taxonomy is a tree, so this one nested sort makes *every* rank's equality
  predicate contiguous at once and zone maps prune all of them. That is why there
  are no indexes: DuckDB's ART indexes must fit in RAM at build time, which is
  exactly what a browser does not have.
- `taxon` — `taxon_lc → (taxon_name, taxon_rank, n_records)`. Replaces
  `bold.public.search`: it returns which rank column to filter on, so the main
  query is a single-column equality rather than an OR across ten columns (which
  would defeat zone-map pruning entirely). It also makes result size knowable
  *before* anything is materialised.
- `_meta` — `snapshot_id`, `schema_version`, `row_count`.

`order` is a SQL keyword, so it is stored as `order_` and aliased on projection —
the same treatment Phase 1 gives `country/ocean` → `country_ocean`.

## Recording the outcome

Fill in the table above, then add a short verdict here and update
`docs/static-datapackage-plan.md` §4B/§4C with the decision and the numbers
behind it.
