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
Everything works natively; WSL is not needed. Six differences:

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
2. **Allow scripts in this terminal, first thing.** Both `emsdk_env.ps1` and
   `package_fixture.ps1` are blocked by the default execution policy. This
   affects the current terminal only and needs no admin rights:

   ```powershell
   Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
   ```

3. **Environment variables** are set separately, not inline, and
   `SPIKE_LOCAL_DB` is read after `runApp` has moved into `app/`:
   `$env:SPIKE_LOCAL_DB = "../fixtures/bold_spike_01.duckdb"`
4. **emsdk** uses `.\emsdk.bat install latest`, `.\emsdk.bat activate latest`,
   `.\emsdk_env.ps1`.
5. **Use `package_fixture.ps1`**, not the `.sh`. Same argument, same outputs.

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
Rscript -e 'shiny::runApp("app", port = 8080)'
```

`runApp` moves the working directory into `app/`, so the default this falls back
to is `../fixtures/bold_spike_01.duckdb`. Override it with `SPIKE_LOCAL_DB` only
as an absolute path, or one written relative to `app/`.

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
```

**Never stage a fixture inside `app/`.** `shinylive::export()` bundles everything
under the app directory into its payload, and that payload is built as a single
JavaScript array: a ~180 MB fixture there kills the app outright with
`Error starting app! Invalid array length`, before any of this app's code runs.
`export.R` refuses to export if `app/fixtures/` exists, and copies the one
fixture `app.R` names from `fixtures/` into `site/` itself.

Repeat steps 2–4 for `_02` and `_03`.

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

Measured on Windows 11, Chrome, R 4.5.1 in webR, DuckDB 1.5.2.

| Measure | Pass | 49 MB | 180 MB | 441 MB |
|---|---|---|---|---|
| Mount time | — | 5 s | 12–20 s | 42 s |
| **wasm linear memory added by mount alone** | **≈ 0** | not measured³ | **0 MB** | **0 MB** |
| wasm added by open + count(\*) | — | not measured³ | 53 MB (115→168) | 51 MB (115→166) |
| Steady tab memory after mount | — | not isolated¹ | 500 MB | 500 MB |
| **Transient tab peak during mount** | < 3 GB | not observed | not observed | **1.3 GB** |
| Taxon resolve | < 1 s | 0.035 s | 0.083 s | 0.075 s |
| Family query, ~5,000 rows | < 5 s | 0.116 s | 0.177 s | 0.170 s |
| 20 consecutive queries | no crash, memory stable | median 0.085 s, R heap delta 0 | median 0.138 s, R heap delta 0 | median 0.120 s, R heap delta 0 |

¹ The 49 MB tab reading spans page load *and* mount together, so it does not
isolate the image.

² *(retired — the wasm rows measure this directly.)*

³ The wasm probe was added after the 49 MB run. The 180 and 441 MB results
decide the row; re-run fixture 01 only if the mid-point is wanted.

## Verdict

**WORKERFS is lazy. Target 4B is viable.** Mounting cost **0 MB** of wasm linear
memory at both 180 MB and 441 MB, so the 4 GB wasm ceiling — the thing that would
have killed 4B outright — is not the binding constraint.

Three findings, in order of how much they matter:

**1. Nothing scales with file size except the download.** Opening the database
and counting rows cost 51–53 MB of wasm at both sizes. Queries cost the same at
8.5 M rows as at 4.2 M: ~0.17 s for 5,000 rows, median 0.12 s over 20
consecutive runs, zero R heap growth. Steady tab memory settled at 500 MB for
both the 180 MB and the 441 MB image. DuckDB reads what a query touches, which
is what the taxonomic sort order and the `taxon` lookup were designed to make
small.

**2. The real constraint is the transient peak during mount, not steady state.**
The 441 MB image spiked the tab to **1.3 GB** for a few seconds before settling
back to 500 MB — roughly 3× the file, consistent with the image existing as both
a download buffer and a Blob at once. This is the number that would break a
low-memory machine, and it is invisible at 180 MB. **It is also the number that
governs how large a real snapshot can be**: a 1.5 GB snapshot implies a ~4 GB
transient, which is not safe on an 8 GB laptop.

**3. Mount time is a download, and it is paid on every visit.** 42 s for 441 MB
over localhost, with no network in the way. Over the internet it is worse, and
IDBFS persistence (untested — see below) is what turns it into a one-time cost
rather than a per-visit one.

### What this does not yet establish

- **IDBFS persistence.** Probe built (`4. Persistence (IDBFS)` in the app), not
  yet run. See below — the expected answer is that IDBFS is the *wrong* cache
  for the snapshot, and the measurement is there to confirm or refute that.
- **Real BOLD data.** These fixtures are synthetic and calibrated for *cardinality
  and skew*, not real free text. The Phase 1 estimate implies ~75 bytes/row
  against the ~45–54 measured here, so a real snapshot of the same row count will
  be larger — and file size is what drives both the transient peak and the
  download. **Re-measure with `--tsv` before committing to a snapshot size.**
- **Low-memory machines.** Everything here was measured on one Windows 11 laptop
  with headroom for a 1.3 GB spike. An 8 GB machine is the case to check.

### Caching the snapshot: IDBFS is probably the wrong layer

`docs/static-datapackage-plan.md` §4B assumes "a service worker plus IDBFS caches
the app and the database". For the *app* that is right. For the *database* it
looks wrong, and for the same reason the spike succeeded.

Emscripten's IDBFS is not a storage backend in its own right: it syncs between
**MEMFS and IndexedDB**, so its contents live in wasm linear memory, and
Emscripten has a known out-of-memory failure loading large files into it.
Caching the snapshot there would therefore trade a re-download for the 4 GB
ceiling — spending exactly the resource WORKERFS was found to preserve.

**How to run the probe:**

1. Point `app.R` at `bold_spike_01` (49 MB) and re-export. The probe refuses
   above 200 MB: copying 441 MB into MEMFS is the documented way to kill the tab,
   and 49 MB is enough to show whether the cost scales 1:1.
2. Mount the fixture as usual, then press **Probe IDBFS**. It mounts IDBFS,
   populates from IndexedDB, copies the database in, persists it, and reports the
   wasm delta at each step.
3. **Reload the page, mount, and press Probe IDBFS again.** The second run
   reports whether the file survived and what restoring it costs.

**What the numbers mean:**

- *Copy delta ≈ 49 MB* → IDBFS holds the snapshot in linear memory. Confirmed
  wrong layer; the cache has to be HTTP-level instead.
- *Copy delta ≈ 0* → IDBFS is doing something smarter than the docs suggest, and
  the plan's assumption survives.
- *Second visit finds the file* → persistence works at all, whatever it costs.

**The likely correct answer, if the probe confirms the concern:** cache the
`.data` image at the **HTTP layer** — service worker Cache API, or plain
cache headers — so `webr::mount()` fetches it from disk cache instead of the
network. That keeps WORKERFS lazy *and* removes the re-download, rather than
trading one for the other. Untested; it is the obvious next step if IDBFS fails.

### Recommended snapshot budget

Keep the shipped snapshot **at or below ~500 MB** until the transient peak is
measured on a low-memory machine. That is comfortable on the evidence here
(441 MB worked with a 1.3 GB spike) and leaves the 4 GB wasm ceiling irrelevant,
which is the position the spike was run to reach.

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
