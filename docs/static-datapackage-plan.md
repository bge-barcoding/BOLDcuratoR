# BOLDcuratoR: static data package backend + multi-user hosting

## Context

BOLDcuratoR is a Shiny app (`app.R` + `R/modules/*`, R6 classes, renv) that curates
BOLD specimen records. Today every search hits the BOLD API live through
BOLDconnectR, and the app is deployed to shinyapps.io.

Two problems drive this work:

1. **Concurrency.** ~20 students on a course all searching at once will hammer the
   BOLD API through one shared key, hit rate limits, and queue behind each other.
2. **Speed.** BIN expansion — the slowest step — currently issues one HTTP round
   trip per 50 BINs. For 1,000 BINs that's 20 sequential fetches, each `rbind`ed
   onto a growing frame. Minutes to tens of minutes per search.

BOLD publishes a static public data package (~3 GB `.gz` → ~30 GB TSV, BCDM schema,
CC-BY-SA 4.0, released weekly). Building that into a queryable local snapshot turns
BIN expansion into a single sub-second query, removes the API key requirement for
students, and makes results reproducible against a named snapshot.

Intended outcome: the snapshot is the *only* data source. BOLDconnectR, the BOLD
API and the image checks are dropped entirely (see **Decisions** below), which
removes API keys, rate limits and the shared-key ceiling from the app. New
snapshots import with one command.

There is already a design doc at `docs/offline-implementation-plan.md` (merged,
unimplemented). Keep its backend-abstraction shape; this plan corrects five things
in it that would fail in production (see **Corrections** below).

### Decisions taken with the user

Revised after IT confirmed Docker/Kubernetes, and after narrowing scope.

| Decision | Choice |
|---|---|
| Data source | **Snapshot only.** Drop BOLDconnectR, the BOLD API and the image checks. |
| Sequences (`nuc`) | **Excluded** from the shipped snapshot (separate table, built but not shipped) |
| Build location | Build once on a builder machine, publish to versioned Zenodo record, monthly |
| Refresh | Automated check + pull, plus a manual-upload path |
| Targets | **Two, explored in parallel:** (1) Docker image on NHM Kubernetes, (2) local/offline shinylive — with a Python rewrite (4C) as the fallback for (2) |
| Local-app meaning | Hosted URL, cached to run offline after first visit. True air-gapped use is out of scope. |

**Consequences of dropping the API and image checks** — these simplify the earlier
design considerably and supersede parts of Phase 3:

- **`HAS_IMAGE` is removed**, not stubbed. Delete it from `SPECIMEN_SCORING_CRITERIA`
  (`R/config/constants.R:72-74`) and drop the image requirement from `RANK_2` so ranks
  stay internally consistent. Maximum quality score falls by 1. Scores are therefore
  **not comparable with the current app's output** — say so in `about.md` and in the
  export header, and update any documentation quoting score ranges.
- `R/utils/image_utils.R` and the image-cache design (former Phase 3.5) are dropped.
- No API key, so `mod_user_info_*` loses the key field and the shared-key limits in
  `R/config/download_limits.R` become purely memory guards.
- Private/unpublished records are simply out of scope. Confirm this is acceptable
  for the course before building (former Phase 0.2).
- No `backend_api.R` — the backend abstraction collapses to one implementation, so
  build it as a plain module rather than an R6 interface with one subclass.

---

## RESOLVED — the 4B spike answered it: 4B is viable

**The question was:** can shinylive + webR + duckdb query a few-hundred-MB
snapshot fast enough to be usable, inside the 4 GB wasm ceiling?

**The answer is yes, and the ceiling turned out not to be the constraint.**
Measured on Windows 11 / Chrome / webR R 4.5.1 / DuckDB 1.5.2, full numbers and
method in `spike/shinylive-duckdb/README.md`:

| | 180 MB image | 441 MB image |
|---|---|---|
| wasm linear memory added by the mount | **0 MB** | **0 MB** |
| wasm added by opening the DB + count(\*) | 53 MB | 51 MB |
| 5,000-row taxon query | 0.177 s | 0.170 s |
| 20 consecutive queries | median 0.138 s, no heap growth | median 0.120 s, no heap growth |
| steady tab memory after mount | 500 MB | 500 MB |
| transient tab peak during mount | not observed | **1.3 GB** |
| mount time (localhost) | 12–20 s | 42 s |

WORKERFS streams rather than copying the image into linear memory, so the
snapshot does not consume the wasm budget. Nothing except the download scales
with file size: DuckDB pays for the pages a query touches, which is what the
taxonomic sort order and the `taxon` lookup exist to keep small.

**The constraint moved rather than disappeared.** Two things now govern how big a
shipped snapshot can be, and neither is the 4 GB ceiling:

1. **The transient peak while mounting** — ~3× the image size (1.3 GB for a
   441 MB file), for a few seconds, before settling back. A 1.5 GB snapshot
   implies a ~4 GB spike, which is not safe on an 8 GB laptop. This is now the
   binding number and it has only been measured on one well-provisioned machine.
2. **The download, repeated per visit** — 42 s for 441 MB over *localhost*.
   IDBFS persistence is what makes this a one-time cost, and it is still
   **untested**. Without it the offline story in §4B is considerably weaker than
   written.

**Recommended snapshot budget: ≤ ~500 MB**, until the transient peak is measured
on a low-memory machine. Note this cuts against §8's ~2 GB figure for 4A/4C — see
the open question there about whether one snapshot serves all targets.

**Caveat on sizing.** The fixtures are synthetic, calibrated for cardinality and
skew rather than real free text, and came in at 45–54 bytes/row against the ~75
bytes/row Phase 1 assumes. A real snapshot of the same row count will be *larger*,
and file size drives both constraints above. Re-measure with `--tsv` on the real
public package before committing to a size.

### Consequences for the decision

- **4B proceeds.** A zero-install local option is viable at a fraction of 4C's
  cost, and the risk that justified 4C as a fallback has been retired by
  measurement rather than assumption.
- **4C is no longer the presumed destination for the local option.** The
  consolidation argument in §4C stands on its own merits and is unaffected by
  this result; what has gone is the "4B might not work at all" premise beneath it.
- **4A is unaffected** and proceeds in R as planned.

### Follow-ups this opens

1. Test IDBFS persistence across reloads. This is the difference between
   "download once" and "download every visit" and is the largest remaining
   unknown in 4B.
2. Re-run the spike with `--tsv` against the real package to get true
   bytes/row and a real snapshot size.
3. Measure the mount transient on an 8 GB machine, which sets the real budget.

### How it was measured

The spike lives in `spike/shinylive-duckdb/` — throwaway code, kept separate from
`R/`, not wired into the app. `build_fixture.R` builds synthetic DuckDB fixtures
at three sizes (49 / 180 / 441 MB) using the Phase 1 schema, `package_fixture.sh`
(or `.ps1` on Windows) wraps each as an Emscripten WORKERFS image, and `export.R`
produces the static site. The app reports wasm linear memory, read from inside the
webR worker via `webr::eval_js()`, at three points around the mount — Chrome's
Task Manager cannot distinguish wasm from Blob memory, and `performance.memory`
only sees the main thread, which never touches the file.

The four unknowns it was built to probe, and how they came out:

1. **Does `dbConnect(..., read_only = TRUE)` work against a WORKERFS mount?**
   Yes, at all three sizes.
2. **Is WORKERFS genuinely lazy for DuckDB's access pattern?** Yes — 0 MB of wasm
   linear memory added by the mount at both 180 MB and 441 MB. This was the
   measurement the whole spike existed for.
3. **Does IDBFS persist the fixture across reloads?** **Still untested** — the
   largest remaining unknown in 4B.
4. **Does memory grow across repeated queries?** No. 20 consecutive queries at
   both sizes, zero R heap growth.

Every pass/fail threshold set in advance was met, except that the peak-memory
threshold needs restating: it was written as "< 3 GB, headroom under the 4 GB
cap", on the assumption that the image would land in wasm. It does not, so the
number to watch is the ~3× transient tab peak during mount, on the machine with
the least RAM — not headroom under a ceiling that turned out to be irrelevant.


---

## Architecture

```
BOLD data package (.tsv.gz, login-gated)
        │  manual download OR automated, see Phase 0
        ▼
[builder machine]  data-prep/build_snapshot.R      ~30-60 min, ~60 GB scratch
        │          data-prep/verify_snapshot.R     fails loudly, never publishes bad data
        ▼
Zenodo versioned record (concept DOI → always latest)
        │  bold_snapshot_<id>.duckdb        ~8 GB  (with sequences)
        │  bold_snapshot_<id>.meta.duckdb   ~2 GB  (metadata only, optional)
        ▼
[app server]  data-prep/pull_snapshot.R  (cron/systemd timer)
        │     downloads if new → verifies → atomic publish → updates pointer
        ▼
/srv/bold-snapshots/current.json ──▶ bold_snapshot_2026-09-01.duckdb  (mode 0444)
        │                             bold_snapshot_2026-08-01.duckdb  (rollback)
        ▼
Shiny workers, each opening the file READ-ONLY (DuckDB permits N read-only processes)
```

The pointer file may name **either** a local `.duckdb` path **or** an `s3://`
Parquet prefix. Same DuckDB SQL runs against both. This is what lets the same
codebase serve the NHM server (local file, fastest) and a managed host with no
persistent disk (remote Parquet) without a fork — see **Phase 4**.

---

## Corrections to `docs/offline-implementation-plan.md`

Apply these when updating that doc; they are not stylistic.

1. **§5.1 `read_csv_auto(..., sample_size=-1)` on a `.gz` will exhaust RAM.** A gzip
   stream is unseekable, so DuckDB cannot rewind to re-sniff types and buffers the
   whole file. Pass an explicit `columns = {...}` map with `auto_detect = false`.
2. **§5.4 "build indexes" is harmful here.** DuckDB ART indexes must fit in RAM at
   build time and are not buffer-managed. With 20 read-only processes sharing one
   file they are the most likely OOM cause. Use sort order + zone maps instead —
   they are alternatives, not complements.
3. **§5.6 `VACUUM` does not reclaim space in DuckDB.** Staging-then-dropping inside
   one file keeps the staging bytes forever. Write the sorted result directly into a
   freshly `ATTACH`ed target file.
4. **§6 symlink swap does not work.** DuckDB resolves the symlink at `open()` and
   holds the fd. Replacing the symlink leaves live workers reading the old inode
   indefinitely — no crash, just silently stale data. Use a versioned filename plus
   a pointer file (Phase 2.3).
5. **§4 mode-aware `HAS_IMAGE` is moot.** Superseded by the scope decision above:
   image checks are dropped entirely and the criterion is removed from scoring, so
   there is no mode to be aware of. (For the record, the doc's premise was also
   wrong — `caos.boldsystems.org/api/images` is unauthenticated, so images *could*
   have been kept offline had we wanted them.)

---

## Phase 0 — Verify before writing code

Both of these can invalidate the design. Neither takes long.

1. **Download mechanics.** The package is login-gated
   (`bench.boldsystems.org/index.php/datapackage?id=BOLD_Public.<date>`). Establish
   whether an API key header works or whether it needs a form-login cookie, and
   whether the per-snapshot URL is stable or a one-shot signed link.
   If it needs an interactive login, that step stays manual — one person, one
   download, drop the `.gz` into the builder's source dir. Everything downstream
   still automates. Do not contort the design to avoid this.
2. **Are the course datasets public in BOLD at course time?** The public package
   contains public records only. If students curate their own *unpublished* data,
   offline mode cannot serve those records at all. If so, plan an **overlay**: a
   small second DuckDB file in the same schema holding the course's own records,
   `ATTACH`ed read-only alongside the snapshot, queried as
   `specimen UNION ALL overlay.specimen`. Built once per course from one
   authenticated `bold.fetch`. This keeps one source per result set.
3. **Licence sign-off.** BOLD packages are CC-BY-SA 4.0, so mirroring a derived
   DuckDB file on Zenodo with attribution is permitted. Get it noted in writing
   before the course, and put the attribution in the Zenodo record and in the app's
   `about.md`.

---

## Phase 1 — Snapshot build (`data-prep/`)

New files: `data-prep/build_snapshot.R`, `data-prep/verify_snapshot.R`,
`data-prep/README.md`.

### 1.1 Schema

Keep ~57 of the 79 BCDM columns. Derive the required set programmatically from
`R/config/constants.R` (`SPECIMEN_SCORING_CRITERIA`, `PREFERRED_COLUMNS`) and
`R/modules/export/mod_export.R` (`get_specimen_columns`) rather than hardcoding —
that keeps one source of truth and makes verification assertion 2 meaningful.

Required (app breaks without): the scoring-criteria fields (`species`,
`taxonomy_notes`, `short_note`, `collection_notes`, `voucher_type`, `notes`,
`nuc_basecount`, `bin_uri`, `identified_by`, `identification_method`, `collectors`,
`collection_date_start`, `collection_date_end`, `country/ocean`, `site`, `sector`,
`region`, `coord`, `inst`, `museumid`), all ten taxonomic rank columns
(`kingdom`…`subspecies`), `processid`, `identification`, `identification_rank`,
`nuc`, `marker_code`, `bold_recordset_code_arr`.

**Drop `identifier_email`** — shipping 20 M personal email addresses to a teaching
server is a data-protection problem, not a size one. Flag this to whoever signs off
the deployment.

**Do not name a column `country.ocean`.** DuckDB parses a dotted identifier as
`table.column`. Use physical `country_ocean` / `province_state` and alias at
projection time (`country_ocean AS "country.ocean"`) so the rest of the app is
untouched. This forces an explicit projection list instead of `SELECT *`, which is
wanted anyway. `order` is a SQL keyword and a required column — quote it everywhere.

Tables:

| Table | Sorted by | Purpose |
|---|---|---|
| `specimen` | `kingdom, phylum, class, "order", family, subfamily, genus, species, processid` | main table, no `nuc` |
| `sequence` | `processid` | `nuc` only, joined on demand |
| `specimen_recordset` | `recordset_code, sid` | exploded `bold_recordset_code_arr` |
| `taxon` | `taxon_lc` | name → rank lookup, with `n_records` |
| `bin_species` | `bin_uri` | BIN ↔ species counts across all records |
| `_meta` | — | `snapshot_id`, `source_sha256`, `row_count`, `schema_version`, … |

The taxonomic sort is the key design choice: taxonomy is a strict tree, so one
nested `ORDER BY` makes *every* rank's equality predicate contiguous at once. Zone
maps then prune `WHERE "order" = 'Lepidoptera'` and `WHERE family = 'Nymphalidae'`
and `WHERE species = '…'` from the same physical layout. That is why no indexes are
needed.

Splitting `nuc` out matters for three reasons: it lets you ship a ~2 GB
sequence-free snapshot; it makes lazy sequence fetch enforceable (today `nuc` rides
through every `rbind` and every `serialize()` into SQLite); and it keeps sequence
blocks from evicting the hot metadata columns from the OS page cache.

Expected sizes — **treat as estimates to be measured on the first real build**, and
record the measured values in `data-prep/README.md`: `specimen` ~1.5 GB,
`sequence` ~6 GB, side tables ~0.5 GB. Total ~8 GB with sequences, ~2 GB without.

### 1.2 Ingest

```r
con <- DBI::dbConnect(duckdb::duckdb(dbdir = staging_path, read_only = FALSE))
DBI::dbExecute(con, "SET memory_limit = '8GB'")       # 6GB on a 16GB box
DBI::dbExecute(con, "SET threads = 4")                # 2 on a 16GB box
DBI::dbExecute(con, "SET preserve_insertion_order = false")
DBI::dbExecute(con, "SET temp_directory = '/mnt/scratch/duckdb_tmp'")   # local disk
DBI::dbExecute(con, "SET max_temp_directory_size = '80GB'")
```

`read_csv` with `delim='\t'`, `header=true`, `auto_detect=false`,
`compression='gzip'`, `columns={...all 79 as VARCHAR...}`, `quote=''`, `escape=''`,
`nullstr=['','None','NA']`, `ignore_errors=false`.

Ingest everything as `VARCHAR`, then `TRY_CAST` the four numerics
(`specimenid`, `elev`, `depth`, `nuc_basecount`). BCDM dates are partial
(`2015`, `2015-07`) and the app only tests them for emptiness, so leave them text.

`quote=''` matters: with the default `"`, one unbalanced double-quote in a `notes`
field swallows the rest of the file into a single value. Verify against real data.

If the builder has the disk, `gunzip` first — a gzip stream can't be read in
parallel, so the plain TSV ingests 3–4× faster. Do not pipe through `/dev/stdin`.

### 1.3 Write the sorted output

`ATTACH` the final file and `CREATE TABLE out.specimen AS SELECT … ORDER BY …`
directly into it, then delete the staging file wholesale. This is what avoids the
`VACUUM` that wouldn't have worked.

Explode `bold_recordset_code_arr` (values look like `['AANIC','DS-ANIC2A']`) with
`regexp_replace` + `string_split` + `unnest`, and **assert the parse worked** —
every non-empty value must match `^\[.*\]$`, and `count(DISTINCT recordset_code)`
must land in the 10⁵ range. A result of 1 or 10⁷ means the split is wrong. Also
check the real codes against the `^DS-[A-Z0-9]+$` pattern that
`mod_data_import_utils.R:50` enforces — if BOLD uses lowercase, that validator
rejects legitimate input before the query runs.

Finish with `DETACH out; CHECKPOINT; dbDisconnect(con, shutdown = TRUE)`.
**A DuckDB file with a leftover `.wal` beside it cannot be opened read-only** —
DuckDB can't replay a WAL without write access, so every Shiny worker would refuse
it. Verification must open the file read-only in a *fresh process* to catch this.

### 1.4 Expected cost

~30–60 min wall clock, ~11 GB peak RSS, ~35 GB peak temp, **~60 GB free disk
required**, on 16 GB / 8 vCPU / NVMe. On 32 GB with the TSV pre-decompressed,
12–20 min. On network storage, multiply by 3–5.

---

## Phase 2 — Distribution and refresh

### 2.1 Publish (`data-prep/publish_zenodo.R`)

Use the `zen4R` CRAN package. Zenodo allows 50 GB and 100 files per record, and
versioning gives a **concept DOI that always resolves to the latest version** —
which is exactly the "pull latest" primitive needed. Upload
`bold_snapshot_<id>.duckdb`, optionally the metadata-only variant, and a small
`manifest.json` (`snapshot_id`, `sha256`, `row_count`, `schema_version`, source
package id). Three files, well inside the limits.

### 2.2 Pull (`data-prep/pull_snapshot.R`)

Runs on each app server from cron or a systemd timer. Queries the concept DOI for
the latest version, compares its `snapshot_id` against the running
`_meta.snapshot_id`, and **no-ops if unchanged** — so the job is cadence-agnostic
and costs nothing when BOLD hasn't published. If new: download to a staging dir on
the same filesystem, verify checksum, run `verify_snapshot.R`, then publish.

Also support `--file <path>` for the manual-upload path: same verify-then-publish
sequence against a locally supplied file, no Zenodo involved.

### 2.3 Atomic publish and swap

```
/srv/bold-snapshots/
  current.json                      {"db_path": "...", "snapshot_id": "...", "rows": N}
  bold_snapshot_2026-09-01.duckdb   live, mode 0444, owned by a non-shiny user
  bold_snapshot_2026-08-01.duckdb   previous, kept for rollback
  staging/                          same filesystem, so rename() is atomic
```

Publish order: build in `staging/` → verify → `file.rename()` into the serve dir
(atomic, so no worker ever sees a partial file) → write `current.json.tmp` →
`file.rename()` over `current.json` (atomic, readers get whole-old or whole-new).
Keep the previous snapshot; delete the one before it at the *start* of the next
build, guarded by `fuser -s <file> ||` so a long-lived worker is never rug-pulled.

Rollback is repointing `current.json` — seconds, no restart.

**Two hard rules, or 20 students get locked out:** nothing may ever open the serving
file read-write (not the builder, not a stray `duckdb` CLI session — the CLI
defaults to read-write and takes an exclusive cross-process lock), and the file must
have no `.wal` beside it.

---

## Phase 3 — App changes

Follow the backend abstraction in `docs/offline-implementation-plan.md` §2. Default
mode stays `"api"` until the snapshot backend passes its tests, then flips to
`"snapshot"`.

### 3.1 New files

- `R/utils/snapshot_store.R` — `SnapshotStore` R6: pointer resolution, driver
  lifecycle, `ensure_current()`, `_meta` reader.
- `R/utils/specimen_merge.R` — lift `merge_specimens` / dedupe helpers out of
  `mod_data_import_server.R` (no behaviour change).
- `R/modules/data_import/backends/backend_base.R` / `backend_api.R` /
  `backend_snapshot.R`.

### 3.2 Connection lifecycle

One driver **instance per R process**, one **connection per Shiny session**.
Build the driver once in `global.R`; connections are near-free and give per-session
cancellation.

```r
# global.R — once per worker process
snapshot <- SnapshotStore$new(pointer = Sys.getenv("BOLDCURATOR_SNAPSHOT_POINTER",
                                                  "/srv/bold-snapshots/current.json"))
# server function — per session
con <- snapshot$connect()
session$onSessionEnded(function() DBI::dbDisconnect(con))   # NOT shutdown = TRUE
```

`dbDisconnect(con, shutdown = TRUE)` tears the shared instance down under every
other session in that worker. Only `SnapshotStore` may shut the driver down.

Per-process config (these are global-per-instance in DuckDB; they cannot be set per
connection): `read_only = TRUE`, `memory_limit = "768MB"`, `threads = 2`,
`preserve_insertion_order = "false"`. Not `threads = 1` — that disables the parallel
scan and roughly triples query latency.

Implementation note to verify: the R `duckdb` package caches driver instances by
`dbdir`, and `config` on a second `duckdb(dbdir = p)` call for the same path may be
ignored. Construct the driver exactly once and assert the settings took with
`SELECT current_setting('memory_limit')`.

`ensure_current()` stats the pointer file (~microseconds) at the entry to each
backend method and reopens if `db_path` changed. It must **never** fire mid-query or
between paginated fetches of one result set. When reopening, retire the old driver
and shut it down once its last connection closes rather than pulling it immediately.

### 3.3 The four access points

**Taxonomy search** — replace `bold.public.search` with a two-step resolve-then-query.
The plan doc's `WHERE species IN (…) OR genus IN (…) OR family IN (…)` is wrong: it
covers 3 of 10 ranks and a cross-column disjunction defeats zone-map pruning.

Step 1 hits the small `taxon` table (`WHERE taxon_lc IN (?)`, <50 ms) and returns
the **exact rank column** to filter on, the canonical casing, `n_records`, and
ambiguity (a name that is both a genus and a subfamily returns two rows — prompt
rather than silently OR-ing). This also answers open question #5 in the plan doc,
gives a typeahead source for the taxa textarea, and lets the existing size-check
modal fire *before* anything is fetched.

Step 2 is a single-column equality per resolved rank, with rank names chosen from a
fixed whitelist of the ten BCDM columns — never interpolate user text into an
identifier position; values go through `DBI::dbBind`.

Drop the title-casing hack at `mod_data_import_server.R:695` — the `taxon_lc` lookup
is case-insensitive by construction.

Push the continent/country filter into the seed CTE as
`country_ocean IN (?)`, which reproduces `filter_specimens_by_continent`'s semantics
exactly (SQL `IN` is false for NULL, matching its NA-dropping). This also retires
the HTTP-422 workaround documented at `mod_data_import_server.R:674-676`.

**Dataset / project codes** — both resolve through `specimen_recordset`, which is
sorted by `recordset_code`, so it's a pruned range read of a two-column table. Keep
the per-code loop (or diff returned codes against input) so you can report *which*
codes produced nothing — offline there is no 401, and a private code returns zero
rows looking exactly like a typo.

**Fetch by processid** — `duckdb_register()` the id vector as a temp relation and
`SEMI JOIN`, rather than building a 100k-element `IN` list.

**BIN expansion — the big win.** The 50-BIN HTTP loop collapses to one statement:

```sql
WITH seed AS (
  SELECT sid, bin_uri FROM specimen
  WHERE <resolved rank predicate> AND country_ocean IN (?)
),
seed_bins AS (
  SELECT DISTINCT bin_uri FROM seed WHERE bin_uri IS NOT NULL AND bin_uri <> ''
)
SELECT <projection> FROM specimen s
WHERE s.sid IN (SELECT sid FROM seed)           -- keeps seed records with no BIN
   OR s.bin_uri IN (SELECT bin_uri FROM seed_bins);
```

Two hash semi-joins over one scan — sub-second even with 50,000 seed BINs, against
minutes today. The `sid IN seed` branch preserves the current behaviour where
BIN-less seed records survive expansion.

### 3.4 Memory — the real 20-user risk

This is independent of DuckDB and is what will actually bring a box down. A 50,000-row
frame with sequences is ~125 MB; the pipeline holds several live copies
(`merge_specimens` reallocates per batch, `process_specimen_data` ~3 copies,
`SpecimenProcessor` ~4 more), and `save_session_state` `serialize()`s the *entire*
frame to a raw vector **every 60 seconds per session** (`app.R:660-675`). Realistic
peak is 0.8–1.2 GB per session; twenty of those is 16–24 GB.

Do these three, in order; they are small and they make snapshot mode *safer* under
load than API mode:

1. **Pre-emptive size check.** `taxon.n_records` and a `count(*)` pre-flight make the
   size known before materialising anything. Move the existing modal
   (`mod_data_import_server.R:279-322`) in front of the fetch, and have snapshot mode
   enforce a hard cap via `LIMIT` — the R-side cost is now the binding constraint,
   not the API.
2. **Lazy, streamed sequences.** Never project `nuc` in a search. In the FASTA/TSV
   export handlers, `dbSendQuery` + `dbFetch(res, n = 5000)` in a loop, appending to
   the connection — constant memory regardless of result size. This also fixes the
   current `download_fasta` handler, which loops over a fully-materialised frame.
3. **Vectorise `SpecimenScorer$score_specimens`** (`specimen_scorer.R:38-52`). It
   loops `for (i in 1:nrow())` extracting `specimens[i,]` — 10,000 rows ≈ 15 s,
   50,000 ≈ 75 s, blocking the whole R process and therefore every session
   multiplexed onto it. The 17 criteria are per-row regex and emptiness tests;
   vectorised `grepl` over whole columns is a contained rewrite.

   *Deliberately not precomputing scores into the snapshot.* It would couple an
   hours-long rebuild to every `constants.R` edit behind a hash that will drift.
   Revisit only if the vectorised scorer proves insufficient.

Also worth doing while in there: `auto_select_best_specimens` (`app.R:424-466`) loops
over unique `(bin_uri, country)` combos re-filtering the whole frame each time —
O(n × combos). A single grouped `dplyr::slice_max` is a ~5-line replacement.

### 3.5 Images — SUPERSEDED, images are dropped

> Retained only to record what was considered. The scope decision removes image
> checks and the `HAS_IMAGE` criterion outright; none of the below applies.

<details><summary>Original text</summary>


`caos.boldsystems.org/api/images` needs no key. Keep `check_specimen_images()` in
snapshot mode and add `image_cache(processid TEXT PRIMARY KEY, has_image INTEGER,
checked_at TEXT)` beside `sessions.sqlite`. With 20 students on overlapping taxa the
hit rate is high within the first hour, and the current cost (10,000 records ≈ 20
requests × 0.5 s sleep, blocking) collapses on repeats. No scoring change; offline
and API ranks stay comparable. Only if the image service is unreachable does
`has_image` become `NA`, and then ranks are marked provisional rather than silently
recomputed.

</details>

### 3.6 UI and provenance

- Data-source toggle in the user-info bar per plan doc §3.1, with the snapshot date
  and record count beside it.
- Stamp `snapshot_id` into every export (file header, Excel summary sheet). A curated
  spreadsheet with no provenance is a data-integrity problem six months later.
- Store `snapshot_id` on the `sessions` row. On resume across a swap, warn that BIN
  membership and identifications may have changed.
- Consider persisting the *query* plus processid list plus annotations instead of the
  whole frame (`session_persistence.R`). Session blobs drop from ~125 MB to a few KB
  and resume gets faster — only safe because a named snapshot makes the result
  reproducible.
- **Dataset/project code inputs have full server support but no UI input** — see
  Open questions.

---

## Phase 4 — Two delivery targets

Both consume the same snapshot from Phase 1 and the same query layer from Phase 3.
They differ only in how the file reaches the query engine, and in how tight the size
budget is.

| | Target A: Docker/K8s | Target B: local shinylive |
|---|---|---|
| Size budget | ~2 GB comfortable | **a few hundred MB** (hard) |
| Runs | NHM Kubernetes | user's browser, wasm32 |
| Install for user | none (URL) | none (URL), offline after first load |
| Main risk | none technical | R `duckdb` under webR; DuckDB I/O through Emscripten FS |

### 4A — Docker image on NHM Kubernetes

IT confirmed Docker + Kubernetes, which removes the concurrency problem entirely
(replicas, not one R process). They raised two objections, both addressed by baking
the snapshot into the image rather than mounting a volume:

- *"Wouldn't be tracked the same way."* The image **is** their tracked, versioned,
  immutable artifact. `boldcurator:2026-09-01` is the snapshot version; rollback is
  deploying the previous tag. No PVC, no volume lifecycle, no backup question, and
  no shared-file concurrency concern since each replica has its own copy.
- *"Getting into the realms of a website."* No new infrastructure primitive is
  introduced — it stays an image and a Deployment.

**The GitLab file-size limit is a symptom; the real constraint is that the snapshot
must never enter git.** Splitting the TSV per Phylum/Class/Order does not fix this —
30 × 70 MB is still 2 GB in the repo, and git retains every monthly version forever,
so the repo reaches ~24 GB within a year and becomes unclonable. Worse than the
single-file problem.

Instead the repo holds a URL and a checksum, and the build fetches:

```dockerfile
FROM rocker/shiny:4.4.1
RUN install2.r --error duckdb DBI dplyr ...        # expensive layer, stays cached
COPY R/ /srv/shiny-server/R/
COPY app.R global.R /srv/shiny-server/
ADD --checksum=sha256:<sha> \
    https://zenodo.org/records/<id>/files/bold_snapshot_<date>.duckdb \
    /opt/bold/snapshot.duckdb                       # data layer LAST
```

`ADD --checksum` needs BuildKit; otherwise `RUN curl -fsSL … && sha256sum -c`.
Refresh = edit two lines, commit, CI rebuilds and tags. Fully tracked in GitLab with
nothing large in git.

If IT prefers the blob to stay inside NHM, **GitLab Package Registry (generic
packages)** is artifact storage rather than git and is designed for exactly this —
the Dockerfile pulls from there instead of Zenodo. Same shape, same checksum.

Keep the data `COPY`/`ADD` last so the R-package layer stays cached across refreshes.
Budget roughly 2 GB of image, pulled once per node then cached.

**Taxonomic splitting is still worth doing — for scoping, not file size.** Shipping
only the clades the course needs is the largest size lever after dropping sequences.
Note Phylum is a poor split for BOLD (Arthropoda dominates), so Class or Order gives
a more even distribution. Once the build fetches at image-build time, a single
DuckDB file is simpler and faster than many Parquet files.

**Fallback if the image gets too large:** S3-compatible object storage (MinIO/Ceph
internally, or R2/S3) holding hive-partitioned Parquet, queried via DuckDB `httpfs`
with the prefix in a ConfigMap. Pods stay stateless, refresh is an upload plus a
ConfigMap edit with the old prefix kept for rollback, at the cost of ~1–4 s per query
instead of ~100 ms. Install the `httpfs` extension at image build time, never at
runtime, or every pod phones `extensions.duckdb.org` on startup.

### 4B — Local / offline shinylive

Dropping BOLDconnectR, the API and image checks removes three of the blockers
identified earlier (CORS, building a `BOLDconnectR` wasm binary, and rank divergence
from missing images). Two hard constraints remain.

**1. A shinylive export cannot be opened from `file://`** — it requires an HTTP
server. "Unzip and double-click index.html" does not work. The workable shape is:
host the export on GitHub Pages, the user visits a URL, and a service worker plus
IDBFS caches the app and the database on first visit so it runs offline afterwards.
That is install-free and offline *after* first run, but not a file handed out on a
USB stick. **Confirmed acceptable** — true air-gapped use is out of scope. Two
consequences to handle: first visit needs internet (and downloads the whole
snapshot), and clearing site data forces a re-download, so surface both in the UI.

**2. wasm32 caps a browser tab at 4 GB of linear memory**, shared between R's heap,
DuckDB's buffers and result frames. The practical snapshot budget is therefore a few
hundred megabytes, not 2 GB — an order of magnitude tighter than 4A. Trimming fields
and scoping records is not an optimisation here; it determines viability.

Mitigation to test rather than assume: webR can mount a filesystem image with
**WORKERFS**, which avoids copying contents into memory until actually read, and
**IDBFS** persists it across page loads so it downloads once. Whether DuckDB's
random-access read pattern performs acceptably through Emscripten's filesystem is
the experiment that decides this option.

Remaining unknowns to clear, cheapest first:

1. **R `duckdb` as a webR binary.** <https://webr.r-wasm.org/latest/> →
   `webr::install("duckdb"); library(duckdb)`. Minutes. Hard blocker if absent.
2. **Other packages as wasm binaries**: `shinydashboard`, `DT`, `shinyjs`,
   `shinycssloaders`, `writexl` (C code), `R6`, `dplyr`, `tidyr`, `purrr`, `logger`,
   `jsonlite`, `digest`, `markdown`.
3. **`RSQLite` session persistence** must be replaced with browser storage or dropped.
4. **DuckDB read performance through WORKERFS** — the spike below.

**Sequencing: do not port the app to find out.** (a) webR duckdb test; (b) build one
scoped snapshot and measure its actual size against the few-hundred-MB budget;
(c) one-day spike — taxon search plus results table only, in shinylive, against that
file mounted via WORKERFS. Decide after (c).

### 4C — Python rewrite as a downloadable desktop app (the fallback, and possibly the destination)

Considered as an alternative to 4B. **It removes every technical unknown in 4B:** no
4 GB wasm ceiling (so the snapshot can be 2 GB+ and the aggressive trimming stops
being load-bearing), no question about `duckdb` under webR (Python wheels are
first-party and mature on macOS arm64/x86 and Windows), no Emscripten filesystem I/O
question, no `RSQLite`-in-browser problem. 4B is low-effort/high-risk; 4C is
high-effort/low-risk.

**The stronger argument is consolidation, not risk.** As planned, 4A and 4B have
different fates: 4A works today in R, 4B might not work at all. A Python rewrite
serves *both* targets from one codebase — the same app in the container and on a
laptop. The alternative, if 4B fails, is maintaining R for Docker plus something else
for local, with the scoring and BAGS logic implemented twice. That is the outcome to
avoid.

**Shape.** Shiny for Python, packaged with Briefcase or PyInstaller (optionally a
Tauri shell), built for both platforms on GitHub Actions runners. Launches a local
server and opens the browser; no Electron, no WebView2 dependency.

**Cost — less than the raw 8,800 R LOC suggests**, because dropping the API and image
checks deletes rather than ports a large part of it:
`mod_data_import_server.R` (963 lines) is mostly retry logic, batching, size modals
and merge loops, all of which collapse into a few SQL queries against a local DuckDB;
`image_utils.R` goes entirely. Estimate the core (search → score → rank → BAGS → BIN
→ select → export) at **2,500–3,500 lines of Python, 3–6 weeks** focused.
The fiddliest part is `R/utils/table_utils.R` (1,282 lines of DT with custom JS);
Shiny for Python has modules and editable `DataGrid`/`DataTable` with sorting,
filtering and selection, but it is not a drop-in and the custom JS needs rethinking.

**De-risking the scientific logic** — the concern that makes rewrites dangerous:
run the R and Python implementations over the same fixture and diff `quality_score`,
`criteria_met`, `rank`, BAGS grade and BIN concordance row by row. Build that harness
first, not last; it turns translation correctness into a test rather than a judgement.

**Downsides, stated plainly:**

- *Install friction is worse than a URL.* Unsigned apps hit Gatekeeper on macOS and
  SmartScreen on Windows. A genuine double-click needs Apple Developer ($99/yr) plus
  Microsoft Artifact Signing (~$10/mo) and CI on both platforms. 4B has zero friction;
  4C has one scary dialog unless you pay. This cuts against the "easy to install" goal.
- *Delivery risk.* The R app works today. A rewrite that stalls at 80% leaves nothing.

### Decision gate — RUN, and passed

`duckdb` is available in webR, the spike was run, and 4B performed: 0 MB of wasm
linear memory for the mount at both 180 MB and 441 MB, sub-200 ms queries, stable
memory across repeated runs. See the resolved section at the top of this document
for the numbers and for the two constraints that replaced the 4 GB ceiling (the
~3× transient peak during mount, and per-visit download pending IDBFS).

The local option is **4B**. 4C's case now rests on the consolidation argument
alone, not on 4B being unworkable. 4A proceeds in R in parallel and was never
blocked by this.

---

## Phase 4 (superseded) — hosting options assessed before IT confirmed Kubernetes

### 4.1 shinyapps.io cannot host this

1 GB bundle (Free/Starter) / 5 GB (Basic+), ephemeral per-instance disk, no shared
volume. An 8 GB — or even 2 GB — snapshot is not deployable there. Moving off it is a
real decision, not a detail.

### 4.2 NHM Shiny server — questions for IT

Send this list before building anything host-specific:

1. Which software and version — **Shiny Server open source**, **Posit Connect**, or
   **Docker/Kubernetes**? This is the single most important answer.
2. Is there a persistent volume of ≥100 GB readable by the `shiny` user, that
   survives redeploys? What path?
3. Shell access, or deploy-only? Can we run a cron job / systemd timer?
4. RAM and vCPU available to this app. Target ≥32 GB / 8 vCPU (see 4.4).
5. Outbound HTTPS allowed to `zenodo.org` and `caos.boldsystems.org`?
6. Who administers TLS, auth, and the hostname?

### 4.3 Plan for both software cases

**If Posit Connect** — easiest path, no workaround needed. It runs multiple R
processes per app; set max processes and max connections per process
(start: 6 processes × 4 connections). Deploy with `rsconnect`, point
`BOLDCURATOR_SNAPSHOT_POINTER` at the volume.

**If Shiny Server open source** — it launches **one R process per app**, and R is
single-threaded, so 20 students' queries serialise. Two workable shapes:

- *Multi-instance behind nginx (no new tooling):* run N copies of the app as
  systemd services on ports 3001–3006 (`shiny::runApp(port = …)`), with nginx
  upstream + `proxy_set_header Upgrade`/`Connection` for websockets and sticky
  routing. Caveat: nginx open source only offers `ip_hash`/`hash` for stickiness,
  and if students reach the server through one institutional NAT they all share an
  IP and `ip_hash` does nothing. Test this before the course; if it bites, use a
  cookie-based route (`hash $cookie_…`) or move to the container option.
- *Containerise:* a `Dockerfile` (rocker/shiny base + renv restore) plus the
  snapshot bind-mounted read-only. Works with ShinyProxy, plain Docker Compose with
  N replicas, or Kubernetes — and is the same artifact used for commercial hosting,
  so it is not wasted work.

**If Docker/Kubernetes** — the container path above, with the snapshot on a
ReadOnlyMany volume. Set `container-memory` limits; an over-limit container is
killed, not throttled.

### 4.4 Sizing

Per worker: DuckDB `threads=2`, `memory_limit=768MB`; R baseline ~250 MB; peak
result frames ~500 MB (see 3.4). Box total ≈ hot snapshot working set (~2 GB, served
from OS page cache) + workers × ~0.9 GB. **32 GB / 8 vCPU** is the safe worst case
for 20 genuinely concurrent workers; in practice Shiny multiplexes several sessions
per process, so 20 students is often 3–6 processes.

The ~2 GB sequence-free working set fully caches in RAM after the first few queries,
which is the strongest practical argument for splitting `nuc` out.

### 4.5 Commercial alternatives (if NHM can't provide disk or shell)

| Option | Shape | Rough cost | Verdict |
|---|---|---|---|
| **VPS + Docker Compose** (Hetzner / DigitalOcean / Lightsail) | 8 vCPU / 32 GB / NVMe, N Shiny containers behind Caddy or nginx, snapshot on local disk | ~£50–200/mo — **confirm current pricing, quotes moved in 2026** | **Best performance/£.** You own OS patching and TLS. |
| **ShinyProxy on that VPS** | One container per student, full isolation | same VM cost, needs more RAM headroom | Use if per-user isolation matters; memory grows linearly with users, so cap `container-memory`. |
| **Managed host + Parquet on Cloudflare R2** | Keep shinyapps.io Basic or Posit Connect Cloud; snapshot exported as hive-partitioned Parquet on R2, queried via DuckDB `httpfs` | ~£10–30/mo host + ~£0.15/mo storage (**R2 has no egress fees**) | **Best "pay for reliability, zero ops" answer.** ~1–4 s per query instead of 50–200 ms. Genuinely viable for a course. |
| **Posit Connect Cloud** | Managed, multi-process; but 24 GiB runtime disk is **not persisted**, so an 8 GB snapshot re-downloads on every cold start | per-seat | Only viable paired with the R2 Parquet option above. |
| **MotherDuck** | Hosted DuckDB | Lite tier is 10 GB / smallest compute only; read scaling needs the ~$250/mo Business tier | Overkill for 20 students. |

**Recommendation:** primary = NHM server with a local snapshot, if IT can provide
disk and shell. Fallback requiring no IT involvement = managed host + Parquet on R2.
Because the pointer file abstracts local-vs-remote, both are the same codebase —
build the Parquet export path (`data-prep/export_parquet.R`, partitioned by `class`
or `phylum` to stay under Zenodo's 100-file cap if also mirrored) even if you expect
to use the local file, as the escape hatch.

---

## What the snapshot cannot do

State these plainly in `README.md` and `about.md`:

- **Private / unpublished / early-release records are absent.** This is the one that
  may decide the course design — see Phase 0.2.
- **Dataset/project codes covering private records return zero rows, silently.**
  Report which requested codes matched nothing.
- **Freshness** is bounded by BOLD's release cadence plus the pull interval.
- Records are occasionally retracted; a processid in a saved session may be missing
  from a newer snapshot. Don't let rehydration drop rows silently.

### Where the snapshot is strictly better

- **BAGS grade E becomes correct.** `check_shared_bins`
  (`R/utils/bags_grading.R:85-115`) can currently only see BINs among the records the
  user happened to download, so "this BIN is shared with another species" is
  systematically under-detected. The `bin_species` table evaluates it against all
  public records. This is a scientific improvement, not just a speedup — arguably the
  strongest argument for the whole project.
- BIN expansion: one query instead of N/50 sequential fetches.
- No API key, no shared-key rate limit, no 1,800-character URL ceiling
  (`mod_data_import_server.R:117`), no HTTP 422s from country filters.
- Reproducible: a named snapshot id means a result set regenerates exactly.

---

## Verification

**Build** (`data-prep/verify_snapshot.R`, run against the staging file from a *fresh
R process*, read-only; fail the job rather than publish):

1. File opens read-only in a clean process — catches a leftover `.wal`.
2. Row count within −2% / +40% of the previous snapshot.
3. All required columns present, derived from `constants.R` not hardcoded.
4. No null/empty/duplicate `processid`.
5. `specimen_recordset ANTI JOIN specimen` is empty; `count(DISTINCT recordset_code)` > 1000.
6. `resolve("lepidoptera")` → `order`; `resolve("nymphalidae")` → `family`;
   `resolve("danaus plexippus")` → `species`.
7. Null fractions sane: `bin_uri` < 40%, `species` < 40%, `country_ocean` < 30%,
   `nuc_basecount` < 10%.
8. `sequence ANTI JOIN specimen` empty; `seq_rows <= n_rows`.
9. **End-to-end smoke:** run all four access-point queries plus BIN expansion, assert
   non-empty and non-error, record latency; then feed one result through
   `process_specimen_data()` + `validate_specimen_data()` and assert `valid == TRUE`.
   This is the assertion that actually catches schema drift.

**App:** extend `tests/testthat/` with a tiny fixture snapshot (a few thousand rows,
committed or generated in `setup.R`) so backend tests run in CI without the 8 GB
file. Assert the API and snapshot backends return the same column set and the same
records for a taxon present in both.

**Concurrency:** before the course, run a load test — 20 parallel headless sessions
(`shinytest2` or a simple `curl`/websocket script) each running a representative
taxon search, measuring wall clock and peak RSS. This is the test that decides the
box size and whether the nginx stickiness workaround holds.

**Refresh:** run `pull_snapshot.R` against a second snapshot while sessions are live;
confirm existing sessions are unaffected, the next search uses the new file, and
rollback by repointing `current.json` works.

---

## Open questions

1. **Does `duckdb` exist as a webR binary?** The decision gate — run this first; it
   determines whether the local option is 4B or 4C.
2. **Taxonomic scope of the snapshot.** The single biggest lever on size, and it
   differs per target: ~2 GB is fine for 4A and 4C, but 4B needs a few hundred MB.
   Decide whether one scoped snapshot serves all, or 4A/4C ship broader coverage.
3. **Download automation vs manual** (Phase 0.1).
4. **Are the records the course needs public?** (Phase 0.2) — with the API dropped,
   private records are simply unavailable; there is no fallback path any more.
5. **Dataset/project code UI.** `prepare_search_params`, validation, and the fetch
   phases all support dataset/project codes, but `mod_data_import_ui.R` has no input
   for them — the code paths are unreachable, `README.md` still advertises them, and
   `tests/testthat/test-mod_data_import.R:83-84` asserts they render (so that test
   should be failing). Restore the inputs, or remove the dead paths and the README
   claim. Decide before wiring the snapshot backend, since it changes what
   `specimen_recordset` is for.
5. **Marker filter.** The public package includes non-COI markers, but BIN, BAGS and
   the 500 bp `SEQ_QUALITY` threshold all assume COI-5P. Filter to COI-5P at build
   time, or keep everything and filter in the query? Affects snapshot size materially.
6. Snapshot retention beyond current + previous — any need to cite older ones?

## Unrelated bug found in passing

`R/modules/export/mod_export.R:229` whitelists `"institution"`, but the BCDM/app
column is `inst`. The `intersect()` at lines 34/107 silently drops it, so the
institution column is missing from every Excel and TSV export today. One-word fix,
worth taking with this work.
