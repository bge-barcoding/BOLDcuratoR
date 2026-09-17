# BOLDcuratoR: static data package backend + multi-user hosting

Status: planning, not yet implemented. Supersedes `docs/offline-implementation-plan.md`,
whose backend-abstraction shape is kept but which contains five production-breaking
errors corrected below.

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

Intended outcome: the snapshot is the default data source; the live API stays
available as an opt-in fallback for users with their own key who need the newest
records or their own private datasets. New snapshots import with one command.

There is already a design doc at `docs/offline-implementation-plan.md` (merged,
unimplemented). Keep its backend-abstraction shape; this plan corrects five things
in it that would fail in production (see **Corrections** below).

### Decisions taken with the user

| Decision | Choice |
|---|---|
| Data source default | Snapshot default, live API as opt-in fallback |
| Sequences (`nuc`) | Included, in a separate table joined on demand |
| Build location | Build once on a builder machine, publish to versioned Zenodo record |
| Refresh | Automated check + pull, plus a manual-upload path |
| Hosting | Spec the NHM Shiny server for both software cases; also cost commercial options |

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
5. **§4 mode-aware `HAS_IMAGE` is unnecessary.** `caos.boldsystems.org/api/images`
   is unauthenticated and independent of BOLDconnectR. Keep calling it offline and
   add a cache table. No scoring-logic change, and offline/API ranks stay comparable.

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

### 3.5 Images — keep them, cache them

`caos.boldsystems.org/api/images` needs no key. Keep `check_specimen_images()` in
snapshot mode and add `image_cache(processid TEXT PRIMARY KEY, has_image INTEGER,
checked_at TEXT)` beside `sessions.sqlite`. With 20 students on overlapping taxa the
hit rate is high within the first hour, and the current cost (10,000 records ≈ 20
requests × 0.5 s sleep, blocking) collapses on repeats. No scoring change; offline
and API ranks stay comparable. Only if the image service is unreachable does
`has_image` become `NA`, and then ranks are marked provisional rather than silently
recomputed.

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

## Phase 4 — Hosting

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

1. **Shiny server software and disk at NHM** (Phase 4.2) — blocks the hosting half.
2. **Download automation vs manual** (Phase 0.1).
3. **Are course datasets public?** (Phase 0.2) — may require the overlay design.
4. **Dataset/project code UI.** `prepare_search_params`, validation, and the fetch
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

---

## Appendix: per-user distribution instead of a server

Considered as an alternative to Phase 4: users install the app locally and it pulls
the snapshot from Zenodo on first run. This removes the concurrency problem
entirely, removes the NHM IT dependency, costs nothing to host, and works offline.

**Everything in Phases 1–3 is unchanged and shared.** Only the pointer resolution
differs: instead of `/srv/bold-snapshots/current.json` maintained by a cron job, the
app resolves a per-user cache directory and downloads the snapshot itself if absent
or superseded.

### The download is the binding constraint, not the packaging

8 GB per user from Zenodo. Twenty students on the same campus network at 09:00 is
the realistic failure mode. Mitigations, all enabled by the `specimen`/`sequence`
table split in Phase 1.1:

- Ship the **metadata-only build (~2 GB)** as the desktop default; fetch sequences
  from the BOLD API for selected specimens only, at export time.
- Accept a local file path / pre-seeded cache so snapshots can be handed out on USB
  or from a network share, bypassing the download entirely.
- Verify by checksum and resume partial downloads; never leave a half-written file
  where the app will try to open it.

### Option A — R package with one-line install (rejected)

**Rejected:** the focal users are non-technical. Requiring them to install R and then a
package is too much. Retained here only to record why.

`pak::pak("bge-barcoding/BOLDcuratoR")` then `BOLDcuratoR::run_app()`.

- Cross-platform at no cost: CRAN supplies macOS and Windows binaries for every
  dependency including `duckdb`. No Electron, no code signing, no notarization.
- Snapshot cached in `tools::R_user_dir("BOLDcuratoR", "data")`.
- The repo already has `DESCRIPTION` and `renv.lock`; converting `app.R` +
  `R/modules/*` into a package with an exported `run_app()` is the bulk of the work.
- Requires R to be installed — acceptable for a barcoding course, not for a general
  public release.
- Roughly a week on top of Phases 1–3, and it would have de-risked the NHM hosting
  unknown — but that does not outweigh the install burden on the target users.

### Option B — Python rewrite with native installers

Shiny for Python, packaged with Briefcase or Tauri + PyInstaller, built for both
platforms on GitHub Actions runners. `duckdb` wheels exist for macOS (arm64 + x86)
and Windows.

- Signing: Apple Developer Program ($99/yr, notarization is mandatory for
  distribution outside the App Store) and Microsoft Artifact Signing (~$10/mo).
  Note EV certificates no longer bypass SmartScreen — reputation accrues over time,
  so early users still see warnings.
- Real cost is the rewrite: ~9,000 lines of R across R6 classes, DT tables, BAGS
  grading, BIN concordance and 17 scoring criteria. That is re-validating scientific
  logic, not porting UI. Months.
- Only justified if desktop distribution is a long-term goal rather than a fix for
  one course.

### Option C — shinylive, no install at all

Shiny compiled to WebAssembly, hosted as static files (GitHub Pages), querying
hive-partitioned Parquet on R2 or Zenodo via HTTP range requests entirely
client-side. Concurrency stops being a concept — 20 users is 20 browsers.

**Python route.** DuckDB's Python client is compiled to WASM and available in
Pyodide's package repository, so the data layer is known to work. Costs the full
Option B rewrite.

**R route (no rewrite) — plausible, unverified, several independent blockers.**
Note that DuckDB-Wasm is the *JavaScript* build; shinylive-for-R runs under webR and
needs the **R `duckdb` package as a webR binary**, which is a different artifact.
Evidence suggests it exists (R-universe builds wasm binaries for all CRAN packages,
and `duckdb/duckdb-r` issue #66 is from someone running duckdb under webR far enough
to hit an extension-loading problem), but this was not confirmed.

Verify in this order, cheapest first — each can kill the route on its own:

1. **`duckdb` under webR.** Open <https://webr.r-wasm.org/latest/> and run
   `webr::install("duckdb"); library(duckdb)`. Minutes.
2. **CORS.** The BOLD API and `caos.boldsystems.org/api/images` probably do not send
   `Access-Control-Allow-Origin`. In a browser that kills both the live-API fallback
   *and* the image check — and since `HAS_IMAGE` is a ranking criterion, ranks would
   shift relative to the server version. Test with `fetch()` from any browser
   console. Minutes.
3. **`httpfs` extension under webR** — needed to read remote Parquet, and the subject
   of the open issue above.
4. **`BOLDconnectR` has no wasm binary** (GitHub-only), so it must be built with
   `rwasm` and rebuilt on every upstream change.
5. **wasm32 caps a tab at ~4 GB** of address space, shared between the R heap, the
   DuckDB buffer pool and result frames. The Phase 3.4 memory analysis is
   per-server-process; here it is per-tab against a hard ceiling.
6. **`RSQLite` session persistence** must be replaced with browser storage
   (IndexedDB/OPFS) or dropped.

If 1 and 2 both pass, spend **one day on a spike** — smallest possible slice (taxon
search → results table, over remote Parquet) — before committing anything larger.
Do not port the app to find out.

### R Shiny as a packaged desktop binary — not recommended

`electricShine` builds Windows only, and documents why macOS is hard: R
installations hard-code paths, so the bundle is not relocatable. The cross-platform
R + Electron templates are experimental and single-maintainer. Choosing this means
maintaining packaging infrastructure rather than curation features.

### Recommendation

**"No install for the user" is already satisfied by a hosted web app** — that is the
current deployment and what Phases 1–4 preserve. Users get a URL and install
nothing. Shinylive removes *the server*, not the user's install step; it is not a
substitute for hosting, it is a substitute for running a box.

So:

- Keep the hosted model. Option A is rejected on install burden (above).
- If the goal is also "no server to operate", the low-risk form is already in the
  plan — **Phase 4.5, managed host + Parquet on Cloudflare R2**. Near-zero ops, all
  existing R code keeps working, no WASM risk.
- Treat Option C (R route) as a time-boxed experiment gated on the two cheap tests,
  not as a plan. Option B stays deferred until after the course.
