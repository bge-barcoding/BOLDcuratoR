# Offline (DuckDB) Implementation Plan

Status: planning, not yet implemented.
Branch: `claude/offline-tsv-implementation-4NUEY`.

## 1. Goal

Add an offline data backend (DuckDB over a pre-built BOLD snapshot) to the
existing app **without forking it**. The same Shiny app should support:

1. **Live BOLD API mode** — current behaviour, BOLDconnectR + image API.
2. **Offline snapshot mode** — DuckDB query against a pre-built file built
   from a TSV dump (~20 M records).
3. **Auto mode** (default) — start with the API, prompt to switch to the
   snapshot when the query is too large for a live fetch.

One codebase, one deploy, two backends.

## 2. Backend abstraction

Introduce an R6 class `DataBackend` with two implementations. Everything
that currently lives in `mod_data_import_server.R` Phases 1–4 calls the
backend instead of `BOLDconnectR::*` directly.

```
R/modules/data_import/backends/
  backend_base.R        # DataBackend R6 (interface) + factory
  backend_api.R         # ApiBackend (current BOLDconnectR logic, lifted as-is)
  backend_offline.R     # OfflineBackend (DuckDB queries)
```

Required methods (each returns a `data.frame` matching the BOLD column shape):

| Method | API impl | Offline impl |
|---|---|---|
| `fetch_by_dataset_codes(codes)` | `bold.fetch(get_by="dataset_codes")` | `JOIN specimen_recordsets WHERE recordset_code IN (...)` |
| `fetch_by_project_codes(codes)` | `bold.fetch(get_by="project_codes")` | same join, different prefix |
| `search_taxonomy(taxa)` | `bold.public.search` + `bold.fetch` | `WHERE species IN (...) OR genus IN (...) OR family IN (...)` |
| `fetch_by_bin_uris(bins)` | `bold.fetch(get_by="bin_uris")` | `WHERE bin_uri IN (...)` |
| `count_taxonomy(taxa)` | `bold.public.search` then `nrow` | `SELECT count(*) FROM ... WHERE ...` (cheap) |
| `snapshot_date()` | `NULL` (live) | `SELECT created_at FROM _meta` |
| `is_available()` | always TRUE | `file.exists(db_path)` and openable |

Backends share the merge/dedupe helpers (currently inside the data-import
module — lift them to `R/utils/specimen_merge.R`).

## 3. Mode selection

### 3.1 User-visible toggle (in the user-info bar, top of page)

```
Data source: ( ) Live BOLD API   ( ) Offline snapshot (2026-04-12)   (•) Auto
```

- If the offline file is missing, hide "Offline" and "Auto", default to "Live".
- If no API key is set, hide "Live" and "Auto", default to "Offline".
- Persisted in the saved-session state so resumed sessions remember the choice.
- Snapshot date displayed inline so users know how stale the offline data is.

### 3.2 Auto mode logic

In auto mode the data-import handler:

1. Estimates result size cheaply (taxonomy: `bold.public.search` returns
   processid lists without sequences; datasets/projects: count from the
   API headers if available, otherwise fall through to step 3).
2. If the estimate exceeds `AUTO_FALLBACK_THRESHOLDS` (proposed: 10,000
   records OR 1,000 unique BINs), show the existing size-check modal with
   a third button:
   - **Cancel**
   - **Continue with API** (current behaviour, slow)
   - **Switch to offline snapshot** (re-runs the entire search via DuckDB)
3. Below the threshold: proceed with the API as today.

Re-running the entire search via the offline backend (rather than mixing
backends mid-pipeline) avoids data-consistency surprises — every record in
the result set comes from the same source.

### 3.3 Backend resolution at query time

```
selected_mode <- input$data_source_mode  # "api" | "offline" | "auto"

backend <- switch(selected_mode,
  api      = ApiBackend$new(api_key),
  offline  = OfflineBackend$new(db_path),
  auto     = ApiBackend$new(api_key)  # may switch mid-flow per 3.2
)
```

`auto` always *starts* with the API; the switch happens only if the size
check trips. The chosen backend is recorded on the resulting specimen
data frame (`attr(specimens, "data_source")`) so downstream modules can
see what produced it.

## 4. HAS_IMAGE handling (mode-dependent)

The image API only exists for live mode. The TSV has no image column.
Three options were considered; the plan adopts **option C** below.

- (A) Drop `HAS_IMAGE` everywhere → loses Rank 2 distinction for API users.
- (B) Keep it everywhere → offline records can never reach Rank 2.
- (C) **Mode-aware**: keep `HAS_IMAGE` in scoring criteria, but skip the
  image API call in offline mode and treat `HAS_IMAGE` as "not required"
  inside `RANK_2` when `has_image` is unavailable on the data frame.

Implementation:
- `specimen_processor.R`: check `attr(specimens, "data_source")`. If
  `"offline"`, skip `check_specimen_images()` entirely and leave
  `has_image` absent.
- `specimen_processor.R` ranker: when evaluating `RANK_2`, ignore
  `HAS_IMAGE` requirement if the criterion is missing from the data set
  (i.e. column not present). API records evaluate it normally.
- `image_utils.R` stays in the codebase, used only by the API backend.

## 5. Data prep (one-time, separate from app)

`data-prep/build_offline_db.R` (new):

1. Read TSV with `duckdb::dbExecute("CREATE TABLE specimens AS SELECT * FROM read_csv_auto('<tsv>', delim='\t', header=TRUE, sample_size=-1)")`.
2. Rename columns:
   - `country/ocean` → `country.ocean`
   - `province/state` → `province.state`
   (keeps the rest of the codebase untouched)
3. Build a `specimen_recordsets(processid, recordset_code)` side table by
   exploding `bold_recordset_code_arr`. Sample value:
   `['AANIC', 'DS-ANIC2A', 'DS-ARHODIA', ...]`. SQL:
   ```sql
   CREATE TABLE specimen_recordsets AS
   SELECT processid,
          trim(unnest(string_split(
            regexp_replace(bold_recordset_code_arr, '[\[\]'']', '', 'g'),
            ', ')))
          AS recordset_code
   FROM specimens
   WHERE bold_recordset_code_arr IS NOT NULL
     AND bold_recordset_code_arr != '';
   ```
4. Indexes (DuckDB uses ART by default; declare for query planner hints):
   - `specimens.processid` (PK)
   - `specimens.bin_uri`
   - `specimens.species`, `genus`, `family`
   - `specimens."country.ocean"`
   - `specimen_recordsets.recordset_code`
   - `specimen_recordsets.processid`
5. Write a `_meta` table:
   ```sql
   CREATE TABLE _meta (
     key TEXT PRIMARY KEY,
     value TEXT
   );
   INSERT INTO _meta VALUES
     ('snapshot_date',  '<source TSV date>'),
     ('source_file',    '<TSV filename>'),
     ('row_count',      '<N>'),
     ('schema_version', '1');
   ```
6. Compact + optimise: `CHECKPOINT;` then `VACUUM;`.

`data-prep/README.md` documents:
- prerequisites (DuckDB R package, ~50 GB free disk, ~30 GB RAM headroom)
- expected runtime
- how to refresh (re-run script with new TSV path → atomic symlink swap on the server)

## 6. Deployment paths (Shiny Server)

| File | Path | Permissions | Set via |
|---|---|---|---|
| Source TSV | `/srv/bold-data/source/<date>.tsv` | not needed at runtime | manual |
| **DuckDB** (live, app reads this) | `/srv/bold-data/bold_offline.duckdb` (symlink) | `shiny:shiny`, mode 0644 | env `BOLDCURATOR_DB_PATH` |
| Sessions SQLite | `data/sessions.sqlite` (in app dir) | writable by `shiny` | unchanged |

App resolves the DuckDB path with this priority:

1. Env var `BOLDCURATOR_DB_PATH`
2. `config/offline_db.json` → `{"db_path": "..."}`
3. Default `data/bold_offline.duckdb` (dev convenience)

If the path doesn't resolve to a readable DuckDB file, the offline option
disappears from the toggle and the app continues in API-only mode (no
hard failure).

**Open**: confirm with hosting that `/srv/bold-data` (or equivalent) is
writeable by the data-prep user and readable by the `shiny` user, and
that the path persists across deploys.

## 7. UI changes

### 7.1 User-info bar (`mod_user_info_*`)

- API key field stays — required for API/auto modes.
- Add data-source toggle (radio group) per §3.1.
- Add small label: `Snapshot: 2026-04-12 (12.3 M records)` when offline
  mode is selectable, sourced from `_meta`.

### 7.2 Data input (`mod_data_import_ui.R`)

- No structural changes — same taxa / dataset / project / country
  inputs.
- Surface the active backend in the progress messages
  ("Searching offline snapshot…" vs "Searching BOLD…") and in result
  notifications.

### 7.3 Size-check modal (`mod_data_import_server.R`)

- Add the third button per §3.2 only when `selected_mode == "auto"` and
  the offline backend is available.
- Shared-key blocked modal: replace the hard-stop with a "Use offline
  snapshot" CTA when offline is available.

## 8. File-level change list

### New
- `docs/offline-implementation-plan.md` (this file)
- `data-prep/build_offline_db.R`
- `data-prep/README.md`
- `R/utils/offline_db.R` (connection helper, path resolver, `_meta` reader)
- `R/utils/specimen_merge.R` (lifted from `mod_data_import_server.R`)
- `R/modules/data_import/backends/backend_base.R`
- `R/modules/data_import/backends/backend_api.R`
- `R/modules/data_import/backends/backend_offline.R`
- `config/offline_db.json` (optional, gitignored if it contains paths)

### Modified
- `global.R` — add `library(duckdb)`; keep `library(BOLDconnectR)`.
- `DESCRIPTION` — add `duckdb` to Imports.
- `app.R` — source the new files; mount the backend factory; relax the
  API-key gating to "API key required only when API backend selected".
- `R/config/constants.R` — add `AUTO_FALLBACK_THRESHOLDS`; update
  `SPECIMEN_RANK_CRITERIA` evaluator (or the ranker) per §4.
- `R/config/column_definitions.R` — add display names for `sovereign_inst`,
  `identifier_email`, `primers_forward`, `primers_reverse`.
- `R/modules/data_import/mod_data_import_server.R` — replace direct
  BOLDconnectR calls with `backend$...()` calls; update size-check modal
  per §7.3.
- `R/modules/data_import/mod_data_import_ui.R` — minor copy changes to
  surface backend in progress text.
- `R/modules/user/mod_user_info_ui.R` + `_server.R` — add data-source
  toggle + snapshot date label.
- `R/modules/specimen_handling/specimen_processor.R` — mode-aware image
  check per §4.
- `R/modules/specimen_handling/specimen_scorer.R` — leave `HAS_IMAGE`
  scoring path; only the *required-for-rank* logic changes.
- `R/utils/session_persistence.R` — persist `data_source_mode` and
  `data_source` attribute alongside other state keys.
- `README.md` — document the two backends and where the snapshot lives.

### Removed
- Nothing. Image check, BOLDconnectR, and the API-key path all stay
  for live mode.

## 9. Open questions to resolve before / during implementation

1. **Snapshot path on the in-house Shiny Server** — see §6. Need confirmed
   absolute path and ownership.
2. **Auto-fallback thresholds** — proposed 10,000 records / 1,000 BINs.
   Confirm or override.
3. **Default mode** — proposing `"auto"`. Alternatives: `"api"` (current
   behaviour preserved) or `"offline"` (snapshot-first).
4. **Snapshot refresh cadence** — quarterly? On demand? Affects how the
   data-prep README is written and whether we need an automated job.
5. **Taxonomy search shape** — current `search_taxonomy` against DuckDB
   matches `species`, `genus`, OR `family`. Confirm this matches BOLD's
   semantics closely enough; otherwise we may need to inspect what
   `bold.public.search` does for ambiguous inputs (e.g. a name that's
   both a genus and a synonym).
6. **HAS_IMAGE rank policy** — confirm option C in §4 vs simpler
   alternatives. Affects offline-mode rank distribution.
7. **`bold_recordset_code_arr` separator robustness** — sample uses
   `, ` between quoted entries; the prep regex assumes that. If real
   data has variants (no space, different quote styles), prep will need
   to be hardened. Easy to verify with `SELECT DISTINCT bold_recordset_code_arr LIKE ...`.

## 10. Suggested commit sequence (on `claude/offline-tsv-implementation-4NUEY`)

1. `docs/offline-implementation-plan.md` — this file.
2. `data-prep/build_offline_db.R` + `data-prep/README.md`.
3. `R/utils/offline_db.R` + `R/utils/specimen_merge.R` (lift helpers,
   no behaviour change).
4. Backend abstraction: `backend_base.R` + `backend_api.R` (wrap
   existing logic; behaviour unchanged when default mode is `"api"`).
5. `backend_offline.R` (DuckDB queries).
6. Wire the factory + toggle into `app.R`, `mod_user_info_*`,
   `mod_data_import_server.R`. Default mode: `"api"` so existing flow
   is preserved.
7. Size-check modal: add the "Switch to offline" button (auto mode).
8. Mode-aware HAS_IMAGE in `specimen_processor.R` / ranker.
9. Persist `data_source_mode` in session state.
10. Polish: progress messages, snapshot-date display, `README.md` update.

Each commit should keep the app runnable. Tests (where they exist) and
manual smoke tests after every commit.

## 11. Out of scope

- Replacing the SQLite session store with DuckDB (different concern).
- Migrating away from BOLDconnectR for live mode (not requested; keep it).
- Pre-computed BAGS/BIN analyses in the snapshot (compute on the fly per
  query, same as today).
- Multi-snapshot support (single active snapshot at a time).
