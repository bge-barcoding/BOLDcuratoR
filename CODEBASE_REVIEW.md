# BOLDcuratoR Codebase Review

## Table of Contents

- [1. Architecture Overview](#1-architecture-overview)
- [2. Module Map & Data Flow](#2-module-map--data-flow)
- [3. Source Loading & Double-Sourcing Problem](#3-source-loading--double-sourcing-problem)
- [4. File-by-File Inventory](#4-file-by-file-inventory)
- [5. Orphan Functions & Dead Code](#5-orphan-functions--dead-code)
- [6. Duplicate / Conflicting Definitions](#6-duplicate--conflicting-definitions)
- [7. Missing Connections (Broken Wiring)](#7-missing-connections-broken-wiring)
- [8. Unused Infrastructure](#8-unused-infrastructure)
- [9. State Management Complexity](#9-state-management-complexity)
- [10. Proposed Solutions](#10-proposed-solutions)

---

## 1. Architecture Overview

BOLDcuratoR is a **Shiny web application** (not an R package) that helps curate barcode-of-life specimen data from the BOLD database. It uses `shinydashboard` for the UI, R6 classes for state management and processing, and DataTables (DT) for interactive specimen tables.

### Core Components

| Component | File | Role |
|-----------|------|------|
| Entry point | `app.R` | UI definition, server definition, R6 class wrappers, module initialization |
| Bootstrap | `global.R` | Package installation, directory creation, config loading, file sourcing |
| State | `R/modules/state/state_manager.R` | `StateManager` R6 class - centralized reactive state |
| Logging | `R/modules/logging/mod_logging.R` | `LoggingManager` R6 class - action/export tracking |
| Export | `R/modules/export/mod_export.R` | `ExportManager` R6 class - file export handling |
| Processing | `R/modules/specimen_handling/specimen_processor.R` | `SpecimenProcessor` R6 pipeline |
| Scoring | `R/modules/specimen_handling/specimen_scorer.R` | `SpecimenScorer` R6 - quality scoring |
| Validation | `R/modules/specimen_handling/specimen_validator.R` | `SpecimenValidator` R6 - data validation flags |
| Haplotypes | `R/modules/haplotype_analysis/haplotype_manager.R` | `HaplotypeManager` R6 - haplotype clustering |
| Alignment | `R/modules/haplotype_analysis/sequence_aligner.R` | `SequenceAligner` R6 - MSA operations |

### Shiny Module Pattern

Each UI tab follows the standard `mod_*_ui()` / `mod_*_server()` pattern:

| Tab | UI | Server | Utils |
|-----|----|---------| ------|
| Data Import | `mod_data_import_ui.R` | `mod_data_import_server.R` | `mod_data_import_utils.R` |
| BIN Analysis | `mod_bin_analysis_ui.R` | `mod_bin_analysis_server.R` | `mod_bin_analysis_utils.R` |
| Specimens | `mod_specimen_handling_ui.R` | `mod_specimen_handling_server.R` | _(inline)_ |
| BAGS A-E | `mod_bags_grading_ui.R` | `mod_bags_grading_server.R` | `mod_bags_grading_utils.R` |
| Haplotypes | `mod_haplotype_analysis_ui.R` | `mod_haplotype_analysis_server.R` | `mod_haplotype_analysis_utils.R` |
| Export History | `mod_export_history_ui.R` | `mod_export_history_server.R` | _(none)_ |
| User Info | `mod_user_info_ui.R` | `mod_user_info_server.R` | _(none)_ |

---

## 2. Module Map & Data Flow

```
User Info ──> StateManager.user_info
                  │
Data Import ──> StateManager.specimen_data
                  │
                  ├──> SpecimenProcessor (validate → score → rank)
                  │         │
                  │         └──> StateManager.specimen_data (processed)
                  │
                  ├──> calculate_bags_grade()
                  │         │
                  │         └──> StateManager.bags_grades
                  │
                  ├──> BIN Analysis ──> mod_bin_analysis_server
                  │
                  ├──> Specimen Handling ──> mod_specimen_handling_server
                  │         │
                  │         ├──> format_specimen_table() ──> DT output
                  │         └──> sync_table_states()
                  │
                  ├──> BAGS A-E ──> mod_bags_grading_server (×5)
                  │         │
                  │         ├──> filter_grade_specimens()
                  │         ├──> organize_grade_specimens()
                  │         ├──> create_grade_tables()
                  │         │       └──> format_grade_table()
                  │         │               └──> format_specimen_table()
                  │         └──> sync_table_states()
                  │
                  └──> Haplotype Analysis ──> HaplotypeManager
                            └──> SequenceAligner

Export ──> ExportManager ──> LoggingManager
Export History ──> LoggingManager.get_export_history()
```

---

## 3. Source Loading & Double-Sourcing Problem

**This is the most critical structural issue.** Files are sourced **twice**: once by `global.R` (which globs everything) and once explicitly by `app.R`.

### How it happens

**`global.R` (lines 106-137):**
```r
.source_utils <- function() {
  utils_files <- list.files("R/utils", pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(utils_files, source))  # Sources ALL .R files in R/utils/
}

.source_modules <- function() {
  module_files <- list.files("R/modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  invisible(lapply(module_files, source))  # Sources ALL .R files in R/modules/
}
```

**`app.R` (lines 10-57):** Then explicitly sources 30+ specific files again.

### Consequences

1. **Every file is executed twice** - R6 class definitions, function definitions, and side effects all run twice
2. **`global.R` sources files that `app.R` does NOT list** - this includes `old_mod_bags_table_utils.R`, `mod_bags_table_utils.R`, `table_state_handler.R`, `db_handler.R`, and `table_state_utils.R`
3. **Order-dependent bugs** - `global.R` sources alphabetically within directories, `app.R` sources in a specific deliberate order. The second sourcing (from `app.R`) overwrites definitions from the first, but if a function was already called between the two source passes, results may differ.

---

## 4. File-by-File Inventory

### Files sourced by BOTH `global.R` and `app.R` (30 files)

All files listed in `app.R` lines 10-57 are also glob-sourced by `global.R`.

### Files sourced by `global.R` ONLY (not listed in `app.R`)

| File | Status | Notes |
|------|--------|-------|
| `R/utils/db_handler.R` | **ORPHAN** | Defines `init_database()`, `get_specimen_flags()`, etc. None are called from any module. |
| `R/utils/table_state_utils.R` | **ORPHAN** | Defines `init_table_state()`, `get_table_state()`, `handle_flag_change()`, etc. None are called from any module. |
| `R/modules/bags_grading/mod_bags_table_utils.R` | **ORPHAN** | Defines `create_specimen_table()` (duplicate of function in `old_mod_bags_table_utils.R`). Not called. |
| `R/modules/bags_grading/old_mod_bags_table_utils.R` | **ORPHAN/LEGACY** | Old version of bags table utils. Prefix `old_` indicates it was superseded. |
| `R/modules/state/table_state_handler.R` | **EMPTY** | 0 bytes of content. Placeholder file. |
| `R/config/column_definitions.R` | Sourced by both | Listed in `app.R` line 11. |

### Files NOT sourced at all

| File | Status | Notes |
|------|--------|-------|
| `R/modules/specimen_handling/specimen_validator.R` | **NOT IN `app.R`** | But `global.R` glob picks it up. The `SpecimenValidator` R6 class IS used in `app.R:246`. So it works only because `global.R` runs first. |
| `2024-11-22_v7_4_22.R` | **LEGACY** | Root-level backup file from Nov 2024. Not sourced anywhere. |

---

## 5. Orphan Functions & Dead Code

### Fully Orphaned Files (defined but never called from any active code)

| File | Functions | Issue |
|------|-----------|-------|
| **`R/utils/db_handler.R`** | `init_database()`, `get_specimen_flags()`, `get_specimen_notes()`, `update_specimen_flag()`, `update_specimen_note()` | **Zero callers.** The database persistence layer was built but never wired into any module. The SQLite DB at `data/specimen_tracking.db` exists but is never read or written by the running app. |
| **`R/utils/table_state_utils.R`** | `init_table_state()`, `get_table_state()`, `update_table_state()`, `handle_flag_change()`, `handle_note_change()`, `get_table_callback()` (different signature from table_utils.R version), `save_table_state()`, `load_table_state()`, `verify_state_integrity()`, `clean_state_data()` | **Zero callers.** An alternative table state system that was never integrated. Note: `get_table_callback()` exists in both this file and `table_utils.R` with **different implementations and signatures**, creating a name collision risk. |
| **`R/utils/specimen_ranking.R`** | `calculate_specimen_rank()`, `rank_species_specimens()` | **Zero callers from modules.** Ranking is done inline by `SpecimenProcessor$rank_specimens()` private method instead. |
| **`R/utils/specimen_validation.R`** | `validate_specimen()`, `validate_criteria()`, `validate_quality_metrics()` | **Zero callers from modules.** Validation is done by `SpecimenValidator` R6 class instead. |
| **`R/modules/bags_grading/mod_bags_table_utils.R`** | `create_specimen_table()`, `create_table_container()`, `add_column_definitions()`, `create_species_colors()`, `format_flag_text()`, `generate_table_caption()`, `format_specimen_fields()`, `format_metrics()`, `create_download_filename()`, `validate_table_input()` | **Zero callers.** Superseded by `mod_bags_grading_utils.R` which defines its own `generate_table_caption()`, `create_grade_tables()`, `format_grade_table()`. |
| **`R/modules/bags_grading/old_mod_bags_table_utils.R`** | Same set as `mod_bags_table_utils.R` (nearly identical) | **Legacy file.** Clearly marked with `old_` prefix. |

### Orphaned Functions Within Active Files

| File | Function | Issue |
|------|----------|-------|
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | `update_filtered_data()` (line 628) | Defined after the `moduleServer()` block. Never called - filtering is done inline in the `observe()` block at line 202. |
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | `validate_specimen_data()` (line 666) | Defined after the `moduleServer()` block. Never called. |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | `validate_bags_data()` (line 8) | Defined but never called from `mod_bags_grading_server.R`. |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | `prepare_download_data()` (line 328) | Defined but never called. No download handler exists in the BAGS grading server. |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | `find_specimen_in_groups()` (line 353) | Defined but never called. |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | `calculate_selection_metrics()` (line 371) | Defined but never called. |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | `calculate_flag_metrics()` (line 394) | Defined but never called. |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | `validate_specimen_selection()` (line 417) | Defined but never called. |
| `R/utils/table_utils.R` | `format_interactive_column()` (line 274) | Defined but call is **commented out** (lines 233-242). |
| `R/utils/table_utils.R` | `format_table_columns()` (line 548) | Defined but never called from any module. |
| `R/utils/table_utils.R` | `prepare_table_data()` (line 680) | Defined but never called. `prepare_module_data()` is used instead. |
| `R/utils/table_utils.R` | `format_specimen_fields()` (line 1498) | Defined but never called from any module. |
| `R/utils/table_utils.R` | `format_metrics()` (line 1518) | Defined but never called from any module. |
| `R/utils/table_utils.R` | `create_download_filename()` (line 1534) | Defined but never called from any module. |
| `R/utils/table_utils.R` | `validate_table_input()` (line 1550) | Defined but never called from any module. |

---

## 6. Duplicate / Conflicting Definitions

Due to the double-sourcing and multiple files defining the same function names, several **name collisions** exist:

| Function | Defined In | Last-Wins |
|----------|-----------|-----------|
| `create_specimen_table()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R` | Whichever is sourced last (alphabetical order from `global.R`) |
| `create_table_container()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `table_utils.R` | `table_utils.R` (sourced by `app.R` after `global.R`) |
| `generate_table_caption()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `mod_bags_grading_utils.R`, `table_utils.R` | `table_utils.R` (last sourced by `app.R`) |
| `format_specimen_fields()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `table_utils.R` | `table_utils.R` |
| `format_metrics()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `table_utils.R` | `table_utils.R` |
| `create_download_filename()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `table_utils.R` | `table_utils.R` |
| `validate_table_input()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `table_utils.R` | `table_utils.R` |
| `create_species_colors()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R`, `table_utils.R` | `table_utils.R` |
| `format_flag_text()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R` | Neither called; both orphans |
| `add_column_definitions()` | `mod_bags_table_utils.R`, `old_mod_bags_table_utils.R` | Neither called; both orphans |
| `get_table_callback()` | `table_state_utils.R` (args: `ns, state_data`), `table_utils.R` (args: `ns, flag_options`) | `table_utils.R` version is used. Signatures differ - would break if wrong one resolves. |
| `sync_table_states()` | `table_state_utils.R`, `table_utils.R` | `table_utils.R` version is used (different implementation). |

---

## 7. Missing Connections (Broken Wiring)

### 7.1 `SpecimenProcessor$get_metrics()` does not exist

**File:** `mod_specimen_handling_server.R:163`
```r
metrics <- processor$get_metrics()
```

The `SpecimenProcessor` R6 class has no public `get_metrics()` method. It stores metrics in `private$last_metrics` but never exposes them. This call will produce a runtime error when reached.

### 7.2 `validate_selected_specimens` referenced but not defined as a standalone function

**File:** `app.R:343`
```r
state$update_state("selected_specimens", current_selections, validate_selected_specimens)
```

`validate_selected_specimens` is expected to be a validation function passed to `update_state()`, but it is never defined anywhere in the codebase. This will produce a runtime error (object not found) when a specimen is selected from the BAGS grade tabs.

### 7.3 Database layer completely disconnected

`db_handler.R` defines a full SQLite persistence layer (`init_database`, `get_specimen_flags`, `update_specimen_flag`, etc.) but:
- No module ever calls `init_database()` to create a connection
- No module reads from or writes to the database
- The `specimen_tracking.db` file exists at 98KB but is never touched at runtime
- `LoggingManager` (the R6 class in `mod_logging.R`) handles its own database internally rather than using `db_handler.R`

### 7.4 Interfaces file (`interfaces.R`) is decorative only

The file defines interface contracts (`api_interface`, `specimen_interface`, `bags_interface`, etc.) with stub functions. None of these interfaces are:
- Implemented by any R6 class
- Checked at runtime
- Used for any dispatch or validation

R does not enforce interface contracts, so these are documentation-only. No code references `api_interface$set_api_key` or any other interface member.

---

## 8. Unused Infrastructure

### 8.1 Table State Utils (`table_state_utils.R`)

A comprehensive table state management system was built with:
- `init_table_state()` - state store initialization
- `handle_flag_change()` / `handle_note_change()` - event handlers
- `save_table_state()` / `load_table_state()` - persistence to disk
- `verify_state_integrity()` / `clean_state_data()` - integrity checks
- Its own `get_table_callback()` with a different JavaScript implementation

**None of these are called.** Table state management is handled differently:
- `table_utils.R` has its own `sync_table_states()` and `get_table_callback()`
- `StateManager` R6 class handles global state
- Each module manages its own `reactiveValues`

### 8.2 Specimen Ranking Utils (`specimen_ranking.R`)

`calculate_specimen_rank()` and `rank_species_specimens()` were written as utility functions, but the `SpecimenProcessor` R6 class has its own `private$rank_specimens()` method that duplicates this logic. The utility functions are never called.

### 8.3 Specimen Validation Utils (`specimen_validation.R`)

`validate_specimen()`, `validate_criteria()`, and `validate_quality_metrics()` are standalone functions that duplicate what `SpecimenValidator` R6 class does. The R6 class is used; the standalone functions are not.

### 8.4 Empty File

`R/modules/state/table_state_handler.R` is empty (0 bytes). It appears to be a placeholder that was never implemented.

### 8.5 Legacy Root File

`2024-11-22_v7_4_22.R` sits in the project root. It appears to be a full backup of an older version of the app. It is not sourced and should be removed from the repository.

---

## 9. State Management Complexity

The application has **three overlapping state management approaches** running simultaneously:

### Layer 1: `StateManager` R6 class (centralized)
- `state$get_store()`, `state$update_state()`
- Used by all modules as the source of truth

### Layer 2: Module-local `reactiveValues` (per-module)
- Each module maintains its own `rv <- reactiveValues(...)` with `selected_specimens`, `flagged_specimens`, `curator_notes`, `metrics`
- These are synced to/from `StateManager` via `sync_state_with_rv()` / `sync_rv_with_state()`

### Layer 3: JavaScript `localStorage` (client-side)
- `get_table_callback()` in `table_utils.R` writes state to `localStorage`
- `stateManager.save()` persists flag/note changes to the browser
- Cross-table sync via `CustomEvent('stateChange')`

### Issues with This Approach
1. **State key naming inconsistency**: `flagged_specimens` (local rv) maps to `specimen_flags` (StateManager), `curator_notes` maps to `specimen_curator_notes`. This mapping is done in `sync_state_with_rv()`/`sync_rv_with_state()` but is fragile.
2. **Bidirectional sync observers** run continuously, checking all keys every reactive cycle
3. **Both** the specimen handling module AND each BAGS grading module maintain their own flags/notes/selections. When a flag is set in the BAGS A tab, it must propagate to the Specimens tab and vice versa through StateManager.
4. **`invalidateLater(100)`** is used as a hack to force table refreshes after state changes (see `mod_specimen_handling_server.R:415,446,476` and `mod_bags_grading_server.R:268,293`).

---

## 10. Proposed Solutions

### P1. Fix Double-Sourcing (Critical)

**Choose one sourcing strategy.** The recommended approach:

- **Remove the glob-based sourcing from `global.R`** (delete `.source_utils()` and `.source_modules()` calls)
- **Keep the explicit sourcing in `app.R`** since it gives you control over load order
- **Add the missing files to `app.R`'s source list** if they are needed (e.g., `specimen_validator.R` which is currently only picked up by the glob)

Alternatively, go fully glob-based:
- Remove all explicit `source()` calls from `app.R`
- Rely entirely on `global.R`'s glob sourcing
- But this sacrifices control over load order

### P2. Delete Orphan Files (High Priority)

These files can be safely removed:

| File | Action |
|------|--------|
| `R/modules/bags_grading/old_mod_bags_table_utils.R` | **Delete** - legacy file, all functions are duplicates |
| `R/modules/bags_grading/mod_bags_table_utils.R` | **Delete** - all functions duplicated in `table_utils.R` or `mod_bags_grading_utils.R`, never called |
| `R/modules/state/table_state_handler.R` | **Delete** - empty file |
| `R/utils/table_state_utils.R` | **Delete or integrate** - never called; `table_utils.R` has its own implementations |
| `2024-11-22_v7_4_22.R` | **Delete** - legacy backup file in project root |

### P3. Remove or Integrate Orphan Utility Functions

| File | Functions to Remove | Reason |
|------|--------------------|----|
| `R/utils/specimen_ranking.R` | `calculate_specimen_rank()`, `rank_species_specimens()` | Duplicated by `SpecimenProcessor$rank_specimens()` |
| `R/utils/specimen_validation.R` | `validate_specimen()`, `validate_criteria()`, `validate_quality_metrics()` | Duplicated by `SpecimenValidator` R6 class |
| `R/utils/db_handler.R` | All 5 functions | Never called. Either wire into `LoggingManager` / modules or remove. |

**Decision point for `db_handler.R`:** If persistent flag/note storage across sessions is a desired feature, wire `db_handler.R` into the specimen handling and BAGS modules. If not, delete it and the `data/specimen_tracking.db` file.

### P4. Clean Up Dead Functions in Active Files

**`R/utils/table_utils.R`** - remove these unused functions:
- `format_interactive_column()` (commented-out caller at line 233)
- `format_table_columns()` (never called)
- `prepare_table_data()` (never called; `prepare_module_data()` is used)
- `format_specimen_fields()` (never called)
- `format_metrics()` (never called)
- `create_download_filename()` (never called)
- `validate_table_input()` (never called)

**`R/modules/bags_grading/mod_bags_grading_utils.R`** - remove:
- `validate_bags_data()` (never called)
- `prepare_download_data()` (never called)
- `find_specimen_in_groups()` (never called)
- `calculate_selection_metrics()` (never called)
- `calculate_flag_metrics()` (never called)
- `validate_specimen_selection()` (never called)

**`R/modules/specimen_handling/mod_specimen_handling_server.R`** - remove:
- `update_filtered_data()` at line 628 (never called; logic is inline)
- `validate_specimen_data()` at line 666 (never called)

### P5. Fix Broken References (Critical)

1. **Add `get_metrics()` to `SpecimenProcessor`:**
   ```r
   # In specimen_processor.R, add to public methods:
   get_metrics = function() {
     private$last_metrics
   }
   ```

2. **Define `validate_selected_specimens` or remove the reference in `app.R:343`:**
   ```r
   # Either define:
   validate_selected_specimens <- function(value) {
     list(valid = is.list(value), messages = NULL)
   }
   # Or change app.R:343 to not pass a validation function
   ```

### P6. Simplify Interfaces File

`R/modules/interfaces.R` currently defines interface stubs that are never checked or enforced. Options:

1. **Delete** the file and the `source()` call in `app.R` if they serve no documentation purpose
2. **Convert to documentation** - keep as a reference but add a comment that these are aspirational contracts, not enforced

### P7. Consolidate State Management (Medium Priority)

The three-layer state system (StateManager + local rv + JS localStorage) creates complexity. A simpler approach:

1. **Remove `sync_state_with_rv()` / `sync_rv_with_state()`** helper functions
2. **Use `StateManager` as the single source of truth** - modules read from `state$get_store()` directly
3. **Remove local `rv` duplicates** of state that exists in StateManager (selected_specimens, flagged_specimens, curator_notes)
4. **Keep module-specific `rv`** only for truly local state (filtered_data, processing_status)
5. **Standardize key names** - remove the mapping layer between `flagged_specimens` ↔ `specimen_flags`

### P8. Add the Missing `specimen_validator.R` to `app.R` Source List

Currently `app.R` sources `specimen_processor.R` and `specimen_scorer.R` but NOT `specimen_validator.R`. The validator only loads because `global.R` globs it. If you fix P1 by removing glob sourcing, this will break. Add:

```r
source("R/modules/specimen_handling/specimen_validator.R")
```

to `app.R` before `specimen_processor.R`.

---

## Work Items

Ordered for safe, incremental execution. Each item is independent and results in one commit/PR.

### Round 1 — Fix Runtime Errors (Critical)

- [x] **W1. Add `get_metrics()` to SpecimenProcessor** `P1-critical` `bug`
  - **File:** `R/modules/specimen_handling/specimen_processor.R`
  - **Problem:** `mod_specimen_handling_server.R:163` calls `processor$get_metrics()` but the method doesn't exist. Will crash at runtime.
  - **Fix:** Add `get_metrics = function() { private$last_metrics }` to `SpecimenProcessor` public methods.

- [x] **W2. Define or remove `validate_selected_specimens`** `P1-critical` `bug`
  - **File:** `app.R:343`
  - **Problem:** `validate_selected_specimens` is passed to `state$update_state()` as a validation callback but is never defined anywhere. Will error when specimens are selected from BAGS tabs.
  - **Fix:** Either define the function or remove the third argument from the `update_state()` call.

### Round 2 — Fix Double-Sourcing (Critical)

- [x] **W3. Eliminate double-sourcing between global.R and app.R** `P1-critical` `architecture`
  - **Files:** `global.R`, `app.R`
  - **Problem:** `global.R` globs all R files, then `app.R` sources ~30 files explicitly. Everything runs twice.
  - **Fix:** Remove `.source_utils()` and `.source_modules()` from `global.R`. Keep `global.R` for package installation, directory creation, and config loading only. Keep `app.R` as the single source of truth for file loading order.
  - **Depends on:** W4 (must add missing source to `app.R` first).

- [x] **W4. Add missing `specimen_validator.R` to app.R** `P1-critical` `bug`
  - **File:** `app.R`
  - **Problem:** `app.R` sources `specimen_processor.R` and `specimen_scorer.R` but not `specimen_validator.R`. Works only because `global.R` globs it. Will break after W3.
  - **Fix:** Add `source("R/modules/specimen_handling/specimen_validator.R")` before `specimen_processor.R` in `app.R`.

### Round 3 — Delete Orphan Files (High Priority)

- [x] **W5. Delete orphan and legacy files** `P2-high` `cleanup`
  - **Files to delete:**
    - `R/modules/bags_grading/old_mod_bags_table_utils.R` (legacy copy)
    - `R/modules/bags_grading/mod_bags_table_utils.R` (superseded, zero callers)
    - `R/modules/state/table_state_handler.R` (empty)
    - `R/utils/table_state_utils.R` (zero callers, alternative implementation never wired in)
    - `R/utils/specimen_ranking.R` (duplicated by SpecimenProcessor)
    - `R/utils/specimen_validation.R` (duplicated by SpecimenValidator)
  - **Verify:** Confirm no caller exists for any function in these files (already verified in this review).

- [x] **W6. Decide fate of `db_handler.R`** `P2-high` `architecture`
  - **File:** `R/utils/db_handler.R`
  - **Problem:** Full SQLite persistence layer (init_database, get/update specimen flags/notes) with zero callers. `LoggingManager` handles its own DB separately.
  - **Decision:** Deleted. Zero callers, recoverable from git history if persistence is needed later.

### Round 4 — Clean Dead Functions in Active Files (Medium)

- [x] **W7. Remove orphan functions from `table_utils.R`** `P3-medium` `cleanup`
  - **File:** `R/utils/table_utils.R`
  - **Remove:** `format_interactive_column()`, `format_table_columns()`, `prepare_table_data()`, `format_specimen_fields()`, `format_metrics()`, `create_download_filename()`, `validate_table_input()`
  - **Keep:** All functions that ARE called (`format_specimen_table()`, `get_table_callback()`, `sync_table_states()`, `prepare_module_data()`, `order_columns()`, `get_flag_options()`, `get_table_css()`, `create_table_container()`, formatting helpers, and the `format_*_table()` family).

- [x] **W8. Remove orphan functions from `mod_bags_grading_utils.R`** `P3-medium` `cleanup`
  - **File:** `R/modules/bags_grading/mod_bags_grading_utils.R`
  - **Remove:** `validate_bags_data()`, `prepare_download_data()`, `find_specimen_in_groups()`, `calculate_selection_metrics()`, `calculate_flag_metrics()`, `validate_specimen_selection()`
  - **Keep:** `filter_grade_specimens()`, `calculate_grade_metrics()`, `organize_grade_specimens()`, `create_grade_tables()`, `format_grade_table()`, `generate_table_caption()`

- [x] **W9. Remove orphan functions from `mod_specimen_handling_server.R`** `P3-medium` `cleanup`
  - **File:** `R/modules/specimen_handling/mod_specimen_handling_server.R`
  - **Remove:** `update_filtered_data()` (line 628), `validate_specimen_data()` (line 666) — both defined outside the `moduleServer()` block and never called.

### Round 5 — Structural Improvements (Medium, Optional)

- [x] **W10. Remove `interfaces.R`** `P3-medium` `cleanup`
  - **File:** `R/modules/interfaces.R`
  - **Problem:** Defines interface stubs (`api_interface`, `specimen_interface`, etc.) that are never checked, implemented, or referenced. R doesn't enforce these.
  - **Fix:** Deleted file and removed `source()` call from `app.R`.

- [x] **W11. Fix state persistence across views and filtering** `P3-medium` `architecture`
  - **Files:** `app.R`, `mod_bags_grading_server.R`, `R/utils/table_utils.R`
  - **Problems fixed:**
    - **(a) Species-vs-processid key mismatch (app.R):** Cross-module selection handler used `species` as dictionary key but specimen handling expected `processid`. Fixed to key by processid with full metadata, and deselect previous representative for same species (radio button = one per species).
    - **(b) BAGS module only restored curator_notes from state, not selections or flags.** Added restoration of `selected_specimens` and `specimen_flags` from StateManager on init, so switching BAGS tabs preserves all annotations.
    - **(c) JS localStorage keys invalidated on every table re-render.** `stateVersion` was incremented on each render, making localStorage keys stale. Fixed by using stable `specimen_{processid}` keys shared across all tables. Removed `stateVersion` tracking entirely.

---

## Summary of Issues by Severity

| Severity | Count | Issues |
|----------|-------|--------|
| **Critical** | 3 | Double-sourcing, `get_metrics()` missing method, `validate_selected_specimens` undefined |
| **High** | 6 | 5 orphan files, db_handler disconnected |
| **Medium** | 16 | Orphan functions in active files, name collisions, state complexity |
| **Low** | 3 | Empty file, legacy root file, decorative interfaces |

### Dead Code Count

- **5 fully orphan files** (never effectively called)
- **1 empty placeholder file**
- **1 legacy backup file**
- **~25 orphan functions** within active files
- **~12 duplicate function definitions** across files
- **2 broken runtime references** that will produce errors when triggered
