# BOLDcuratoR - Planned Updates

Tracking document for planned codebase updates. Each item is formatted as a GitHub-style issue with investigation findings, options, and implementation details.

---

## Issue #1: Remove filtering panel and logic from BAGS tabs

**Priority:** Medium
**Labels:** `ui`, `refactor`, `bags-grading`

### Description

The BAGS grade tabs (A-E) each have a "Filter Controls" box with rank filters and criteria filters that duplicate the specimen tab's filtering. These should be removed from the BAGS tabs while leaving them intact on the Specimen tab.

### Current State

The filtering panel is defined in two places:

- **BAGS UI:** `R/modules/bags_grading/mod_bags_grading_ui.R` lines 45-103 — renders a "Filter Controls" box with `rank_filter` (selectInput), `criteria_filter` (selectInput, multiple), and `reset_filters` button.
- **BAGS Server:** `R/modules/bags_grading/mod_bags_grading_server.R` lines 72-103 — calls `filter_grade_specimens()` using these filter inputs.
- **BAGS Utils:** `R/modules/bags_grading/mod_bags_grading_utils.R` lines 41-68 — `filter_grade_specimens()` function applies rank, quality, and criteria filters.
- **Specimen UI (keep):** `R/modules/specimen_handling/mod_specimen_handling_ui.R` lines 38-95 — "Quality Filters" box with the same filter types.
- **Specimen Server (keep):** `R/modules/specimen_handling/mod_specimen_handling_server.R` lines 201-268 — filter observe block.

### Options

**Option A (Recommended): Remove filter UI and simplify server logic**
1. Remove the "Filter Controls" `box()` from `mod_bags_grading_ui.R` (lines 45-103).
2. Simplify the server observer in `mod_bags_grading_server.R` to skip filter calls — just filter by grade species and pass through.
3. Remove the `reset_filters` handler (lines 238-243).
4. Optionally keep `filter_grade_specimens()` in utils but remove the rank/quality/criteria branches (or remove the function entirely and inline the grade filtering).

**Option B: Hide filters behind a toggle**
Keep the code but wrap filters in a collapsible panel, hidden by default. Lower effort but adds UI complexity.

### Files to Modify

| File | Change |
|------|--------|
| `R/modules/bags_grading/mod_bags_grading_ui.R` | Remove filter controls box (lines 45-103) |
| `R/modules/bags_grading/mod_bags_grading_server.R` | Remove filter input references and `reset_filters` handler |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | Simplify or remove `filter_grade_specimens()` |

### Notes

- The `download_data` and `show_help` buttons are co-located with the filter controls in the BAGS UI. These need to be relocated (e.g., into the specimen tables box header) when the filter box is removed.
- Each BAGS tab (A-E) is a separate module instance, so the change only needs to happen once in the shared module code.
- The `min_quality_score` input is referenced in server code but never actually defined in either UI — a latent bug that can be cleaned up as part of this work.

---

## Issue #2: Ensure curator annotations are present in all download formats

**Priority:** High
**Labels:** `bug`, `export`, `annotations`

### Description

Curator annotations (selected status, flags, curator notes) should be included in every download format. Currently, annotation inclusion is inconsistent across export paths.

### Current State

**Annotation columns tracked:**
- `selected` — boolean (stored in `state$selected_specimens` as named list by processid)
- `flag` — string: `""`, `"misidentification"`, `"id_uncertain"`, `"data_issue"`, `"other_issue"` (stored in `state$specimen_flags`)
- `curator_notes` — free text (stored in `state$specimen_curator_notes`)

**Export paths and annotation coverage:**

| Export Path | File | Annotations Included? |
|-------------|------|-----------------------|
| Specimen tab → "Download Filtered Data" TSV | `mod_specimen_handling_server.R:532-546` | **Partial** — writes `rv$filtered_data` which has annotations from `prepare_module_data()` but raw `write.table()` may include HTML widget markup |
| Specimen tab → "Download Selected" TSV | `mod_specimen_handling_server.R:548-564` | **Partial** — same issue as filtered |
| BAGS tab → `prepare_download_data()` | `mod_bags_grading_utils.R:321-346` | **Yes** — adds `selected` and `flag` columns explicitly, but **missing `curator_notes`** |
| ExportManager → Excel | `mod_export.R:12-72` | **No** — uses `get_specimen_columns()` (line 199-204) which returns a fixed list of 17 columns that does **not** include `selected`, `flag`, or `curator_notes` |
| ExportManager → TSV | `mod_export.R:74-118` | **No** — same `get_specimen_columns()` list |
| ExportManager → FASTA | `mod_export.R:120-163` | N/A — FASTA format is sequence-only |
| DT table built-in CSV/Excel buttons | `table_utils.R:107-125` | **Yes** — exports visible table data including interactive columns |

### Options

**Option A (Recommended): Update `get_specimen_columns()` and `prepare_download_data()`**
1. Add `"selected"`, `"flag"`, `"curator_notes"` to the `get_specimen_columns()` list in `mod_export.R:199-204`.
2. Before export, merge annotation state into the specimen data (similar to how `prepare_module_data()` works in `table_utils.R:429-538`).
3. Fix `prepare_download_data()` in `mod_bags_grading_utils.R:328-346` to also include `curator_notes`.
4. Ensure TSV download handlers in `mod_specimen_handling_server.R` write clean text values, not HTML widget markup.

**Option B: Create a dedicated `prepare_export_data()` utility**
A single function that takes raw specimen data + state and produces a clean export-ready data frame with all annotations merged in. All export paths call this function.

### Files to Modify

| File | Change |
|------|--------|
| `R/modules/export/mod_export.R` | Update `get_specimen_columns()` to include annotation columns; merge annotations before export |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | Add `curator_notes` to `prepare_download_data()` |
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | Ensure download handlers produce clean text (no HTML) |
| `R/utils/table_utils.R` | Consider extracting annotation-merge logic into a reusable `merge_annotations()` function |

### Notes

- The `prepare_module_data()` function in `table_utils.R:429-538` already has robust logic for merging annotations — this can be refactored into a shared utility.
- Need to handle the case where annotations reference processids no longer in the filtered data.
- FASTA export does not need annotations but the header line could optionally include flag status.

---

## Issue #3: Dynamic species section sizing with pagination in BAGS tabs

**Priority:** Medium
**Labels:** `ui`, `enhancement`, `bags-grading`

### Description

On the BAGS tabs, each species section (rendered as a separate DT table) should grow/shrink based on the number of records, up to a maximum of 25 rows. Beyond 25 records, add page navigation.

### Current State

- **Table creation:** `R/modules/bags_grading/mod_bags_table_utils.R` lines 30-54 — DT options use `pageLength = -1` (show all rows) and `dom = "t"` (table only, no pagination controls).
- **Table rendering:** `R/modules/bags_grading/mod_bags_grading_utils.R` lines 193-241 — `create_grade_tables()` iterates over organized groups and creates one table per group.
- **Organization:** Species groups can contain anywhere from 1 to hundreds of records depending on the grade and species.

### Options

**Option A (Recommended): Dynamic `pageLength` with conditional pagination**
1. In `create_specimen_table()` (`mod_bags_table_utils.R`), change:
   - `pageLength` from `-1` to `min(nrow(data), 25)`
   - `dom` from `"t"` to `ifelse(nrow(data) > 25, "tip", "t")` — adds **i**nfo and **p**agination only when needed.
2. Add CSS to remove unnecessary padding when tables are small.

**Option B: Virtual scrolling**
Use DT's `scroller` extension instead of pagination. Shows a scrollable area sized to content (up to a max height). More modern feel but requires the Scroller extension.

**Option C: Collapsible sections**
Keep showing all rows but wrap each species section in a collapsible accordion. Default-expanded for small sets, collapsed for large ones.

### Files to Modify

| File | Change |
|------|--------|
| `R/modules/bags_grading/mod_bags_table_utils.R` | Update `create_specimen_table()` to accept row count and set `pageLength`/`dom` dynamically |
| `R/modules/bags_grading/mod_bags_grading_utils.R` | Pass record count to table creation |

### Implementation Detail (Option A)

```r
# In create_specimen_table(), replace static options:
n_rows <- nrow(data)
options <- list(
  scrollX = TRUE,
  fixedColumns = list(left = 2),
  ordering = TRUE,
  order = list(list(2, 'desc')),
  dom = if (n_rows > 25) "tip" else "t",
  pageLength = min(n_rows, 25),
  # ... rest of options
)
```

### Notes

- This change applies to all five BAGS grade tabs (A-E) since they share the same module code.
- Grade E tables (shared BINs) can be large — pagination is most impactful there.
- Consider adding `lengthMenu` option to allow users to override the 25-row limit if desired.

---

## Issue #4: Species checklist / gap analysis panel

**Priority:** High
**Labels:** `feature`, `new-panel`, `analysis`

### Description

Develop a new analysis panel (similar to BIN Analysis) focused on species. It should present a checklist of all taxa found in the data and, if a taxa input list was provided, perform a gap analysis showing which taxa from the input list are present/absent in the results.

### Current State

**BIN Analysis panel (template to follow):**
- UI: `R/modules/bin_analysis/mod_bin_analysis_ui.R` — summary value boxes + tabbed content with DT tables
- Server: `R/modules/bin_analysis/mod_bin_analysis_server.R` — observe block processes data, renders tables and value boxes
- Utils: `R/modules/bin_analysis/mod_bin_analysis_utils.R` — `analyze_bin_data()`, `process_bin_content()`, `check_taxonomic_concordance()`

**Taxa input list (for gap analysis):**
- The user enters taxa in `input$taxa_input` (textarea in data import UI, `mod_data_import_ui.R`)
- Parsed to a vector in `prepare_search_params()` (`mod_data_import_utils.R:88-98`) as `params$taxonomy`
- Currently **not persisted in state** after search — only the resulting `specimen_data` is stored
- To enable gap analysis, the original taxa input list needs to be stored in state

**Species data available in processed specimens:**
- `species` column — species name (cleaned, with sp./spp./cf./aff. set to NA)
- `identification` — raw identification field (uncleaned)
- `genus`, `family`, `order` — higher taxonomy
- `bin_uri` — BIN assignment
- BAGS grade available from `state$bags_grades`

### Options

**Option A (Recommended): New `species_analysis` module**

Create a new module following the BIN analysis pattern:

1. **Files to create:**
   - `R/modules/species_analysis/mod_species_analysis_ui.R`
   - `R/modules/species_analysis/mod_species_analysis_server.R`
   - `R/modules/species_analysis/mod_species_analysis_utils.R`

2. **UI layout:**
   - Summary value boxes: Total Species, Species with BINs, Species without BINs, (if input list) Input Taxa Found, Input Taxa Missing
   - Tab 1: **Species Checklist** — DT table with columns: Species, Specimen Count, BIN Count, BIN URIs, BAGS Grade, Countries, Mean Quality Score
   - Tab 2: **Gap Analysis** (shown only if input list provided) — DT table with columns: Input Taxon, Status (Found/Missing/Partial Match), Matched Species, Specimen Count, Notes
   - Tab 3: **Summary Statistics** — breakdown by family/order, coverage metrics
   - Download button for the checklist and gap analysis as TSV/Excel

3. **Gap analysis logic:**
   - Exact match: input taxon matches `species` column
   - Partial match: input taxon matches at genus level (e.g., input "Aus bus" not found, but "Aus" genus has other species)
   - Higher-level match: input is a genus/family name that appears in data
   - Missing: no match at any level

4. **State change:** Store original taxa input list in `state$search_taxa` during data import so it persists for gap analysis.

**Option B: Add species tab to existing BIN analysis panel**
Less modular but avoids creating a new module. Add species analysis as additional tabs in the BIN Analysis panel.

### Files to Create/Modify

| File | Action |
|------|--------|
| `R/modules/species_analysis/mod_species_analysis_ui.R` | **Create** — UI definition |
| `R/modules/species_analysis/mod_species_analysis_server.R` | **Create** — server logic |
| `R/modules/species_analysis/mod_species_analysis_utils.R` | **Create** — analysis functions |
| `R/modules/data_import/mod_data_import_server.R` | **Modify** — store `params$taxonomy` in state |
| `R/modules/state/state_manager.R` | **Modify** — add `search_taxa` to initial state |
| `app.R` | **Modify** — source new module, add sidebar menu item, register module server |

### Gap Analysis Algorithm (Pseudocode)

```r
perform_gap_analysis <- function(input_taxa, specimen_data) {
  results <- data.frame(
    input_taxon = input_taxa,
    status = "Missing",
    matched_species = "",
    specimen_count = 0,
    notes = ""
  )

  for (i in seq_along(input_taxa)) {
    taxon <- input_taxa[i]

    # Exact species match
    exact <- specimen_data[specimen_data$species == taxon, ]
    if (nrow(exact) > 0) {
      results$status[i] <- "Found"
      results$matched_species[i] <- taxon
      results$specimen_count[i] <- nrow(exact)
      next
    }

    # Genus-level match
    genus <- strsplit(taxon, " ")[[1]][1]
    genus_matches <- specimen_data[
      grepl(paste0("^", genus, " "), specimen_data$species, ignore.case = TRUE), ]
    if (nrow(genus_matches) > 0) {
      results$status[i] <- "Partial (genus match)"
      results$matched_species[i] <- paste(unique(genus_matches$species), collapse = "; ")
      results$specimen_count[i] <- nrow(genus_matches)
      results$notes[i] <- "Exact species not found but genus present"
      next
    }

    # Higher taxonomy match (genus/family as standalone input)
    if (!grepl(" ", taxon)) {
      family_matches <- specimen_data[specimen_data$family == taxon | specimen_data$genus == taxon, ]
      if (nrow(family_matches) > 0) {
        results$status[i] <- "Found (higher taxon)"
        results$matched_species[i] <- paste(unique(family_matches$species), collapse = "; ")
        results$specimen_count[i] <- nrow(family_matches)
      }
    }
  }

  results
}
```

### Notes

- The BIN analysis module is the best template to copy from — same modular structure (UI/server/utils split).
- The taxa input is currently discarded after search. Persisting it in state is essential for gap analysis and is a small change in `mod_data_import_server.R`.
- Consider fuzzy matching for common misspellings or synonym resolution (future enhancement).

---

## Issue #5: Download records marked with issues/annotations in TSV format

**Priority:** Medium
**Labels:** `feature`, `export`, `annotations`

### Description

Add a download button that exports only records that have been flagged with issues or have curator annotations (flags and/or notes), in TSV format.

### Current State

**Existing download buttons (Specimen tab):**
- `download_filtered` — downloads all filtered specimens as TSV (`mod_specimen_handling_server.R:532-546`)
- `download_selected` — downloads selected specimens as TSV (`mod_specimen_handling_server.R:548-564`)

**Annotation data locations:**
- Flags: `rv$flagged_specimens` — named list by processid, each entry has `$flag`, `$timestamp`, `$species`, `$user`
- Notes: `rv$curator_notes` — named list by processid, each entry has `$text`, `$timestamp`, `$user`
- Global state mirrors: `state$specimen_flags`, `state$specimen_curator_notes`

**There is no existing button for downloading annotated/flagged records only.**

### Options

**Option A (Recommended): Add download button to Specimen tab**

1. Add a new `downloadButton` in `mod_specimen_handling_ui.R` alongside existing download buttons (around line 110-118):
   ```r
   downloadButton(ns("download_annotated"),
                  "Download Annotated Records",
                  class = "btn-warning")
   ```

2. Add handler in `mod_specimen_handling_server.R`:
   ```r
   output$download_annotated <- downloadHandler(
     filename = function() {
       paste0("annotated_specimens_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
     },
     content = function(file) {
       data <- rv$filtered_data
       flags <- rv$flagged_specimens
       notes <- rv$curator_notes

       # Get processids with any annotation
       annotated_ids <- unique(c(names(flags), names(notes)))
       annotated_data <- data[data$processid %in% annotated_ids, ]

       # Merge annotation details
       annotated_data <- merge_annotations(annotated_data, flags, notes)

       write.table(annotated_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
     }
   )
   ```

**Option B: Add to BAGS tabs as well**
Extend the same button to each BAGS grade tab, downloading annotated records for that specific grade.

### Files to Modify

| File | Change |
|------|--------|
| `R/modules/specimen_handling/mod_specimen_handling_ui.R` | Add `downloadButton("download_annotated", ...)` near line 110-118 |
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | Add `output$download_annotated` handler |
| `R/utils/table_utils.R` | Extract/create `merge_annotations()` utility (reusable for Issue #2) |

### Notes

- Depends on Issue #2 (annotation merge utility) — implement that first or together.
- The TSV should include all standard specimen columns plus `flag`, `curator_notes`, `flag_timestamp`, `flag_user` for full audit trail.
- Consider whether "annotated" means flags OR notes OR both — recommend: any record with at least one non-empty flag or note.

---

## Issue #6: Download records marked as selected in TSV format

**Priority:** Low
**Labels:** `enhancement`, `export`

### Description

Ensure there is a working download button for records the user has marked as "selected" in TSV format.

### Current State

**This functionality largely exists already:**

- **UI button:** `mod_specimen_handling_ui.R:115-118` — `downloadButton(ns("download_selected"), "Download Selected", class = "btn-info")`
- **Server handler:** `mod_specimen_handling_server.R:548-564` — filters `rv$filtered_data` by processids in `rv$selected_specimens`

**Issues with current implementation:**
1. The download writes raw `rv$filtered_data` columns which may include HTML widget markup from the interactive `selected`/`flag`/`curator_notes` columns (these are rendered as checkbox/dropdown/textarea HTML in the table).
2. Annotation columns (flag, curator_notes) may not be clean text values in the download.
3. No equivalent download button exists on the BAGS tabs.

### Options

**Option A (Recommended): Fix existing handler and add annotation merge**
1. In the `download_selected` handler, use the same `merge_annotations()` utility (from Issue #2/5) to produce clean text columns.
2. Ensure `selected`, `flag`, and `curator_notes` columns contain plain text values, not HTML.
3. Optionally add a "Download Selected" button to BAGS tabs.

**Option B: Replace with ExportManager path**
Route selected downloads through `ExportManager$export_tsv()` for consistency. Requires passing selection state to the export manager.

### Files to Modify

| File | Change |
|------|--------|
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | Fix `output$download_selected` handler to clean HTML artifacts |
| `R/utils/table_utils.R` | Ensure `merge_annotations()` (from Issue #2) returns clean text |

### Notes

- This is mostly a fix/polish of existing functionality rather than new feature development.
- Shares the annotation merge utility with Issues #2 and #5.
- Consider consolidating all three download buttons (filtered, selected, annotated) into a dropdown or button group for cleaner UI.

---

## Issue #7: Read-only annotations in specimen table

**Priority:** Medium
**Labels:** `feature`, `ui`, `annotations`, `investigation`

### Description

Investigate how to display curator annotations (flags and notes) that were made in BAGS tabs back in the Specimen table, but in read-only mode (non-editable).

### Current State

**Annotation flow:**
1. Users make annotations (select, flag, add notes) in **both** the Specimen tab and BAGS tabs.
2. Both tabs write to the **same** global state via `StateManager`:
   - `state$selected_specimens`
   - `state$specimen_flags`
   - `state$specimen_curator_notes`
3. The Specimen tab reads annotations from `rv$selected_specimens`, `rv$flagged_specimens`, `rv$curator_notes` (local reactive copies synced with state).
4. The BAGS tabs also read from their own local `rv` copies synced with the same state.

**Current specimen table rendering:**
- `table_utils.R:140-209` — renders `selected` as a checkbox, `flag` as a `<select>` dropdown, `curator_notes` as an editable field.
- These are **always interactive** — there's no read-only mode.

**State sync mechanism:**
- `mod_specimen_handling_server.R:10-57` — `sync_state_with_rv()` and `sync_rv_with_state()` functions.
- `table_utils.R:376-426` — `sync_table_states()` pushes flag/note values into data frame.

### Options

**Option A (Recommended): Add `read_only` parameter to `format_specimen_table()`**

1. Add a `read_only = FALSE` parameter to `format_specimen_table()` in `table_utils.R`.
2. When `read_only = TRUE`, render annotation columns differently:
   - `selected`: plain text "Yes"/"No" or a disabled checkbox (`<input type="checkbox" disabled>`)
   - `flag`: plain text label instead of `<select>` dropdown
   - `curator_notes`: plain text instead of editable field
3. Call with `read_only = TRUE` from the Specimen tab, `read_only = FALSE` from BAGS tabs.

**Column rendering (read-only mode):**
```javascript
// Selected column - read only
function(data, type, row) {
  if (type === 'display') {
    return data ? '✓' : '';
  }
  return data;
}

// Flag column - read only
function(data, type, row) {
  if (type === 'display') {
    var labels = {
      'misidentification': 'Misidentification',
      'id_uncertain': 'ID Uncertain',
      'data_issue': 'Data Issue',
      'other_issue': 'Other Issue'
    };
    return labels[data] || '';
  }
  return data;
}

// Curator notes - read only
// Just render as plain text (no contenteditable)
```

**Option B: Separate "Annotation Summary" section in specimen tab**
Instead of modifying the main table, add a separate collapsible section below the specimen table showing a summary of all annotations:
- A small DT table showing only annotated records with columns: processid, species, flag, curator_notes, annotated_by, timestamp.
- This keeps the main specimen table clean and avoids the read-only rendering complexity.

**Option C: Bidirectional editing with conflict resolution**
Make annotations editable in both places with a sync mechanism. Most complex option — would need conflict resolution when the same record is annotated differently in Specimen vs BAGS tabs. Not recommended unless there's a clear use case.

### Files to Modify

| File | Change |
|------|--------|
| `R/utils/table_utils.R` | Add `read_only` parameter to `format_specimen_table()`, create read-only column renderers |
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | Pass `read_only = TRUE` when rendering specimen table; ensure state sync pulls latest annotations from BAGS tabs |
| `R/modules/bags_grading/mod_bags_table_utils.R` | Ensure table callback sends annotations to shared state (already working) |

### Key Considerations

- **State freshness:** The specimen tab needs to reactively update when annotations change in BAGS tabs. Current `sync_rv_with_state()` should handle this, but verify the reactive dependency chain fires correctly.
- **Performance:** Adding read-only rendering should not degrade table performance since it's simpler than interactive rendering.
- **UX clarity:** Users should clearly understand where annotations are editable (BAGS tabs) vs. read-only (Specimen tab). Consider a visual indicator like a lock icon or muted styling.
- **Table callback changes:** The JavaScript callback in `format_specimen_table()` (`table_utils.R:710-809`) attaches event handlers for flag/note changes. These should be conditionally excluded when `read_only = TRUE` to prevent sending spurious Shiny input updates.

---

## Implementation Order

Recommended sequencing based on dependencies and impact:

```
Phase 1 - Foundation
  ├── #2 Annotation merge utility (shared dependency for #5, #6)
  ├── #1 Remove BAGS filters (independent, clean refactor)
  └── #8a Wire up existing SQLite for annotation persistence

Phase 2 - Downloads & Persistence
  ├── #5 Download annotated records (depends on #2)
  ├── #6 Fix selected records download (depends on #2)
  └── #8b RDS persistence for specimen data + session metadata

Phase 3 - UI Enhancements
  ├── #3 Dynamic pagination in BAGS tabs (independent)
  ├── #7 Read-only annotations in specimen table (independent)
  └── #8c Session resume UI + auto-save timer

Phase 4 - New Feature
  └── #4 Species checklist / gap analysis panel (largest scope, independent)
```

---

## Cross-cutting Concerns

### Shared Annotation Merge Utility

Issues #2, #5, and #6 all need a clean way to merge annotations into specimen data for export. Create a single utility function:

```r
# R/utils/export_utils.R (new file) or add to table_utils.R
merge_annotations_for_export <- function(data, selections, flags, notes) {
  data$selected <- data$processid %in% names(selections)
  data$flag <- sapply(data$processid, function(pid) {
    if (!is.null(flags[[pid]])) as.character(flags[[pid]]$flag %||% "") else ""
  })
  data$curator_notes <- sapply(data$processid, function(pid) {
    if (!is.null(notes[[pid]])) as.character(notes[[pid]]$text %||% "") else ""
  })
  data$flag_user <- sapply(data$processid, function(pid) {
    if (!is.null(flags[[pid]])) as.character(flags[[pid]]$user %||% "") else ""
  })
  data$flag_timestamp <- sapply(data$processid, function(pid) {
    if (!is.null(flags[[pid]])) as.character(flags[[pid]]$timestamp %||% "") else ""
  })
  data
}
```

### State Persistence for Taxa Input

Issue #4 requires the original taxa input list to persist in state. This is a one-line addition to `state_manager.R` initial state and a small addition to `mod_data_import_server.R`:

```r
# In state_manager.R initial_state:
search_taxa = NULL

# In mod_data_import_server.R after successful search:
state$update_state("search_taxa", params$taxonomy)
```

---

## Issue #8: Saved session state — resume work across sessions

**Priority:** High
**Labels:** `feature`, `infrastructure`, `state-management`, `persistence`

### Description

Users need to be able to resume their work after closing the browser, losing internet, or returning the next day. This requires persisting application state (specimen data, annotations, selections, search parameters) to disk and providing a mechanism to restore it. Auto-saving as actions are made is essential to prevent data loss from unexpected disconnections.

### Current State — What Exists Already

The codebase has **significant infrastructure already built** for this but **none of it is wired up**:

| Component | Status | Location |
|-----------|--------|----------|
| **SQLite database handler** | Fully implemented, **never called** | `R/utils/db_handler.R` |
| SQLite database file | Exists on disk (98KB) | `data/specimen_tracking.db` |
| Tables: `specimen_flags`, `specimen_notes`, `specimen_selections` | Schema defined, tables created | `db_handler.R:18-61` |
| CRUD functions: `get_specimen_flags()`, `update_specimen_flag()`, etc. | Implemented with parameterised queries | `db_handler.R:77-179` |
| **RDS save/load for table state** | Implemented, **never called** | `R/utils/table_state_utils.R:346-397` |
| `save_table_state()` / `load_table_state()` | Working, with integrity verification | `table_state_utils.R:350-369` |
| `verify_state_integrity()` | Checks for required fields | `table_state_utils.R:374-380` |
| **StateManager** | In-memory only, no persistence | `R/modules/state/state_manager.R` |
| State history tracking | Records changes but discards on session end | `state_manager.R:327-336` |
| Session token (`session$token`) | Used for logging, not persistence | `app.R:240,351,404,420` |
| `session$onSessionEnded` | Calls `state$reset_state()` — **destroys all state** | `app.R:417-424` |
| **Directories** | `data/`, `logs/`, `output/`, `temp/` created at startup | `global.R:141-148` |
| DBI + RSQLite | Listed as dependencies, loaded | `global.R:31-32` |

### What Needs to Be Persisted

| Data | Size Estimate | Volatility | Storage Strategy |
|------|---------------|------------|------------------|
| `specimen_data` (data frame) | 1-50 MB (hundreds to tens of thousands of rows, ~20+ columns including `nuc` sequences) | Set once per import, rarely changes | RDS file (fast serialization of large data frames) |
| `selected_specimens` | < 100 KB | Changes frequently (user clicks) | SQLite (row-level updates) |
| `specimen_flags` | < 100 KB | Changes frequently | SQLite (already has table schema) |
| `specimen_curator_notes` | < 500 KB | Changes occasionally | SQLite (already has table schema) |
| `bags_grades` | < 500 KB | Set once after processing | RDS file (alongside specimen data) |
| `bin_analysis` | 1-5 MB | Set once after analysis | RDS file |
| `user_info` (minus API key) | < 1 KB | Set once | SQLite or JSON |
| `search_taxa` (input list) | < 10 KB | Set once per search | RDS or JSON |
| `specimen_metrics` | < 1 KB | Derived, can be recalculated | Skip — recalculate on restore |

### Options

#### Option A (Recommended): Hybrid SQLite + RDS with auto-save

Use the **existing SQLite database** for annotation state (flags, notes, selections) since it supports row-level updates efficiently, and **RDS files** for large data frames (specimen data, analysis results) since R serialization is much faster and smaller than SQLite for bulk data.

**Architecture:**

```
data/sessions/{session_id}/
  ├── session_meta.json     # session metadata, user info, search params, timestamps
  ├── specimen_data.rds     # large data frame
  ├── bags_grades.rds       # BAGS grade assignments
  └── bin_analysis.rds      # BIN analysis results (if run)

data/specimen_tracking.db   # shared across sessions (existing file)
  ├── specimen_flags         # (existing table)
  ├── specimen_notes         # (existing table)
  ├── specimen_selections    # (existing table)
  └── sessions               # NEW: session registry table
```

**Session registry table (new):**
```sql
CREATE TABLE sessions (
  session_id TEXT PRIMARY KEY,
  user_email TEXT,
  user_name TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  search_taxa TEXT,          -- JSON array of input taxa
  specimen_count INTEGER,
  status TEXT DEFAULT 'active'  -- active, completed, abandoned
);
```

**Annotation tables (modify existing):**
Add `session_id` column to existing tables so annotations are scoped to sessions:
```sql
ALTER TABLE specimen_flags ADD COLUMN session_id TEXT;
ALTER TABLE specimen_notes ADD COLUMN session_id TEXT;
ALTER TABLE specimen_selections ADD COLUMN session_id TEXT;
```

**Auto-save triggers:**

| Event | What to Save | Method |
|-------|-------------|--------|
| Specimen data imported | `specimen_data`, `bags_grades`, session metadata | `saveRDS()` + SQLite session row |
| BIN analysis completed | `bin_analysis` | `saveRDS()` |
| Specimen selected/deselected | Selection state | SQLite `update_specimen_selection()` (immediate) |
| Flag changed | Flag state | SQLite `update_specimen_flag()` (already implemented) |
| Curator note changed | Note state | SQLite `update_specimen_note()` (already implemented) |
| Periodic (every 60s if dirty) | Full state snapshot | `saveRDS()` of complete state |

**Session restore flow:**
1. On app start, show a "Resume Session" panel if saved sessions exist
2. User picks a session from a list (showing date, taxa searched, record count)
3. Load `specimen_data.rds`, `bags_grades.rds`, `bin_analysis.rds`
4. Query SQLite for flags/notes/selections scoped to that session_id
5. Populate `StateManager` with loaded data
6. Skip directly to the specimen/BAGS tabs

#### Option B: Pure SQLite (everything in the database)

Store specimen data as a SQLite table instead of RDS. This is more "database-native" but has drawbacks:
- **Slower**: Inserting thousands of rows with 20+ columns is significantly slower than `saveRDS()`
- **Schema rigidity**: Column names from BOLD API can vary; SQLite requires fixed schemas
- **Larger on disk**: SQLite stores text less efficiently than R's native serialization
- **Advantage**: SQL queries enable partial loading and filtering at the database level

#### Option C: Shiny bookmarking (`enableBookmarking`)

Shiny's built-in bookmarking serializes `input` values to URL params or server-side files. However:
- **Not suitable**: It only saves `input` widget values, not reactive data or computed state
- `specimen_data` (megabytes) cannot go in a URL or even server-side bookmark
- Annotations and selections are stored in `reactiveValues`, not `input`
- Would need to be combined with another approach anyway
- **Not recommended** as a primary mechanism, but could supplement Option A for widget state (filter values, active tab, etc.)

#### Option D: Browser-side persistence (shinyStore / cookies)

Use `shinyStore` or localStorage via JavaScript to cache state in the user's browser:
- **Advantage**: No server-side storage needed, survives page refresh
- **Disadvantage**: 5-10 MB localStorage limit is too small for specimen data with sequences; data is tied to the browser/device; won't work across devices
- **Not recommended** as primary approach, but could be useful for small UI state (active tab, filter selections)

### Recommended Approach: Option A with phased implementation

**Phase 1 — Wire up existing SQLite for annotations (lowest effort, highest impact)**

This alone solves the "lost annotations" problem during internet drops.

1. Initialize DB connection in `app.R` server function:
   ```r
   db_con <- init_database("data/specimen_tracking.db", logging_manager)
   session$onSessionEnded(function() { dbDisconnect(db_con) })
   ```

2. In `mod_bags_grading_server.R`, after each flag/note/selection change, call the existing DB functions:
   ```r
   # In the flag observer (lines 246-270):
   update_specimen_flag(db_con, flag$processid, flag$flag, user_info)

   # In the note observer (lines 272-296):
   update_specimen_note(db_con, note$processid, note$notes, user_info)
   ```

3. On session start, load existing annotations from DB and merge into state.

**Phase 2 — RDS persistence for specimen data**

1. After data import succeeds, save to RDS:
   ```r
   session_dir <- file.path("data", "sessions", session$token)
   dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
   saveRDS(processed_data, file.path(session_dir, "specimen_data.rds"))
   ```

2. After BAGS grading:
   ```r
   saveRDS(grades, file.path(session_dir, "bags_grades.rds"))
   ```

3. Write session metadata:
   ```r
   jsonlite::write_json(list(
     session_id = session$token,
     user_email = store$user_info$email,
     created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
     search_taxa = store$search_taxa,
     specimen_count = nrow(processed_data)
   ), file.path(session_dir, "session_meta.json"), auto_unbox = TRUE)
   ```

**Phase 3 — Session resume UI**

1. Add a "Resume Session" panel to the data import tab sidebar:
   ```r
   # Scan for saved sessions
   session_dirs <- list.dirs("data/sessions", recursive = FALSE)
   session_list <- lapply(session_dirs, function(d) {
     meta_file <- file.path(d, "session_meta.json")
     if (file.exists(meta_file)) jsonlite::fromJSON(meta_file) else NULL
   })
   ```

2. Render as a selectInput or DT table with columns: Date, User, Taxa, Records
3. "Resume" button loads the selected session's state

**Phase 4 — Auto-save on state change (resilience)**

1. Add a debounced observer that watches for state changes and saves dirty state:
   ```r
   # In app.R, after state initialization:
   auto_save_timer <- reactiveTimer(60000)  # every 60 seconds

   observe({
     auto_save_timer()
     isolate({
       store <- state$get_store()
       if (!is.null(store$specimen_data)) {
         save_session_state(session$token, store, db_con)
       }
     })
   })
   ```

2. For annotations, save immediately (no debounce) since SQLite handles concurrent writes well and each annotation is a single row upsert.

### Files to Create/Modify

| File | Action | Phase |
|------|--------|-------|
| `R/utils/session_persistence.R` | **Create** — `save_session_state()`, `load_session_state()`, `list_saved_sessions()`, `cleanup_old_sessions()` | 2 |
| `R/utils/db_handler.R` | **Modify** — add `session_id` to existing tables, add sessions table, add session-scoped query functions | 1 |
| `R/modules/state/state_manager.R` | **Modify** — add `save_to_disk()` / `restore_from_disk()` methods, accept DB connection | 1-2 |
| `app.R` | **Modify** — initialize DB connection, wire auto-save, add session resume logic, change `onSessionEnded` to save instead of destroy | 1-2 |
| `R/modules/bags_grading/mod_bags_grading_server.R` | **Modify** — call DB write functions after flag/note/selection changes | 1 |
| `R/modules/specimen_handling/mod_specimen_handling_server.R` | **Modify** — call DB write functions after changes | 1 |
| `R/modules/data_import/mod_data_import_ui.R` | **Modify** — add "Resume Session" panel | 3 |
| `R/modules/data_import/mod_data_import_server.R` | **Modify** — add session restore handler, save session after import | 2-3 |

### Key Considerations

- **Session ID strategy**: `session$token` is Shiny's built-in session identifier. It changes each time the user connects. For resume capability, we need a **user-facing session ID** (e.g., a short UUID stored in a cookie or shown to the user) so they can reconnect to a previous session even with a new `session$token`.
- **Multi-user**: If the app is deployed on a server with multiple users, session directories should be user-scoped. Consider `data/sessions/{user_email_hash}/{session_id}/`.
- **Cleanup**: Old sessions accumulate on disk. Add a `cleanup_old_sessions(max_age_days = 30)` function called on app start.
- **Security**: Session data may contain sensitive specimen data. Ensure session directories are not web-accessible. Consider encryption if deployed on shared infrastructure.
- **`onSessionEnded` change**: Currently destroys state (`state$reset_state()`). This must be changed to save state instead. The reset should only happen when the user explicitly starts a new session.
- **Specimen data size**: The `nuc` column (DNA sequences) can be very large. Consider storing it in a separate RDS file or offering an option to exclude sequences from the session save.
- **Concurrency**: SQLite supports concurrent reads but only one writer. For single-user or low-concurrency use this is fine. For multi-user deployment, consider WAL mode: `dbExecute(con, "PRAGMA journal_mode=WAL")`.

### Session Lifecycle (with persistence)

```
┌─ NEW SESSION ──────────────────────────────────────────────┐
│                                                            │
│  1. App start                                              │
│  2. Check for saved sessions → show resume panel if found  │
│  3a. User starts fresh → normal flow                       │
│  3b. User resumes → load RDS + SQLite state                │
│                                                            │
├─ WORKING SESSION ──────────────────────────────────────────┤
│                                                            │
│  4. Data imported → save specimen_data.rds + session meta  │
│  5. BAGS graded → save bags_grades.rds                     │
│  6. Each annotation → immediate SQLite write               │
│  7. Periodic timer → snapshot full state                   │
│                                                            │
├─ SESSION INTERRUPTED ──────────────────────────────────────┤
│                                                            │
│  8a. Browser closed → onSessionEnded saves final state     │
│  8b. Internet drop → last auto-save preserved              │
│  8c. Server crash → last auto-save preserved               │
│                                                            │
├─ SESSION RESUMED ──────────────────────────────────────────┤
│                                                            │
│  9. User returns → picks session from list                 │
│  10. Load RDS files into StateManager                      │
│  11. Load annotations from SQLite                          │
│  12. Continue working                                      │
│                                                            │
└────────────────────────────────────────────────────────────┘
```
