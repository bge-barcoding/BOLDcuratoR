# BOLDcuratoR Implementation Plan

## Status Key
- [ ] Not started
- [x] Complete
- [~] In progress

---

## 1. BAGS Tables: Fix curator_notes inline editing (no box resize)
**Problem:** The curator_notes text input box resizes (grows to 60px tall) on focus, pushing content around and hiding text while typing.
**Root Cause:** CSS in `R/utils/table_utils.R:633-639` — `.specimen-notes:focus` sets `height: 60px !important; position: absolute !important;` which makes the box jump out of flow and obscure content.
**Fix:**
- [ ] In `R/utils/table_utils.R` lines 624-639, change the `.specimen-notes:focus` CSS to keep the input inline:
  - Remove `height: 60px !important`
  - Remove `position: absolute !important`
  - Remove `z-index: 1000 !important`
  - Keep `background: white` and `box-shadow` for visual focus indicator
  - Keep the base height at `20px` so it stays inline
- **Files:** `R/utils/table_utils.R:624-639`

---

## 2. BAGS Tables: Remove all download options
**Problem:** BAGS tables have copy/csv/excel DT buttons that don't work properly (csv/excel hang, copy useless), plus a `Download Data` button. Downloads should only exist on Species Analysis and BIN Analysis tables.
**Fix:**
- [ ] In `R/modules/bags_grading/mod_bags_grading_ui.R:56-59`, remove the `downloadButton(ns("download_data"), ...)` and its container div
- [ ] In `R/modules/bags_grading/mod_bags_grading_utils.R:198-206`, in `format_grade_table()`, change `buttons = c('copy', 'csv', 'excel')` to `buttons = list()` and change `dom` from `"Bfrtip"/"Bt"` to remove the `B` (buttons) element — use `"frtip"/"t"` respectively
- [ ] In `R/modules/bags_grading/mod_bags_grading_server.R:257-276`, remove the `output$download_data` downloadHandler block entirely
- **Files:** `R/modules/bags_grading/mod_bags_grading_ui.R`, `R/modules/bags_grading/mod_bags_grading_utils.R`, `R/modules/bags_grading/mod_bags_grading_server.R`

---

## 3. Specimen Table: Fix downloads (annotated/selected/filtered) exporting HTML instead of TSV
**Problem:** The download handlers in specimen_handling write data via `write.table()` but the data contains HTML tags from DT rendering (e.g., `<div class="cell-content">...</div>` wrappers and `<input>` elements from interactive columns).
**Root Cause:** The `rv$filtered_data` used in download handlers contains raw data, but `merge_annotations_for_export()` already strips HTML. The issue is that download filenames use `.tsv` extension but the browser may be rendering them as HTML if content-type headers are wrong, OR the data itself has HTML from DT column renderers leaking into the data frame.
**Fix:**
- [ ] In `R/modules/specimen_handling/mod_specimen_handling_server.R:272-353`, verify that download handlers use `rv$filtered_data` (raw data, not DT-rendered data) and `merge_annotations_for_export()` (which already has `strip_html()`)
- [ ] Ensure `content_type` parameter is set in `downloadHandler` calls: add `contentType = "text/tab-separated-values"` to prevent browser rendering as HTML
- [ ] Test that the files are actually TSV and not HTML by checking `write.table()` output
- **Files:** `R/modules/specimen_handling/mod_specimen_handling_server.R:272-353`

---

## 4. Specimen Table: Fix/remove DT copy/csv/excel buttons
**Problem:** The DT Buttons (copy, csv, excel) hang on the specimen records table. The table uses `read_only = TRUE` and has `buttons = c('copy', 'csv', 'excel')`.
**Root Cause:** DT Buttons may conflict with the large dataset + FixedColumns extension + the custom column renderers producing HTML content. The DataTables Buttons export uses the `orthogonal` data source which may not properly resolve when custom JS renderers are in use, causing the browser to hang.
**Fix:**
- [ ] In `R/modules/specimen_handling/mod_specimen_handling_server.R:213`, change `buttons = c('copy', 'csv', 'excel')` to `buttons = list()` to remove DT buttons from the specimen table
- [ ] In `R/utils/table_utils.R:38`, change the default `dom` to not include `B` when buttons are empty — or handle in the calling code
- [ ] The specimen table already has dedicated download buttons (`download_filtered`, `download_selected`, `download_annotated`) which work via proper `downloadHandler` calls, so removing DT buttons is the correct approach
- **Files:** `R/modules/specimen_handling/mod_specimen_handling_server.R`, `R/utils/table_utils.R`

---

## 5. Specimen Table: Fix curator_notes/flags not transferring from BAGS tables
**Problem:** Annotations (flags, curator_notes) entered in BAGS grade tables don't appear in the Specimen Records table.
**Root Cause Analysis:** The architecture *should* work via the StateManager:
1. BAGS tables → JS callback → `Shiny.setInputValue(ns("specimen_flag"/"specimen_notes"))` → `observeEvent` in `mod_bags_grading_server.R:206-253` → `state$update_state("specimen_flags"/"specimen_curator_notes")`
2. Specimen table → `observe()` at `mod_specimen_handling_server.R:32-58` → reads `store$specimen_flags` and `store$specimen_curator_notes` → calls `prepare_module_data()` → `DT::replaceData()`

**Architecture review (StateManager confirmed working):**
The StateManager uses `reactiveValues` (line 196 of `state_manager.R`). `update_state()` does `private$store[[key]] <- converted_value` which triggers Shiny reactive invalidation. The reactive chain SHOULD work:
1. BAGS JS callback → `Shiny.setInputValue(ns("specimen_flag"/"specimen_notes"), payload)` ✓
2. `observeEvent(input$specimen_flag/notes)` in `mod_bags_grading_server.R:206-253` → reads current state, modifies, writes back via `state$update_state()` ✓
3. `observe()` in `mod_specimen_handling_server.R:32-58` → reactive dependency on `store$specimen_flags`/`store$specimen_curator_notes` → `prepare_module_data()` → `DT::replaceData()` ✓

**Likely root cause: `DT::replaceData()` on unrendered proxy.**
If user visits BAGS first, adds annotations, then annotation state updates trigger the `observe()` in specimen_handling. But the DTOutput hasn't been rendered yet (Specimens tab not visited), so `specimen_proxy` references a non-existent DataTable. `replaceData()` silently fails. When user finally visits Specimens tab, `renderDT` fires and reads annotations via `isolate()` — which SHOULD include them. But `isolate()` prevents reactive dependency, so if the read happens at the wrong moment (before state is fully committed), it could miss annotations.

**Investigation & fix plan:**
- [ ] Add diagnostic logging in `mod_bags_grading_server.R:206-253` to confirm flags/notes are written to state (print `current_flags`/`current_notes` after update)
- [ ] Add diagnostic logging in `mod_specimen_handling_server.R:32-58` to confirm the observer fires and what annotations it sees
- [ ] Test scenario: Add annotation in BAGS → switch to Specimens tab → verify annotations appear
- [ ] If annotations appear on first visit but not on subsequent BAGS edits, the issue is `replaceData()` on an active tab — check for DT proxy issues
- [ ] If annotations don't appear on first visit, the issue is in `prepare_module_data()` or `renderDT` — check data flow
- [ ] Potential fix: Force re-render of specimen table when tab becomes active by observing the sidebar tab selection and invalidating `rv$filtered_data`
- [ ] Alternative fix: Use `outputOptions(output, "specimen_table", suspendWhenHidden = FALSE)` to keep the DT rendered even when tab is not visible — BUT this may cause performance issues
- [ ] Another approach: Instead of relying on `replaceData()`, make `renderDT` reactive to annotation changes (remove `isolate()` wrappers) and use `debounce()` to prevent excessive re-renders
- **Files:** `R/modules/bags_grading/mod_bags_grading_server.R:206-253`, `R/modules/specimen_handling/mod_specimen_handling_server.R:32-58`, `R/modules/state/state_manager.R`, `R/utils/table_utils.R:360-486` (JS callback)

---

## 6. Remove Export History tab
**Problem:** Export History tab should be removed entirely.
**Fix:**
- [ ] In `app.R:49-51`, remove the `source()` lines for export_history modules
- [ ] In `app.R:75`, remove `menuItem("Export History", ...)` from sidebar
- [ ] In `app.R:180-181`, remove `tabItem(tabName = "export_history", ...)` from tabItems
- [ ] Optionally remove the module files themselves: `R/modules/export_history/mod_export_history_ui.R`, `R/modules/export_history/mod_export_history_server.R`
- **Files:** `app.R:49-51, 75, 180-181`

---

## 7. Remove sidebar collapse/hide feature
**Problem:** The left sidebar with tab options can be hidden/collapsed, which the user wants removed.
**Root Cause:** shinydashboard provides a built-in sidebar toggle button in the header (the hamburger menu icon). This is part of `dashboardHeader()` and can be disabled.
**Fix:**
- [ ] In `app.R:60`, add `disable = TRUE` to `dashboardHeader()` to remove the sidebar toggle: `dashboardHeader(title = "BOLDcuratoR", .list = list(tags$li(class = "dropdown")))` or use `dashboardHeader(title = "BOLDcuratoR", disable = FALSE)` — actually the toggle is controlled by a different mechanism
- [ ] Add CSS to hide the sidebar toggle button: `.sidebar-toggle { display: none !important; }` in the `tags$head(tags$style(...))` block in `app.R:86-149`
- [ ] Alternatively, add `shinydashboard::dashboardHeader(title = "BOLDcuratoR")` with a custom CSS `.sidebar-toggle { display: none !important; }`
- **Files:** `app.R:86-149`

---

## 8. Reorder tabs
**Problem:** Current order: Data Input, BIN Analysis, Specimens, BAGS A-E, Species Analysis, Export History, About
**Required order:** User Info (already at top), Data Input, Species Analysis, BIN Analysis, Bags A-E, About
**Fix:**
- [ ] In `app.R:65-77`, reorder the `menuItem()` calls in `sidebarMenu()` to:
  1. `menuItem("Data Input", tabName = "input", ...)`
  2. `menuItem("Species Analysis", tabName = "species_analysis", ...)`
  3. `menuItem("BIN Analysis", tabName = "bins", ...)`
  4. `menuItem("BAGS Grade A", ...)` through `menuItem("BAGS Grade E", ...)`
  5. `menuItem("About", tabName = "about", ...)`
  - Remove "Specimens" and "Export History" menu items
- [ ] In `app.R:152-187`, reorder `tabItems()` to match
- [ ] Note: "Specimens" tab is being removed from the sidebar since it's mentioned nowhere in the new order. Confirm with user whether Specimens tab should be accessible or removed. **Decision needed:** Keep Specimens accessible but just not in sidebar? Or remove entirely? (The Specimens tab shows the read-only view of all specimens with annotations. The user listed it as having download issues but not removal.)
  - **Assumption:** Keep the Specimens tab but move it after BIN Analysis and before BAGS grades, since the user's list says "leave it on specimen table, species analysis and bin analysis tables only" (referring to downloads). The new order would be: Data Input, Species Analysis, BIN Analysis, Specimens, Bags A-E, About.
- **Files:** `app.R:65-77, 152-187`

---

## 9. Data Input: Scope session resume to current user
**Problem:** Multiple users of the shiny app can see each other's sessions when using Resume Previous Session.
**Current behavior:** `filter_sessions_by_user()` in `R/utils/session_persistence.R:174-200` already filters by user email/ORCID/name. But if no identifiers are provided (user hasn't filled in User Info yet), it falls back to showing ALL sessions (line 184).
**Fix:**
- [ ] In `R/utils/session_persistence.R:184`, instead of returning all sessions when no identifiers provided, return an empty data frame. Change `if (!has_email && !has_orcid && !has_name) return(all_sessions)` to `if (!has_email && !has_orcid && !has_name) return(empty_df)` — but we need to define `empty_df` or use `all_sessions[0, , drop = FALSE]`
- [ ] Actually, `filter_sessions_by_user` doesn't have access to `empty_df`. Change line 184 to: `if (!has_email && !has_orcid && !has_name) return(all_sessions[0, , drop = FALSE])`
- [ ] In `R/modules/data_import/mod_data_import_server.R:29`, update the "No saved sessions" message to indicate the user should fill in their details first: already says "Enter your details in the User Info panel to find previous sessions." — this is correct
- **Files:** `R/utils/session_persistence.R:184`

---

## 10. User Information: Multiple fixes
### 10a. Fix API key test button flash
**Problem:** "Use shared key" button appears briefly before being hidden.
**Root Cause:** In `mod_user_info_server.R:13-19`, the observe that hides the button uses `bindEvent(TRUE, once = TRUE)` which fires after the initial render. The button is visible in the UI HTML before the server hides it.
**Fix:**
- [ ] In `mod_user_info_ui.R:93-97`, add `style = "display: none;"` to the `actionButton(ns("use_shared_key"), ...)` so it starts hidden
- [ ] In `mod_user_info_server.R:13-19`, change `shinyjs::hide` to `shinyjs::show` — flip the logic so it only shows the button if a fallback key exists (button starts hidden, show only if key found)
- **Files:** `R/modules/user/mod_user_info_ui.R:93-97`, `R/modules/user/mod_user_info_server.R:13-19`

### 10b. More compact spacing
**Problem:** Boxes are spaced too far apart.
**Fix:**
- [ ] In `R/modules/user/mod_user_info_ui.R:11-45`, adjust CSS to be more aggressive — the current CSS uses `height: 5px !important` for `.form-control` which is likely too small (the input is only 5px tall). This needs balancing:
  - Set `.form-control` height to `28px` (reasonable for text input)
  - Set `.form-group` margin to `2px 0`
  - Remove excessive padding on `.box-body` and `.shiny-input-container`
  - Set labels to `margin-bottom: 1px; font-size: 11px;`
- **Files:** `R/modules/user/mod_user_info_ui.R:11-45`

### 10c. Remove ORCID field
**Fix:**
- [ ] In `R/modules/user/mod_user_info_ui.R:68-77`, remove the `textInput(ns("orcid"), ...)` block
- [ ] In `R/modules/user/mod_user_info_server.R:48-52`, remove ORCID validation
- [ ] In `R/modules/user/mod_user_info_server.R:92-97`, remove `orcid = input$orcid` from the `state$update_state("user_info", ...)` call
- [ ] In `R/modules/user/mod_user_info_server.R:145-147`, remove ORCID restore from state
- [ ] In `validate_user_info()` at `mod_user_info_server.R:167-201`, remove ORCID references
- [ ] Note: Session persistence still saves/loads `user_orcid` — keep that for backward compatibility but don't collect it anymore
- **Files:** `R/modules/user/mod_user_info_ui.R`, `R/modules/user/mod_user_info_server.R`

### 10d. Option to email user their results
**Fix:**
- [ ] This is a feature request that requires email infrastructure. Options:
  1. Add a "Email Results" button that uses `sendmailR` or `blastula` package to send results as email attachment
  2. Add an email field (already exists) and a button that triggers an export + email flow
- [ ] This is a larger feature — mark as deferred/separate ticket. For now, add a placeholder button that shows a notification saying "Email feature coming soon" or implement basic email if `sendmailR`/`blastula` is available
- **Files:** `R/modules/user/mod_user_info_ui.R`, `R/modules/user/mod_user_info_server.R`, new dependency

---

## 11. BIN Analysis: UI fixes
### 11a. Remove "BIN Content Details" tab label
**Problem:** The tab label "BIN Content Details" is unnecessary since it's the only tab.
**Fix:**
- [ ] In `R/modules/bin_analysis/mod_bin_analysis_ui.R:29-40`, replace `tabBox()` with just a plain `div()` containing the DTOutput. Remove the `tabPanel("BIN Content Details", ...)` wrapper.
- **Files:** `R/modules/bin_analysis/mod_bin_analysis_ui.R:29-40`

### 11b. Truncate countries column with hover
**Problem:** Countries list is long, causing the table to scroll horizontally.
**Fix:**
- [ ] In `R/utils/table_utils.R` — `format_bin_content_table()` (lines 790-819), add a `columnDefs` entry for the "countries" column with a JS render function that truncates to ~50 chars with `...` and puts the full value in a `title` attribute for hover tooltip
- [ ] Alternatively, add CSS `max-width: 200px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;` targeting the countries column
- **Files:** `R/utils/table_utils.R:790-819`

---

## 12. Species Analysis: UI fixes
### 12a. Fix grade colors (E=red, D=grey)
**Problem:** Currently D=red (#dc3545) and E=gray (#6c757d). The user says E is worse than D, so E should be red and D should be grey.
**Fix:**
- [ ] In `R/modules/species_analysis/mod_species_analysis_server.R:110-119`, swap the colors:
  - Change D from `#dc3545` (red) to `#6c757d` (gray)
  - Change E from `#6c757d` (gray) to `#dc3545` (red)
- [ ] Also update in `R/utils/table_utils.R:974-977` (`format_bags_grade_table()`):
  - Change D from `#dc3545` to `#6c757d`
  - Change E from `#6c757d` to `#dc3545`
- [ ] Also update in `R/modules/bags_grading/mod_bags_grading_ui.R:23-24`:
  - Change D status from `"danger"` to match grey (use `"default"` or custom)
  - Change E status from `"danger"` to `"danger"` (keep red)
- **Files:** `R/modules/species_analysis/mod_species_analysis_server.R:110-119`, `R/utils/table_utils.R:974-977`, `R/modules/bags_grading/mod_bags_grading_ui.R:23-24`

### 12b. Truncate bin_uris and countries columns
**Problem:** Long semicolon-separated lists make the table scroll horizontally.
**Fix:**
- [ ] In `R/modules/species_analysis/mod_species_analysis_server.R:95-128`, add `columnDefs` to the `DT::datatable()` options that applies truncation with hover tooltip for `bin_uris` and `countries` columns:
  ```javascript
  render = JS("function(data, type, row) {
    if (type === 'display' && data && data.length > 50) {
      return '<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>';
    }
    return data;
  }")
  ```
- **Files:** `R/modules/species_analysis/mod_species_analysis_server.R:95-128`

---

## 13. Consistency: Update grade colors everywhere
After swapping D and E colors, ensure consistency across:
- [ ] `format_bags_grade_table()` in `table_utils.R:974-977`
- [ ] Species Analysis checklist in `mod_species_analysis_server.R:110-119`
- [ ] BAGS module UI status boxes in `mod_bags_grading_ui.R:19-25`
- [ ] BAGS value box colors in `mod_bags_grading_server.R:195-202`

---

## Implementation Order (recommended)
1. **Quick wins first:** Remove Export History tab (#6), reorder tabs (#8), remove sidebar collapse (#7)
2. **BAGS tables:** Remove downloads (#2), fix curator_notes CSS (#1)
3. **Species/BIN Analysis:** Fix grade colors (#12a, #13), truncate columns (#11b, #12b), remove tab label (#11a)
4. **User Info:** Remove ORCID (#10c), fix button flash (#10a), compact spacing (#10b)
5. **Session resume:** Scope to user (#9)
6. **Specimen table:** Fix/remove DT buttons (#4), fix download formats (#3)
7. **Hard problem:** Debug annotation transfer (#5) — requires StateManager investigation
8. **Deferred:** Email results feature (#10d)
