# BOLDcuratoR Implementation Plan

## Status Key
- [ ] Not started
- [x] Complete
- [~] In progress

---

## 1. BAGS Tables: Fix curator_notes inline editing (no box resize)
**Status:** [x] Complete
**Commit:** `b2e0f62` — Fix curator_notes input to stay inline when focused
**What was done:** Removed height expansion (60px), absolute positioning, and z-index from `.specimen-notes:focus` CSS. Input now stays at 20px and types inline.

---

## 2. BAGS Tables: Remove all download options
**Status:** [x] Complete
**Commit:** `c6c9ea7` — Remove all download options from BAGS grade tables
**What was done:**
- Removed Download Data button from UI
- Removed DT Buttons (copy/csv/excel) by passing empty buttons list and removing 'B' from dom
- Removed downloadHandler from server

---

## 3. Specimen Table: Fix downloads exporting HTML instead of TSV
**Status:** [x] Complete
**Commit:** `ea229e1` — Fix specimen downloads to serve as TSV not HTML
**What was done:** Added `contentType = "text/tab-separated-values"` to all three downloadHandler calls (download_filtered, download_selected, download_annotated).

---

## 4. Specimen Table: Fix/remove DT copy/csv/excel buttons
**Status:** [x] Complete
**Commit:** `b37ff4a` — Remove DT copy/csv/excel buttons from specimen records table
**What was done:** Removed DT Buttons and 'B' from dom string. Table already has dedicated download buttons via downloadHandler.

---

## 5. Specimen Table: Fix curator_notes/flags not transferring from BAGS tables
**Status:** [x] Complete
**Commit:** `c325193` — Fix annotation transfer from BAGS tables to specimen records
**Root cause:** The `renderDT` in specimen_handling used `isolate()` on annotation reads, preventing reactive dependency. When annotations changed in BAGS tabs, the replaceData() observer fired but silently failed on unrendered DT proxy. When the user navigated to Specimens tab, renderDT didn't re-fire because its only dependency (rv$filtered_data) hadn't changed.
**Fix:** Removed `isolate()` from annotation reads in renderDT so it takes reactive dependencies on specimen_flags, specimen_curator_notes, and selected_specimens. Shiny's suspendWhenHidden defers re-render until tab is active.

---

## 6. Remove Export History tab
**Status:** [x] Complete
**Commit:** `d04e973` — Remove Export History tab
**What was done:** Removed source imports, sidebar menu item, and tab content. Module files remain in repo.

---

## 7. Remove sidebar collapse/hide feature
**Status:** [x] Complete
**Commit:** `21fef7d` — Hide sidebar collapse toggle button
**What was done:** Added CSS `.sidebar-toggle { display: none !important; }`.

---

## 8. Reorder tabs
**Status:** [x] Complete
**Commit:** `b0bf6dd` — Reorder sidebar tabs per user request
**New order:** User Info (top), Data Input, Species Analysis, BIN Analysis, Specimens, BAGS Grade A-E, About.

---

## 9. Data Input: Scope session resume to current user
**Status:** [x] Complete
**Commit:** `39d4ccd` — Scope session resume to current user only
**What was done:** Changed `filter_sessions_by_user()` to return empty data frame when no user identifiers provided, instead of showing all sessions.

---

## 10. User Information: Multiple fixes
**Status:** [x] Complete (10a, 10b, 10c), [ ] Deferred (10d)
**Commit:** `a43a185` — User Info: remove ORCID, fix button flash, compact spacing
**What was done:**
- 10a: Button starts hidden via `display:none` in HTML; server shows it only if fallback key exists
- 10b: Fixed form-control height from 5px to 28px, font-size 12px on inputs, 11px on labels, margins 2px
- 10c: Removed ORCID text input and all ORCID validation/handling from server
- 10d: Email results feature deferred — requires email infrastructure (sendmailR/blastula)

---

## 11. BIN Analysis: UI fixes
**Status:** [x] Complete
**Commit:** `00e8851` — BIN Analysis: remove tab label, truncate countries column
**What was done:**
- 11a: Replaced tabBox/tabPanel with plain div (removes "BIN Content Details" label)
- 11b: Added JS columnDefs to truncate countries at 50 chars with hover tooltip

---

## 12. Species Analysis: UI fixes
**Status:** [x] Complete
**Commit:** `ef3841c` — Fix BAGS grade colors (D=grey, E=red) and truncate long columns
**What was done:**
- 12a: Swapped D/E colors everywhere — D=#6c757d (grey), E=#dc3545 (red)
- 12b: Added JS column truncation for bin_uris and countries (50 chars + hover tooltip)

---

## 13. Consistency: Grade colors updated everywhere
**Status:** [x] Complete (included in commit `ef3841c`)
- format_bags_grade_table() in table_utils.R ✓
- Species Analysis checklist in mod_species_analysis_server.R ✓
- BAGS module UI status boxes in mod_bags_grading_ui.R ✓
- BAGS value box colors in mod_bags_grading_server.R ✓

---

## Remaining / Deferred
- [ ] **Email results feature (10d):** Requires email infrastructure. Consider `sendmailR` or `blastula` package. Would add "Email Results" button to User Info or export section.
