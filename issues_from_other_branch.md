# BOLDcuratoR Bug Fix Report
## 1. Gap Analysis Case Sensitivity
Issue: perform_gap_analysis() in mod_species_analysis_utils.R uses exact case-sensitive matching (specimen_data$species == taxon). Searching "baetis rhodani" retrieves data from BOLD correctly (import title-cases genus), but the gap analysis compares raw user input against specimen species names and reports "Missing".

Resolution: In mod_species_analysis_utils.R, change the match to tolower(specimen_data$species) == tolower(taxon). Also change matched_species to use exact$species[1] (the actual species name from data) instead of taxon (the user's input).

## 2. Session Save Crashes — Can't Access Reactive Values in onSessionEnded
Issue: session$onSessionEnded fires AFTER the Shiny session is destroyed. Calling isolate(state$get_store()) and then accessing store$specimen_data throws "Can't access reactive value 'specimen_data' outside of reactive consumer". Sessions were never saved.

Resolution: In app.R, create a plain (non-reactive) environment session_snapshot <- new.env(parent = emptyenv()). Add an observe() that continuously snapshots all needed state keys (specimen_data, user_info, bags_grades, bin_analysis, selected_specimens, specimen_flags, specimen_curator_notes, search_taxa) into session_snapshot$store as a plain list. In onSessionEnded, read from session_snapshot$store instead of reactive values. save_session_state() works with plain lists since R's $ accessor works on lists. Also remove state$reset_state() from onSessionEnded (redundant — session is already gone).

## 3. Session Resume — sendInputMessage Doesn't Work for Module-to-Parent Communication
Issue: mod_data_import_server uses session$sendInputMessage("resume_session", list(value = session_id)) to communicate the selected session ID to app.R. For an actionButton, sendInputMessage triggers a click, making the value an incremented click count (a number), not the session ID string. The app.R handler then receives a number, not a session ID.

Resolution: In mod_data_import_server.R, create a reactiveVal(NULL) called selected_session_id. In the observeEvent(input$resume_session, ...), set selected_session_id(session_id) instead of calling sendInputMessage. Return it from the module: list(selected_session_id = selected_session_id). In app.R, capture the return: data_import <- mod_data_import_server(...), then replace observeEvent(input$'data_import-resume_session', ...) with observeEvent(data_import$selected_session_id(), ...).

## 4. Annotations NEVER Reach the StateManager — Four Bugs in the JS/R Chain
This is the root cause of annotations not appearing in the specimen table, curated exports showing "no curated records", and session saves having empty annotation lists.

### 4a. DT Row Data is Array-Based, Not Object-Based
Issue: DT::datatable(..., rownames = FALSE) makes row.data() return an array like ["PROCESSID123", "Sialis", ...], not a named object. Every JS callback checks if (!data?.processid) return — data.processid is always undefined on an array, so every handler exits immediately. Shiny.setInputValue is never called.

Resolution: In get_table_callback() in table_utils.R, at callback init time, build a column-name-to-index map:

var _colMap = {};
table.columns().every(function(idx) {
  var name = $(this.header()).text().trim();
  _colMap[name] = idx;
});

Add helper functions rowObj(rowApi) (converts array row to named object) and setField(rowApi, field, value) (sets a field by column name on the array). Replace all row.data() / data.processid usage with these helpers throughout the callback.

### 4b. notifyShiny Uses Non-Existent data-processid Selector
Issue: notifyShiny does table.row('[data-processid=...]').data(). DT doesn't add data-processid attributes to TR elements. Always returns null; function exits early without calling Shiny.setInputValue.

Resolution: Change notifyShiny to accept rowData as a fourth parameter (the event handler already has the row data from rowObj()). Build the payload directly from the passed-in rowData instead of trying to re-fetch it. Update all call sites to pass data as the fourth argument.

### 4c. Curator Notes Column Has No Input Element
Issue: The interactive columnDefs for curator_notes in format_specimen_table() has no render function — it displays plain text. The JS handler listening for change on input.specimen-notes has nothing to bind to. Notes can never be captured.

Resolution: Add a render function to the curator_notes columnDef in interactive mode:

render: JS("function(data, type, row) {
  if (type === 'display') {
    var escaped = (data || '').toString().replace(/\"/g, '&quot;');
    return '<input type=\"text\" class=\"specimen-notes form-control form-control-sm\"' +
           ' value=\"' + escaped + '\" maxlength=\"1000\">';
  }
  return data;
}")

Also disable DT's built-in editable feature (change if (!read_only) to if (FALSE)) — it doesn't work for tables rendered inside renderUI (which is how BAGS tables are created).

### 4d. R Handler Key Mismatch for Curator Notes
Issue: JS payload sends {curator_notes: value} but the R observeEvent(input$specimen_notes, ...) handler in mod_bags_grading_server.R reads note$notes (undefined). Even if JS had worked, the note text would be NULL and the annotation discarded.

Resolution: In mod_bags_grading_server.R, change note$notes to note$curator_notes (3 occurrences in the handler).

## 5. Specimen Table Doesn't Show Annotations from BAGS Tables
Issue (column mismatch): The replaceData observer in mod_specimen_handling_server.R calls prepare_module_data() but not order_columns(). The initial renderDT goes through format_specimen_table() which calls order_columns(). Column positions don't match, so replaceData puts data in wrong columns.

Issue (re-render conflicts): renderDT reads store$selected_specimens etc. without isolate(), creating reactive dependencies. It does a full re-render on every annotation change, conflicting with the replaceData observer and destroying search/pagination state.

Issue (stale data on tab switch): The replaceData observer used isolate(rv$filtered_data), so it didn't fire when filtered data changed — only on annotation changes. If the user visited the Specimens tab after annotating in BAGS, the table could show stale data.

Resolution:

Add prepared <- order_columns(prepared) before DT::replaceData() in the observer.
In renderDT, wrap annotation reads with isolate(): isolate(store$selected_specimens), isolate(store$specimen_flags), isolate(store$specimen_curator_notes).
In the replaceData observer, change data <- isolate(rv$filtered_data) to data <- rv$filtered_data (reactive, not isolated) so it fires both on annotation changes AND data changes.

## 6. DT Built-in Export Buttons Export HTML
Issue: The DT CSV/Excel export buttons use type === 'display' by default, which returns HTML-wrapped values from render functions (<div class="cell-content">...</div>).

Resolution: In format_specimen_table() in table_utils.R, add orthogonal = "filter" to the exportOptions for both CSV and Excel buttons. This makes exports use the raw data (the render functions return unformatted data for non-display types).

Also add HTML stripping in merge_annotations_for_export() as a safety net for all R-side TSV/Excel downloads:

strip_html <- function(x) gsub("<[^>]+>", "", x)
for (col in names(data)) {
  if (is.character(data[[col]])) data[[col]] <- strip_html(data[[col]])
}

## 7. New Feature: Curated Record Export
What: "Download Curated Records (TSV)" button in the Specimens tab. Exports only records with any curation annotation (selected, flagged, or noted) in a focused TSV with columns: processid, sampleid, species, selected, flag, curator_notes, flag_user, flag_timestamp.

Where: Add downloadButton(ns("download_curated"), ...) in mod_specimen_handling_ui.R. Add output$download_curated <- downloadHandler(...) in mod_specimen_handling_server.R. The handler reads from store$specimen_data, collects process IDs from names(selections) + names(flags) + names(notes), filters, calls merge_annotations_for_export(), selects the focused columns, and writes TSV.