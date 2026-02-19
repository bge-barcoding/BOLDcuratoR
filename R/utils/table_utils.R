# R/utils/table_utils.R
#
# DT table formatting and display. Annotation logic (extract_annotation,
# prepare_module_data, merge_annotations_for_export, etc.) lives in
# annotation_utils.R for clarity and testability.

# Source annotation utilities (defines extract_annotation, get_flag_options,
# order_columns, prepare_module_data, merge_annotations_for_export)
if (!exists("extract_annotation")) {
  source("R/utils/annotation_utils.R")
}

#' Format specimen table with standardized styling and interactive elements
#'
#' @param data Data frame of specimen data
#' @param ns Namespace function for Shiny
#' @param buttons Vector of export button types
#' @param page_length Number of rows per page
#' @param selection Selection type ('none', 'single', 'multiple')
#' @param color_by Optional column for color coding
#' @param current_selections Current selections list
#' @param current_flags Current flags list
#' @param current_notes Current curator notes list
#' @param logger Optional logger instance
#' @param dom DT dom option string controlling which elements are displayed
#' @param read_only If TRUE, render annotation columns as plain text instead of interactive widgets
#' @return DT datatable object or NULL if processing fails
#' @export
format_specimen_table <- function(data, ns = NULL,
                                  buttons = c('copy', 'csv', 'excel'),
                                  page_length = 50,
                                  selection = 'none',
                                  color_by = NULL,
                                  current_selections = NULL,
                                  current_flags = NULL,
                                  current_notes = NULL,
                                  logger = NULL,
                                  dom = 'Bfrtip',
                                  read_only = FALSE,
                                  scroll_y = "500px") {

  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty input data")
    return(NULL)
  }

  # Ensure data is a data frame and reset rownames
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  rownames(data) <- NULL

  # Ensure annotation columns exist with proper types (single pass)
  data$selected <- if (!is.null(data$selected)) as.logical(data$selected) else FALSE
  data$flag <- if (!is.null(data$flag)) as.character(data$flag) else ""
  data$curator_notes <- if (!is.null(data$curator_notes)) as.character(data$curator_notes) else ""

  # Order the columns properly
  data <- order_columns(data)

  # Updated DT options
  dt_options <- list(
    scrollX = TRUE,
    scrollY = scroll_y,
    scrollCollapse = TRUE,
    pageLength = page_length,
    autoWidth = FALSE,
    dom = dom,
    buttons = list(
      list(
        extend = 'csv',
        exportOptions = list(
          columns = ':visible',
          rows = ':all',
          orthogonal = 'filter'
        )
      ),
      list(
        extend = 'excel',
        exportOptions = list(
          columns = ':visible',
          rows = ':all',
          orthogonal = 'filter'
        )
      ),
      'copy'
    ),
    extensions = c('FixedColumns', 'Buttons'),
    fixedColumns = list(
      left = 3
    ),
    # Add server-side processing parameters
    processing = FALSE,
    serverSide = FALSE, # Force client-side processing
    # Add search configuration
    search = list(
      regex = FALSE,
      caseInsensitive = TRUE,
      search = ""
    ),

    columnDefs = if (read_only) {
      # Read-only mode: plain text for annotation columns
      list(
        # Selected column — display checkmark or empty
        list(
          targets = which(names(data) == "selected") - 1,
          width = "40px",
          className = 'dt-center fixed-col',
          orderable = TRUE,
          searchable = FALSE,
          render = JS("
            function(data, type, row) {
              if (type === 'display') return data === true ? '\u2713' : '';
              return data;
            }
          ")
        ),
        # Flag column — display label text
        list(
          targets = which(names(data) == "flag") - 1,
          width = "100px",
          className = 'dt-center fixed-col',
          orderable = TRUE,
          searchable = TRUE,
          render = JS("
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
          ")
        ),
        # Curator notes — plain text
        list(
          targets = which(names(data) == "curator_notes") - 1,
          width = "150px",
          className = 'dt-body-left fixed-col',
          orderable = FALSE,
          searchable = TRUE
        ),
        # All other columns
        list(
          targets = "_all",
          className = "dt-body-left",
          width = "100px",
          maxWidth = "100px",
          render = JS("
            function(data, type, row) {
              if (type === 'display') {
                return '<div class=\"cell-content\" title=\"' +
                       (data || '').toString().replace(/\\\"/g, '&quot;') + '\">' +
                       (data || '') + '</div>';
              }
              return data;
            }
          ")
        )
      )
    } else {
      # Interactive mode: checkboxes, dropdowns, editable fields
      list(
        # Selected column
        list(
          targets = which(names(data) == "selected") - 1,
          width = "40px",
          className = 'dt-center fixed-col',
          orderable = FALSE,
          searchable = FALSE,
          render = JS("
        function(data, type, row) {
          if(type === 'display') {
            return '<input type=\"checkbox\" class=\"specimen-select\"' +
                   (data == true ? ' checked' : '') + '>';
          }
          return data;
        }
      ")
        ),
        # Flag column
        list(
          targets = which(names(data) == "flag") - 1,
          width = "100px",
          className = 'dt-center fixed-col',
          orderable = FALSE,
          searchable = FALSE,
          render = JS(sprintf("
      function(data, type, row) {
        if(type === 'display') {
          var flagOptions = %s;
          var select = '<select class=\"specimen-flag form-select form-select-sm\">';
          select += '<option value=\"\">None</option>';
          select += '<option value=\"misidentification\"' + (data === 'misidentification' ? ' selected' : '') + '>Misidentification</option>';
          select += '<option value=\"id_uncertain\"' + (data === 'id_uncertain' ? ' selected' : '') + '>ID Uncertain</option>';
          select += '<option value=\"data_issue\"' + (data === 'data_issue' ? ' selected' : '') + '>Data Issue</option>';
          select += '<option value=\"other_issue\"' + (data === 'other_issue' ? ' selected' : '') + '>Other Issue</option>';
          select += '</select>';
          return select;
        }
        return data;
      }
    ", jsonlite::toJSON(get_flag_options(), auto_unbox = TRUE)))
        ),
        # Curator notes column
        list(
          targets = which(names(data) == "curator_notes") - 1,
          width = "100px",
          className = 'dt-center fixed-col',
          orderable = FALSE,
          searchable = FALSE,
          render = JS("
            function(data, type, row) {
              if (type === 'display') {
                return '<input type=\"text\" class=\"specimen-notes form-control form-control-sm\"' +
                       ' value=\"' + (data || '').toString().replace(/\"/g, '&quot;') + '\"' +
                       ' placeholder=\"Add note...\">';
              }
              return data;
            }
          ")
        ),
        # All other columns
        list(
          targets = "_all",
          className = "dt-body-left",
          width = "100px",
          maxWidth = "100px",
          render = JS("
          function(data, type, row) {
            if (type === 'display') {
              var style = $(this).attr('style') || '';
              return '<div class=\"cell-content\" title=\"' +
                     (data || '').toString().replace(/\\\"/g, '&quot;') + '\" style=\"' +
                     style + '\">' + (data || '') + '</div>';
            }
            return data;
          }
        ")
        )
      )
    }
  )

  # Add callback if namespace provided (skip for read-only — no interactive handlers needed)
  if (!is.null(ns) && !read_only) {
    dt_options$callback <- get_table_callback(ns, get_flag_options())
  }

  # Create base DT object
  tryCatch({
    dt_args <- list(
      data = data,
      options = dt_options,
      selection = 'none',
      rownames = FALSE,
      escape = FALSE,
      extensions = c('Buttons', 'FixedColumns')
    )

    # Note: curator_notes editing is handled via explicit <input> elements
    # rendered in the columnDef, not DT's built-in cell editing.

    dt <- do.call(DT::datatable, dt_args)

    # Add metric column formatting
    if ("quality_score" %in% names(data)) {
      dt <- format_quality_score(dt, data)
    }

    if ("rank" %in% names(data)) {
      dt <- format_rank(dt)
    }

    # Add color coding if specified
    if (!is.null(color_by) && color_by %in% names(data)) {
      dt <- add_color_coding(dt, data, color_by)
    }

    return(dt)

  }, error = function(e) {
    if (!is.null(logger)) {
      logger$error("Error creating DT object", details = list(
        error = e$message,
        data_dims = dim(data),
        data_class = class(data),
        column_names = names(data)
      ))
    }
    return(NULL)
  })
}

# Helper function to format quality score
format_quality_score <- function(dt, data) {
  dt %>% formatStyle(
    'quality_score',
    background = styleColorBar(c(0, max(14, max(data$quality_score, na.rm = TRUE))),
                               "#28a745"),
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  )
}

# Helper function to format rank
format_rank <- function(dt) {
  dt %>% formatStyle(
    'rank',
    backgroundColor = styleEqual(
      c(1:7),
      c('#28a745', '#28a745', '#17a2b8', '#17a2b8',
        '#ffc107', '#ffc107', '#dc3545')
    ),
    color = styleEqual(
      c(1:7),
      rep('white', 7)
    )
  )
}

# Helper function to add color coding
add_color_coding <- function(dt, data, color_by) {
  unique_values <- unique(data[[color_by]])
  colors <- if(length(unique_values) <= 3) {
    c("#e6f3ff", "#cce6ff", "#b3d9ff")
  } else if(length(unique_values) <= 5) {
    c("#e6f3ff", "#cce6ff", "#b3d9ff", "#99ccff", "#80bfff")
  } else {
    colorRampPalette(c("#e6f3ff", "#80bfff"))(length(unique_values))
  }

  dt %>% formatStyle(
    color_by,
    backgroundColor = styleEqual(unique_values, colors[1:length(unique_values)])
  )
}

# NOTE: prepare_module_data, merge_annotations_for_export,
# extract_annotation, get_flag_options, and order_columns are defined in
# R/utils/annotation_utils.R (sourced above).

#' Get table callback JavaScript
#' @param ns Namespace function for Shiny
#' @param flag_options Named list of flag options
#' @return JavaScript callback code
#' @keywords internal
get_table_callback <- function(ns, flag_options = NULL) {
  if (is.null(ns)) return(NULL)

  flags <- get_flag_options()
  flag_js <- sprintf("const flagOptions = %s;\n",
                     jsonlite::toJSON(flags, auto_unbox = TRUE))

  JS(sprintf("
    function(table) {
      %s

      // Build column-name-to-index map.  DT with rownames=FALSE returns
      // arrays from row.data(), not named objects.  Without this map,
      // data.processid is always undefined and every handler exits early.
      var _colMap = {};
      table.columns().every(function(idx) {
        var name = $(this.header()).text().trim();
        _colMap[name] = idx;
      });

      // Convert an array row to a named object using _colMap
      var rowObj = function(rowApi) {
        var arr = rowApi.data();
        if (!arr) return null;
        if (!Array.isArray(arr)) return arr;
        var obj = {};
        for (var name in _colMap) {
          obj[name] = arr[_colMap[name]];
        }
        return obj;
      };

      // Set a field value on an array row by column name
      var setField = function(rowApi, field, value) {
        var arr = rowApi.data();
        if (!arr) return;
        if (Array.isArray(arr)) {
          var idx = _colMap[field];
          if (idx !== undefined) {
            arr[idx] = value;
            rowApi.data(arr);
          }
        } else {
          arr[field] = value;
          rowApi.data(arr);
        }
      };

      // The R StateManager is the single source of truth for annotations.
      // This JS layer only handles:
      //   1. Sending user edits to Shiny (notifyShiny)
      //   2. Syncing UI across grade tables on the same page (broadcast)

      var notifyShiny = function(processid, type, value, rowData) {
        if (!rowData) return;
        var payload = {
          processid: processid,
          species: rowData.species || '',
          bin_uri: rowData.bin_uri || '',
          timestamp: Date.now(),
          table_id: table.table().node().id || 'default'
        };
        payload[type] = value;

        if (type === 'flag') {
          Shiny.setInputValue('%s', payload, {priority: 'event'});
        } else if (type === 'curator_notes') {
          Shiny.setInputValue('%s', payload, {priority: 'event'});
        }
      };

      var updateDOM = function($row, changes) {
        if (!$row || !changes) return;
        if (changes.flag !== undefined) {
          $row.find('select.specimen-flag').val(changes.flag);
        }
        if (changes.curator_notes !== undefined) {
          $row.find('input.specimen-notes').val(changes.curator_notes);
        }
      };

      // Broadcast a change to other tables showing the same specimen
      var broadcast = function(processid, type, value) {
        document.dispatchEvent(new CustomEvent('annotationChange', {
          detail: {
            processid: processid,
            type: type,
            value: value,
            sourceTableId: table.table().node().id || 'default'
          }
        }));
      };

      // --- Event handlers ---

      table.on('change', 'select.specimen-flag', function(e) {
        e.stopPropagation();
        var row = table.row($(this).closest('tr'));
        var data = rowObj(row);
        if (!data || !data.processid) return;

        var value = this.value;
        setField(row, 'flag', value);
        notifyShiny(data.processid, 'flag', value, data);
        broadcast(data.processid, 'flag', value);
      });

      table.on('change', 'input.specimen-notes', function(e) {
        e.stopPropagation();
        var row = table.row($(this).closest('tr'));
        var data = rowObj(row);
        if (!data || !data.processid) return;

        var value = this.value.trim();
        setField(row, 'curator_notes', value);
        notifyShiny(data.processid, 'curator_notes', value, data);
        broadcast(data.processid, 'curator_notes', value);
      });

      // Listen for annotation changes broadcast from other tables
      document.addEventListener('annotationChange', function(e) {
        var d = e.detail;
        if (d.sourceTableId === (table.table().node().id || 'default')) return;

        table.rows().every(function() {
          var data = rowObj(this);
          if (data && data.processid === d.processid) {
            setField(this, d.type, d.value);
            updateDOM($(this.node()), { [d.type]: d.value });
          }
        });
      });
    }
  ", flag_js, ns("specimen_flag"), ns("specimen_notes")))
}

#' Get standard table CSS
#' @return CSS string for consistent table styling
#' @export
get_table_css <- function() {
  "
  /* DataTables Core */
  .dataTables_wrapper {
    padding: 0 !important;
    table-layout: fixed !important;
    width: 100% !important;
    line-height: 1 !important;
  }

  .dataTables_scrollBody {
    overflow-x: scroll !important;
    overflow-y: auto !important;
    white-space: nowrap !important;
    max-height: 70vh !important;
  }

  /* Core Table Layout */
  .datatable {
    width: 100% !important;
    margin-bottom: 15px !important;
    border: 1px solid #dee2e6;
    border-collapse: separate;
    border-spacing: 0;
    font-size: 11px !important;
    line-height: 1 !important;
    table-layout: fixed !important;
  }

  /* Fixed Row Heights */
  .datatable tbody tr {
    height: 24px !important;
    max-height: 24px !important;
    min-height: 24px !important;
    line-height: 24px !important;
    white-space: nowrap !important;
  }

  /* Header Style */
  .datatable thead th {
    height: 28px !important;
    max-height: 28px !important;
    padding: 4px !important;
    font-size: 12px !important;
    font-weight: 600;
    background-color: #f8f9fa;
    border-bottom: 2px solid #dee2e6;
    white-space: nowrap !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
    position: sticky !important;
    top: 0 !important;
    z-index: 1 !important;
  }

  /* Cell Style */
  .datatable tbody td {
    height: 24px !important;
    max-height: 24px !important;
    padding: 2px 4px !important;
    line-height: 20px !important;
    vertical-align: middle !important;
    border-top: 1px solid #dee2e6;
    white-space: nowrap !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
    min-width: 100px !important;
    max-width: 100px !important;
  }

  /* Fixed Column Styles */
  .fixed-col {
    background-color: white !important;
    min-width: 100px !important;
    max-width: 100px !important;
  }

  /* Column-specific widths */
  .datatable td.selected-col {
    width: 40px !important;
    min-width: 40px !important;
    max-width: 40px !important;
  }

  .datatable td.flag-col {
    width: 100px !important;
    min-width: 100px !important;
    max-width: 100px !important;
  }

  .datatable td.notes-col {
    width: 100px !important;
    min-width: 100px !important;
    max-width: 100px !important;
  }

  /* Content overflow handling */
  .cell-content {
    max-width: 100px !important;
    white-space: nowrap !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
    display: block !important;
  }

  /* Interactive Elements */
  .specimen-select {
    width: 16px !important;
    height: 16px !important;
    padding: 0 !important;
    margin: 0 auto !important;
    display: block !important;
  }

  .specimen-flag {
    width: 100% !important;
    min-width: 90px !important;
    height: 20px !important;
    padding: 1px 4px !important;
    font-size: 11px !important;
    line-height: 1.2 !important;
    border: 1px solid #ced4da !important;
    border-radius: 4px !important;
    background-color: #fff !important;
  }

  .specimen-flag:focus {
    border-color: #80bdff !important;
    outline: 0 !important;
    box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25) !important;
  }

  .specimen-notes {
    width: 100% !important;
    height: 20px !important;
    font-size: 11px !important;
    line-height: 1.2 !important;
    padding: 1px 4px !important;
    transition: all 0.2s ease-in-out !important;
  }

  .specimen-notes:focus {
    height: 60px !important;
    position: absolute !important;
    z-index: 1000 !important;
    background: white !important;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2) !important;
  }

  /* Scrolling */
  .datatable-scroll {
    overflow-y: auto !important;
    overflow-x: auto !important;
    white-space: nowrap !important;
  }

  .datatable-header {
    position: sticky !important;
    top: 0 !important;
    z-index: 1 !important;
    background-color: #f8f9fa !important;
  }

  /* Row Selection */
  .datatable tbody tr.selected {
    background-color: #e2e6ea !important;
  }

  /* Fixed Columns */
  .DTFC_LeftWrapper {
    border-right: 2px solid #dee2e6 !important;
    background-color: #fff !important;
    z-index: 2 !important;
  }

  .DTFC_LeftHeadWrapper {
    border-bottom: 2px solid #dee2e6 !important;
    z-index: 3 !important;
  }

  .DTFC_LeftBodyWrapper {
    border-right: 2px solid #dee2e6 !important;
    box-shadow: 4px 0px 8px rgba(0,0,0,0.1) !important;
    overflow-y: hidden !important;
  }

  .DTFC_Cloned {
    background-color: white !important;
  }

  .DTFC_LeftHeadWrapper table,
  .DTFC_LeftBodyWrapper table {
    margin-top: 0 !important;
    border-collapse: collapse !important;
  }

  .DTFC_LeftHeadWrapper table thead th {
    background-color: #f8f9fa !important;
    border-bottom: 2px solid #dee2e6 !important;
  }

  .DTFC_LeftBodyLiner {
    overflow-x: hidden !important;
    overflow-y: hidden !important;
  }

  /* Button Styling */
  .dt-buttons {
    margin-bottom: 10px !important;
  }

  .dt-button {
    margin-right: 5px !important;
  }

  /* Shadow effect */
  .DTFC_LeftWrapper::after {
    content: '' !important;
    position: absolute !important;
    top: 0 !important;
    right: 0 !important;
    width: 4px !important;
    height: 100% !important;
    box-shadow: 2px 0 5px rgba(0,0,0,0.1) !important;
  }
  "
}

#' Create species-specific color scheme
#' @param species Vector of species names
#' @return Named vector of colors
#' @keywords internal
create_species_colors <- function(species) {
  if (length(species) == 0) return(NULL)

  unique_species <- unique(species)
  if (length(unique_species) <= 3) {
    colors <- c("#e6f3ff", "#cce6ff", "#b3d9ff")
  } else if (length(unique_species) <= 5) {
    colors <- c("#e6f3ff", "#cce6ff", "#b3d9ff", "#99ccff", "#80bfff")
  } else {
    colors <- colorRampPalette(c("#e6f3ff", "#80bfff"))(length(unique_species))
  }

  setNames(colors[1:length(unique_species)], unique_species)
}

#' Create table container with title and controls
#' @param table DT datatable object
#' @param title Table title
#' @param caption Optional table caption
#' @return UI element
#' @export
create_table_container <- function(table, title, caption = NULL) {
  div(
    class = "table-wrapper mb-4",
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      h4(class = "m-0", title),
      if(!is.null(caption)) div(class = "text-muted", caption)
    ),
    div(class = "table-responsive", table)
  )
}

#' Format BIN summary table
#' @param summary_data Data frame of BIN summary data
#' @export
format_bin_summary_table <- function(summary_data) {
  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(NULL)
  }

  required_cols <- c("bin_uri", "concordant_bins", "bin_coverage")
  display_cols <- intersect(required_cols, names(summary_data))

  datatable(
    summary_data[, display_cols, drop = FALSE],
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "500px",
      fixedHeader = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'concordant_bins',
      backgroundColor = styleInterval(c(0), c('#f8d7da', '#d4edda'))
    ) %>%
    formatPercentage('bin_coverage', digits = 1)
}

#' Format BIN content table
#' @param content_data Data frame of BIN content data
#' @export
format_bin_content_table <- function(content_data) {
  if (is.null(content_data) || nrow(content_data) == 0) {
    return(NULL)
  }

  required_cols <- c("bin_uri", "total_records", "unique_species",
                     "species_list", "countries", "concordance")
  display_cols <- intersect(required_cols, names(content_data))

  datatable(
    content_data[, display_cols, drop = FALSE],
    options = list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "500px",
      fixedHeader = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'concordance',
      backgroundColor = styleEqual(
        c("Concordant", "Discordant"),
        c('#d4edda', '#f8d7da')
      ),
      fontWeight = 'bold'
    )
}

#' Format BIN statistics table
#' @param stats_data Data frame of BIN statistics
#' @export
format_bin_stats_table <- function(stats_data) {
  if (is.null(stats_data)) return(NULL)

  if (is.list(stats_data) && !is.data.frame(stats_data)) {
    stats_df <- as.data.frame(t(unlist(stats_data)))
  } else {
    stats_df <- stats_data
  }

  percent_cols <- grep("percent|coverage|rate", names(stats_df), value = TRUE)
  for (col in percent_cols) {
    if (col %in% names(stats_df)) {
      stats_df[[col]] <- paste0(round(as.numeric(stats_df[[col]]), 1), "%")
    }
  }

  datatable(
    stats_df,
    options = list(
      dom = 't',
      ordering = FALSE,
      fixedHeader = TRUE
    ),
    rownames = FALSE
  ) %>%
    formatRound(
      columns = intersect(c("avg_species_per_bin", "mean_distance"), names(stats_df)),
      digits = 2
    )
}

#' Format history table
#' @param history_data Data frame of selection history
#' @export
format_history_table <- function(history_data) {
  if (is.null(history_data) || nrow(history_data) == 0) {
    return(NULL)
  }

  required_cols <- c("timestamp", "processid", "species", "quality_score",
                     "action", "bin_uri", "selected", "flag")
  display_cols <- intersect(required_cols, names(history_data))

  if (length(display_cols) == 0) {
    return(NULL)
  }

  dt <- datatable(
    history_data[, display_cols, drop = FALSE],
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      scrollY = "500px",
      fixedHeader = TRUE,
      dom = 'Bfrtip',
      ordering = TRUE,
      order = list(list(0, 'desc')), # Sort by timestamp descending
      buttons = c('copy', 'csv'),
      columnDefs = list(
        list(
          targets = which(display_cols == "timestamp") - 1,
          render = JS("function(data, type, row) {
            if (type === 'display') {
              return moment(data).format('YYYY-MM-DD HH:mm:ss');
            }
            return data;
          }")
        ),
        list(
          targets = "_all",
          className = "dt-center"
        )
      )
    ),
    selection = 'none',
    rownames = FALSE,
    class = 'history-table'
  )

  # Add column formatting
  if ("quality_score" %in% display_cols) {
    dt <- dt %>%
      formatStyle(
        'quality_score',
        background = styleColorBar(c(0, 14), '#28a745'),
        backgroundSize = '98% 88%'
      ) %>%
      formatRound('quality_score', digits = 1)
  }

  if ("action" %in% display_cols) {
    dt <- dt %>%
      formatStyle(
        'action',
        backgroundColor = styleEqual(
          c("selected", "deselected", "flagged", "note_added"),
          c('#d4edda', '#f8d7da', '#fff3cd', '#cce5ff')
        )
      )
  }

  if ("selected" %in% display_cols) {
    dt <- dt %>%
      formatStyle(
        'selected',
        backgroundColor = styleEqual(
          c(TRUE, FALSE),
          c('#d4edda', '#f8d7da')
        )
      )
  }

  if ("flag" %in% display_cols) {
    dt <- dt %>%
      formatStyle(
        'flag',
        backgroundColor = function(data) {
          ifelse(data != "", '#fff3cd', '')
        }
      )
  }

  dt
}

#' Format BAGS grade table
#' @param grade_data Data frame of BAGS grades
#' @export
format_bags_grade_table <- function(grade_data) {
  if (is.null(grade_data) || nrow(grade_data) == 0) {
    return(NULL)
  }

  required_cols <- c("species", "bags_grade", "specimen_count",
                     "bin_count", "shared_bins", "bin_coverage")
  display_cols <- intersect(required_cols, names(grade_data))

  datatable(
    grade_data[, display_cols, drop = FALSE],
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      scrollY = "500px",
      fixedHeader = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'bags_grade',
      backgroundColor = styleEqual(
        c("A", "B", "C", "D", "E"),
        c('#28a745', '#17a2b8', '#ffc107', '#dc3545', '#6c757d')
      ),
      color = 'white',
      fontWeight = 'bold'
    ) %>%
    formatStyle(
      'bin_coverage',
      background = styleColorBar(c(0, 100), '#28a745')
    )
}

#' Format export history table
#' @param history_data Data frame of export history
#' @export
format_export_history_table <- function(history_data) {
  if (is.null(history_data) || nrow(history_data) == 0) {
    return(NULL)
  }

  required_cols <- c("timestamp", "export_type", "file_name",
                     "record_count", "file_size", "success")
  display_cols <- intersect(required_cols, names(history_data))

  datatable(
    history_data[, display_cols, drop = FALSE],
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      scrollY = "500px",
      fixedHeader = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    ),
    selection = 'none',
    rownames = FALSE
  ) %>%
    formatStyle(
      'success',
      backgroundColor = styleEqual(
        c(TRUE, FALSE),
        c('#d4edda', '#f8d7da')
      )
    ) %>%
    formatDate('timestamp', method = "toLocaleString") %>%
    formatBytes('file_size')
}

#' Generate table caption
#' @param grade BAGS grade
#' @param group_info Group information list
#' @return Caption string
#' @keywords internal
generate_table_caption <- function(grade, group_info) {
  if (is.null(grade) || is.null(group_info)) {
    return("")
  }

  switch(grade,
         "A" = sprintf("Species: %s (>10 specimens with valid BINs)",
                       group_info$species),
         "B" = sprintf("Species: %s (3-10 specimens with valid BINs)",
                       group_info$species),
         "C" = sprintf("Species: %s - BIN: %s",
                       group_info$species, group_info$bin),
         "D" = sprintf("Species: %s (<3 specimens with valid BINs)",
                       group_info$species),
         "E" = sprintf("Shared BIN: %s (%d species)",
                       group_info$bin, group_info$species_count),
         ""
  )
}

