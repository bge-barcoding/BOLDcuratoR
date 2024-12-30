# R/utils/table_utils.R

#' Get standard table columns
#' @return Vector of standard column names
#' @keywords internal
get_standard_columns <- function() {
  c("selected", "flag", "curator_notes",
    "specimen_rank", "quality_score", "processid", "bin_uri",
    "species", "identification", "identified_by",
    "identification_method", "country.ocean",
    "collection_date_start", "collectors", "inst",
    "criteria_met")
}

#' Get available flag options
#' @return Named list of flag options
#' @export
get_flag_options <- function() {
  c(
    "None" = "",
    "Misidentification" = "misidentification",
    "ID Uncertain" = "id_uncertain",
    "Data Issue" = "data_issue",
    "Quality Issue" = "quality_issue",
    "Other Issue" = "other_issue"
  )
}

#' Format specimen table with standardized styling
#' @param data Data frame of specimen data
#' @param ns Namespace function for Shiny
#' @param buttons Vector of export button types
#' @param page_length Number of rows per page
#' @param selection Selection type
#' @param color_by Optional column for color coding
#' @param current_selections List of currently selected specimens
#' @param current_flags List of current specimen flags
#' @param current_notes List of current curator notes
#' @param logger Optional logger instance
#' @export
format_specimen_table <- function(data, ns = NULL,
                                  buttons = c('copy', 'csv', 'excel'),
                                  page_length = 50,
                                  selection = 'none',
                                  color_by = NULL,
                                  current_selections = NULL,
                                  current_flags = NULL,
                                  current_notes = NULL,
                                  logger = NULL) {
  if (!is.null(logger)) {
    logger$info("Formatting specimen table", list(
      rows = nrow(data),
      columns = names(data)
    ))
  }

  # Get standard columns
  cols <- get_standard_columns()

  # Prepare data with interactive columns
  data <- prepare_table_data(data, cols, current_selections,
                             current_flags, current_notes)

  # Get flag options for dropdown
  flag_options <- get_flag_options()
  flag_js <- sprintf(
    "var flagOptions = %s;",
    jsonlite::toJSON(flag_options, auto_unbox = TRUE)
  )

  # DT options
  options <- list(
    scrollX = TRUE,
    scrollY = "500px",
    pageLength = page_length,
    autoWidth = FALSE,
    dom = 'Bfrtip',
    buttons = buttons,
    fixedColumns = list(leftColumns = 3),
    columnDefs = list(
      list(
        targets = 0:2,
        searchable = FALSE,
        orderable = FALSE,
        width = "100px",
        className = 'dt-center'
      ),
      list(
        targets = "_all",
        className = "dt-body-left nowrap",
        height = "22px"
      ),
      list(
        targets = which(cols == "criteria_met") - 1,
        width = "200px",
        render = JS("
        function(data, type, row) {
          if (type === 'display') {
            return '<div class=\"criteria-content\" title=\"' +
                   (data || '') + '\">' + (data || '') + '</div>';
          }
          return data;
      }")
      )
    )
  )

  # Add callback if namespace provided
  if (!is.null(ns)) {
    options$callback <- get_table_callback(ns, flag_options)
  }

  # Create base table
  dt <- datatable(
    data,
    options = options,
    selection = selection,
    rownames = FALSE,
    escape = FALSE
  )

  # Add standard formatting
  dt <- format_table_columns(dt, data, color_by = NULL)

  dt
}

#' Prepare table data with interactive columns
#' @param data Input data frame
#' @param cols Required columns
#' @param current_selections Current selections
#' @param current_flags Current flags
#' @param current_notes Current curator notes
#' @param logger Optional logger instance
#' @return Prepared data frame
#' @keywords internal
prepare_table_data <- function(data, cols, current_selections, current_flags,
                               current_notes, logger = NULL) {

  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty or invalid input data")
    return(data.frame())
  }

  # Add selected column
  if (!"selected" %in% names(data)) {
    data$selected <- sapply(data$processid, function(pid) {
      !is.null(current_selections) && !is.null(current_selections[[pid]])
    })
  }

  # Add flag dropdown column
  if (!"flag" %in% names(data)) {
    data$flag <- sapply(data$processid, function(pid) {
      if (!is.null(current_flags) && !is.null(current_flags[[pid]])) {
        current_flags[[pid]]$flag
      } else {
        ""
      }
    })
  }

  # Add curator notes column
  if (!"curator_notes" %in% names(data)) {
    data$curator_notes <- sapply(data$processid, function(pid) {
      if (!is.null(current_notes) && !is.null(current_notes[[pid]])) {
        current_notes[[pid]]
      } else {
        ""
      }
    })
  }

  # Add any missing columns
  for (col in setdiff(cols, names(data))) {
    data[[col]] <- NA
  }

  # Reorder columns to match preferred order
  data[, cols]
}

#' Format table columns with styling
#' @param dt DT datatable object
#' @param data Original data frame
#' @param color_by Optional column for color coding
#' @param logger Optional logger instance
#' @return Formatted datatable
#' @keywords internal
format_table_columns <- function(dt, data, color_by = NULL, logger = NULL) {

  # Format selected column as checkbox
  dt <- dt %>%
    formatStyle(
      'selected',
      target = "cell",
      render = JS("
        function(data, type, row) {
          if(type === 'display') {
            return '<input type=\"checkbox\" class=\"specimen-select\"' +
                   (data ? ' checked' : '') + '>';
          }
          return data;
        }
      ")
    )

  # Format flag column as dropdown using defined options
  dt <- dt %>%
    formatStyle(
      'flag',
      target = "cell",
      render = JS("
        function(data, type, row) {
          if(type === 'display') {
            var select = '<select class=\"specimen-flag form-select form-select-sm\">';
            Object.entries(flagOptions).forEach(function([label, value]) {
              select += '<option value=\"' + value + '\"' +
                       (data === value ? ' selected' : '') + '>' +
                       label + '</option>';
            });
            select += '</select>';
            return select;
          }
          return data;
        }
      ")
    )

  # Format curator notes as expandable input
  dt <- dt %>%
    formatStyle(
      'curator_notes',
      target = "cell",
      render = JS("
        function(data, type, row) {
          if(type === 'display') {
            var input = '<input type=\"text\" class=\"specimen-notes form-control form-control-sm\"' +
                       ' style=\"width:100%;height:24px;padding:2px 6px;\"' +
                       ' value=\"' + (data || '') + '\"' +
                       ' placeholder=\"Add notes...\">';
            return input;
          }
          return data;
        }
      ")
    )

  # Add other standard formatting
  if ("quality_score" %in% names(data)) {
    dt <- dt %>%
      formatStyle(
        'quality_score',
        background = styleColorBar(c(0, 14), "#28a745"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  }

  if ("specimen_rank" %in% names(data)) {
    dt <- dt %>%
      formatStyle(
        'specimen_rank',
        backgroundColor = styleEqual(
          c(1, 2, 3, 4, 5, 6, 7),
          c('#28a745', '#28a745', '#17a2b8', '#17a2b8',
            '#ffc107', '#ffc107', '#dc3545')
        ),
        color = styleEqual(
          c(1, 2, 3, 4, 5, 6, 7),
          rep('white', 7)
        )
      )
  }

  if (!is.null(color_by) && color_by %in% names(data)) {
    unique_values <- unique(data[[color_by]])
    colors <- create_species_colors(unique_values)

    dt <- dt %>%
      formatStyle(
        color_by,
        backgroundColor = styleEqual(unique_values, colors)
      )
  }

  dt
}

#' Get table callback JavaScript
#' @param ns Namespace function for Shiny
#' @param flag_options Named list of flag options
#' @return JavaScript callback code
#' @keywords internal
get_table_callback <- function(ns, flag_options = NULL) {
  if (is.null(ns)) return(NULL)

  # Add flag options to JavaScript
  flag_js <- if (!is.null(flag_options)) {
    sprintf("var flagOptions = %s;\n",
            jsonlite::toJSON(flag_options, auto_unbox = TRUE))
  } else {
    ""
  }

  JS(sprintf("
    function(table) {
      %s

      // Handle checkbox changes
      table.on('change', 'input.specimen-select', function() {
        var $cell = $(this).closest('td');
        var row = table.row($cell.closest('tr'));
        var data = row.data();
        Shiny.setInputValue(
          '%s',
          {
            processid: data.processid,
            selected: this.checked,
            species: data.species,
            bin_uri: data.bin_uri,
            quality_score: data.quality_score
          },
          {priority: 'event'}
        );
      });

      // Handle flag dropdown changes
      table.on('change', 'select.specimen-flag', function() {
        var $cell = $(this).closest('td');
        var row = table.row($cell.closest('tr'));
        var data = row.data();
        Shiny.setInputValue(
          '%s',
          {
            processid: data.processid,
            flag: this.value,
            species: data.species,
            bin_uri: data.bin_uri
          },
          {priority: 'event'}
        );
      });

      // Handle curator notes changes
      table.on('change', 'input.specimen-notes', function() {
        var $input = $(this);
        var row = table.row($input.closest('tr'));
        var data = row.data();
        Shiny.setInputValue(
          '%s',
          {
            processid: data.processid,
            notes: $input.val(),
            species: data.species,
            bin_uri: data.bin_uri
          },
          {priority: 'event'}
        );
      });
    }
  ", flag_js, ns("specimen_selection"), ns("specimen_flag"), ns("specimen_notes")))
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
    overflow-x: auto !important;
    white-space: nowrap !important;
    min-height: 120px !important;
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
    width: auto !important;
  }

  /* Interactive Elements */
  .specimen-select {
    width: 16px !important;
    height: 16px !important;
    padding: 0 !important;
    margin: 0 !important;
    vertical-align: middle !important;
  }

  .specimen-flag {
    width: 100% !important;
    min-width: 120px !important;
    height: 20px !important;
    padding: 1px 4px !important;
    font-size: 11px !important;
    line-height: 1.2 !important;
  }

  .specimen-notes {
    width: 100% !important;
    min-width: 150px !important;
    height: 20px !important;
    font-size: 11px !important;
    line-height: 1.2 !important;
    transition: all 0.2s ease-in-out !important;
  }

  .specimen-notes:focus {
    height: 60px !important;
    position: absolute !important;
    z-index: 1000 !important;
    background: white !important;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2) !important;
  }

  /* Force No-Wrap */
  .datatable td,
  .datatable th,
  .datatable tr,
  .datatable div {
    white-space: nowrap !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
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
    border-right: 2px solid #dee2e6;
    background-color: #fff !important;
  }

  .DTFC_LeftHeadWrapper {
    border-bottom: 2px solid #dee2e6;
  }

  /* Button Styling */
  .dt-buttons {
    margin-bottom: 10px !important;
  }

  .dt-button {
    margin-right: 5px !important;
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

#' Format haplotype summary table
#' @param summary_data Data frame of haplotype summary data
#' @export
format_haplotype_summary_table <- function(summary_data) {
  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(NULL)
  }

  required_cols <- c("species", "unique_haplotypes", "haplotype_diversity",
                     "coverage", "total_specimens")
  display_cols <- intersect(required_cols, names(summary_data))

  datatable(
    summary_data[, display_cols, drop = FALSE],
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
      'haplotype_diversity',
      background = styleColorBar(c(0, 1), '#28a745')
    ) %>%
    formatRound('haplotype_diversity', digits = 3) %>%
    formatStyle(
      'unique_haplotypes',
      background = styleColorBar(
        c(0, max(summary_data$unique_haplotypes)),
        '#17a2b8'
      )
    )
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

#' Format specimen fields for display
#' @param specimen Specimen data
#' @return List of formatted fields
#' @keywords internal
format_specimen_fields <- function(specimen) {
  if (is.null(specimen)) return(list())

  list(
    processid = specimen$processid %||% NA,
    quality_score = if (!is.null(specimen$quality_score))
      sprintf("%.1f", specimen$quality_score) else NA,
    identification_rank = specimen$identification_rank %||% NA,
    bin_uri = specimen$bin_uri %||% "",
    species = specimen$species %||% "",
    collection_date = if (!is.null(specimen$collection_date_start))
      format(specimen$collection_date_start, "%Y-%m-%d") else NA,
    country = specimen$country.ocean %||% ""
  )
}

#' Format metrics for display
#' @param metrics List of metrics
#' @return Formatted metrics list
#' @keywords internal
format_metrics <- function(metrics) {
  if (is.null(metrics)) return(list())

  list(
    specimen_count = format(metrics$specimen_count %||% 0, big.mark = ","),
    bin_count = format(metrics$bin_count %||% 0, big.mark = ","),
    selected_count = format(metrics$selected_count %||% 0, big.mark = ","),
    quality_avg = sprintf("%.2f", metrics$quality_avg %||% 0)
  )
}

#' Create download filename
#' @param grade BAGS grade
#' @param type Export type
#' @return Formatted filename
#' @keywords internal
create_download_filename <- function(grade, type = "csv") {
  paste0(
    "bags_grade_",
    tolower(grade %||% ""),
    "_",
    format(Sys.time(), "%Y%m%d_%H%M"),
    ".",
    type
  )
}

#' Validate table input
#' @param data Input data frame
#' @param required_cols Required columns
#' @return List with validation results
#' @keywords internal
validate_table_input <- function(data, required_cols) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "Empty data"))
  }

  if (is.null(required_cols) || length(required_cols) == 0) {
    return(list(valid = FALSE, message = "No required columns specified"))
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Missing columns: %s",
                        paste(missing_cols, collapse = ", "))
    ))
  }

  list(valid = TRUE, message = NULL)
}
