# R/utils/table_utils.R

#' Get standard table columns
#' @return Vector of standard column names
#' @keywords internal
get_standard_columns <- function() {
  c("selected", "flag", "notes",
    "quality_score", "processid", "bin_uri",
    "species", "identification", "identified_by",
    "identification_method", "country.ocean",
    "collection_date_start", "collectors", "inst",
    "criteria_met", "specimen_rank")
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
#' @param current_notes List of current specimen notes
#' @param logger Optional logger instance
#' @export
format_specimen_table <- function(data, ns = NULL,
                                  buttons = c('copy', 'csv', 'excel'),
                                  page_length = 25,
                                  selection = 'none',
                                  color_by = NULL,
                                  current_selections = NULL,
                                  current_flags = NULL,
                                  current_notes = NULL,
                                  logger = NULL) {

  if (!is.null(logger)) {
    logger$info("format_specimen_table input", list(
      row_count = nrow(data),
      columns = names(data),
      has_selections = !is.null(current_selections),
      has_flags = !is.null(current_flags),
      has_notes = !is.null(current_notes)
    ))
  }

  cols <- get_standard_columns()

  if (!is.null(logger)) {
    missing_cols <- setdiff(cols, names(data))
    extra_cols <- setdiff(names(data), cols)
    logger$info("Column analysis", list(
      standard_cols = cols,
      missing_cols = missing_cols,
      extra_cols = extra_cols
    ))
  }

  prepared_data <- prepare_table_data(data, cols, current_selections,
                                      current_flags, current_notes, logger)

  options <- list(
    scrollX = TRUE,
    scrollY = "500px",
    fixedHeader = TRUE,
    fixedColumns = list(leftColumns = 3),
    ordering = TRUE,
    order = list(list(3, 'desc')),
    pageLength = page_length,
    dom = 'Bfrtip',
    buttons = buttons,
    columnDefs = list(
      list(
        targets = 0:2,
        searchable = FALSE,
        orderable = FALSE,
        width = "50px"
      ),
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
  )

  if (!is.null(ns)) {
    callback <- get_table_callback(ns)
    options$callback <- callback
  }

  display_cols <- intersect(cols, names(prepared_data))
  display_data <- prepared_data[, display_cols, drop = FALSE]

  dt <- datatable(
    display_data,
    options = options,
    selection = selection,
    rownames = FALSE
  )

  dt <- format_table_columns(dt, display_data, color_by, logger)

  dt
}

#' Prepare table data with interactive columns
#' @param data Input data frame
#' @param cols Required columns
#' @param current_selections Current selections
#' @param current_flags Current flags
#' @param current_notes Current notes
#' @param logger Optional logger instance
#' @return Prepared data frame
#' @keywords internal
prepare_table_data <- function(data, cols, current_selections, current_flags,
                               current_notes, logger = NULL) {

  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty or invalid input data")
    return(data.frame())
  }

  if (!"selected" %in% names(data)) {
    data$selected <- sapply(data$processid, function(pid) {
      !is.null(current_selections) && !is.null(current_selections[[pid]])
    })
  }

  if (!"flag" %in% names(data)) {
    data$flag <- sapply(data$processid, function(pid) {
      if (!is.null(current_flags) && !is.null(current_flags[[pid]])) {
        current_flags[[pid]]$flag
      } else {
        ""
      }
    })
  }

  if (!"notes" %in% names(data)) {
    data$notes <- sapply(data$processid, function(pid) {
      if (!is.null(current_notes) && !is.null(current_notes[[pid]])) {
        current_notes[[pid]]
      } else {
        ""
      }
    })
  }

  for (col in setdiff(cols, names(data))) {
    data[[col]] <- NA
  }

  if (!is.null(logger)) {
    logger$info("Interactive columns added", list(
      selected_count = sum(data$selected),
      flag_count = sum(data$flag != ""),
      notes_count = sum(data$notes != "")
    ))
  }

  data
}

#' Format table columns with styling
#' @param dt DT datatable object
#' @param data Original data frame
#' @param color_by Optional column for color coding
#' @param logger Optional logger instance
#' @return Formatted datatable
#' @keywords internal
format_table_columns <- function(dt, data, color_by = NULL, logger = NULL) {

  if (!is.null(logger)) {
    logger$info("Starting column formatting", list(
      available_columns = names(data),
      has_quality_score = "quality_score" %in% names(data),
      has_specimen_rank = "specimen_rank" %in% names(data),
      color_by = color_by
    ))
  }

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
#' @return JavaScript callback code
#' @keywords internal
get_table_callback <- function(ns) {
  if (is.null(ns)) return(NULL)

  JS(sprintf("
    function(table) {
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

      table.on('change', 'textarea.specimen-notes', function() {
        var $cell = $(this).closest('td');
        var row = table.row($cell.closest('tr'));
        var data = row.data();
        Shiny.setInputValue(
          '%s',
          {
            processid: data.processid,
            notes: this.value
          },
          {priority: 'event'}
        );
      });
    }
  ", ns("specimen_selection"), ns("specimen_flag"), ns("specimen_notes")))
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
