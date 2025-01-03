# R/utils/table_utils.R

# Source the constants file for PREFERRED_COLUMNS
if (!exists("PREFERRED_COLUMNS")) {
  source("R/config/constants.R")
}

#' Get standard table columns
#' depreciated in favour of preferred_columns in constants.R
#' @return Vector of standard column names
#' @keywords internal
# get_standard_columns <- function() {
#  c("selected", "flag", "curator_notes",
#    "rank", "quality_score", "processid", "bin_uri",
#    "identification", "identified_by",
#    "identification_method", "country.ocean",
#    "collection_date_start", "collectors", "inst",
#    "criteria_met")
#}

#' Order columns according to preferred configuration
#' @param data Data frame to reorder
#' @return Data frame with reordered columns
#' @keywords internal
order_columns <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Get preferred column order using the PREFERRED_COLUMNS function
  ordered_cols <- PREFERRED_COLUMNS(data)
  # Get actual columns that exist in the data
  valid_cols <- intersect(ordered_cols, names(data))
  # Get any remaining columns not in the preferred list
  other_cols <- setdiff(names(data), ordered_cols)

  # Combine the columns in the desired order
  data[, c(valid_cols, other_cols)]
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
    "Other Issue" = "other_issue"
  )
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
                                  logger = NULL) {

  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty input data")
    return(NULL)
  }

  # Add this check for column definitions
  # only if renaming columns from standard BOLD names
  #if (!exists("COLUMN_DEFINITIONS", envir = .GlobalEnv)) {
  #  source("R/config/column_definitions.R")
  #}

  # Ensure data is a data frame and reset rownames
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  rownames(data) <- NULL

  # previous interactive columns
  #data$selected <- FALSE
  #data$flag <- ""
  #data$curator_notes <- ""

  # updated interactive columns
  data$selected <- as.character(data$selected)  # Convert logical to text "TRUE"/"FALSE"
  data$flag <- as.character(data$flag)
  data$curator_notes <- as.character(data$curator_notes)

  # Update with current values if provided
  if (!is.null(current_selections)) {
    data$selected <- sapply(data$processid, function(pid) !is.null(current_selections[[pid]]))
  }
  if (!is.null(current_flags)) {
    data$flag <- sapply(data$processid, function(pid) {
      if (!is.null(current_flags[[pid]]) && !is.null(current_flags[[pid]]$flag)) {
        current_flags[[pid]]$flag
      } else {
        ""
      }
    })
  }
  if (!is.null(current_notes)) {
    data$curator_notes <- sapply(data$processid, function(pid) {
      if (!is.null(current_notes[[pid]])) {
        as.character(current_notes[[pid]])
      } else {
        ""
      }
    })
  }
  # order the columns properly
  data <- order_columns(data)

  if("curator_notes" %in% names(data)) {
    data$curator_notes <- as.character(data$curator_notes)
  }

  # Updated DT options
  dt_options <- list(
    scrollX = TRUE,
    scrollY = "500px",
    pageLength = page_length,
    autoWidth = FALSE,  # Changed to FALSE to ensure fixed widths
    dom = 'Bfrtip',
    buttons = buttons,
    extensions = c('FixedColumns', 'Buttons'),
    fixedColumns = list(
      left = 3
    ),
    columnDefs = list(
      # Selected column
      list(
        targets = which(names(data) == "selected") - 1,
        width = "40px",
        className = 'dt-center fixed-col',
        orderable = FALSE,
        searchable = FALSE
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
        className = 'dt-center fixed-col editable',
        orderable = FALSE,
        searchable = FALSE
      ),
      # All other columns
      list(
        targets = "_all",
        className = "dt-body-left",
        width = "100px",  # Default width
        maxWidth = "100px",
        render = JS("
        function(data, type, row) {
          if (type === 'display') {
            return '<div class=\"cell-content\" title=\"' +
                   (data || '').toString().replace(/\"/g, '&quot;') +
                   '\">' + (data || '') + '</div>';
          }
          return data;
        }
      ")
      )
    )
  )

  # Add callback if namespace provided
  if (!is.null(ns)) {
    dt_options$callback <- get_table_callback(ns, get_flag_options())
  }

  # Create base DT object
  tryCatch({
    dt <- DT::datatable(
      data,
      options = dt_options,
      selection = selection,
      rownames = FALSE,
      escape = FALSE,
      editable = list(
        target = 'cell',
        disable = list(columns = setdiff(seq_len(ncol(data))-1,
                                         which(names(data) %in% c("curator_notes"))-1))
      ),
      extensions = c('Buttons', 'FixedColumns', 'Select')

      # column renaming lines which break the app
      # tables don't appear and I can't figure it out
      # leaving here for future activation
      #colnames = sapply(names(data), function(col) {
      #  if(col %in% names(COLUMN_DEFINITIONS)) COLUMN_DEFINITIONS[[col]] else tools::toTitleCase(gsub("_", " ", col))
      #}),
      #escape = c("Selected", "Issue", "Curator_Notes")
    )

    # Add interactive column formatting using new column names
    for(col_pair in list(c("selected", "selected"),
                         c("flag", "flag"),
                         c("curator_notes", "curator_notes"))) {
      orig_col <- col_pair[1]
      new_col <- col_pair[2]
      if (orig_col %in% names(data)) {
        dt <- format_interactive_column(dt, new_col)
      }
    }

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

# Helper function to format interactive columns
format_interactive_column <- function(dt, col) {
  if (col == "selected") {
    dt %>% formatStyle(
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


  } else if (col == "flag") {
      dt %>% formatStyle(
        'flag',
        target = "cell",
        render = JS(sprintf("
      function(data, type, row) {
        if(type === 'display') {
          const options = %s;
          let select = '<select class=\"specimen-flag form-select form-select-sm\">';
          Object.entries(options).forEach(([label, value]) => {
            select += '<option value=\"' + value + '\"' +
                     (data === value ? ' selected' : '') + '>' +
                     label + '</option>';
          });
          select += '</select>';
          return select;
        }
        return data;
      }
    ", jsonlite::toJSON(get_flag_options(), auto_unbox = TRUE)))
      )
  } else if (col == "curator_notes") {
    dt %>% formatStyle(
      'curator_notes',
      cursor = 'text'
    )
  } else {
    dt
  }
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

# Handles data preparation consistently
prepare_module_data <- function(data,
                                current_selections = NULL,
                                current_flags = NULL,
                                current_notes = NULL,
                                logger = NULL) {

  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty input data to prepare_module_data")
    return(data.frame())
  }

  # Log initial state
  if (!is.null(logger)) {
    logger$info("Pre-format specimen table data", details = list(
      rows = nrow(data),
      columns = names(data),
      sample_processids = head(data$processid)
    ))
  }

  # Convert problematic column types
  for (col in names(data)) {
    if (is.list(data[[col]]) || is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  # Add missing standard columns if needed
  missing_cols <- setdiff(PREFERRED_COLUMNS(data), names(data))
  for (col in missing_cols) {
    data[[col]] <- NA_character_
  }

  # Add interactive columns
  data$selected <- FALSE
  data$flag <- ""
  data$curator_notes <- ""

  # Update with current values
  if (!is.null(current_selections)) {
    data$selected <- sapply(data$processid, function(pid) {
      !is.null(current_selections[[pid]])
    })
  }

  if (!is.null(current_flags)) {
    data$flag <- sapply(data$processid, function(pid) {
      if (!is.null(current_flags[[pid]]) && !is.null(current_flags[[pid]]$flag)) {
        current_flags[[pid]]$flag
      } else {
        ""
      }
    })
  }

  if (!is.null(current_notes)) {
    data$curator_notes <- sapply(data$processid, function(pid) {
      if (!is.null(current_notes[[pid]])) {
        as.character(current_notes[[pid]])
      } else {
        ""
      }
    })
  }

  # Final cleanup - ensure no NULL values
  for (col in names(data)) {
    if (any(sapply(data[[col]], is.null))) {
      data[[col]][sapply(data[[col]], is.null)] <- NA
    }
  }

  # Log final state
  if (!is.null(logger)) {
    logger$info("Post-format specimen table", details = list(
      table_class = class(data),
      table_columns = if(is.data.frame(data)) names(data) else "Not a data frame"
    ))
  }

  return(data)
}


#' Format specific table columns with styling
#' @param dt DT datatable object
#' @param data Original data frame
#' @param color_by Optional column for color coding
#' @param logger Optional logger instance
#' @return Formatted datatable
#' @keywords internal
format_table_columns <- function(dt, data, color_by = NULL, logger = NULL) {
  if (is.null(dt)) return(NULL)

  tryCatch({
    # Add interactive column formatting
    if ("selected" %in% names(data)) {
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
    }

    if ("flag" %in% names(data)) {
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
    }

    if ("curator_notes" %in% names(data)) {
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
    }

    # Add metric column formatting
    if ("quality_score" %in% names(data)) {
      dt <- dt %>%
        formatStyle(
          'quality_score',
          background = styleColorBar(c(0, max(14, max(data$quality_score, na.rm = TRUE))),
                                     "#28a745"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    }

    if ("rank" %in% names(data)) {
      dt <- dt %>%
        formatStyle(
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

    # Add color coding if specified
    if (!is.null(color_by) && color_by %in% names(data)) {
      unique_values <- unique(data[[color_by]])
      colors <- create_species_colors(unique_values)

      if (!is.null(colors)) {
        dt <- dt %>%
          formatStyle(
            color_by,
            backgroundColor = styleEqual(unique_values, colors)
          )
      }
    }

    if (!is.null(logger)) {
      logger$info("Column formatting applied successfully")
    }

    return(dt)

  }, error = function(e) {
    if (!is.null(logger)) {
      logger$error("Error formatting table columns",
                   details = list(error = e$message))
    }
    return(dt)  # Return original table if formatting fails
  })
}

#' Prepare table data with interactive columns while preserving all existing columns
#' @param data Input data frame
#' @param custom_cols Preferred columns to ensure exist
#' @param current_selections Current selections
#' @param current_flags Current flags
#' @param current_notes Current curator notes
#' @param logger Optional logger instance
#' @return Prepared data frame
#' @keywords internal
# Revised prepare_table_data function
prepare_table_data <- function(data, current_selections, current_flags,
                               current_notes, logger = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty or invalid input data to prepare_table_data")
    return(data.frame())
  }

  # Log initial state
  if (!is.null(logger)) {
    logger$info("Starting prepare_table_data", details = list(
      initial_columns = names(data),
      data_classes = sapply(data, class),
      row_count = nrow(data)
    ))
  }

  # Convert to data frame and preserve row names
  rn <- rownames(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  rownames(data) <- rn

  # Convert column names to lowercase
  original_names <- names(data)
  names(data) <- tolower(names(data))

  if (!is.null(logger)) {
    logger$info("Column name conversion", details = list(
      original = original_names,
      converted = names(data)
    ))
  }

  # Check for required processid column
  if (!"processid" %in% names(data)) {
    if (!is.null(logger)) logger$error("Missing required processid column")
    return(data.frame())
  }

  # Handle specimen_rank to rank conversion
  if ("specimen_rank" %in% names(data) && !"rank" %in% names(data)) {
    if (!is.null(logger)) {
      logger$info("Converting specimen_rank to rank", details = list(
        specimen_rank_values = head(data$specimen_rank)
      ))
    }
    data$rank <- data$specimen_rank
    data$specimen_rank <- NULL
  }

  # Convert problematic types
  for (col in names(data)) {
    if (is.list(data[[col]]) || is.factor(data[[col]])) {
      if (!is.null(logger)) {
        logger$info(sprintf("Converting column %s from %s to character",
                            col, class(data[[col]])[1]))
      }
      data[[col]] <- as.character(data[[col]])
    }
  }

  # Add missing standard columns first
  missing_cols <- setdiff(custom_cols, names(data))
  if (length(missing_cols) > 0) {
    if (!is.null(logger)) {
      logger$info("Adding missing standard columns", details = list(
        missing_columns = missing_cols
      ))
    }
    for (col in missing_cols) {
      data[[col]] <- NA_character_
    }
  }

  # Then add/update interactive columns
  data$selected <- FALSE
  if (!is.null(current_selections)) {
    data$selected <- sapply(data$processid, function(pid) {
      !is.null(current_selections[[pid]])
    }, USE.NAMES = FALSE)
  }

  data$flag <- ""
  if (!is.null(current_flags)) {
    data$flag <- sapply(data$processid, function(pid) {
      if (!is.null(current_flags[[pid]]) && !is.null(current_flags[[pid]]$flag)) {
        as.character(current_flags[[pid]]$flag)
      } else {
        ""
      }
    }, USE.NAMES = FALSE)
  }

  data$curator_notes <- ""
  if (!is.null(current_notes)) {
    data$curator_notes <- sapply(data$processid, function(pid) {
      if (!is.null(current_notes[[pid]])) {
        if (is.list(current_notes[[pid]])) {
          current_notes[[pid]]$text %||% ""
        } else {
          as.character(current_notes[[pid]])
        }
      } else {
        ""
      }
    }, USE.NAMES = FALSE)
  }

  if (!is.null(logger)) {
    logger$info("Interactive columns added", details = list(
      has_selected = "selected" %in% names(data),
      has_flag = "flag" %in% names(data),
      has_notes = "curator_notes" %in% names(data)
    ))
  }

  # Final cleanup - ensure no NULL values
  for (col in names(data)) {
    if (any(sapply(data[[col]], is.null))) {
      if (!is.null(logger)) {
        logger$info(sprintf("Replacing NULL values in column %s", col))
      }
      data[[col]][sapply(data[[col]], is.null)] <- NA
    }
  }

  # Ensure rank is numeric
  if ("rank" %in% names(data)) {
    data$rank <- as.numeric(data$rank)
  }

  # Order columns according to preference
  data <- order_columns(data)

  # Log final state
  if (!is.null(logger)) {
    logger$info("Prepare table data complete", details = list(
      final_columns = names(data),
      row_count = nrow(data),
      column_classes = sapply(data, class),
      rank_info = if("rank" %in% names(data)) list(
        class = class(data$rank),
        unique_values = unique(data$rank),
        has_na = any(is.na(data$rank))
      ) else NULL
    ))
  }

  return(data)
}

#' Get table callback JavaScript
#' @param ns Namespace function for Shiny
#' @param flag_options Named list of flag options
#' @return JavaScript callback code
#' @keywords internal
get_table_callback <- function(ns, flag_options = NULL) {
  if (is.null(ns)) return(NULL)

  # Get flag options directly from function
  flags <- get_flag_options()
  flag_js <- sprintf("const flagOptions = %s;\n",
                     jsonlite::toJSON(flags, auto_unbox = TRUE))

  JS(sprintf("
    function(table) {
      %s

      // Handle checkbox changes
      table.on('change', 'input.specimen-select', function() {
        const $cell = $(this).closest('td');
        const row = table.row($cell.closest('tr'));
        const data = row.data();
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
        const $cell = $(this).closest('td');
        const row = table.row($cell.closest('tr'));
        const data = row.data();
        const flagValue = this.value;
        const flagLabel = $(this).find('option:selected').text();

        Shiny.setInputValue(
          '%s',
          {
            processid: data.processid,
            flag: flagValue,
            flag_label: flagLabel,
            species: data.species,
            bin_uri: data.bin_uri
          },
          {priority: 'event'}
        );
      });

      // Handle curator notes changes
      table.on('change', 'input.specimen-notes', function() {
        const $input = $(this);
        const row = table.row($input.closest('tr'));
        const data = row.data();
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
    overflow-x: scroll !important;
    overflow-y: scroll !important;
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
