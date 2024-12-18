# R/modules/bags_grading/mod_bags_table_utils.R

#' Create specimen table with frozen columns
#' @param ns Namespace function
#' @param data Specimen data
#' @param group_id Group identifier
#' @param color_by Optional column to use for color coding
#' @return DT datatable object
#' @keywords internal
create_specimen_table <- function(ns, data, group_id, color_by = NULL) {
  # Define column order
  cols <- c(
    "selected", "flag", "quality_score", "criteria_met", "processid",
    "identification", "identified_by", "identification_method",
    "country.ocean", "collection_date_start", "collectors", "inst",
    "bin_uri"
  )

  # Add remaining columns
  remaining_cols <- setdiff(names(data), cols)
  cols <- c(cols, remaining_cols)

  # Ensure data has all required columns
  for(col in cols) {
    if(!col %in% names(data)) {
      data[[col]] <- NA
    }
  }

  # Create DT options
  options <- list(
    scrollX = TRUE,
    fixedColumns = list(left = 2),
    ordering = FALSE,
    dom = "t",
    pageLength = -1,
    columnDefs = list(
      list(
        targets = 0:1,  # Selection and flag columns
        searchable = FALSE,
        orderable = FALSE
      )
    )
  )

  # Create color coding if needed
  if(!is.null(color_by)) {
    unique_values <- unique(data[[color_by]])
    colors <- create_species_colors(unique_values)
  }

  # Create the datatable
  dt <- datatable(
    data[, cols],
    options = options,
    selection = "none",
    rownames = FALSE,
    callback = JS(sprintf("
      function(table) {
        // Selection handler
        table.on('change', 'input.specimen-select', function() {
          var $cell = $(this).closest('td');
          var row = table.row($cell.closest('tr'));
          var data = row.data();
          Shiny.setInputValue(
            '%s',
            {processid: data.processid, selected: this.checked},
            {priority: 'event'}
          );
        });

        // Flag handler
        table.on('change', 'select.specimen-flag', function() {
          var $cell = $(this).closest('td');
          var row = table.row($cell.closest('tr'));
          var data = row.data();
          Shiny.setInputValue(
            '%s',
            {processid: data.processid, flag: this.value},
            {priority: 'event'}
          );
        });

        // Initialize fixed columns
        new $.fn.dataTable.FixedColumns(table, {
          leftColumns: 2
        });
      }
    ", ns("specimen_selection"), ns("specimen_flag")))
  )

  # Add column formatting
  dt <- dt %>%
    formatStyle(
      "selected",
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
    ) %>%
    formatStyle(
      "flag",
      target = "cell",
      render = JS("
        function(data, type, row) {
          if(type === 'display') {
            return '<select class=\"specimen-flag\">' +
                   '<option value=\"\"' + (data === '' ? ' selected' : '') + '></option>' +
                   '<option value=\"Misidentified\"' + (data === 'Misidentified' ? ' selected' : '') + '>Misidentified</option>' +
                   '<option value=\"ID uncertain\"' + (data === 'ID uncertain' ? ' selected' : '') + '>ID uncertain</option>' +
                   '</select>';
          }
          return data;
        }
      ")
    ) %>%
    formatStyle(
      "quality_score",
      background = styleColorBar(c(0, 14), "#28a745"),
      backgroundSize = "98% 88%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    ) %>%
    formatStyle(
      "criteria_met",
      target = "cell",
      render = JS("
        function(data, type, row) {
          if(type === 'display' && data) {
            return data.split('; ').join('<br>');
          }
          return data;
        }
      ")
    )

  # Add color coding if specified
  if(!is.null(color_by) && !is.null(colors)) {
    dt <- dt %>% formatStyle(
      color_by,
      target = "row",
      backgroundColor = styleEqual(names(colors), unname(colors))
    )
  }

  dt
}

#' Create table container with title and controls
#' @param ns Namespace function
#' @param table DT datatable object
#' @param title Table title
#' @param caption Optional table caption
#' @return UI element
#' @keywords internal
create_table_container <- function(ns, table, title, caption = NULL) {
  div(
    class = "specimen-table-container",
    style = "margin-bottom: 2rem;",

    # Title section
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      h4(class = "m-0", title),
      if(!is.null(caption)) {
        div(class = "text-muted", caption)
      }
    ),

    # Table
    div(
      class = "table-responsive",
      table
    )
  )
}

#' Add column definitions to table
#' @param table DT datatable object
#' @param definitions List of column definitions
#' @return Modified DT datatable object
#' @keywords internal
add_column_definitions <- function(table, definitions) {
  for(col in names(definitions)) {
    def <- definitions[[col]]
    table <- table %>%
      formatStyle(
        col,
        backgroundColor = def$background,
        color = def$color,
        fontWeight = def$fontWeight,
        textAlign = def$textAlign
      )
  }
  table
}

#' Create column definitions for specimen table
#' @return List of column definitions
#' @keywords internal
get_column_definitions <- function() {
  list(
    "quality_score" = list(
      background = "#f8f9fa",
      textAlign = "center",
      fontWeight = "bold"
    ),
    "specimen_rank" = list(
      textAlign = "center",
      fontWeight = "bold"
    ),
    "processid" = list(
      fontWeight = "bold"
    ),
    "bin_uri" = list(
      background = "#f8f9fa",
      textAlign = "center"
    ),
    "criteria_met" = list(
      background = "#ffffff"
    )
  )
}
