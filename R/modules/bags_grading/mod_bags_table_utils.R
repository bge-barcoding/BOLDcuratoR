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
    ordering = TRUE,
    order = list(list(2, 'desc')), # Sort by quality score by default
    dom = "t",
    pageLength = -1,
    columnDefs = list(
      list(
        targets = 0:1,  # Selection and flag columns
        searchable = FALSE,
        orderable = FALSE,
        width = "50px"
      ),
      list(
        targets = 2,  # Quality score column
        width = "80px"
      ),
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
  )

  # Create color coding if needed
  if(!is.null(color_by) && color_by %in% names(data)) {
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

        // Add tooltips
        table.on('mouseover', 'td', function() {
          var $cell = $(this);
          var cellData = $cell.text();
          if (cellData.length > 20) {
            $cell.attr('title', cellData);
          }
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
            return '<select class=\"specimen-flag form-control\">' +
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
    class = "specimen-table-container mb-4",

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

#' Create species-specific color scheme for shared BINs
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

#' Format flag text for display
#' @param flag_value Raw flag value
#' @return Formatted flag text
#' @keywords internal
format_flag_text <- function(flag_value) {
  if (is.null(flag_value) || is.na(flag_value) || flag_value == "") {
    return("")
  }
  switch(flag_value,
         "Misidentified" = "Misidentified",
         "ID uncertain" = "ID uncertain",
         ""
  )
}

#' Generate table caption based on grouping
#' @param grade BAGS grade
#' @param group_info Group information list
#' @return Caption string
#' @keywords internal
generate_table_caption <- function(grade, group_info) {
  switch(grade,
         "A" = sprintf("Species: %s (>10 specimens, single BIN)", group_info$species),
         "B" = sprintf("Species: %s (3-10 specimens, single BIN)", group_info$species),
         "C" = sprintf("Species: %s - BIN: %s", group_info$species, group_info$bin),
         "D" = sprintf("Species: %s (<3 specimens, single BIN)", group_info$species),
         "E" = sprintf("Shared BIN: %s (%d species)", group_info$bin, group_info$species_count),
         ""
  )
}

#' Format specimen fields for display
#' @param specimen Specimen data
#' @return List of formatted fields
#' @keywords internal
format_specimen_fields <- function(specimen) {
  list(
    processid = specimen$processid,
    quality_score = sprintf("%.1f", specimen$quality_score),
    criteria_met = gsub("; ", "<br>", specimen$criteria_met),
    bin_uri = specimen$bin_uri %||% "",
    species = specimen$species %||% "",
    collection_date = format(specimen$collection_date_start, "%Y-%m-%d"),
    country = specimen$country.ocean %||% ""
  )
}
