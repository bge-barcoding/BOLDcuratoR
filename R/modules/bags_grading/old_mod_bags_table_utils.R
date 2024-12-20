# R/modules/bags_grading/mod_bags_table_utils.R

#' Create specimen table with frozen columns and selection handling
#' @param ns Namespace function
#' @param data Specimen data
#' @param group_id Group identifier
#' @param color_by Optional column to use for color coding
#' @param current_selections List of currently selected specimens
#' @param current_flags List of current specimen flags
#' @return DT datatable object
#' @keywords internal
create_specimen_table <- function(ns, data, group_id, color_by = NULL,
                                  current_selections = NULL, current_flags = NULL) {
  # Define column order
  cols <- c(
    "selected", "flag",
    "processid", "species", "bin_uri",
    "quality_score", "specimen_rank", "identification_rank",
    "country.ocean", "collection_date_start",
    "identified_by", "institution"
  )

  # Add remaining columns
  remaining_cols <- setdiff(names(data), cols)
  cols <- c(cols, remaining_cols)

  # Add selection and flag columns if not present
  if(!"selected" %in% names(data)) {
    data$selected <- sapply(data$processid, function(pid) {
      !is.null(current_selections) && !is.null(current_selections[[pid]])
    })
  }

  if(!"flag" %in% names(data)) {
    data$flag <- sapply(data$processid, function(pid) {
      if(!is.null(current_flags) && !is.null(current_flags[[pid]])) {
        current_flags[[pid]]$flag
      } else {
        ""
      }
    })
  }

  # Create DT options
  options <- list(
    scrollX = TRUE,
    scrollY = "500px",
    fixedColumns = list(leftColumns = 2),
    ordering = TRUE,
    order = list(list(5, 'desc')), # Sort by quality score by default
    pageLength = 50,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    columnDefs = list(
      list(
        targets = 0:1,  # Selection and flag columns
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

  # Create color coding if needed
  if(!is.null(color_by) && color_by %in% names(data)) {
    unique_values <- unique(data[[color_by]])
    colors <- create_species_colors(unique_values)
  }

  # Create the datatable
  dt <- datatable(
    data[, cols, drop = FALSE],
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

        // Flag handler
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

        // Initialize selections and flags
        table.rows().every(function() {
          var data = this.data();
          if(data.selected) {
            this.nodes().to$().find('input.specimen-select').prop('checked', true);
          }
          if(data.flag) {
            this.nodes().to$().find('select.specimen-flag').val(data.flag);
          }
        });
      }
    ", ns("specimen_selection"), ns("specimen_flag")))
  )

  # Add column formatting
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
    ) %>%
    formatStyle(
      'flag',
      target = "cell",
      render = JS("
        function(data, type, row) {
          if(type === 'display') {
            var selected = data || '';
            return '<select class=\"specimen-flag form-control\">' +
                   '<option value=\"\"' + (selected === '' ? ' selected' : '') + '></option>' +
                   '<option value=\"Misidentified\"' + (selected === 'Misidentified' ? ' selected' : '') + '>Misidentified</option>' +
                   '<option value=\"ID uncertain\"' + (selected === 'ID uncertain' ? ' selected' : '') + '>ID uncertain</option>' +
                   '</select>';
          }
          return data;
        }
      ")
    ) %>%
    formatStyle(
      'quality_score',
      background = styleColorBar(c(0, 14), "#28a745"),
      backgroundSize = "98% 88%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    ) %>%
    formatStyle(
      'specimen_rank',
      backgroundColor = styleInterval(
        c(1.5, 2.5, 3.5, 4.5, 5.5),
        c('#28a745', '#28a745', '#17a2b8', '#17a2b8', '#ffc107', '#ffc107')
      ),
      color = 'white'
    ) %>%
    formatStyle(
      'identification_rank',
      backgroundColor = styleEqual(
        c("species", "subspecies"),
        c('#d1e7dd', '#cfe2ff')
      )
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
         "A" = sprintf("Species: %s (>10 specimens with valid BINs)", group_info$species),
         "B" = sprintf("Species: %s (3-10 specimens with valid BINs)", group_info$species),
         "C" = sprintf("Species: %s - BIN: %s", group_info$species, group_info$bin),
         "D" = sprintf("Species: %s (<3 specimens with valid BINs)", group_info$species),
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
    specimen_rank = specimen$specimen_rank,
    identification_rank = specimen$identification_rank,
    bin_uri = specimen$bin_uri %||% "",
    species = specimen$species %||% "",
    collection_date = format(specimen$collection_date_start, "%Y-%m-%d"),
    country = specimen$country.ocean %||% ""
  )
}

#' Format metrics for display
#' @param metrics List of metrics
#' @return Formatted metrics list
#' @keywords internal
format_metrics <- function(metrics) {
  list(
    specimen_count = format(metrics$specimen_count, big.mark = ","),
    bin_count = format(metrics$bin_count, big.mark = ","),
    selected_count = format(metrics$selected_count, big.mark = ","),
    quality_avg = sprintf("%.2f", metrics$quality_avg)
  )
}

#' Create download filename
#' @param grade BAGS grade
#' @param type Export type
#' @return Formatted filename
#' @keywords internal
create_download_filename <- function(grade, type = "csv") {
  paste0(
    "bags_grade_", tolower(grade), "_",
    format(Sys.time(), "%Y%m%d_%H%M"),
    ".", type
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

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Missing columns: %s", paste(missing_cols, collapse = ", "))
    ))
  }

  list(valid = TRUE, message = NULL)
}
