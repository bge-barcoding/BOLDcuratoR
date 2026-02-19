# R/modules/bags_grading/mod_bags_grading_utils.R

#' Calculate grade metrics
#' @param data Data frame of grade specimens
#' @return List of metrics
#' @keywords internal
calculate_grade_metrics <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(list(
      total_species = 0,
      total_specimens = 0,
      avg_quality = 0,
      countries = 0
    ))
  }

  list(
    total_species = length(unique(data$species)),
    total_specimens = nrow(data),
    avg_quality = mean(data$quality_score, na.rm = TRUE),
    countries = length(unique(data$country.ocean))
  )
}

#' Organize specimens by grade with proper group info
#' @param specimens Data frame of specimens
#' @param grade BAGS grade
#' @return Organized list of specimens with attributes
#' @keywords internal
organize_grade_specimens <- function(specimens, grade) {
  if (is.null(specimens) || nrow(specimens) == 0) return(list())

  # Sort specimens by quality score within groups
  specimens <- specimens[order(-specimens$quality_score), ]

  switch(grade,
         # Grades A, B, D: Group by species
         "A" = {
           groups <- split(specimens, specimens$species)
           # Add species info to each group
           for (species in names(groups)) {
             attr(groups[[species]], "info") <- list(
               species = species,
               specimen_count = nrow(groups[[species]])
             )
           }
           groups
         },

         "B" = {
           groups <- split(specimens, specimens$species)
           for (species in names(groups)) {
             attr(groups[[species]], "info") <- list(
               species = species,
               specimen_count = nrow(groups[[species]])
             )
           }
           groups
         },

         "D" = {
           groups <- split(specimens, specimens$species)
           for (species in names(groups)) {
             attr(groups[[species]], "info") <- list(
               species = species,
               specimen_count = nrow(groups[[species]])
             )
           }
           groups
         },

         # Grade C: Group by species then BIN
         "C" = {
           result <- list()
           species_groups <- split(specimens, specimens$species)

           for (species in names(species_groups)) {
             bin_groups <- split(species_groups[[species]], species_groups[[species]]$bin_uri)
             for (bin in names(bin_groups)) {
               group_name <- paste(species, bin, sep = "_")
               result[[group_name]] <- bin_groups[[bin]]
               attr(result[[group_name]], "info") <- list(
                 species = species,
                 bin = bin,
                 specimen_count = nrow(bin_groups[[bin]])
               )
             }
           }
           result
         },

         # Grade E: Group by shared BINs
         "E" = {
           result <- list()
           bins <- unique(specimens$bin_uri)

           for (bin in bins) {
             bin_specimens <- specimens[specimens$bin_uri == bin,]
             if (length(unique(bin_specimens$species)) > 1) {
               result[[bin]] <- bin_specimens[order(-bin_specimens$quality_score),]
               attr(result[[bin]], "info") <- list(
                 bin = bin,
                 species_count = length(unique(bin_specimens$species)),
                 species = paste(sort(unique(bin_specimens$species)), collapse = ", ")
               )
             }
           }
           result
         },

         # Default case
         list()
  )
}

#' Create tables for each group of specimens within a grade
#' @param organized List of organized specimen groups
#' @param grade BAGS grade
#' @param ns Namespace function
#' @param current_sel Current selections
#' @param current_flags Current flags
#' @param current_notes Current notes
#' @param logger Logger instance
#' @return List of formatted tables
#' @keywords internal
create_grade_tables <- function(organized, grade, ns, current_sel, current_flags,
                                current_notes, logger) {

  if (is.null(organized) || length(organized) == 0) {
    logger$warn("No organized data to create tables")
    return(list())
  }

  tables <- lapply(seq_along(organized), function(i) {
    group_data <- organized[[i]]
    group_info <- attributes(organized[[i]])$info

    tryCatch({
      logger$info(sprintf("Creating table %d for grade %s", i, grade))

      # First prepare the data
      prepared_data <- prepare_module_data(
        data = group_data,
        current_selections = current_sel,
        current_flags = current_flags,
        current_notes = current_notes,
        logger = logger
      )

      # Create table
      dt <- format_grade_table(
        data = prepared_data,
        ns = ns,
        grade = grade
      )

      if (!is.null(dt)) {
        div(
          class = "specimen-table-container mb-2",
          h4(class = "table-title", generate_table_caption(grade, group_info)),
          dt
        )
      }

    }, error = function(e) {
      logger$error(sprintf("Error creating table %d for grade %s", i, grade),
                   list(error = e$message))
      NULL
    })
  })

  # Remove NULL entries
  tables[!sapply(tables, is.null)]
}

#' Format the grade tables
#' @param data Data frame of specimen data
#' @param ns Namespace function for Shiny
#' @param grade BAGS grade
#' @return DT datatable object
#' @keywords internal
format_grade_table <- function(data, ns = NULL, grade) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  # Dynamic page length: show all rows up to 25, paginate beyond that
  n_rows <- nrow(data)
  dynamic_page_length <- min(n_rows, 25)

  # Calculate dynamic scroll height based on actual row count.
  # Each row is 24px, header is 28px, plus some padding.
  # Cap at 500px for large tables; let scrollCollapse shrink small ones.
  rows_to_show <- min(n_rows, dynamic_page_length)
  content_height <- (rows_to_show * 24) + 28 + 10
  dynamic_scroll_y <- paste0(min(content_height, 500), "px")

  # Data arrives already prepared with annotations from create_grade_tables.
  # Go straight to formatting.
  dt <- format_specimen_table(
    data = data,
    ns = ns,
    buttons = list(),
    page_length = dynamic_page_length,
    selection = 'none',
    dom = if (n_rows > 25) "frtip" else "t",
    scroll_y = dynamic_scroll_y
  )

  if(grade == "E" && !is.null(dt)) {
    # Use identification column instead of species for coloring
    unique_identifications <- sort(unique(as.character(data$identification)))
    n_identifications <- length(unique_identifications)

    # Define visually distinct pastel colors for better species differentiation
    base_colors <- c(
      "#cce6ff",  # Light blue
      "#ffe6cc",  # Light orange
      "#e6ffcc",  # Light green
      "#ffcce6",  # Light pink
      "#e6ccff",  # Light purple
      "#ccffe6",  # Light mint
      "#ffcccc",  # Light red
      "#ccccff"   # Light indigo
    )

    # If we need more colors than our base set, generate them
    colors <- if(n_identifications <= length(base_colors)) {
      base_colors[1:n_identifications]
    } else {
      # For more species, generate additional colors while maintaining distinctness
      colorRampPalette(base_colors)(n_identifications)
    }

    # Apply styling to the identification column
    dt <- dt %>% formatStyle(
      'identification',  # Changed from 'species' to 'identification'
      backgroundColor = styleEqual(
        levels = unique_identifications,
        values = colors
      )
    )
  }

  dt
}

#' Generate table caption based on grade and grouping
#' @param grade BAGS grade
#' @param group_info List of grouping information
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

