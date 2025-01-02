# R/modules/bags_grading/mod_bags_grading_utils.R

#' Validate BAGS data consistency
#' @param specimens Data frame of specimens
#' @param grades BAGS grades data frame
#' @return List with validation results
#' @keywords internal
validate_bags_data <- function(specimens, grades) {
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(list(valid = FALSE, message = "No specimen data available"))
  }

  if (is.null(grades) || nrow(grades) == 0) {
    return(list(valid = FALSE, message = "No BAGS grades available"))
  }

  # Validate data consistency
  species_match <- all(grades$species %in% specimens$species)
  if (!species_match) {
    return(list(valid = FALSE, message = "Mismatch between specimens and BAGS grades"))
  }

  # Validate grade format
  valid_grades <- all(grades$bags_grade %in% c("A", "B", "C", "D", "E"))
  if (!valid_grades) {
    return(list(valid = FALSE, message = "Invalid BAGS grades detected"))
  }

  list(valid = TRUE, message = NULL)
}

#' Filter specimens by grade and criteria
#' @param specimens Data frame of specimens
#' @param grades Data frame of BAGS grades
#' @param target_grade Target BAGS grade
#' @param rank_filter Rank filter value
#' @param quality_filter Quality score filter
#' @param criteria_filter Vector of required criteria
#' @return Filtered specimen data frame
#' @keywords internal
filter_grade_specimens <- function(specimens, grades, target_grade,
                                   rank_filter, quality_filter, criteria_filter) {
  # Get species for target grade
  grade_species <- grades$species[grades$bags_grade == target_grade]
  filtered <- specimens[specimens$species %in% grade_species, ]

  # Apply rank filter
  if (!is.null(rank_filter) && rank_filter != "All") {
    filtered <- filtered[filtered$rank == as.numeric(rank_filter), ]
  }

  # Apply quality filter - Fix the logical comparison
  quality_score <- as.numeric(quality_filter)
  if (!is.null(quality_filter) && !is.na(quality_score) && quality_score > 0) {
    filtered <- filtered[filtered$quality_score >= quality_score, ]
  }

  # Apply criteria filter
  if (!is.null(criteria_filter) && length(criteria_filter) > 0) {
    filtered <- filtered[sapply(filtered$criteria_met, function(x) {
      if (is.na(x) || x == "") return(FALSE)
      criteria_list <- strsplit(x, "; ")[[1]]
      all(criteria_filter %in% criteria_list)
    }), ]
  }

  filtered
}

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
          class = "specimen-table-container mb-4",
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

  prepared_data <- prepare_module_data(
    data = data,
    current_selections = NULL,
    current_flags = NULL,
    current_notes = NULL
  )

  dt <- format_specimen_table(
    data = prepared_data,
    ns = ns,
    buttons = c('copy', 'csv', 'excel'),
    page_length = 50,
    selection = 'none'
  )

  if(grade == "E" && !is.null(dt)) {
    unique_species <- sort(unique(as.character(prepared_data$species)))
    n_species <- length(unique_species)
    colors <- colorRampPalette(c("#e6f3ff", "#80bfff"))(n_species)

    dt <- dt %>% formatStyle(
      'species',
      backgroundColor = styleEqual(
        levels = unique_species,
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

#' Prepare specimen data for download
#' @param specimens Data frame of specimens
#' @param grade BAGS grade
#' @param selections List of selected specimens
#' @param flags List of specimen flags
#' @return Formatted data frame for download
#' @keywords internal
prepare_download_data <- function(specimens, grade, selections, flags) {
  if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

  # Add selection and flag status
  specimens$selected <- specimens$processid %in% names(selections)
  specimens$flag <- sapply(specimens$processid, function(pid) flags[[pid]]$flag %||% "")

  # Order columns
  cols <- c(
    "processid", "species", "bin_uri", "quality_score",
    "criteria_met", "selected", "flag",
    setdiff(names(specimens), c(
      "processid", "species", "bin_uri", "quality_score",
      "criteria_met", "selected", "flag"
    ))
  )

  specimens[, cols]
}

#' Find specimen in nested groups
#' @param groups Nested list of specimen groups
#' @param processid Process ID to find
#' @return Specimen data frame row or NULL if not found
#' @keywords internal
find_specimen_in_groups <- function(groups, processid) {
  for(group in groups) {
    if(is.data.frame(group)) {
      specimen <- group[group$processid == processid,]
      if(nrow(specimen) > 0) return(specimen)
    } else if(is.list(group)) {
      result <- find_specimen_in_groups(group, processid)
      if(!is.null(result)) return(result)
    }
  }
  NULL
}

#' Calculate selection metrics for grade
#' @param selections List of selections
#' @param specimens Data frame of specimens
#' @return List of selection metrics
#' @keywords internal
calculate_selection_metrics <- function(selections, specimens) {
  if (is.null(selections) || length(selections) == 0) {
    return(list(
      total_selected = 0,
      percent_selected = 0,
      avg_quality_selected = 0
    ))
  }

  selected_specimens <- specimens[specimens$processid %in% names(selections), ]

  list(
    total_selected = length(selections),
    percent_selected = length(selections) / nrow(specimens) * 100,
    avg_quality_selected = mean(selected_specimens$quality_score, na.rm = TRUE)
  )
}

#' Calculate flag metrics
#' @param flags List of flags
#' @param specimens Data frame of specimens
#' @return List of flag metrics
#' @keywords internal
calculate_flag_metrics <- function(flags, specimens) {
  if (is.null(flags) || length(flags) == 0) {
    return(list(
      total_flagged = 0,
      flags_by_type = table(character(0)),
      percent_flagged = 0
    ))
  }

  flag_types <- sapply(flags, function(x) x$flag)

  list(
    total_flagged = length(flags),
    flags_by_type = table(flag_types),
    percent_flagged = length(flags) / nrow(specimens) * 100
  )
}

#' Validate specimen selection
#' @param processid Process ID to validate
#' @param specimens Data frame of specimens
#' @return List with validation results
#' @keywords internal
validate_specimen_selection <- function(processid, specimens) {
  if (!processid %in% specimens$processid) {
    return(list(valid = FALSE, message = "Invalid specimen ID"))
  }

  specimen <- specimens[specimens$processid == processid, ]

  # Add any specific validation rules here
  # For example, checking quality score thresholds

  list(valid = TRUE, message = NULL)
}
