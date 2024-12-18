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
    filtered <- filtered[filtered$specimen_rank == as.numeric(rank_filter), ]
  }

  # Apply quality filter
  if (!is.null(quality_filter) && quality_filter > 0) {
    filtered <- filtered[filtered$quality_score >= quality_filter, ]
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

#' Organize specimens by grade
#' @param specimens Data frame of specimens
#' @param grade BAGS grade
#' @return Organized list of specimens
#' @keywords internal
organize_grade_specimens <- function(specimens, grade) {
  if (is.null(specimens) || nrow(specimens) == 0) return(list())

  # Sort specimens by quality score within groups
  specimens <- specimens[order(-specimens$quality_score), ]

  switch(grade,
         # Grades A, B, D: Group by species
         "A" = split(specimens, specimens$species),
         "B" = split(specimens, specimens$species),
         "D" = split(specimens, specimens$species),
         # Grade C: Group by species then BIN
         "C" = {
           species_groups <- split(specimens, specimens$species)
           lapply(species_groups, function(species_group) {
             split(species_group, species_group$bin_uri)
           })
         },
         # Grade E: Group by shared BINs
         "E" = {
           bins <- unique(specimens$bin_uri)
           shared_bins <- list()

           for(bin in bins) {
             bin_specimens <- specimens[specimens$bin_uri == bin,]
             if(length(unique(bin_specimens$species)) > 1) {
               shared_bins[[bin]] <- bin_specimens[order(-bin_specimens$quality_score),]
             }
           }
           shared_bins
         },
         # Default case
         list()
  )
}

#' Calculate BIN sharing metrics for grade E specimens
#' @param specimens Data frame of specimens
#' @return List of BIN sharing metrics
#' @keywords internal
calculate_bin_sharing_metrics <- function(specimens) {
  if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

  # Get BIN sharing data
  bin_species <- tapply(specimens$species, specimens$bin_uri, function(x) {
    length(unique(x))
  })

  list(
    total_shared_bins = length(bin_species),
    max_species_per_bin = max(bin_species),
    avg_species_per_bin = mean(bin_species),
    bin_distribution = table(bin_species)
  )
}

#' Create color scheme for species
#' @param species Vector of species names
#' @return Named vector of colors
#' @keywords internal
create_species_colors <- function(species) {
  if (length(species) == 0) return(NULL)

  unique_species <- unique(species)

  # Define color palette for different numbers of species
  if (length(unique_species) <= 3) {
    colors <- c("#e6f3ff", "#cce6ff", "#b3d9ff")
  } else if (length(unique_species) <= 5) {
    colors <- c("#e6f3ff", "#cce6ff", "#b3d9ff", "#99ccff", "#80bfff")
  } else {
    # For more species, generate colors programmatically
    colors <- colorRampPalette(c("#e6f3ff", "#80bfff"))(length(unique_species))
  }

  setNames(colors[1:length(unique_species)], unique_species)
}

#' Format specimen flags for display
#' @param flags List of specimen flags
#' @return Character vector of formatted flags
#' @keywords internal
format_specimen_flags <- function(flags) {
  if (is.null(flags) || length(flags) == 0) return(character(0))

  sapply(flags, function(flag) {
    if (is.null(flag) || is.na(flag) || flag == "") {
      return("")
    }
    switch(flag,
           "Misidentified" = "Misidentified",
           "ID uncertain" = "ID uncertain",
           ""
    )
  })
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

#' Format grade-specific data for display
#' @param specimens Data frame of specimens
#' @param grade BAGS grade
#' @return Formatted data frame
#' @keywords internal
format_grade_data <- function(specimens, grade) {
  # Add species rank summary
  specimens$species_rank <- factor(sapply(specimens$species, function(sp) {
    if (is.na(sp) || sp == "") return("No ID")
    if (grepl("cf\\.|aff\\.", sp)) return("cf./aff.")
    if (grepl("sp\\.|spp\\.", sp)) return("sp./spp.")
    "Species"
  }))

  specimens
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
    "specimen_rank", "criteria_met", "selected", "flag",
    setdiff(names(specimens), c(
      "processid", "species", "bin_uri", "quality_score",
      "specimen_rank", "criteria_met", "selected", "flag"
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
