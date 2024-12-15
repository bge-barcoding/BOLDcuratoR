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

#' Get species with grade
#' @param selected_specimens List of selected specimens
#' @param grade_species Vector of species in grade
#' @return Vector of species with grade
#' @keywords internal
get_species_with_grade <- function(selected_specimens, grade_species) {
  if (!is.list(selected_specimens)) return(character(0))

  names(selected_specimens)[names(selected_specimens) %in% grade_species]
}

#' Validate specimen selection
#' @param selection_info Selection information list
#' @return Boolean indicating if selection is valid
#' @keywords internal
validate_specimen_selection <- function(selection_info) {
  if (!is.list(selection_info) ||
      !all(c("species", "processid") %in% names(selection_info))) {
    return(FALSE)
  }
  TRUE
}

#' Update specimen selection in state
#' @param state State manager instance
#' @param selection_info Selection information
#' @param grade BAGS grade
#' @keywords internal
update_specimen_selection <- function(state, selection_info, grade) {
  current_selections <- state$get_store()$selected_specimens
  if (!is.list(current_selections)) {
    current_selections <- list()
  }

  current_selections[[selection_info$species]] <- selection_info$processid
  state$update_state("selected_specimens", current_selections)

  user_info <- state$get_store()$user_info
  state$track_activity(
    "specimen_selected",
    list(
      species = selection_info$species,
      processid = selection_info$processid,
      grade = grade,
      user_email = user_info$email,
      user_name = user_info$name
    )
  )
}
