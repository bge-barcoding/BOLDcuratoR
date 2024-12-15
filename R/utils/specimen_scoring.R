# R/utils/specimen_scoring.R

#' Calculate quality score for a specimen
#' @param specimen Single specimen record
#' @return List containing score and criteria met
#' @export
calculate_quality_score <- function(specimen) {
  tryCatch({
    score <- 0
    criteria_met <- character()

    # Load scoring criteria
    scoring_criteria <- get_scoring_criteria()

    # Check each criterion
    for (criterion in names(scoring_criteria)) {
      if (check_criterion(specimen, criterion, scoring_criteria[[criterion]])) {
        score <- score + 1
        criteria_met <- c(criteria_met, criterion)
      }
    }

    # Validate results
    validation <- validate_quality_score(score, criteria_met)
    if (!validation$valid) {
      warning(paste(validation$messages, collapse = "; "))
    }

    list(
      score = score,
      percentage = round((score / length(scoring_criteria)) * 100, 1),
      criteria_met = paste(criteria_met, collapse = "; ")
    )
  }, error = function(e) {
    warning(sprintf("Error calculating quality score: %s", e$message))
    list(score = 0, percentage = 0, criteria_met = "")
  })
}

#' Get scoring criteria from constants
#' @return List of scoring criteria
#' @keywords internal
get_scoring_criteria <- function() {
  SPECIMEN_SCORING_CRITERIA
}

#' Validate quality score calculation
#' @param score Numeric score
#' @param criteria_met Vector of met criteria
#' @return List with validation results
#' @keywords internal
validate_quality_score <- function(score, criteria_met) {
  messages <- character()

  # Validate score range
  if (!is.numeric(score) || score < 0 ||
      score > length(SPECIMEN_SCORING_CRITERIA)) {
    messages <- c(messages, "Invalid score value")
  }

  # Validate criteria
  if (length(criteria_met) > 0) {
    invalid_criteria <- setdiff(criteria_met, names(SPECIMEN_SCORING_CRITERIA))
    if (length(invalid_criteria) > 0) {
      messages <- c(messages,
                    sprintf("Invalid criteria detected: %s",
                            paste(invalid_criteria, collapse = ", ")))
    }
  }

  # Check score-criteria consistency
  if (length(criteria_met) != score) {
    messages <- c(messages, "Score does not match number of criteria met")
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}

#' Check if a specimen meets a specific criterion
#' @param specimen Specimen record
#' @param criterion_name Criterion name
#' @param rules List of rules for the criterion
#' @return Boolean indicating if criterion is met
#' @keywords internal
check_criterion <- function(specimen, criterion_name, rules) {
  tryCatch({
    field_values <- sapply(rules$fields, function(field) {
      if (!field %in% names(specimen)) return(NA)
      specimen[[field]]
    })

    if (all(is.na(field_values))) return(FALSE)

    switch(criterion_name,
           "SPECIES_ID" = check_species_id(field_values, rules),
           "TYPE_SPECIMEN" = check_type_specimen(field_values, rules),
           "SEQ_QUALITY" = check_sequence_quality(field_values, rules),
           "PUBLIC_VOUCHER" = check_public_voucher(field_values, rules),
           check_general_criterion(field_values, rules))
  }, error = function(e) {
    warning(sprintf("Error checking criterion %s: %s", criterion_name, e$message))
    FALSE
  })
}

#' Check species ID criterion
#' @param values Field values
#' @param rules Criterion rules
#' @return Boolean indicating if criterion is met
#' @keywords internal
check_species_id <- function(values, rules) {
  if (all(is.na(values))) return(FALSE)

  species_name <- values[1]
  !is.na(species_name) &&
    nchar(trimws(species_name)) > 0 &&
    !grepl(rules$negative_pattern, species_name)
}

#' Check type specimen criterion
#' @param values Field values
#' @param rules Criterion rules
#' @return Boolean indicating if criterion is met
#' @keywords internal
check_type_specimen <- function(values, rules) {
  if (all(is.na(values))) return(FALSE)

  any(sapply(values, function(value) {
    !is.na(value) && value != "" &&
      grepl(rules$positive_pattern, tolower(value), ignore.case = TRUE)
  }))
}

#' Check sequence quality criterion
#' @param values Field values
#' @param rules Criterion rules
#' @return Boolean indicating if criterion is met
#' @keywords internal
check_sequence_quality <- function(values, rules) {
  if (all(is.na(values))) return(FALSE)

  valid_bin <- !is.na(values["bin_uri"]) && values["bin_uri"] != ""
  valid_length <- !is.na(values["nuc_basecount"]) &&
    as.numeric(values["nuc_basecount"]) >= rules$min_length

  valid_bin && valid_length
}

#' Check public voucher criterion
#' @param values Field values
#' @param rules Criterion rules
#' @return Boolean indicating if criterion is met
#' @keywords internal
check_public_voucher <- function(values, rules) {
  if (all(is.na(values))) return(FALSE)

  voucher_value <- tolower(trimws(values[1]))
  !is.na(voucher_value) && voucher_value != "" &&
    !grepl(rules$negative_pattern, voucher_value, ignore.case = TRUE) &&
    grepl(rules$positive_pattern, voucher_value, ignore.case = TRUE)
}

#' Check general criterion
#' @param values Field values
#' @param rules Criterion rules
#' @return Boolean indicating if criterion is met
#' @keywords internal
check_general_criterion <- function(values, rules) {
  if (all(is.na(values))) return(FALSE)

  any(sapply(values, function(value) {
    !is.na(value) && value != "" &&
      (!is.null(rules$negative_pattern) &&
         !grepl(rules$negative_pattern, value, ignore.case = TRUE)) &&
      (is.null(rules$positive_pattern) ||
         grepl(rules$positive_pattern, value, ignore.case = TRUE))
  }))
}
