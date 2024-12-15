# R/utils/specimen_validation.R

#' Validate specimen data
#' @param specimen Single specimen record
#' @return List with validation results
#' @export
validate_specimen <- function(specimen) {
  tryCatch({
    # Initialize validation result
    validation <- list(
      valid = TRUE,
      messages = character(0),
      warnings = character(0)
    )

    # Required fields validation
    required_fields <- c("processid", "species", "bin_uri")
    missing_fields <- required_fields[!required_fields %in% names(specimen)]

    if (length(missing_fields) > 0) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               sprintf("Missing required fields: %s",
                                       paste(missing_fields, collapse = ", ")))
    }

    # Data type validation
    validation <- c(validation, validate_data_types(specimen))

    # Content validation
    validation <- c(validation, validate_content(specimen))

    # Return combined validation results
    list(
      valid = validation$valid,
      messages = unique(validation$messages),
      warnings = unique(validation$warnings)
    )
  }, error = function(e) {
    list(
      valid = FALSE,
      messages = sprintf("Validation error: %s", e$message),
      warnings = character(0)
    )
  })
}

#' Validate specimen criteria
#' @param criteria Character vector of criteria
#' @return List with validation results
#' @export
validate_criteria <- function(criteria) {
  tryCatch({
    validation <- list(
      valid = TRUE,
      messages = character(0)
    )

    if (is.null(criteria) || length(criteria) == 0) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               "No criteria provided")
      return(validation)
    }

    # Get valid criteria from constants
    valid_criteria <- unlist(SPECIMEN_RANK_CRITERIA)

    # Check for invalid criteria
    invalid_criteria <- setdiff(criteria, valid_criteria)
    if (length(invalid_criteria) > 0) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               sprintf("Invalid criteria found: %s",
                                       paste(invalid_criteria, collapse = ", ")))
    }

    validation
  }, error = function(e) {
    list(
      valid = FALSE,
      messages = sprintf("Criteria validation error: %s", e$message)
    )
  })
}

#' Validate quality metrics
#' @param metrics List of quality metrics
#' @return List with validation results
#' @export
validate_quality_metrics <- function(metrics) {
  tryCatch({
    validation <- list(
      valid = TRUE,
      messages = character(0)
    )

    required_metrics <- c("quality_score", "specimen_rank", "criteria_met")

    # Check for missing metrics
    missing_metrics <- required_metrics[!required_metrics %in% names(metrics)]
    if (length(missing_metrics) > 0) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               sprintf("Missing required metrics: %s",
                                       paste(missing_metrics, collapse = ", ")))
    }

    # Validate metric values if present
    if ("quality_score" %in% names(metrics)) {
      if (!is.numeric(metrics$quality_score) ||
          metrics$quality_score < 0 ||
          metrics$quality_score > 14) {
        validation$valid <- FALSE
        validation$messages <- c(validation$messages,
                                 "Invalid quality score (must be numeric between 0-14)")
      }
    }

    if ("specimen_rank" %in% names(metrics)) {
      if (!is.numeric(metrics$specimen_rank) ||
          !metrics$specimen_rank %in% 1:7) {
        validation$valid <- FALSE
        validation$messages <- c(validation$messages,
                                 "Invalid specimen rank (must be between 1-7)")
      }
    }
    validation
  }, error = function(e) {
    list(
      valid = FALSE,
      messages = sprintf("Quality metrics validation error: %s", e$message)
    )
  })
}

#' Validate specimen data types
#' @param specimen Specimen record
#' @return List with validation results
#' @keywords internal
validate_data_types <- function(specimen) {
  validation <- list(
    valid = TRUE,
    messages = character(0)
  )

  # Validate processid
  if (!is.null(specimen$processid) && !is.character(specimen$processid)) {
    validation$valid <- FALSE
    validation$messages <- c(validation$messages, "processid must be character type")
  }

  # Validate species
  if (!is.null(specimen$species) && !is.character(specimen$species)) {
    validation$valid <- FALSE
    validation$messages <- c(validation$messages, "species must be character type")
  }

  # Validate bin_uri
  if (!is.null(specimen$bin_uri) && !is.character(specimen$bin_uri)) {
    validation$valid <- FALSE
    validation$messages <- c(validation$messages, "bin_uri must be character type")
  }

  validation
}

#' Validate specimen content
#' @param specimen Specimen record
#' @return List with validation results
#' @keywords internal
validate_content <- function(specimen) {
  validation <- list(
    valid = TRUE,
    messages = character(0),
    warnings = character(0)
  )

  # Species name validation
  if (!is.null(specimen$species) && !is.na(specimen$species)) {
    species_name <- trimws(specimen$species)
    if (grepl("sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.", species_name)) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               "Invalid species name format")
    }
  }

  # BIN validation
  if (!is.null(specimen$bin_uri) && !is.na(specimen$bin_uri)) {
    if (!grepl("^BOLD:", specimen$bin_uri)) {
      validation$warnings <- c(validation$warnings,
                               "BIN URI may not be in correct format")
    }
  }

  # Quality scores validation
  if (!is.null(specimen$quality_score) && !is.na(specimen$quality_score)) {
    if (!is.numeric(specimen$quality_score) ||
        specimen$quality_score < 0 ||
        specimen$quality_score > 14) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               "Invalid quality score (must be between 0-14)")
    }
  }

  # Rank validation
  if (!is.null(specimen$specimen_rank) && !is.na(specimen$specimen_rank)) {
    if (!is.numeric(specimen$specimen_rank) ||
        !specimen$specimen_rank %in% 1:7) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               "Invalid specimen rank (must be between 1-7)")
    }
  }

  # Criteria validation
  if (!is.null(specimen$criteria_met) && !is.na(specimen$criteria_met)) {
    criteria <- strsplit(specimen$criteria_met, "; ")[[1]]
    validation_result <- validate_criteria(criteria)
    if (!validation_result$valid) {
      validation$valid <- FALSE
      validation$messages <- c(validation$messages,
                               validation_result$messages)
    }
  }

  validation
}
