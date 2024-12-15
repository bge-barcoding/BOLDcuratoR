# R/modules/specimen_handling/specimen_validator.R

#' @title SpecimenValidator R6 Class
#' @description Validates specimen data and criteria
#' @export
SpecimenValidator <- R6::R6Class(
  "SpecimenValidator",

  public = list(
    #' @description Initialize the validator
    #' @param logger Logger instance
    initialize = function(logger) {
      private$logger <- logger
      private$error_boundary <- ErrorBoundary$new()
    },

    #' @description Validate specimen data
    #' @param specimens Data frame of specimen data
    #' @return Validated specimen data frame or NULL if validation fails
    validate_specimens = function(specimens) {
      private$error_boundary$catch({
        # Basic validation
        if (is.null(specimens) || nrow(specimens) == 0) {
          private$logger$warn("Empty specimen data provided for validation")
          return(NULL)
        }

        # Check required columns
        validation <- private$validate_required_columns(specimens)
        if (!validation$valid) {
          private$logger$error("Missing required columns", validation$messages)
          return(NULL)
        }

        # Validate data types
        validation <- private$validate_data_types(specimens)
        if (!validation$valid) {
          private$logger$error("Invalid data types", validation$messages)
          return(NULL)
        }

        # Validate species names
        valid_specimens <- private$validate_species_names(specimens)
        if (nrow(valid_specimens) == 0) {
          private$logger$warn("No specimens with valid species names found")
          return(NULL)
        }

        private$logger$info(sprintf("Validated %d specimens (%d invalid specimens removed)",
                                    nrow(valid_specimens),
                                    nrow(specimens) - nrow(valid_specimens)))

        valid_specimens
      })
    },

    #' @description Validate specimen criteria
    #' @param criteria Character vector of criteria
    #' @return Logical indicating if criteria are valid
    validate_criteria = function(criteria) {
      private$error_boundary$catch({
        if (is.null(criteria) || length(criteria) == 0) return(FALSE)

        # Get valid criteria from constants
        valid_criteria <- unlist(SPECIMEN_RANK_CRITERIA)

        # Check if all provided criteria are valid
        all(criteria %in% valid_criteria)
      })
    },

    #' @description Validate specimen quality metrics
    #' @param specimens Data frame of specimens
    #' @param min_quality Minimum required quality score
    #' @param max_rank Maximum acceptable rank
    #' @return Data frame of specimens meeting quality criteria
    validate_quality = function(specimens, min_quality = 0, max_rank = 7) {
      private$error_boundary$catch({
        if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

        # Validate score range
        if (!all(is.na(specimens$quality_score) |
                 (specimens$quality_score >= 0 & specimens$quality_score <= 14))) {
          private$logger$error("Invalid quality scores detected")
          return(NULL)
        }

        # Validate rank range
        if (!all(is.na(specimens$specimen_rank) |
                 (specimens$specimen_rank >= 1 & specimens$specimen_rank <= 7))) {
          private$logger$error("Invalid ranks detected")
          return(NULL)
        }

        # Apply quality filters
        quality_specimens <- specimens
        if (!is.null(min_quality) && min_quality > 0) {
          quality_specimens <- quality_specimens[
            quality_specimens$quality_score >= min_quality,
          ]
        }
        if (!is.null(max_rank) && max_rank < 7) {
          quality_specimens <- quality_specimens[
            quality_specimens$specimen_rank <= max_rank,
          ]
        }

        private$logger$info(sprintf("Quality validation: %d specimens meet criteria",
                                    nrow(quality_specimens)))

        quality_specimens
      })
    }
  ),

  private = list(
    logger = NULL,
    error_boundary = NULL,

    #' @description Validate required columns presence
    #' @param specimens Data frame of specimens
    #' @return List with validation results
    validate_required_columns = function(specimens) {
      required_cols <- c("processid", "species", "bin_uri")
      missing_cols <- setdiff(required_cols, names(specimens))

      list(
        valid = length(missing_cols) == 0,
        messages = if (length(missing_cols) > 0) {
          sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", "))
        } else character()
      )
    },

    #' @description Validate data types of columns
    #' @param specimens Data frame of specimens
    #' @return List with validation results
    validate_data_types = function(specimens) {
      messages <- character()

      # Check processid type
      if (!is.character(specimens$processid)) {
        messages <- c(messages, "processid must be character type")
      }

      # Check species type
      if (!is.character(specimens$species)) {
        messages <- c(messages, "species must be character type")
      }

      # Check bin_uri type
      if (!is.character(specimens$bin_uri)) {
        messages <- c(messages, "bin_uri must be character type")
      }

      list(
        valid = length(messages) == 0,
        messages = messages
      )
    },

    #' @description Validate and clean species names
    #' @param specimens Data frame of specimens
    #' @return Data frame with valid specimens
    validate_species_names = function(specimens) {
      # Remove records with invalid species names
      valid_specimens <- specimens[!is.na(specimens$species) &
                                     specimens$species != "" &
                                     !grepl("sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.",
                                            specimens$species), ]

      # Clean remaining species names
      valid_specimens$species <- trimws(valid_specimens$species)

      valid_specimens
    }
  )
)
