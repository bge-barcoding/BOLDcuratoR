# R/modules/specimen_handling/specimen_validator.R

#' @title SpecimenValidator R6 Class
#' @description Validates and flags specimens for different analysis types
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

    #' @description Process and flag specimens
    #' @param specimens Data frame of specimen data
    #' @return Processed specimen data frame with validation flags
    validate_specimens = function(specimens) {
      private$error_boundary$catch({
        if (is.null(specimens) || nrow(specimens) == 0) {
          private$logger$warn("Empty specimen data provided for validation")
          return(NULL)
        }

        private$logger$info(sprintf("Validating %d specimens", nrow(specimens)))

        # Validation flag - valid species names without sp. cf. etc
        specimens$valid_species <- !is.na(specimens$species) &
          specimens$species != "" &
          !grepl("sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.", specimens$species)

        # Validation flag - species identified above species level
        specimens$species_id <- !is.na(specimens$species) & specimens$species != ""

        # Validation flag - record in a BIN
        specimens$valid_bin <- !is.na(specimens$bin_uri) &
          specimens$bin_uri != ""

        # Log validation results
        n_species_valid <- sum(specimens$valid_species)
        n_species_id <- sum(specimens$species_id)
        n_bin_valid <- sum(specimens$valid_bin)

        private$logger$info(sprintf("Validation complete: %d/%d valid species names, %d/%d species level IDs, %d/%d valid BINs",
                                    n_species_valid, nrow(specimens),
                                    n_species_id, nrow(specimens),
                                    n_bin_valid, nrow(specimens)
        ))

        specimens
      })
    },

    #' @description Get specimens valid for BIN analysis
    #' @param specimens Data frame of specimens
    #' @return Specimens with valid BINs
    get_bin_valid = function(specimens) {
      specimens[specimens$valid_bin,]
    },

    #' @description Get specimens valid for BAGS analysis
    #' @param specimens Data frame of specimens
    #' @return Specimens with valid species names
    get_species_id = function(specimens) {
      specimens[specimens$species_id,]
    }
  ),

  private = list(
    logger = NULL,
    error_boundary = NULL
  )
)
