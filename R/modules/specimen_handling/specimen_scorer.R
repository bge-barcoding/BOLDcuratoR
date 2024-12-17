# R/modules/specimen_handling/specimen_scorer.R

#' @title SpecimenScorer R6 Class
#' @description Handles specimen scoring and quality metrics
#' @export
SpecimenScorer <- R6::R6Class(
  "SpecimenScorer",

  public = list(
    #' @description Initialize the scorer
    #' @param logger Logger instance
    initialize = function(logger) {
      private$logger <- logger
      private$error_boundary <- ErrorBoundary$new()
      private$load_scoring_criteria()
    },

    #' @description Score a set of specimens
    #' @param specimens Data frame of specimens
    #' @return Scored specimen data frame
    score_specimens = function(specimens) {
      private$error_boundary$catch({
        if (is.null(specimens) || nrow(specimens) == 0) {
          private$logger$warn("Empty specimen data provided for scoring")
          return(NULL)
        }

        private$logger$info(sprintf("Scoring %d specimens", nrow(specimens)))

        # Initialize score columns
        specimens$quality_score <- as.numeric(NA)
        specimens$criteria_met <- as.character(NA)

        # Score each specimen
        for (i in 1:nrow(specimens)) {
          score_result <- private$calculate_specimen_score(specimens[i,])
          specimens$quality_score[i] <- score_result$score
          specimens$criteria_met[i] <- score_result$criteria_met
        }

        # Validate scoring results
        validation <- private$validate_scoring_results(specimens)
        if (!validation$valid) {
          private$logger$error("Scoring validation failed", validation$messages)
          return(NULL)
        }

        private$logger$info("Specimen scoring complete")
        specimens
      })
    }
  ),

  private = list(
    logger = NULL,
    error_boundary = NULL,
    scoring_criteria = NULL,

    load_scoring_criteria = function() {
      private$scoring_criteria <- SPECIMEN_SCORING_CRITERIA
    },

    calculate_specimen_score = function(specimen) {
      score <- 0
      criteria_met <- character()

      for (criterion_name in names(private$scoring_criteria)) {
        criterion <- private$scoring_criteria[[criterion_name]]

        if (private$check_criterion(specimen, criterion_name, criterion)) {
          score <- score + 1
          criteria_met <- c(criteria_met, criterion_name)
        }
      }

      list(
        score = score,
        criteria_met = paste(criteria_met, collapse = "; ")
      )
    },

    check_criterion = function(specimen, criterion_name, criterion_rules) {
      # Get field values
      field_values <- sapply(criterion_rules$fields, function(field) {
        if (!field %in% names(specimen)) return(NA)
        specimen[[field]]
      })

      if (all(is.na(field_values))) return(FALSE)

      switch(criterion_name,
             "SPECIES_ID" = private$check_species_id(field_values, criterion_rules),
             "TYPE_SPECIMEN" = private$check_type_specimen(field_values, criterion_rules),
             "SEQ_QUALITY" = private$check_sequence_quality(field_values, criterion_rules),
             "PUBLIC_VOUCHER" = private$check_public_voucher(field_values, criterion_rules),
             private$check_general_criterion(field_values, criterion_rules))
    },

    check_species_id = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      species_name <- values[1]
      !is.na(species_name) &&
        nchar(trimws(species_name)) > 0 &&
        !grepl(rules$negative_pattern, species_name)
    },

    check_type_specimen = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      # Special check for "type" in voucher_type field
      if (!is.na(values["voucher_type"]) &&
          grepl("type", tolower(values["voucher_type"]), ignore.case = TRUE)) {
        return(TRUE)
      }

      # Standard check for type terms in all fields
      any(sapply(values, function(value) {
        !is.na(value) && value != "" &&
          grepl(rules$positive_pattern, tolower(value), ignore.case = TRUE)
      }))
    },

    check_sequence_quality = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      has_bin <- !is.na(values["bin_uri"]) && values["bin_uri"] != ""
      has_length <- !is.na(values["nuc_basecount"]) &&
        as.numeric(values["nuc_basecount"]) >= rules$min_length

      has_bin && has_length
    },

    check_public_voucher = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      voucher_value <- tolower(trimws(values[1]))
      !is.na(voucher_value) && voucher_value != "" &&
        !grepl(rules$negative_pattern, voucher_value, ignore.case = TRUE) &&
        (is.null(rules$positive_pattern) ||
           grepl(rules$positive_pattern, voucher_value, ignore.case = TRUE))
    },

    check_general_criterion = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      any(sapply(values, function(value) {
        if (is.na(value) || value == "") return(FALSE)

        # Check negative pattern if it exists
        if (!is.null(rules$negative_pattern) &&
            grepl(rules$negative_pattern, value, ignore.case = TRUE)) {
          return(FALSE)
        }

        # Check positive pattern if it exists
        if (!is.null(rules$positive_pattern) &&
            !grepl(rules$positive_pattern, value, ignore.case = TRUE)) {
          return(FALSE)
        }

        TRUE
      }))
    },

    validate_scoring_results = function(specimens) {
      messages <- character()

      # Check for missing scores
      missing_scores <- sum(is.na(specimens$quality_score))
      if (missing_scores > 0) {
        messages <- c(messages,
                      sprintf("%d specimens missing quality scores", missing_scores))
      }

      # Validate score range
      invalid_scores <- sum(!is.na(specimens$quality_score) &
                              (specimens$quality_score < 0 |
                                 specimens$quality_score > length(private$scoring_criteria)))
      if (invalid_scores > 0) {
        messages <- c(messages,
                      sprintf("%d specimens have invalid quality scores", invalid_scores))
      }

      list(
        valid = length(messages) == 0,
        messages = messages
      )
    }
  )
)
