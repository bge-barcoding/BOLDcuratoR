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
      private$logger$info("Starting specimen scoring")

      # Ensure scoring criteria are loaded
      if (is.null(private$scoring_criteria)) {
        private$load_scoring_criteria()
        if (is.null(private$scoring_criteria)) {
          private$logger$error("Failed to load scoring criteria")
          return(NULL)
        }
      }

      # Initialize scoring columns
      specimens$quality_score <- NA_real_
      specimens$criteria_met <- NA_character_

      # Score each specimen
      for (i in 1:nrow(specimens)) {
        tryCatch({
          score_result <- private$calculate_specimen_score(specimens[i,])
          specimens$quality_score[i] <- score_result$score
          specimens$criteria_met[i] <- score_result$criteria_met
        }, error = function(e) {
          private$logger$warn(sprintf(
            "Error scoring specimen %s: %s",
            specimens$processid[i],
            e$message
          ))
          specimens$quality_score[i] <- 0
          specimens$criteria_met[i] <- ""
        })
      }

      # Convert and validate scores
      specimens$quality_score <- as.numeric(specimens$quality_score)
      specimens$quality_score[is.na(specimens$quality_score)] <- 0

      # Validate results
      validation <- private$validate_scoring_results(specimens)
      if (!validation$valid) {
        private$logger$warn(sprintf(
          "Scoring validation issues: %s",
          paste(validation$messages, collapse = "; ")
        ))
      }

      specimens
    }
  ),

  private = list(
    logger = NULL,
    error_boundary = NULL,
    scoring_criteria = NULL,

    #' @description Load scoring criteria from global environment
    load_scoring_criteria = function() {
      tryCatch({
        # Force reload from global environment
        private$scoring_criteria <- get("SPECIMEN_SCORING_CRITERIA", envir = .GlobalEnv)
        if (is.null(private$scoring_criteria)) {
          private$logger$error("SPECIMEN_SCORING_CRITERIA not found in global environment")
        }
      }, error = function(e) {
        private$logger$error(sprintf("Error loading scoring criteria: %s", e$message))
        private$scoring_criteria <- NULL
      })
    },

    #' @description Calculate score for a single specimen
    #' @param specimen Single specimen record
    #' @return List with score and criteria met
    calculate_specimen_score = function(specimen) {
      if (is.null(private$scoring_criteria)) {
        return(list(score = 0, criteria_met = ""))
      }

      score <- 0
      criteria_met <- character()

      # Check each criterion
      for (criterion_name in names(private$scoring_criteria)) {
        criterion <- private$scoring_criteria[[criterion_name]]

        if (private$check_criterion(specimen, criterion_name, criterion)) {
          score <- score + 1
          criteria_met <- c(criteria_met, criterion_name)
        }
      }

      list(
        score = score,
        criteria_met = ifelse(length(criteria_met) > 0,
                              paste(criteria_met, collapse = "; "),
                              "")
      )
    },

    #' @description Check if a specimen meets a criterion
    #' @param specimen Specimen record
    #' @param criterion_name Name of criterion
    #' @param criterion_rules Rules for criterion
    #' @return Boolean indicating if criterion is met
    check_criterion = function(specimen, criterion_name, criterion_rules) {
      # Get field values
      field_values <- sapply(criterion_rules$fields, function(field) {
        if (!field %in% names(specimen)) return(NA_character_)
        as.character(specimen[[field]])
      })
      names(field_values) <- criterion_rules$fields

      if (all(is.na(field_values))) return(FALSE)

      # Apply criterion-specific checks
      switch(criterion_name,
             "SPECIES_ID" = private$check_species_id(field_values, criterion_rules),
             "TYPE_SPECIMEN" = private$check_type_specimen(field_values, criterion_rules),
             "SEQ_QUALITY" = private$check_sequence_quality(field_values, criterion_rules),
             "PUBLIC_VOUCHER" = private$check_public_voucher(field_values, criterion_rules),
             "HAS_IMAGE" = private$check_has_image(field_values, criterion_rules),
             private$check_general_criterion(field_values, criterion_rules))
    },

    #' @description Check species ID criterion
    check_species_id = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      species_name <- values[1]
      !is.na(species_name) &&
        nchar(trimws(species_name)) > 0 &&
        !grepl(rules$negative_pattern, species_name, ignore.case = TRUE)
    },

    #' @description Check type specimen criterion
    check_type_specimen = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      # Special check for "type" in voucher_type field
      if (!is.na(values["voucher_type"]) &&
          grepl("type", tolower(values["voucher_type"]), ignore.case = TRUE)) {
        return(TRUE)
      }

      # Check for type terms in all fields
      any(sapply(values, function(value) {
        !is.na(value) && value != "" &&
          grepl(rules$positive_pattern, tolower(value), ignore.case = TRUE)
      }))
    },

    #' @description Check sequence quality criterion
    check_sequence_quality = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      # Check BIN existence and sequence length
      has_bin <- !is.na(values["bin_uri"]) && values["bin_uri"] != ""
      has_length <- !is.na(values["nuc_basecount"]) &&
        suppressWarnings(as.numeric(values["nuc_basecount"])) >= rules$min_length

      has_bin && has_length
    },

    #' @description Check public voucher criterion
    check_public_voucher = function(values, rules) {
      if (all(is.na(values))) return(FALSE)

      voucher_value <- tolower(trimws(values[1]))
      !is.na(voucher_value) && voucher_value != "" &&
        !grepl(rules$negative_pattern, voucher_value, ignore.case = TRUE) &&
        (is.null(rules$positive_pattern) ||
           grepl(rules$positive_pattern, voucher_value, ignore.case = TRUE))
    },

    #' @description Check if specimen has an image (based on API lookup)
    check_has_image = function(values, rules) {
      val <- values[["has_image"]]
      if (is.na(val)) return(FALSE)
      # has_image is stored as logical TRUE/FALSE; as.character gives "TRUE"/"FALSE"
      isTRUE(as.logical(val))
    },

    #' @description Check general criterion
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
        if (!is.null(rules$positive_pattern)) {
          return(grepl(rules$positive_pattern, value, ignore.case = TRUE))
        }

        TRUE
      }))
    },

    #' @description Validate scoring results
    #' @param specimens Scored specimens data frame
    #' @return List with validation results
    validate_scoring_results = function(specimens) {
      messages <- character()

      # Check for missing scores
      missing_scores <- sum(is.na(specimens$quality_score))
      if (missing_scores > 0) {
        messages <- c(messages,
                      sprintf("%d specimens missing quality scores", missing_scores))
      }

      # Validate score range
      max_score <- length(private$scoring_criteria)
      invalid_scores <- sum(!is.na(specimens$quality_score) &
                              (specimens$quality_score < 0 |
                                 specimens$quality_score > max_score))
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
