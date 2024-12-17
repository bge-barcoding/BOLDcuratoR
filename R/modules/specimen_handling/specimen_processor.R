# R/modules/specimen_handling/specimen_processor.R

#' @title SpecimenProcessor R6 Class
#' @description Manages specimen processing pipeline including validation, scoring, and ranking
#' @export
SpecimenProcessor <- R6::R6Class(
  "SpecimenProcessor",

  public = list(
    #' @description Initialize the processor
    #' @param validator SpecimenValidator instance
    #' @param scorer SpecimenScorer instance
    #' @param logger Logger instance
    initialize = function(validator, scorer, logger) {
      private$validator <- validator
      private$scorer <- scorer
      private$logger <- logger
      private$error_boundary <- ErrorBoundary$new()
    },

    #' @description Process a set of specimens through the complete pipeline
    #' @param specimens Data frame of specimen data
    #' @return Processed specimen data frame
    process_specimens = function(specimens) {
      private$error_boundary$catch({
        # Initial validation
        if (is.null(specimens) || nrow(specimens) == 0) {
          private$logger$warn("Empty specimen data provided")
          return(NULL)
        }

        private$logger$info(sprintf("Processing %d specimens", nrow(specimens)))

        # Run validation pipeline
        validated <- private$validator$validate_specimens(specimens)
        if (is.null(validated)) {
          private$logger$error("Specimen validation failed")
          return(NULL)
        }

        # Score specimens using consolidated scorer
        tryCatch({
          scored <- private$scorer$score_specimens(validated)
          if (is.null(scored)) {
            private$logger$error("Specimen scoring failed")
            return(NULL)
          }
        }, error = function(e) {
          private$logger$error(sprintf("Scoring error: %s", e$message))
          return(NULL)
        })

        # Rank specimens
        ranked <- private$rank_specimens(scored)
        if (is.null(ranked)) {
          private$logger$error("Specimen ranking failed")
          return(NULL)
        }

        # Calculate quality metrics
        metrics <- private$calculate_quality_metrics(ranked)
        private$last_metrics <- metrics

        private$logger$info(sprintf("Successfully processed %d specimens", nrow(ranked)))
        ranked
      })
    },

    #' @description Get the latest processing metrics
    #' @return List of quality metrics
    get_metrics = function() {
      private$last_metrics
    },

    #' @description Select best specimens based on quality criteria
    #' @param processed_specimens Data frame of processed specimens
    #' @param selection_criteria List of selection criteria
    #' @return List of selected specimen IDs by species
    select_best_specimens = function(processed_specimens, selection_criteria = NULL) {
      private$error_boundary$catch({
        if (is.null(selection_criteria)) {
          selection_criteria <- list(
            min_quality_score = 0,
            max_rank = 7,
            required_criteria = NULL
          )
        }

        selections <- list()
        species_list <- unique(processed_specimens$species)

        for (species in species_list) {
          species_specimens <- processed_specimens[processed_specimens$species == species, ]

          # Apply selection criteria
          valid_specimens <- private$apply_selection_criteria(
            species_specimens,
            selection_criteria
          )

          # Select best specimen
          if (nrow(valid_specimens) > 0) {
            best_specimen <- private$select_best_specimen(valid_specimens)
            selections[[species]] <- best_specimen$processid
          }
        }

        private$logger$info(sprintf("Selected best specimens for %d species",
                                    length(selections)))
        selections
      })
    }
  ),

  private = list(
    validator = NULL,
    scorer = NULL,
    logger = NULL,
    error_boundary = NULL,
    last_metrics = NULL,

    #' @description Rank specimens based on quality score and criteria
    #' @param scored_specimens Data frame of scored specimens
    #' @return Ranked specimen data frame
    rank_specimens = function(scored_specimens) {
      if (is.null(scored_specimens) || nrow(scored_specimens) == 0) {
        return(NULL)
      }

      # Get ranking criteria from constants
      rank_criteria <- SPECIMEN_RANK_CRITERIA

      # Apply ranking based on criteria met
      scored_specimens$specimen_rank <- sapply(scored_specimens$criteria_met, function(criteria) {
        if (is.na(criteria) || criteria == "") return(7)
        criteria_list <- strsplit(criteria, "; ")[[1]]

        for (rank in 1:6) {
          rank_key <- paste0("RANK_", rank)
          if (all(rank_criteria[[rank_key]] %in% criteria_list)) {
            return(rank)
          }
        }
        return(7)
      })

      scored_specimens
    },

    #' @description Calculate quality metrics for specimens
    #' @param specimens Data frame of specimens
    #' @return List of quality metrics
    calculate_quality_metrics = function(specimens) {
      if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

      list(
        total_specimens = nrow(specimens),
        avg_quality_score = mean(specimens$quality_score, na.rm = TRUE),
        median_quality_score = median(specimens$quality_score, na.rm = TRUE),
        rank_distribution = table(specimens$specimen_rank),
        species_count = length(unique(specimens$species)),
        criteria_coverage = private$calculate_criteria_coverage(specimens)
      )
    },

    #' @description Calculate criteria coverage metrics
    #' @param specimens Data frame of specimens
    #' @return List of criteria coverage statistics
    calculate_criteria_coverage = function(specimens) {
      if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

      criteria_counts <- sapply(names(SPECIMEN_SCORING_CRITERIA), function(criterion) {
        sum(grepl(criterion, specimens$criteria_met, fixed = TRUE))
      })

      list(
        total_criteria = length(SPECIMEN_SCORING_CRITERIA),
        criteria_counts = criteria_counts,
        avg_criteria_per_specimen = mean(sapply(specimens$criteria_met, function(x) {
          if (is.na(x) || x == "") return(0)
          length(strsplit(x, "; ")[[1]])
        }))
      )
    },

    #' @description Apply selection criteria to specimens
    #' @param specimens Data frame of specimens
    #' @param criteria Selection criteria list
    #' @return Filtered specimen data frame
    apply_selection_criteria = function(specimens, criteria) {
      valid_specimens <- specimens

      if (!is.null(criteria$min_quality_score)) {
        valid_specimens <- valid_specimens[
          valid_specimens$quality_score >= criteria$min_quality_score,
        ]
      }

      if (!is.null(criteria$max_rank)) {
        valid_specimens <- valid_specimens[
          valid_specimens$specimen_rank <= criteria$max_rank,
        ]
      }

      if (!is.null(criteria$required_criteria)) {
        valid_specimens <- valid_specimens[sapply(valid_specimens$criteria_met, function(x) {
          if (is.na(x) || x == "") return(FALSE)
          criteria_list <- strsplit(x, "; ")[[1]]
          all(criteria$required_criteria %in% criteria_list)
        }), ]
      }

      valid_specimens
    },

    #' @description Select best specimen from valid specimens
    #' @param valid_specimens Data frame of valid specimens
    #' @return Single best specimen
    select_best_specimen = function(valid_specimens) {
      valid_specimens[order(
        -valid_specimens$quality_score,
        valid_specimens$specimen_rank,
        valid_specimens$processid
      )[1], ]
    }
  )
)
