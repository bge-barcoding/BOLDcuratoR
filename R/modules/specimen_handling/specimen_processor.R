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

        # Ensure proper data frame format
        specimens <- private$ensure_data_frame(specimens)

        # Run validation pipeline
        validated <- private$validator$validate_specimens(specimens)
        if (is.null(validated)) {
          private$logger$error("Specimen validation failed")
          return(NULL)
        }

        # Score specimens
        private$logger$info("Scoring specimens...")
        scored <- tryCatch({
          private$scorer$score_specimens(validated)
        }, error = function(e) {
          private$logger$error(sprintf("Scoring error: %s", e$message))
          NULL
        })

        if (is.null(scored)) {
          private$logger$error("Specimen scoring failed")
          return(NULL)
        }

        # Ensure score columns exist and are properly formatted
        scored <- private$format_score_columns(scored)

        # Rank specimens
        private$logger$info("Ranking specimens...")
        ranked <- private$rank_specimens(scored)
        if (is.null(ranked)) {
          private$logger$error("Specimen ranking failed")
          return(NULL)
        }

        # Calculate and store quality metrics
        metrics <- private$calculate_quality_metrics(ranked)
        private$last_metrics <- metrics

        # Final data cleanup
        result <- private$cleanup_data(ranked)

        if (!is.null(result) && nrow(result) > 0) {
          private$logger$info(sprintf("Successfully processed %d specimens", nrow(result)))
        } else {
          private$logger$warn("No specimens processed successfully")
        }

        result
      })
    },

    #' @description Get metrics from last processing run
    #' @return List of quality metrics or NULL if no processing has occurred
    get_metrics = function() {
      private$last_metrics
    }
  ),

  private = list(
    validator = NULL,
    scorer = NULL,
    logger = NULL,
    error_boundary = NULL,
    last_metrics = NULL,

    #' @description Ensure proper data frame format
    #' @param data Input data
    #' @return Properly formatted data frame
    ensure_data_frame = function(data) {
      if (!is.data.frame(data)) {
        data <- as.data.frame(data, stringsAsFactors = FALSE)
      }
      rownames(data) <- NULL
      data
    },

    #' @description Format score columns ensuring proper types
    #' @param data Scored specimen data
    #' @return Data frame with properly formatted score columns
    format_score_columns = function(data) {
      if (!("quality_score" %in% names(data))) {
        data$quality_score <- rep(NA_real_, nrow(data))
      }
      if (!("rank" %in% names(data))) {
        data$rank <- rep(NA_integer_, nrow(data))
      }
      if (!("criteria_met" %in% names(data))) {
        data$criteria_met <- rep(NA_character_, nrow(data))
      }

      data$quality_score <- as.numeric(data$quality_score)
      data$rank <- as.integer(data$rank)
      data$criteria_met <- as.character(data$criteria_met)

      data
    },

    #' @description Convert metrics to proper format
    #' @param metrics Metrics list
    #' @return Properly formatted metrics
    convert_metrics = function(metrics) {
      if (!is.list(metrics)) return(NULL)

      metrics <- lapply(metrics, function(x) {
        if (inherits(x, "table")) {
          as.data.frame(x, stringsAsFactors = FALSE)
        } else if (is.list(x)) {
          private$convert_metrics(x)
        } else {
          x
        }
      })

      metrics
    },

    #' @description Rank specimens based on quality score and criteria
    #' @param scored_specimens Data frame of scored specimens
    #' @return Ranked specimen data frame
    rank_specimens = function(scored_specimens) {
      if (is.null(scored_specimens) || nrow(scored_specimens) == 0) {
        return(NULL)
      }

      tryCatch({
        # Force reload of ranking criteria from global environment
        rank_criteria <- get0("SPECIMEN_RANK_CRITERIA", envir = .GlobalEnv)
        if (is.null(rank_criteria)) {
          private$logger$error("Failed to load SPECIMEN_RANK_CRITERIA from global environment")
          # Return specimens with default rank 7
          scored_specimens$rank <- 7L
          return(scored_specimens)
        }

        # Apply ranking based on criteria met
        scored_specimens$rank <- sapply(scored_specimens$criteria_met, function(criteria) {
          if (is.na(criteria) || criteria == "") return(7L)
          criteria_list <- strsplit(criteria, "; ")[[1]]

          for (rank in 1:6) {
            rank_key <- paste0("RANK_", rank)
            if (!is.null(rank_criteria[[rank_key]]) &&
                all(rank_criteria[[rank_key]] %in% criteria_list)) {
              return(as.integer(rank))
            }
          }
          return(7L)
        })

        # Ensure rank is integer
        scored_specimens$rank <- as.integer(scored_specimens$rank)
        scored_specimens

      }, error = function(e) {
        private$logger$error(sprintf("Error in rank_specimens: %s", e$message))
        # Return specimens with default rank on error
        scored_specimens$rank <- 7L
        scored_specimens
      })
    },

    #' @description Calculate quality metrics for specimens
    #' @param specimens Data frame of specimens
    #' @return List of quality metrics
    calculate_quality_metrics = function(specimens) {
      if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

      tryCatch({
        # Calculate basic metrics
        metrics <- list(
          total_specimens = nrow(specimens),
          avg_quality_score = mean(specimens$quality_score, na.rm = TRUE),
          median_quality_score = median(specimens$quality_score, na.rm = TRUE),
          rank_distribution = as.data.frame(table(specimens$rank),
                                            stringsAsFactors = FALSE),
          species_count = length(unique(specimens$species)),
          criteria_coverage = private$calculate_criteria_coverage(specimens)
        )

        # Convert table objects
        metrics <- private$convert_metrics(metrics)

        metrics
      }, error = function(e) {
        private$logger$error(sprintf("Error calculating metrics: %s", e$message))
        NULL
      })
    },

    #' @description Calculate criteria coverage metrics
    #' @param specimens Data frame of specimens
    #' @return List of criteria coverage statistics
    calculate_criteria_coverage = function(specimens) {
      if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

      # Get scoring criteria from global environment
      scoring_criteria <- get("SPECIMEN_SCORING_CRITERIA", envir = .GlobalEnv)
      if (is.null(scoring_criteria)) {
        private$logger$error("Failed to load SPECIMEN_SCORING_CRITERIA")
        return(NULL)
      }

      # Calculate criteria counts
      criteria_counts <- lapply(names(scoring_criteria), function(criterion) {
        sum(grepl(criterion, specimens$criteria_met, fixed = TRUE))
      })
      names(criteria_counts) <- names(scoring_criteria)

      # Create data frame
      criteria_df <- data.frame(
        criterion = names(criteria_counts),
        count = unlist(criteria_counts),
        stringsAsFactors = FALSE
      )

      # Calculate average criteria per specimen
      avg_criteria <- mean(sapply(specimens$criteria_met, function(x) {
        if (is.na(x) || x == "") return(0)
        length(strsplit(x, "; ")[[1]])
      }))

      list(
        total_criteria = length(scoring_criteria),
        criteria_counts = criteria_df,
        avg_criteria_per_specimen = avg_criteria
      )
    },

    #' @description Clean up processed data
    #' @param data Processed specimen data
    #' @return Cleaned data frame
    cleanup_data = function(data) {
      if (is.null(data) || nrow(data) == 0) return(data)

      # Ensure required columns exist
      required_cols <- c("quality_score", "rank", "criteria_met")
      for (col in required_cols) {
        if (!(col %in% names(data))) {
          data[[col]] <- NA
        }
      }

      # Convert numeric columns
      data$quality_score <- as.numeric(data$quality_score)
      data$rank <- as.integer(data$rank)

      # Fill NAs with appropriate values
      data$quality_score[is.na(data$quality_score)] <- 0
      data$rank[is.na(data$rank)] <- 7L
      data$criteria_met[is.na(data$criteria_met)] <- ""

      data
    }
  )
)
