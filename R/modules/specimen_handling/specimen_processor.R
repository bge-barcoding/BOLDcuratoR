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
        tryCatch({
          scored <- private$scorer$score_specimens(validated)
          if (is.null(scored)) {
            private$logger$error("Specimen scoring failed")
            return(NULL)
          }

          # Ensure score columns are properly formatted
          scored <- private$format_score_columns(scored)

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

        # Calculate and store quality metrics
        metrics <- private$calculate_quality_metrics(ranked)
        private$last_metrics <- metrics

        # Final data cleanup
        result <- private$cleanup_data(ranked)

        private$logger$info(sprintf("Successfully processed %d specimens", nrow(result)))
        result
      })
    },

    #' @description Get the latest processing metrics
    #' @return List of quality metrics
    get_metrics = function() {
      if (!is.null(private$last_metrics)) {
        private$convert_metrics(private$last_metrics)
      } else {
        NULL
      }
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

      # Remove row names
      rownames(data) <- NULL

      # Convert any table columns to proper format
      for (col in names(data)) {
        if (inherits(data[[col]], "table")) {
          data[[col]] <- private$convert_table_column(data[[col]])
        }
      }

      data
    },

    #' @description Convert table column to vector
    #' @param col Table column
    #' @return Vector
    convert_table_column = function(col) {
      if (inherits(col, "table")) {
        as.vector(col)
      } else {
        col
      }
    },

    #' @description Format score columns ensuring proper types
    #' @param data Scored specimen data
    #' @return Data frame with properly formatted score columns
    format_score_columns = function(data) {
      if ("quality_score" %in% names(data)) {
        data$quality_score <- as.numeric(data$quality_score)
      }
      if ("specimen_rank" %in% names(data)) {
        data$specimen_rank <- as.integer(data$specimen_rank)
      }
      if ("criteria_met" %in% names(data)) {
        data$criteria_met <- as.character(data$criteria_met)
      }
      data
    },

    #' @description Convert metrics to proper format
    #' @param metrics Metrics list
    #' @return Properly formatted metrics
    convert_metrics = function(metrics) {
      if (!is.list(metrics)) return(NULL)

      # Convert any table objects in metrics
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

      # Ensure rank is numeric
      scored_specimens$specimen_rank <- as.integer(scored_specimens$specimen_rank)

      scored_specimens
    },

    #' @description Calculate quality metrics for specimens
    #' @param specimens Data frame of specimens
    #' @return List of quality metrics
    calculate_quality_metrics = function(specimens) {
      if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

      # Calculate basic metrics
      metrics <- list(
        total_specimens = nrow(specimens),
        avg_quality_score = mean(specimens$quality_score, na.rm = TRUE),
        median_quality_score = median(specimens$quality_score, na.rm = TRUE),
        rank_distribution = as.data.frame(table(specimens$specimen_rank),
                                          stringsAsFactors = FALSE),
        species_count = length(unique(specimens$species)),
        criteria_coverage = private$calculate_criteria_coverage(specimens)
      )

      # Convert any table objects in metrics
      metrics <- private$convert_metrics(metrics)

      metrics
    },

    #' @description Calculate criteria coverage metrics
    #' @param specimens Data frame of specimens
    #' @return List of criteria coverage statistics
    calculate_criteria_coverage = function(specimens) {
      if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

      # Calculate criteria counts
      criteria_counts <- lapply(names(SPECIMEN_SCORING_CRITERIA), function(criterion) {
        sum(grepl(criterion, specimens$criteria_met, fixed = TRUE))
      })
      names(criteria_counts) <- names(SPECIMEN_SCORING_CRITERIA)

      # Convert to data frame
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
        total_criteria = length(SPECIMEN_SCORING_CRITERIA),
        criteria_counts = criteria_df,
        avg_criteria_per_specimen = avg_criteria
      )
    },

    #' @description Final data cleanup before returning
    #' @param data Data frame to clean
    #' @return Cleaned data frame
    cleanup_data = function(data) {
      if (is.null(data) || nrow(data) == 0) return(data)

      # Ensure all character columns are properly encoded
      char_cols <- sapply(data, is.character)
      if (any(char_cols)) {
        for (col in names(data)[char_cols]) {
          data[[col]] <- enc2utf8(data[[col]])
        }
      }

      # Remove any empty numeric columns
      num_cols <- sapply(data, is.numeric)
      if (any(num_cols)) {
        empty_cols <- sapply(data[num_cols], function(x) all(is.na(x)))
        if (any(empty_cols)) {
          data <- data[, !empty_cols, drop = FALSE]
        }
      }

      # Remove row names
      rownames(data) <- NULL

      # Sort by quality score and rank
      if (all(c("quality_score", "specimen_rank") %in% names(data))) {
        data <- data[order(-data$quality_score, data$specimen_rank), ]
      }

      data
    }
  )
)
