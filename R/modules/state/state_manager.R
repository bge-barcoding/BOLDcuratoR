# R/modules/state/state_manager.R

#' @title StateManager R6 Class
#' @description Manages centralized application state with validation and error handling
#' @importFrom R6 R6Class
#' @importFrom shiny reactiveValues isolate
#' @importFrom jsonlite toJSON fromJSON
#' @export
StateManager <- R6::R6Class(
  "StateManager",

  public = list(
    #' @description Initialize state manager
    #' @param session Shiny session object
    #' @param logger Optional logger instance
    initialize = function(session, logger = NULL) {
      private$session <- session
      private$logger <- logger
      private$error_boundary <- ErrorBoundary$new()
      private$initialize_store()
      private$setup_observers()
    },

    #' @description Get reactive store
    #' @return Reactive values object containing application state
    get_store = function() {
      private$store
    },

    #' @description Update state with validation and data type conversion
    #' @param key State key to update
    #' @param value New value
    #' @param validation_fn Optional validation function
    #' @param cleanup_fn Optional cleanup function
    #' @return Logical indicating success
    update_state = function(key, value, validation_fn = NULL, cleanup_fn = NULL) {
      success <- private$error_boundary$catch({
        # Convert value to proper format before validation
        converted_value <- private$convert_value(value)

        # Pre-update validation
        if (!is.null(validation_fn)) {
          validation_result <- validation_fn(converted_value)
          if (!validation_result$valid) {
            private$log_error("Validation failed", validation_result$messages)
            return(FALSE)
          }
        }

        # Store previous value for change tracking and rollback
        old_value <- isolate(private$store[[key]])

        tryCatch({
          # Update store with converted value
          private$store[[key]] <- converted_value

          # Track state change
          private$track_state_change(key, old_value, converted_value)

          # Run cleanup if provided
          if (!is.null(cleanup_fn)) {
            cleanup_fn(old_value)
          }

          private$log_info(sprintf("State updated successfully: %s", key))
          TRUE
        }, error = function(e) {
          private$revert_state(key, old_value)
          private$log_error("State update failed", e$message)
          FALSE
        })
      })

      return(!is.null(success) && success)
    },

    #' @description Reset state to initial values
    #' @param keys Optional vector of specific keys to reset
    reset_state = function(keys = NULL) {
      private$error_boundary$catch({
        if (is.null(keys)) {
          private$initialize_store()
          private$log_info("Full state reset completed")
        } else {
          for (key in keys) {
            if (key %in% names(private$initial_state)) {
              private$store[[key]] <- private$initial_state[[key]]
              private$log_info(sprintf("State reset for key: %s", key))
            }
          }
        }
        TRUE
      })
    }
  ),

  private = list(
    session = NULL,
    logger = NULL,
    store = NULL,
    error_boundary = NULL,
    state_history = list(),
    initial_state = NULL,

    initialize_store = function() {
      private$initial_state <- list(
        # User state
        user_info = list(
          email = NULL,
          name = NULL,
          orcid = NULL,
          bold_api_key = NULL
        ),

        # Data state
        specimen_data = NULL,
        bin_analysis = NULL,
        bags_grades = NULL,
        selected_specimens = list(),

        # Processing state
        processing = list(
          active = FALSE,
          progress = 0,
          message = NULL,
          sub_progress = 0,
          stage = NULL
        ),

        # Error state
        error = list(
          has_error = FALSE,
          message = NULL,
          details = NULL,
          timestamp = NULL,
          source = NULL
        )
      )

      private$store <- do.call(reactiveValues, private$initial_state)
    },

    setup_observers = function() {
      # Monitor error state
      observe({
        error_state <- isolate(private$store$error)
        if (error_state$has_error) {
          private$handle_error_state(error_state)
        }
      })
    },

    #' @description Convert value to proper format for state storage
    #' @param value Value to convert
    #' @return Converted value
    convert_value = function(value) {
      if (is.null(value)) return(NULL)

      if (is.data.frame(value)) {
        return(private$convert_data_frame(value))
      } else if (is.list(value)) {
        return(private$convert_list(value))
      } else if (inherits(value, "table")) {
        return(private$convert_table(value))
      }

      # Return unchanged for other types
      value
    },

    #' @description Convert data frame ensuring proper format
    #' @param df Data frame to convert
    #' @return Converted data frame
    convert_data_frame = function(df) {
      # Ensure proper row names
      if (!is.null(rownames(df)) && !all(rownames(df) == as.character(seq_len(nrow(df))))) {
        df <- data.frame(df, stringsAsFactors = FALSE, row.names = NULL)
      }

      # Convert any table columns to data frames
      for (col in names(df)) {
        if (inherits(df[[col]], "table")) {
          df[[col]] <- private$convert_table(df[[col]])
        }
      }

      df
    },

    #' @description Convert list ensuring proper format for all elements
    #' @param lst List to convert
    #' @return Converted list
    convert_list = function(lst) {
      lapply(lst, function(x) {
        if (is.data.frame(x)) {
          private$convert_data_frame(x)
        } else if (is.list(x)) {
          private$convert_list(x)
        } else if (inherits(x, "table")) {
          private$convert_table(x)
        } else {
          x
        }
      })
    },

    #' @description Convert table object to data frame
    #' @param tbl Table object to convert
    #' @return Data frame
    convert_table = function(tbl) {
      if (!inherits(tbl, "table")) return(tbl)

      # Convert table to data frame
      df <- as.data.frame(tbl, stringsAsFactors = FALSE)

      # Add proper column names if missing
      if (is.null(colnames(df))) {
        colnames(df) <- paste0("V", seq_len(ncol(df)))
      }

      # Clean row names
      rownames(df) <- NULL

      df
    },

    track_state_change = function(key, old_value, new_value) {
      change_record <- list(
        timestamp = Sys.time(),
        key = key,
        old_value = private$prepare_for_history(old_value),
        new_value = private$prepare_for_history(new_value),
        session_id = private$session$token
      )
      private$state_history <- c(private$state_history, list(change_record))
    },

    #' @description Prepare value for history tracking
    #' @param value Value to prepare
    #' @return Prepared value
    prepare_for_history = function(value) {
      if (is.data.frame(value)) {
        list(
          type = "data.frame",
          rows = nrow(value),
          cols = ncol(value),
          names = names(value)
        )
      } else if (is.list(value)) {
        list(
          type = "list",
          length = length(value),
          names = names(value)
        )
      } else {
        value
      }
    },

    handle_error_state = function(error_state) {
      if (!is.null(private$logger)) {
        private$logger$error(error_state$message, list(
          details = error_state$details,
          source = error_state$source,
          timestamp = error_state$timestamp
        ))
      }
    },

    revert_state = function(key, old_value) {
      private$store[[key]] <- old_value
      private$log_warn(sprintf("State reverted for key: %s", key))
    },

    log_info = function(message, details = NULL) {
      if (!is.null(private$logger)) {
        private$logger$info(message, details)
      }
    },

    log_warn = function(message, details = NULL) {
      if (!is.null(private$logger)) {
        private$logger$warn(message, details)
      }
    },

    log_error = function(message, details = NULL) {
      if (!is.null(private$logger)) {
        private$logger$error(message, details)
      }
    }
  )
)
