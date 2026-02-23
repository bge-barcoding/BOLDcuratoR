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

    #' @description Validate state requirements
    #' @param required_keys Vector of required state keys
    #' @return List with validation results
    validate_state = function(required_keys) {
      private$error_boundary$catch({
        if (is.null(required_keys) || length(required_keys) == 0) {
          return(list(valid = FALSE, messages = "No required keys specified"))
        }

        store <- isolate(private$store)
        missing_keys <- required_keys[!required_keys %in% names(store)]

        if (length(missing_keys) > 0) {
          return(list(
            valid = FALSE,
            messages = sprintf("Missing required state: %s",
                               paste(missing_keys, collapse = ", "))
          ))
        }

        list(valid = TRUE, messages = NULL)
      })
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
          # Reset all keys in the existing store rather than creating a new
          # reactiveValues object, so that external references remain valid.
          for (key in names(private$initial_state)) {
            private$store[[key]] <- private$initial_state[[key]]
          }
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
    },

    #' @description Get state metrics
    #' @return List of state metrics
    get_metrics = function() {
      store <- isolate(private$store)
      list(
        total_keys = length(names(store)),
        updated_keys = length(private$state_history),
        last_update = if (length(private$state_history) > 0)
          private$state_history[[length(private$state_history)]]$timestamp else NULL
      )
    },

    #' @description Get state history
    #' @param key Optional key to filter history
    #' @return List of state changes
    get_history = function(key = NULL) {
      if (is.null(key)) {
        private$state_history
      } else {
        Filter(function(x) x$key == key, private$state_history)
      }
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
        search_taxa = NULL,
        selected_specimens = list(),
        specimen_flags = list(),
        specimen_curator_notes = list(),
        specimen_updated_ids = list(),
        specimen_metrics = NULL,
        specimen_history = list(),

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

      # Monitor processing state
      observe({
        proc_state <- isolate(private$store$processing)
        if (proc_state$active) {
          private$log_info("Processing state update", list(
            progress = proc_state$progress,
            message = proc_state$message,
            stage = proc_state$stage
          ))
        }
      })
    },

    # Validation functions
    validate_curator_notes = function(notes) {
      if (!is.list(notes)) {
        return(list(valid = FALSE, messages = "Curator notes must be a list"))
      }

      # Validate each note
      invalid_notes <- Filter(function(note) {
        !is.character(note$text) || nchar(note$text) > 1000  # Max length validation
      }, notes)

      if (length(invalid_notes) > 0) {
        return(list(
          valid = FALSE,
          messages = "Invalid curator note format or length"
        ))
      }

      list(valid = TRUE, messages = NULL)
    },

    validate_flags = function(flags) {
      if (!is.list(flags)) {
        return(list(valid = FALSE, messages = "Flags must be a list"))
      }

      valid_flags <- c("misidentification", "id_uncertain", "data_issue", "other_issue")

      invalid_flags <- Filter(function(flag) {
        flag_val <- if (is.list(flag)) flag$flag else flag
        !is.null(flag_val) && nchar(flag_val) > 0 && !(flag_val %in% valid_flags)
      }, flags)

      if (length(invalid_flags) > 0) {
        return(list(
          valid = FALSE,
          messages = "Invalid flag values detected"
        ))
      }

      list(valid = TRUE, messages = NULL)
    },

    validate_selected_specimens = function(selections) {
      if (!is.list(selections)) {
        return(list(valid = FALSE, messages = "Selections must be a list"))
      }

      specimen_data <- isolate(private$store$specimen_data)
      if (is.null(specimen_data)) {
        return(list(valid = FALSE, messages = "No specimen data available"))
      }

      invalid_ids <- setdiff(names(selections), specimen_data$processid)
      if (length(invalid_ids) > 0) {
        return(list(
          valid = FALSE,
          messages = "Invalid specimen IDs in selection"
        ))
      }

      list(valid = TRUE, messages = NULL)
    },

    # Value conversion functions
    convert_value = function(value) {
      if (is.null(value)) return(NULL)

      if (is.data.frame(value)) {
        return(private$convert_data_frame(value))
      } else if (is.list(value)) {
        return(private$convert_list(value))
      } else if (inherits(value, "table")) {
        return(private$convert_table(value))
      }

      value
    },

    convert_data_frame = function(df) {
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      if (!is.null(rownames(df))) {
        rownames(df) <- NULL
      }
      df
    },

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

    convert_table = function(tbl) {
      if (!inherits(tbl, "table")) return(tbl)
      df <- as.data.frame(tbl, stringsAsFactors = FALSE)
      if (is.null(colnames(df))) {
        colnames(df) <- paste0("V", seq_len(ncol(df)))
      }
      rownames(df) <- NULL
      df
    },

    # State tracking functions
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

    # Error handling functions
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

    # Logging functions
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
