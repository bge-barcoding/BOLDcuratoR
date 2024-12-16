#' Enhanced State Manager for BOLDcuratoR
#' @description Centralized state management with validation, error handling, and state consistency
#' @importFrom R6 R6Class
#' @importFrom shiny reactiveValues isolate
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

    #' @description Update state with validation
    #' @param key State key to update
    #' @param value New value
    #' @param validation_fn Optional validation function
    #' @param cleanup_fn Optional cleanup function
    #' @return Logical indicating success
    update_state = function(key, value, validation_fn = NULL, cleanup_fn = NULL) {
      success <- private$error_boundary$catch({
        # Pre-update validation
        if (!is.null(validation_fn)) {
          validation_result <- validation_fn(value)
          if (!validation_result$valid) {
            private$log_error("Validation failed", validation_result$messages)
            return(FALSE)
          }
        }

        # Store previous value for change tracking and rollback
        old_value <- isolate(private$store[[key]])

        tryCatch({
          # Validate state transition if applicable
          if (key == "api_key_status") {
            transition_validation <- private$validate_state_transition(old_value, value)
            if (!transition_validation$valid) {
              private$log_error("Invalid state transition", transition_validation$messages)
              return(FALSE)
            }
          }

          # Update store
          private$store[[key]] <- value

          # Track state change
          private$track_state_change(key, old_value, value)

          # Validate state consistency
          state_validation <- private$validate_state_consistency()
          if (!state_validation$valid) {
            private$revert_state(key, old_value)
            private$log_error("State consistency validation failed", state_validation$messages)
            return(FALSE)
          }

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

    #' @description Validate API key
    #' @param key API key to validate
    #' @return Logical indicating success
    validate_api_key = function(key) {
      private$error_boundary$catch({
        current_state <- isolate(private$store$api_key_status)

        new_state <- list(
          is_set = FALSE,
          key = key,
          last_validated = Sys.time(),
          validation_attempts = current_state$validation_attempts + 1,
          last_attempt = Sys.time()
        )

        if (private$validate_key_format(key)) {
          new_state$is_set <- TRUE
        }

        self$update_state("api_key_status", new_state, private$validate_api_key_state)
      })
    },

    #' @description Validate specific state components
    #' @param required_keys Vector of state keys that must be valid
    #' @return List with validation results
    validate_state = function(required_keys) {
      results <- private$error_boundary$catch({
        validation_results <- lapply(required_keys, function(key) {
          validator <- private$get_validator(key)
          if (is.null(validator)) {
            return(list(valid = TRUE, messages = character()))
          }
          validator(isolate(private$store[[key]]))
        })

        list(
          valid = all(sapply(validation_results, function(x) x$valid)),
          messages = unlist(lapply(validation_results, function(x) x$messages))
        )
      })

      if (is.null(results)) {
        return(list(valid = FALSE, messages = "Validation failed"))
      }
      results
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
    },

    #' @description Get state change history
    #' @param key Optional state key to filter history
    #' @param from_time Optional start time filter
    #' @param to_time Optional end time filter
    #' @return Filtered history list
    get_history = function(key = NULL, from_time = NULL, to_time = NULL) {
      history <- private$state_history

      if (!is.null(key)) {
        history <- history[sapply(history, function(x) x$key == key)]
      }

      if (!is.null(from_time)) {
        history <- history[sapply(history, function(x) x$timestamp >= from_time)]
      }

      if (!is.null(to_time)) {
        history <- history[sapply(history, function(x) x$timestamp <= to_time)]
      }

      history
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
        # Core application state
        api_key_status = list(
          is_set = FALSE,
          key = NULL,
          last_validated = NULL,
          validation_attempts = 0,
          last_attempt = NULL
        ),

        # User state
        user_info = list(
          email = NULL,
          name = NULL,
          orcid = NULL
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
          message = NULL
        ),

        # Error state
        error = list(
          has_error = FALSE,
          message = NULL,
          details = NULL,
          timestamp = NULL,
          source = NULL
        ),

        # UI state
        ui_state = list(
          active_tab = "input",
          filters = list(),
          selected_items = list()
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
        processing <- isolate(private$store$processing)
        if (processing$active) {
          private$update_progress(processing)
        }
      })

      # Monitor API key status
      observe({
        api_status <- isolate(private$store$api_key_status)
        if (!is.null(api_status$last_attempt)) {
          private$handle_api_status_change(api_status)
        }
      })
    },

    track_state_change = function(key, old_value, new_value) {
      change_record <- list(
        timestamp = Sys.time(),
        key = key,
        old_value = old_value,
        new_value = new_value,
        session_id = private$session$token
      )
      private$state_history <- c(private$state_history, list(change_record))
    },

    validate_state_consistency = function() {
      messages <- character()

      # Validate specimen data consistency
      specimen_data <- isolate(private$store$specimen_data)
      if (!is.null(specimen_data)) {
        # Check BAGS grades consistency
        bags_grades <- isolate(private$store$bags_grades)
        if (!is.null(bags_grades)) {
          specimen_species <- unique(specimen_data$species)
          bags_species <- unique(bags_grades$species)
          if (!all(bags_species %in% specimen_species)) {
            messages <- c(messages, "BAGS grades contain species not present in specimen data")
          }
        }

        # Validate selected specimens
        selected_specimens <- isolate(private$store$selected_specimens)
        if (!is.null(selected_specimens) && length(selected_specimens) > 0) {
          selected_ids <- unlist(selected_specimens)
          if (!all(selected_ids %in% specimen_data$processid)) {
            messages <- c(messages, "Selected specimens contain invalid process IDs")
          }
        }
      }

      list(
        valid = length(messages) == 0,
        messages = messages
      )
    },

    validate_key_format = function(key) {
      !is.null(key) &&
        nchar(trimws(key)) == 36 &&
        grepl("^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$",
              key, ignore.case = TRUE)
    },

    validate_api_key_state = function(state) {
      if (!is.list(state)) {
        return(list(
          valid = FALSE,
          messages = "API key state must be a list"
        ))
      }

      required_fields <- c("is_set", "key", "last_validated",
                           "validation_attempts", "last_attempt")
      missing_fields <- setdiff(required_fields, names(state))

      if (length(missing_fields) > 0) {
        return(list(
          valid = FALSE,
          messages = sprintf("Missing required fields: %s",
                             paste(missing_fields, collapse = ", "))
        ))
      }

      if (state$is_set && !is.null(state$key)) {
        valid_format <- private$validate_key_format(state$key)
        if (!valid_format) {
          return(list(
            valid = FALSE,
            messages = "Invalid API key format"
          ))
        }
      }

      list(valid = TRUE, messages = character())
    },

    validate_state_transition = function(from_state, to_state) {
      if (from_state$is_set && !to_state$is_set && !is.null(to_state$key)) {
        return(list(
          valid = FALSE,
          messages = "Cannot set new key while clearing API key status"
        ))
      }

      list(valid = TRUE, messages = character())
    },

    get_validator = function(key) {
      switch(key,
             "api_key_status" = private$validate_api_key_state,
             "user_info" = validate_user_info,
             "specimen_data" = validate_specimen_data,
             "bin_analysis" = validate_bin_analysis,
             "bags_grades" = validate_bags_grades,
             "selected_specimens" = validate_selected_specimens,
             "processing" = validate_processing_state,
             NULL
      )
    },

    revert_state = function(key, old_value) {
      private$store[[key]] <- old_value
      private$log_warn(sprintf("State reverted for key: %s", key))
    },

    handle_error_state = function(error_state) {
      if (!is.null(private$logger)) {
        private$logger$error(error_state$message, list(
          details = error_state$details,
          source = error_state$source,
          timestamp = error_state$timestamp
        ))
      }

      if (!is.null(private$session)) {
        showNotification(
          error_state$message,
          type = "error",
          duration = NULL
        )
      }
    },

    handle_api_status_change = function(api_status) {
      if (api_status$is_set) {
        private$log_info("API key validated successfully")
      } else if (api_status$validation_attempts > 0) {
        private$log_warn(sprintf("API key validation failed. Attempts: %d",
                                 api_status$validation_attempts))
      }
    },

    update_progress = function(processing) {
      if (!is.null(private$session)) {
        withProgress(
          message = processing$message,
          value = processing$progress / 100,
          {
            # Progress updates handled by individual modules
          }
        )
      }
    },

    log_info = function(message) {
      if (!is.null(private$logger)) {
        private$logger$info(message)
      }
    },

    log_warn = function(message) {
      if (!is.null(private$logger)) {
        private$logger$warn(message)
      }
    },

    log_error = function(message, details = NULL) {
      if (!is.null(private$logger)) {
        private$logger$error(message, details)
      }
    }
  )
)

#' Error Boundary R6 Class
#' @description Error handling and recovery system
#' @export
ErrorBoundary <- R6::R6Class(
  "ErrorBoundary",

  public = list(
    #' @description Execute code with error handling
    #' @param expr Expression to evaluate
    #' @return Result of expression or NULL on error
    catch = function(expr) {
      tryCatch(
        expr,
        error = function(e) {
          private$handle_error(e)
          NULL
        },
        warning = function(w) {
          private$handle_warning(w)
          NULL
        }
      )
    }
  ),

  private = list(
    handle_error = function(error) {
      warning(sprintf("Error caught: %s", error$message))
    },

    handle_warning = function(warning) {
      warning(sprintf("Warning: %s", warning$message))
    }
  )
)
