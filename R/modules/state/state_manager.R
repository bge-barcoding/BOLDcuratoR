# R/modules/state/state_manager.R

#' @title StateManager R6 Class
#' @description Manages centralized application state with validation and error handling
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
          # Update store
          private$store[[key]] <- value

          # Track state change
          private$track_state_change(key, old_value, value)

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
    },

    #' @description Log action with state context
    #' @param action_type Type of action
    #' @param details Action details
    #' @param error Optional error details
    log_action = function(action_type, details = NULL, error = NULL) {
      if (!is.null(private$logger)) {
        context <- list(
          session_id = private$session$token,
          timestamp = Sys.time(),
          action = action_type,
          details = details,
          error = error
        )

        if (!is.null(error)) {
          private$logger$error(action_type, context)
        } else {
          private$logger$info(action_type, context)
        }
      }
    },

    #' @description Handle error state
    #' @param error Error object
    #' @param source Error source
    handle_error = function(error, source = NULL) {
      tryCatch({
        error_details <- list(
          has_error = TRUE,
          message = if (is.character(error)) error else error$message,
          details = if (is.list(error)) error else NULL,
          timestamp = Sys.time(),
          source = source
        )

        self$update_state("error", error_details)
        self$log_action("error_occurred", error_details)

      }, error = function(e) {
        private$log_error("Error handling failed", e$message)
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

      # Monitor processing state
      observe({
        processing <- isolate(private$store$processing)
        if (processing$active) {
          private$update_progress(processing)
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

    update_progress = function(processing) {
      if (!is.null(private$session)) {
        withProgress(
          message = processing$message,
          value = processing$progress / 100,
          {
            if (!is.null(processing$stage)) {
              incProgress(
                amount = processing$sub_progress / 100,
                detail = processing$stage
              )
            }
          }
        )
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

#' @title ErrorBoundary R6 Class
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
