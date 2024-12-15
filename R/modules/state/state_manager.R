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
    #' @return Logical indicating success
    update_state = function(key, value, validation_fn = NULL) {
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

          # Validate state consistency
          state_validation <- private$validate_state_consistency()
          if (!state_validation$valid) {
            private$revert_state(key, old_value)
            private$log_error("State consistency validation failed", state_validation$messages)
            return(FALSE)
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

        # Combine validation results
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

    #' Initialize reactive store with default values
    initialize_store = function() {
      private$initial_state <- list(
        # Core application state
        api_key_status = list(
          is_set = FALSE,
          key = NULL,
          last_validated = NULL
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

    #' Setup reactive observers for state monitoring
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

    #' Track state changes for history
    #' @param key State key that changed
    #' @param old_value Previous value
    #' @param new_value New value
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

    #' Validate state consistency across components
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

    #' Get appropriate validator function for a state key
    #' @param key State key
    #' @return Validator function or NULL if no validator exists
    get_validator = function(key) {
      switch(key,
             "api_key_status" = validate_api_key,
             "user_info" = validate_user_info,
             "specimen_data" = validate_specimen_data,
             "bin_analysis" = validate_bin_analysis,
             "bags_grades" = validate_bags_grades,
             "selected_specimens" = validate_selected_specimens,
             "processing" = validate_processing_state,
             NULL
      )
    },

    #' Revert state to previous value
    #' @param key State key to revert
    #' @param old_value Previous value to restore
    revert_state = function(key, old_value) {
      private$store[[key]] <- old_value
      private$log_warn(sprintf("State reverted for key: %s", key))
    },

    #' Handle error state changes
    #' @param error_state Current error state
    handle_error_state = function(error_state) {
      if (!is.null(private$logger)) {
        private$logger$error(error_state$message, list(
          details = error_state$details,
          source = error_state$source,
          timestamp = error_state$timestamp
        ))
      }

      # Update UI with error message
      if (!is.null(private$session)) {
        showNotification(
          error_state$message,
          type = "error",
          duration = NULL
        )
      }
    },

    #' Update progress indicators
    #' @param processing Current processing state
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

    #' Logging helpers
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

# Validation Functions

#' Validate API key format and expiration
#' @param value API key status list
#' @return Validation result list
validate_api_key <- function(value) {
  if (!is.list(value) || !all(c("is_set", "key") %in% names(value))) {
    return(list(valid = FALSE, messages = "Invalid API key status format"))
  }

  if (value$is_set && (is.null(value$key) || nchar(trimws(value$key)) != 36)) {
    return(list(valid = FALSE, messages = "Invalid API key format"))
  }

  list(valid = TRUE, messages = character())
}

#' Validate user information
#' @param value User info list
#' @return Validation result list
validate_user_info <- function(value) {
  if (!is.list(value)) {
    return(list(valid = FALSE, messages = "User info must be a list"))
  }

  # Require either email or name
  if (is.null(value$email) && is.null(value$name)) {
    return(list(valid = FALSE, messages = "Either email or name must be provided"))
  }

  # Validate email format if provided
  if (!is.null(value$email) && !grepl("^[^@]+@[^@]+\\.[^@]+$", value$email)) {
    return(list(valid = FALSE, messages = "Invalid email format"))
  }

  list(valid = TRUE, messages = character())
}

#' Validate specimen data format and content
#' @param value Specimen data frame
#' @return Validation result list
validate_specimen_data <- function(value) {
  messages <- character()

  if (is.null(value)) {
    return(list(valid = FALSE, messages = "Specimen data is NULL"))
  }

  if (!is.data.frame(value)) {
    return(list(valid = FALSE, messages = "Specimen data must be a data frame"))
  }

  if (nrow(value) == 0) {
    return(list(valid = FALSE, messages = "Specimen data is empty"))
  }

  # Required columns
  required_cols <- c("processid", "species", "bin_uri")
  missing_cols <- setdiff(required_cols, names(value))
  if (length(missing_cols) > 0) {
    messages <- c(messages, sprintf("Missing required columns: %s",
                                    paste(missing_cols, collapse = ", ")))
  }

  # Check for duplicate process IDs
  if ("processid" %in% names(value) && any(duplicated(value$processid))) {
    messages <- c(messages, "Duplicate process IDs found")
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}

#' Validate BIN analysis results
#' @param value BIN analysis list
#' @return Validation result list
validate_bin_analysis <- function(value) {
  if (is.null(value)) {
    return(list(valid = FALSE, messages = "BIN analysis is NULL"))
  }

  required_components <- c("bin_summary", "bin_content", "stats")
  missing_components <- setdiff(required_components, names(value))

  if (length(missing_components) > 0) {
    return(list(
      valid = FALSE,
      messages = sprintf("Missing BIN analysis components: %s",
                         paste(missing_components, collapse = ", "))
    ))
  }

  list(valid = TRUE, messages = character())
}

#' Validate BAGS grades data
#' @param value BAGS grades data frame
#' @return Validation result list
validate_bags_grades <- function(value) {
  messages <- character()

  if (is.null(value)) {
    return(list(valid = FALSE, messages = "BAGS grades data is NULL"))
  }

  if (!is.data.frame(value)) {
    return(list(valid = FALSE, messages = "BAGS grades must be a data frame"))
  }

  # Check required columns
  required_cols <- c("species", "bags_grade", "specimen_count", "bin_count")
  missing_cols <- setdiff(required_cols, names(value))
  if (length(missing_cols) > 0) {
    messages <- c(messages, sprintf("Missing required columns: %s",
                                    paste(missing_cols, collapse = ", ")))
  }

  # Validate grade values
  if ("bags_grade" %in% names(value)) {
    invalid_grades <- setdiff(unique(value$bags_grade), c("A", "B", "C", "D", "E"))
    if (length(invalid_grades) > 0) {
      messages <- c(messages, sprintf("Invalid BAGS grades found: %s",
                                      paste(invalid_grades, collapse = ", ")))
    }
  }

  # Validate numeric columns
  numeric_cols <- c("specimen_count", "bin_count")
  for (col in intersect(numeric_cols, names(value))) {
    if (!is.numeric(value[[col]]) || any(value[[col]] < 0, na.rm = TRUE)) {
      messages <- c(messages, sprintf("Invalid values in column %s (must be non-negative numeric)", col))
    }
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}

#' Validate selected specimens
#' @param value Selected specimens list
#' @return Validation result list
validate_selected_specimens <- function(value) {
  messages <- character()

  if (!is.list(value)) {
    return(list(valid = FALSE, messages = "Selected specimens must be a list"))
  }

  # Validate species names as keys
  invalid_species <- sapply(names(value), function(species) {
    is.null(species) || nchar(trimws(species)) == 0 ||
      grepl("sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.", species)
  })

  if (any(invalid_species)) {
    messages <- c(messages, "Invalid species names in selection")
  }

  # Validate process IDs as values
  invalid_ids <- sapply(value, function(id) {
    is.null(id) || !is.character(id) || length(id) != 1 ||
      nchar(trimws(id)) == 0
  })

  if (any(invalid_ids)) {
    messages <- c(messages, "Invalid process IDs in selection")
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}

#' Validate processing state
#' @param value Processing state list
#' @return Validation result list
validate_processing_state <- function(value) {
  messages <- character()

  if (!is.list(value)) {
    return(list(valid = FALSE, messages = "Processing state must be a list"))
  }

  # Check required fields
  required_fields <- c("active", "progress", "message")
  missing_fields <- setdiff(required_fields, names(value))
  if (length(missing_fields) > 0) {
    messages <- c(messages, sprintf("Missing processing state fields: %s",
                                    paste(missing_fields, collapse = ", ")))
  }

  # Validate progress value
  if (!is.null(value$progress)) {
    if (!is.numeric(value$progress) || value$progress < 0 || value$progress > 100) {
      messages <- c(messages, "Progress must be numeric between 0 and 100")
    }
  }

  # Validate active flag
  if (!is.null(value$active) && !is.logical(value$active)) {
    messages <- c(messages, "Active flag must be logical")
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}
