# R/modules/logging/mod_logging.R

#' @title LoggingManager R6 Class
#' @description Handles logging and state tracking
#' @importFrom R6 R6Class
#' @export
LoggingManager <- R6::R6Class(
  "LoggingManager",

  public = list(
    #' @description Initialize manager
    initialize = function() {
      private$msgs <- list()
      private$initialize_store()
    },

    #' @description Log info message
    #' @param message Message text
    #' @param details Optional details
    info = function(message, details = NULL) {
      private$add_log_entry("INFO", message, details)
    },

    #' @description Log warning message
    #' @param message Warning message
    #' @param details Optional details
    warn = function(message, details = NULL) {
      private$add_log_entry("WARNING", message, details)
    },

    #' @description Log error message
    #' @param message Error message
    #' @param details Optional details
    error = function(message, details = NULL) {
      private$add_log_entry("ERROR", message, details)
    },

    #' @description Get all log messages
    #' @param level Optional level filter
    #' @return List of log messages
    get_logs = function(level = NULL) {
      if(is.null(level)) return(private$msgs)
      private$msgs[sapply(private$msgs, function(x) x$level == level)]
    },

    #' @description Clear all logs
    clear_logs = function() {
      private$msgs <- list()
    },

    #' @description Log specimen selection
    #' @param user_email User email
    #' @param user_name User name
    #' @param session_id Session identifier
    #' @param processid Specimen process ID
    #' @param selected Boolean indicating if specimen is selected
    #' @param metadata Optional metadata
    log_specimen_selection = function(user_email = NULL,
                                      user_name = NULL,
                                      session_id,
                                      processid,
                                      selected,
                                      metadata = NULL) {
      tryCatch({
        if (missing(session_id) || missing(processid)) {
          stop("Missing required parameters: session_id and processid")
        }

        details <- list(
          user_email = user_email,
          user_name = user_name,
          session_id = session_id,
          processid = processid,
          selected = selected,
          metadata = metadata,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )

        private$add_log_entry(
          "SELECTION",
          sprintf("Specimen %s %s",
                  processid,
                  if(selected) "selected" else "deselected"),
          details
        )

        TRUE
      }, error = function(e) {
        private$add_log_entry(
          "ERROR",
          "Failed to log specimen selection",
          list(error = e$message)
        )
        FALSE
      })
    },

    #' @description Log specimen flag
    #' @param user_email User email
    #' @param user_name User name
    #' @param session_id Session identifier
    #' @param processid Specimen process ID
    #' @param flag_type Flag type
    #' @param metadata Optional metadata
    log_specimen_flag = function(user_email = NULL,
                                 user_name = NULL,
                                 session_id,
                                 processid,
                                 flag_type,
                                 metadata = NULL) {
      tryCatch({
        if (missing(session_id) || missing(processid)) {
          stop("Missing required parameters: session_id and processid")
        }

        # Validate flag type
        valid_flags <- c("Misidentified", "ID uncertain")
        if (!is.null(flag_type) && nchar(flag_type) > 0 &&
            !flag_type %in% valid_flags) {
          stop("Invalid flag type")
        }

        details <- list(
          user_email = user_email,
          user_name = user_name,
          session_id = session_id,
          processid = processid,
          flag_type = flag_type,
          metadata = metadata,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )

        private$add_log_entry(
          "FLAG",
          sprintf("Specimen %s flagged as %s",
                  processid, flag_type),
          details
        )

        TRUE
      }, error = function(e) {
        private$add_log_entry(
          "ERROR",
          "Failed to log specimen flag",
          list(error = e$message)
        )
        FALSE
      })
    },

    #' @description Log export action
    #' @param session_id Session identifier
    #' @param user_email User email
    #' @param user_name User name
    #' @param export_type Type of export
    #' @param file_name Export file name
    #' @param record_count Number of records exported
    #' @param file_size File size in bytes
    #' @param format Export format
    #' @param success Boolean indicating success
    #' @param error_message Optional error message
    #' @param metadata Optional metadata
    log_export = function(session_id,
                          user_email = NULL,
                          user_name = NULL,
                          export_type,
                          file_name,
                          record_count,
                          file_size,
                          format,
                          success,
                          error_message = NULL,
                          metadata = NULL) {
      tryCatch({
        details <- list(
          session_id = session_id,
          user_email = user_email,
          user_name = user_name,
          export_type = export_type,
          file_name = file_name,
          record_count = record_count,
          file_size = file_size,
          format = format,
          success = success,
          error_message = error_message,
          metadata = metadata,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )

        private$add_log_entry(
          "EXPORT",
          sprintf("Export %s: %s",
                  if(success) "completed" else "failed",
                  file_name),
          details
        )

        TRUE
      }, error = function(e) {
        private$add_log_entry(
          "ERROR",
          "Failed to log export",
          list(error = e$message)
        )
        FALSE
      })
    },

    #' @description Log action
    #' @param session_id Session identifier
    #' @param action_type Type of action
    #' @param process_ids Vector of process IDs
    #' @param metadata Optional metadata
    log_action = function(session_id,
                          action_type,
                          process_ids = character(0),
                          metadata = NULL) {
      tryCatch({
        details <- list(
          session_id = session_id,
          action_type = action_type,
          process_ids = process_ids,
          metadata = metadata,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )

        private$add_log_entry(
          "ACTION",
          sprintf("Action: %s", action_type),
          details
        )

        TRUE
      }, error = function(e) {
        private$add_log_entry(
          "ERROR",
          "Failed to log action",
          list(error = e$message)
        )
        FALSE
      })
    }
  ),

  private = list(
    msgs = NULL,

    initialize_store = function() {
      private$msgs <- list()
    },

    add_log_entry = function(level, message, details = NULL) {
      entry <- list(
        timestamp = Sys.time(),
        level = level,
        message = message,
        details = details
      )
      private$msgs[[length(private$msgs) + 1]] <- entry

      # Print to console for debugging
      cat(sprintf("[%s] %s: %s\n",
                  format(entry$timestamp, "%Y-%m-%d %H:%M:%S"),
                  entry$level,
                  entry$message))

      if(!is.null(details)) {
        cat("  Details:",
            if(is.list(details)) jsonlite::toJSON(details, auto_unbox = TRUE)
            else details, "\n")
      }
    }
  )
)
