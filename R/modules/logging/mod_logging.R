# R/modules/logging/mod_logging.R

#' Logging Manager R6 Class
#' @description Handles centralized logging of specimen selections, user actions, and exports
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite toJSON fromJSON
#' @export
LoggingManager <- R6Class(
  "LoggingManager",

  public = list(
    initialize = function(db_path = "data/specimen_tracking.db") {
      private$db_path <- db_path
      private$initialize_db()
    },

    info = function(message, details = NULL) {
      private$log_message("INFO", message, details)
    },

    warn = function(message, details = NULL) {
      private$log_message("WARNING", message, details)
    },

    error = function(message, details = NULL) {
      private$log_message("ERROR", message, details)
    },

    # log_action function
    log_action = function(user_email = NULL,
                          user_name = NULL,
                          session_id,
                          action_type,
                          process_ids = character(0),
                          metadata = NULL) {
      tryCatch({
        if (missing(session_id) || missing(action_type)) {
          stop("Missing required parameters: session_id and action_type are mandatory")
        }

        # Ensure process_ids is proper format
        process_ids <- as.character(process_ids)
        if (!is.character(process_ids)) process_ids <- character(0)

        process_ids_json <- if (length(process_ids) > 0) {
          toJSON(process_ids)
        } else {
          "[]"
        }

        metadata_json <- if (!is.null(metadata)) toJSON(metadata) else NULL

        dbExecute(
          private$conn,
          "INSERT INTO user_actions (timestamp, user_email, user_name, session_id,
                               action_type, process_ids, metadata)
       VALUES (datetime('now'), ?, ?, ?, ?, ?, ?)",
          params = list(user_email, user_name, session_id, action_type,
                        process_ids_json, metadata_json)
        )

        TRUE
      }, error = function(e) {
        warning(sprintf("Failed to log action: %s", e$message))
        FALSE
      })
    },

    get_specimen_history = function(process_id) {
      tryCatch({
        history <- dbGetQuery(
          private$conn,
          "SELECT * FROM specimens_tracking WHERE process_id = ?",
          params = list(process_id)
        )

        if (nrow(history) > 0) {
          history$action_history <- lapply(history$action_history, function(x) {
            if (!is.na(x)) fromJSON(x) else NULL
          })
        }

        history
      }, error = function(e) {
        warning(sprintf("Failed to get specimen history: %s", e$message))
        NULL
      })
    },

    get_user_actions = function(user_identifier = NULL,
                                from_date = NULL,
                                to_date = NULL,
                                limit = 100) {
      tryCatch({
        query <- "SELECT * FROM user_actions WHERE 1=1"
        params <- list()

        if (!is.null(user_identifier)) {
          query <- paste0(query, " AND (user_email = ? OR user_name = ?)")
          params <- c(params, user_identifier, user_identifier)
        }

        if (!is.null(from_date)) {
          query <- paste0(query, " AND timestamp >= ?")
          params <- c(params, from_date)
        }

        if (!is.null(to_date)) {
          query <- paste0(query, " AND timestamp <= ?")
          params <- c(params, to_date)
        }

        query <- paste0(query, " ORDER BY timestamp DESC LIMIT ?")
        params <- c(params, limit)

        actions <- dbGetQuery(private$conn, query, params = params)
        if (nrow(actions) > 0) {
          actions$process_ids <- lapply(actions$process_ids, function(x) {
            if (!is.na(x)) fromJSON(x) else character(0)
          })
          actions$metadata <- lapply(actions$metadata, function(x) {
            if (!is.na(x)) fromJSON(x) else NULL
          })
        }

        actions
      }, error = function(e) {
        warning(sprintf("Failed to get user actions: %s", e$message))
        NULL
      })
    },

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
        if (missing(session_id) || missing(export_type) ||
            missing(file_name) || missing(format)) {
          stop("Missing required parameters")
        }

        metadata_json <- if (!is.null(metadata)) toJSON(metadata) else NULL

        dbExecute(
          private$conn,
          "INSERT INTO export_history (
            timestamp, session_id, user_email, user_name,
            export_type, file_name, record_count, file_size,
            format, success, error_message, metadata
          ) VALUES (datetime('now'), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
          params = list(
            session_id, user_email, user_name,
            export_type, file_name, record_count, file_size,
            format, success, error_message, metadata_json
          )
        )
        TRUE
      }, error = function(e) {
        warning(sprintf("Failed to log export: %s", e$message))
        FALSE
      })
    },

    get_export_history = function(from_date = NULL,
                                  to_date = NULL,
                                  export_type = NULL,
                                  format = NULL,
                                  limit = 100) {
      tryCatch({
        query <- "SELECT * FROM export_history WHERE 1=1"
        params <- list()

        if (!is.null(from_date)) {
          query <- paste0(query, " AND timestamp >= ?")
          params <- c(params, from_date)
        }

        if (!is.null(to_date)) {
          query <- paste0(query, " AND timestamp <= ?")
          params <- c(params, to_date)
        }

        if (!is.null(export_type)) {
          query <- paste0(query, " AND export_type = ?")
          params <- c(params, export_type)
        }

        if (!is.null(format)) {
          query <- paste0(query, " AND format = ?")
          params <- c(params, format)
        }

        query <- paste0(query, " ORDER BY timestamp DESC LIMIT ?")
        params <- c(params, limit)

        history <- dbGetQuery(private$conn, query, params = params)
        if (nrow(history) > 0) {
          history$metadata <- lapply(history$metadata, function(x) {
            if (!is.na(x)) fromJSON(x) else NULL
          })
        }

        history
      }, error = function(e) {
        warning(sprintf("Failed to get export history: %s", e$message))
        NULL
      })
    },

    get_export_stats = function(from_date = NULL, to_date = NULL) {
      tryCatch({
        query <- "
          SELECT
            COUNT(*) as total_exports,
            SUM(CASE WHEN success = 1 THEN 1 ELSE 0 END) as successful_exports,
            SUM(CASE WHEN success = 0 THEN 1 ELSE 0 END) as failed_exports,
            SUM(record_count) as total_records_exported,
            SUM(file_size) as total_size_exported,
            AVG(CASE WHEN success = 1 THEN record_count ELSE NULL END) as avg_records_per_export
          FROM export_history
          WHERE 1=1"

        params <- list()

        if (!is.null(from_date)) {
          query <- paste0(query, " AND timestamp >= ?")
          params <- c(params, from_date)
        }

        if (!is.null(to_date)) {
          query <- paste0(query, " AND timestamp <= ?")
          params <- c(params, to_date)
        }

        dbGetQuery(private$conn, query, params = params)
      }, error = function(e) {
        warning(sprintf("Failed to get export stats: %s", e$message))
        NULL
      })
    },

    finalize = function() {
      if (!is.null(private$conn)) {
        dbDisconnect(private$conn)
      }
    }
  ),

  private = list(
    conn = NULL,
    db_path = NULL,

    initialize_db = function() {
      if (!dir.exists(dirname(private$db_path))) {
        dir.create(dirname(private$db_path), recursive = TRUE)
      }

      private$conn <- dbConnect(SQLite(), private$db_path)

      # User actions table
      dbExecute(private$conn, "
        CREATE TABLE IF NOT EXISTS user_actions (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          timestamp TEXT NOT NULL,
          user_email TEXT,
          user_name TEXT,
          session_id TEXT NOT NULL,
          action_type TEXT NOT NULL,
          process_ids TEXT NOT NULL,
          metadata TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )
      ")

      # Specimen tracking table
      dbExecute(private$conn, "
        CREATE TABLE IF NOT EXISTS specimens_tracking (
          process_id TEXT PRIMARY KEY,
          last_action TEXT NOT NULL,
          last_user TEXT,
          last_updated DATETIME DEFAULT CURRENT_TIMESTAMP,
          action_history TEXT
        )
      ")

      # Export history table
      dbExecute(private$conn, "
        CREATE TABLE IF NOT EXISTS export_history (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          timestamp TEXT NOT NULL,
          session_id TEXT NOT NULL,
          user_email TEXT,
          user_name TEXT,
          export_type TEXT NOT NULL,
          file_name TEXT NOT NULL,
          record_count INTEGER,
          file_size INTEGER,
          format TEXT NOT NULL,
          success BOOLEAN NOT NULL,
          error_message TEXT,
          metadata TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )
      ")

      dbExecute(private$conn, "CREATE INDEX IF NOT EXISTS idx_user_actions_timestamp ON user_actions(timestamp)")
      dbExecute(private$conn, "CREATE INDEX IF NOT EXISTS idx_export_history_timestamp ON export_history(timestamp)")
      dbExecute(private$conn, "CREATE INDEX IF NOT EXISTS idx_specimens_last_updated ON specimens_tracking(last_updated)")
    },

    log_message = function(level, message, details = NULL) {
      if (missing(message)) return(invisible(NULL))

      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      if (!is.null(details)) {
        details_str <- if (is.list(details)) toJSON(details) else as.character(details)
      } else {
        details_str <- NULL
      }

      cat(sprintf("[%s] %s: %s%s\n",
                  timestamp,
                  level,
                  message,
                  if (!is.null(details_str)) paste0(" - ", details_str) else ""),
          file = stderr())
    }
  )
)
