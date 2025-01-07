# R/utils/db_handler.R

library(RSQLite)
library(DBI)

#' Initialize database connection and create tables if needed
#' @param db_path Path to SQLite database file
#' @param logger Optional logger instance
#' @return Database connection object
init_database <- function(db_path, logger = NULL) {
  tryCatch({
    # Create database directory if it doesn't exist
    dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)

    # Connect to database
    con <- dbConnect(RSQLite::SQLite(), db_path)

    # Create specimen_flags table if it doesn't exist
    if (!dbExistsTable(con, "specimen_flags")) {
      dbExecute(con, "
        CREATE TABLE specimen_flags (
          processid TEXT NOT NULL,
          flag TEXT,
          user_email TEXT,
          user_name TEXT,
          user_orcid TEXT,
          timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (processid)
        )
      ")
    }

    # Create specimen_notes table if it doesn't exist
    if (!dbExistsTable(con, "specimen_notes")) {
      dbExecute(con, "
        CREATE TABLE specimen_notes (
          processid TEXT NOT NULL,
          note TEXT,
          user_email TEXT,
          user_name TEXT,
          user_orcid TEXT,
          timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (processid)
        )
      ")
    }

    # Create specimen_selections table if it doesn't exist
    if (!dbExistsTable(con, "specimen_selections")) {
      dbExecute(con, "
        CREATE TABLE specimen_selections (
          processid TEXT NOT NULL,
          selected BOOLEAN,
          user_email TEXT,
          user_name TEXT,
          user_orcid TEXT,
          timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (processid)
        )
      ")
    }

    if (!is.null(logger)) {
      logger$info("Database initialized successfully")
    }

    con

  }, error = function(e) {
    if (!is.null(logger)) {
      logger$error("Database initialization failed", list(error = e$message))
    }
    stop(sprintf("Database initialization failed: %s", e$message))
  })
}

#' Get specimen flags from database
#' @param con Database connection
#' @param processids Optional vector of process IDs to filter
#' @return List of flags with processid as names
get_specimen_flags <- function(con, processids = NULL) {
  query <- if (!is.null(processids)) {
    sprintf("SELECT * FROM specimen_flags WHERE processid IN ('%s')",
            paste(processids, collapse = "','"))
  } else {
    "SELECT * FROM specimen_flags"
  }

  result <- dbGetQuery(con, query)

  if (nrow(result) == 0) return(list())

  flags <- lapply(seq_len(nrow(result)), function(i) {
    list(
      flag = result$flag[i],
      user = list(
        email = result$user_email[i],
        name = result$user_name[i],
        orcid = result$user_orcid[i]
      ),
      timestamp = result$timestamp[i]
    )
  })

  setNames(flags, result$processid)
}

#' Get specimen notes from database
#' @param con Database connection
#' @param processids Optional vector of process IDs to filter
#' @return List of notes with processid as names
get_specimen_notes <- function(con, processids = NULL) {
  query <- if (!is.null(processids)) {
    sprintf("SELECT * FROM specimen_notes WHERE processid IN ('%s')",
            paste(processids, collapse = "','"))
  } else {
    "SELECT * FROM specimen_notes"
  }

  result <- dbGetQuery(con, query)

  if (nrow(result) == 0) return(list())

  notes <- lapply(seq_len(nrow(result)), function(i) {
    list(
      text = result$note[i],
      user = list(
        email = result$user_email[i],
        name = result$user_name[i],
        orcid = result$user_orcid[i]
      ),
      timestamp = result$timestamp[i]
    )
  })

  setNames(notes, result$processid)
}

#' Update specimen flag in database
#' @param con Database connection
#' @param processid Specimen process ID
#' @param flag Flag value
#' @param user_info User information list
update_specimen_flag <- function(con, processid, flag, user_info) {
  if (is.null(flag) || nchar(flag) == 0) {
    dbExecute(con, "DELETE FROM specimen_flags WHERE processid = ?",
              list(processid))
  } else {
    dbExecute(con,
              "INSERT OR REPLACE INTO specimen_flags
       (processid, flag, user_email, user_name, user_orcid, timestamp)
       VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)",
              list(processid, flag,
                   user_info$email %||% NA_character_,
                   user_info$name %||% NA_character_,
                   user_info$orcid %||% NA_character_))
  }
}

#' Update specimen note in database
#' @param con Database connection
#' @param processid Specimen process ID
#' @param note Note text
#' @param user_info User information list
update_specimen_note <- function(con, processid, note, user_info) {
  if (is.null(note) || nchar(note) == 0) {
    dbExecute(con, "DELETE FROM specimen_notes WHERE processid = ?",
              list(processid))
  } else {
    dbExecute(con,
              "INSERT OR REPLACE INTO specimen_notes
       (processid, note, user_email, user_name, user_orcid, timestamp)
       VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)",
              list(processid, note,
                   user_info$email %||% NA_character_,
                   user_info$name %||% NA_character_,
                   user_info$orcid %||% NA_character_))
  }
}
