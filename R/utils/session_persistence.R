# R/utils/session_persistence.R
# SQLite-backed session persistence for BOLDcuratoR.
# Stores session metadata in a `sessions` table and serialised R objects
# (data frames, lists) as BLOBs in a `session_data` table.

# Null-coalescing helper (base R %||% requires R >= 4.4.0)
.null_default <- function(x, default) {
  if (is.null(x)) default else x
}

#' Initialise (or open) the session database
#'
#' Creates the SQLite file and tables if they don't already exist.
#' Uses WAL journal mode for safe concurrent reads across Shiny workers.
#'
#' @param db_path Path to the SQLite file (default: "data/sessions.sqlite")
#' @return A DBI connection object
#' @export
init_session_db <- function(db_path = "data/sessions.sqlite") {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
  DBI::dbExecute(con, "PRAGMA foreign_keys=ON")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      session_id     TEXT PRIMARY KEY,
      created_at     TEXT NOT NULL,
      updated_at     TEXT NOT NULL,
      user_email     TEXT DEFAULT '',
      user_name      TEXT DEFAULT '',
      user_orcid     TEXT DEFAULT '',
      specimen_count INTEGER DEFAULT 0,
      species_count  INTEGER DEFAULT 0,
      search_taxa    TEXT DEFAULT ''
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS session_data (
      session_id TEXT NOT NULL,
      key        TEXT NOT NULL,
      value      BLOB NOT NULL,
      updated_at TEXT NOT NULL,
      PRIMARY KEY (session_id, key),
      FOREIGN KEY (session_id) REFERENCES sessions(session_id) ON DELETE CASCADE
    )
  ")

  con
}

#' Save session state to database
#'
#' Persists specimen data, BAGS grades, BIN analysis, annotations, and
#' metadata into the SQLite session database.
#'
#' @param session_id Unique session identifier
#' @param store The state store (reactiveValues accessed via isolate, or a plain list)
#' @param con DBI connection returned by \code{init_session_db}
#' @return TRUE on success, FALSE on failure
#' @export
save_session_state <- function(session_id, store, con) {
  tryCatch({
    now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

    # ----- metadata row (UPSERT) -----
    existing <- DBI::dbGetQuery(
      con,
      "SELECT created_at FROM sessions WHERE session_id = ?",
      params = list(session_id)
    )
    created_at <- if (nrow(existing) > 0) existing$created_at[1] else now

    search_taxa_json <- if (!is.null(store$search_taxa)) {
      jsonlite::toJSON(store$search_taxa, auto_unbox = TRUE)
    } else {
      ""
    }

    DBI::dbExecute(con, "
      INSERT INTO sessions (session_id, created_at, updated_at,
                            user_email, user_name, user_orcid,
                            specimen_count, species_count, search_taxa)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(session_id) DO UPDATE SET
        updated_at     = excluded.updated_at,
        user_email     = excluded.user_email,
        user_name      = excluded.user_name,
        user_orcid     = excluded.user_orcid,
        specimen_count = excluded.specimen_count,
        species_count  = excluded.species_count,
        search_taxa    = excluded.search_taxa
    ", params = list(
      session_id,
      created_at,
      now,
      .null_default(if (!is.null(store$user_info)) store$user_info$email else NULL, ""),
      .null_default(if (!is.null(store$user_info)) store$user_info$name else NULL, ""),
      .null_default(if (!is.null(store$user_info)) store$user_info$orcid else NULL, ""),
      if (!is.null(store$specimen_data)) nrow(store$specimen_data) else 0L,
      if (!is.null(store$specimen_data)) length(unique(store$specimen_data$species)) else 0L,
      search_taxa_json
    ))

    # ----- data blobs -----
    data_keys <- c("specimen_data", "bags_grades", "bin_analysis",
                   "selected_specimens", "specimen_flags",
                   "specimen_curator_notes", "specimen_updated_ids")

    for (key in data_keys) {
      value <- store[[key]]
      if (is.null(value)) next

      blob <- list(serialize(value, NULL))
      DBI::dbExecute(con, "
        INSERT INTO session_data (session_id, key, value, updated_at)
        VALUES (?, ?, ?, ?)
        ON CONFLICT(session_id, key) DO UPDATE SET
          value      = excluded.value,
          updated_at = excluded.updated_at
      ", params = list(session_id, key, blob, now))
    }

    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to save session state: %s", e$message))
    FALSE
  })
}

#' Load session state from database
#'
#' @param session_id Session ID to restore
#' @param con DBI connection returned by \code{init_session_db}
#' @return Named list of state components, or NULL if session not found
#' @export
load_session_state <- function(session_id, con) {
  # Check session exists
  meta <- DBI::dbGetQuery(
    con,
    "SELECT * FROM sessions WHERE session_id = ?",
    params = list(session_id)
  )
  if (nrow(meta) == 0) return(NULL)

  tryCatch({
    state <- list()

    # Deserialise data blobs
    rows <- DBI::dbGetQuery(
      con,
      "SELECT key, value FROM session_data WHERE session_id = ?",
      params = list(session_id)
    )

    for (i in seq_len(nrow(rows))) {
      blob <- rows$value[[i]]
      state[[ rows$key[i] ]] <- unserialize(blob)
    }

    # Attach metadata
    state$metadata <- as.list(meta[1, ])
    state$search_taxa <- tryCatch(
      jsonlite::fromJSON(meta$search_taxa[1]),
      error = function(e) meta$search_taxa[1]
    )

    state
  }, error = function(e) {
    warning(sprintf("Failed to load session state: %s", e$message))
    NULL
  })
}

#' List saved sessions
#'
#' @param con DBI connection returned by \code{init_session_db}
#' @return Data frame of saved sessions with metadata, or empty data frame
#' @export
list_saved_sessions <- function(con) {
  empty_df <- data.frame(
    session_id = character(0),
    created_at = character(0),
    updated_at = character(0),
    user_email = character(0),
    user_name = character(0),
    user_orcid = character(0),
    specimen_count = integer(0),
    species_count = integer(0),
    stringsAsFactors = FALSE
  )

  tryCatch({
    result <- DBI::dbGetQuery(con, "
      SELECT session_id, created_at, updated_at,
             user_email, user_name, user_orcid,
             specimen_count, species_count
      FROM sessions
      ORDER BY updated_at DESC
    ")
    if (nrow(result) == 0) return(empty_df)
    result
  }, error = function(e) {
    warning(sprintf("Failed to list sessions: %s", e$message))
    empty_df
  })
}

#' Filter saved sessions by user identifiers
#'
#' Matches sessions where ANY of the provided identifiers (email, ORCID, name)
#' match the session metadata.  Also checks whether the session_id is a hash
#' of the identifier (the app hashes user IDs into deterministic session IDs).
#'
#' @param user_email Optional email to match
#' @param user_orcid Optional ORCID to match
#' @param user_name Optional name to match
#' @param con DBI connection returned by \code{init_session_db}
#' @return Data frame of matching sessions
#' @export
filter_sessions_by_user <- function(user_email = NULL, user_orcid = NULL,
                                     user_name = NULL, con) {
  all_sessions <- list_saved_sessions(con)
  if (nrow(all_sessions) == 0) return(all_sessions)

  has_email <- !is.null(user_email) && nchar(trimws(user_email)) > 0
  has_orcid <- !is.null(user_orcid) && nchar(trimws(user_orcid)) > 0
  has_name  <- !is.null(user_name) && nchar(trimws(user_name)) > 0

  if (!has_email && !has_orcid && !has_name) {
    return(all_sessions[0, , drop = FALSE])
  }

  matches <- rep(FALSE, nrow(all_sessions))

  if (has_email) {
    email_hash <- digest::digest(tolower(trimws(user_email)), algo = "md5")
    matches <- matches |
      (tolower(trimws(all_sessions$user_email)) == tolower(trimws(user_email))) |
      (all_sessions$session_id == email_hash)
  }
  if (has_orcid) {
    orcid_hash <- digest::digest(trimws(user_orcid), algo = "md5")
    matches <- matches |
      (trimws(all_sessions$user_orcid) == trimws(user_orcid)) |
      (all_sessions$session_id == orcid_hash)
  }
  if (has_name) {
    name_hash <- digest::digest(tolower(trimws(user_name)), algo = "md5")
    matches <- matches |
      (tolower(trimws(all_sessions$user_name)) == tolower(trimws(user_name))) |
      (all_sessions$session_id == name_hash)
  }

  all_sessions[matches, , drop = FALSE]
}

#' Clean up old sessions beyond a maximum age
#'
#' @param max_age_days Maximum age in days before a session is deleted
#' @param con DBI connection returned by \code{init_session_db}
#' @return Number of sessions cleaned up
#' @export
cleanup_old_sessions <- function(max_age_days = 30, con) {
  tryCatch({
    cutoff <- format(Sys.time() - as.difftime(max_age_days, units = "days"),
                     "%Y-%m-%dT%H:%M:%S")
    # CASCADE foreign key deletes session_data rows automatically
    result <- DBI::dbExecute(
      con,
      "DELETE FROM sessions WHERE updated_at < ?",
      params = list(cutoff)
    )
    result
  }, error = function(e) {
    warning(sprintf("Failed to cleanup sessions: %s", e$message))
    0
  })
}
