# R/utils/session_persistence.R

# Null-coalescing helper (base R %||% requires R >= 4.4.0)
.null_default <- function(x, default) {
  if (is.null(x)) default else x
}

#' Save session state to disk
#'
#' Persists specimen data, BAGS grades, BIN analysis, annotations, and
#' metadata to a session directory under data/sessions/.
#'
#' @param session_id Unique session identifier
#' @param store The state store (reactiveValues accessed via isolate)
#' @param session_dir Base directory for sessions (default: "data/sessions")
#' @return TRUE on success, FALSE on failure
#' @export
save_session_state <- function(session_id, store, session_dir = "data/sessions") {
  tryCatch({
    dir_path <- file.path(session_dir, session_id)
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

    # Save large data frames as RDS
    if (!is.null(store$specimen_data)) {
      saveRDS(store$specimen_data, file.path(dir_path, "specimen_data.rds"))
    }
    if (!is.null(store$bags_grades)) {
      saveRDS(store$bags_grades, file.path(dir_path, "bags_grades.rds"))
    }
    if (!is.null(store$bin_analysis)) {
      saveRDS(store$bin_analysis, file.path(dir_path, "bin_analysis.rds"))
    }

    # Save annotations as RDS
    saveRDS(store$selected_specimens, file.path(dir_path, "selected_specimens.rds"))
    saveRDS(store$specimen_flags, file.path(dir_path, "specimen_flags.rds"))
    saveRDS(store$specimen_curator_notes, file.path(dir_path, "specimen_curator_notes.rds"))
    saveRDS(store$specimen_updated_ids, file.path(dir_path, "specimen_updated_ids.rds"))

    # Save session metadata as JSON
    meta <- list(
      session_id = session_id,
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      search_taxa = store$search_taxa,
      specimen_count = if (!is.null(store$specimen_data)) nrow(store$specimen_data) else 0,
      species_count = if (!is.null(store$specimen_data)) length(unique(store$specimen_data$species)) else 0,
      user_email = if (!is.null(store$user_info)) store$user_info$email else NULL,
      user_name = if (!is.null(store$user_info)) store$user_info$name else NULL,
      user_orcid = if (!is.null(store$user_info)) store$user_info$orcid else NULL
    )

    # Write created_at only on first save
    meta_file <- file.path(dir_path, "session_meta.json")
    if (file.exists(meta_file)) {
      existing <- tryCatch(jsonlite::fromJSON(meta_file), error = function(e) list())
      meta$created_at <- existing$created_at
    } else {
      meta$created_at <- meta$updated_at
    }

    jsonlite::write_json(meta, meta_file, auto_unbox = TRUE, pretty = TRUE)

    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to save session state: %s", e$message))
    FALSE
  })
}

#' Load session state from disk
#'
#' @param session_id Session ID to restore
#' @param session_dir Base directory for sessions
#' @return Named list of state components, or NULL on failure
#' @export
load_session_state <- function(session_id, session_dir = "data/sessions") {
  dir_path <- file.path(session_dir, session_id)
  if (!dir.exists(dir_path)) return(NULL)

  tryCatch({
    state <- list()

    rds_files <- list(
      specimen_data = "specimen_data.rds",
      bags_grades = "bags_grades.rds",
      bin_analysis = "bin_analysis.rds",
      selected_specimens = "selected_specimens.rds",
      specimen_flags = "specimen_flags.rds",
      specimen_curator_notes = "specimen_curator_notes.rds",
      specimen_updated_ids = "specimen_updated_ids.rds"
    )

    for (key in names(rds_files)) {
      path <- file.path(dir_path, rds_files[[key]])
      if (file.exists(path)) {
        state[[key]] <- readRDS(path)
      }
    }

    # Load metadata
    meta_file <- file.path(dir_path, "session_meta.json")
    if (file.exists(meta_file)) {
      state$metadata <- jsonlite::fromJSON(meta_file)
      state$search_taxa <- state$metadata$search_taxa
    }

    state
  }, error = function(e) {
    warning(sprintf("Failed to load session state: %s", e$message))
    NULL
  })
}

#' List saved sessions
#'
#' @param session_dir Base directory for sessions
#' @return Data frame of saved sessions with metadata, or empty data frame
#' @export
list_saved_sessions <- function(session_dir = "data/sessions") {
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

  if (!dir.exists(session_dir)) return(empty_df)

  session_dirs <- list.dirs(session_dir, recursive = FALSE, full.names = FALSE)
  if (length(session_dirs) == 0) return(empty_df)

  sessions <- lapply(session_dirs, function(sid) {
    meta_file <- file.path(session_dir, sid, "session_meta.json")
    if (!file.exists(meta_file)) return(NULL)

    tryCatch({
      meta <- jsonlite::fromJSON(meta_file)
      data.frame(
        session_id = sid,
        created_at = .null_default(meta$created_at, ""),
        updated_at = .null_default(meta$updated_at, ""),
        user_email = .null_default(meta$user_email, ""),
        user_name = .null_default(meta$user_name, ""),
        user_orcid = .null_default(meta$user_orcid, ""),
        specimen_count = .null_default(meta$specimen_count, 0L),
        species_count = .null_default(meta$species_count, 0L),
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  sessions <- sessions[!sapply(sessions, is.null)]
  if (length(sessions) == 0) return(empty_df)

  result <- do.call(rbind, sessions)
  result[order(result$updated_at, decreasing = TRUE), ]
}

#' Filter saved sessions by user identifiers
#'
#' Matches sessions where ANY of the provided identifiers (email, ORCID, name)
#' match the session metadata. This allows users to find their previous sessions
#' regardless of which identifier they used when creating them.
#'
#' @param user_email Optional email to match
#' @param user_orcid Optional ORCID to match
#' @param user_name Optional name to match
#' @param session_dir Base directory for sessions
#' @return Data frame of matching sessions, or all sessions if no identifiers provided
#' @export
filter_sessions_by_user <- function(user_email = NULL, user_orcid = NULL,
                                     user_name = NULL, session_dir = "data/sessions") {
  all_sessions <- list_saved_sessions(session_dir)
  if (nrow(all_sessions) == 0) return(all_sessions)

  # If no identifiers provided, return all sessions
  has_email <- !is.null(user_email) && nchar(trimws(user_email)) > 0
  has_orcid <- !is.null(user_orcid) && nchar(trimws(user_orcid)) > 0
  has_name  <- !is.null(user_name) && nchar(trimws(user_name)) > 0

  if (!has_email && !has_orcid && !has_name) return(all_sessions[0, , drop = FALSE])

  # Match on any identifier (OR logic)
  matches <- rep(FALSE, nrow(all_sessions))

  if (has_email) {
    matches <- matches | (tolower(trimws(all_sessions$user_email)) == tolower(trimws(user_email)))
  }
  if (has_orcid) {
    matches <- matches | (trimws(all_sessions$user_orcid) == trimws(user_orcid))
  }
  if (has_name) {
    matches <- matches | (tolower(trimws(all_sessions$user_name)) == tolower(trimws(user_name)))
  }

  all_sessions[matches, , drop = FALSE]
}

#' Clean up old sessions beyond a maximum age
#'
#' @param max_age_days Maximum age in days before a session is deleted
#' @param session_dir Base directory for sessions
#' @return Number of sessions cleaned up
#' @export
cleanup_old_sessions <- function(max_age_days = 30, session_dir = "data/sessions") {
  sessions <- list_saved_sessions(session_dir)
  if (nrow(sessions) == 0) return(0)

  cutoff <- Sys.time() - as.difftime(max_age_days, units = "days")
  old_sessions <- sessions[as.POSIXct(sessions$updated_at) < cutoff, ]

  removed <- 0
  for (sid in old_sessions$session_id) {
    dir_path <- file.path(session_dir, sid)
    if (dir.exists(dir_path)) {
      unlink(dir_path, recursive = TRUE)
      removed <- removed + 1
    }
  }

  removed
}
