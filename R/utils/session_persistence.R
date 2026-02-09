# R/utils/session_persistence.R

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

    # Save session metadata as JSON
    meta <- list(
      session_id = session_id,
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      search_taxa = store$search_taxa,
      specimen_count = if (!is.null(store$specimen_data)) nrow(store$specimen_data) else 0,
      species_count = if (!is.null(store$specimen_data)) length(unique(store$specimen_data$species)) else 0,
      user_email = if (!is.null(store$user_info)) store$user_info$email else NULL,
      user_name = if (!is.null(store$user_info)) store$user_info$name else NULL
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
      specimen_curator_notes = "specimen_curator_notes.rds"
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
  if (!dir.exists(session_dir)) {
    return(data.frame(
      session_id = character(0),
      created_at = character(0),
      updated_at = character(0),
      user_email = character(0),
      specimen_count = integer(0),
      species_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  session_dirs <- list.dirs(session_dir, recursive = FALSE, full.names = FALSE)
  if (length(session_dirs) == 0) {
    return(data.frame(
      session_id = character(0),
      created_at = character(0),
      updated_at = character(0),
      user_email = character(0),
      specimen_count = integer(0),
      species_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  sessions <- lapply(session_dirs, function(sid) {
    meta_file <- file.path(session_dir, sid, "session_meta.json")
    if (!file.exists(meta_file)) return(NULL)

    tryCatch({
      meta <- jsonlite::fromJSON(meta_file)
      data.frame(
        session_id = sid,
        created_at = meta$created_at %||% "",
        updated_at = meta$updated_at %||% "",
        user_email = meta$user_email %||% "",
        specimen_count = meta$specimen_count %||% 0L,
        species_count = meta$species_count %||% 0L,
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  sessions <- sessions[!sapply(sessions, is.null)]
  if (length(sessions) == 0) {
    return(data.frame(
      session_id = character(0),
      created_at = character(0),
      updated_at = character(0),
      user_email = character(0),
      specimen_count = integer(0),
      species_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, sessions)
  result[order(result$updated_at, decreasing = TRUE), ]
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
