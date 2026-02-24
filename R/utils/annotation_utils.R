# R/utils/annotation_utils.R
#
# Core annotation pipeline: extracting, merging, and preparing curator
# annotations (flags, notes, selections) for display and export.
# These functions are the critical path for curator work — annotations
# entered in BAGS grade tables must persist through to the specimen
# table and exports.
#
# All functions are pure (no Shiny dependency) and covered by
# tests/testthat/test-annotation_pipeline.R.

# Source the constants file for PREFERRED_COLUMNS
if (!exists("PREFERRED_COLUMNS")) {
  source("R/config/constants.R")
}

#' Extract a scalar value from an annotation entry
#'
#' Annotations may be stored as bare strings or as lists with named fields
#' (e.g. \code{list(flag = "misidentification", user = "...", timestamp = "...")}).
#' This helper normalises both formats to a single character value.
#'
#' @param entry The annotation entry (list or character scalar)
#' @param fields Character vector of field names to try, in priority order
#' @param default Value to return when entry is NULL or extraction fails
#' @return Character scalar
#' @keywords internal
extract_annotation <- function(entry, fields, default = "") {
  if (is.null(entry)) return(default)
  if (is.list(entry)) {
    for (f in fields) {
      val <- entry[[f]]
      if (!is.null(val)) return(as.character(val))
    }
    return(default)
  }
  as.character(entry)
}

#' Get available flag options
#' @return Named character vector of flag options
#' @export
get_flag_options <- function() {
  c(
    "None" = "",
    "Misidentification" = "misidentification",
    "Synonym" = "synonym",
    "ID Uncertain" = "id_uncertain",
    "Data Issue" = "data_issue",
    "Other Issue" = "other_issue"
  )
}

#' Order columns according to preferred configuration
#' @param data Data frame to reorder
#' @return Data frame with reordered columns
#' @keywords internal
order_columns <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Get preferred column order using the PREFERRED_COLUMNS function
  ordered_cols <- PREFERRED_COLUMNS(data)
  # Get actual columns that exist in the data
  valid_cols <- intersect(ordered_cols, names(data))
  # Get any remaining columns not in the preferred list
  other_cols <- setdiff(names(data), ordered_cols)

  # Combine the columns in the desired order
  data[, c(valid_cols, other_cols)]
}

#' Prepare data for table display with annotation columns merged
#'
#' Takes raw specimen data and merges in the current annotation state
#' (selections, flags, curator notes) so the table renders correctly.
#' This is the single point where annotations are applied to display data.
#'
#' @param data Data frame of specimen data (must contain processid column)
#' @param current_selections Named list of selections keyed by processid
#' @param current_flags Named list of flags keyed by processid
#' @param current_notes Named list of curator notes keyed by processid
#' @param current_updated_ids Named list of updated IDs keyed by processid
#' @param logger Optional logger instance
#' @return Data frame with annotation columns merged
#' @export
prepare_module_data <- function(data,
                                current_selections = NULL,
                                current_flags = NULL,
                                current_notes = NULL,
                                current_updated_ids = NULL,
                                logger = NULL) {

  if (is.null(data) || nrow(data) == 0) {
    if (!is.null(logger)) logger$warn("Empty input data to prepare_module_data")
    return(data.frame())
  }

  # Log initial state
  if (!is.null(logger)) {
    logger$info("Pre-format specimen table data", details = list(
      rows = nrow(data),
      columns = names(data),
      sample_processids = head(data$processid)
    ))
  }

  # Convert problematic column types
  for (col in names(data)) {
    if (is.list(data[[col]]) || is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  # Add missing standard columns if needed
  missing_cols <- setdiff(PREFERRED_COLUMNS(data), names(data))
  for (col in missing_cols) {
    data[[col]] <- NA_character_
  }

  # Merge annotation columns from state into the data frame.
  # extract_annotation() handles both list and bare-string formats.
  data$selected <- vapply(data$processid, function(pid) {
    !is.null(current_selections[[pid]])
  }, logical(1), USE.NAMES = FALSE)

  data$flag <- vapply(data$processid, function(pid) {
    extract_annotation(current_flags[[pid]], c("flag", "value"))
  }, character(1), USE.NAMES = FALSE)

  data$updated_id <- vapply(data$processid, function(pid) {
    extract_annotation(current_updated_ids[[pid]], c("text", "value"))
  }, character(1), USE.NAMES = FALSE)

  data$curator_notes <- vapply(data$processid, function(pid) {
    extract_annotation(current_notes[[pid]], c("text", "note", "value"))
  }, character(1), USE.NAMES = FALSE)

  # Final cleanup - ensure no NULL values
  for (col in names(data)) {
    if (any(sapply(data[[col]], is.null))) {
      data[[col]][sapply(data[[col]], is.null)] <- NA
    }
  }

  # Log final state
  if (!is.null(logger)) {
    logger$info("Post-format specimen table", details = list(
      table_class = class(data),
      table_columns = if(is.data.frame(data)) names(data) else "Not a data frame"
    ))
  }

  # Ensure proper types before returning
  data$selected <- as.logical(data$selected)
  data$flag <- as.character(data$flag)
  data$updated_id <- as.character(data$updated_id)
  data$curator_notes <- as.character(data$curator_notes)
  if ("quality_score" %in% names(data)) {
    data$quality_score <- as.numeric(data$quality_score)
  }
  if ("rank" %in% names(data)) {
    data$rank <- as.numeric(data$rank)
  }
  return(data)
}

#' Merge curator annotations into specimen data for export
#'
#' Produces clean text columns for selected, flag, curator_notes, and
#' audit-trail fields (flag_user, flag_timestamp). Used by all export
#' paths (ExportManager, download handlers) to ensure consistent
#' annotation inclusion.
#'
#' @param data Data frame of specimen data (must contain processid column)
#' @param selections Named list of selections keyed by processid
#' @param flags Named list of flags keyed by processid (each entry has $flag, $user, $timestamp)
#' @param notes Named list of curator notes keyed by processid (each entry has $text, $user, $timestamp)
#' @param updated_ids Named list of updated IDs keyed by processid (each entry has $text, $user, $timestamp)
#' @return Data frame with clean text annotation columns appended
#' @export
merge_annotations_for_export <- function(data, selections = NULL, flags = NULL, notes = NULL, updated_ids = NULL) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Coerce list and factor columns to plain character first so that

  # write.table() never encounters complex types and strip_html covers
  # every column.
  for (col in names(data)) {
    if (is.list(data[[col]])) {
      data[[col]] <- vapply(data[[col]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        paste(x, collapse = "; ")
      }, character(1))
    } else if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  # Strip any HTML tags from character columns (safety net for DT render
  # functions that wrap values in <div class="cell-content">...</div>).
  strip_html <- function(x) gsub("<[^>]+>", "", x)
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      data[[col]] <- strip_html(data[[col]])
    }
  }

  data$selected <- data$processid %in% names(selections)

  data$flag <- vapply(data$processid, function(pid) {
    extract_annotation(flags[[pid]], c("flag", "value"))
  }, character(1), USE.NAMES = FALSE)

  data$updated_id <- vapply(data$processid, function(pid) {
    extract_annotation(updated_ids[[pid]], c("text", "value"))
  }, character(1), USE.NAMES = FALSE)

  data$curator_notes <- vapply(data$processid, function(pid) {
    extract_annotation(notes[[pid]], c("text", "note", "value"))
  }, character(1), USE.NAMES = FALSE)

  # Metadata fields (user, timestamp) only exist in list-format entries.
  # Bare-string flags have no metadata, so return default "".
  data$flag_user <- vapply(data$processid, function(pid) {
    entry <- flags[[pid]]
    if (is.list(entry)) extract_annotation(entry, "user") else ""
  }, character(1), USE.NAMES = FALSE)

  data$flag_timestamp <- vapply(data$processid, function(pid) {
    entry <- flags[[pid]]
    if (is.list(entry)) extract_annotation(entry, "timestamp") else ""
  }, character(1), USE.NAMES = FALSE)

  # Reorder: annotation columns first, then the rest
  annotation_cols <- c("selected", "flag", "updated_id", "curator_notes",
                       "flag_user", "flag_timestamp")
  other_cols <- setdiff(names(data), annotation_cols)
  data[, c(annotation_cols, other_cols)]
}
