# R/utils/specimen_ranking.R

#' Calculate specimen rank based on criteria met
#' @param specimen Single specimen record
#' @return Numeric rank (1-7)
#' @export
calculate_specimen_rank <- function(specimen) {
  tryCatch({
    if (is.null(specimen$criteria_met) || is.na(specimen$criteria_met) ||
        specimen$criteria_met == "") {
      return(7)
    }

    criteria_list <- strsplit(specimen$criteria_met, "; ")[[1]]
    rank_criteria <- get_rank_criteria()

    for (rank in 1:6) {
      rank_key <- paste0("RANK_", rank)
      if (all(rank_criteria[[rank_key]] %in% criteria_list)) {
        return(rank)
      }
    }

    return(7)
  }, error = function(e) {
    warning(sprintf("Error calculating specimen rank: %s", e$message))
    7
  })
}

#' Get rank criteria from constants
#' @return List of criteria for each rank
#' @keywords internal
get_rank_criteria <- function() {
  SPECIMEN_RANK_CRITERIA
}

#' Rank specimens within a species group
#' @param specimens Data frame of specimens for one species
#' @return Data frame with updated ranks
#' @export
rank_species_specimens <- function(specimens) {
  tryCatch({
    if (is.null(specimens) || nrow(specimens) == 0) return(specimens)

    # Calculate ranks
    specimens$rank <- sapply(1:nrow(specimens), function(i) {
      calculate_specimen_rank(specimens[i,])
    })

    # Sort by rank and quality score
    specimens <- specimens[order(specimens$rank, -specimens$quality_score), ]

    # Validate rankings
    validation <- validate_specimen_ranks(specimens)
    if (!validation$valid) {
      warning(paste(validation$messages, collapse = "; "))
    }

    specimens
  }, error = function(e) {
    warning(sprintf("Error ranking specimens: %s", e$message))
    specimens
  })
}

#' Validate specimen ranks
#' @param specimens Data frame of ranked specimens
#' @return List with validation results
#' @keywords internal
validate_specimen_ranks <- function(specimens) {
  messages <- character()

  # Check rank range
  invalid_ranks <- specimens$rank[!is.na(specimens$rank) &
                                    (specimens$rank < 1 | specimens$rank > 7)]
  if (length(invalid_ranks) > 0) {
    messages <- c(messages, "Invalid rank values detected")
  }

  # Check rank distribution
  rank_dist <- table(specimens$rank)
  if (length(rank_dist) > 0 && !all(names(rank_dist) %in% 1:7)) {
    messages <- c(messages, "Unexpected rank values present")
  }

  # Validate rank-criteria consistency
  for (i in 1:nrow(specimens)) {
    spec <- specimens[i,]
    if (!is.na(spec$rank) && spec$rank < 7) {
      rank_key <- paste0("RANK_", spec$rank)
      required_criteria <- SPECIMEN_RANK_CRITERIA[[rank_key]]

      if (!is.na(spec$criteria_met) && spec$criteria_met != "") {
        criteria_list <- strsplit(spec$criteria_met, "; ")[[1]]
        if (!all(required_criteria %in% criteria_list)) {
          messages <- c(messages, sprintf(
            "Rank-criteria mismatch for specimen %s", spec$processid))
        }
      }
    }
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}
