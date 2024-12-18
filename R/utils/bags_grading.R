# R/utils/bags_grading.R

#' Calculate BAGS grade for specimens
#' @param specimens Data frame of specimens
#' @return Data frame with BAGS grades and analysis
#' @export
calculate_bags_grade <- function(specimens) {
  tryCatch({
    # Filter for specimens with any taxonomic ID
    specimens <- specimens[!is.na(specimens$species) | !is.na(specimens$genus), ]

    if (nrow(specimens) == 0) {
      return(create_empty_grades_df())
    }

    # Group by valid species names
    valid_taxa <- unique(specimens$species[!is.na(specimens$species) & specimens$species != ""])
    grades <- lapply(valid_taxa, function(taxon) {
      # Get all specimens in BINs containing this taxon
      taxon_specimens <- specimens[specimens$species == taxon & !is.na(specimens$species), ]

      if (nrow(taxon_specimens) == 0) {
        return(NULL)
      }

      # Count specimens with valid BINs
      valid_bins <- taxon_specimens$bin_uri[!is.na(taxon_specimens$bin_uri) &
                                              taxon_specimens$bin_uri != ""]
      bin_count <- length(unique(valid_bins))

      # Count valid specimens (excluding cf./aff.)
      valid_specimens <- sum(!grepl("cf\\.|aff\\.", taxon_specimens$species))

      # Check for shared/discordant BINs
      has_shared_bins <- check_shared_bins(specimens, taxon, valid_bins)

      # Calculate BIN coverage
      bin_coverage <- calculate_bin_coverage(taxon_specimens)

      # Create result row
      data.frame(
        species = taxon,
        bags_grade = determine_bags_grade(valid_specimens, bin_count, has_shared_bins),
        specimen_count = valid_specimens,
        bin_count = bin_count,
        shared_bins = has_shared_bins,
        bin_coverage = bin_coverage,
        stringsAsFactors = FALSE
      )
    })

    result <- do.call(rbind, grades[!sapply(grades, is.null)])
    if (is.null(result)) return(create_empty_grades_df())

    # Add row names
    rownames(result) <- NULL
    result

  }, error = function(e) {
    warning(sprintf("Error calculating BAGS grades: %s", e$message))
    create_empty_grades_df()
  })
}

#' Check if BINs are shared/discordant
#' @param specimens All specimens data frame
#' @param taxon Species being checked
#' @param taxon_bins BINs assigned to this taxon
#' @return Boolean indicating if BINs are shared
#' @keywords internal
check_shared_bins <- function(specimens, taxon, taxon_bins) {
  if (length(taxon_bins) == 0) return(FALSE)

  # For each BIN containing the taxon
  any(sapply(taxon_bins, function(bin) {
    # Get all specimens in this BIN
    bin_specimens <- specimens[specimens$bin_uri == bin & !is.na(specimens$bin_uri), ]

    if (nrow(bin_specimens) == 0) return(FALSE)

    # Get unique valid species in this BIN (excluding cf./aff.)
    bin_species <- unique(bin_specimens$species[
      !is.na(bin_specimens$species) &
        !grepl("cf\\.|aff\\.", bin_specimens$species) &
        bin_specimens$species != ""
    ])

    # Check if this BIN contains other valid species
    length(bin_species) > 1 ||
      (length(bin_species) == 1 && bin_species != taxon)
  }))
}

#' Determine BAGS grade based on criteria
#' @param specimen_count Number of valid specimens
#' @param bin_count Number of unique BINs
#' @param has_shared_bins Boolean indicating if BINs are shared
#' @return Character indicating BAGS grade (A-E)
#' @keywords internal
determine_bags_grade <- function(specimen_count, bin_count, has_shared_bins) {
  tryCatch({
    # Handle invalid inputs
    if (is.na(specimen_count) || is.na(bin_count)) return('E')

    # Grade E: Shared BINs
    if (has_shared_bins) return('E')

    # Grade C: Multiple BINs
    if (bin_count > 1) return('C')

    # Grade D: Less than 3 specimens
    if (specimen_count < 3) return('D')

    # Grade A: More than 10 specimens
    if (specimen_count >= 10) return('A')

    # Grade B: 3-10 specimens
    return('B')

  }, error = function(e) {
    warning(sprintf("Error determining BAGS grade: %s", e$message))
    'E'
  })
}

#' Calculate BIN coverage percentage
#' @param specimens Specimen data frame
#' @return Numeric percentage of specimens with BINs
#' @keywords internal
calculate_bin_coverage <- function(specimens) {
  if (nrow(specimens) == 0) return(0)

  # Calculate percentage of specimens with valid BINs
  mean(!is.na(specimens$bin_uri) & specimens$bin_uri != "") * 100
}

#' Create empty grades data frame
#' @return Empty data frame with correct structure
#' @keywords internal
create_empty_grades_df <- function() {
  data.frame(
    species = character(),
    bags_grade = character(),
    specimen_count = integer(),
    bin_count = integer(),
    shared_bins = logical(),
    bin_coverage = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Validate BAGS grades
#' @param grades Data frame of BAGS grades
#' @return List with validation results
#' @export
validate_bags_grades <- function(grades) {
  if (is.null(grades) || nrow(grades) == 0) {
    return(list(valid = FALSE, messages = "Empty grades data frame"))
  }

  messages <- character()

  # Check required columns
  required_cols <- c("species", "bags_grade", "specimen_count",
                     "bin_count", "shared_bins", "bin_coverage")
  missing_cols <- setdiff(required_cols, names(grades))

  if (length(missing_cols) > 0) {
    messages <- c(messages,
                  sprintf("Missing required columns: %s",
                          paste(missing_cols, collapse = ", ")))
  }

  # Validate grade values
  invalid_grades <- grades$bags_grade[!grades$bags_grade %in% c("A", "B", "C", "D", "E")]
  if (length(invalid_grades) > 0) {
    messages <- c(messages, "Invalid grade values detected")
  }

  # Validate numeric columns
  if ("specimen_count" %in% names(grades)) {
    if (any(grades$specimen_count < 0, na.rm = TRUE)) {
      messages <- c(messages, "Negative specimen counts detected")
    }
  }

  if ("bin_count" %in% names(grades)) {
    if (any(grades$bin_count < 0, na.rm = TRUE)) {
      messages <- c(messages, "Negative BIN counts detected")
    }
  }

  if ("bin_coverage" %in% names(grades)) {
    if (any(grades$bin_coverage < 0 | grades$bin_coverage > 100, na.rm = TRUE)) {
      messages <- c(messages, "Invalid BIN coverage values detected")
    }
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}
