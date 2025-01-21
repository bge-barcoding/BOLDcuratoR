# R/utils/bags_grading.R

#' Calculate BAGS grade for specimens
#' @param specimens Data frame of specimens
#' @return Data frame with BAGS grades and analysis
#' @export
calculate_bags_grade <- function(specimens) {
  tryCatch({
    # Filter for specimens with species-level identification
    specimens <- specimens[
      specimens$identification_rank %in% c("species", "subspecies") &
        !is.na(specimens$species) &
        specimens$species != "",
    ]

    if (nrow(specimens) == 0) {
      return(create_empty_grades_df())
    }

    # Get unique species
    valid_taxa <- unique(specimens$species)

    # Process each taxon
    grades <- lapply(valid_taxa, function(taxon) {
      # Get all specimens for this exact species name
      taxon_specimens <- specimens[specimens$species == taxon, ]

      if (nrow(taxon_specimens) == 0) {
        return(NULL)
      }

      # Get valid BINs (excluding NA/empty values)
      valid_bins <- unique(taxon_specimens$bin_uri[
        !is.na(taxon_specimens$bin_uri) &
          taxon_specimens$bin_uri != ""
      ])

      bin_count <- length(valid_bins)

      # Count total specimens
      specimen_count <- nrow(taxon_specimens)

      # Enhanced shared BIN detection
      has_shared_bins <- check_shared_bins(specimens, taxon, valid_bins)

      # Create result row
      data.frame(
        species = taxon,
        bags_grade = determine_bags_grade(specimen_count, bin_count, has_shared_bins),
        specimen_count = specimen_count,
        bin_count = bin_count,
        shared_bins = has_shared_bins,
        stringsAsFactors = FALSE
      )
    })

    result <- do.call(rbind, grades[!sapply(grades, is.null)])
    if (is.null(result)) return(create_empty_grades_df())

    rownames(result) <- NULL
    result

  }, error = function(e) {
    warning(sprintf("Error calculating BAGS grades: %s", e$message))
    create_empty_grades_df()
  })
}

#' Enhanced check for shared/discordant BINs
#' @param specimens All specimens data frame
#' @param taxon Species being checked
#' @param taxon_bins BINs assigned to this taxon
#' @return Boolean indicating if BINs are shared
#' @keywords internal
check_shared_bins <- function(specimens, taxon, taxon_bins) {
  if (length(taxon_bins) == 0) return(FALSE)

  # For each BIN assigned to the taxon
  shared <- sapply(taxon_bins, function(bin) {
    # Get all specimens in this BIN across all species
    bin_specimens <- specimens[
      !is.na(specimens$bin_uri) &
        specimens$bin_uri != "" &
        specimens$bin_uri == bin &
        !is.na(specimens$species) &
        specimens$species != "" &
        specimens$identification_rank %in% c("species", "subspecies"),
    ]

    if (nrow(bin_specimens) == 0) return(FALSE)

    # Get unique species in this BIN
    bin_species <- unique(bin_specimens$species)

    # Check if this BIN contains multiple species
    length(bin_species) > 1 ||
      (length(bin_species) == 1 && bin_species != taxon)
  })

  # Return TRUE if any BIN is shared
  any(shared, na.rm = TRUE)
}

#' Determine BAGS grade based on criteria
#' @param specimen_count Number of specimens
#' @param bin_count Number of unique BINs
#' @param has_shared_bins Boolean indicating if BINs are shared
#' @return Character indicating BAGS grade (A-E)
#' @keywords internal
determine_bags_grade <- function(specimen_count, bin_count, has_shared_bins) {
  tryCatch({
    # Input validation with strict type checking
    if (!is.numeric(specimen_count) || !is.numeric(bin_count)) return('E')
    if (is.na(specimen_count) || is.na(bin_count)) return('E')
    if (is.na(has_shared_bins)) has_shared_bins <- FALSE

    # Grade E conditions (highest priority):
    # - Shared BINs
    # - Invalid specimen/BIN counts
    if (has_shared_bins || specimen_count < 0 || bin_count < 0) return('E')

    # Grade C: Multiple BINs
    if (bin_count > 1) return('C')

    # Grade D: Less than 3 specimens
    if (specimen_count < 3) return('D')

    # Grade A: 11 or more specimens
    if (specimen_count >= 11) return('A')

    # Grade B: 3-10 specimens with single BIN
    return('B')

  }, error = function(e) {
    warning(sprintf("Error determining BAGS grade: %s", e$message))
    'E'
  })
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
                     "bin_count", "shared_bins")
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

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}
