# R/utils/bags_grading.R

#' Calculate BAGS grade for specimens
#' @param specimens Data frame of specimens
#' @return Data frame with BAGS grades and analysis
#' @export
calculate_bags_grade <- function(specimens) {
  tryCatch({
    # Filter for valid species names
    valid_specimens <- specimens[!is.na(specimens$species) &
                                   specimens$species != "" &
                                   !grepl("sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.",
                                          specimens$species), ]

    if (nrow(valid_specimens) == 0) {
      return(create_empty_grades_df())
    }

    # Group by species
    species_data <- split(valid_specimens, valid_specimens$species)

    # Calculate grades for each species
    grades <- lapply(species_data, calculate_species_grade, full_dataset = valid_specimens)

    # Combine and return results
    result <- do.call(rbind, grades)
    if (is.null(result)) return(create_empty_grades_df())
    result
  }, error = function(e) {
    warning(sprintf("Error calculating BAGS grades: %s", e$message))
    create_empty_grades_df()
  })
}

#' Calculate grade for a single species
#' @param species_specimens Specimens for one species
#' @param full_dataset Complete specimen dataset for BIN sharing check
#' @return Data frame row with grade analysis
#' @keywords internal
calculate_species_grade <- function(species_specimens, full_dataset) {
  tryCatch({
    species_name <- unique(species_specimens$species)[1]
    specimen_count <- nrow(species_specimens)

    # Get valid BINs
    species_bins <- get_valid_bins(species_specimens)
    bin_count <- length(species_bins)

    # Check for shared BINs
    has_shared_bins <- check_shared_bins(species_bins, full_dataset)

    # Calculate metrics
    bin_coverage <- calculate_bin_coverage(species_specimens)
    quality_metrics <- calculate_quality_metrics(species_specimens)

    data.frame(
      species = species_name,
      bags_grade = determine_bags_grade(specimen_count, bin_count, has_shared_bins),
      specimen_count = specimen_count,
      bin_count = bin_count,
      shared_bins = has_shared_bins,
      bin_coverage = bin_coverage,
      avg_quality = quality_metrics$avg_quality,
      min_quality = quality_metrics$min_quality,
      max_quality = quality_metrics$max_quality,
      countries = length(unique(species_specimens$country.ocean)),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning(sprintf("Error calculating grade for species %s: %s", species_name, e$message))
    NULL
  })
}

#' Get valid BINs for specimens
#' @param specimens Specimen data frame
#' @return Character vector of valid BIN URIs
#' @keywords internal
get_valid_bins <- function(specimens) {
  unique(specimens$bin_uri[!is.na(specimens$bin_uri) & specimens$bin_uri != ""])
}

#' Check if BINs are shared with other species
#' @param bins Vector of BIN URIs
#' @param full_dataset Complete specimen dataset
#' @return Boolean indicating if any BINs are shared
#' @keywords internal
check_shared_bins <- function(bins, full_dataset) {
  if (length(bins) == 0) return(FALSE)

  any(sapply(bins, function(bin) {
    bin_specimens <- full_dataset[!is.na(full_dataset$bin_uri) &
                                    full_dataset$bin_uri == bin, ]
    length(unique(bin_specimens$species)) > 1
  }))
}

#' Calculate BIN coverage percentage
#' @param specimens Specimen data frame
#' @return Numeric percentage of specimens with BINs
#' @keywords internal
calculate_bin_coverage <- function(specimens) {
  if (nrow(specimens) == 0) return(0)
  mean(!is.na(specimens$bin_uri) & specimens$bin_uri != "") * 100
}

#' Calculate specimen quality metrics
#' @param specimens Specimen data frame
#' @return List of quality statistics
#' @keywords internal
calculate_quality_metrics <- function(specimens) {
  if (nrow(specimens) == 0) {
    return(list(avg_quality = 0, min_quality = 0, max_quality = 0))
  }

  quality_scores <- specimens$quality_score
  list(
    avg_quality = mean(quality_scores, na.rm = TRUE),
    min_quality = min(quality_scores, na.rm = TRUE),
    max_quality = max(quality_scores, na.rm = TRUE)
  )
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
    avg_quality = numeric(),
    min_quality = numeric(),
    max_quality = numeric(),
    countries = integer(),
    stringsAsFactors = FALSE
  )
}

#' Determine BAGS grade
#' @param specimen_count Number of specimens
#' @param bin_count Number of BINs
#' @param has_shared_bins Boolean indicating if BINs are shared
#' @return Character indicating BAGS grade (A-E)
#' @export
determine_bags_grade <- function(specimen_count, bin_count, has_shared_bins) {
  tryCatch({
    if (is.na(specimen_count) || is.na(bin_count)) return('E')
    if (has_shared_bins) return('E')
    if (bin_count > 1) return('C')
    if (specimen_count < 3) return('D')
    if (specimen_count > 10) return('A')
    'B'
  }, error = function(e) {
    warning(sprintf("Error determining BAGS grade: %s", e$message))
    'E'  # Return lowest grade on error
  })
}

#' Validate BAGS grades data frame
#' @param grades Data frame of BAGS grades
#' @return Boolean indicating if grades are valid
#' @export
validate_bags_grades <- function(grades) {
  tryCatch({
    if (!is.data.frame(grades) || nrow(grades) == 0) return(FALSE)

    required_cols <- c("species", "bags_grade", "specimen_count",
                       "bin_count", "shared_bins", "bin_coverage")
    if (!all(required_cols %in% names(grades))) return(FALSE)

    if (!all(grades$bags_grade %in% c("A", "B", "C", "D", "E"))) return(FALSE)
    if (!is.numeric(grades$specimen_count)) return(FALSE)
    if (!is.numeric(grades$bin_count)) return(FALSE)
    if (!is.logical(grades$shared_bins)) return(FALSE)

    TRUE
  }, error = function(e) {
    warning(sprintf("Error validating BAGS grades: %s", e$message))
    FALSE
  })
}

#' Summarize BAGS grades
#' @param grades Data frame of BAGS grades
#' @return List of summary statistics
#' @export
summarize_bags_grades <- function(grades) {
  tryCatch({
    if (!validate_bags_grades(grades)) {
      return(create_empty_summary())
    }

    list(
      total_species = nrow(grades),
      grade_counts = table(grades$bags_grade),
      avg_specimens = mean(grades$specimen_count),
      avg_bins = mean(grades$bin_count),
      shared_bins_count = sum(grades$shared_bins),
      avg_coverage = mean(grades$bin_coverage),
      timestamp = Sys.time()
    )
  }, error = function(e) {
    warning(sprintf("Error summarizing BAGS grades: %s", e$message))
    create_empty_summary()
  })
}

#' Create empty summary list
#' @return Empty summary structure
#' @keywords internal
create_empty_summary <- function() {
  list(
    total_species = 0,
    grade_counts = table(character()),
    avg_specimens = 0,
    avg_bins = 0,
    shared_bins_count = 0,
    avg_coverage = 0,
    timestamp = Sys.time()
  )
}
