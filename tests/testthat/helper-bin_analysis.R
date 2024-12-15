# Test Helpers for BIN Analysis Module

#' Create Sample Specimen Data
#'
#' @param num_specimens Number of specimens to generate
#' @param num_bins Number of unique BINs
#' @param num_species Number of unique species
#' @return Data frame of specimen data
create_sample_specimens <- function(num_specimens = 10, num_bins = 3, num_species = 4) {
  # Generate random BINs
  bins <- paste0("BOLD:AA", sprintf("%03d", 1:num_bins))
  species <- paste("Species", LETTERS[1:num_species])
  countries <- c("Canada", "USA", "Mexico", "Brazil")

  # Create specimen data
  data.frame(
    bin_uri = sample(bins, num_specimens, replace = TRUE),
    species = sample(species, num_specimens, replace = TRUE),
    country.ocean = sample(countries, num_specimens, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Create Expected BIN Analysis Results
#'
#' @param specimen_data Data frame of specimen data
#' @return List of expected analysis results
create_expected_results <- function(specimen_data) {
  # Calculate expected values
  unique_bins <- unique(specimen_data$bin_uri)
  unique_species <- unique(specimen_data$species)
  unique_countries <- unique(specimen_data$country.ocean)

  list(
    total_bins = length(unique_bins),
    total_species = length(unique_species),
    total_countries = length(unique_countries),
    total_specimens = nrow(specimen_data)
  )
}

#' Mock BIN API Response
#'
#' @param bin_uri BIN URI to mock
#' @return List simulating API response
mock_bin_api_response <- function(bin_uri) {
  list(
    bin_uri = bin_uri,
    member_count = sample(1:100, 1),
    species_count = sample(1:10, 1),
    institution_count = sample(1:20, 1)
  )
}
