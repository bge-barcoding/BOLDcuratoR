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
      taxon_bins <- specimens$bin_uri[specimens$species == taxon & !is.na(specimens$bin_uri)]
      bin_specimens <- specimens[specimens$bin_uri %in% taxon_bins, ]
      bin_count <- length(unique(taxon_bins))

      # Count specimens with taxonomy matching the taxon
      matching_specimens <- sum(is_concordant_taxonomy(bin_specimens, taxon))

      # Check for shared/discordant BINs
      has_shared_bins <- check_shared_bins(bin_specimens, taxon)

      data.frame(
        species = taxon,
        bags_grade = determine_bags_grade(matching_specimens, bin_count, has_shared_bins),
        specimen_count = matching_specimens,
        bin_count = bin_count,
        shared_bins = has_shared_bins,
        bin_coverage = calculate_bin_coverage(specimens[specimens$species == taxon, ]),
        stringsAsFactors = FALSE
      )
    })

    result <- do.call(rbind, grades)
    if (is.null(result)) return(create_empty_grades_df())
    result
  }, error = function(e) {
    warning(sprintf("Error calculating BAGS grades: %s", e$message))
    create_empty_grades_df()
  })
}

#' Check if BINs are shared/discordant
#' @param bin_specimens Specimens from BINs
#' @param reference_taxon Reference species name
#' @return Boolean indicating if BINs are shared
#' @keywords internal
check_shared_bins <- function(bin_specimens, reference_taxon) {
  # Get taxonomy of reference taxon
  ref_taxonomy <- get_taxonomy_hierarchy(bin_specimens[bin_specimens$species == reference_taxon,][1,])

  # For each specimen, check if taxonomy is discordant
  any(sapply(1:nrow(bin_specimens), function(i) {
    specimen <- bin_specimens[i,]

    # Always discordant if cf. or aff.
    if (!is.na(specimen$species) &&
        grepl("cf\\.|aff\\.", specimen$species)) {
      return(TRUE)
    }

    # Check taxonomy concordance
    specimen_taxonomy <- get_taxonomy_hierarchy(specimen)
    !is_concordant_hierarchy(specimen_taxonomy, ref_taxonomy)
  }))
}

#' Get taxonomy hierarchy for a specimen
#' @param specimen Single specimen record
#' @return Named list of taxonomic ranks
#' @keywords internal
get_taxonomy_hierarchy <- function(specimen) {
  list(
    order = specimen$order,
    family = specimen$family,
    genus = specimen$genus,
    species = specimen$species
  )
}

#' Check if taxonomies are concordant
#' @param test_taxonomy Taxonomy to check
#' @param ref_taxonomy Reference taxonomy
#' @return Boolean indicating concordance
#' @keywords internal
is_concordant_hierarchy <- function(test_taxonomy, ref_taxonomy) {
  # Find lowest populated rank in test taxonomy
  ranks <- c("species", "genus", "family", "order")
  test_rank <- ranks[!sapply(test_taxonomy[ranks], is.na)][1]

  if (is.na(test_rank)) return(FALSE)

  # Compare at and above that rank
  rank_index <- which(ranks == test_rank)
  ranks_to_check <- ranks[rank_index:length(ranks)]

  all(sapply(ranks_to_check, function(rank) {
    test_val <- test_taxonomy[[rank]]
    ref_val <- ref_taxonomy[[rank]]

    # Special handling for species level
    if (rank == "species" && !is.na(test_val)) {
      if (grepl("sp\\.|spp\\.", test_val)) {
        return(TRUE) # sp./spp. is concordant
      }
    }

    # NA values are concordant
    if (is.na(test_val) || is.na(ref_val)) return(TRUE)

    test_val == ref_val
  }))
}

#' Check if specimen taxonomy is concordant with reference taxon
#' @param specimens Data frame of specimens
#' @param reference_taxon Reference species name
#' @return Boolean vector of concordance for each specimen
#' @keywords internal
is_concordant_taxonomy <- function(specimens, reference_taxon) {
  # Get reference taxonomy
  ref_specimen <- specimens[specimens$species == reference_taxon,][1,]
  ref_taxonomy <- get_taxonomy_hierarchy(ref_specimen)

  # Check each specimen
  sapply(1:nrow(specimens), function(i) {
    specimen <- specimens[i,]

    # cf/aff specimens don't count toward specimen totals
    if (!is.na(specimen$species) && grepl("cf\\.|aff\\.", specimen$species)) {
      return(FALSE)
    }

    # Check taxonomy concordance
    specimen_taxonomy <- get_taxonomy_hierarchy(specimen)
    is_concordant_hierarchy(specimen_taxonomy, ref_taxonomy)
  })
}

#' Determine BAGS grade
#' @param specimen_count Number of specimens
#' @param bin_count Number of BINs
#' @param has_shared_bins Boolean indicating if BINs are shared
#' @return Character indicating BAGS grade (A-E)
#' @keywords internal
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

#' Calculate BIN coverage percentage
#' @param specimens Specimen data frame
#' @return Numeric percentage of specimens with BINs
#' @keywords internal
calculate_bin_coverage <- function(specimens) {
  if (nrow(specimens) == 0) return(0)
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
