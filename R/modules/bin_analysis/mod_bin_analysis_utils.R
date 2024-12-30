# R/modules/bin_analysis/mod_bin_analysis_utils.R

#' Validate specimen data format and content
#' @param data Data frame of specimen data
#' @return List with validation results
#' @keywords internal
validate_specimen_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No specimen data available"))
  }

  required_cols <- c("processid", "bin_uri", "species", "genus", "family", "order")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Missing required columns: %s",
                        paste(missing_cols, collapse = ", "))
    ))
  }

  if (sum(!is.na(data$bin_uri)) == 0) {
    return(list(valid = FALSE, message = "No BIN data available for analysis"))
  }

  list(valid = TRUE, message = NULL)
}

#' Create empty content data frame
#' @return Empty content data frame
#' @keywords internal
create_empty_content <- function() {
  data.frame(
    bin_uri = character(),
    total_records = integer(),
    unique_species = integer(),
    species_list = character(),
    countries = character(),
    concordance = character(),
    stringsAsFactors = FALSE
  )
}

#' Check taxonomic concordance
#' @param specimens Data frame of specimens in a BIN
#' @return Boolean indicating if specimens are concordant
#' @keywords internal
check_taxonomic_concordance <- function(specimens) {
  # Get unique valid species identifications (excluding sp./spp./cf./aff.)
  valid_species <- specimens$species[!is.na(specimens$species) &
                                       !grepl("sp\\.|spp\\.|cf\\.|aff\\.", specimens$species)]
  valid_species <- unique(valid_species[valid_species != ""])

  if (length(valid_species) > 1) {
    return(FALSE)  # Multiple valid species = discordant
  }

  # If we have a valid species, check if any cf./aff. qualifiers conflict
  if (length(valid_species) == 1) {
    reference_species <- valid_species[1]
    qualified_ids <- specimens$species[grepl("cf\\.|aff\\.", specimens$species)]

    if (any(!grepl(paste0("cf\\.|aff\\.", reference_species), qualified_ids))) {
      return(FALSE)  # Conflicting qualified IDs
    }
  }

  # If no valid species, check genus level
  genus_ids <- unique(specimens$genus[!is.na(specimens$genus) & specimens$genus != ""])
  if (length(genus_ids) > 1) {
    return(FALSE)  # Multiple genera = discordant
  }

  # If no genus, check family level
  if (length(genus_ids) == 0) {
    family_ids <- unique(specimens$family[!is.na(specimens$family) & specimens$family != ""])
    if (length(family_ids) > 1) {
      return(FALSE)  # Multiple families = discordant
    }
  }

  # If no family, check order level
  if (length(genus_ids) == 0 && length(family_ids) == 0) {
    order_ids <- unique(specimens$order[!is.na(specimens$order) & specimens$order != ""])
    if (length(order_ids) > 1) {
      return(FALSE)  # Multiple orders = discordant
    }
  }

  return(TRUE)  # No conflicts found
}

#' Process BIN Content
#' @param specimen_data Data frame of specimen data
#' @param unique_bins Vector of unique BIN URIs
#' @return Data frame of BIN content analysis
#' @keywords internal
process_bin_content <- function(specimen_data, unique_bins) {
  if (length(unique_bins) == 0) {
    return(create_empty_content())
  }

  bin_data <- lapply(unique_bins, function(bin) {
    bin_specimens <- specimen_data[specimen_data$bin_uri == bin & bin != "", , drop = FALSE]
    if (nrow(bin_specimens) == 0) return(NULL)

    # Clean species names
    bin_specimens$species <- as.character(bin_specimens$species)
    bin_specimens$species[is.na(bin_specimens$species)] <- ""

    # Get species list excluding sp./spp.
    species_list <- unique(bin_specimens$species[
      !grepl("sp\\.|spp\\.", bin_specimens$species) &
        bin_specimens$species != ""
    ])

    # Check concordance
    is_concordant <- check_taxonomic_concordance(bin_specimens)

    bin_data <- data.frame(
      bin_uri = bin,
      total_records = nrow(bin_specimens),
      unique_species = length(species_list),
      species_list = paste(sort(species_list), collapse = "; "),
      countries = paste(sort(unique(as.character(
        bin_specimens$country.ocean[!is.na(bin_specimens$country.ocean)]))),
        collapse = "; "),
      concordance = if(is_concordant) "Concordant" else "Discordant",
      bin_coverage = nrow(bin_specimens) / nrow(specimen_data), # Added for coverage metric
      concordant_bins = as.numeric(is_concordant), # Added for numerical concordance
      stringsAsFactors = FALSE
    )
  })

  bin_data <- do.call(rbind, Filter(Negate(is.null), bin_data))
  if (is.null(bin_data)) return(create_empty_content())

  bin_data
}

#' Analyze BIN Data
#' @param specimen_data Data frame of specimen data
#' @return List containing BIN analysis results
#' @keywords internal
analyze_bin_data <- function(specimen_data) {
  if (is.null(specimen_data) || nrow(specimen_data) == 0) {
    return(list(content = create_empty_content()))
  }

  if (!"bin_uri" %in% names(specimen_data)) {
    warning("bin_uri column not found in specimen data")
    return(list(content = create_empty_content()))
  }

  specimen_data$bin_uri <- as.character(specimen_data$bin_uri)
  specimen_data$bin_uri[is.na(specimen_data$bin_uri)] <- ""

  unique_bins <- unique(specimen_data$bin_uri[specimen_data$bin_uri != ""])
  bin_content <- process_bin_content(specimen_data, unique_bins)

  list(content = bin_content)
}
