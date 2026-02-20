# R/modules/species_analysis/mod_species_analysis_utils.R

#' Build species checklist from specimen data
#' @param specimen_data Data frame of specimen data
#' @param bags_grades Data frame of BAGS grades (optional)
#' @return Data frame with one row per species
#' @keywords internal
build_species_checklist <- function(specimen_data, bags_grades = NULL) {
  if (is.null(specimen_data) || nrow(specimen_data) == 0) {
    return(data.frame(
      species = character(0),
      specimen_count = integer(0),
      bin_count = integer(0),
      bin_uris = character(0),
      bags_grade = character(0),
      countries = character(0),
      mean_quality_score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  species_list <- unique(specimen_data$species[!is.na(specimen_data$species)])

  checklist <- do.call(rbind, lapply(species_list, function(sp) {
    sp_data <- specimen_data[specimen_data$species == sp & !is.na(specimen_data$species), ]

    bins <- unique(sp_data$bin_uri[!is.na(sp_data$bin_uri) & sp_data$bin_uri != ""])
    countries <- unique(sp_data$country.ocean[!is.na(sp_data$country.ocean) & sp_data$country.ocean != ""])

    grade <- ""
    if (!is.null(bags_grades) && sp %in% bags_grades$species) {
      grade <- bags_grades$bags_grade[bags_grades$species == sp][1]
    }

    data.frame(
      species = sp,
      specimen_count = nrow(sp_data),
      bin_count = length(bins),
      bin_uris = paste(bins, collapse = "; "),
      bags_grade = grade,
      countries = paste(countries, collapse = "; "),
      mean_quality_score = round(mean(sp_data$quality_score, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
  }))

  checklist[order(checklist$species), ]
}

#' Perform gap analysis comparing input taxa list against specimen data
#' @param input_taxa Either a character vector (legacy flat list) or a list
#'   of character vectors where each element is a group of synonyms:
#'   c("valid name", "synonym1", "synonym2"). The first name in each group
#'   is treated as the valid name.
#' @param specimen_data Data frame of specimen data
#' @return Data frame with gap analysis results
#' @keywords internal
perform_gap_analysis <- function(input_taxa, specimen_data) {
  if (is.null(input_taxa) || length(input_taxa) == 0) return(NULL)

  # Normalise input: convert flat character vector to list-of-vectors
  if (is.character(input_taxa)) {
    input_taxa <- as.list(input_taxa)
  }

  # Build display label for each group (comma-separated)
  group_labels <- vapply(input_taxa, function(g) paste(g, collapse = ", "), character(1))

  if (is.null(specimen_data) || nrow(specimen_data) == 0) {
    return(data.frame(
      input_taxon = group_labels,
      status = "Missing",
      matched_species = "",
      specimen_count = 0L,
      notes = "No specimen data available",
      stringsAsFactors = FALSE
    ))
  }

  results <- data.frame(
    input_taxon = group_labels,
    status = "Missing",
    matched_species = "",
    specimen_count = 0L,
    notes = "",
    stringsAsFactors = FALSE
  )

  species_lower <- tolower(specimen_data$species)

  for (i in seq_along(input_taxa)) {
    group <- input_taxa[[i]]
    if (length(group) == 0) next

    # Try each name in the group (valid name first, then synonyms)
    for (name in group) {
      name <- trimws(name)
      if (nchar(name) == 0) next

      match_idx <- which(!is.na(species_lower) & species_lower == tolower(name))
      if (length(match_idx) > 0) {
        results$status[i] <- "Found"
        results$matched_species[i] <- specimen_data$species[match_idx[1]]
        results$specimen_count[i] <- length(match_idx)
        if (name != group[1]) {
          results$notes[i] <- paste("Matched via synonym:", name)
        }
        break
      }
    }
  }

  results
}

#' Build summary statistics by family and order
#' @param specimen_data Data frame of specimen data
#' @return Data frame with summary statistics
#' @keywords internal
build_summary_stats <- function(specimen_data) {
  if (is.null(specimen_data) || nrow(specimen_data) == 0) return(NULL)

  families <- unique(specimen_data$family[!is.na(specimen_data$family)])

  stats <- do.call(rbind, lapply(families, function(fam) {
    fam_data <- specimen_data[specimen_data$family == fam & !is.na(specimen_data$family), ]
    data.frame(
      family = fam,
      order = if ("order" %in% names(fam_data)) fam_data$order[1] else NA_character_,
      species_count = length(unique(fam_data$species[!is.na(fam_data$species)])),
      specimen_count = nrow(fam_data),
      bin_count = length(unique(fam_data$bin_uri[!is.na(fam_data$bin_uri) & fam_data$bin_uri != ""])),
      mean_quality_score = round(mean(fam_data$quality_score, na.rm = TRUE), 2),
      countries = length(unique(fam_data$country.ocean[!is.na(fam_data$country.ocean)])),
      stringsAsFactors = FALSE
    )
  }))

  stats[order(-stats$species_count), ]
}
