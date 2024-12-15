# R/modules/bin_analysis/mod_bin_analysis_utils.R

#' Validate specimen data format and content
#' @param data Data frame of specimen data
#' @return List with validation results
#' @keywords internal
validate_specimen_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No specimen data available"))
  }

  required_cols <- c("processid", "bin_uri", "species", "country.ocean")
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

#' Process BIN Analysis
#' @param specimen_data Data frame of specimen data
#' @param analysis_results Reactive value for results
#' @param processing_status Reactive value for status
#' @param state State management instance
#' @param logger Logger instance
#' @keywords internal
process_bin_analysis <- function(specimen_data, analysis_results, processing_status,
                                 state, logger) {
  tryCatch({
    processing_status(list(
      is_processing = TRUE,
      message = "Starting BIN analysis...",
      error = NULL
    ))

    logger$info("Starting BIN analysis", list(
      total_records = nrow(specimen_data),
      records_with_bins = sum(!is.na(specimen_data$bin_uri))
    ))

    results <- analyze_bin_data(specimen_data)

    if (is.null(results$summary) || is.null(results$content) ||
        is.null(results$stats)) {
      stop("BIN analysis produced incomplete results")
    }

    state$update_state("bin_analysis", results, validate_bin_analysis)
    analysis_results(results)

    processing_status(list(
      is_processing = FALSE,
      message = "BIN analysis completed successfully",
      error = NULL
    ))

    logger$info("BIN analysis completed", list(
      total_bins = results$stats$total_bins,
      concordant_bins = results$stats$concordant_bins,
      discordant_bins = results$stats$discordant_bins
    ))

  }, error = function(e) {
    error_msg <- sprintf("BIN analysis error: %s", e$message)

    processing_status(list(
      is_processing = FALSE,
      message = NULL,
      error = error_msg
    ))

    logger$error(error_msg)
    analysis_results(NULL)
  })
}

#' Analyze BIN Data
#' @param specimen_data Data frame of specimen data
#' @return List containing BIN analysis results
#' @keywords internal
analyze_bin_data <- function(specimen_data) {
  if (is.null(specimen_data) || nrow(specimen_data) == 0) {
    return(create_empty_results())
  }

  if (!"bin_uri" %in% names(specimen_data)) {
    warning("bin_uri column not found in specimen data")
    return(create_empty_results())
  }

  specimen_data$bin_uri <- as.character(specimen_data$bin_uri)
  specimen_data$bin_uri[is.na(specimen_data$bin_uri)] <- ""

  unique_bins <- unique(specimen_data$bin_uri[specimen_data$bin_uri != ""])
  bin_content <- process_bin_content(specimen_data, unique_bins)
  bin_summary <- create_bin_summary(bin_content, specimen_data)
  bin_stats <- calculate_bin_statistics(bin_content, specimen_data)

  list(
    summary = bin_summary,
    content = bin_content,
    stats = bin_stats
  )
}

#' Create empty results structure
#' @return List of empty result components
#' @keywords internal
create_empty_results <- function() {
  list(
    summary = create_empty_summary(),
    content = create_empty_content(),
    stats = create_empty_stats()
  )
}

#' Create empty summary data frame
#' @return Empty summary data frame
#' @keywords internal
create_empty_summary <- function() {
  data.frame(
    total_bins = 0,
    total_records = 0,
    concordant_bins = 0,
    discordant_bins = 0,
    total_species = 0,
    total_countries = 0,
    records_without_bins = 0,
    stringsAsFactors = FALSE
  )
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

#' Create empty stats list
#' @return Empty stats list
#' @keywords internal
create_empty_stats <- function() {
  list(
    total_bins = 0,
    concordant_bins = 0,
    discordant_bins = 0,
    shared_bins = 0,
    avg_species_per_bin = 0,
    max_species_per_bin = 0,
    records_without_bins = 0,
    bin_coverage = 0
  )
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
    bin_specimens <- specimen_data[specimen_data$bin_uri == bin & bin != "", ]
    if (nrow(bin_specimens) == 0) return(NULL)

    bin_specimens$species <- as.character(bin_specimens$species)
    bin_specimens$species[is.na(bin_specimens$species)] <- "Unknown"

    data.frame(
      bin_uri = bin,
      total_records = nrow(bin_specimens),
      unique_species = length(unique(bin_specimens$species)),
      species_list = paste(sort(unique(bin_specimens$species)), collapse = "; "),
      countries = paste(sort(unique(as.character(
        bin_specimens$country.ocean[!is.na(bin_specimens$country.ocean)]))),
        collapse = "; "),
      concordance = if(length(unique(bin_specimens$species)) == 1)
        "Concordant" else "Discordant",
      stringsAsFactors = FALSE
    )
  })

  bin_data <- do.call(rbind, Filter(Negate(is.null), bin_data))
  if (is.null(bin_data)) return(create_empty_content())

  bin_data
}

#' Create BIN Summary
#' @param bin_content Data frame of BIN content analysis
#' @param specimen_data Original specimen data
#' @return Data frame of BIN summary statistics
#' @keywords internal
create_bin_summary <- function(bin_content, specimen_data) {
  records_without_bins <- sum(specimen_data$bin_uri == "" |
                                is.na(specimen_data$bin_uri))

  if (is.null(bin_content) || nrow(bin_content) == 0) {
    summary <- create_empty_summary()
    summary$total_records <- nrow(specimen_data)
    summary$records_without_bins <- records_without_bins
    return(summary)
  }

  specimen_data$species <- as.character(specimen_data$species)
  specimen_data$species[is.na(specimen_data$species)] <- "Unknown"

  data.frame(
    total_bins = nrow(bin_content),
    total_records = nrow(specimen_data),
    concordant_bins = sum(bin_content$concordance == "Concordant"),
    discordant_bins = sum(bin_content$concordance == "Discordant"),
    total_species = length(unique(specimen_data$species)),
    total_countries = length(unique(unlist(strsplit(bin_content$countries, "; ")))),
    records_without_bins = records_without_bins,
    stringsAsFactors = FALSE
  )
}

#' Calculate BIN Statistics
#' @param bin_content Data frame of BIN content analysis
#' @param specimen_data Original specimen data
#' @return List of BIN statistics
#' @keywords internal
calculate_bin_statistics <- function(bin_content, specimen_data) {
  records_without_bins <- sum(specimen_data$bin_uri == "" |
                                is.na(specimen_data$bin_uri))

  if (is.null(bin_content) || nrow(bin_content) == 0) {
    stats <- create_empty_stats()
    stats$records_without_bins <- records_without_bins
    stats$bin_coverage <- 0
    return(stats)
  }

  shared_bins <- sum(bin_content$unique_species > 1)
  species_per_bin <- bin_content$unique_species
  total_records <- nrow(specimen_data)
  records_with_bins <- total_records - records_without_bins
  bin_coverage <- if(total_records > 0)
    records_with_bins / total_records * 100 else 0

  list(
    total_bins = nrow(bin_content),
    concordant_bins = sum(bin_content$concordance == "Concordant"),
    discordant_bins = sum(bin_content$concordance == "Discordant"),
    shared_bins = shared_bins,
    avg_species_per_bin = mean(species_per_bin),
    max_species_per_bin = max(species_per_bin),
    records_without_bins = records_without_bins,
    bin_coverage = bin_coverage
  )
}
