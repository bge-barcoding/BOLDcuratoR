# R/modules/data_import/mod_data_import_utils.R

#' Validate data import input
#' @param taxa_input Character vector of taxa input
#' @param dataset_codes Character vector of dataset codes
#' @param project_codes Character vector of project codes
#' @param countries Character vector of countries
#' @return Named list with validation results
#' @keywords internal
validate_data_import_input <- function(taxa_input, dataset_codes, project_codes, countries) {
  # Initialize results list
  results <- list(
    valid = TRUE,
    messages = character(0)
  )

  # Validate that at least one type of input is provided
  if (is.null(taxa_input) && is.null(dataset_codes) &&
      is.null(project_codes) && is.null(countries)) {
    results$valid <- FALSE
    results$messages <- c(results$messages,
                          "Please provide at least one of: taxa, dataset codes, project codes, or geographic region")
    return(results)
  }

  # Taxa validation
  if (!is.null(taxa_input) && length(taxa_input) > 0) {
    taxa_input <- trimws(taxa_input[nchar(taxa_input) > 0])
    if (length(taxa_input) > 0) {
      # Check for invalid characters
      invalid_chars <- grepl("[^A-Za-z0-9[:space:]\\-_\\., ]", taxa_input)
      if (any(invalid_chars)) {
        results$valid <- FALSE
        results$messages <- c(results$messages,
                              "Taxa names contain invalid characters. Please use only letters, numbers, spaces, hyphens, underscores, and periods.")
      }

      # Check name length
      long_names <- nchar(taxa_input) > 100
      if (any(long_names)) {
        results$messages <- c(results$messages,
                              "Warning: Some taxa names are unusually long. Please verify accuracy.")
      }
    }
  }

  # Dataset code validation
  if (!is.null(dataset_codes) && length(dataset_codes) > 0) {
    dataset_codes <- trimws(dataset_codes[nchar(dataset_codes) > 0])
    invalid_codes <- !grepl("^DS-[A-Z0-9]+$", dataset_codes)
    if (any(invalid_codes)) {
      results$valid <- FALSE
      results$messages <- c(results$messages,
                            "Invalid dataset code format. Codes should be in the format DS-XXXX.")
    }
  }

  # Project code validation
  if (!is.null(project_codes) && length(project_codes) > 0) {
    project_codes <- trimws(project_codes[nchar(project_codes) > 0])
    invalid_chars <- grepl("[^A-Za-z0-9\\-_]", project_codes)
    if (any(invalid_chars)) {
      results$valid <- FALSE
      results$messages <- c(results$messages,
                            "Project codes contain invalid characters. Please use only letters, numbers, hyphens, and underscores.")
    }
  }

  # Country validation
  if (!is.null(countries) && length(countries) > 0) {
    all_countries <- unique(unlist(CONTINENT_COUNTRIES))
    unknown_countries <- setdiff(countries, all_countries)
    if (length(unknown_countries) > 0) {
      results$messages <- c(results$messages,
                            sprintf("Warning: Unrecognized countries: %s",
                                    paste(unknown_countries, collapse = ", ")))
    }
  }

  results
}

#' Prepare search parameters from input
#' @param input Shiny input object
#' @param selected_countries Vector of selected countries
#' @return List of search parameters
#' @keywords internal
prepare_search_params <- function(input, selected_countries) {
  params <- list()

  # Process taxa input.
  # Each line may contain comma-separated names: "valid name, synonym1, synonym2".
  # Split into individual search terms and also preserve the grouping for gap analysis.
  if (!is.null(input$taxa_input) && nchar(trimws(input$taxa_input)) > 0) {
    lines <- unlist(strsplit(input$taxa_input, "\n"))
    lines <- trimws(lines[nchar(trimws(lines)) > 0])
    if (length(lines) > 0) {
      # Build groups (list of character vectors) and flat search list
      taxonomy_groups <- lapply(lines, function(line) {
        names <- trimws(unlist(strsplit(line, ",")))
        names[nchar(names) > 0]
      })
      taxonomy_groups <- taxonomy_groups[lengths(taxonomy_groups) > 0]

      # Flat vector of all individual names to search
      all_names <- unique(unlist(taxonomy_groups))
      params$taxonomy <- all_names
      params$taxonomy_groups <- taxonomy_groups
    }
  }

  # Process dataset codes
  if (!is.null(input$dataset_codes) && nchar(trimws(input$dataset_codes)) > 0) {
    codes <- unlist(strsplit(input$dataset_codes, "\n"))
    codes <- trimws(codes[nchar(codes) > 0])
    if (length(codes) > 0) {
      params$dataset_codes <- codes
    }
  }

  # Process project codes
  if (!is.null(input$project_codes) && nchar(trimws(input$project_codes)) > 0) {
    codes <- unlist(strsplit(input$project_codes, "\n"))
    codes <- trimws(codes[nchar(codes) > 0])
    if (length(codes) > 0) {
      params$project_codes <- codes
    }
  }

  # Add geographic parameters
  if (!is.null(selected_countries) && length(selected_countries) > 0) {
    params$geography <- selected_countries
  }

  params
}

#' Process specimen data
#' @param specimens Data frame of raw specimen data
#' @return Processed specimen data frame
#' @keywords internal
process_specimen_data <- function(specimens) {
  # Handle empty input
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(data.frame())
  }

  if (exists("logging_manager") && !is.null(logging_manager)) {
    logging_manager$info("Columns before processing", list(
      num_columns = length(names(specimens)),
      column_names = names(specimens)
    ))
  }

  # Make a copy of the data
  processed <- as.data.frame(specimens, stringsAsFactors = FALSE)

  if (exists("logging_manager") && !is.null(logging_manager)) {
    logging_manager$info("Columns after data.frame conversion", list(
      num_columns = length(names(processed)),
      column_names = names(processed),
      missing_columns = setdiff(names(specimens), names(processed))
    ))
  }

  # Clean species names
  if ("species" %in% names(processed)) {
    processed$species <- trimws(processed$species)
    processed$species[processed$species == ""] <- NA
    processed$species[grepl("^sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.", processed$species)] <- NA
  }

  # Clean BIN URIs
  if ("bin_uri" %in% names(processed)) {
    processed$bin_uri <- trimws(processed$bin_uri)
    processed$bin_uri[processed$bin_uri == ""] <- NA
  }

  # Clean country data
  if ("country.ocean" %in% names(processed)) {
    processed$country.ocean <- trimws(processed$country.ocean)
    processed$country.ocean[processed$country.ocean == ""] <- NA
  }

  # Add source tracking
  processed$data_source <- "BOLD"
  processed$import_date <- Sys.time()

  # Remove duplicate records
  processed <- processed[!duplicated(processed$processid), ]

  # Sort by processid
  processed <- processed[order(processed$processid), ]

  if (exists("logging_manager") && !is.null(logging_manager)) {
    logging_manager$info("Columns after final processing", list(
      num_columns = length(names(processed)),
      column_names = names(processed),
      missing_columns = setdiff(names(specimens), names(processed))
    ))
  }

  processed
}

#' Clean geographic input
#' @param countries Vector of country names
#' @return Cleaned and validated country names
#' @keywords internal
clean_geographic_input <- function(countries) {
  if (is.null(countries) || length(countries) == 0) {
    return(NULL)
  }

  # Clean country names
  countries <- trimws(countries)
  countries <- countries[countries != ""]

  # Remove duplicates
  unique(countries)
}

#' Format error message
#' @param message Error message text
#' @param type Error type (error, warning, info)
#' @return Formatted error message
#' @keywords internal
format_error_message <- function(message, type = "error") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sprintf("[%s] %s: %s", timestamp, toupper(type), message)
}

#' Calculate search query length
#' @param params List of search parameters
#' @return Total query length
#' @keywords internal
calculate_query_length <- function(params) {
  total_length <- 0

  if (!is.null(params$taxonomy)) {
    total_length <- total_length + sum(nchar(params$taxonomy))
  }
  if (!is.null(params$dataset_codes)) {
    total_length <- total_length + sum(nchar(params$dataset_codes))
  }
  if (!is.null(params$project_codes)) {
    total_length <- total_length + sum(nchar(params$project_codes))
  }
  if (!is.null(params$geography)) {
    total_length <- total_length + sum(nchar(params$geography))
  }

  total_length
}

#' Batch process specimens
#' @param specimens Data frame of specimens
#' @param batch_size Size of batches
#' @return List of specimen batches
#' @keywords internal
batch_process_specimens <- function(specimens, batch_size = 1000) {
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(list())
  }

  # Calculate number of batches
  n_batches <- ceiling(nrow(specimens) / batch_size)

  # Split into batches
  split(specimens, cut(seq_len(nrow(specimens)), n_batches, labels = FALSE))
}

#' Merge search results
#' @param existing Existing results data frame
#' @param new_results New results data frame
#' @return Combined results data frame
#' @keywords internal
merge_search_results <- function(existing, new_results) {
  if (is.null(existing)) return(new_results)
  if (is.null(new_results)) return(existing)

  # Remove duplicates based on processid
  combined <- rbind(
    existing,
    new_results[!new_results$processid %in% existing$processid, ]
  )

  # Sort by processid
  combined[order(combined$processid), ]
}

#' Validate search results
#' @param results Search results data frame
#' @return List with validation results
#' @keywords internal
validate_search_results <- function(results) {
  if (is.null(results) || nrow(results) == 0) {
    return(list(valid = FALSE, message = "No results found"))
  }

  # Check required columns
  required_cols <- c("processid", "species", "bin_uri")
  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Missing required columns: %s",
                        paste(missing_cols, collapse = ", "))
    ))
  }

  # Check for empty required fields
  empty_processids <- sum(is.na(results$processid) | results$processid == "")
  if (empty_processids > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("%d records missing processid", empty_processids)
    ))
  }

  list(valid = TRUE, message = NULL)
}

#' Format search parameters for display
#' @param params Search parameters list
#' @return Formatted string
#' @keywords internal
format_search_params <- function(params) {
  parts <- character()

  if (!is.null(params$taxonomy)) {
    parts <- c(parts, sprintf("Taxa: %d",
                              length(params$taxonomy)))
  }
  if (!is.null(params$dataset_codes)) {
    parts <- c(parts, sprintf("Datasets: %d",
                              length(params$dataset_codes)))
  }
  if (!is.null(params$project_codes)) {
    parts <- c(parts, sprintf("Projects: %d",
                              length(params$project_codes)))
  }
  if (!is.null(params$geography)) {
    parts <- c(parts, sprintf("Countries: %d",
                              length(params$geography)))
  }

  paste(parts, collapse = " | ")
}

#' Track search metrics
#' @param results Search results data frame
#' @return List of metrics
#' @keywords internal
calculate_search_metrics <- function(results) {
  if (is.null(results) || nrow(results) == 0) {
    return(list(
      total_records = 0,
      unique_species = 0,
      unique_bins = 0,
      countries = 0
    ))
  }

  list(
    total_records = nrow(results),
    unique_species = length(unique(results$species[!is.na(results$species)])),
    unique_bins = length(unique(results$bin_uri[!is.na(results$bin_uri)])),
    countries = length(unique(results$country.ocean[!is.na(results$country.ocean)]))
  )
}
