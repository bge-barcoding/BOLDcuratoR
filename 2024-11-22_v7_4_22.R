# Package installation and loading
if (!require("remotes")) install.packages("remotes")
if (!require("devtools")) install.packages("devtools")
if (!require("BOLDconnectR")) {
  devtools::install_github("boldsystems-central/BOLDconnectR")
  library(BOLDconnectR)
}

# Check and install required packages
required_packages <- c(
  "shiny",
  "writexl",
  "DT",
  "shinydashboard",
  "shinyjs",
  "dplyr",
  "shinycssloaders",
  "tidyr",
  "logger",
  "R6",
  "markdown",
  "purrr",
  "ape",
  "utils"

)

# Install and load missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load primary packages
library(shiny)
library(BOLDconnectR)
library(writexl)
library(DT)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(shinycssloaders)
library(tidyr)
library(logger)
library(purrr)
library(ape)

# Initialize logging
if (!dir.exists("logs")) {
  dir.create("logs")
}

# Enhanced logging setup
setup_logging <- function() {
  log_file <- file.path("logs", format(Sys.time(), "%Y%m%d_app.log"))
  log_appender(appender_file(log_file))
}

# Initialize the logger
setup_logging()

# Enhanced logging function with proper reactive context handling
log_message <- function(msg, session = NULL, rv = NULL, type = "info") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- paste(timestamp, "-", type, ":", msg)

  # Always log to file and console
  message(full_msg)
  log_appender(appender_file("logs/app_logs.txt"))

  # Update reactive values if in reactive context
  if (!is.null(rv) && !is.null(session)) {
    isolate({
      rv$log_messages <- c(rv$log_messages, full_msg)
      if (type == "error") {
        rv$last_error <- msg
      }
    })
  }

  return(full_msg)
}

# Enhanced batch progress tracking
update_batch_progress <- function(msg, session = NULL, rv = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- paste(timestamp, "-", msg)

  if (!is.null(rv) && !is.null(session)) {
    isolate({
      rv$batch_progress <- c(rv$batch_progress, full_msg)
    })
  }

  return(full_msg)
}

# Enhanced progress update function
update_progress <- function(session, rv, type, value = NULL, detail = NULL) {
  isolate({
    switch(type,
           "start_batch" = {
             rv$progress$current_batch <- rv$progress$current_batch + 1
             rv$progress$stage_message <- sprintf("Processing batch %d of %d",
                                                  rv$progress$current_batch, rv$progress$total_batches)
           },
           "start_taxon" = {
             rv$progress$current_taxon <- rv$progress$current_taxon + 1
             rv$progress$current_operation <- sprintf("Processing taxon %d of %d",
                                                      rv$progress$current_taxon, rv$progress$total_taxa)
           },
           "operation" = {
             rv$progress$current_operation <- value
             if (!is.null(detail)) {
               rv$progress$operation_details <- detail
             }
           },
           "sub_progress" = {
             rv$progress$sub_progress <- value
           },
           "reset" = {
             rv$progress$current_batch <- 0
             rv$progress$current_taxon <- 0
             rv$progress$current_operation <- ""
             rv$progress$operation_details <- ""
             rv$progress$sub_progress <- 0
             rv$progress$stage_message <- ""
             rv$processing_stats$start_time <- Sys.time()
             rv$processing_stats$end_time <- NULL
             rv$processing_stats$total_requests <- 0
             rv$processing_stats$successful_requests <- 0
             rv$processing_stats$failed_requests <- 0
             rv$processing_stats$cached_requests <- 0
           }
    )
  })
}

# Rate limiter for BOLD API
rate_limiter <- (function() {
  last_bold_request <- Sys.time()
  request_counts <- list(
    total = 0,
    successful = 0,
    failed = 0,
    cached = 0
  )
  min_interval_bold <- 0.06  # 1000 requests per minute = 0.06 seconds between requests

  list(
    wait_for_bold = function(session = NULL, rv = NULL) {
      current_time <- Sys.time()
      time_diff <- difftime(current_time, last_bold_request, units = "secs")
      if (time_diff < min_interval_bold) {
        Sys.sleep(min_interval_bold - as.numeric(time_diff))
      }
      last_bold_request <<- Sys.time()
      request_counts$total <<- request_counts$total + 1

      if (!is.null(rv) && !is.null(session)) {
        isolate({
          rv$processing_stats$total_requests <- request_counts$total
          log_message(sprintf("BOLD request %d", request_counts$total), session, rv)
        })
      }
      return(request_counts$total)
    },

    record_success = function(session = NULL, rv = NULL) {
      request_counts$successful <<- request_counts$successful + 1
      if (!is.null(rv)) {
        isolate({
          rv$processing_stats$successful_requests <- request_counts$successful
        })
      }
    },

    record_failure = function(session = NULL, rv = NULL) {
      request_counts$failed <<- request_counts$failed + 1
      if (!is.null(rv)) {
        isolate({
          rv$processing_stats$failed_requests <- request_counts$failed
        })
      }
    },

    record_cache = function(session = NULL, rv = NULL) {
      request_counts$cached <<- request_counts$cached + 1
      if (!is.null(rv)) {
        isolate({
          rv$processing_stats$cached_requests <- request_counts$cached
        })
      }
    },

    get_counts = function() {
      request_counts
    },

    reset_counts = function() {
      request_counts$total <<- 0
      request_counts$successful <<- 0
      request_counts$failed <<- 0
      request_counts$cached <<- 0
    }
  )
})()

# Initialize BOLD API key
if (!exists("apikey")) {
  apikey <- NULL
}

# Basic utility functions
create_batches <- function(items, batch_size = 10) {
  split(items, ceiling(seq_along(items)/batch_size))
}

process_batch <- function(batch, FUN, ...) {
  map(batch, FUN, ...)
}


# Define continent country lists
continent_countries <- list(
  Europe = c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
             "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic",
             "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
             "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia",
             "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova",
             "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway",
             "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia",
             "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine",
             "United Kingdom", "Vatican City"),

  Asia = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh",
           "Bhutan", "Brunei", "Cambodia", "China", "Cyprus", "Georgia",
           "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan",
           "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia",
           "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman",
           "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi Arabia",
           "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan",
           "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan",
           "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"),

  Africa = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
             "Burundi", "Cameroon", "Cape Verde", "Central African Republic",
             "Chad", "Comoros", "Congo", "Democratic Republic of the Congo",
             "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia",
             "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
             "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya",
             "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius",
             "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
             "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles",
             "Sierra Leone", "Somalia", "South Africa", "South Sudan",
             "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia",
             "Uganda", "Zambia", "Zimbabwe"),

  "North America" = c("Canada", "United States", "Mexico", "Greenland",
                      "Bermuda", "Saint Pierre and Miquelon"),

  "Central America" = c("Belize", "Costa Rica", "El Salvador", "Guatemala",
                        "Honduras", "Nicaragua", "Panama"),

  "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                      "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                      "Uruguay", "Venezuela", "French Guiana"),

  Oceania = c("Australia", "Fiji", "Kiribati", "Marshall Islands",
              "Micronesia", "Nauru", "New Zealand", "Palau",
              "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga",
              "Tuvalu", "Vanuatu")
)

# Enhanced BOLD summary function with specimen handling
get_bold_summary <- function(taxon, dataset_codes = NULL, project_codes = NULL, countries = NULL, session = NULL, rv = NULL) {
  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("Getting BOLD summary for taxa batch"), session, rv)
    if (!is.null(countries)) {
      log_message(sprintf("Searching in %d countries", length(countries)), session, rv)
      log_message(sprintf("Countries: %s", paste(countries, collapse=", ")), session, rv)
    }
  }

  tryCatch({
    combined_data <- NULL

    # If we have many countries, we need to batch the searches
    if (!is.null(countries) && length(countries) > 5) {
      # Split countries into batches of 5
      country_batches <- split(countries, ceiling(seq_along(countries)/5))

      for (i in seq_along(country_batches)) {
        batch <- country_batches[[i]]
        rate_limiter$wait_for_bold()

        # Handle multiple taxa in a single search for each country batch
        log_message(sprintf("Batch %d/%d - Making API call with:", i, length(country_batches)), session, rv)
        log_message(sprintf("taxonomy: %s", paste(taxon, collapse=", ")), session, rv)
        log_message(sprintf("geography: %s", paste(batch, collapse=", ")), session, rv)

        # Make the call with error handling for each batch
        batch_results <- tryCatch({
          results <- bold.public.search(taxonomy = taxon, geography = batch)
          if (!is.null(results) && nrow(results) > 0) {
            log_message(sprintf("Batch %d returned %d records", i, nrow(results)), session, rv)
            results
          } else {
            log_message(sprintf("Batch %d returned no results", i), session, rv)
            NULL
          }
        }, error = function(e) {
          log_message(sprintf("Batch %d failed: %s", i, e$message), session, rv, "warning")
          NULL
        })

        # Add results to combined_data if we got any
        if (!is.null(batch_results) && nrow(batch_results) > 0) {
          if (is.null(combined_data)) {
            combined_data <- batch_results
          } else {
            combined_data <- rbind(combined_data, batch_results[!batch_results$processid %in% combined_data$processid,])
          }
        }
      }
    } else {
      # For small numbers of countries or no countries
      rate_limiter$wait_for_bold()

      # Log the exact call we're about to make
      log_message("Making single API call with:", session, rv)
      log_message(sprintf("taxonomy: %s", paste(taxon, collapse=", ")), session, rv)
      log_message(sprintf("geography: %s",
                          if(!is.null(countries)) paste(countries, collapse=", ") else "NULL"),
                  session, rv)

      search_results <- bold.public.search(taxonomy = taxon, geography = countries)

      # Log the results
      if (!is.null(search_results) && nrow(search_results) > 0) {
        log_message(sprintf("Search returned %d records", nrow(search_results)), session, rv)
        combined_data <- search_results
      } else {
        log_message("Search returned no results", session, rv)
      }
    }

    # Only proceed with fetching if we have any results
    if (!is.null(combined_data) && nrow(combined_data) > 0) {
      rate_limiter$wait_for_bold()
      log_message(sprintf("Fetching detailed data for %d records", nrow(combined_data)), session, rv)
      taxon_data <- bold.fetch(
        get_by = "processid",
        identifiers = combined_data$processid
      )
      combined_data <- taxon_data
    }

    # Get dataset specimens if provided
    if (!is.null(dataset_codes) && length(dataset_codes) > 0) {
      for (dataset in dataset_codes) {
        rate_limiter$wait_for_bold()
        dataset_specimens <- bold.fetch(
          get_by = "dataset_codes",
          identifiers = dataset
        )

        if (!is.null(dataset_specimens) && nrow(dataset_specimens) > 0) {
          if (is.null(combined_data)) {
            combined_data <- dataset_specimens
          } else {
            combined_data <- rbind(combined_data, dataset_specimens)
          }
        }
      }
    }

    # Get project specimens if provided
    if (!is.null(project_codes) && length(project_codes) > 0) {
      for (project in project_codes) {
        rate_limiter$wait_for_bold()
        project_specimens <- bold.fetch(
          get_by = "project_codes",
          identifiers = project
        )

        if (!is.null(project_specimens) && nrow(project_specimens) > 0) {
          if (is.null(combined_data)) {
            combined_data <- project_specimens
          } else {
            combined_data <- rbind(combined_data, project_specimens)
          }
        }
      }
    }

    # Remove duplicates based on processid
    if (!is.null(combined_data)) {
      combined_data <- combined_data[!duplicated(combined_data$processid), ]

      summary <- bold.data.summarize(
        bold_df = combined_data,
        summarize_by = "all_data"
      )

      return(list(
        summary = summary,
        data = combined_data,
        specimens = combined_data
      ))
    }

    return(NULL)

  }, error = function(e) {
    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("BOLD summary failed for %s: %s", paste(taxon, collapse=", "), e$message),
                  session, rv, "error")
    }
    return(NULL)
  })
}

# Complete analyze_bin_content function
analyze_bin_content <- function(taxa, existing_data = NULL, valid_name = NULL, session = NULL, rv = NULL) {
  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("Analyzing BIN content for %s", paste(taxa, collapse=", ")), session, rv)
  }

  tryCatch({
    # Get specimen data if not provided
    specimen_data <- if (!is.null(existing_data)) {
      existing_data
    } else {
      # Do batch search for all taxa at once
      search_results <- bold.public.search(taxonomy = taxa)
      if (is.null(search_results) || nrow(search_results) == 0) {
        return(NULL)
      }
      bold.fetch(
        get_by = "processid",
        identifiers = search_results$processid
      )
    }

    if (is.null(specimen_data) || nrow(specimen_data) == 0) {
      return(NULL)
    }

    # Add name columns
    specimen_data$Valid_Name <- if (!is.null(valid_name)) valid_name else taxa[1]
    specimen_data$Search_Name <- taxa[1]

    # Get unique BINs
    bins_to_check <- unique(specimen_data$bin_uri[!is.na(specimen_data$bin_uri) & specimen_data$bin_uri != ""])
    if (length(bins_to_check) == 0) {
      return(list(
        bin_summary = data.frame(
          Total_BINs = 0,
          Total_Records = nrow(specimen_data),
          Concordant_BINs = 0,
          Discordant_BINs = 0,
          Total_Species = 1,
          Total_Countries = length(unique(specimen_data$country.ocean[!is.na(specimen_data$country.ocean)])),
          stringsAsFactors = FALSE
        ),
        bin_content = data.frame(),
        specimen_data = specimen_data
      ))
    }

    # Get ALL specimens for each BIN using BOLDconnectR in batches
    all_bin_data <- list()

    # Create batches of max 50 BINs per batch
    bin_batches <- split(bins_to_check, ceiling(seq_along(bins_to_check)/50))

    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("Processing %d unique BINs in %d batches",
                          length(bins_to_check),
                          length(bin_batches)),
                  session, rv)
    }

    # Process each batch of BINs
    for (i in seq_along(bin_batches)) {
      bin_batch <- bin_batches[[i]]

      if (!is.null(session) && !is.null(rv)) {
        log_message(sprintf("Fetching specimens for BIN batch %d of %d (containing %d BINs)",
                            i, length(bin_batches), length(bin_batch)),
                    session, rv)
      }

      rate_limiter$wait_for_bold()
      # Fetch ALL specimens in this batch of BINs as a single query
      bin_specimens <- bold.fetch(
        get_by = "bin_uris",
        identifiers = paste(bin_batch, collapse = ",")
      )

      if (!is.null(bin_specimens) && nrow(bin_specimens) > 0) {
        # Add reference to original taxon
        bin_specimens$Original_Taxon <- paste(taxa, collapse=", ")
        bin_specimens$Valid_Name <- valid_name
        all_bin_data[[i]] <- bin_specimens
      }
    }

    # Combine all BIN data
    all_specimens <- do.call(rbind, all_bin_data)

    if (is.null(all_specimens) || nrow(all_specimens) == 0) {
      if (!is.null(session) && !is.null(rv)) {
        log_message(sprintf("No specimen data found for BINs of %s", paste(taxa, collapse=", ")), session, rv)
      }
      return(list(
        bin_summary = data.frame(
          Total_BINs = length(bins_to_check),
          Total_Records = nrow(specimen_data),
          Concordant_BINs = 0,
          Discordant_BINs = 0,
          Total_Species = 1,
          Total_Countries = length(unique(specimen_data$country.ocean[!is.na(specimen_data$country.ocean)])),
          stringsAsFactors = FALSE
        ),
        bin_content = data.frame(),
        specimen_data = specimen_data
      ))
    }

    # Create summarization with all species in BINs
    bin_content <- all_specimens %>%
      group_by(bin_uri) %>%
      summarize(
        Total_Records = n(),
        Unique_Species = n_distinct(species[!is.na(species) & species != ""]),
        Species_List = paste(sort(unique(species[!is.na(species) & species != ""])), collapse = "; "),
        Countries = if("country.ocean" %in% colnames(.)) {
          paste(sort(unique(country.ocean[!is.na(country.ocean)])), collapse = "; ")
        } else {
          "No country data"
        },
        Concordance = if(n_distinct(species[!is.na(species) & species != ""]) == 1) {
          "Concordant"
        } else {
          "Discordant"
        },
        .groups = 'drop'
      )

    # Calculate overall summary
    bin_summary <- data.frame(
      Total_BINs = nrow(bin_content),
      Total_Records = sum(bin_content$Total_Records),
      Concordant_BINs = sum(bin_content$Concordance == "Concordant"),
      Discordant_BINs = sum(bin_content$Concordance == "Discordant"),
      Total_Species = n_distinct(all_specimens$species[!is.na(all_specimens$species) & all_specimens$species != ""]),
      Total_Countries = if(all(bin_content$Countries != "No country data")) {
        length(unique(unlist(strsplit(bin_content$Countries, "; "))))
      } else {
        0
      },
      stringsAsFactors = FALSE
    )

    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("Successfully analyzed BIN content for %s: found %d BINs with %d unique species total",
                          paste(taxa, collapse=", "), bin_summary$Total_BINs, bin_summary$Total_Species), session, rv)
    }

    return(list(
      bin_summary = bin_summary,
      bin_content = as.data.frame(bin_content),
      specimen_data = all_specimens
    ))

  }, error = function(e) {
    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("BIN analysis failed for %s: %s", paste(taxa, collapse=", "), e$message),
                  session, rv, "error")
    }
    return(NULL)
  })
}

# Add this function before process_taxa_batch
analyze_bin_content_for_species <- function(species_specimens, all_specimens, search_taxon, species, session = NULL, rv = NULL) {
  tryCatch({
    # Get all BINs for this species
    species_bins <- unique(species_specimens$bin_uri[!is.na(species_specimens$bin_uri) & species_specimens$bin_uri != ""])

    if (length(species_bins) == 0) {
      return(NULL)
    }

    # Get all specimens from these BINs
    bin_specimens <- all_specimens[all_specimens$bin_uri %in% species_bins, ]

    if (nrow(bin_specimens) == 0) {
      return(NULL)
    }

    # Add reference information
    bin_specimens$Valid_Name <- search_taxon
    bin_specimens$Search_Name <- species
    bin_specimens$Name_Status <- "species_in_higher_taxon"

    # Process BIN content
    bin_content <- process_bin_content(bin_specimens)

    if (nrow(bin_content) == 0) {
      return(NULL)
    }

    # Create summary
    bin_summary <- create_bin_summary(bin_content, bin_specimens)

    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("Successfully analyzed BINs for species %s in %s: found %d BINs",
                          species, search_taxon, bin_summary$Total_BINs),
                  session, rv)
    }

    return(list(
      bin_summary = bin_summary,
      bin_content = bin_content,
      specimen_data = bin_specimens
    ))
  }, error = function(e) {
    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("BIN analysis failed for species %s in %s: %s",
                          species, search_taxon, e$message),
                  session, rv, "error")
    }
    return(NULL)
  })
}

process_higher_taxa <- function(specimens, search_taxon, taxonomic_level, session = NULL, rv = NULL) {
  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("Processing higher taxon %s at level %s", search_taxon, taxonomic_level), session, rv)
  }

  # Validate inputs
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(list(
      summary = data.frame(),
      bin_analysis = list(),
      specimens = data.frame()
    ))
  }

  # Create summary for each unique species found
  unique_species <- unique(specimens$species[!is.na(specimens$species) & specimens$species != ""])

  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("Found %d unique species", length(unique_species)), session, rv)
  }

  # Initialize storage for summaries and BIN analyses
  summaries <- list()
  bin_analyses <- list()

  # Process each unique species
  for (species in unique_species) {
    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("Processing species: %s", species), session, rv)
    }

    # Get specimens for this species
    species_specimens <- specimens[specimens$species == species, ]

    # Get BINs for this species
    species_bins <- length(unique(species_specimens$bin_uri[!is.na(species_specimens$bin_uri)]))

    # Create summary row
    summary_row <- data.frame(
      Valid_Name = search_taxon,  # The higher taxon that was searched
      Search_Name = search_taxon, # The higher taxon that was searched
      Name_Status = taxonomic_level,  # e.g., "genus" or "family"
      Found_Species = species,    # The actual species found
      Total_Specimens = nrow(species_specimens),
      Total_BINs = species_bins,
      Species_Found = species,
      Low_Specimen_Count = if(nrow(species_specimens) < 3) "Warning" else "OK",
      Countries = paste(sort(unique(species_specimens$country.ocean[!is.na(species_specimens$country.ocean)])),
                        collapse = "; "),
      Status = "Success",
      stringsAsFactors = FALSE
    )

    summaries[[species]] <- summary_row

    # Process BIN analysis for this species
    bin_result <- analyze_bin_content_for_species(
      species_specimens,
      specimens,  # Pass all specimens for BIN lookup
      search_taxon,
      species,
      session,
      rv
    )

    if (!is.null(bin_result)) {
      bin_analyses[[paste(search_taxon, species, sep="_")]] <- bin_result
    }
  }

  # Combine all summaries
  combined_summary <- do.call(rbind, summaries)

  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("Completed processing higher taxon %s with %d species",
                        search_taxon, nrow(combined_summary)), session, rv)
  }

  return(list(
    summary = combined_summary,
    bin_analysis = bin_analyses,
    specimens = specimens
  ))
}

# Process taxa batch with focus on taxonomy and BIN analysis
process_taxa_batch <- function(taxa_batch, dataset_codes = NULL, project_codes = NULL, countries = NULL, session = NULL, rv = NULL) {
  # Initialize storage
  bold_results <- list()
  specimen_data <- list()
  bin_specimens_by_bin <- list()
  initial_specimens_by_taxon <- list()
  current_row <- 1

  # Parse taxa_batch into groups (only if taxa provided)
  taxa_groups <- if (!is.null(taxa_batch) && length(taxa_batch) > 0 && any(nchar(taxa_batch) > 0)) {
    lapply(taxa_batch, function(line) {
      taxa <- trimws(unlist(strsplit(line, ",")))
      taxa[taxa != ""]
    })
  } else {
    NULL
  }

  # Get all unique taxa for searching (if any provided)
  all_taxa_to_search <- if (!is.null(taxa_groups)) {
    unique(unlist(taxa_groups))
  } else {
    NULL
  }

  if (!is.null(all_taxa_to_search)) {
    log_message(sprintf("\nAll unique taxa to search: %s", paste(all_taxa_to_search, collapse=", ")), session, rv)
  }

  # Make the API call with vectors
  tryCatch({
    if (!is.null(dataset_codes) && length(dataset_codes) > 0) {
      # If we have dataset codes, search by those directly
      rate_limiter$wait_for_bold()
      combined_specimens <- bold.fetch(
        get_by = "dataset_codes",
        identifiers = dataset_codes
      )
    } else if (!is.null(project_codes) && length(project_codes) > 0) {
      # If we have project codes, search by those directly
      rate_limiter$wait_for_bold()
      combined_specimens <- bold.fetch(
        get_by = "project_codes",
        identifiers = project_codes
      )
    } else {
      # Otherwise do the regular taxonomy search
      search_results <- bold.public.search(
        taxonomy = all_taxa_to_search,
        geography = countries
      )

      if (!is.null(search_results) && nrow(search_results) > 0) {
        rate_limiter$wait_for_bold()
        combined_specimens <- bold.fetch(
          get_by = "processid",
          identifiers = search_results$processid
        )
      }
    }

    # If we got specimens from any source, process them
    if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
      # If no taxa were provided, create a summary based on what we found
      if (is.null(taxa_groups)) {
        # Get unique species from the results
        unique_species <- unique(combined_specimens$species[!is.na(combined_specimens$species) &
                                                              combined_specimens$species != ""])

        # Mark specimens as direct matches
        combined_specimens$Query_Source <- "Direct"

        # Create summary for each species found
        for (species in unique_species) {
          species_specimens <- combined_specimens[combined_specimens$species == species &
                                                    combined_specimens$Query_Source == "Direct", ]

          species_bins <- length(unique(species_specimens$bin_uri[!is.na(species_specimens$bin_uri)]))

          bold_results[[current_row]] <- data.frame(
            Valid_Name = species,  # Use species as both valid and search name
            Search_Name = species,
            Name_Status = "Found in dataset/project",
            Total_Specimens = nrow(species_specimens),
            Total_BINs = species_bins,
            Species_Found = species,
            Low_Specimen_Count = if(nrow(species_specimens) < 3) "Warning" else "OK",
            Countries = paste(sort(unique(species_specimens$country.ocean[!is.na(species_specimens$country.ocean)])),
                              collapse = "; "),
            Status = "Success",
            stringsAsFactors = FALSE
          )

          current_row <- current_row + 1
        }

        # Apply ranking after all specimen data is processed
        log_message("Applying quality ranking to specimens...", session, rv)
        combined_specimens <- apply_specimen_ranking(combined_specimens)

        # Log ranking results
        grade_summary <- table(combined_specimens$quality_grade)
        log_message(sprintf("Specimen quality grades: A=%d, B=%d, C=%d, D=%d, F=%d",
                            grade_summary["A"], grade_summary["B"],
                            grade_summary["C"], grade_summary["D"],
                            grade_summary["F"]),
                    session, rv)

        return(list(
          bold = do.call(rbind, bold_results),
          bins = list(),  # No BIN analysis for dataset/project only searches
          specimens = combined_specimens
        ))
      }

      # If we get here, we're doing a regular taxon search
      # Check for higher taxa search
      if (length(taxa_groups) == 1 && length(taxa_groups[[1]]) == 1) {
        search_taxon <- taxa_groups[[1]][1]

        # Try to determine taxonomic level
        taxonomic_level <- if (!is.null(combined_specimens$genus) &&
                               all(combined_specimens$genus == search_taxon, na.rm = TRUE)) {
          "genus"
        } else if (!is.null(combined_specimens$family) &&
                   all(combined_specimens$family == search_taxon, na.rm = TRUE)) {
          "family"
        } else if (!is.null(combined_specimens$order) &&
                   all(combined_specimens$order == search_taxon, na.rm = TRUE)) {
          "order"
        } else {
          "higher_taxon"
        }

        # Process as higher taxon if we have multiple species
        unique_species <- unique(combined_specimens$species[!is.na(combined_specimens$species) &
                                                              combined_specimens$species != ""])
        if (length(unique_species) > 1) {
          log_message(sprintf("Processing as higher taxon (%s) with %d species",
                              taxonomic_level, length(unique_species)), session, rv)

          results <- process_higher_taxa(combined_specimens, search_taxon,
                                         taxonomic_level, session, rv)

          # Apply ranking to specimens before returning
          if (!is.null(results$specimens) && nrow(results$specimens) > 0) {
            log_message("Applying quality ranking to specimens...", session, rv)
            results$specimens <- apply_specimen_ranking(results$specimens)

            # Log ranking results
            grade_summary <- table(results$specimens$quality_grade)
            log_message(sprintf("Specimen quality grades: A=%d, B=%d, C=%d, D=%d, F=%d",
                                grade_summary["A"], grade_summary["B"],
                                grade_summary["C"], grade_summary["D"],
                                grade_summary["F"]),
                        session, rv)
          }

          return(list(
            bold = results$summary,
            bins = results$bin_analysis,
            specimens = results$specimens
          ))
        }
      }

      # If we get here, we're doing a regular species-level search
      # Get all unique BINs
      all_bins <- unique(combined_specimens$bin_uri[!is.na(combined_specimens$bin_uri) &
                                                      combined_specimens$bin_uri != ""])

      if (length(all_bins) > 0) {
        log_message(sprintf("Processing %d unique BINs", length(all_bins)), session, rv)

        # Fetch ALL specimens for BINs
        rate_limiter$wait_for_bold()
        bin_specimens <- bold.fetch(
          get_by = "bin_uris",
          identifiers = all_bins
        )

        if (!is.null(bin_specimens) && nrow(bin_specimens) > 0) {
          # Mark specimens by source
          combined_specimens$Query_Source <- "Direct"
          bin_specimens$Query_Source <- "BIN"

          # Combine unique specimens
          unique_specimens <- bin_specimens[!bin_specimens$processid %in% combined_specimens$processid,]
          all_specimens_combined <- rbind(combined_specimens, unique_specimens)

          # Apply ranking after all specimens are combined
          log_message("Applying quality ranking to specimens...", session, rv)
          all_specimens_combined <- apply_specimen_ranking(all_specimens_combined)

          # Log ranking results
          grade_summary <- table(all_specimens_combined$quality_grade)
          log_message(sprintf("Specimen quality grades: A=%d, B=%d, C=%d, D=%d, F=%d",
                              grade_summary["A"], grade_summary["B"],
                              grade_summary["C"], grade_summary["D"],
                              grade_summary["F"]),
                      session, rv)

          # Process BIN analyses for each taxa group
          bin_analyses <- list()

          for (taxa_group in taxa_groups) {
            valid_name <- taxa_group[1]
            all_names <- taxa_group

            for (search_name in all_names) {
              # Get initial specimens (exact matches only)
              initial_data <- combined_specimens[combined_specimens$species == search_name &
                                                   combined_specimens$Query_Source == "Direct", ]

              # Create the summary row based on exact matches only
              bold_results[[current_row]] <- create_summary_row(valid_name, search_name, initial_data)

              # Process BIN analysis if we have any exact matches
              if (!is.null(initial_data) && nrow(initial_data) > 0) {
                # Get ALL specimens from the BINs found in exact matches
                matched_bins <- unique(initial_data$bin_uri[!is.na(initial_data$bin_uri) &
                                                              initial_data$bin_uri != ""])
                bin_specimens <- all_specimens_combined[all_specimens_combined$bin_uri %in% matched_bins, ]

                bin_result <- analyze_bin_content_with_data(
                  initial_data,
                  bin_specimens,
                  valid_name,
                  search_name,
                  session,
                  rv
                )

                if (!is.null(bin_result)) {
                  bin_analyses[[paste(valid_name, search_name, sep="_")]] <- bin_result
                  bold_results[[current_row]]$Total_BINs <- bin_result$bin_summary$Total_BINs
                }
              }

              current_row <- current_row + 1
            }
          }

          return(list(
            bold = do.call(rbind, bold_results),
            bins = bin_analyses,
            specimens = all_specimens_combined
          ))
        }
      }
    }
  }, error = function(e) {
    log_message(sprintf("Error in API call: %s", e$message), session, rv, "error")
    return(NULL)
  })

  # If we get here, return empty results
  return(list(
    bold = data.frame(),
    bins = list(),
    specimens = data.frame()
  ))
}

# create summary from specimens
create_summary_from_specimens <- function(specimens, taxa_groups) {
  # Create summary data frame
  summaries <- lapply(taxa_groups, function(group) {
    valid_name <- group[1]
    lapply(group, function(search_name) {
      matching_specimens <- specimens[specimens$species == search_name, ]
      data.frame(
        Valid_Name = valid_name,
        Search_Name = search_name,
        Name_Status = if(search_name == valid_name) "Valid" else "Synonym",
        Total_Specimens = nrow(matching_specimens),
        Total_BINs = length(unique(matching_specimens$bin_uri[!is.na(matching_specimens$bin_uri)])),
        Species_Found = paste(sort(unique(matching_specimens$species)), collapse = "; "),
        Low_Specimen_Count = if(nrow(matching_specimens) == 0) "None"
        else if(nrow(matching_specimens) < 3) "Warning"
        else "OK",
        Countries = paste(sort(unique(matching_specimens$country.ocean[!is.na(matching_specimens$country.ocean)])), collapse = "; "),
        Status = if(nrow(matching_specimens) > 0) "Success" else "No records found in BOLD",
        stringsAsFactors = FALSE
      )
    })
  })
  do.call(rbind, unlist(summaries, recursive = FALSE))
}

analyze_bins_for_specimens <- function(specimens, taxa_groups) {
  bin_analyses <- list()

  for (group in taxa_groups) {
    valid_name <- group[1]
    for (search_name in group) {
      key <- paste(valid_name, search_name, sep="_")
      matching_specimens <- specimens[specimens$species == search_name, ]

      if (nrow(matching_specimens) > 0) {
        bin_result <- analyze_bin_content(
          taxa = search_name,
          existing_data = matching_specimens,
          valid_name = valid_name
        )
        if (!is.null(bin_result)) {
          bin_analyses[[key]] <- bin_result
        }
      }
    }
  }

  return(bin_analyses)
}

# Helper function to create summary row (updated version)
create_summary_row <- function(valid_name, search_name, initial_data) {
  species_list <- if (!is.null(initial_data) && nrow(initial_data) > 0) {
    unique_species <- unique(initial_data$species[!is.na(initial_data$species) & initial_data$species != ""])
    paste(sort(unique_species), collapse = "; ")
  } else ""

  data.frame(
    Valid_Name = valid_name,
    Search_Name = search_name,
    Name_Status = if (search_name == valid_name) "Valid" else "Synonym",
    Total_Specimens = if (!is.null(initial_data)) nrow(initial_data) else 0,
    Total_BINs = 0,  # Will be updated later if BINs are found
    Species_Found = species_list,
    Low_Specimen_Count = if (!is.null(initial_data)) {
      if (nrow(initial_data) == 0) "None"
      else if (nrow(initial_data) < 3) "Warning"
      else "OK"
    } else "None",
    Countries = if (!is.null(initial_data) && nrow(initial_data) > 0) {
      unique_countries <- unique(initial_data$country.ocean[!is.na(initial_data$country.ocean)])
      paste(sort(unique_countries), collapse = "; ")
    } else "",
    Status = if (!is.null(initial_data)) {
      if (nrow(initial_data) > 0) "Success"
      else "No records found in BOLD"
    } else "No data found",
    stringsAsFactors = FALSE
  )
}

# Helper function to create empty BIN result
create_empty_bin_result <- function(initial_data) {
  list(
    bin_summary = data.frame(
      Total_BINs = 0,
      Total_Records = if(!is.null(initial_data)) nrow(initial_data) else 0,
      Concordant_BINs = 0,
      Discordant_BINs = 0,
      Total_Species = if(!is.null(initial_data)) 1 else 0,
      Total_Countries = if(!is.null(initial_data)) {
        length(unique(initial_data$country.ocean[!is.na(initial_data$country.ocean)]))
      } else 0,
      stringsAsFactors = FALSE
    ),
    bin_content = data.frame(),
    specimen_data = initial_data
  )
}

# Helper function to process BIN content
process_bin_content <- function(specimens) {
  # Add error handling for empty specimens
  if(is.null(specimens) || nrow(specimens) == 0) {
    return(data.frame())
  }

  specimens %>%
    group_by(bin_uri) %>%
    summarize(
      Total_Records = n(),
      Unique_Species = n_distinct(species[!is.na(species) & species != ""]),
      Species_List = paste(sort(unique(species[!is.na(species) & species != ""])), collapse = "; "),
      Countries = if("country.ocean" %in% colnames(.)) {
        paste(sort(unique(country.ocean[!is.na(country.ocean)])), collapse = "; ")
      } else {
        "No country data"
      },
      Concordance = if(n_distinct(species[!is.na(species) & species != ""]) == 1) {
        "Concordant"
      } else {
        "Discordant"
      },
      .groups = 'drop'
    )
}

# Helper function to create BIN summary
create_bin_summary <- function(bin_content, all_specimens) {
  # Add error handling for empty inputs
  if(is.null(bin_content) || nrow(bin_content) == 0 ||
     is.null(all_specimens) || nrow(all_specimens) == 0) {
    return(data.frame(
      Total_BINs = 0,
      Total_Records = 0,
      Concordant_BINs = 0,
      Discordant_BINs = 0,
      Total_Species = 0,
      Total_Countries = 0,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Total_BINs = nrow(bin_content),
    Total_Records = sum(bin_content$Total_Records),
    Concordant_BINs = sum(bin_content$Concordance == "Concordant", na.rm = TRUE),
    Discordant_BINs = sum(bin_content$Concordance == "Discordant", na.rm = TRUE),
    Total_Species = n_distinct(all_specimens$species[!is.na(all_specimens$species) & all_specimens$species != ""]),
    Total_Countries = if(all(bin_content$Countries != "No country data")) {
      length(unique(unlist(strsplit(bin_content$Countries, "; "))))
    } else {
      0
    },
    stringsAsFactors = FALSE
  )
}

# Modified analyze_bin_content_with_data function
analyze_bin_content_with_data <- function(initial_data, bin_specimens, valid_name, taxa_group, session = NULL, rv = NULL) {
  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("Analyzing BIN content for group: %s", paste(taxa_group, collapse=", ")), session, rv)
  }

  tryCatch({
    # Get relevant BINs from initial data (no species filtering)
    taxon_bins <- unique(initial_data$bin_uri[!is.na(initial_data$bin_uri) & initial_data$bin_uri != ""])

    if (length(taxon_bins) == 0) {
      if (!is.null(session) && !is.null(rv)) {
        log_message(sprintf("No BINs found for group: %s", paste(taxa_group, collapse=", ")), session, rv)
      }
      return(create_empty_bin_result(initial_data))
    }

    # Get all specimens from these BINs
    all_bin_specimens <- bin_specimens[bin_specimens$bin_uri %in% taxon_bins, ]

    if (nrow(all_bin_specimens) == 0) {
      if (!is.null(session) && !is.null(rv)) {
        log_message(sprintf("No specimen data found in BINs for group: %s", paste(taxa_group, collapse=", ")), session, rv)
      }
      return(create_empty_bin_result(initial_data))
    }

    # Add reference information
    all_bin_specimens$Valid_Name <- valid_name
    all_bin_specimens$Original_Group <- paste(taxa_group, collapse="|")

    # Add Name_Status based on species match
    all_bin_specimens$Name_Status <- sapply(all_bin_specimens$species, function(sp) {
      if (is.na(sp) || sp == "") return(NA)
      if (sp == valid_name) return("Valid")
      if (sp %in% taxa_group) return("Synonym")
      return("Other")
    })

    # Create BIN content analysis
    bin_content <- process_bin_content(all_bin_specimens)

    if (nrow(bin_content) == 0) {
      if (!is.null(session) && !is.null(rv)) {
        log_message(sprintf("Failed to process BIN content for group: %s", paste(taxa_group, collapse=", ")), session, rv)
      }
      return(create_empty_bin_result(initial_data))
    }

    # Create summary
    bin_summary <- create_bin_summary(bin_content, all_bin_specimens)

    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("Successfully analyzed BINs for group: %s - found %d BINs with %d species",
                          paste(taxa_group, collapse=", "),
                          bin_summary$Total_BINs,
                          bin_summary$Total_Species),
                  session, rv)
    }

    list(
      bin_summary = bin_summary,
      bin_content = bin_content,
      specimen_data = all_bin_specimens
    )

  }, error = function(e) {
    if (!is.null(session) && !is.null(rv)) {
      log_message(sprintf("BIN analysis failed for group: %s - %s",
                          paste(taxa_group, collapse=", "),
                          e$message),
                  session, rv, "error")
    }
    return(create_empty_bin_result(initial_data))
  })
}

# Function to convert score and criteria to rank
determine_specimen_rank <- function(score, criteria_met) {
  # Split criteria into list
  criteria_list <- unlist(strsplit(criteria_met, "; "))

  # Rank 1: Must have both SPECIES_ID and TYPE_SPECIMEN
  if (all(c("SPECIES_ID", "TYPE_SPECIMEN") %in% criteria_list)) {
    return(1)
  }

  # Rank 2: Species ID, Good sequence, Public voucher, Images, Identifier/ID method,
  # Site, Date, Country, Coord, Collector
  if ("SPECIES_ID" %in% criteria_list &&
      "SEQ_QUALITY" %in% criteria_list &&
      "HAS_IMAGE" %in% criteria_list &&
      any(c("PUBLIC_VOUCHER", "INSTITUTION", "MUSEUM_ID") %in% criteria_list) &&
      any(c("IDENTIFIER", "ID_METHOD") %in% criteria_list) &&
      "SITE" %in% criteria_list &&
      "COLLECTION_DATE" %in% criteria_list &&
      "COUNTRY" %in% criteria_list &&
      "COORD" %in% criteria_list &&
      "COLLECTORS" %in% criteria_list) {
    return(2)
  }

  # Rank 3: Species ID, Good sequence, Public voucher, Images, Identifier/ID method, Country
  if ("SPECIES_ID" %in% criteria_list &&
      "SEQ_QUALITY" %in% criteria_list &&
      "HAS_IMAGE" %in% criteria_list &&
      any(c("PUBLIC_VOUCHER", "INSTITUTION", "MUSEUM_ID") %in% criteria_list) &&
      any(c("IDENTIFIER", "ID_METHOD") %in% criteria_list) &&
      "COUNTRY" %in% criteria_list) {
    return(3)
  }

  # Rank 4: Species ID, Good sequence, Images, Country
  if ("SPECIES_ID" %in% criteria_list &&
      "SEQ_QUALITY" %in% criteria_list &&
      "HAS_IMAGE" %in% criteria_list &&
      "COUNTRY" %in% criteria_list) {
    return(4)
  }

  # Rank 5: Species ID, Good sequence, Images
  if ("SPECIES_ID" %in% criteria_list &&
      "SEQ_QUALITY" %in% criteria_list &&
      "HAS_IMAGE" %in% criteria_list) {
    return(5)
  }

  # Rank 6: Species ID, Good sequence
  if ("SPECIES_ID" %in% criteria_list &&
      "SEQ_QUALITY" %in% criteria_list) {
    return(6)
  }

  # If none of the above criteria are met
  return(7)
}

# Modify the calculate_specimen_score function to include rank
calculate_specimen_score <- function(specimen) {
  score <- 0
  criteria_met <- character()

  # 1. SPECIES_ID - Updated to exclude sp. and numbers
  if (!is.null(specimen$species) && !is.na(specimen$species) &&
      nchar(trimws(specimen$species)) > 0) {
    # Clean the species name
    species_name <- trimws(specimen$species)

    # Check if the species name is valid (no sp., spp., or numbers)
    if (!grepl("sp\\.|spp\\.|[0-9]", species_name) &&
        !grepl("^sp$", species_name, ignore.case = TRUE) &&
        !grepl("aff\\.", species_name) &&
        !grepl("cf\\.", species_name)) {
      score <- score + 1
      criteria_met <- c(criteria_met, "SPECIES_ID")
    }
  }

  # 2. TYPE_SPECIMEN - Updated to check multiple fields and specific type terms
  if (!is.null(specimen)) {
    # List of specific type terms to check for
    type_terms <- paste(
      "holotype", "lectotype", "isotype", "syntype", "paratype",
      "neotype", "allotype", "paralectotype", "hapantotype", "cotype",
      sep = "|"
    )

    # Check for specific type terms in any of the note fields
    has_specific_type <- FALSE
    fields_to_check <- c("taxonomy_notes", "short_note", "collection_notes",
                         "voucher_type", "notes")

    for (field in fields_to_check) {
      if (!is.null(specimen[[field]]) && !is.na(specimen[[field]]) &&
          nchar(trimws(specimen[[field]])) > 0) {
        if (grepl(type_terms, tolower(specimen[[field]]), ignore.case = TRUE)) {
          has_specific_type <- TRUE
          break
        }
      }
    }

    # Check for "type" in voucher_type
    has_type_in_voucher <- FALSE
    if (!is.null(specimen$voucher_type) && !is.na(specimen$voucher_type) &&
        nchar(trimws(specimen$voucher_type)) > 0) {
      has_type_in_voucher <- grepl("type", tolower(specimen$voucher_type),
                                   ignore.case = TRUE)
    }

    # Criterion is met if either condition is true
    if (has_specific_type || has_type_in_voucher) {
      score <- score + 1
      criteria_met <- c(criteria_met, "TYPE_SPECIMEN")
    }
  }

  # 3. SEQ_QUALITY - Updated to require both sequence length and BIN
  if (!is.null(specimen$nuc_basecount) && !is.na(specimen$nuc_basecount) &&
      !is.null(specimen$bin_uri) && !is.na(specimen$bin_uri) &&
      nchar(trimws(specimen$bin_uri)) > 0) {
    tryCatch({
      if (as.numeric(specimen$nuc_basecount) >= 500) {
        score <- score + 1
        criteria_met <- c(criteria_met, "SEQ_QUALITY")
      }
    }, error = function(e) {
      # If conversion fails, skip this criterion
    })
  }

  # 4. PUBLIC_VOUCHER - Updated with specific positive and negative terms
  if (!is.null(specimen$voucher_type) && !is.na(specimen$voucher_type) &&
      nchar(trimws(specimen$voucher_type)) > 0) {
    voucher_value <- tolower(trimws(specimen$voucher_type))

    # First check for negative terms (these override positive matches)
    has_negative <- grepl(
      'DNA|e-vouch|privat|no voucher|unvouchered|destr|lost|missing|no specimen|none|not vouchered|person|Photo Voucher Only|not registered',
      voucher_value,
      ignore.case = TRUE
    )

    # Only check positive terms if no negative terms found
    if (!has_negative) {
      has_positive <- grepl(
        'herb|museum|registered|type|national|CBG|INHS|deposit|harbarium|hebarium|holot',
        voucher_value,
        ignore.case = TRUE
      )

      if (has_positive) {
        score <- score + 1
        criteria_met <- c(criteria_met, "PUBLIC_VOUCHER")
      }
    }
  }

  # 5. HAS_IMAGE
  if (TRUE) {  # Instead of checking image_url
    score <- score + 1
    criteria_met <- c(criteria_met, "HAS_IMAGE")
  }

  # 6. IDENTIFIER - Updated to exclude specific names and BOLD
  if (!is.null(specimen$identified_by) && !is.na(specimen$identified_by) &&
      nchar(trimws(specimen$identified_by)) > 0) {
    identifier <- trimws(specimen$identified_by)
    # Check that identifier is not in exclusion list and doesn't contain BOLD
    if (!grepl("Kate Perez|Angela Telfer|BOLD", identifier, ignore.case = TRUE)) {
      score <- score + 1
      criteria_met <- c(criteria_met, "IDENTIFIER")
    }
  }

  # 7. ID_METHOD - Updated with comprehensive term matching
  if (!is.null(specimen$identification_method) && !is.na(specimen$identification_method) &&
      nchar(trimws(specimen$identification_method)) > 0) {
    id_method <- tolower(trimws(specimen$identification_method))
    # Pattern matches specific terms including partial word matches
    if (grepl("descr|det|diss|exam|expert|genit|identifier|key|label|literature|micros|mor|taxonomic|type|vou|guide|flora|specimen|traditional|visual|wing|logical|knowledge|photo|verified",
              id_method, ignore.case = TRUE)) {
      score <- score + 1
      criteria_met <- c(criteria_met, "ID_METHOD")
    }
  }

  # 8. COLLECTORS
  if (!is.null(specimen$collectors) && !is.na(specimen$collectors) &&
      nchar(trimws(specimen$collectors)) > 0) {
    score <- score + 1
    criteria_met <- c(criteria_met, "COLLECTORS")
  }

  # 9. COLLECTION_DATE
  if (!is.null(specimen$collection_date_start) && !is.na(specimen$collection_date_start) &&
      nchar(trimws(specimen$collection_date_start)) > 0) {
    score <- score + 1
    criteria_met <- c(criteria_met, "COLLECTION_DATE")
  }

  # 10. COUNTRY
  if (!is.null(specimen$country.ocean) && !is.na(specimen$country.ocean) &&
      nchar(trimws(specimen$country.ocean)) > 0) {
    score <- score + 1
    criteria_met <- c(criteria_met, "COUNTRY")
  }

  # 11. SITE
  if (!is.null(specimen$site) && !is.na(specimen$site) &&
      nchar(trimws(specimen$site)) > 0) {
    score <- score + 1
    criteria_met <- c(criteria_met, "SITE")
  }

  # 12. COORD
  if (!is.null(specimen$coord) && !is.na(specimen$coord) &&
      nchar(trimws(specimen$coord)) > 0) {
    score <- score + 1
    criteria_met <- c(criteria_met, "COORD")
  }

  # 13. INSTITUTION - Updated to exclude specific terms
  if (!is.null(specimen$inst) && !is.na(specimen$inst) &&
      nchar(trimws(specimen$inst)) > 0) {
    inst_value <- tolower(trimws(specimen$inst))
    # Check that institution value doesn't contain any excluded terms
    if (!grepl("genbank|no voucher|personal|private|research collection of|unknown|unvouchered",
               inst_value, ignore.case = TRUE)) {
      score <- score + 1
      criteria_met <- c(criteria_met, "INSTITUTION")
    }
  }

  # 14. MUSEUM_ID
  if (!is.null(specimen$museumid) && !is.na(specimen$museumid) &&
      nchar(trimws(specimen$museumid)) > 0) {
    score <- score + 1
    criteria_met <- c(criteria_met, "MUSEUM_ID")
  }

  # Calculate percentage score
  percentage_score <- (score / 14) * 100

  # Determine rank based on criteria met
  rank <- determine_specimen_rank(score, paste(criteria_met, collapse = "; "))

  # Return all scoring information including new rank
  return(list(
    score = score,
    percentage = round(percentage_score, 1),
    rank = rank,
    criteria_met = paste(criteria_met, collapse = "; ")
  ))
}

# Update apply_specimen_ranking accordingly
apply_specimen_ranking <- function(specimen_data) {
  # Apply ranking to each specimen
  rankings <- lapply(1:nrow(specimen_data), function(i) {
    calculate_specimen_score(specimen_data[i,])
  })

  # Extract scores and add to specimen data
  specimen_data$quality_score <- sapply(rankings, function(x) x$score)
  specimen_data$specimen_rank <- sapply(rankings, function(x) x$rank)
  specimen_data$criteria_met <- sapply(rankings, function(x) x$criteria_met)

  return(specimen_data)
}

# BAGS grading updated
calculate_bags_grade <- function(specimens, session = NULL, rv = NULL) {
  # Function to determine BAGS grade
  determine_bags_grade <- function(specimenCount, binCount, hasSharedBins) {
    if (hasSharedBins) return('E')
    if (binCount > 1) return('C')
    if (specimenCount < 3) return('D')
    if (specimenCount > 10) return('A')
    return('B')
  }

  if (sum(!is.na(specimens$species)) == 0) {
    log_message("No valid species found in specimen data", session, rv, "warning")
    return(data.frame())
  }

  # Filter for valid specimens and species names
  valid_specimens <- specimens[!is.na(specimens$species) &
                                 specimens$species != "" &
                                 !grepl("sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\.", specimens$species), ]

  if (nrow(valid_specimens) == 0) {
    log_message("No specimens remaining after filtering non-species identifications",
                session, rv, "warning")
    return(data.frame())
  }

  # Group by species
  species_data <- split(valid_specimens, valid_specimens$species)

  grades <- lapply(species_data, function(species_specimens) {
    species_name <- unique(species_specimens$species)[1]
    specimen_count <- nrow(species_specimens)

    # Get valid BINs (not NA or empty)
    species_bins <- unique(species_specimens$bin_uri[!is.na(species_specimens$bin_uri) &
                                                       species_specimens$bin_uri != ""])
    bin_count <- length(species_bins)

    # Check for shared BINs
    has_shared_bins <- FALSE
    if (length(species_bins) > 0) {
      for (bin in species_bins) {
        # Only look at specimens with valid BINs
        bin_specimens <- valid_specimens[!is.na(valid_specimens$bin_uri) &
                                           valid_specimens$bin_uri == bin,]
        if (length(unique(bin_specimens$species)) > 1) {
          has_shared_bins <- TRUE
          break
        }
      }
    }

    # Get BAGS grade
    bags_grade <- determine_bags_grade(specimen_count, bin_count, has_shared_bins)

    data.frame(
      species = species_name,
      bags_grade = bags_grade,
      specimen_count = specimen_count,
      bin_count = bin_count,
      shared_bins = has_shared_bins,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, grades)

  if (!is.null(session) && !is.null(rv)) {
    log_message(sprintf("BAGS analysis complete: %d specimens, %d species-level identifications",
                        nrow(specimens), nrow(valid_specimens)), session, rv)

    grade_summary <- table(result$bags_grade)
    log_message(sprintf("BAGS grades: A=%d, B=%d, C=%d, D=%d, E=%d",
                        grade_summary["A"] %||% 0, grade_summary["B"] %||% 0,
                        grade_summary["C"] %||% 0, grade_summary["D"] %||% 0,
                        grade_summary["E"] %||% 0), session, rv)
  }

  return(result)
}

# Calculate haplotypes
calculate_haplotypes <- function(specimens) {
  # Remove specimens without sequences
  specimens_with_seq <- specimens[!is.na(specimens$nuc) & specimens$nuc != "", ]

  # Create empty haplotype mapping
  haplotype_mapping <- list()
  haplotype_counter <- 1

  # Create haplotype identifier for each unique sequence
  for(i in 1:nrow(specimens_with_seq)) {
    seq <- specimens_with_seq$nuc[i]
    # Skip if sequence is missing or empty
    if(is.na(seq) || seq == "") next

    # Clean sequence (remove gaps and make uppercase)
    clean_seq <- toupper(gsub("-", "", seq))

    # If this is a new haplotype, assign it a number
    if(is.null(haplotype_mapping[[clean_seq]])) {
      haplotype_mapping[[clean_seq]] <- paste0("Hap", haplotype_counter)
      haplotype_counter <- haplotype_counter + 1
    }

    # Assign haplotype ID to specimen
    specimens_with_seq$haplotype[i] <- haplotype_mapping[[clean_seq]]
  }

  return(specimens_with_seq)
}

# Select best specimens for each haplotype and country
select_best_specimens <- function(specimens) {
  best_specimens <- list()

  # First calculate haplotypes
  specimens <- calculate_haplotypes(specimens)

  # Process each species
  for (species in unique(specimens$species)) {
    species_specimens <- specimens[specimens$species == species, ]

    # Group by country and haplotype
    for (country in unique(species_specimens$country.ocean)) {
      country_specimens <- species_specimens[species_specimens$country.ocean == country, ]

      for (haplotype in unique(country_specimens$haplotype)) {
        if (is.na(haplotype)) next

        hap_specimens <- country_specimens[country_specimens$haplotype == haplotype, ]

        # Sort by quality score and rank
        hap_specimens <- hap_specimens[order(-hap_specimens$quality_score,
                                             hap_specimens$specimen_rank), ]

        # Select best specimen
        if (nrow(hap_specimens) > 0) {
          best_specimen <- hap_specimens[1, ]

          # Create key combining species, country, and haplotype
          key <- paste(species, country, haplotype, sep = "_")
          best_specimens[[key]] <- best_specimen$processid
        }
      }
    }
  }

  return(best_specimens)
}

summarize_haplotype_diversity <- function(specimens) {
  specimens <- calculate_haplotypes(specimens)

  # Group by species
  species_summary <- lapply(split(specimens, specimens$species), function(sp_specimens) {
    n_specimens <- nrow(sp_specimens)
    n_haplotypes <- length(unique(sp_specimens$haplotype[!is.na(sp_specimens$haplotype)]))
    n_countries <- length(unique(sp_specimens$country.ocean))

    data.frame(
      species = sp_specimens$species[1],
      total_specimens = n_specimens,
      unique_haplotypes = n_haplotypes,
      countries = n_countries,
      haplotype_diversity = n_haplotypes/n_specimens,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, species_summary)
}

# Validate BIN data
validate_bin_data <- function(bin_analysis, session = NULL, rv = NULL) {
  if (is.null(bin_analysis)) {
    log_message("BIN analysis is NULL", session, rv, "warning")
    return(FALSE)
  }

  valid <- TRUE

  for (name in names(bin_analysis)) {
    if (is.null(bin_analysis[[name]])) {
      log_message(sprintf("NULL entry for %s in BIN analysis", name), session, rv, "warning")
      valid <- FALSE
      next
    }

    # Check bin_summary
    if (is.null(bin_analysis[[name]]$bin_summary)) {
      log_message(sprintf("Missing bin_summary for %s", name), session, rv, "warning")
      valid <- FALSE
    } else if (nrow(bin_analysis[[name]]$bin_summary) == 0) {
      log_message(sprintf("Empty bin_summary for %s", name), session, rv, "warning")
      valid <- FALSE
    }

    # Check bin_content
    if (is.null(bin_analysis[[name]]$bin_content)) {
      log_message(sprintf("Missing bin_content for %s", name), session, rv, "warning")
      valid <- FALSE
    }
  }

  return(valid)
}

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "BOLDcheckR"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("table")),
      menuItem("BIN Analysis", tabName = "bins", icon = icon("dna")),
      menuItem("Specimens", tabName = "specimens", icon = icon("microscope")),
      menuItem("BAGS Grade A", tabName = "bags_a", icon = icon("star")),
      menuItem("BAGS Grade B", tabName = "bags_b", icon = icon("star-half-alt")),
      menuItem("BAGS Grade C", tabName = "bags_c", icon = icon("exclamation-circle")),
      menuItem("BAGS Grade D", tabName = "bags_d", icon = icon("exclamation-triangle")),
      menuItem("BAGS Grade E", tabName = "bags_e", icon = icon("times-circle")),
      menuItem("Haplotype Analysis", tabName = "haplotypes", icon = icon("dna")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
		.content-wrapper {
		  overflow: auto;
		}
		.table-wrapper {
		  overflow-x: auto;
		}
		.small-box {
		  margin-bottom: 15px;
		}
		.failed-query {
		  background-color: #fff3cd !important;
		}
		.datatable .failed-query:hover {
		  background-color: #ffe7b6 !important;
		}
		/* Make tables more compact */
		.datatable {
		  width: 100% !important;
		  font-size: 12px !important;
		}
		.datatable td {
		  padding: 4px 8px !important;
		  white-space: nowrap;
		}
		.datatable th {
		  padding: 5px 8px !important;
		  white-space: nowrap;
		  font-size: 13px !important;
		}
		/* Style the radio column */
		.select-column {
		  position: sticky !important;
		  left: 0 !important;
		  background: white !important;
		  z-index: 1 !important;
		  max-width: 50px !important;
		  min-width: 50px !important;
		}
		.select-column-header {
		  position: sticky !important;
		  left: 0 !important;
		  background: white !important;
		  z-index: 2 !important;
		}
		/* Add shadow to indicate scrolling */
		.select-column::after {
		  content: '';
		  position: absolute;
		  top: 0;
		  right: -5px;
		  height: 100%;
		  width: 5px;
		  background: linear-gradient(to right, rgba(0,0,0,0.1), rgba(0,0,0,0));
		}
	  "))
    ),
    tabItems(
      # Data Input Tab
      tabItem(tabName = "input",
              fluidRow(
                box(
                  title = "Input Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  # API setup at the top
                  textInput("bold_api_key", "BOLD API Key:", ""),
                  actionButton("set_api_key", "Set API Key", class = "btn-info"),
                  helpText("Contact support@boldsystems.org to obtain an API key"),
                  hr(),
                  # clear search button
                  actionButton("clear_input", "Clear Input Fields",
                               class = "btn-warning",
                               icon = icon("eraser")),
                  hr(),
                  # Main data inputs
                  textAreaInput("taxa_input",
                                "Enter taxa (one per line, comma-separated for synonyms):",
                                rows = 8,
                                placeholder = "Valid name 1, Synonym A, Synonym B\nValid name 2, Synonym C"),
                  textAreaInput("dataset_codes",
                                "Dataset codes (one per line):",
                                rows = 3,
                                placeholder = "DS-EXAMPLE1\nDS-EXAMPLE2"),
                  textAreaInput("project_codes",
                                "Project codes (one per line):",
                                rows = 3,
                                placeholder = "PROJECT1\nPROJECT2"),

                  # Geographic filters box
                  box(
                    title = "Geographic Filters",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,

                    checkboxGroupInput("continents", "Select Continents:",
                                       choices = names(continent_countries),
                                       inline = TRUE),

                    textAreaInput("countries",
                                  "Additional Countries (one per line):",
                                  rows = 3,
                                  placeholder = "Enter additional countries not covered by continent selection"),

                    div(id = "url_warning", style = "display: none;",
                        tags$p(class = "text-warning",
                               icon("warning"),
                               "Warning: Query length approaching limit. Consider reducing selected regions.")),

                    helpText("Note: Selecting multiple continents will include all countries from each selected continent.")
                  ),

                  # Action buttons
                  actionButton("submit", "Get Data",
                               class = "btn-primary",
                               icon = icon("search")),
                  downloadButton("download_csv", "Download Results",
                                 class = "btn-success"),
                  br(), br(),
                  actionButton("clear_results", "Clear Results",
                               class = "btn-warning",
                               icon = icon("trash")),
                  hr(),
                ),

                box(
                  title = "Summary Results",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  div(class = "table-wrapper",
                      withSpinner(DTOutput("summary_table"))
                  )
                )
              )
      ),

      # BIN Analysis Tab
      tabItem(tabName = "bins",
              fluidRow(
                box(
                  title = "BIN Analysis Results",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel("BIN Summary",
                             div(class = "table-wrapper",
                                 withSpinner(DTOutput("bin_summary_table"))
                             )
                    ),
                    tabPanel("BIN Content Details",
                             div(class = "table-wrapper",
                                 withSpinner(DTOutput("bin_content_table"))
                             )
                    )
                  )
                )
              )
      ),

      # Specimens Tab
      tabItem(tabName = "specimens",
              fluidRow(
                box(
                  title = "Specimen Quality Summary",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(2,
                           valueBoxOutput("rank_1_box", width = NULL)
                    ),
                    column(2,
                           valueBoxOutput("rank_2_box", width = NULL)
                    ),
                    column(2,
                           valueBoxOutput("rank_3_box", width = NULL)
                    ),
                    column(2,
                           valueBoxOutput("rank_4_box", width = NULL)
                    ),
                    column(2,
                           valueBoxOutput("rank_5_box", width = NULL)
                    ),
                    column(2,
                           valueBoxOutput("rank_6_box", width = NULL)
                    ),
                    column(2,
                           valueBoxOutput("rank_7_box", width = NULL)
                    )
                  )
                ),
                box(
                  title = "Quality Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(3,
                           selectInput("rank_filter", "Filter by Rank:",
                                       choices = c("All", "1", "2", "3", "4", "5", "6", "7"),
                                       selected = "All")
                    ),
                    column(3,
                           selectInput("min_quality_score", "Minimum Quality Score:",
                                       choices = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"),
                                       selected = "0")
                    ),
                    column(6,
                           checkboxGroupInput("criteria_filter", "Filter by Criteria Met:",
                                              choices = c("SPECIES_ID", "TYPE_SPECIMEN", "SEQ_QUALITY",
                                                          "PUBLIC_VOUCHER", "HAS_IMAGE", "IDENTIFIER",
                                                          "ID_METHOD", "COLLECTORS", "COLLECTION_DATE",
                                                          "COUNTRY", "SITE", "COORD", "INSTITUTION",
                                                          "MUSEUM_ID"),
                                              inline = TRUE)
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Specimen Records",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  br(), br(),
                  div(class = "table-wrapper",
                      withSpinner(DTOutput("specimen_table"))
                  )
                )
              )
      ),

      # BAGS Grade Tabs
      tabItem(tabName = "bags_a",
              fluidRow(
                box(
                  title = "Grade A Species (>10 specimens, 1 BIN)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("bags_a_specimens")
                )
              )
      ),

      tabItem(tabName = "bags_b",
              fluidRow(
                box(
                  title = "Grade B Species (3-10 specimens, 1 BIN)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("bags_b_specimens")
                )
              )
      ),

      tabItem(tabName = "bags_c",
              fluidRow(
                box(
                  title = "Grade C Species (>1 BIN)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("bags_c_specimens")
                )
              )
      ),

      tabItem(tabName = "bags_d",
              fluidRow(
                box(
                  title = "Grade D Species (<3 specimens, 1 BIN)",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("bags_d_specimens")
                )
              )
      ),

      tabItem(tabName = "bags_e",
              fluidRow(
                box(
                  title = "Grade E Species (>1 species in BIN)",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("bags_e_specimens")
                )
              )
      ),

      # Add this new tab item in the tabItems section of dashboardBody
      tabItem(tabName = "haplotypes",
              fluidRow(
                box(
                  title = "Haplotype Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(12,
                           h4("Haplotype Diversity Summary"),
                           DTOutput("haplotype_summary")
                    )
                  ),
                  fluidRow(
                    column(4,
                           selectInput("selected_species",
                                       "Select Species for Haplotype Details",
                                       choices = NULL)
                    ),
                    column(8,
                           DTOutput("species_haplotypes")
                    )
                  )
                )
              )
      ),

      # About Tab
      tabItem(tabName = "about",
              box(
                title = "BOLDcuratoR",
                width = 12,
                includeMarkdown("about.md")
              )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Get preferred column order
  get_preferred_column_order <- function() {
    c("Valid_Name", "Search_Name", "Name_Status", "quality_score",
      "specimen_rank", "bin_uri", "processid", "specimenid",
      "identification", "identification_rank", "identification_method", "identified_by",
      "collectors", "country.ocean", "collection_date_start", "short_note", "taxonomy_notes",
      "collection_notes", "notes", "voucher_type", "subspecies", "species", "genus",
      "family", "order", "class", "phylum", "nuc_basecount", "bold_recordset_code_arr",
      "museumid", "fieldid", "collection_code", "inst", "record_id", "insdc_acs",
      "sampleid", "taxid", "sex", "life_stage", "reproduction", "habitat", "site_code",
      "geoid", "realm", "biome", "ecoregion", "region", "sector", "site", "country_iso",
      "province.state", "elev", "depth", "coord", "coord_source", "coord_accuracy",
      "elev_accuracy", "depth_accuracy", "collection_event_id", "collection_time",
      "collection_date_end", "sampling_protocol", "specimen_linkout", "processid_minted_date",
      "tissue_type", "associated_taxa", "associated_specimens", "funding_src", "kingdom",
      "subfamily", "tribe", "species_reference", "marker_code", "sequence_run_site",
      "sequence_upload_date", "bin_created_date", "Query_Source", "Original_Taxon", "nuc", "criteria_met")
  }

  # Initialize reactive values
  rv <- reactiveValues(
    summary_data = NULL,
    failed_queries = NULL,
    bin_analysis = NULL,
    specimen_data = NULL,
    log_messages = character(),
    api_key_set = FALSE,
    batch_progress = character(),
    last_error = NULL,
    bags_grades = NULL,
    selected_specimens = reactiveVal(list()),
    progress = list(
      current_batch = 0,
      total_batches = 0,
      current_taxon = 0,
      total_taxa = 0,
      current_operation = "",
      operation_details = "",
      status = "idle",
      sub_progress = 0,
      stage_message = ""
    ),
    processing_stats = list(
      start_time = NULL,
      end_time = NULL,
      total_requests = 0,
      successful_requests = 0,
      failed_requests = 0,
      cached_requests = 0
    )
  )

  # Main specimen table renderer
  output$specimen_table <- renderDT({
    req(rv$specimen_data)

    # Start with all data
    filtered_data <- rv$specimen_data

    # Apply rank filter
    if (!is.null(input$rank_filter) && input$rank_filter != "All") {
      filtered_data <- filtered_data[filtered_data$specimen_rank == as.numeric(input$rank_filter), ]
    }

    # Apply minimum quality score filter
    if (!is.null(input$min_quality_score) && input$min_quality_score > 0) {
      filtered_data <- filtered_data[filtered_data$quality_score >= input$min_quality_score, ]
    }

    # Apply criteria filters
    if (!is.null(input$criteria_filter) && length(input$criteria_filter) > 0) {
      filtered_data <- filtered_data[sapply(filtered_data$criteria_met, function(x) {
        if (is.na(x) || x == "") return(FALSE)
        criteria_list <- unlist(strsplit(x, "; "))
        all(input$criteria_filter %in% criteria_list)
      }), ]
    }

    # Get column order
    display_columns <- get_specimen_columns()
    display_columns <- intersect(display_columns, names(filtered_data))

    # Create datatable with basic formatting
    dt <- datatable(filtered_data[, display_columns],
                    options = list(
                      pageLength = 25,
                      scrollX = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel')
                    ),
                    rownames = FALSE
    )

    # Only apply Name_Status formatting if the column exists
    if ("Name_Status" %in% names(filtered_data)) {
      dt <- dt %>%
        formatStyle(
          'Name_Status',
          backgroundColor = styleEqual(
            c("Valid", "Synonym"),
            c('#e3f2fd', 'white')
          ),
          fontWeight = styleEqual(
            c("Valid", "Synonym"),
            c('bold', 'normal')
          ),
          fontStyle = styleEqual(
            c("Valid", "Synonym"),
            c('normal', 'italic')
          )
        )
    }

    # Apply specimen rank formatting if column exists
    if ("specimen_rank" %in% names(filtered_data)) {
      dt <- dt %>%
        formatStyle(
          'specimen_rank',
          backgroundColor = styleInterval(
            c(1.5, 2.5, 3.5, 4.5, 5.5),
            c('#28a745', '#28a745', '#17a2b8', '#17a2b8', '#ffc107', '#ffc107')
          )
        )
    }

    dt
  })

  # Rank summary boxes
  output$rank_1_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 1, na.rm = TRUE)
    valueBox(
      count,
      "Rank 1 Specimens",
      icon = icon("trophy"),
      color = "green"
    )
  })

  output$rank_2_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 2, na.rm = TRUE)
    valueBox(
      count,
      "Rank 2 Specimens",
      icon = icon("medal"),
      color = "green"
    )
  })

  output$rank_3_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 3, na.rm = TRUE)
    valueBox(
      count,
      "Rank 3 Specimens",
      icon = icon("award"),
      color = "blue"
    )
  })

  output$rank_4_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 4, na.rm = TRUE)
    valueBox(
      count,
      "Rank 4 Specimens",
      icon = icon("check"),
      color = "blue"
    )
  })

  output$rank_5_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 5, na.rm = TRUE)
    valueBox(
      count,
      "Rank 5 Specimens",
      icon = icon("circle-check"),
      color = "orange"
    )
  })

  output$rank_6_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 6, na.rm = TRUE)
    valueBox(
      count,
      "Rank 6 Specimens",
      icon = icon("circle"),
      color = "orange"
    )
  })

  output$rank_7_box <- renderValueBox({
    req(rv$specimen_data)
    count <- sum(rv$specimen_data$specimen_rank == 7, na.rm = TRUE)
    valueBox(
      count,
      "Rank 7 Specimens",
      icon = icon("circle"),
      color = "red"
    )
  })


  # haplotype summary
  output$haplotype_summary <- renderDT({
    req(rv$specimen_data)

    summary <- summarize_haplotype_diversity(rv$specimen_data)

    datatable(summary,
              options = list(
                pageLength = 25,
                scrollX = TRUE
              ),
              rownames = FALSE
    ) %>%
      formatRound('haplotype_diversity', digits = 3) %>%
      formatStyle(
        'unique_haplotypes',
        background = styleColorBar(c(0, max(summary$unique_haplotypes)), '#28a745')
      )
  })

  # species haplotypes
  output$species_haplotypes <- renderDT({
    req(input$selected_species, rv$specimen_data)

    # Get specimens for selected species
    species_specimens <- rv$specimen_data[rv$specimen_data$species == input$selected_species, ]

    # Calculate haplotypes
    species_specimens <- calculate_haplotypes(species_specimens)

    # Summarize by haplotype
    haplotype_data <- split(species_specimens, species_specimens$haplotype)
    haplotype_summary <- lapply(haplotype_data, function(hap_specimens) {
      data.frame(
        haplotype = hap_specimens$haplotype[1],
        specimen_count = nrow(hap_specimens),
        countries = paste(unique(hap_specimens$country.ocean), collapse = ", "),
        specimens = paste(hap_specimens$processid, collapse = ", "),
        stringsAsFactors = FALSE
      )
    })

    haplotype_summary <- do.call(rbind, haplotype_summary)

    datatable(haplotype_summary,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE
    )
  })

  # BIN content table renderer
  output$bin_content_table <- renderDT({
    req(rv$bin_analysis)

    # Process BIN content with error handling
    content_data <- lapply(names(rv$bin_analysis), function(name) {
      tryCatch({
        if (!is.null(rv$bin_analysis[[name]]$bin_content) &&
            nrow(rv$bin_analysis[[name]]$bin_content) > 0) {
          content <- rv$bin_analysis[[name]]$bin_content
          name_parts <- strsplit(name, "_")[[1]]
          content$Valid_Name <- name_parts[1]
          content$Search_Name <- name_parts[2]
          content$Name_Status <- if(name_parts[1] == name_parts[2]) "Valid" else "Synonym"
          return(content)
        }
        return(NULL)
      }, error = function(e) {
        log_message(sprintf("Error processing BIN content for %s: %s", name, e$message),
                    session, rv, "warning")
        return(NULL)
      })
    })

    # Filter out NULL results and combine
    content_data <- content_data[!sapply(content_data, is.null)]

    if (length(content_data) == 0) {
      return(NULL)
    }

    content_df <- do.call(rbind, content_data)

    # Reorder columns
    col_order <- c("Valid_Name", "Search_Name", "Name_Status", "bin_uri",
                   setdiff(names(content_df),
                           c("Valid_Name", "Search_Name", "Name_Status", "bin_uri")))
    content_df <- content_df[, intersect(col_order, names(content_df))]

    datatable(content_df,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE
    ) %>%
      formatStyle(
        'Concordance',
        backgroundColor = styleEqual(
          c("Concordant", "Discordant"),
          c('#d4edda', '#f8d7da')
        )
      ) %>%
      formatStyle(
        'Name_Status',
        backgroundColor = styleEqual(
          c("Valid", "Synonym"),
          c('#e3f2fd', 'white')
        ),
        fontWeight = styleEqual(
          c("Valid", "Synonym"),
          c('bold', 'normal')
        ),
        fontStyle = styleEqual(
          c("Valid", "Synonym"),
          c('normal', 'normal')
        )
      )
  })

  # Reactive value for combined countries
  selected_countries <- reactive({
    # Get countries from selected continents
    continent_selected <- unlist(continent_countries[input$continents])

    # Get additional countries from text input
    additional_countries <- NULL
    if (!is.null(input$countries) && nchar(input$countries) > 0) {
      additional_countries <- unlist(strsplit(input$countries, "\n"))
      additional_countries <- trimws(additional_countries[nchar(additional_countries) > 0])
    }

    # Combine and remove duplicates
    unique(c(continent_selected, additional_countries))
  })

  # Set API key
  observeEvent(input$set_api_key, {
    req(input$bold_api_key)
    tryCatch({
      apikey <<- input$bold_api_key
      bold.apikey(apikey)
      rv$api_key_set <- TRUE
      log_message("API key set successfully", session, rv)
      showNotification("API key set successfully", type = "message")
      updateTabItems(session, "tabs", "input")
    }, error = function(e) {
      log_message(paste("Failed to set API key:", e$message), session, rv, "error")
      showNotification(paste("Failed to set API key:", e$message),
                       type = "error", duration = NULL)
    })
  })

  # clear input button
  observeEvent(input$clear_input, {
    # Clear all input fields except API key
    updateTextAreaInput(session, "taxa_input", value = "")
    updateTextAreaInput(session, "dataset_codes", value = "")
    updateTextAreaInput(session, "project_codes", value = "")
    updateTextAreaInput(session, "countries", value = "")

    # Uncheck all continent checkboxes
    updateCheckboxGroupInput(session, "continents",
                             choices = names(continent_countries),
                             selected = character(0))

    # Show notification
    showNotification("Input fields cleared", type = "message")
    log_message("Input fields cleared by user", session, rv)
  })

  # Progress outputs
  output$progress_status_text <- renderText({
    if (rv$progress$status == "processing") {
      rv$progress$stage_message
    } else {
      ""
    }
  })

  output$progress_details <- renderText({
    if (rv$progress$status == "processing") {
      paste(
        rv$progress$current_operation,
        rv$progress$operation_details,
        sep = "\n"
      )
    }
  })

  output$progress_stats <- renderText({
    if (rv$progress$status == "processing") {
      sprintf(
        "Progress: %d%%\nProcessed %d of %d taxa\nTotal requests: %d\nSuccessful: %d\nFailed: %d",
        rv$progress$sub_progress,
        rv$progress$current_taxon,
        rv$progress$total_taxa,
        rv$processing_stats$total_requests,
        rv$processing_stats$successful_requests,
        rv$processing_stats$failed_requests
      )
    }
  })

  # Progress observer
  observe({
    if (rv$progress$status == "processing") {
      shinyjs::runjs(sprintf(
        "document.getElementById('progress_bar').style.width = '%d%%';",
        rv$progress$sub_progress
      ))
    }
  })

  # Add with other observers in server function
  observe({
    req(rv$specimen_data)
    updateSelectInput(session, "selected_species",
                      choices = sort(unique(rv$specimen_data$species)))
  })

  # Output renderers
  output$api_stats <- renderText({
    counts <- rate_limiter$get_counts()
    paste(
      "Total Requests:", rv$processing_stats$total_requests, "\n",
      "Successful:", rv$processing_stats$successful_requests, "\n",
      "Failed:", rv$processing_stats$failed_requests, "\n",
      "Last Error:", if(!is.null(rv$last_error)) rv$last_error else "None", "\n"
    )
  })

  output$processing_log <- renderText({
    paste(rev(rv$log_messages), collapse = "\n")
  })

  output$batch_progress <- renderText({
    paste(rev(rv$batch_progress), collapse = "\n")
  })

  # Main submit button handler
  observeEvent(input$submit, {
    req(input$taxa_input)

    # Check API key
    if (!rv$api_key_set) {
      showNotification("Please set your BOLD API key first",
                       type = "error", duration = NULL)
      log_message("Attempted to submit without API key", session, rv, "error")
      updateTabItems(session, "tabs", "setup")
      return()
    }

    # Clear previous data and logs
    rv$log_messages <- character()
    rv$last_error <- NULL
    rv$specimen_data <- NULL
    rv$bin_analysis <- NULL
    rv$bags_grades <- NULL  # Clear previous BAGS grades
    rv$selected_specimens(list())  # Clear previous selections
    rate_limiter$reset_counts()

    # Parse and clean input taxa
    taxa_input <- unlist(strsplit(input$taxa_input, "\n"))
    taxa_input <- unique(trimws(taxa_input[nchar(taxa_input) > 0]))

    # Process dataset codes into vector
    dataset_codes <- if (!is.null(input$dataset_codes) && nchar(input$dataset_codes) > 0) {
      as.vector(trimws(unlist(strsplit(input$dataset_codes, "\n"))))
    } else {
      NULL
    }

    # Process project codes into vector
    project_codes <- if (!is.null(input$project_codes) && nchar(input$project_codes) > 0) {
      as.vector(trimws(unlist(strsplit(input$project_codes, "\n"))))
    } else {
      NULL
    }

    # Get selected countries as vector
    countries <- if (!is.null(selected_countries()) && length(selected_countries()) > 0) {
      as.vector(selected_countries())
    } else {
      NULL
    }

    # Initialize progress tracking
    isolate({
      rv$progress$status <- "processing"
      rv$progress$sub_progress <- 0
      rv$progress$current_operation <- "Starting search..."
      rv$progress$operation_details <- ""
      rv$progress$stage_message <- "Processing taxa..."
      rv$progress$total_taxa <- length(taxa_input)
      rv$progress$current_taxon <- 0
      rv$processing_stats$start_time <- Sys.time()
    })

    # Log search parameters
    log_message(sprintf("Processing %d taxa", length(taxa_input)), session, rv)
    if (!is.null(countries)) {
      log_message(sprintf("Including %d countries: %s",
                          length(countries),
                          paste(countries, collapse=", ")), session, rv)
    }
    if (!is.null(dataset_codes)) {
      log_message(sprintf("Including %d datasets: %s",
                          length(dataset_codes),
                          paste(dataset_codes, collapse=", ")), session, rv)
    }
    if (!is.null(project_codes)) {
      log_message(sprintf("Including %d projects: %s",
                          length(project_codes),
                          paste(project_codes, collapse=", ")), session, rv)
    }

    # Process taxa
    tryCatch({
      # Update progress
      isolate({
        rv$progress$current_operation <- "Searching BOLD database..."
        rv$progress$sub_progress <- 10
      })

      # Process all taxa using our simplified process_taxa_batch
      results <- process_taxa_batch(
        taxa_batch = taxa_input,
        dataset_codes = dataset_codes,
        project_codes = project_codes,
        countries = countries,
        session = session,
        rv = rv
      )

      if (!is.null(results)) {
        if (nrow(results$specimens) > 0) {
          # Update reactive values with results
          rv$summary_data <- results$bold
          rv$bin_analysis <- results$bins
          rv$specimen_data <- results$specimens

          # Calculate BAGS grades
          rv$bags_grades <- calculate_bags_grade(rv$specimen_data)

          # Pre-select best specimens for each species
          species_list <- unique(rv$specimen_data$species)
          selections <- list()
          for(sp in species_list) {
            species_specimens <- rv$specimen_data[rv$specimen_data$species == sp,]
            species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                         species_specimens$specimen_rank),]
            best_specimen <- species_specimens[species_specimens$specimen_rank <= 3,][1,]
            if (!is.null(best_specimen) && nrow(best_specimen) > 0) {
              selections[[sp]] <- best_specimen$processid
            }
          }
          rv$selected_specimens(selections)

          # Log success
          log_message(sprintf("Successfully processed %d taxa, found %d specimens",
                              length(taxa_input),
                              nrow(results$specimens)), session, rv)

          # Log BAGS grades summary
          bags_summary <- table(rv$bags_grades$bags_grade)
          log_message(sprintf("BAGS grades summary: A=%d, B=%d, C=%d, D=%d, E=%d",
                              bags_summary["A"], bags_summary["B"],
                              bags_summary["C"], bags_summary["D"],
                              bags_summary["E"]),
                      session, rv)

          # Show success notification
          showNotification(
            sprintf("Found %d specimens for %d taxa",
                    nrow(results$specimens),
                    length(taxa_input)),
            type = "message"
          )
        } else {
          log_message("No specimens found for any taxa", session, rv, "warning")
          showNotification("No specimens found for any taxa", type = "warning")
        }
      } else {
        log_message("No results returned from processing", session, rv, "warning")
        showNotification("No results found", type = "warning")
      }

    }, error = function(e) {
      log_message(sprintf("Error processing taxa: %s", e$message), session, rv, "error")
      showNotification(sprintf("Error: %s", e$message), type = "error")
    })

    # Update completion status
    isolate({
      rv$progress$status <- "complete"
      rv$progress$sub_progress <- 100
      rv$progress$current_operation <- "Processing complete"
      rv$processing_stats$end_time <- Sys.time()
    })

    # Log completion time
    processing_time <- as.numeric(difftime(rv$processing_stats$end_time,
                                           rv$processing_stats$start_time,
                                           units = "secs"))

    log_message(sprintf("Completed processing in %.1f seconds", processing_time), session, rv)

    # Update completion notification
    showNotification(
      sprintf("Processed %d taxa in %.1f seconds",
              length(taxa_input),
              processing_time),
      type = "message"
    )

    # Clear progress
    isolate({
      rv$progress$stage_message <- ""
      rv$progress$current_operation <- ""
      rv$progress$operation_details <- ""
    })
  })

  # Clear results
  observeEvent(input$clear_results, {
    # Reset all reactive values
    rv$summary_data <- NULL
    rv$failed_queries <- NULL
    rv$bin_analysis <- NULL
    rv$specimen_data <- NULL
    rv$log_messages <- character()
    rv$batch_progress <- character()
    rv$last_error <- NULL

    # Reset progress tracking
    rv$progress$status <- "idle"
    rv$progress$sub_progress <- 0
    rv$progress$current_operation <- ""
    rv$progress$operation_details <- ""
    rv$progress$stage_message <- ""
    rv$progress$current_batch <- 0
    rv$progress$current_taxon <- 0

    # Reset processing stats
    rv$processing_stats$start_time <- NULL
    rv$processing_stats$end_time <- NULL
    rv$processing_stats$total_requests <- 0
    rv$processing_stats$successful_requests <- 0
    rv$processing_stats$failed_requests <- 0
    rv$processing_stats$cached_requests <- 0

    # Reset the progress bar
    shinyjs::runjs("document.getElementById('progress_bar').style.width = '0%';")

    # Reset rate limiter counts
    rate_limiter$reset_counts()

    # Show notification
    showNotification("Results cleared", type = "message")
    log_message("Results cleared by user", session, rv)
  })

  # Table outputs
  output$summary_table <- renderDT({
    req(rv$summary_data)

    # Create factor to preserve original order based on Valid_Name
    taxa_input <- trimws(unlist(strsplit(input$taxa_input, "\n")))
    taxa_input <- taxa_input[nchar(taxa_input) > 0]
    valid_names <- sapply(strsplit(taxa_input, ","), function(x) trimws(x[1]))

    if ("Valid_Name" %in% colnames(rv$summary_data)) {
      rv$summary_data$Valid_Name <- factor(rv$summary_data$Valid_Name,
                                           levels = unique(valid_names),
                                           ordered = TRUE)

      summary_data <- rv$summary_data[order(rv$summary_data$Valid_Name,
                                            rv$summary_data$Name_Status == "Valid",
                                            decreasing = c(FALSE, TRUE)), ]
    } else {
      summary_data <- rv$summary_data
    }

    datatable(summary_data,
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE
    ) %>%
      formatStyle(
        'Species_Found',
        backgroundColor = styleInterval(
          c(1, 5, 10),
          c('#ffecec', '#e6ffe6', '#ccffcc', '#99ff99')
        )
      ) %>%
      formatStyle(
        'Total_Specimens',
        backgroundColor = styleInterval(
          c(1, 10, 100),
          c('#ffecec', '#f7f7f7', '#e7e7e7', '#d7d7d7')
        )
      ) %>%
      formatStyle(
        'Total_BINs',
        backgroundColor = styleInterval(
          c(1, 5, 10),
          c('#ffecec', '#e6ffe6', '#ccffcc', '#99ff99')
        )
      ) %>%
      formatStyle(
        'Low_Specimen_Count',
        backgroundColor = styleEqual(
          c("Warning", "OK"),
          c('#fff3cd', 'white')
        )
      ) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c("Success", "No data found", "Error"),
          c('#d4edda', '#fff3cd', '#f8d7da')
        )
      ) %>%
      formatStyle(
        'Name_Status',
        backgroundColor = styleEqual(
          c("Valid", "Synonym"),
          c('#e3f2fd', '#e3f2fd')
        ),
        fontWeight = styleEqual(
          c("Valid", "Synonym"),
          c('normal', 'normal')
        ),
        fontStyle = styleEqual(
          c("Valid", "Synonym"),
          c('normal', 'normal')
        )
      )
  })

  # Modified version of the bin summary table render function
  output$bin_summary_table <- renderDT({
    req(rv$bin_analysis)

    # Process BIN summaries with error handling
    summary_data <- lapply(names(rv$bin_analysis), function(name) {
      tryCatch({
        if (!is.null(rv$bin_analysis[[name]]$bin_summary) &&
            nrow(rv$bin_analysis[[name]]$bin_summary) > 0) {
          name_parts <- strsplit(name, "_")[[1]]
          summary_df <- rv$bin_analysis[[name]]$bin_summary
          summary_df$Valid_Name <- name_parts[1]
          summary_df$Search_Name <- name_parts[2]
          summary_df$Name_Status <- if(name_parts[1] == name_parts[2]) "Valid" else "Synonym"
          return(summary_df)
        }
        return(NULL)
      }, error = function(e) {
        log_message(sprintf("Error processing BIN summary for %s: %s", name, e$message),
                    session, rv, "warning")
        return(NULL)
      })
    })

    # Filter out NULL results and combine
    summary_data <- summary_data[!sapply(summary_data, is.null)]

    if (length(summary_data) == 0) {
      return(NULL)
    }

    summary_df <- do.call(rbind, summary_data)

    # Reorder columns
    col_order <- c("Valid_Name", "Search_Name", "Name_Status",
                   setdiff(names(summary_df),
                           c("Valid_Name", "Search_Name", "Name_Status")))
    summary_df <- summary_df[, intersect(col_order, names(summary_df))]

    datatable(summary_df,
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE
    ) %>%
      formatStyle(
        'Name_Status',
        backgroundColor = styleEqual(
          c("Valid", "Synonym"),
          c('#e3f2fd', 'white')
        ),
        fontWeight = styleEqual(
          c("Valid", "Synonym"),
          c('bold', 'normal')
        ),
        fontStyle = styleEqual(
          c("Valid", "Synonym"),
          c('normal', 'italic')
        )
      )
  })

  # Modified version of the bin content table render function
  output$bin_content_table <- renderDT({
    req(rv$bin_analysis)

    # Process BIN content with error handling
    content_data <- lapply(names(rv$bin_analysis), function(name) {
      tryCatch({
        if (!is.null(rv$bin_analysis[[name]]$bin_content) &&
            nrow(rv$bin_analysis[[name]]$bin_content) > 0) {
          content <- rv$bin_analysis[[name]]$bin_content
          name_parts <- strsplit(name, "_")[[1]]
          content$Valid_Name <- name_parts[1]
          content$Search_Name <- name_parts[2]
          content$Name_Status <- if(name_parts[1] == name_parts[2]) "Valid" else "Synonym"
          return(content)
        }
        return(NULL)
      }, error = function(e) {
        log_message(sprintf("Error processing BIN content for %s: %s", name, e$message),
                    session, rv, "warning")
        return(NULL)
      })
    })

    # Filter out NULL results and combine
    content_data <- content_data[!sapply(content_data, is.null)]

    if (length(content_data) == 0) {
      return(NULL)
    }

    content_df <- do.call(rbind, content_data)

    # Reorder columns
    col_order <- c("Valid_Name", "Search_Name", "Name_Status", "bin_uri",
                   setdiff(names(content_df),
                           c("Valid_Name", "Search_Name", "Name_Status", "bin_uri")))
    content_df <- content_df[, intersect(col_order, names(content_df))]

    datatable(content_df,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE
    ) %>%
      formatStyle(
        'Concordance',
        backgroundColor = styleEqual(
          c("Concordant", "Discordant"),
          c('#d4edda', '#f8d7da')
        )
      ) %>%
      formatStyle(
        'Name_Status',
        backgroundColor = styleEqual(
          c("Valid", "Synonym"),
          c('#e3f2fd', 'white')
        ),
        fontWeight = styleEqual(
          c("Valid", "Synonym"),
          c('bold', 'normal')
        ),
        fontStyle = styleEqual(
          c("Valid", "Synonym"),
          c('normal', 'normal')
        )
      )
  })

  # Create specimen tables for each species with specific columns and formatting
  get_specimen_columns <- function() {
    c(
      "select",  # Radio button column
      "Valid_Name", "Search_Name", "Name_Status",
      # Quality metrics
      "quality_score", "specimen_rank",
      # Rest of your columns
      "bin_uri", "processid", "specimenid", "identification",
      "identification_rank", "identification_method", "identified_by",
      "collectors", "country.ocean", "collection_date_start",
      "short_note", "taxonomy_notes", "collection_notes", "notes",
      "voucher_type", "subspecies", "species", "genus", "family",
      "order", "class", "phylum", "nuc_basecount", "bold_recordset_code_arr",
      "museumid", "fieldid", "collection_code", "inst", "record_id",
      "insdc_acs", "sampleid", "taxid", "sex", "life_stage",
      "reproduction", "habitat", "site_code", "geoid", "realm",
      "biome", "ecoregion", "region", "sector", "site",
      "country_iso", "province.state", "elev", "depth", "coord",
      "coord_source", "coord_accuracy", "elev_accuracy",
      "depth_accuracy", "collection_event_id", "collection_time",
      "collection_date_end", "sampling_protocol", "specimen_linkout",
      "processid_minted_date", "tissue_type", "associated_taxa",
      "associated_specimens", "funding_src", "kingdom", "subfamily",
      "tribe", "species_reference", "marker_code",
      "sequence_run_site", "sequence_upload_date",
      "bin_created_date", "Query_Source", "Original_Taxon", "nuc", "criteria_met"
    )
  }

  # Update createSpeciesTable to use common columns
  createSpeciesTable <- function(species_specimens) {
    # Add radio button column
    species_specimens$select <- sapply(species_specimens$processid, function(pid) {
      sprintf('<input type="radio" name="select_%s" value="%s" %s/>',
              make.names(species_specimens$species[1]),
              pid,
              ifelse(pid == rv$selected_specimens()[[species_specimens$species[1]]], 'checked', ''))
    })
    # Get common column order starting with select column
    display_columns <- c("select", setdiff(get_specimen_columns(), "select"))

    # Filter to only available columns
    display_columns <- intersect(display_columns, names(species_specimens))

    # Create the datatable with updated formatting
    dt <- datatable(
      species_specimens[, display_columns],
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'ftp',
        order = list(
          list(which(display_columns == "quality_score"), 'desc'),
          list(which(display_columns == "specimen_rank"), 'asc')
        ),
        columnDefs = list(
          list(
            targets = 0,  # First column (select)
            className = 'select-column'
          ),
          list(
            targets = "_all",
            className = 'dt-body-nowrap'
          )
        ),
        fixedColumns = list(
          leftColumns = 1
        )
      ),
      selection = 'none',
      escape = FALSE,
      rownames = FALSE,
      class = 'compact'
    ) %>%
      formatStyle(
        'specimen_rank',
        backgroundColor = styleInterval(
          c(1.5, 2.5, 3.5, 4.5, 5.5),
          c('#28a745', '#28a745', '#17a2b8', '#17a2b8', '#ffc107', '#ffc107')
        )
      ) %>%
      formatStyle(
        'quality_score',
        background = styleColorBar(c(0, 14), '#28a745'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )

    return(dt)
  }

  # Render tables for each BAGS grade
  observe({
    req(rv$specimen_data, rv$bags_grades)

    # Handle tables for each grade
    grades <- c("a", "b", "c", "d", "e")
    for(grade in grades) {
      grade_species <- rv$bags_grades$identification[rv$bags_grades$bags_grade == toupper(grade)]

      for(species in grade_species) {
        local({
          species_local <- species
          grade_local <- grade

          output_id <- paste0("species_table_", grade_local, "_", make.names(species_local))

          output[[output_id]] <- renderDT({
            # Get and sort specimens for this species
            species_specimens <- rv$specimen_data[rv$specimen_data$species == species_local,]
            species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                         species_specimens$specimen_rank),]

            # Create formatted table
            createSpeciesTable(species_specimens)
          })
        })
      }
    }
  })

  # Handle specimen selection from tables
  observe({
    req(rv$specimen_data, rv$bags_grades)

    grades <- c("a", "b", "c", "d", "e")
    for(grade in grades) {
      grade_species <- rv$bags_grades$identification[rv$bags_grades$bags_grade == toupper(grade)]

      for(species in grade_species) {
        local({
          species_local <- species
          grade_local <- grade
          table_id <- paste0("species_table_", grade_local, "_", make.names(species_local))
          radio_id <- paste0("select_", grade_local, "_", make.names(species_local))

          # Observe table selection
          observeEvent(input[[paste0(table_id, "_rows_selected")]], {
            selected_row <- input[[paste0(table_id, "_rows_selected")]]
            if(length(selected_row) > 0) {
              species_specimens <- rv$specimen_data[rv$specimen_data$species == species_local,]
              species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                           species_specimens$specimen_rank),]
              selected_processid <- species_specimens$processid[selected_row]

              # Update radio button
              updateRadioButtons(
                session,
                radio_id,
                selected = selected_processid
              )

              # Update selected specimens
              current_selections <- rv$selected_specimens()
              current_selections[[species_local]] <- selected_processid
              rv$selected_specimens(current_selections)
            }
          })

          # Observe radio button selection
          observeEvent(input[[radio_id]], {
            selected_processid <- input[[radio_id]]
            current_selections <- rv$selected_specimens()
            current_selections[[species_local]] <- selected_processid
            rv$selected_specimens(current_selections)
          })
        })
      }
    }
  })

  # Add summary box for selected specimens
  output$selected_specimens_summary <- renderUI({
    req(rv$selected_specimens())

    selected_count <- length(rv$selected_specimens())

    box(
      title = "Selected Specimens",
      status = "primary",
      width = 12,

      fluidRow(
        column(4,
               valueBox(
                 selected_count,
                 "Total Selected Specimens",
                 icon = icon("check-circle"),
                 color = "blue",
                 width = NULL
               )
        ),
        column(8,
               downloadButton("download_selected_full", "Download Full Records (TSV)",
                              class = "btn-success"),
               downloadButton("download_selected_processids", "Download Process IDs",
                              class = "btn-info")
        )
      ),

      DTOutput("selected_specimens_table")
    )
  })

  # Render table of selected specimens
  output$selected_specimens_table <- renderDT({
    req(rv$selected_specimens())

    selected_ids <- unlist(rv$selected_specimens())
    if(length(selected_ids) == 0) {
      return(NULL)
    }

    selected_data <- rv$specimen_data[rv$specimen_data$processid %in% selected_ids,]

    datatable(
      selected_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ft'
      ),
      selection = 'none'
    )
  })

  # BAGS Grade A tab output
  output$bags_a_specimens <- renderUI({
    req(rv$specimen_data, rv$bags_grades)

    grade_a_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == "A"]
    if(length(grade_a_species) == 0) {
      return(h4("No species with Grade A"))
    }

    lapply(grade_a_species, function(species) {
      species_specimens <- rv$specimen_data[rv$specimen_data$species == species,]
      species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                   species_specimens$specimen_rank),]

      box(
        title = species,
        status = "success",
        width = 12,
        collapsible = TRUE,

        fluidRow(
          column(8,
                 DTOutput(paste0("species_table_a_", make.names(species)))
          )
        )
      )
    })
  })

  # BAGS Grade B tab output
  output$bags_b_specimens <- renderUI({
    req(rv$specimen_data, rv$bags_grades)

    grade_b_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == "B"]
    if(length(grade_b_species) == 0) {
      return(h4("No species with Grade B"))
    }

    lapply(grade_b_species, function(species) {
      species_specimens <- rv$specimen_data[rv$specimen_data$species == species,]
      species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                   species_specimens$specimen_rank),]

      box(
        title = species,
        status = "info",
        width = 12,
        collapsible = TRUE,

        fluidRow(
          column(8,
                 DTOutput(paste0("species_table_b_", make.names(species)))
          )
        )
      )
    })
  })

  # BAGS Grade C tab output
  output$bags_c_specimens <- renderUI({
    req(rv$specimen_data, rv$bags_grades)

    grade_c_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == "C"]
    if(length(grade_c_species) == 0) {
      return(h4("No species with Grade C"))
    }

    lapply(grade_c_species, function(species) {
      species_specimens <- rv$specimen_data[rv$specimen_data$species == species,]
      species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                   species_specimens$specimen_rank),]

      box(
        title = species,
        status = "warning",
        width = 12,
        collapsible = TRUE,

        fluidRow(
          column(8,
                 DTOutput(paste0("species_table_c_", make.names(species)))
          )
        )
      )
    })
  })

  # BAGS Grade D tab output
  output$bags_d_specimens <- renderUI({
    req(rv$specimen_data, rv$bags_grades)

    grade_d_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == "D"]
    if(length(grade_d_species) == 0) {
      return(h4("No species with Grade D"))
    }

    lapply(grade_d_species, function(species) {
      species_specimens <- rv$specimen_data[rv$specimen_data$species == species,]
      species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                   species_specimens$specimen_rank),]

      box(
        title = species,
        status = "danger",
        width = 12,
        collapsible = TRUE,

        fluidRow(
          column(8,
                 DTOutput(paste0("species_table_d_", make.names(species)))
          )
        )
      )
    })
  })

  # BAGS Grade E tab output
  output$bags_e_specimens <- renderUI({
    req(rv$specimen_data, rv$bags_grades)

    grade_e_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == "E"]
    if(length(grade_e_species) == 0) {
      return(h4("No species with Grade E"))
    }

    lapply(grade_e_species, function(species) {
      species_specimens <- rv$specimen_data[rv$specimen_data$species == species,]
      species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                   species_specimens$specimen_rank),]

      box(
        title = species,
        status = "danger",
        width = 12,
        collapsible = TRUE,

        fluidRow(
          column(8,
                 DTOutput(paste0("species_table_e_", make.names(species)))
          )
        )
      )
    })
  })

  # Render specimen tables for each grade
  observe({
    req(rv$specimen_data, rv$bags_grades)

    grades <- c("a", "b", "c", "d", "e")
    for(grade in grades) {
      grade_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == toupper(grade)]

      for(species in grade_species) {
        local({
          species_local <- species
          grade_local <- grade

          output_id <- paste0("species_table_", grade_local, "_", make.names(species_local))

          output[[output_id]] <- renderDT({
            species_specimens <- rv$specimen_data[rv$specimen_data$species == species_local,]
            species_specimens <- species_specimens[order(-species_specimens$quality_score,
                                                         species_specimens$specimen_rank),]

            createSpeciesTable(species_specimens)
          })
        })
      }
    }
  })

  # Handle specimen selection
  observe({
    req(rv$specimen_data, rv$bags_grades)

    grades <- c("a", "b", "c", "d", "e")
    for(grade in grades) {
      grade_species <- rv$bags_grades$species[rv$bags_grades$bags_grade == toupper(grade)]

      for(species in grade_species) {
        local({
          species_local <- species
          grade_local <- grade

          input_id <- paste0("select_", grade_local, "_", make.names(species_local))

          observeEvent(input[[input_id]], {
            selected_processid <- input[[input_id]]
            current_selections <- rv$selected_specimens()
            current_selections[[species_local]] <- selected_processid
            rv$selected_specimens(current_selections)
          })
        })
      }
    }
  })

  # Update the download_csv handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("BOLD_analysis_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      sheets <- list()

      if (!is.null(rv$summary_data)) {
        sheets$Summary <- rv$summary_data
      }

      if (!is.null(rv$specimen_data)) {
        # Parse taxa groups from input
        taxa_input <- unlist(strsplit(input$taxa_input, "\n"))
        taxa_groups <- lapply(taxa_input, function(line) {
          taxa <- trimws(unlist(strsplit(line, ",")))
          taxa[taxa != ""]
        })

        # Create lookup table from taxa groups
        taxa_lookup <- do.call(rbind, lapply(taxa_groups, function(group) {
          valid_name <- group[1]
          data.frame(
            species = group,
            Valid_Name = valid_name,
            Search_Name = group,
            Name_Status = ifelse(group == valid_name, "Valid", "Synonym"),
            stringsAsFactors = FALSE
          )
        }))

        # Add taxonomy information to specimens
        specimen_data <- merge(
          rv$specimen_data,
          taxa_lookup,
          by.x = "species",
          by.y = "species",
          all.x = TRUE
        )

        # Get preferred column order with quality metrics
        preferred_order <- c(
          "Valid_Name", "Search_Name", "Name_Status",
          # Quality metrics
          "quality_grade", "quality_score", "specimen_rank",
          # Rest of your columns
          "bin_uri", "processid", "specimenid", "identification",
          "identification_rank", "identification_method", "identified_by",
          "collectors", "country.ocean", "collection_date_start",
          "short_note", "taxonomy_notes", "collection_notes", "notes",
          "voucher_type", "subspecies", "species", "genus", "family",
          "order", "class", "phylum", "nuc_basecount", "bold_recordset_code_arr",
          "museumid", "fieldid", "collection_code", "inst", "record_id",
          "insdc_acs", "sampleid", "taxid", "sex", "life_stage",
          "reproduction", "habitat", "site_code", "geoid", "realm",
          "biome", "ecoregion", "region", "sector", "site",
          "country_iso", "province.state", "elev", "depth", "coord",
          "coord_source", "coord_accuracy", "elev_accuracy",
          "depth_accuracy", "collection_event_id", "collection_time",
          "collection_date_end", "sampling_protocol", "specimen_linkout",
          "processid_minted_date", "tissue_type", "associated_taxa",
          "associated_specimens", "funding_src", "kingdom", "subfamily",
          "tribe", "species_reference", "marker_code",
          "sequence_run_site", "sequence_upload_date",
          "bin_created_date", "Query_Source", "Original_Taxon", "nuc", "criteria_met"
        )

        # Arrange columns
        available_cols <- intersect(preferred_order, names(specimen_data))
        remaining_cols <- setdiff(names(specimen_data), preferred_order)
        final_order <- c(available_cols, remaining_cols)

        sheets$Specimens <- specimen_data[, final_order]
      }

      if (!is.null(rv$bin_analysis)) {
        # BIN Summary sheet
        bin_summaries <- lapply(names(rv$bin_analysis), function(name) {
          if (!is.null(rv$bin_analysis[[name]]$bin_summary)) {
            name_parts <- strsplit(name, "_")[[1]]
            summary_df <- rv$bin_analysis[[name]]$bin_summary
            summary_df$Valid_Name <- name_parts[1]
            summary_df$Search_Name <- name_parts[2]
            summary_df$Name_Status <- if(name_parts[1] == name_parts[2]) "Valid" else "Synonym"
            col_order <- c("Valid_Name", "Search_Name", "Name_Status",
                           setdiff(names(summary_df),
                                   c("Valid_Name", "Search_Name", "Name_Status")))
            summary_df[, col_order]
          }
        })

        if (length(bin_summaries) > 0) {
          sheets$BIN_Summary <- do.call(rbind, bin_summaries)
        }

        # BIN Content sheet
        bin_contents <- lapply(names(rv$bin_analysis), function(name) {
          if (!is.null(rv$bin_analysis[[name]]$bin_content)) {
            content <- rv$bin_analysis[[name]]$bin_content
            name_parts <- strsplit(name, "_")[[1]]
            content$Valid_Name <- name_parts[1]
            content$Search_Name <- name_parts[2]
            content$Name_Status <- if(name_parts[1] == name_parts[2]) "Valid" else "Synonym"
            content
          }
        })

        if (length(bin_contents) > 0) {
          sheets$BIN_Content <- do.call(rbind, bin_contents)
        }
      }

      # Create a Quality Summary sheet
      if (!is.null(rv$specimen_data) && any(c("quality_grade", "quality_score") %in% names(rv$specimen_data))) {
        quality_summary <- data.frame(
          Grade = c("A", "B", "C", "D", "F"),
          Count = c(
            sum(rv$specimen_data$quality_grade == "A", na.rm = TRUE),
            sum(rv$specimen_data$quality_grade == "B", na.rm = TRUE),
            sum(rv$specimen_data$quality_grade == "C", na.rm = TRUE),
            sum(rv$specimen_data$quality_grade == "D", na.rm = TRUE),
            sum(rv$specimen_data$quality_grade == "F", na.rm = TRUE)
          ),
          Percentage = c(
            mean(rv$specimen_data$quality_grade == "A", na.rm = TRUE) * 100,
            mean(rv$specimen_data$quality_grade == "B", na.rm = TRUE) * 100,
            mean(rv$specimen_data$quality_grade == "C", na.rm = TRUE) * 100,
            mean(rv$specimen_data$quality_grade == "D", na.rm = TRUE) * 100,
            mean(rv$specimen_data$quality_grade == "F", na.rm = TRUE) * 100
          )
        )
        quality_summary$Percentage <- round(quality_summary$Percentage, 1)
        sheets$Quality_Summary <- quality_summary
      }

      writexl::write_xlsx(sheets, file)
    }
  )

  # Clean up on session end
  session$onSessionEnded(function() {
    rate_limiter$reset_counts()
    if (exists("apikey")) {
      rm(apikey, envir = .GlobalEnv)
    }
    log_message("Session ended", session, rv)
  })

  # Handle specimen selection
  observe({
    req(rv$specimen_data)

    lapply(unique(rv$specimen_data$species), function(species) {
      observeEvent(input[[paste0("select_", gsub(" ", "_", species))]], {
        selected_processid <- input[[paste0("select_", gsub(" ", "_", species))]]
        current_selections <- rv$selected_specimens()
        current_selections[[species]] <- selected_processid
        rv$selected_specimens(current_selections)
      })
    })
  })

  # Add this observe block in your server function
  observe({
    if (!is.null(rv$bin_analysis)) {
      valid <- validate_bin_data(rv$bin_analysis, session, rv)
      if (!valid) {
        log_message("Invalid BIN analysis data detected", session, rv, "warning")
        # Optionally show a notification to the user
        showNotification("Some BIN analysis data may be incomplete",
                         type = "warning",
                         duration = 5)
      }
    }
  })

  # Add download handlers
  output$download_selected_full <- downloadHandler(
    filename = function() {
      paste0("selected_specimens_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
    },
    content = function(file) {
      selected_ids <- unlist(rv$selected_specimens())
      selected_data <- rv$specimen_data[rv$specimen_data$processid %in% selected_ids,]
      write.table(selected_data, file, sep="\t", row.names=FALSE, quote=FALSE)
    }
  )

  output$download_selected_processids <- downloadHandler(
    filename = function() {
      paste0("selected_processids_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
    },
    content = function(file) {
      selected_ids <- unlist(rv$selected_specimens())
      writeLines(selected_ids, file)
    }
  )
}

# Create default about.md if missing
if (!file.exists("about.md")) {
  about_content <- c(
    "# BOLDcuratoR",
    "",
    "## Overview",
    "This application helps check and curate BOLD for specimens based on taxonomy, geography, datasets and projects.",
    "",
    "## Features",
    "- Process multiple taxa with their synonyms",
    "- Process multiple datasets and projects",
    "- Process multiple countries or continents",
    "- Fetch all specimens based on search parameters",
    "- Fetch all specimens in each downloaded BIN",
    "- Analyze BIN content and concordance",
    "- Rank all specimens",
    "- Grade all species using BAGS",
    "- Select representative specimens for curated datasets",
    "- Flag specimens for update on BOLD",
    "- Download all resulting data in spreadsheet and fasta format",
    "",
    "## Usage",
    "1. Enter your BOLD API key",
    "2. Input taxa (one per line, synonyms after valid names, separated by commas)",
    "3. Include any datasets and/or projects",
    "4. Include any countries and/or continents",
    "5. Click 'Get Data' to start analysis",
    "",
    paste("Last updated:", format(Sys.time(), "%Y-%m-%d"))
  )
  writeLines(about_content, "about.md")
}

# Error handling for missing directories
for (dir in c("logs", "output")) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
}

# Enhanced global options
options(shiny.maxRequestSize = 30*1024^2)  # Increase max request size to 30MB
options(timeout = 300)                      # 5-minute timeout
options(scipen = 999)                       # Avoid scientific notation
options(shiny.sanitize.errors = FALSE)      # Show detailed error messages

# Run the application
shinyApp(ui = ui, server = server)
