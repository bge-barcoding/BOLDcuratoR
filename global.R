# First ensure R6 is available
if (!require("R6", quietly = TRUE)) {
  install.packages("R6")
  if (!require("R6", quietly = TRUE)) {
    stop("Failed to load critical package: R6")
  }
}

# Define required packages
required_packages <- c(
  # Core packages
  "shiny",
  "writexl",
  "DT",
  "htmlwidgets",
  "shinydashboard",
  "shinyjs",
  "dplyr",
  "shinycssloaders",
  "tidyr",
  "R6",
  "logger",
  "markdown",
  "purrr",
  "utils",
  "remotes",
  "devtools",
  "jsonlite",
  "httr",

  # Database packages
  "DBI",
  "RSQLite"
)

# Install missing packages function
.install_packages <- function() {
  tryCatch({
    installed_pkgs <- installed.packages()[,"Package"]
    missing_pkgs <- required_packages[!(required_packages %in% installed_pkgs)]

    if(length(missing_pkgs) > 0) {
      message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
      install.packages(missing_pkgs)
    }
    return(TRUE)
  }, error = function(e) {
    message("Error installing packages: ", e$message)
    return(FALSE)
  })
}

# Load packages with error handling
.load_packages <- function() {
  # Then load other packages
  for(pkg in setdiff(required_packages, "R6")) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(sprintf("Failed to load package: %s", pkg))
    }
  }
}

# Install BOLDconnectR if missing
.install_boldconnectr <- function() {
  if (!require("BOLDconnectR")) {
    tryCatch({
      devtools::install_github("boldsystems-central/BOLDconnectR")
      return(TRUE)
    }, error = function(e) {
      message("Failed to install BOLDconnectR: ", e$message)
      return(FALSE)
    })
  }
  return(TRUE)
}

# Create required directories
.create_directories <- function() {
  required_dirs <- c("data", "logs", "output", "temp", "config")
  for(dir in required_dirs) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

# Initialize analysis parameters
.initialize_analysis_params <- function() {
  tryCatch({
    config_file <- "config/analysis_params.json"
    if(file.exists(config_file)) {
      params <- jsonlite::fromJSON(config_file)
    } else {
      params <- list(
        bin = list(
          min_specimens = 3,
          min_coverage = 0.8,
          concordance_threshold = 0.95
        ),
        bags = list(
          min_quality_score = 0,
          max_rank = 7
        )
      )
      jsonlite::write_json(params, config_file, pretty = TRUE)
    }
    return(params)
  }, error = function(e) {
    warning("Error initializing analysis parameters: ", e$message)
    return(NULL)
  })
}

#' Get fallback BOLD API key for testing or shared use
#'
#' Checks in order:
#'   1. BOLD_API_KEY environment variable
#'   2. .bold_api_key file in app root (one line, gitignored)
#'
#' @return API key string or NULL if not found
get_fallback_api_key <- function() {
  # 1. Environment variable
  key <- Sys.getenv("BOLD_API_KEY", unset = "")
  if (nchar(key) > 0) return(key)

  # 2. Local file
  key_file <- ".bold_api_key"
  if (file.exists(key_file)) {
    key <- trimws(readLines(key_file, n = 1, warn = FALSE))
    if (nchar(key) > 0) return(key)
  }

  NULL
}

# Main initialization
tryCatch({
  # First install and load all required packages
  if(!.install_packages()) {
    stop("Package installation failed")
  }
  .load_packages()

  if(!.install_boldconnectr()) {
    stop("BOLDconnectR installation failed")
  }

  # Create required directories
  .create_directories()

  # Load configuration
  source("R/config/constants.R")

  # Initialize analysis parameters
  analysis_params <- .initialize_analysis_params()
  if(is.null(analysis_params)) {
    stop("Failed to initialize analysis parameters")
  }

  # Note: R source files (utils + modules) are loaded by app.R in explicit order.
  # Do NOT glob-source here — app.R controls the load order.

  options(
    shiny.maxRequestSize = 30*1024^2,  # 30MB
    timeout = 300,                      # 5 minutes
    scipen = 999,                       # Avoid scientific notation
    shiny.sanitize.errors = FALSE,      # Show detailed errors
    shiny.fullstacktrace = TRUE,        # Show full error traces
    warn = 1                            # Show warnings immediately
  )

}, error = function(e) {
  stop("Initialization failed: ", e$message)
})
