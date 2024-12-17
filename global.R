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

  # Database packages
  "DBI",
  "RSQLite",

  # Analysis packages
  "ape",
  "igraph",

  # Bioconductor packages
  "Biostrings",
  "msa",
  "DECIPHER"
)

# Install missing packages function
.install_packages <- function() {
  tryCatch({
    installed_pkgs <- installed.packages()[,"Package"]
    missing_pkgs <- required_packages[!(required_packages %in% installed_pkgs)]

    if(length(missing_pkgs) > 0) {
      message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))

      if(!require("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }

      bioc_packages <- c("Biostrings", "msa", "DECIPHER")
      cran_packages <- setdiff(missing_pkgs, bioc_packages)

      if(length(cran_packages) > 0) {
        install.packages(cran_packages)
      }

      bioc_to_install <- intersect(missing_pkgs, bioc_packages)
      if(length(bioc_to_install) > 0) {
        BiocManager::install(bioc_to_install)
      }
    }
    return(TRUE)
  }, error = function(e) {
    message("Error installing packages: ", e$message)
    if(grepl("Bioconductor", e$message)) {
      message("Please try installing Bioconductor packages manually:\n",
              "if (!require('BiocManager')) install.packages('BiocManager')\n",
              "BiocManager::install(c('Biostrings', 'msa', 'DECIPHER'))")
    }
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

# Source utility functions with error handling
.source_utils <- function() {
  tryCatch({
    utils_path <- "R/utils"
    utils_files <- list.files(utils_path, pattern = "\\.R$", full.names = TRUE)
    if(length(utils_files) == 0) {
      warning("No utility files found in ", utils_path)
      return(FALSE)
    }
    invisible(lapply(utils_files, source))
    return(TRUE)
  }, error = function(e) {
    warning("Error sourcing utility files: ", e$message)
    return(FALSE)
  })
}

# Source modules with error handling
.source_modules <- function() {
  tryCatch({
    modules_path <- "R/modules"
    module_files <- list.files(modules_path, pattern = "\\.R$",
                               recursive = TRUE, full.names = TRUE)
    if(length(module_files) == 0) {
      warning("No module files found in ", modules_path)
      return(FALSE)
    }
    invisible(lapply(module_files, source))
    return(TRUE)
  }, error = function(e) {
    warning("Error sourcing module files: ", e$message)
    return(FALSE)
  })
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
        haplotype = list(
          min_overlap = 100,
          match_score = 1,
          mismatch_penalty = -1,
          gap_penalty = -2
        ),
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

  # Load configuration first
  source("R/config/constants.R")

  # Source utilities and modules
  if(!.source_utils()) {
    stop("Failed to source utility files")
  }
  if(!.source_modules()) {
    stop("Failed to source module files")
  }

  # Initialize components that depend on loaded packages
  analysis_params <- .initialize_analysis_params()
  if(is.null(analysis_params)) {
    stop("Failed to initialize analysis parameters")
  }

  # Create logging manager after ensuring R6 is loaded
  if (!exists("R6Class")) {
    stop("R6 package not properly loaded")
  }
  logging_manager <- LoggingManager$new()

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
