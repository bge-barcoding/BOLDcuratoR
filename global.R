# Package loading and installation
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
  "logger",
  "R6",
  "markdown",
  "purrr",
  "utils",
  "remotes",
  "devtools",

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

# Install missing packages including Bioconductor packages
.install_if_missing <- function(packages) {
  tryCatch({
    installed_pkgs <- installed.packages()[,"Package"]
    new_packages <- packages[!(packages %in% installed_pkgs)]

    if(length(new_packages)) {
      message("Installing missing packages: ", paste(new_packages, collapse = ", "))

      # Install BiocManager if not present
      if(!require("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }

      # Separate Bioconductor and CRAN packages
      bioc_packages <- c("Biostrings", "msa", "DECIPHER")
      cran_packages <- setdiff(new_packages, bioc_packages)

      # Install CRAN packages
      if(length(cran_packages) > 0) {
        install.packages(cran_packages)
      }

      # Install Bioconductor packages
      bioc_to_install <- intersect(new_packages, bioc_packages)
      if(length(bioc_to_install) > 0) {
        BiocManager::install(bioc_to_install)
      }
    }
  }, error = function(e) {
    message("Error installing packages: ", e$message)
    if(grepl("Bioconductor", e$message)) {
      message("Please try installing Bioconductor packages manually using:\n",
              "if (!require('BiocManager')) install.packages('BiocManager')\n",
              "BiocManager::install(c('Biostrings', 'msa', 'DECIPHER'))")
    }
    stop("Package installation failed")
  })
}

# Install BOLDconnectR if missing
if (!require("BOLDconnectR")) {
  tryCatch({
    devtools::install_github("boldsystems-central/BOLDconnectR")
  }, error = function(e) {
    message("Failed to install BOLDconnectR: ", e$message)
    stop("BOLDconnectR installation failed")
  })
}

# Load required packages with error handling
.load_packages <- function(packages) {
  failed_packages <- character()

  for(pkg in packages) {
    tryCatch({
      suppressWarnings(suppressMessages(
        library(pkg, character.only = TRUE)
      ))
    }, error = function(e) {
      failed_packages <<- c(failed_packages, pkg)
    })
  }

  if(length(failed_packages) > 0) {
    stop("Failed to load packages: ", paste(failed_packages, collapse = ", "))
  }
}

# Install missing packages
.install_if_missing(required_packages)

# Load all packages
.load_packages(required_packages)

# Source utility functions
.source_utils <- function() {
  tryCatch({
    utils_path <- "R/utils"
    utils_files <- list.files(utils_path, pattern = "\\.R$", full.names = TRUE)
    if(length(utils_files) == 0) {
      warning("No utility files found in ", utils_path)
      return(FALSE)
    }
    invisible(lapply(utils_files, source))
    TRUE
  }, error = function(e) {
    warning("Error sourcing utility files: ", e$message)
    FALSE
  })
}

# Source modules
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
    TRUE
  }, error = function(e) {
    warning("Error sourcing module files: ", e$message)
    FALSE
  })
}

# Initialize logging
if (!dir.exists("logs")) {
  dir.create("logs")
}

# Create logging manager instance
logging_manager <- LoggingManager$new()

# Load configuration
tryCatch({
  source("R/config/constants.R")
}, error = function(e) {
  stop("Failed to load constants: ", e$message)
})

# Set global options
options(
  shiny.maxRequestSize = 30*1024^2,  # 30MB max request size
  timeout = 300,                      # 5-minute timeout
  scipen = 999,                       # Avoid scientific notation
  shiny.sanitize.errors = FALSE,      # Show detailed error messages
  shiny.fullstacktrace = TRUE,        # Show full stack traces
  warn = 1                            # Show warnings immediately
)

# Create required directories
for(dir in c("data", "logs", "output", "temp")) {
  if(!dir.exists(dir)) {
    dir.create(dir)
  }
}

# Source utilities and modules
if(!.source_utils()) {
  stop("Failed to source utility files")
}

if(!.source_modules()) {
  stop("Failed to source module files")
}

# Initialize analysis parameters
.initialize_analysis_params <- function() {
  # Load from config if exists
  config_file <- "config/analysis_params.json"
  if(file.exists(config_file)) {
    params <- jsonlite::fromJSON(config_file)
  } else {
    # Default parameters
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
  params
}

# Initialize analysis parameters
analysis_params <- .initialize_analysis_params()
