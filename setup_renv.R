# setup_renv.R
# Run this script ONCE on your local machine to initialize renv
# and generate the renv.lock file for reproducible deployment.
#
# Usage:
#   cd BOLDcuratoR
#   Rscript setup_renv.R
#
# After running, commit the generated files:
#   renv.lock
#   renv/activate.R
#   renv/settings.json
#   .Rprofile

cat("=== BOLDcuratoR renv setup ===\n\n")

# Step 1: Install renv if needed
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installing renv...\n")
  install.packages("renv")
}

# Step 2: Initialize renv for this project
cat("Initializing renv...\n")
renv::init(
  bare = TRUE  # don't auto-discover — we'll install explicitly
)

# Step 3: Install all runtime dependencies
# CRAN packages
cran_pkgs <- c(
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinycssloaders",
  "DT",
  "htmlwidgets",
  "dplyr",
  "tidyr",
  "purrr",
  "R6",
  "logger",
  "markdown",
  "jsonlite",
  "writexl",
  "DBI",
  "RSQLite",
  "httr",
  "digest"
)

cat("Installing CRAN packages...\n")
renv::install(cran_pkgs)

# GitHub packages
cat("Installing BOLDconnectR from GitHub...\n")
renv::install("boldsystems-central/BOLDconnectR")

# Step 4: Snapshot the current state
cat("Taking renv snapshot...\n")
renv::snapshot(prompt = FALSE)

cat("\n=== Done! ===\n")
cat("Generated files to commit:\n")
cat("  renv.lock\n")
cat("  renv/activate.R\n")
cat("  renv/settings.json\n")
cat("  .Rprofile\n")
cat("\nOn your Shiny server, run:  renv::restore()\n")
