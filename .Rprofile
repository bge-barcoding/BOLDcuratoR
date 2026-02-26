source("renv/activate.R")

# Auto-restore on fresh clone: if a core dependency is missing,
# the renv library hasn't been populated yet.
if (interactive() && !requireNamespace("shinydashboard", quietly = TRUE)) {
  message("BOLDcuratoR: packages not yet installed. Running renv::restore()...")
  renv::restore(prompt = FALSE)
}
