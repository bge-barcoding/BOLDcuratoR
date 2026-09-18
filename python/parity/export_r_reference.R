#!/usr/bin/env Rscript
# Run the R app's scientific logic over the parity fixture and dump the result.
#
# This is the REFERENCE side of the harness. It deliberately calls the shipped
# R code rather than reimplementing it -- including auto_select_best_specimens,
# which is lifted out of app.R by text extraction rather than copied, so it
# cannot silently drift from what the app actually runs.
#
# Run from the repository root:
#   Rscript --vanilla python/parity/export_r_reference.R
#
# --vanilla matters: the repo's .Rprofile bootstraps renv, which tries to reach
# CRAN. Alternatively set RENV_CONFIG_AUTOLOADER_ENABLED=FALSE.
#
# Only R6 is required. Nothing here touches the network: has_image is preset to
# a logical with no NAs, which is the documented precondition for
# SpecimenProcessor to skip check_specimen_images() entirely
# (specimen_processor.R:45-47).

suppressPackageStartupMessages(library(R6))

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1) args[1] else "."
outdir <- if (length(args) >= 2) args[2] else file.path(root, "python/parity/fixtures")

rf <- function(...) file.path(root, ...)

source(rf("R/config/constants.R"))
source(rf("R/utils/ErrorBoundary.R"))
source(rf("R/modules/specimen_handling/specimen_validator.R"))
source(rf("R/modules/specimen_handling/specimen_scorer.R"))
source(rf("R/modules/specimen_handling/specimen_processor.R"))
source(rf("R/utils/bags_grading.R"))
source(rf("R/modules/bin_analysis/mod_bin_analysis_utils.R"))
source(rf("R/modules/data_import/mod_data_import_utils.R"))
# deliberately NOT sourced: global.R, app.R, R/utils/image_utils.R

# --- lift auto_select_best_specimens out of app.R ---------------------------
# It is defined inside server(), so it cannot be sourced. The agent-verified
# fact that makes this safe: it closes over nothing from the server scope --
# every symbol is its `specimens` argument, a local, or base R. Extracting the
# real source text keeps the reference honest; a hand-copy would drift.
extract_function <- function(path, name) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(sprintf("^\\s*%s\\s*<-\\s*function", name), lines)
  if (length(start) != 1L) {
    stop(sprintf("Expected exactly one definition of %s in %s, found %d",
                 name, path, length(start)))
  }
  depth <- 0L
  for (i in seq(start, length(lines))) {
    code <- gsub('"[^"]*"|#.*$', "", lines[i])
    depth <- depth + lengths(regmatches(code, gregexpr("\\{", code)))[1] -
      lengths(regmatches(code, gregexpr("\\}", code)))[1]
    if (i > start || grepl("\\{", code)) {
      if (depth == 0L) {
        return(eval(parse(text = paste(lines[start:i], collapse = "\n")),
                    envir = globalenv()))
      }
    }
  }
  stop(sprintf("Could not find the end of %s in %s", name, path))
}

auto_select_best_specimens <- extract_function(rf("app.R"),
                                               "auto_select_best_specimens")
cat("Extracted auto_select_best_specimens from app.R\n")

# --- a logger that records nothing -----------------------------------------
NullLogger <- R6::R6Class("NullLogger", public = list(
  info = function(msg, ...) invisible(NULL),
  warn = function(msg, ...) invisible(NULL),
  error = function(msg, ...) invisible(NULL)
))

# --- input ------------------------------------------------------------------
fixture <- file.path(outdir, "parity_input.tsv")
specimens <- read.delim(fixture, sep = "\t", quote = "\"",
                        check.names = FALSE, colClasses = "character",
                        na.strings = character(0), stringsAsFactors = FALSE)
cat(sprintf("Read %d rows from %s\n", nrow(specimens), fixture))

cases <- specimens[, c("processid", "case")]
specimens$case <- NULL

# --- the pipeline, in the order app.R runs it -------------------------------
processed <- process_specimen_data(specimens)

# The precondition for skipping the image API: present, logical, no NAs.
processed$has_image <- rep(FALSE, nrow(processed))

logger <- NullLogger$new()
processor <- SpecimenProcessor$new(SpecimenValidator$new(logger),
                                   SpecimenScorer$new(logger), logger)
scored <- processor$process_specimens(processed)
stopifnot(!is.null(scored))
cat(sprintf("Scored %d rows\n", nrow(scored)))

grades <- calculate_bags_grade(scored)
cat(sprintf("Graded %d species\n", nrow(grades)))

bins <- analyze_bin_data(scored)$content
cat(sprintf("Analysed %d BINs\n", nrow(bins)))

selections <- auto_select_best_specimens(scored)
cat(sprintf("Auto-selected %d representatives\n", length(selections)))

# --- output -----------------------------------------------------------------
norm <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

per_specimen <- data.frame(
  processid = norm(scored$processid),
  species = norm(scored$species),
  quality_score = norm(scored$quality_score),
  criteria_met = norm(scored$criteria_met),
  rank = norm(scored$rank),
  selected = norm(scored$processid %in% names(selections)),
  stringsAsFactors = FALSE
)
per_specimen <- per_specimen[order(per_specimen$processid), ]

write.csv(per_specimen, file.path(outdir, "r_specimens.csv"),
          row.names = FALSE, na = "")

bags_out <- if (nrow(grades)) data.frame(
  species = norm(grades$species),
  bags_grade = norm(grades$bags_grade),
  specimen_count = norm(grades$specimen_count),
  bin_count = norm(grades$bin_count),
  shared_bins = norm(grades$shared_bins),
  stringsAsFactors = FALSE
) else data.frame(species = character(), bags_grade = character(),
                  specimen_count = character(), bin_count = character(),
                  shared_bins = character())
write.csv(bags_out[order(bags_out$species), ], file.path(outdir, "r_bags.csv"),
          row.names = FALSE, na = "")

bins_out <- if (nrow(bins)) data.frame(
  bin_uri = norm(bins$bin_uri),
  total_records = norm(bins$total_records),
  unique_species = norm(bins$unique_species),
  species_list = norm(bins$species_list),
  concordance = norm(bins$concordance),
  stringsAsFactors = FALSE
) else data.frame(bin_uri = character(), total_records = character(),
                  unique_species = character(), species_list = character(),
                  concordance = character())
write.csv(bins_out[order(bins_out$bin_uri), ], file.path(outdir, "r_bins.csv"),
          row.names = FALSE, na = "")

cat(sprintf("Wrote r_specimens.csv, r_bags.csv, r_bins.csv to %s\n", outdir))
