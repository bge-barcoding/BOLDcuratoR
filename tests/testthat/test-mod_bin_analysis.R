# Tests for BIN analysis utility functions
# tests/testthat/test-mod_bin_analysis.R

library(testthat)
library(shiny)
library(shinydashboard)
library(DT)

source("../../R/modules/bin_analysis/mod_bin_analysis_utils.R")
source("../../R/modules/bin_analysis/mod_bin_analysis_ui.R")

# ---------------------------------------------------------------------------
# Test data
# ---------------------------------------------------------------------------

sample_specimen_data <- data.frame(
  processid = c("P1", "P2", "P3", "P4", "P5"),
  bin_uri = c("BOLD:AAA", "BOLD:AAA", "BOLD:BBB", "BOLD:BBB", "BOLD:CCC"),
  species = c("Species A", "Species A", "Species B", "Species C", "Species D"),
  country.ocean = c("Canada", "USA", "Canada", "Mexico", "Canada"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# process_bin_content
# ---------------------------------------------------------------------------

test_that("process_bin_content generates correct analysis", {
  unique_bins <- unique(sample_specimen_data$bin_uri)
  result <- process_bin_content(sample_specimen_data, unique_bins)

  expect_equal(nrow(result), 3)
  expect_equal(result$concordance[result$bin_uri == "BOLD:AAA"], "Concordant")
  expect_equal(result$concordance[result$bin_uri == "BOLD:BBB"], "Discordant")
  expect_equal(result$unique_species[result$bin_uri == "BOLD:AAA"], 1)
  expect_equal(result$unique_species[result$bin_uri == "BOLD:BBB"], 2)
  expect_true(grepl("Canada", result$countries[result$bin_uri == "BOLD:AAA"]))
})

# ---------------------------------------------------------------------------
# analyze_bin_data
# ---------------------------------------------------------------------------

test_that("analyze_bin_data integrates all components correctly", {
  result <- analyze_bin_data(sample_specimen_data)

  expect_type(result, "list")
  expect_true("content" %in% names(result))
  expect_equal(nrow(result$content), 3)
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("analyze_bin_data handles invalid input gracefully", {
  invalid_data <- data.frame(
    bin_uri = character(0),
    species = character(0),
    country.ocean = character(0)
  )

  result <- analyze_bin_data(invalid_data)
  expect_equal(nrow(result$content), 0)
})

test_that("analyze_bin_data handles NULL input", {
  result <- analyze_bin_data(NULL)
  expect_equal(nrow(result$content), 0)
})

# ---------------------------------------------------------------------------
# validate_specimen_data
# ---------------------------------------------------------------------------

test_that("validate_specimen_data checks required columns", {
  valid <- validate_specimen_data(sample_specimen_data)
  expect_true(valid$valid)

  invalid <- data.frame(x = 1)
  result <- validate_specimen_data(invalid)
  expect_false(result$valid)
})

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

test_that("mod_bin_analysis_ui generates expected structure", {
  ui <- mod_bin_analysis_ui("test")
  html <- as.character(ui)
  expect_true(grepl("test-", html))
})
