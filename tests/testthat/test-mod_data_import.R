# Tests for data import utility functions
# tests/testthat/test-mod_data_import.R

library(testthat)
library(shiny)
library(shinydashboard)
library(DT)

source("../../R/config/constants.R")
source("../../R/modules/data_import/mod_data_import_utils.R")
source("../../R/modules/data_import/mod_data_import_ui.R")

# ---------------------------------------------------------------------------
# clean_geographic_input
# ---------------------------------------------------------------------------

test_that("clean_geographic_input handles various inputs", {
  expect_null(clean_geographic_input(NULL))
  expect_null(clean_geographic_input(character(0)))

  countries <- c(" Canada ", "USA", "Mexico ", " Mexico")
  result <- clean_geographic_input(countries)
  expect_equal(length(result), 3)
  expect_true(all(c("Canada", "USA", "Mexico") %in% result))

  countries <- c("Canada", "", " ", "USA")
  result <- clean_geographic_input(countries)
  expect_equal(length(result), 2)
  expect_true(all(c("Canada", "USA") %in% result))
})

# ---------------------------------------------------------------------------
# validate_data_import_input
# ---------------------------------------------------------------------------

test_that("validate_data_import_input performs proper validation", {
  result <- validate_data_import_input(NULL, NULL, NULL, NULL)
  expect_false(result$valid)
  expect_true(length(result$messages) > 0)

  result <- validate_data_import_input(
    taxa_input = "Species A\nSpecies B",
    dataset_codes = NULL,
    project_codes = NULL,
    countries = NULL
  )
  expect_true(result$valid)
  expect_equal(length(result$messages), 0)

  result <- validate_data_import_input(
    taxa_input = NULL,
    dataset_codes = "DS-TEST1",
    project_codes = NULL,
    countries = NULL
  )
  expect_true(result$valid)
})

# ---------------------------------------------------------------------------
# format_error_message
# ---------------------------------------------------------------------------

test_that("format_error_message creates properly formatted messages", {
  msg <- "Test error"
  result <- format_error_message(msg, "error")
  expect_match(result, "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]")
  expect_match(result, "ERROR")
  expect_match(result, msg)

  result <- format_error_message(msg, "warning")
  expect_match(result, "WARNING")
})

# ---------------------------------------------------------------------------
# UI generation
# ---------------------------------------------------------------------------

test_that("mod_data_import_ui generates expected structure", {
  ui <- mod_data_import_ui("test")

  html <- as.character(ui)
  expect_true(grepl("test-taxa_input", html))
  expect_true(grepl("test-dataset_codes", html))
  expect_true(grepl("test-project_codes", html))
})
