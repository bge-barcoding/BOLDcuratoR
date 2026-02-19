# Tests for BAGS grading module
# tests/testthat/test-bags_grading.R
#
# The BAGS grading module filters specimens by grade and renders interactive
# tables.  Module-server integration tests are covered by end-to-end tests.
# Here we test the UI generation.

library(testthat)
library(shiny)
library(shinydashboard)
library(DT)

# Source UI module
source("../../R/modules/bags_grading/mod_bags_grading_ui.R")

test_that("BAGS grading module UI generates correctly", {
  ui <- mod_bags_grading_ui("test", "A")

  html <- as.character(ui)
  expect_true(grepl("test-status_container", html))
  expect_true(grepl("test-specimen_tables", html))
  expect_true(grepl("grade-description", html))
  expect_true(grepl("BAGS Grade A Overview", html))
})
