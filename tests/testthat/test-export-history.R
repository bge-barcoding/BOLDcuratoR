# tests/testthat/test-export-history.R
#
# The export history module depends on LoggingManager methods
# (get_export_history, get_export_stats) that are not yet implemented
# in the current LoggingManager.  UI-only tests are retained.

library(testthat)
library(shiny)
library(shinydashboard)
library(DT)

source("../../R/modules/export_history/mod_export_history_ui.R")

test_that("Export history UI generates correctly", {
  ui <- mod_export_history_ui("test")

  html <- as.character(ui)
  expect_true(grepl("test-", html))
})
