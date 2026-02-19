# Tests for specimen handling module
# tests/testthat/test-specimen_handling.R
#
# The specimen handling module displays specimens in a read-only table and
# syncs annotations from the BAGS tables via the StateManager.  Module-server
# integration tests require a full Shiny test harness; we test the UI
# generation and the underlying pure-function helpers here.

library(testthat)
library(shiny)
library(shinydashboard)
library(DT)

# Source UI module (pure function, no state required)
source("../../R/modules/specimen_handling/mod_specimen_handling_ui.R")

test_that("specimen handling UI module generates correctly", {
  ui <- mod_specimen_handling_ui("test")

  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
  html <- as.character(ui)
  expect_true(grepl("specimen_table", html))
})
