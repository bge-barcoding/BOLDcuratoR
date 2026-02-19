# Tests for User Information Module
# tests/testthat/test-user-info.R

library(testthat)
library(shiny)
library(shinydashboard)

# Source UI module
source("../../R/modules/user/mod_user_info_ui.R")

test_that("user info UI module generates correctly", {
  ui <- mod_user_info_ui("test")

  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
  html <- as.character(ui)
  expect_true(grepl('id="test-email"', html))
  expect_true(grepl('id="test-name"', html))
  expect_true(grepl('id="test-orcid"', html))
})
