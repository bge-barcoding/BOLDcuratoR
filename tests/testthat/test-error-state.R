# Tests for error state management in StateManager
# tests/testthat/test-error-state.R

library(testthat)
library(shiny)
library(R6)

source("../../R/utils/ErrorBoundary.R")
source("../../R/modules/state/state_manager.R")

make_session <- function() {
  list(token = "test-token", ns = function(x) paste0("test-", x))
}

make_logger <- function() {
  list(info = function(...) {}, warn = function(...) {}, error = function(...) {})
}

test_that("error state initializes correctly", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  expect_false(store$error$has_error)
  expect_null(store$error$message)
  expect_null(store$error$details)
  expect_null(store$error$source)
  expect_null(store$error$timestamp)
})

test_that("error state can be set via update_state", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  state$update_state("error", list(
    has_error = TRUE,
    message   = "Test error",
    details   = list(code = 500),
    source    = "test_module",
    timestamp = Sys.time()
  ))

  expect_true(store$error$has_error)
  expect_equal(store$error$message, "Test error")
  expect_equal(store$error$details$code, 500)
  expect_equal(store$error$source, "test_module")
})

test_that("error state can be cleared via update_state", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  state$update_state("error", list(
    has_error = TRUE,
    message   = "Test error",
    details   = NULL,
    source    = NULL,
    timestamp = NULL
  ))

  state$update_state("error", list(
    has_error = FALSE,
    message   = NULL,
    details   = NULL,
    source    = NULL,
    timestamp = NULL
  ))

  expect_false(store$error$has_error)
  expect_null(store$error$message)
})

test_that("error state changes are tracked in history", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())

  state$update_state("error", list(
    has_error = TRUE, message = "Error 1",
    details = NULL, source = "module_1", timestamp = NULL
  ))
  state$update_state("error", list(
    has_error = TRUE, message = "Error 2",
    details = NULL, source = "module_2", timestamp = NULL
  ))

  history <- state$get_history("error")
  expect_true(length(history) >= 2)
})
