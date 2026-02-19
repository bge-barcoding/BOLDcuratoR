# Tests for StateManager R6 class
# tests/testthat/test-state.R
#
# The StateManager is the single source of truth for all application state.
# These tests verify initialization, update, validation, reset, and history.

library(testthat)
library(shiny)
library(R6)

source("../../R/utils/ErrorBoundary.R")
source("../../R/modules/state/state_manager.R")

# Helper: create a minimal mock session for StateManager
make_session <- function() {
  list(
    token = "test-token",
    ns    = function(x) paste0("test-", x)
  )
}

# Helper: create a mock logger
make_logger <- function() {
  list(
    msgs  = list(),
    info  = function(msg, ...) {},
    warn  = function(msg, ...) {},
    error = function(msg, ...) {}
  )
}

# ---------------------------------------------------------------------------
# Initialization
# ---------------------------------------------------------------------------

test_that("StateManager initializes with default state", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  expect_type(store$user_info, "list")
  expect_null(store$user_info$email)
  expect_null(store$user_info$name)
  expect_null(store$user_info$orcid)
  expect_null(store$specimen_data)
  expect_true(is.list(store$selected_specimens))
  expect_equal(length(store$selected_specimens), 0)
  expect_true(is.list(store$specimen_flags))
  expect_equal(length(store$specimen_flags), 0)
  expect_true(is.list(store$specimen_curator_notes))
  expect_equal(length(store$specimen_curator_notes), 0)
  expect_false(store$processing$active)
  expect_false(store$error$has_error)
})

# ---------------------------------------------------------------------------
# update_state
# ---------------------------------------------------------------------------

test_that("update_state sets values and returns TRUE", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  result <- state$update_state("user_info", list(
    email = "test@example.com",
    name  = "Test User",
    orcid = "0000-0001-2345-6789",
    bold_api_key = NULL
  ))

  expect_true(result)
  expect_equal(store$user_info$email, "test@example.com")
  expect_equal(store$user_info$name, "Test User")
  expect_equal(store$user_info$orcid, "0000-0001-2345-6789")
})

test_that("update_state stores specimen_data", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  df <- data.frame(processid = c("P1", "P2"), species = c("A", "B"),
                   stringsAsFactors = FALSE)
  result <- state$update_state("specimen_data", df)

  expect_true(result)
  expect_equal(nrow(store$specimen_data), 2)
  expect_equal(store$specimen_data$processid, c("P1", "P2"))
})

test_that("update_state stores flags and notes", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  flags <- list(P1 = list(flag = "misidentification", user = "me"))
  state$update_state("specimen_flags", flags)
  expect_equal(store$specimen_flags$P1$flag, "misidentification")

  notes <- list(P2 = list(text = "Check voucher", user = "me"))
  state$update_state("specimen_curator_notes", notes)
  expect_equal(store$specimen_curator_notes$P2$text, "Check voucher")
})

# ---------------------------------------------------------------------------
# validate_state
# ---------------------------------------------------------------------------

test_that("validate_state detects missing keys", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())

  result <- state$validate_state(c("user_info", "specimen_data"))
  expect_true(result$valid)

  result2 <- state$validate_state(c("nonexistent_key"))
  expect_false(result2$valid)
})

# ---------------------------------------------------------------------------
# reset_state
# ---------------------------------------------------------------------------

test_that("reset_state clears specific keys", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  state$update_state("specimen_flags", list(P1 = list(flag = "issue")))
  expect_equal(length(store$specimen_flags), 1)

  state$reset_state("specimen_flags")
  expect_equal(length(store$specimen_flags), 0)
})

test_that("reset_state can reset everything", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  store <- state$get_store()

  state$update_state("user_info", list(email = "test@example.com"))
  state$update_state("specimen_flags", list(P1 = list(flag = "issue")))

  state$reset_state()

  expect_null(store$user_info$email)
  expect_equal(length(store$specimen_flags), 0)
})

# ---------------------------------------------------------------------------
# State history
# ---------------------------------------------------------------------------

test_that("get_history records updates", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())

  state$update_state("specimen_flags", list(P1 = list(flag = "issue")))
  state$update_state("specimen_flags", list(P2 = list(flag = "other")))

  history <- state$get_history("specimen_flags")
  expect_true(length(history) >= 2)
  expect_equal(history[[1]]$key, "specimen_flags")
})

# ---------------------------------------------------------------------------
# get_metrics
# ---------------------------------------------------------------------------

test_that("get_metrics returns summary", {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE))

  state <- StateManager$new(make_session(), make_logger())
  state$update_state("specimen_flags", list(P1 = list(flag = "issue")))

  metrics <- state$get_metrics()
  expect_true(metrics$total_keys > 0)
  expect_true(metrics$updated_keys > 0)
})
