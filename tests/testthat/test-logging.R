# Tests for LoggingManager R6 class
# tests/testthat/test-logging.R
#
# The LoggingManager stores log entries in memory (no database).

library(testthat)
library(R6)

source("../../R/modules/logging/mod_logging.R")

test_that("LoggingManager initializes correctly", {
  logger <- LoggingManager$new()

  logs <- logger$get_logs()
  expect_true(is.list(logs))
  expect_equal(length(logs), 0)
})

test_that("info/warn/error methods store log entries", {
  logger <- LoggingManager$new()

  logger$info("Info message")
  logger$warn("Warning message")
  logger$error("Error message")

  logs <- logger$get_logs()
  expect_equal(length(logs), 3)
  expect_equal(logs[[1]]$level, "INFO")
  expect_equal(logs[[2]]$level, "WARNING")
  expect_equal(logs[[3]]$level, "ERROR")
})

test_that("get_logs filters by level", {
  logger <- LoggingManager$new()

  logger$info("I1")
  logger$warn("W1")
  logger$info("I2")
  logger$error("E1")

  info_logs <- logger$get_logs("INFO")
  expect_equal(length(info_logs), 2)

  error_logs <- logger$get_logs("ERROR")
  expect_equal(length(error_logs), 1)
})

test_that("clear_logs removes all entries", {
  logger <- LoggingManager$new()

  logger$info("Some message")
  expect_equal(length(logger$get_logs()), 1)

  logger$clear_logs()
  expect_equal(length(logger$get_logs()), 0)
})

test_that("log_action records action entries", {
  logger <- LoggingManager$new()

  result <- logger$log_action(
    session_id = "test_session",
    action_type = "select",
    process_ids = c("BOLD:123", "BOLD:456")
  )

  expect_true(result)
  action_logs <- logger$get_logs("ACTION")
  expect_equal(length(action_logs), 1)
  expect_equal(action_logs[[1]]$details$action_type, "select")
})

test_that("log_specimen_selection records selections", {
  logger <- LoggingManager$new()

  result <- logger$log_specimen_selection(
    user_email = "test@example.com",
    session_id = "sess1",
    processid = "BOLD:123",
    selected = TRUE
  )

  expect_true(result)
  sel_logs <- logger$get_logs("SELECTION")
  expect_equal(length(sel_logs), 1)
  expect_true(grepl("selected", sel_logs[[1]]$message))
})

test_that("log_export records export entries", {
  logger <- LoggingManager$new()

  result <- logger$log_export(
    session_id = "sess1",
    export_type = "tsv",
    file_name = "test.tsv",
    record_count = 100,
    file_size = 1024,
    format = "tsv",
    success = TRUE
  )

  expect_true(result)
  export_logs <- logger$get_logs("EXPORT")
  expect_equal(length(export_logs), 1)
  expect_true(export_logs[[1]]$details$success)
})
