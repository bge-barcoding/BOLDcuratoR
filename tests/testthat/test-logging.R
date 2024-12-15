# tests/testthat/test-logging.R

library(testthat)
library(DBI)
library(RSQLite)

test_that("LoggingManager initializes correctly", {
  # Use temporary database for testing
  tmp_db <- tempfile()
  logger <- LoggingManager$new(tmp_db)

  expect_true(file.exists(tmp_db))
  unlink(tmp_db)
})

test_that("log_action records user actions correctly", {
  tmp_db <- tempfile()
  logger <- LoggingManager$new(tmp_db)

  # Test basic logging
  result <- logger$log_action(
    user_email = "test@example.com",
    session_id = "test_session",
    action_type = "select",
    process_ids = c("BOLD:123", "BOLD:456")
  )

  expect_true(result)

  # Verify action was logged
  actions <- logger$get_user_actions("test@example.com")
  expect_equal(nrow(actions), 1)
  expect_equal(actions$action_type, "select")
  expect_equal(length(actions$process_ids[[1]]), 2)

  unlink(tmp_db)
})

test_that("get_specimen_history returns correct history", {
  tmp_db <- tempfile()
  logger <- LoggingManager$new(tmp_db)

  # Log multiple actions for same specimen
  logger$log_action(
    user_email = "user1@example.com",
    session_id = "session1",
    action_type = "select",
    process_ids = "BOLD:123"
  )

  logger$log_action(
    user_email = "user2@example.com",
    session_id = "session2",
    action_type = "flag",
    process_ids = "BOLD:123"
  )

  history <- logger$get_specimen_history("BOLD:123")
  expect_equal(nrow(history), 1)
  expect_equal(length(history$action_history[[1]]), 2)

  unlink(tmp_db)
})

test_that("get_user_actions filters correctly", {
  tmp_db <- tempfile()
  logger <- LoggingManager$new(tmp_db)

  # Log actions with different dates
  logger$log_action(
    user_email = "test@example.com",
    session_id = "session1",
    action_type = "select",
    process_ids = "BOLD:123"
  )

  Sys.sleep(1) # Ensure different timestamps

  logger$log_action(
    user_email = "test@example.com",
    session_id = "session2",
    action_type = "flag",
    process_ids = "BOLD:456"
  )

  # Test date filtering
  mid_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  Sys.sleep(1)

  logger$log_action(
    user_email = "test@example.com",
    session_id = "session3",
    action_type = "select",
    process_ids = "BOLD:789"
  )

  # Get actions before mid_date
  early_actions <- logger$get_user_actions(
    user_identifier = "test@example.com",
    to_date = mid_date
  )
  expect_equal(nrow(early_actions), 2)

  # Get actions after mid_date
  late_actions <- logger$get_user_actions(
    user_identifier = "test@example.com",
    from_date = mid_date
  )
  expect_equal(nrow(late_actions), 2) # Includes the mid_date action

  unlink(tmp_db)
})
