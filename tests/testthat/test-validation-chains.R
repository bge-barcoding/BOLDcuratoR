library(testthat)
library(shiny)

test_that("validation chain executes in order", {
  session <- create_mock_session()
  state <- initialize_state(session)

  validation_result <- state$validate_state(c(
    "user_info",
    "api_key",
    "specimen_data"
  ))

  expect_equal(validation_result$validation_order,
               c("user_info", "api_key", "specimen_data"))
  expect_false(validation_result$is_valid)
})

test_that("validation chain stops on first failure", {
  session <- create_mock_session()
  state <- initialize_state(session)

  # Set valid user info but invalid API key
  state$set_user_info(email = "test@example.com", name = "Test User")

  validation_result <- state$validate_state(c(
    "user_info",
    "api_key",
    "specimen_data"
  ))

  expect_equal(validation_result$failed_validation, "api_key")
  expect_false(validation_result$is_valid)
})

test_that("validation chain succeeds with valid state", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$set_user_info(email = "test@example.com", name = "Test User")
  state$set_api_key("valid_key")
  state$set_specimen_data(data.frame(id = 1))

  validation_result <- state$validate_state(c(
    "user_info",
    "api_key",
    "specimen_data"
  ))

  expect_true(validation_result$is_valid)
  expect_null(validation_result$failed_validation)
})

test_that("validation errors are propagated", {
  session <- create_mock_session()
  state <- initialize_state(session)

  validation_result <- state$validate_state("user_info")

  expect_true(!is.null(state$get_reactive_values()$error_state$message))
  expect_equal(state$get_reactive_values()$error_state$source_module, "validation")
})
