library(testthat)
library(shiny)

test_that("error state initialization works", {
  session <- create_mock_session()
  state <- initialize_state(session)

  error_state <- state$get_reactive_values()$error_state
  expect_false(error_state$has_error)
  expect_null(error_state$message)
  expect_null(error_state$details)
  expect_null(error_state$source_module)
  expect_null(error_state$timestamp)
})

test_that("error state gets set correctly", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$set_error(
    message = "Test error",
    details = list(code = 500),
    source = "test_module"
  )

  error_state <- state$get_reactive_values()$error_state
  expect_true(error_state$has_error)
  expect_equal(error_state$message, "Test error")
  expect_equal(error_state$details$code, 500)
  expect_equal(error_state$source_module, "test_module")
  expect_false(is.null(error_state$timestamp))
})

test_that("error state can be cleared", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$set_error("Test error")
  state$clear_error()

  error_state <- state$get_reactive_values()$error_state
  expect_false(error_state$has_error)
  expect_null(error_state$message)
})

test_that("error state tracks history", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$set_error("Error 1", source = "module_1")
  state$set_error("Error 2", source = "module_2")

  error_history <- state$get_error_history()
  expect_equal(length(error_history), 2)
  expect_equal(error_history[[1]]$message, "Error 1")
  expect_equal(error_history[[2]]$message, "Error 2")
})
