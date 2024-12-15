library(testthat)
library(shiny)

test_that("user state initialization works", {
  session <- create_mock_session()
  state <- initialize_state(session)

  expect_type(state$get_reactive_values()$user_info, "list")
  expect_null(state$get_reactive_values()$user_info$email)
  expect_null(state$get_reactive_values()$user_info$name)
  expect_null(state$get_reactive_values()$user_info$orcid)
})

test_that("user state updates work", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$set_user_info(
    email = "test@example.com",
    name = "Test User",
    orcid = "0000-0001-2345-6789"
  )

  user_info <- state$get_reactive_values()$user_info
  expect_equal(user_info$email, "test@example.com")
  expect_equal(user_info$name, "Test User")
  expect_equal(user_info$orcid, "0000-0001-2345-6789")
})

test_that("user state validates email", {
  session <- create_mock_session()
  state <- initialize_state(session)

  expect_error(
    state$set_user_info(email = "invalid-email"),
    "Invalid email format"
  )
})

test_that("user state validates ORCID", {
  session <- create_mock_session()
  state <- initialize_state(session)

  expect_error(
    state$set_user_info(orcid = "invalid-orcid"),
    "Invalid ORCID format"
  )
})

test_that("user state can be cleared", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$set_user_info(
    email = "test@example.com",
    name = "Test User"
  )

  state$clear_user_info()

  user_info <- state$get_reactive_values()$user_info
  expect_null(user_info$email)
  expect_null(user_info$name)
  expect_null(user_info$orcid)
})
