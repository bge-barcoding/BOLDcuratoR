# Tests for User Information Module
# tests/testthat/test-user-info.R

library(testthat)
library(shiny)

test_that("user info UI module generates correctly", {
  ui <- mod_user_info_ui("test")

  # Check UI elements exist
  expect_true(inherits(ui, "shiny.tag"))
  html <- as.character(ui)
  expect_true(grepl('id="test-email"', html))
  expect_true(grepl('id="test-name"', html))
  expect_true(grepl('id="test-orcid"', html))
  expect_true(grepl('id="test-save"', html))
  expect_true(grepl('id="test-clear"', html))
})

test_that("user info server validates email correctly", {
  testServer(mod_user_info_server, args = list(
    state = initialize_state(MockSession$new()),
    logger = list(
      info = function(...) {},
      error = function(...) {}
    )
  ), {
    # Test invalid email
    session$setInputs(
      email = "invalid-email",
      save = 1
    )
    expect_true(any(grepl("Invalid email format",
                          output$validation_message)))

    # Test valid email
    session$setInputs(
      email = "test@example.com",
      save = 2
    )
    expect_false(any(grepl("Invalid email format",
                           output$validation_message)))
  })
})

test_that("user info server validates ORCID correctly", {
  testServer(mod_user_info_server, args = list(
    state = initialize_state(MockSession$new()),
    logger = list(
      info = function(...) {},
      error = function(...) {}
    )
  ), {
    # Test invalid ORCID
    session$setInputs(
      orcid = "0000-0000-0000",
      save = 1
    )
    expect_true(any(grepl("Invalid ORCID format",
                          output$validation_message)))

    # Test valid ORCID
    session$setInputs(
      orcid = "0000-0000-0000-0000",
      save = 2
    )
    expect_false(any(grepl("Invalid ORCID format",
                           output$validation_message)))
  })
})

test_that("user info saves and clears correctly", {
  state <- initialize_state(MockSession$new())

  testServer(mod_user_info_server, args = list(
    state = state,
    logger = list(
      info = function(...) {},
      error = function(...) {}
    )
  ), {
    # Test saving valid info
    session$setInputs(
      email = "test@example.com",
      name = "Test User",
      orcid = "0000-0000-0000-0000",
      save = 1
    )

    user_info <- state$get_reactive_values()$user_info
    expect_equal(user_info$email, "test@example.com")
    expect_equal(user_info$name, "Test User")
    expect_equal(user_info$orcid, "0000-0000-0000-0000")

    # Test clearing info
    session$setInputs(clear = 1)

    user_info <- state$get_reactive_values()$user_info
    expect_null(user_info$email)
    expect_null(user_info$name)
    expect_null(user_info$orcid)
  })
})

# Mock Session class for testing
MockSession <- R6::R6Class("MockSession",
                           public = list(
                             token = "test-token",
                             ns = function(x) paste0("test-", x),
                             sendCustomMessage = function(type, message) NULL,
                             sendInputMessage = function(inputId, message) NULL
                           )
)
