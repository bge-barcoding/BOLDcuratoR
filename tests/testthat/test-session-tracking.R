library(testthat)
library(shiny)

test_that("session initialization works", {
  session <- create_mock_session()
  state <- initialize_state(session)

  session_state <- state$get_reactive_values()$session_state
  expect_false(is.null(session_state$session_id))
  expect_false(is.null(session_state$start_time))
})

test_that("session activity tracking works", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$track_activity("test_action")

  session_state <- state$get_reactive_values()$session_state
  expect_equal(length(session_state$activity_log), 1)
  expect_equal(session_state$activity_log[[1]]$action, "test_action")
})

test_that("session metrics are tracked", {
  session <- create_mock_session()
  state <- initialize_state(session)

  state$track_activity("action1")
  state$track_activity("action2")

  metrics <- state$get_session_metrics()
  expect_equal(metrics$total_actions, 2)
  expect_false(is.null(metrics$session_duration))
})
