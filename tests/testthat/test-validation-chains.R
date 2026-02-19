# Tests for validation chains
# tests/testthat/test-validation-chains.R
#
# The validation chain concept (ordered per-field validation with
# is_valid / failed_validation / validation_order) was removed during
# the StateManager refactoring.  The current StateManager.validate_state()
# simply checks that keys exist.  See test-state.R for current coverage.

library(testthat)

test_that("placeholder: validation now tested in test-state.R", {
  expect_true(TRUE)
})
