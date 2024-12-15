# Test Helpers for Data Import Module

#' Create Mock RV Object
#'
#' @return A reactive values object for testing
create_mock_rv <- function() {
  reactiveValues(
    log_messages = character(),
    last_error = NULL,
    api_key_set = FALSE
  )
}

#' Create Mock Session Object
#'
#' @return A mock session object for testing
create_mock_session <- function() {
  structure(
    list(
      ns = function(x) paste0("test-", x),
      sendCustomMessage = function(type, message) NULL,
      sendInputMessage = function(inputId, message) NULL
    ),
    class = "ShinySession"
  )
}

#' Create Sample Test Data
#'
#' @return List of sample test data
create_test_data <- function() {
  list(
    taxa = c(
      "Species A, Synonym A1, Synonym A2",
      "Species B, Synonym B1"
    ),
    countries = c("Canada", "USA", "Mexico"),
    dataset_codes = c("DS-TEST1", "DS-TEST2"),
    project_codes = c("PROJECT1", "PROJECT2")
  )
}
