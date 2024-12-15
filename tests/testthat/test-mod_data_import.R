library(testthat)
library(shiny)

test_that("parse_taxa_input handles various input formats", {
  # Test empty input
  expect_null(parse_taxa_input(""))
  expect_null(parse_taxa_input(NULL))

  # Test single taxon
  input <- "Species A"
  result <- parse_taxa_input(input)
  expect_equal(length(result), 1)
  expect_equal(result[[1]], "Species A")

  # Test multiple taxa with synonyms
  input <- "Species A, Synonym A1, Synonym A2\nSpecies B, Synonym B1"
  result <- parse_taxa_input(input)
  expect_equal(length(result), 2)
  expect_equal(result[[1]], c("Species A", "Synonym A1", "Synonym A2"))
  expect_equal(result[[2]], c("Species B", "Synonym B1"))
})

test_that("clean_geographic_input handles various inputs", {
  # Test empty input
  expect_null(clean_geographic_input(NULL))
  expect_null(clean_geographic_input(character(0)))

  # Test valid input
  countries <- c(" Canada ", "USA", "Mexico ", " Mexico")
  result <- clean_geographic_input(countries)
  expect_equal(length(result), 3)
  expect_true(all(c("Canada", "USA", "Mexico") %in% result))

  # Test empty strings
  countries <- c("Canada", "", " ", "USA")
  result <- clean_geographic_input(countries)
  expect_equal(length(result), 2)
  expect_true(all(c("Canada", "USA") %in% result))
})

test_that("validate_query_length identifies long queries", {
  # Test short query
  result <- validate_query_length(
    taxa = c("Species A", "Species B"),
    countries = c("Canada", "USA"),
    dataset_codes = "DS-TEST1",
    project_codes = NULL
  )
  expect_true(result$valid)
  expect_null(result$message)

  # Test long query
  long_taxa <- replicate(100, paste0(sample(LETTERS, 20, replace = TRUE), collapse = ""))
  result <- validate_query_length(
    taxa = long_taxa,
    countries = c("Canada", "USA"),
    dataset_codes = "DS-TEST1",
    project_codes = NULL
  )
  expect_false(result$valid)
  expect_match(result$message, "Query length exceeds")
})

test_that("validate_data_import_input performs proper validation", {
  # Test empty input
  result <- validate_data_import_input(NULL, NULL, NULL, NULL)
  expect_false(result$valid)
  expect_true(length(result$messages) > 0)

  # Test valid taxa input
  result <- validate_data_import_input(
    taxa_input = "Species A\nSpecies B",
    dataset_codes = NULL,
    project_codes = NULL,
    countries = NULL
  )
  expect_true(result$valid)
  expect_equal(length(result$messages), 0)

  # Test valid dataset code input
  result <- validate_data_import_input(
    taxa_input = NULL,
    dataset_codes = "DS-TEST1",
    project_codes = NULL,
    countries = NULL
  )
  expect_true(result$valid)
})

test_that("format_error_message creates properly formatted messages", {
  msg <- "Test error"
  result <- format_error_message(msg, "error")
  expect_match(result, "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]")
  expect_match(result, "ERROR")
  expect_match(result, msg)

  result <- format_error_message(msg, "warning")
  expect_match(result, "WARNING")
})

# Test the UI generation
test_that("mod_data_import_ui generates expected structure", {
  ui <- mod_data_import_ui("test")

  # Test that key input elements exist
  expect_true(any(grepl("test-bold_api_key", ui)))
  expect_true(any(grepl("test-taxa_input", ui)))
  expect_true(any(grepl("test-dataset_codes", ui)))
  expect_true(any(grepl("test-project_codes", ui)))
  expect_true(any(grepl("test-continents", ui)))
  expect_true(any(grepl("test-countries", ui)))
})

# Test the server functionality
test_that("mod_data_import_server handles input correctly", {
  testServer(mod_data_import_server, {
    # Test API key setting
    session$setInputs(bold_api_key = "test_key")
    session$setInputs(set_api_key = TRUE)

    # Test input clearing
    session$setInputs(clear_input = TRUE)

    # Test submit with invalid input
    session$setInputs(submit = TRUE)
    expect_false(data_import_rv()$input_validated)

    # Test submit with valid input
    session$setInputs(
      taxa_input = "Species A\nSpecies B",
      dataset_codes = "DS-TEST1",
      submit = TRUE
    )
    expect_true(data_import_rv()$input_validated)
  })
})
