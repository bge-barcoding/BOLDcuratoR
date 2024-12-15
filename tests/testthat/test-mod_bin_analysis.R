library(testthat)
library(shiny)

# Create sample test data
sample_specimen_data <- data.frame(
  bin_uri = c("BOLD:AAA", "BOLD:AAA", "BOLD:BBB", "BOLD:BBB", "BOLD:CCC"),
  species = c("Species A", "Species A", "Species B", "Species C", "Species D"),
  country.ocean = c("Canada", "USA", "Canada", "Mexico", "Canada"),
  stringsAsFactors = FALSE
)

test_that("process_bin_content generates correct analysis", {
  unique_bins <- unique(sample_specimen_data$bin_uri)
  result <- process_bin_content(sample_specimen_data, unique_bins)

  expect_equal(nrow(result), 3)  # Three unique BINs
  expect_equal(result$concordance[result$bin_uri == "BOLD:AAA"], "Concordant")
  expect_equal(result$concordance[result$bin_uri == "BOLD:BBB"], "Discordant")
  expect_equal(result$unique_species[result$bin_uri == "BOLD:AAA"], 1)
  expect_equal(result$unique_species[result$bin_uri == "BOLD:BBB"], 2)
  expect_true(grepl("Canada; USA", result$countries[result$bin_uri == "BOLD:AAA"]))
})

test_that("create_bin_summary calculates correct statistics", {
  unique_bins <- unique(sample_specimen_data$bin_uri)
  bin_content <- process_bin_content(sample_specimen_data, unique_bins)
  result <- create_bin_summary(bin_content, sample_specimen_data)

  expect_equal(result$total_bins, 3)
  expect_equal(result$total_records, 5)
  expect_equal(result$concordant_bins, 2)
  expect_equal(result$discordant_bins, 1)
  expect_equal(result$total_species, 4)
  expect_equal(result$total_countries, 3)
})

test_that("calculate_bin_statistics provides correct metrics", {
  unique_bins <- unique(sample_specimen_data$bin_uri)
  bin_content <- process_bin_content(sample_specimen_data, unique_bins)
  result <- calculate_bin_statistics(bin_content)

  expect_equal(result$total_bins, 3)
  expect_equal(result$shared_bins, 1)
  expect_equal(result$avg_species_per_bin, 4/3)
  expect_equal(result$max_species_per_bin, 2)
})

test_that("analyze_bin_data integrates all components correctly", {
  result <- analyze_bin_data(sample_specimen_data)

  expect_type(result, "list")
  expect_named(result, c("summary", "content", "stats"))
  expect_equal(result$summary$total_bins, 3)
  expect_equal(nrow(result$content), 3)
  expect_equal(result$stats$total_bins, 3)
})

test_that("table formatting functions return datatables", {
  unique_bins <- unique(sample_specimen_data$bin_uri)
  bin_content <- process_bin_content(sample_specimen_data, unique_bins)
  bin_summary <- create_bin_summary(bin_content, sample_specimen_data)
  bin_stats <- calculate_bin_statistics(bin_content)

  expect_s3_class(format_bin_content_table(bin_content), "datatables")
  expect_s3_class(format_bin_summary_table(bin_summary), "datatables")
  expect_s3_class(format_bin_stats_table(bin_stats), "datatables")
})

# Test the UI generation
test_that("mod_bin_analysis_ui generates expected structure", {
  ui <- mod_bin_analysis_ui("test")

  expect_true(any(grepl("test-bin_summary_table", ui)))
  expect_true(any(grepl("test-bin_content_table", ui)))
  expect_true(any(grepl("test-bin_stats_table", ui)))
  expect_true(any(grepl("test-total_bins_box", ui)))
})

# Test the server functionality
test_that("mod_bin_analysis_server handles specimen data correctly", {
  testServer(mod_bin_analysis_server, args = list(
    specimen_data = reactive(sample_specimen_data),
    parent_session = list(),
    rv = reactiveValues()
  ), {
    # Initial state
    expect_true(is.null(bin_analysis_rv()$error))
    expect_false(bin_analysis_rv()$processing)

    # Check that data is processed
    session$flushReact()

    expect_false(is.null(bin_analysis_rv()$bin_summary))
    expect_false(is.null(bin_analysis_rv()$bin_content))
    expect_false(is.null(bin_analysis_rv()$bin_stats))
  })
})

# Test error handling
test_that("analyze_bin_data handles invalid input gracefully", {
  invalid_data <- data.frame(
    bin_uri = character(0),
    species = character(0),
    country.ocean = character(0)
  )

  result <- analyze_bin_data(invalid_data)
  expect_equal(result$summary$total_bins, 0)
  expect_equal(nrow(result$content), 0)
  expect_equal(result$stats$total_bins, 0)
})
