# tests/testthat/test-export-history.R

test_that("Export history UI generates correctly", {
  ui <- mod_export_history_ui("test")

  # Check UI elements exist
  expect_true(any(grepl("test-date_range", as.character(ui))))
  expect_true(any(grepl("test-export_type", as.character(ui))))
  expect_true(any(grepl("test-format", as.character(ui))))
  expect_true(any(grepl("test-history_table", as.character(ui))))
  expect_true(any(grepl("test-download_history", as.character(ui))))
})

test_that("Export history server handles date filtering", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  state <- list(logger = logger)

  # Add test data
  logger$log_export(
    session_id = "test",
    export_type = "excel",
    file_name = "test.xlsx",
    record_count = 100,
    file_size = 1024,
    format = "xlsx",
    success = TRUE
  )

  testServer(mod_export_history_server, args = list(state = state), {
    # Set date range
    session$setInputs(
      date_range = c(Sys.Date() - 1, Sys.Date()),
      export_type = "All",
      format = "All"
    )

    # Check history data
    expect_false(is.null(history_data()))
    expect_equal(nrow(history_data()), 1)

    # Check stats data
    stats <- stats_data()
    expect_equal(stats$total_exports, 1)
    expect_equal(stats$successful_exports, 1)
  })

  unlink(temp_db)
})

test_that("Export history server handles type filtering", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  state <- list(logger = logger)

  # Add test data with different types
  logger$log_export(
    session_id = "test",
    export_type = "excel",
    file_name = "test1.xlsx",
    record_count = 100,
    file_size = 1024,
    format = "xlsx",
    success = TRUE
  )

  logger$log_export(
    session_id = "test",
    export_type = "tsv",
    file_name = "test2.tsv",
    record_count = 50,
    file_size = 512,
    format = "tsv",
    success = TRUE
  )

  testServer(mod_export_history_server, args = list(state = state), {
    # Test excel filter
    session$setInputs(
      date_range = c(Sys.Date() - 1, Sys.Date()),
      export_type = "excel",
      format = "All"
    )
    expect_equal(nrow(history_data()), 1)
    expect_equal(history_data()$export_type, "excel")

    # Test tsv filter
    session$setInputs(export_type = "tsv")
    expect_equal(nrow(history_data()), 1)
    expect_equal(history_data()$export_type, "tsv")
  })

  unlink(temp_db)
})

test_that("Export history server calculates statistics correctly", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  state <- list(logger = logger)

  # Add test data with mixed success
  logger$log_export(
    session_id = "test",
    export_type = "excel",
    file_name = "success.xlsx",
    record_count = 100,
    file_size = 1024,
    format = "xlsx",
    success = TRUE
  )

  logger$log_export(
    session_id = "test",
    export_type = "excel",
    file_name = "failed.xlsx",
    record_count = 0,
    file_size = 0,
    format = "xlsx",
    success = FALSE,
    error_message = "Test error"
  )

  testServer(mod_export_history_server, args = list(state = state), {
    session$setInputs(
      date_range = c(Sys.Date() - 1, Sys.Date()),
      export_type = "All",
      format = "All"
    )

    stats <- stats_data()
    expect_equal(stats$total_exports, 2)
    expect_equal(stats$successful_exports, 1)
    expect_equal(stats$failed_exports, 1)
    expect_equal(stats$total_records_exported, 100)
    expect_equal(stats$total_size_exported, 1024)
  })

  unlink(temp_db)
})
