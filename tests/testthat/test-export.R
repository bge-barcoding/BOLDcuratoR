# tests/testthat/test-export.R

test_that("ExportManager logs successful exports", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  manager <- ExportManager$new(logger, "test-session")

  # Test Excel export logging
  test_data <- list(
    summary_data = data.frame(species = c("A", "B"), count = c(1, 2))
  )
  temp_file <- tempfile(fileext = ".xlsx")

  expect_true(manager$export_excel(
    test_data,
    temp_file,
    user_info = list(email = "test@example.com", name = "Test User")
  ))

  history <- logger$get_export_history(limit = 1)
  expect_equal(nrow(history), 1)
  expect_equal(history$export_type, "excel")
  expect_true(history$success)
  expect_false(is.na(history$record_count))

  unlink(temp_file)
  unlink(temp_db)
})

test_that("ExportManager logs failed exports", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  manager <- ExportManager$new(logger, "test-session")

  # Test invalid data export
  temp_file <- tempfile(fileext = ".xlsx")
  expect_false(manager$export_excel(list(), temp_file))

  history <- logger$get_export_history(limit = 1)
  expect_equal(nrow(history), 1)
  expect_false(history$success)
  expect_false(is.na(history$error_message))

  unlink(temp_db)
})

test_that("ExportManager tracks export statistics", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  manager <- ExportManager$new(logger, "test-session")

  # Generate multiple exports
  test_data <- list(
    specimen_data = data.frame(
      processid = c("A", "B"),
      species = c("sp1", "sp2")
    )
  )

  # Successful exports
  temp_file1 <- tempfile(fileext = ".tsv")
  temp_file2 <- tempfile(fileext = ".xlsx")
  manager$export_tsv(test_data, temp_file1)
  manager$export_excel(test_data, temp_file2)

  # Failed export
  manager$export_excel(list(), tempfile())

  stats <- manager$get_export_stats()
  expect_equal(stats$total_exports, 3)
  expect_equal(stats$successful_exports, 2)
  expect_equal(stats$failed_exports, 1)
  expect_true(stats$total_records_exported > 0)

  unlink(c(temp_file1, temp_file2))
  unlink(temp_db)
})

test_that("ExportManager handles user attribution", {
  temp_db <- tempfile()
  logger <- LoggingManager$new(temp_db)
  manager <- ExportManager$new(logger, "test-session")

  test_data <- list(summary_data = data.frame(x = 1))
  temp_file <- tempfile(fileext = ".xlsx")

  user_info <- list(
    email = "test@example.com",
    name = "Test User"
  )

  manager$export_excel(test_data, temp_file, user_info)

  history <- logger$get_export_history(limit = 1)
  expect_equal(history$user_email, user_info$email)
  expect_equal(history$user_name, user_info$name)

  unlink(temp_file)
  unlink(temp_db)
})
