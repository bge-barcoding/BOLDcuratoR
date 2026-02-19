# Tests for ExportManager
# tests/testthat/test-export.R
#
# The ExportManager delegates to LoggingManager for recording export events.
# We use a mock logger that captures log_export calls.

library(testthat)
library(R6)

source("../../R/config/constants.R")
source("../../R/utils/annotation_utils.R")
source("../../R/modules/export/mod_export.R")

# ---------------------------------------------------------------------------
# Mock logger (matches LoggingManager public interface)
# ---------------------------------------------------------------------------

MockLogger <- R6::R6Class("MockLogger",
  public = list(
    exports = list(),
    info = function(...) invisible(NULL),
    warn = function(...) invisible(NULL),
    error = function(...) invisible(NULL),
    log_export = function(...) {
      self$exports[[length(self$exports) + 1]] <- list(...)
    }
  )
)

# ---------------------------------------------------------------------------
# Helper fixture
# ---------------------------------------------------------------------------

create_test_data <- function() {
  list(
    summary_data = data.frame(species = c("A", "B"), count = c(1, 2)),
    specimen_data = data.frame(
      processid = c("P1", "P2"),
      species = c("Homo sapiens", "Pan troglodytes"),
      genus = c("Homo", "Pan"),
      family = c("Hominidae", "Hominidae"),
      order = c("Primates", "Primates"),
      bin_uri = c("BOLD:AAA", "BOLD:BBB"),
      identified_by = c("X", "Y"),
      identification_method = c("morph", "DNA"),
      collectors = c("A", "B"),
      collection_date_start = c("2020-01-01", "2020-06-01"),
      country.ocean = c("Canada", "USA"),
      coord = c("45,-73", "40,-74"),
      institution = c("Inst A", "Inst B"),
      voucher_type = c("holotype", "vouchered"),
      quality_score = c(12, 10),
      criteria_met = c("SPECIES_ID", "SPECIES_ID"),
      nuc = c("ATCG", "GCTA"),
      stringsAsFactors = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# Excel export
# ---------------------------------------------------------------------------

test_that("ExportManager logs successful Excel exports", {
  skip_if_not_installed("writexl")

  logger <- MockLogger$new()
  manager <- ExportManager$new(logger, "test-session")
  tmpfile <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpfile))

  result <- manager$export_excel(
    create_test_data(),
    tmpfile,
    user_info = list(email = "test@example.com", name = "Test User")
  )

  expect_true(result)
  expect_true(file.exists(tmpfile))
  expect_equal(length(logger$exports), 1)
  expect_equal(logger$exports[[1]]$export_type, "excel")
  expect_true(logger$exports[[1]]$success)
})

test_that("ExportManager logs failed Excel exports", {
  logger <- MockLogger$new()
  manager <- ExportManager$new(logger, "test-session")
  tmpfile <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpfile))

  result <- manager$export_excel(list(), tmpfile)
  expect_false(result)
  expect_equal(length(logger$exports), 1)
  expect_false(logger$exports[[1]]$success)
})

# ---------------------------------------------------------------------------
# TSV export
# ---------------------------------------------------------------------------

test_that("ExportManager exports TSV with annotation columns", {
  logger <- MockLogger$new()
  manager <- ExportManager$new(logger, "test-session")
  tmpfile <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmpfile))

  annotations <- list(
    selections = list(P1 = list(selected = TRUE)),
    flags = list(P2 = list(flag = "id_uncertain")),
    notes = list(P1 = list(text = "Check"))
  )

  result <- manager$export_tsv(create_test_data(), tmpfile, annotations = annotations)
  expect_true(result)
  expect_equal(length(logger$exports), 1)
  expect_equal(logger$exports[[1]]$export_type, "tsv")
  expect_true(logger$exports[[1]]$success)
})

test_that("ExportManager returns FALSE for NULL specimen data in TSV", {
  logger <- MockLogger$new()
  manager <- ExportManager$new(logger, "test-session")
  tmpfile <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmpfile))

  result <- manager$export_tsv(list(specimen_data = NULL), tmpfile)
  expect_false(result)
})
