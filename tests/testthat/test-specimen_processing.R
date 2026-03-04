# Tests for specimen processing pipeline
# tests/testthat/test-specimen_processing.R
#
# The pipeline (validator → scorer → processor) determines quality scores
# and rankings that drive the entire curation workflow.

library(testthat)
library(R6)

source("../../R/config/constants.R")
source("../../R/utils/ErrorBoundary.R")
source("../../R/modules/specimen_handling/specimen_validator.R")
source("../../R/modules/specimen_handling/specimen_scorer.R")
source("../../R/modules/specimen_handling/specimen_processor.R")

# ---------------------------------------------------------------------------
# Mock logger
# ---------------------------------------------------------------------------

MockLogger <- R6::R6Class("MockLogger",
  public = list(
    messages = list(),
    info = function(msg, ...) {
      self$messages[[length(self$messages) + 1]] <- list(level = "info", msg = msg)
    },
    warn = function(msg, ...) {
      self$messages[[length(self$messages) + 1]] <- list(level = "warn", msg = msg)
    },
    error = function(msg, ...) {
      self$messages[[length(self$messages) + 1]] <- list(level = "error", msg = msg)
    }
  )
)

# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------

create_test_specimens <- function() {
  data.frame(
    processid = c("P1", "P2", "P3", "P4"),
    species = c("Homo sapiens", "Pan troglodytes", "Rattus sp.", ""),
    identification_rank = c("species", "species", "species", "genus"),
    bin_uri = c("BOLD:AAA0001", "BOLD:AAA0002", "", NA),
    nuc_basecount = c(658, 500, 300, 100),
    nuc = c("ATCG", "GCTA", "", NA),
    voucher_type = c("holotype", "vouchered", "none", "none"),
    institution = c("Museum A", "Museum B", "", ""),
    inst = c("Museum A", "Museum B", "", ""),
    identified_by = c("Expert A", "Expert B", "", ""),
    country.ocean = c("Canada", "USA", "", ""),
    coord = c("45.5,-73.5", "", "", ""),
    collectors = c("Coll A", "", "", ""),
    collection_date_start = c("2020-01-01", "", "", ""),
    collection_date_end = c("2020-01-02", "", "", ""),
    identification_method = c("morphology", "", "", ""),
    sector = c("Sector A", "", "", ""),
    region = c("Region A", "", "", ""),
    museumid = c("MUS001", "", "", ""),
    site = c("Site A", "", "", ""),
    taxonomy_notes = c("", "", "", ""),
    short_note = c("", "", "", ""),
    collection_notes = c("", "", "", ""),
    notes = c("", "", "", ""),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# SpecimenValidator
# ---------------------------------------------------------------------------

describe("SpecimenValidator", {

  it("flags valid species names", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    specimens <- create_test_specimens()

    result <- validator$validate_specimens(specimens)

    # "Homo sapiens" — valid
    expect_true(result$valid_species[1])
    # "Pan troglodytes" — valid
    expect_true(result$valid_species[2])
    # "Rattus sp." — invalid (contains sp.)
    expect_false(result$valid_species[3])
    # "" — invalid (empty)
    expect_false(result$valid_species[4])
  })

  it("flags species-level IDs", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    specimens <- create_test_specimens()

    result <- validator$validate_specimens(specimens)
    expect_true(result$species_id[1])
    expect_true(result$species_id[2])
    expect_true(result$species_id[3])   # has species text even if invalid name
    expect_false(result$species_id[4])  # empty species
  })

  it("flags valid BINs", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    specimens <- create_test_specimens()

    result <- validator$validate_specimens(specimens)
    expect_true(result$valid_bin[1])
    expect_true(result$valid_bin[2])
    expect_false(result$valid_bin[3])  # empty bin
    expect_false(result$valid_bin[4])  # NA bin
  })

  it("returns NULL for empty input", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    result <- validator$validate_specimens(data.frame())
    expect_null(result)
  })

  it("returns NULL for NULL input", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    result <- validator$validate_specimens(NULL)
    expect_null(result)
  })
})

# ---------------------------------------------------------------------------
# SpecimenScorer
# ---------------------------------------------------------------------------

describe("SpecimenScorer", {

  it("scores specimens with quality_score column", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- create_test_specimens()

    result <- scorer$score_specimens(specimens)
    expect_true("quality_score" %in% names(result))
    expect_true("criteria_met" %in% names(result))
    expect_type(result$quality_score, "double")
  })

  it("assigns higher scores to better specimens", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- create_test_specimens()

    result <- scorer$score_specimens(specimens)
    # P1 has species ID, BIN, sequence, holotype, institution, collector, etc.
    # P4 has nothing useful
    expect_true(result$quality_score[1] > result$quality_score[4])
  })

  it("records which criteria are met", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- create_test_specimens()

    result <- scorer$score_specimens(specimens)
    # P1 should meet SPECIES_ID criterion at minimum
    expect_true(grepl("SPECIES_ID", result$criteria_met[1]))
  })

  it("handles all-NA specimen gracefully", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "EMPTY",
      species = NA,
      identification_rank = NA,
      bin_uri = NA,
      nuc_basecount = NA,
      nuc = NA,
      voucher_type = NA,
      institution = NA,
      inst = NA,
      identified_by = NA,
      country.ocean = NA,
      coord = NA,
      collectors = NA,
      collection_date_start = NA,
      collection_date_end = NA,
      identification_method = NA,
      sector = NA,
      region = NA,
      museumid = NA,
      site = NA,
      taxonomy_notes = NA,
      short_note = NA,
      collection_notes = NA,
      notes = NA,
      stringsAsFactors = FALSE
    )

    result <- scorer$score_specimens(specimens)
    expect_equal(nrow(result), 1)
    expect_true(result$quality_score[1] >= 0)
  })
})

# ---------------------------------------------------------------------------
# SpecimenScorer — None/NA string handling
# ---------------------------------------------------------------------------

describe("SpecimenScorer — None/NA handling", {

  it("treats 'None' as empty for scoring", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "NONE_TEST",
      species = "None",
      identification_rank = "species",
      bin_uri = "None",
      nuc_basecount = 658,
      nuc = "ATCG",
      voucher_type = "None",
      institution = "None",
      inst = "None",
      identified_by = "None",
      country.ocean = "None",
      coord = "None",
      collectors = "None",
      collection_date_start = "None",
      collection_date_end = "None",
      identification_method = "None",
      sector = "None",
      region = "None",
      museumid = "None",
      site = "None",
      taxonomy_notes = "", short_note = "", collection_notes = "", notes = "",
      stringsAsFactors = FALSE
    )

    result <- scorer$score_specimens(specimens)
    # "None" values should all be treated as empty — score should be 0
    expect_equal(result$quality_score[1], 0)
  })

  it("treats 'NA' string as empty for scoring", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "NA_TEST",
      species = "NA",
      identification_rank = "species",
      bin_uri = "NA",
      nuc_basecount = 658,
      nuc = "ATCG",
      voucher_type = "NA",
      institution = "NA",
      inst = "NA",
      identified_by = "NA",
      country.ocean = "NA",
      coord = "NA",
      collectors = "NA",
      collection_date_start = "NA",
      collection_date_end = "NA",
      identification_method = "NA",
      sector = "NA",
      region = "NA",
      museumid = "NA",
      site = "NA",
      taxonomy_notes = "", short_note = "", collection_notes = "", notes = "",
      stringsAsFactors = FALSE
    )

    result <- scorer$score_specimens(specimens)
    expect_equal(result$quality_score[1], 0)
  })
})

# ---------------------------------------------------------------------------
# SpecimenScorer — ID_METHOD 4-way logic
# ---------------------------------------------------------------------------

describe("SpecimenScorer — ID_METHOD", {

  it("passes for pure morphological identification", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "IDM1",
      species = "Homo sapiens",
      identification_rank = "species",
      bin_uri = "BOLD:AAA0001", nuc_basecount = 658, nuc = "ATCG",
      voucher_type = "museum", institution = "Museum", inst = "Museum",
      identified_by = "Expert", country.ocean = "Canada", coord = "45,-73",
      collectors = "Coll", collection_date_start = "2020-01-01",
      collection_date_end = "2020-01-02",
      identification_method = "morphology",
      sector = "A", region = "B", museumid = "M1", site = "S1",
      taxonomy_notes = "", short_note = "", collection_notes = "", notes = "",
      stringsAsFactors = FALSE
    )
    result <- scorer$score_specimens(specimens)
    expect_true(grepl("ID_METHOD", result$criteria_met[1]))
  })

  it("fails for molecular-only identification", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "IDM2",
      species = "Homo sapiens",
      identification_rank = "species",
      bin_uri = "BOLD:AAA0001", nuc_basecount = 658, nuc = "ATCG",
      voucher_type = "museum", institution = "Museum", inst = "Museum",
      identified_by = "Expert", country.ocean = "Canada", coord = "45,-73",
      collectors = "Coll", collection_date_start = "2020-01-01",
      collection_date_end = "2020-01-02",
      identification_method = "DNA barcoding",
      sector = "A", region = "B", museumid = "M1", site = "S1",
      taxonomy_notes = "", short_note = "", collection_notes = "", notes = "",
      stringsAsFactors = FALSE
    )
    result <- scorer$score_specimens(specimens)
    expect_false(grepl("ID_METHOD", result$criteria_met[1]))
  })

  it("fails for mixed identification (morphology AND DNA)", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "IDM3",
      species = "Homo sapiens",
      identification_rank = "species",
      bin_uri = "BOLD:AAA0001", nuc_basecount = 658, nuc = "ATCG",
      voucher_type = "museum", institution = "Museum", inst = "Museum",
      identified_by = "Expert", country.ocean = "Canada", coord = "45,-73",
      collectors = "Coll", collection_date_start = "2020-01-01",
      collection_date_end = "2020-01-02",
      identification_method = "morphology and DNA barcode",
      sector = "A", region = "B", museumid = "M1", site = "S1",
      taxonomy_notes = "", short_note = "", collection_notes = "", notes = "",
      stringsAsFactors = FALSE
    )
    result <- scorer$score_specimens(specimens)
    expect_false(grepl("ID_METHOD", result$criteria_met[1]))
  })

  it("passes for unknown method (no negative match)", {
    logger <- MockLogger$new()
    scorer <- SpecimenScorer$new(logger)
    specimens <- data.frame(
      processid = "IDM4",
      species = "Homo sapiens",
      identification_rank = "species",
      bin_uri = "BOLD:AAA0001", nuc_basecount = 658, nuc = "ATCG",
      voucher_type = "museum", institution = "Museum", inst = "Museum",
      identified_by = "Expert", country.ocean = "Canada", coord = "45,-73",
      collectors = "Coll", collection_date_start = "2020-01-01",
      collection_date_end = "2020-01-02",
      identification_method = "some unknown method",
      sector = "A", region = "B", museumid = "M1", site = "S1",
      taxonomy_notes = "", short_note = "", collection_notes = "", notes = "",
      stringsAsFactors = FALSE
    )
    result <- scorer$score_specimens(specimens)
    expect_true(grepl("ID_METHOD", result$criteria_met[1]))
  })
})

# ---------------------------------------------------------------------------
# SpecimenProcessor (full pipeline integration)
# ---------------------------------------------------------------------------

describe("SpecimenProcessor", {

  it("runs the full pipeline: validate → score → rank", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    result <- processor$process_specimens(specimens)

    expect_false(is.null(result))
    expect_true("quality_score" %in% names(result))
    expect_true("rank" %in% names(result))
    expect_true("criteria_met" %in% names(result))
    expect_true("valid_species" %in% names(result))
    expect_true("valid_bin" %in% names(result))
  })

  it("assigns integer ranks between 1 and 7", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    result <- processor$process_specimens(specimens)

    expect_type(result$rank, "integer")
    expect_true(all(result$rank >= 1 & result$rank <= 7))
  })

  it("fills NA scores with 0 and NA ranks with 7", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    result <- processor$process_specimens(specimens)

    expect_false(any(is.na(result$quality_score)))
    expect_false(any(is.na(result$rank)))
  })

  it("returns NULL for empty input", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    result <- processor$process_specimens(data.frame())
    expect_null(result)
  })

  it("provides metrics after processing", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    processor$process_specimens(specimens)

    metrics <- processor$get_metrics()
    expect_false(is.null(metrics))
    expect_true(!is.null(metrics$total_specimens))
    expect_true(!is.null(metrics$avg_quality_score))
    expect_true(!is.null(metrics$rank_distribution))
  })

  it("skips image API call when has_image column already exists", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    specimens$has_image <- c(TRUE, FALSE, FALSE, TRUE)

    result <- processor$process_specimens(specimens)

    # Verify the existing has_image values are preserved
    expect_equal(result$has_image[result$processid == "P1"], TRUE)
    expect_equal(result$has_image[result$processid == "P2"], FALSE)
    expect_equal(result$has_image[result$processid == "P4"], TRUE)

    # Verify the log says "already present", not "Checking image availability"
    skip_msgs <- Filter(function(m) grepl("already present", m$msg), logger$messages)
    expect_true(length(skip_msgs) > 0)

    check_msgs <- Filter(function(m) grepl("Checking image availability", m$msg), logger$messages)
    expect_equal(length(check_msgs), 0)
  })

  it("runs image check when has_image column is absent", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    # No has_image column — should trigger the API check

    result <- processor$process_specimens(specimens)

    # Verify has_image column was added
    expect_true("has_image" %in% names(result))

    # Verify the log says "Checking image availability"
    check_msgs <- Filter(function(m) grepl("Checking image availability", m$msg), logger$messages)
    expect_true(length(check_msgs) > 0)
  })

  it("runs image check when has_image contains NAs", {
    logger <- MockLogger$new()
    validator <- SpecimenValidator$new(logger)
    scorer <- SpecimenScorer$new(logger)
    processor <- SpecimenProcessor$new(validator, scorer, logger)

    specimens <- create_test_specimens()
    specimens$has_image <- c(TRUE, NA, FALSE, NA)

    result <- processor$process_specimens(specimens)

    # NAs indicate incomplete check — should trigger fresh API call
    check_msgs <- Filter(function(m) grepl("Checking image availability", m$msg), logger$messages)
    expect_true(length(check_msgs) > 0)

    # After processing, no NAs should remain
    expect_false(any(is.na(result$has_image)))
  })
})
