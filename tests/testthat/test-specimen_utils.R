# Tests for specimen handling utilities
# tests/testthat/test-specimen_utils.R

library(testthat)

test_that("calculate_specimen_score works correctly", {
  specimen <- list(
    species = "Test species",
    nuc_basecount = "650",
    bin_uri = "BOLD:123",
    voucher_type = "Holotype",
    identified_by = "John Smith",
    identification_method = "Morphology",
    collectors = "Jane Doe",
    collection_date_start = "2023-01-01",
    country.ocean = "Canada",
    site = "Test Site",
    coord = "45.4215,-75.6972",
    inst = "Museum",
    museumid = "ABC123"
  )

  score <- calculate_specimen_score(specimen)

  expect_true(is.list(score))
  expect_true(all(c("score", "percentage", "rank", "criteria_met") %in% names(score)))
  expect_true(score$score > 0)
  expect_true(score$percentage >= 0 && score$percentage <= 100)
  expect_true(score$rank >= 1 && score$rank <= 7)
})

test_that("determine_specimen_rank works correctly", {
  # Rank 1: Type specimen with species ID
  expect_equal(determine_specimen_rank(2, "SPECIES_ID; TYPE_SPECIMEN"), 1)

  # Rank 2: Full data
  criteria <- paste("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", "PUBLIC_VOUCHER",
                    "IDENTIFIER", "SITE", "COLLECTION_DATE", "COUNTRY", "COORD",
                    "COLLECTORS", sep = "; ")
  expect_equal(determine_specimen_rank(10, criteria), 2)

  # Rank 3: Basic requirements
  criteria <- paste("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", "PUBLIC_VOUCHER",
                    "IDENTIFIER", "COUNTRY", sep = "; ")
  expect_equal(determine_specimen_rank(6, criteria), 3)

  # Rank 4: Minimal requirements
  criteria <- paste("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", "COUNTRY",
                    sep = "; ")
  expect_equal(determine_specimen_rank(4, criteria), 4)

  # Rank 5: Basic data only
  criteria <- paste("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", sep = "; ")
  expect_equal(determine_specimen_rank(3, criteria), 5)

  # Rank 6: Species and sequence only
  criteria <- paste("SPECIES_ID", "SEQ_QUALITY", sep = "; ")
  expect_equal(determine_specimen_rank(2, criteria), 6)

  # Rank 7: Insufficient data
  expect_equal(determine_specimen_rank(1, "SPECIES_ID"), 7)
})

test_that("apply_specimen_ranking handles edge cases", {
  # Empty data frame
  empty_df <- data.frame()
  ranked_empty <- apply_specimen_ranking(empty_df)
  expect_equal(nrow(ranked_empty), 0)

  # Missing columns
  incomplete_df <- data.frame(
    processid = "BOLD:1",
    species = "Test species"
  )
  ranked_incomplete <- apply_specimen_ranking(incomplete_df)
  expect_true("specimen_rank" %in% names(ranked_incomplete))
  expect_true("quality_score" %in% names(ranked_incomplete))

  # NA values
  na_df <- data.frame(
    processid = "BOLD:1",
    species = NA,
    bin_uri = NA,
    stringsAsFactors = FALSE
  )
  ranked_na <- apply_specimen_ranking(na_df)
  expect_true(!is.na(ranked_na$specimen_rank))
  expect_true(!is.na(ranked_na$quality_score))
})

test_that("quality score components are weighted correctly", {
  specimen <- list(
    species = "Test species",
    nuc_basecount = "650",
    bin_uri = "BOLD:123",
    voucher_type = "Holotype",
    identified_by = "John Smith",
    identification_method = "Morphology"
  )

  score <- calculate_specimen_score(specimen)

  # Check individual criteria
  criteria_list <- strsplit(score$criteria_met, "; ")[[1]]
  expect_true("SPECIES_ID" %in% criteria_list)
  expect_true("TYPE_SPECIMEN" %in% criteria_list)
  expect_true("SEQ_QUALITY" %in% criteria_list)
  expect_true("IDENTIFIER" %in% criteria_list)
  expect_true("ID_METHOD" %in% criteria_list)
})
