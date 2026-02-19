# Tests for BAGS grading algorithm
# tests/testthat/test-bags_grading_algorithm.R
#
# The BAGS grading algorithm is the core scientific function of the app.
# Wrong grades (e.g. A when it should be D) mislabel species quality
# and undermine the entire curation workflow.

library(testthat)

source("../../R/utils/bags_grading.R")

# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------

# Build specimen data frames for specific scenarios
make_specimens <- function(species_bins) {
  # species_bins: named list where name = species, value = vector of bin_uris
  # e.g. list("Homo sapiens" = rep("BOLD:AAA0001", 12))
  rows <- lapply(names(species_bins), function(sp) {
    bins <- species_bins[[sp]]
    data.frame(
      processid = paste0("PROC_", sp, "_", seq_along(bins)),
      species = rep(sp, length(bins)),
      bin_uri = bins,
      identification_rank = rep("species", length(bins)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# determine_bags_grade()
# ---------------------------------------------------------------------------

describe("determine_bags_grade", {

  it("returns A for >= 11 specimens, single BIN, no shared BINs", {
    expect_equal(determine_bags_grade(11, 1, FALSE), "A")
    expect_equal(determine_bags_grade(50, 1, FALSE), "A")
  })

  it("returns B for 3-10 specimens, single BIN, no shared BINs", {
    expect_equal(determine_bags_grade(3, 1, FALSE), "B")
    expect_equal(determine_bags_grade(10, 1, FALSE), "B")
  })

  it("returns C for multiple BINs, no shared BINs", {
    expect_equal(determine_bags_grade(5, 2, FALSE), "C")
    expect_equal(determine_bags_grade(20, 3, FALSE), "C")
  })

  it("returns D for < 3 specimens, single BIN, no shared BINs", {
    expect_equal(determine_bags_grade(1, 1, FALSE), "D")
    expect_equal(determine_bags_grade(2, 1, FALSE), "D")
  })

  it("returns E for shared BINs regardless of count", {
    expect_equal(determine_bags_grade(50, 1, TRUE), "E")
    expect_equal(determine_bags_grade(1, 1, TRUE), "E")
  })

  it("returns E for NA inputs", {
    expect_equal(determine_bags_grade(NA, 1, FALSE), "E")
    expect_equal(determine_bags_grade(5, NA, FALSE), "E")
  })

  it("returns E for negative counts", {
    expect_equal(determine_bags_grade(-1, 1, FALSE), "E")
    expect_equal(determine_bags_grade(5, -1, FALSE), "E")
  })

  it("handles boundary between B and A (10 vs 11)", {
    expect_equal(determine_bags_grade(10, 1, FALSE), "B")
    expect_equal(determine_bags_grade(11, 1, FALSE), "A")
  })

  it("handles boundary between D and B (2 vs 3)", {
    expect_equal(determine_bags_grade(2, 1, FALSE), "D")
    expect_equal(determine_bags_grade(3, 1, FALSE), "B")
  })

  it("C takes priority over specimen count thresholds", {
    # Multiple BINs = grade C regardless of specimen count
    expect_equal(determine_bags_grade(1, 2, FALSE), "C")
    expect_equal(determine_bags_grade(50, 5, FALSE), "C")
  })

  it("E takes priority over everything", {
    # Shared BINs = grade E regardless of other criteria
    expect_equal(determine_bags_grade(50, 1, TRUE), "E")
    expect_equal(determine_bags_grade(50, 3, TRUE), "E")
  })
})

# ---------------------------------------------------------------------------
# check_shared_bins()
# ---------------------------------------------------------------------------

describe("check_shared_bins", {

  it("detects shared BINs across species", {
    specimens <- make_specimens(list(
      "Species A" = c("BIN1", "BIN1"),
      "Species B" = c("BIN1", "BIN2")
    ))
    expect_true(check_shared_bins(specimens, "Species A", "BIN1"))
    expect_true(check_shared_bins(specimens, "Species B", c("BIN1", "BIN2")))
  })

  it("returns FALSE when BINs are not shared", {
    specimens <- make_specimens(list(
      "Species A" = c("BIN1", "BIN1"),
      "Species B" = c("BIN2", "BIN2")
    ))
    expect_false(check_shared_bins(specimens, "Species A", "BIN1"))
  })

  it("returns FALSE for empty BIN list", {
    specimens <- make_specimens(list("Species A" = "BIN1"))
    expect_false(check_shared_bins(specimens, "Species A", character(0)))
  })

  it("handles NA BIN values", {
    specimens <- make_specimens(list("Species A" = c("BIN1", NA)))
    expect_false(check_shared_bins(specimens, "Species A", "BIN1"))
  })
})

# ---------------------------------------------------------------------------
# calculate_bags_grade() — full integration
# ---------------------------------------------------------------------------

describe("calculate_bags_grade", {

  it("assigns grade A to species with >= 11 specimens and 1 BIN", {
    specimens <- make_specimens(list(
      "Homo sapiens" = rep("BOLD:AAA0001", 12)
    ))
    result <- calculate_bags_grade(specimens)
    expect_equal(nrow(result), 1)
    expect_equal(result$bags_grade, "A")
    expect_equal(result$specimen_count, 12)
    expect_equal(result$bin_count, 1)
  })

  it("assigns grade B to species with 3-10 specimens and 1 BIN", {
    specimens <- make_specimens(list(
      "Felis catus" = rep("BOLD:BBB0001", 5)
    ))
    result <- calculate_bags_grade(specimens)
    expect_equal(result$bags_grade, "B")
  })

  it("assigns grade C to species with multiple BINs", {
    specimens <- make_specimens(list(
      "Canis lupus" = c("BOLD:CCC0001", "BOLD:CCC0001", "BOLD:CCC0002")
    ))
    result <- calculate_bags_grade(specimens)
    expect_equal(result$bags_grade, "C")
    expect_equal(result$bin_count, 2)
  })

  it("assigns grade D to species with < 3 specimens", {
    specimens <- make_specimens(list(
      "Rattus rattus" = c("BOLD:DDD0001", "BOLD:DDD0001")
    ))
    result <- calculate_bags_grade(specimens)
    expect_equal(result$bags_grade, "D")
    expect_equal(result$specimen_count, 2)
  })

  it("assigns grade E to species with shared BINs", {
    specimens <- make_specimens(list(
      "Species X" = c("SHARED_BIN", "SHARED_BIN"),
      "Species Y" = c("SHARED_BIN", "OTHER_BIN")
    ))
    result <- calculate_bags_grade(specimens)
    # Both species should get E because SHARED_BIN contains multiple species
    expect_true(all(result$bags_grade[result$species %in% c("Species X", "Species Y")] == "E"))
  })

  it("handles multiple species with different grades", {
    specimens <- make_specimens(list(
      "Grade A species" = rep("BIN_A", 15),
      "Grade B species" = rep("BIN_B", 5),
      "Grade D species" = c("BIN_D")
    ))
    result <- calculate_bags_grade(specimens)
    expect_equal(nrow(result), 3)

    a_row <- result[result$species == "Grade A species", ]
    b_row <- result[result$species == "Grade B species", ]
    d_row <- result[result$species == "Grade D species", ]

    expect_equal(a_row$bags_grade, "A")
    expect_equal(b_row$bags_grade, "B")
    expect_equal(d_row$bags_grade, "D")
  })

  it("filters out non-species-level identifications", {
    specimens <- data.frame(
      processid = c("P1", "P2", "P3"),
      species = c("Homo sapiens", "Homo sapiens", "Homo"),
      bin_uri = c("BIN1", "BIN1", "BIN1"),
      identification_rank = c("species", "species", "genus"),
      stringsAsFactors = FALSE
    )
    result <- calculate_bags_grade(specimens)
    # Only species-level records counted
    expect_equal(result$specimen_count, 2)
  })

  it("handles empty input", {
    empty <- data.frame(
      processid = character(0),
      species = character(0),
      bin_uri = character(0),
      identification_rank = character(0),
      stringsAsFactors = FALSE
    )
    result <- calculate_bags_grade(empty)
    expect_equal(nrow(result), 0)
  })

  it("ignores specimens with NA or empty species", {
    specimens <- data.frame(
      processid = c("P1", "P2", "P3"),
      species = c("Homo sapiens", NA, ""),
      bin_uri = c("BIN1", "BIN1", "BIN1"),
      identification_rank = c("species", "species", "species"),
      stringsAsFactors = FALSE
    )
    result <- calculate_bags_grade(specimens)
    expect_equal(nrow(result), 1)
    expect_equal(result$specimen_count, 1)
  })

  it("ignores specimens with NA or empty BINs when counting", {
    specimens <- make_specimens(list(
      "Homo sapiens" = c("BIN1", "BIN1", NA, "")
    ))
    result <- calculate_bags_grade(specimens)
    # bin_count should only count the one valid BIN
    expect_equal(result$bin_count, 1)
    # specimen_count includes all 4 because they have species IDs
    expect_equal(result$specimen_count, 4)
  })
})

# ---------------------------------------------------------------------------
# validate_bags_grades()
# ---------------------------------------------------------------------------

describe("validate_bags_grades", {

  it("validates correct grades", {
    grades <- data.frame(
      species = "Test",
      bags_grade = "A",
      specimen_count = 15,
      bin_count = 1,
      shared_bins = FALSE,
      stringsAsFactors = FALSE
    )
    result <- validate_bags_grades(grades)
    expect_true(result$valid)
  })

  it("detects invalid grade values", {
    grades <- data.frame(
      species = "Test",
      bags_grade = "X",  # invalid
      specimen_count = 5,
      bin_count = 1,
      shared_bins = FALSE,
      stringsAsFactors = FALSE
    )
    result <- validate_bags_grades(grades)
    expect_false(result$valid)
  })

  it("detects negative specimen counts", {
    grades <- data.frame(
      species = "Test",
      bags_grade = "A",
      specimen_count = -1,
      bin_count = 1,
      shared_bins = FALSE,
      stringsAsFactors = FALSE
    )
    result <- validate_bags_grades(grades)
    expect_false(result$valid)
  })

  it("rejects NULL input", {
    result <- validate_bags_grades(NULL)
    expect_false(result$valid)
  })
})

# ---------------------------------------------------------------------------
# create_empty_grades_df()
# ---------------------------------------------------------------------------

describe("create_empty_grades_df", {

  it("returns data frame with correct structure", {
    result <- create_empty_grades_df()
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 0)
    expect_true(all(c("species", "bags_grade", "specimen_count",
                       "bin_count", "shared_bins") %in% names(result)))
  })
})
