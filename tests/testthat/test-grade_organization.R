# Tests for BAGS grade specimen organization
# tests/testthat/test-grade_organization.R
#
# organize_grade_specimens() groups specimens for display in grade-specific
# tables. The grouping logic is complex for grades C (species×BIN) and
# E (shared BINs), and wrong grouping means curators see the wrong
# specimens together.

library(testthat)

source("../../R/modules/bags_grading/mod_bags_grading_utils.R")

# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------

make_grade_data <- function(species_bins) {
  rows <- lapply(names(species_bins), function(sp) {
    bins <- species_bins[[sp]]
    data.frame(
      processid = paste0("P_", gsub(" ", "_", sp), "_", seq_along(bins)),
      species = rep(sp, length(bins)),
      bin_uri = bins,
      quality_score = rep(10, length(bins)),
      identification = rep(sp, length(bins)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Grade A — group by species
# ---------------------------------------------------------------------------

describe("organize_grade_specimens — Grade A", {

  it("groups by species", {
    data <- make_grade_data(list(
      "Species A" = rep("BIN1", 12),
      "Species B" = rep("BIN2", 15)
    ))
    result <- organize_grade_specimens(data, "A")

    expect_equal(length(result), 2)
    expect_true("Species A" %in% names(result))
    expect_true("Species B" %in% names(result))
  })

  it("attaches species info as attribute", {
    data <- make_grade_data(list("Species A" = rep("BIN1", 12)))
    result <- organize_grade_specimens(data, "A")

    info <- attr(result[["Species A"]], "info")
    expect_equal(info$species, "Species A")
    expect_equal(info$specimen_count, 12)
  })

  it("sorts specimens by quality_score descending", {
    data <- data.frame(
      processid = c("P1", "P2", "P3"),
      species = rep("Sp A", 3),
      bin_uri = rep("BIN1", 3),
      quality_score = c(5, 12, 8),
      stringsAsFactors = FALSE
    )
    result <- organize_grade_specimens(data, "A")
    expect_equal(result[["Sp A"]]$quality_score, c(12, 8, 5))
  })

  it("returns empty list for empty input", {
    result <- organize_grade_specimens(data.frame(), "A")
    expect_equal(length(result), 0)
  })
})

# ---------------------------------------------------------------------------
# Grade B — same as A
# ---------------------------------------------------------------------------

describe("organize_grade_specimens — Grade B", {

  it("groups by species like A", {
    data <- make_grade_data(list("Sp" = rep("BIN1", 5)))
    result <- organize_grade_specimens(data, "B")
    expect_equal(length(result), 1)
    expect_equal(attr(result[["Sp"]], "info")$specimen_count, 5)
  })
})

# ---------------------------------------------------------------------------
# Grade C — group by species then BIN
# ---------------------------------------------------------------------------

describe("organize_grade_specimens — Grade C", {

  it("creates separate groups for each species×BIN combination", {
    data <- make_grade_data(list(
      "Species A" = c("BIN1", "BIN1", "BIN2")
    ))
    result <- organize_grade_specimens(data, "C")

    # Should have 2 groups: Species A_BIN1 and Species A_BIN2
    expect_equal(length(result), 2)
  })

  it("attaches both species and bin in info", {
    data <- make_grade_data(list(
      "Species A" = c("BIN1", "BIN2")
    ))
    result <- organize_grade_specimens(data, "C")

    # Check one group's info
    first_info <- attr(result[[1]], "info")
    expect_true(!is.null(first_info$species))
    expect_true(!is.null(first_info$bin))
    expect_true(!is.null(first_info$specimen_count))
  })

  it("handles multiple species with multiple BINs", {
    data <- make_grade_data(list(
      "Sp A" = c("BIN1", "BIN2"),
      "Sp B" = c("BIN3", "BIN4", "BIN4")
    ))
    result <- organize_grade_specimens(data, "C")

    # Sp A: 2 groups (BIN1, BIN2), Sp B: 2 groups (BIN3, BIN4)
    expect_equal(length(result), 4)
  })
})

# ---------------------------------------------------------------------------
# Grade D — group by species
# ---------------------------------------------------------------------------

describe("organize_grade_specimens — Grade D", {

  it("groups by species", {
    data <- make_grade_data(list(
      "Rare Sp" = c("BIN1", "BIN1")
    ))
    result <- organize_grade_specimens(data, "D")
    expect_equal(length(result), 1)
    expect_equal(attr(result[["Rare Sp"]], "info")$specimen_count, 2)
  })
})

# ---------------------------------------------------------------------------
# Grade E — group by shared BINs
# ---------------------------------------------------------------------------

describe("organize_grade_specimens — Grade E", {

  it("groups by BIN, only includes BINs with multiple species", {
    data <- make_grade_data(list(
      "Sp A" = c("SHARED_BIN", "UNIQUE_BIN_A"),
      "Sp B" = c("SHARED_BIN", "UNIQUE_BIN_B")
    ))
    result <- organize_grade_specimens(data, "E")

    # Only SHARED_BIN group (has 2 species); unique BINs excluded
    expect_equal(length(result), 1)
    expect_equal(names(result)[1], "SHARED_BIN")
  })

  it("attaches species count and species list in info", {
    data <- make_grade_data(list(
      "Alpha" = c("BIN_SHARED"),
      "Beta"  = c("BIN_SHARED"),
      "Gamma" = c("BIN_SHARED")
    ))
    result <- organize_grade_specimens(data, "E")

    info <- attr(result[["BIN_SHARED"]], "info")
    expect_equal(info$species_count, 3)
    expect_true(grepl("Alpha", info$species))
    expect_true(grepl("Beta", info$species))
    expect_true(grepl("Gamma", info$species))
  })

  it("returns empty list when no BINs are shared", {
    data <- make_grade_data(list(
      "Sp A" = c("BIN_A"),
      "Sp B" = c("BIN_B")
    ))
    result <- organize_grade_specimens(data, "E")
    expect_equal(length(result), 0)
  })

  it("handles multiple shared BINs", {
    data <- make_grade_data(list(
      "Sp A" = c("SHARED1", "SHARED2"),
      "Sp B" = c("SHARED1", "SHARED2")
    ))
    result <- organize_grade_specimens(data, "E")
    expect_equal(length(result), 2)
  })
})

# ---------------------------------------------------------------------------
# generate_table_caption()
# ---------------------------------------------------------------------------

describe("generate_table_caption", {

  it("generates correct caption for each grade", {
    expect_true(grepl(">10 specimens", generate_table_caption("A",
      list(species = "Sp A"))))
    expect_true(grepl("3-10 specimens", generate_table_caption("B",
      list(species = "Sp B"))))
    expect_true(grepl("BIN:", generate_table_caption("C",
      list(species = "Sp C", bin = "BIN1"))))
    expect_true(grepl("<3 specimens", generate_table_caption("D",
      list(species = "Sp D"))))
    expect_true(grepl("Shared BIN", generate_table_caption("E",
      list(bin = "BIN1", species_count = 3))))
  })
})
