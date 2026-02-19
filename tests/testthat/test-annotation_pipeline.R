# Tests for annotation persistence pipeline
# tests/testthat/test-annotation_pipeline.R
#
# These tests cover the critical path: annotations entered in BAGS tables
# must persist through to the specimen table display and exports.

library(testthat)

# Source the functions under test (they are pure, no Shiny dependency)
source("../../R/config/constants.R")
source("../../R/utils/annotation_utils.R")

# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------

create_test_data <- function(n = 5) {
  data.frame(
    processid = paste0("PROC", seq_len(n)),
    species = rep("Homo sapiens", n),
    bin_uri = rep("BOLD:AAA0001", n),
    identification = rep("Homo sapiens", n),
    identification_rank = rep("species", n),
    quality_score = seq(10, 14, length.out = n),
    rank = rep(1L, n),
    criteria_met = rep("SPECIES_ID; SEQ_QUALITY", n),
    country.ocean = rep("Canada", n),
    stringsAsFactors = FALSE
  )
}

# Flags stored as rich lists (the format written by bags_grading_server)
create_list_flags <- function() {
  list(
    PROC1 = list(flag = "misidentification", timestamp = "2025-01-01", user = "curator@test.com"),
    PROC3 = list(flag = "data_issue", timestamp = "2025-01-02", user = "curator@test.com")
  )
}

# Flags stored as bare strings (possible from older sessions)
create_bare_flags <- function() {
  list(
    PROC1 = "misidentification",
    PROC3 = "data_issue"
  )
}

# Notes stored as rich lists
create_list_notes <- function() {
  list(
    PROC2 = list(text = "Check voucher image", timestamp = "2025-01-01", user = "curator@test.com"),
    PROC4 = list(text = "Possible hybrid", timestamp = "2025-01-02", user = "curator@test.com")
  )
}

# Notes stored as bare strings
create_bare_notes <- function() {
  list(
    PROC2 = "Check voucher image",
    PROC4 = "Possible hybrid"
  )
}

create_selections <- function() {
  list(
    PROC1 = list(timestamp = "2025-01-01", species = "Homo sapiens",
                 quality_score = 14, user = "curator@test.com", selected = TRUE)
  )
}

# ---------------------------------------------------------------------------
# extract_annotation()
# ---------------------------------------------------------------------------

describe("extract_annotation", {

  it("returns default for NULL entry", {
    expect_equal(extract_annotation(NULL, "flag"), "")
    expect_equal(extract_annotation(NULL, "flag", default = "none"), "none")
  })

  it("extracts first matching field from a list", {
    entry <- list(flag = "misidentification", user = "someone")
    expect_equal(extract_annotation(entry, c("flag", "value")), "misidentification")
  })

  it("falls back through fields in order", {
    entry <- list(value = "data_issue")
    expect_equal(extract_annotation(entry, c("flag", "value")), "data_issue")
  })

  it("returns default when no fields match", {
    entry <- list(user = "someone")
    expect_equal(extract_annotation(entry, c("flag", "value")), "")
  })

  it("handles bare character strings", {
    expect_equal(extract_annotation("misidentification", "flag"), "misidentification")
  })

  it("handles bare numeric values by coercing to character", {
    expect_equal(extract_annotation(42, "flag"), "42")
  })

  it("extracts text field from notes", {
    entry <- list(text = "Check voucher", timestamp = "2025-01-01", user = "curator")
    expect_equal(extract_annotation(entry, c("text", "note", "value")), "Check voucher")
  })

  it("extracts note field as fallback", {
    entry <- list(note = "Check voucher")
    expect_equal(extract_annotation(entry, c("text", "note", "value")), "Check voucher")
  })
})

# ---------------------------------------------------------------------------
# prepare_module_data()
# ---------------------------------------------------------------------------

describe("prepare_module_data", {

  it("returns empty data frame for NULL input", {
    result <- prepare_module_data(NULL)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 0)
  })

  it("adds annotation columns with defaults when no annotations provided", {
    data <- create_test_data()
    result <- prepare_module_data(data)

    expect_true("selected" %in% names(result))
    expect_true("flag" %in% names(result))
    expect_true("curator_notes" %in% names(result))

    expect_true(all(result$selected == FALSE))
    expect_true(all(result$flag == ""))
    expect_true(all(result$curator_notes == ""))
  })

  it("merges list-format flags correctly", {
    data <- create_test_data()
    flags <- create_list_flags()
    result <- prepare_module_data(data, current_flags = flags)

    expect_equal(result$flag[1], "misidentification")  # PROC1
    expect_equal(result$flag[2], "")                     # PROC2 — no flag
    expect_equal(result$flag[3], "data_issue")           # PROC3
  })

  it("merges bare-string flags correctly", {
    data <- create_test_data()
    flags <- create_bare_flags()
    result <- prepare_module_data(data, current_flags = flags)

    expect_equal(result$flag[1], "misidentification")
    expect_equal(result$flag[3], "data_issue")
  })

  it("merges list-format notes correctly", {
    data <- create_test_data()
    notes <- create_list_notes()
    result <- prepare_module_data(data, current_notes = notes)

    expect_equal(result$curator_notes[2], "Check voucher image")
    expect_equal(result$curator_notes[4], "Possible hybrid")
    expect_equal(result$curator_notes[1], "")
  })

  it("merges bare-string notes correctly", {
    data <- create_test_data()
    notes <- create_bare_notes()
    result <- prepare_module_data(data, current_notes = notes)

    expect_equal(result$curator_notes[2], "Check voucher image")
    expect_equal(result$curator_notes[4], "Possible hybrid")
  })

  it("merges selections correctly", {
    data <- create_test_data()
    selections <- create_selections()
    result <- prepare_module_data(data, current_selections = selections)

    expect_true(result$selected[1])   # PROC1 is selected
    expect_false(result$selected[2])  # PROC2 is not
  })

  it("handles all annotation types simultaneously", {
    data <- create_test_data()
    result <- prepare_module_data(
      data,
      current_selections = create_selections(),
      current_flags = create_list_flags(),
      current_notes = create_list_notes()
    )

    expect_true(result$selected[1])
    expect_equal(result$flag[1], "misidentification")
    expect_equal(result$curator_notes[2], "Check voucher image")
  })

  it("returns correct column types", {
    data <- create_test_data()
    result <- prepare_module_data(
      data,
      current_selections = create_selections(),
      current_flags = create_list_flags(),
      current_notes = create_list_notes()
    )

    expect_type(result$selected, "logical")
    expect_type(result$flag, "character")
    expect_type(result$curator_notes, "character")
    expect_type(result$quality_score, "double")
  })

  it("handles processids not present in annotations gracefully", {
    data <- create_test_data()
    flags <- list(NONEXISTENT = list(flag = "data_issue"))
    result <- prepare_module_data(data, current_flags = flags)

    expect_true(all(result$flag == ""))
  })

  it("handles empty annotation lists", {
    data <- create_test_data()
    result <- prepare_module_data(data,
      current_selections = list(),
      current_flags = list(),
      current_notes = list()
    )

    expect_true(all(result$selected == FALSE))
    expect_true(all(result$flag == ""))
    expect_true(all(result$curator_notes == ""))
  })
})

# ---------------------------------------------------------------------------
# merge_annotations_for_export()
# ---------------------------------------------------------------------------

describe("merge_annotations_for_export", {

  it("returns input unchanged for NULL data", {
    expect_null(merge_annotations_for_export(NULL))
  })

  it("adds all export columns", {
    data <- create_test_data()
    result <- merge_annotations_for_export(
      data,
      selections = create_selections(),
      flags = create_list_flags(),
      notes = create_list_notes()
    )

    expect_true("selected" %in% names(result))
    expect_true("flag" %in% names(result))
    expect_true("curator_notes" %in% names(result))
    expect_true("flag_user" %in% names(result))
    expect_true("flag_timestamp" %in% names(result))
  })

  it("exports flag audit trail", {
    data <- create_test_data()
    result <- merge_annotations_for_export(
      data, flags = create_list_flags()
    )

    expect_equal(result$flag_user[1], "curator@test.com")
    expect_equal(result$flag_timestamp[1], "2025-01-01")
    expect_equal(result$flag_user[2], "")  # no flag
  })

  it("exports selected as logical", {
    data <- create_test_data()
    result <- merge_annotations_for_export(
      data, selections = create_selections()
    )

    expect_type(result$selected, "logical")
    expect_true(result$selected[1])
    expect_false(result$selected[2])
  })

  it("handles bare-string annotations in export", {
    data <- create_test_data()
    result <- merge_annotations_for_export(
      data,
      flags = create_bare_flags(),
      notes = create_bare_notes()
    )

    expect_equal(result$flag[1], "misidentification")
    expect_equal(result$curator_notes[2], "Check voucher image")
    # Bare strings have no user/timestamp metadata
    expect_equal(result$flag_user[1], "")
    expect_equal(result$flag_timestamp[1], "")
  })

  it("handles NULL annotation arguments", {
    data <- create_test_data()
    result <- merge_annotations_for_export(data)

    expect_true(all(result$selected == FALSE))
    expect_true(all(result$flag == ""))
    expect_true(all(result$curator_notes == ""))
    expect_true(all(result$flag_user == ""))
    expect_true(all(result$flag_timestamp == ""))
  })

  it("is idempotent — running twice gives same result", {
    data <- create_test_data()
    flags <- create_list_flags()
    notes <- create_list_notes()
    sels <- create_selections()

    result1 <- merge_annotations_for_export(data, sels, flags, notes)
    result2 <- merge_annotations_for_export(result1, sels, flags, notes)

    expect_equal(result1$flag, result2$flag)
    expect_equal(result1$curator_notes, result2$curator_notes)
    expect_equal(result1$selected, result2$selected)
  })
})

# ---------------------------------------------------------------------------
# Round-trip: prepare → format → export
# ---------------------------------------------------------------------------

describe("annotation round-trip", {

  it("annotations survive prepare → export pipeline", {
    data <- create_test_data()
    flags <- create_list_flags()
    notes <- create_list_notes()
    sels <- create_selections()

    # Step 1: prepare (as done for table display)
    prepared <- prepare_module_data(data, sels, flags, notes)

    # Step 2: export from original data + state (as done by download handlers)
    exported <- merge_annotations_for_export(data, sels, flags, notes)

    # Annotations should match between display and export
    expect_equal(prepared$flag, exported$flag)
    expect_equal(prepared$curator_notes, exported$curator_notes)
    # selected column: prepare uses logical, export uses logical
    expect_equal(prepared$selected, exported$selected)
  })

  it("mixed format annotations (list + bare) produce consistent results", {
    data <- create_test_data()
    # Mix formats: PROC1 as list, PROC3 as bare string
    flags_mixed <- list(
      PROC1 = list(flag = "misidentification", user = "curator@test.com"),
      PROC3 = "data_issue"
    )

    result_prepare <- prepare_module_data(data, current_flags = flags_mixed)
    result_export <- merge_annotations_for_export(data, flags = flags_mixed)

    expect_equal(result_prepare$flag, result_export$flag)
    expect_equal(result_prepare$flag[1], "misidentification")
    expect_equal(result_prepare$flag[3], "data_issue")
  })
})
