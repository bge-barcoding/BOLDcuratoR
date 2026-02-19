# Tests for the export pipeline with annotations
# tests/testthat/test-export_pipeline.R
#
# Exports are the deliverable curators produce. If annotations are
# missing or columns are wrong, the curation effort is wasted.

library(testthat)
library(R6)

source("../../R/config/constants.R")
source("../../R/utils/annotation_utils.R")

# We test the ExportManager directly — it's an R6 class that calls
# merge_annotations_for_export, selects columns, and writes files.
# We mock the logger since it depends on a database.

# ---------------------------------------------------------------------------
# Mock logger
# ---------------------------------------------------------------------------

MockExportLogger <- R6::R6Class("MockExportLogger",
  public = list(
    exports = list(),
    log_export = function(...) {
      self$exports[[length(self$exports) + 1]] <- list(...)
    },
    get_export_history = function(...) data.frame(),
    get_export_stats = function(...) list(),
    info = function(...) invisible(NULL),
    warn = function(...) invisible(NULL),
    error = function(...) invisible(NULL)
  )
)

# Source the export module (needs R6 and writexl)
source("../../R/modules/export/mod_export.R")

# ---------------------------------------------------------------------------
# Test fixtures
# ---------------------------------------------------------------------------

create_specimen_data <- function() {
  data.frame(
    processid = c("P1", "P2", "P3"),
    species = c("Homo sapiens", "Homo sapiens", "Pan troglodytes"),
    genus = c("Homo", "Homo", "Pan"),
    family = c("Hominidae", "Hominidae", "Hominidae"),
    order = c("Primates", "Primates", "Primates"),
    bin_uri = c("BOLD:AAA0001", "BOLD:AAA0001", "BOLD:AAA0002"),
    identified_by = c("Curator A", "Curator A", "Curator B"),
    identification_method = c("morphology", "morphology", "DNA"),
    collectors = c("Coll A", "Coll B", "Coll C"),
    collection_date_start = c("2020-01-01", "2020-06-15", "2021-03-20"),
    country.ocean = c("Canada", "USA", "Congo"),
    coord = c("45.5,-73.5", "40.7,-74.0", "-4.3,15.3"),
    institution = c("Museum A", "Museum B", "Museum C"),
    voucher_type = c("holotype", "paratype", "vouchered"),
    quality_score = c(12, 10, 8),
    criteria_met = c("SPECIES_ID; SEQ_QUALITY", "SPECIES_ID", "SPECIES_ID"),
    nuc = c("ATCGATCG", "GCTAGCTA", "TTAACCGG"),
    stringsAsFactors = FALSE
  )
}

create_annotations <- function() {
  list(
    selections = list(
      P1 = list(timestamp = "2025-01-01", species = "Homo sapiens",
                quality_score = 12, user = "curator@test.com", selected = TRUE)
    ),
    flags = list(
      P2 = list(flag = "id_uncertain", timestamp = "2025-01-02",
                user = "curator@test.com", species = "Homo sapiens")
    ),
    notes = list(
      P3 = list(text = "Check voucher image", timestamp = "2025-01-03",
                user = "curator@test.com")
    )
  )
}

# ---------------------------------------------------------------------------
# ExportManager — TSV export
# ---------------------------------------------------------------------------

describe("ExportManager TSV export", {

  it("exports TSV with annotation columns", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    annotations <- create_annotations()

    result <- exporter$export_tsv(data, tmpfile, annotations = annotations)
    expect_true(result)
    expect_true(file.exists(tmpfile))

    # Read back and verify annotations
    exported <- read.delim(tmpfile, stringsAsFactors = FALSE)
    expect_true("selected" %in% names(exported))
    expect_true("flag" %in% names(exported))
    expect_true("curator_notes" %in% names(exported))

    # P1 should be selected
    p1 <- exported[exported$processid == "P1", ]
    expect_true(as.logical(p1$selected) || p1$selected == "TRUE")

    # P2 should be flagged
    p2 <- exported[exported$processid == "P2", ]
    expect_equal(p2$flag, "id_uncertain")

    # P3 should have a note
    p3 <- exported[exported$processid == "P3", ]
    expect_equal(p3$curator_notes, "Check voucher image")
  })

  it("exports TSV without sequences when include_sequences=FALSE", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    result <- exporter$export_tsv(data, tmpfile, include_sequences = FALSE)
    expect_true(result)

    exported <- read.delim(tmpfile, stringsAsFactors = FALSE)
    expect_false("nuc" %in% names(exported))
  })

  it("exports TSV with sequences when include_sequences=TRUE", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    result <- exporter$export_tsv(data, tmpfile, include_sequences = TRUE)
    expect_true(result)

    exported <- read.delim(tmpfile, stringsAsFactors = FALSE)
    expect_true("nuc" %in% names(exported))
  })

  it("logs successful export", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    exporter$export_tsv(data, tmpfile)

    expect_equal(length(logger$exports), 1)
    expect_equal(logger$exports[[1]]$export_type, "tsv")
    expect_true(logger$exports[[1]]$success)
  })

  it("returns FALSE for NULL specimen data", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmpfile))

    result <- exporter$export_tsv(list(specimen_data = NULL), tmpfile)
    expect_false(result)
  })
})

# ---------------------------------------------------------------------------
# ExportManager — Excel export
# ---------------------------------------------------------------------------

describe("ExportManager Excel export", {

  it("exports Excel with annotation columns in specimens sheet", {
    skip_if_not_installed("writexl")

    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmpfile))

    data <- list(
      specimen_data = create_specimen_data(),
      bags_grades = data.frame(
        species = c("Homo sapiens", "Pan troglodytes"),
        bags_grade = c("B", "D"),
        specimen_count = c(2L, 1L),
        bin_count = c(1L, 1L),
        shared_bins = c(FALSE, FALSE),
        stringsAsFactors = FALSE
      )
    )
    annotations <- create_annotations()

    result <- exporter$export_excel(data, tmpfile, annotations = annotations)
    expect_true(result)
    expect_true(file.exists(tmpfile))
    expect_true(file.size(tmpfile) > 0)
  })

  it("logs export with sheet names", {
    skip_if_not_installed("writexl")

    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    exporter$export_excel(data, tmpfile)

    expect_equal(length(logger$exports), 1)
    expect_equal(logger$exports[[1]]$export_type, "excel")
    expect_true(logger$exports[[1]]$success)
  })
})

# ---------------------------------------------------------------------------
# ExportManager — FASTA export
# ---------------------------------------------------------------------------

describe("ExportManager FASTA export", {

  it("exports FASTA with correct headers", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".fasta")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    result <- exporter$export_fasta(data, tmpfile)
    expect_true(result)

    lines <- readLines(tmpfile)
    # 3 specimens × 2 lines each (header + sequence)
    expect_equal(length(lines), 6)

    # Check header format: >processid|species|bin|country|date
    expect_true(grepl("^>P1\\|Homo sapiens\\|", lines[1]))
  })

  it("skips specimens without sequences", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".fasta")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    data$specimen_data$nuc[2] <- NA
    data$specimen_data$nuc[3] <- ""

    result <- exporter$export_fasta(data, tmpfile)
    expect_true(result)

    lines <- readLines(tmpfile)
    expect_equal(length(lines), 2)  # only P1
  })

  it("returns FALSE when no sequences available", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".fasta")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data())
    data$specimen_data$nuc <- NA

    result <- exporter$export_fasta(data, tmpfile)
    expect_false(result)
  })

  it("returns FALSE when nuc column missing", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")
    tmpfile <- tempfile(fileext = ".fasta")
    on.exit(unlink(tmpfile))

    data <- list(specimen_data = create_specimen_data()[, -which(names(create_specimen_data()) == "nuc")])
    result <- exporter$export_fasta(data, tmpfile)
    expect_false(result)
  })
})

# ---------------------------------------------------------------------------
# Column selection
# ---------------------------------------------------------------------------

describe("export column selection", {

  it("includes annotation columns in specimen export columns", {
    logger <- MockExportLogger$new()
    exporter <- ExportManager$new(logger, session_id = "test")

    # Access private method via environment hack for testing
    cols <- exporter$.__enclos_env__$private$get_specimen_columns()
    expect_true("selected" %in% cols)
    expect_true("flag" %in% cols)
    expect_true("curator_notes" %in% cols)
    expect_true("processid" %in% cols)
    expect_true("species" %in% cols)
  })
})
