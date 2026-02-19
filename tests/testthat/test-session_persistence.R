# Tests for session persistence (save/load/list/filter/cleanup)
# tests/testthat/test-session_persistence.R
#
# Session persistence is critical: if save/load silently fails,
# curators lose their entire annotation session.

library(testthat)
library(jsonlite)

source("../../R/utils/session_persistence.R")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Create a temp directory for test sessions, cleaned up automatically
setup_test_dir <- function() {
  dir <- tempfile("sessions_test_")
  dir.create(dir, recursive = TRUE)
  dir
}

# Build a mock store (plain list mimicking reactiveValues via isolate)
create_mock_store <- function() {
  list(
    specimen_data = data.frame(
      processid = c("PROC1", "PROC2", "PROC3"),
      species = c("Homo sapiens", "Homo sapiens", "Pan troglodytes"),
      bin_uri = c("BOLD:AAA0001", "BOLD:AAA0001", "BOLD:AAA0002"),
      quality_score = c(12, 10, 8),
      stringsAsFactors = FALSE
    ),
    bags_grades = data.frame(
      species = c("Homo sapiens", "Pan troglodytes"),
      bags_grade = c("B", "D"),
      specimen_count = c(2L, 1L),
      bin_count = c(1L, 1L),
      shared_bins = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    ),
    bin_analysis = NULL,
    selected_specimens = list(
      PROC1 = list(timestamp = "2025-01-01", species = "Homo sapiens",
                   quality_score = 12, user = "curator@test.com", selected = TRUE)
    ),
    specimen_flags = list(
      PROC2 = list(flag = "id_uncertain", timestamp = "2025-01-01",
                   species = "Homo sapiens", user = "curator@test.com")
    ),
    specimen_curator_notes = list(
      PROC3 = list(text = "Check voucher", timestamp = "2025-01-01",
                   user = "curator@test.com")
    ),
    search_taxa = "Homo sapiens",
    user_info = list(
      email = "curator@test.com",
      name = "Test Curator",
      orcid = "0000-0001-2345-6789"
    )
  )
}

# ---------------------------------------------------------------------------
# save_session_state()
# ---------------------------------------------------------------------------

describe("save_session_state", {

  it("saves all components and returns TRUE", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    result <- save_session_state("test_session", store, session_dir = dir)

    expect_true(result)
    expect_true(dir.exists(file.path(dir, "test_session")))
    expect_true(file.exists(file.path(dir, "test_session", "specimen_data.rds")))
    expect_true(file.exists(file.path(dir, "test_session", "bags_grades.rds")))
    expect_true(file.exists(file.path(dir, "test_session", "selected_specimens.rds")))
    expect_true(file.exists(file.path(dir, "test_session", "specimen_flags.rds")))
    expect_true(file.exists(file.path(dir, "test_session", "specimen_curator_notes.rds")))
    expect_true(file.exists(file.path(dir, "test_session", "session_meta.json")))
  })

  it("saves correct metadata in JSON", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("meta_test", store, session_dir = dir)

    meta <- fromJSON(file.path(dir, "meta_test", "session_meta.json"))
    expect_equal(meta$session_id, "meta_test")
    expect_equal(meta$user_email, "curator@test.com")
    expect_equal(meta$user_name, "Test Curator")
    expect_equal(meta$user_orcid, "0000-0001-2345-6789")
    expect_equal(meta$specimen_count, 3)
    expect_equal(meta$species_count, 2)
    expect_equal(meta$search_taxa, "Homo sapiens")
  })

  it("preserves created_at on subsequent saves", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("preserve_test", store, session_dir = dir)

    meta1 <- fromJSON(file.path(dir, "preserve_test", "session_meta.json"))
    created_at <- meta1$created_at

    Sys.sleep(1)  # ensure timestamp differs
    save_session_state("preserve_test", store, session_dir = dir)

    meta2 <- fromJSON(file.path(dir, "preserve_test", "session_meta.json"))
    expect_equal(meta2$created_at, created_at)
    expect_true(meta2$updated_at >= meta1$updated_at)
  })

  it("round-trips annotation data faithfully", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("roundtrip", store, session_dir = dir)

    saved_flags <- readRDS(file.path(dir, "roundtrip", "specimen_flags.rds"))
    expect_equal(saved_flags$PROC2$flag, "id_uncertain")
    expect_equal(saved_flags$PROC2$user, "curator@test.com")

    saved_notes <- readRDS(file.path(dir, "roundtrip", "specimen_curator_notes.rds"))
    expect_equal(saved_notes$PROC3$text, "Check voucher")

    saved_sel <- readRDS(file.path(dir, "roundtrip", "selected_specimens.rds"))
    expect_true(saved_sel$PROC1$selected)
  })

  it("handles store with NULL specimen_data", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    store$specimen_data <- NULL
    result <- save_session_state("null_data", store, session_dir = dir)

    expect_true(result)
    expect_false(file.exists(file.path(dir, "null_data", "specimen_data.rds")))
  })

  it("returns FALSE on write failure", {
    # Point to a path that cannot be created; suppress the expected warning
    result <- suppressWarnings(
      save_session_state("fail", list(), session_dir = "/dev/null/impossible")
    )
    expect_false(result)
  })
})

# ---------------------------------------------------------------------------
# load_session_state()
# ---------------------------------------------------------------------------

describe("load_session_state", {

  it("restores all saved components", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("load_test", store, session_dir = dir)

    loaded <- load_session_state("load_test", session_dir = dir)
    expect_false(is.null(loaded))
    expect_equal(nrow(loaded$specimen_data), 3)
    expect_equal(nrow(loaded$bags_grades), 2)
    expect_equal(loaded$specimen_flags$PROC2$flag, "id_uncertain")
    expect_equal(loaded$specimen_curator_notes$PROC3$text, "Check voucher")
    expect_true(loaded$selected_specimens$PROC1$selected)
  })

  it("restores metadata", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("meta_load", store, session_dir = dir)

    loaded <- load_session_state("meta_load", session_dir = dir)
    expect_equal(loaded$search_taxa, "Homo sapiens")
    expect_equal(loaded$metadata$user_email, "curator@test.com")
  })

  it("returns NULL for nonexistent session", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    result <- load_session_state("nonexistent", session_dir = dir)
    expect_null(result)
  })

  it("handles partial saves gracefully", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    # Create a session dir with only some files
    sess_dir <- file.path(dir, "partial")
    dir.create(sess_dir, recursive = TRUE)
    saveRDS(create_mock_store()$specimen_data, file.path(sess_dir, "specimen_data.rds"))

    loaded <- load_session_state("partial", session_dir = dir)
    expect_false(is.null(loaded))
    expect_equal(nrow(loaded$specimen_data), 3)
    expect_null(loaded$bags_grades)  # wasn't saved
    expect_null(loaded$specimen_flags)  # wasn't saved
  })
})

# ---------------------------------------------------------------------------
# list_saved_sessions()
# ---------------------------------------------------------------------------

describe("list_saved_sessions", {

  it("returns empty data frame for nonexistent directory", {
    result <- list_saved_sessions(session_dir = "/nonexistent/path")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 0)
  })

  it("lists multiple sessions sorted by updated_at descending", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("session_a", store, session_dir = dir)
    Sys.sleep(1)
    save_session_state("session_b", store, session_dir = dir)

    sessions <- list_saved_sessions(session_dir = dir)
    expect_equal(nrow(sessions), 2)
    # Most recently updated first
    expect_equal(sessions$session_id[1], "session_b")
  })

  it("includes correct metadata columns", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("cols_test", store, session_dir = dir)

    sessions <- list_saved_sessions(session_dir = dir)
    expected_cols <- c("session_id", "created_at", "updated_at",
                       "user_email", "user_name", "user_orcid",
                       "specimen_count", "species_count")
    expect_true(all(expected_cols %in% names(sessions)))
  })
})

# ---------------------------------------------------------------------------
# filter_sessions_by_user()
# ---------------------------------------------------------------------------

describe("filter_sessions_by_user", {

  it("filters by email (case insensitive)", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store1 <- create_mock_store()
    store1$user_info$email <- "alice@test.com"
    store2 <- create_mock_store()
    store2$user_info$email <- "bob@test.com"

    save_session_state("alice_session", store1, session_dir = dir)
    save_session_state("bob_session", store2, session_dir = dir)

    result <- filter_sessions_by_user(
      user_email = "ALICE@TEST.COM", session_dir = dir
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$session_id[1], "alice_session")
  })

  it("filters by ORCID", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    store$user_info$orcid <- "0000-0001-9999-0000"
    save_session_state("orcid_session", store, session_dir = dir)

    result <- filter_sessions_by_user(
      user_orcid = "0000-0001-9999-0000", session_dir = dir
    )
    expect_equal(nrow(result), 1)
  })

  it("returns all sessions when no identifiers provided", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("s1", store, session_dir = dir)
    save_session_state("s2", store, session_dir = dir)

    result <- filter_sessions_by_user(session_dir = dir)
    expect_equal(nrow(result), 2)
  })

  it("uses OR logic across identifiers", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store1 <- create_mock_store()
    store1$user_info$email <- "alice@test.com"
    store1$user_info$name <- "Alice"
    store2 <- create_mock_store()
    store2$user_info$email <- "bob@test.com"
    store2$user_info$name <- "Bob"

    save_session_state("alice", store1, session_dir = dir)
    save_session_state("bob", store2, session_dir = dir)

    # Match by name OR email — should find both
    result <- filter_sessions_by_user(
      user_email = "alice@test.com",
      user_name = "Bob",
      session_dir = dir
    )
    expect_equal(nrow(result), 2)
  })
})

# ---------------------------------------------------------------------------
# cleanup_old_sessions()
# ---------------------------------------------------------------------------

describe("cleanup_old_sessions", {

  it("removes sessions older than max_age_days", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("old_session", store, session_dir = dir)

    # Backdate the metadata
    meta_file <- file.path(dir, "old_session", "session_meta.json")
    meta <- fromJSON(meta_file)
    meta$updated_at <- format(Sys.time() - as.difftime(60, units = "days"),
                              "%Y-%m-%dT%H:%M:%S")
    write_json(meta, meta_file, auto_unbox = TRUE, pretty = TRUE)

    removed <- cleanup_old_sessions(max_age_days = 30, session_dir = dir)
    expect_equal(removed, 1)
    expect_false(dir.exists(file.path(dir, "old_session")))
  })

  it("preserves recent sessions", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("recent_session", store, session_dir = dir)

    removed <- cleanup_old_sessions(max_age_days = 30, session_dir = dir)
    expect_equal(removed, 0)
    expect_true(dir.exists(file.path(dir, "recent_session")))
  })

  it("returns 0 for empty directory", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    removed <- cleanup_old_sessions(max_age_days = 30, session_dir = dir)
    expect_equal(removed, 0)
  })
})

# ---------------------------------------------------------------------------
# Round-trip: save → load → verify annotations intact
# ---------------------------------------------------------------------------

describe("session round-trip", {

  it("annotations survive save and load cycle", {
    dir <- setup_test_dir()
    on.exit(unlink(dir, recursive = TRUE))

    store <- create_mock_store()
    save_session_state("roundtrip", store, session_dir = dir)
    loaded <- load_session_state("roundtrip", session_dir = dir)

    # Specimen data intact
    expect_equal(nrow(loaded$specimen_data), nrow(store$specimen_data))
    expect_equal(loaded$specimen_data$processid, store$specimen_data$processid)

    # Annotations intact
    expect_equal(loaded$selected_specimens$PROC1$species, "Homo sapiens")
    expect_equal(loaded$specimen_flags$PROC2$flag, "id_uncertain")
    expect_equal(loaded$specimen_curator_notes$PROC3$text, "Check voucher")

    # BAGS grades intact
    expect_equal(loaded$bags_grades$bags_grade, store$bags_grades$bags_grade)
  })
})
