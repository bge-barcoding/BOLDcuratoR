# Tests for session persistence (save/load/list/filter/cleanup)
# tests/testthat/test-session_persistence.R
#
# Session persistence is critical: if save/load silently fails,
# curators lose their entire annotation session.

library(testthat)
library(DBI)
library(RSQLite)
library(jsonlite)

source("../../R/utils/session_persistence.R")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Create a temporary SQLite DB, cleaned up automatically by test harness
setup_test_db <- function() {
  db_path <- tempfile("sessions_test_", fileext = ".sqlite")
  init_session_db(db_path)
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
    specimen_updated_ids = list(),
    search_taxa = "Homo sapiens",
    user_info = list(
      email = "curator@test.com",
      name = "Test Curator",
      orcid = "0000-0001-2345-6789"
    )
  )
}

# ---------------------------------------------------------------------------
# init_session_db()
# ---------------------------------------------------------------------------

describe("init_session_db", {

  it("creates the database file and tables", {
    db_path <- tempfile("init_test_", fileext = ".sqlite")
    con <- init_session_db(db_path)
    on.exit(DBI::dbDisconnect(con))

    expect_true(file.exists(db_path))
    tables <- DBI::dbListTables(con)
    expect_true("sessions" %in% tables)
    expect_true("session_data" %in% tables)
  })

  it("is idempotent (safe to call twice)", {
    db_path <- tempfile("idempotent_", fileext = ".sqlite")
    con1 <- init_session_db(db_path)
    DBI::dbDisconnect(con1)
    con2 <- init_session_db(db_path)
    on.exit(DBI::dbDisconnect(con2))

    tables <- DBI::dbListTables(con2)
    expect_true("sessions" %in% tables)
  })
})

# ---------------------------------------------------------------------------
# save_session_state()
# ---------------------------------------------------------------------------

describe("save_session_state", {

  it("saves all components and returns TRUE", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    result <- save_session_state("test_session", store, con = con)

    expect_true(result)

    # Check sessions table
    meta <- DBI::dbGetQuery(con, "SELECT * FROM sessions WHERE session_id = 'test_session'")
    expect_equal(nrow(meta), 1)

    # Check session_data table has expected keys
    data_rows <- DBI::dbGetQuery(con,
      "SELECT key FROM session_data WHERE session_id = 'test_session' ORDER BY key")
    saved_keys <- data_rows$key
    expect_true("specimen_data" %in% saved_keys)
    expect_true("bags_grades" %in% saved_keys)
    expect_true("selected_specimens" %in% saved_keys)
    expect_true("specimen_flags" %in% saved_keys)
    expect_true("specimen_curator_notes" %in% saved_keys)
  })

  it("saves correct metadata", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("meta_test", store, con = con)

    meta <- DBI::dbGetQuery(con, "SELECT * FROM sessions WHERE session_id = 'meta_test'")
    expect_equal(meta$user_email, "curator@test.com")
    expect_equal(meta$user_name, "Test Curator")
    expect_equal(meta$user_orcid, "0000-0001-2345-6789")
    expect_equal(meta$specimen_count, 3L)
    expect_equal(meta$species_count, 2L)
  })

  it("preserves created_at on subsequent saves", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("preserve_test", store, con = con)

    meta1 <- DBI::dbGetQuery(con, "SELECT * FROM sessions WHERE session_id = 'preserve_test'")
    created_at <- meta1$created_at

    Sys.sleep(1)  # ensure timestamp differs
    save_session_state("preserve_test", store, con = con)

    meta2 <- DBI::dbGetQuery(con, "SELECT * FROM sessions WHERE session_id = 'preserve_test'")
    expect_equal(meta2$created_at, created_at)
    expect_true(meta2$updated_at >= meta1$updated_at)
  })

  it("round-trips annotation data faithfully", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("roundtrip", store, con = con)
    loaded <- load_session_state("roundtrip", con = con)

    expect_equal(loaded$specimen_flags$PROC2$flag, "id_uncertain")
    expect_equal(loaded$specimen_flags$PROC2$user, "curator@test.com")
    expect_equal(loaded$specimen_curator_notes$PROC3$text, "Check voucher")
    expect_true(loaded$selected_specimens$PROC1$selected)
  })

  it("handles store with NULL specimen_data", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    store$specimen_data <- NULL
    result <- save_session_state("null_data", store, con = con)

    expect_true(result)
    # specimen_data should NOT have a row in session_data
    rows <- DBI::dbGetQuery(con,
      "SELECT key FROM session_data WHERE session_id = 'null_data' AND key = 'specimen_data'")
    expect_equal(nrow(rows), 0)
  })

  it("returns FALSE on write failure", {
    # Invalid connection (closed)
    con <- setup_test_db()
    DBI::dbDisconnect(con)

    result <- suppressWarnings(
      save_session_state("fail", list(), con = con)
    )
    expect_false(result)
  })
})

# ---------------------------------------------------------------------------
# load_session_state()
# ---------------------------------------------------------------------------

describe("load_session_state", {

  it("restores all saved components", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("load_test", store, con = con)

    loaded <- load_session_state("load_test", con = con)
    expect_false(is.null(loaded))
    expect_equal(nrow(loaded$specimen_data), 3)
    expect_equal(nrow(loaded$bags_grades), 2)
    expect_equal(loaded$specimen_flags$PROC2$flag, "id_uncertain")
    expect_equal(loaded$specimen_curator_notes$PROC3$text, "Check voucher")
    expect_true(loaded$selected_specimens$PROC1$selected)
  })

  it("restores metadata", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("meta_load", store, con = con)

    loaded <- load_session_state("meta_load", con = con)
    expect_equal(loaded$search_taxa, "Homo sapiens")
    expect_equal(loaded$metadata$user_email, "curator@test.com")
  })

  it("returns NULL for nonexistent session", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    result <- load_session_state("nonexistent", con = con)
    expect_null(result)
  })

  it("handles partial saves gracefully", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    # Insert only a sessions row and one data key
    now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    DBI::dbExecute(con, "INSERT INTO sessions (session_id, created_at, updated_at)
                         VALUES ('partial', ?, ?)", params = list(now, now))
    blob <- list(serialize(create_mock_store()$specimen_data, NULL))
    DBI::dbExecute(con, "INSERT INTO session_data (session_id, key, value, updated_at)
                         VALUES ('partial', 'specimen_data', ?, ?)",
                   params = list(blob, now))

    loaded <- load_session_state("partial", con = con)
    expect_false(is.null(loaded))
    expect_equal(nrow(loaded$specimen_data), 3)
    expect_null(loaded$bags_grades)
    expect_null(loaded$specimen_flags)
  })
})

# ---------------------------------------------------------------------------
# list_saved_sessions()
# ---------------------------------------------------------------------------

describe("list_saved_sessions", {

  it("returns empty data frame for empty database", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    result <- list_saved_sessions(con = con)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 0)
  })

  it("lists multiple sessions sorted by updated_at descending", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("session_a", store, con = con)
    Sys.sleep(1)
    save_session_state("session_b", store, con = con)

    sessions <- list_saved_sessions(con = con)
    expect_equal(nrow(sessions), 2)
    # Most recently updated first
    expect_equal(sessions$session_id[1], "session_b")
  })

  it("includes correct metadata columns", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("cols_test", store, con = con)

    sessions <- list_saved_sessions(con = con)
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
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store1 <- create_mock_store()
    store1$user_info$email <- "alice@test.com"
    store2 <- create_mock_store()
    store2$user_info$email <- "bob@test.com"

    save_session_state("alice_session", store1, con = con)
    save_session_state("bob_session", store2, con = con)

    result <- filter_sessions_by_user(
      user_email = "ALICE@TEST.COM", con = con
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$session_id[1], "alice_session")
  })

  it("filters by ORCID", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    store$user_info$orcid <- "0000-0001-9999-0000"
    save_session_state("orcid_session", store, con = con)

    result <- filter_sessions_by_user(
      user_orcid = "0000-0001-9999-0000", con = con
    )
    expect_equal(nrow(result), 1)
  })

  it("returns empty when no identifiers provided", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("s1", store, con = con)
    save_session_state("s2", store, con = con)

    result <- filter_sessions_by_user(con = con)
    expect_equal(nrow(result), 0)
  })

  it("uses OR logic across identifiers", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store1 <- create_mock_store()
    store1$user_info$email <- "alice@test.com"
    store1$user_info$name <- "Alice"
    store2 <- create_mock_store()
    store2$user_info$email <- "bob@test.com"
    store2$user_info$name <- "Bob"

    save_session_state("alice", store1, con = con)
    save_session_state("bob", store2, con = con)

    # Match by name OR email — should find both
    result <- filter_sessions_by_user(
      user_email = "alice@test.com",
      user_name = "Bob",
      con = con
    )
    expect_equal(nrow(result), 2)
  })
})

# ---------------------------------------------------------------------------
# cleanup_old_sessions()
# ---------------------------------------------------------------------------

describe("cleanup_old_sessions", {

  it("removes sessions older than max_age_days", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("old_session", store, con = con)

    # Backdate the metadata directly in SQLite
    old_ts <- format(Sys.time() - as.difftime(60, units = "days"),
                     "%Y-%m-%dT%H:%M:%S")
    DBI::dbExecute(con, "UPDATE sessions SET updated_at = ? WHERE session_id = 'old_session'",
                   params = list(old_ts))

    removed <- cleanup_old_sessions(max_age_days = 30, con = con)
    expect_equal(removed, 1)

    # Verify session is gone
    remaining <- DBI::dbGetQuery(con, "SELECT session_id FROM sessions")
    expect_equal(nrow(remaining), 0)

    # Verify cascade deleted session_data too
    data_rows <- DBI::dbGetQuery(con, "SELECT * FROM session_data WHERE session_id = 'old_session'")
    expect_equal(nrow(data_rows), 0)
  })

  it("preserves recent sessions", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("recent_session", store, con = con)

    removed <- cleanup_old_sessions(max_age_days = 30, con = con)
    expect_equal(removed, 0)

    remaining <- DBI::dbGetQuery(con, "SELECT session_id FROM sessions")
    expect_equal(nrow(remaining), 1)
  })

  it("returns 0 for empty database", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    removed <- cleanup_old_sessions(max_age_days = 30, con = con)
    expect_equal(removed, 0)
  })
})

# ---------------------------------------------------------------------------
# Round-trip: save → load → verify annotations intact
# ---------------------------------------------------------------------------

describe("session round-trip", {

  it("annotations survive save and load cycle", {
    con <- setup_test_db()
    on.exit(DBI::dbDisconnect(con))

    store <- create_mock_store()
    save_session_state("roundtrip", store, con = con)
    loaded <- load_session_state("roundtrip", con = con)

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
