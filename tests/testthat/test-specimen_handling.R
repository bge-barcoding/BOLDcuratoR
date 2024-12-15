# Tests for specimen handling functionality
# tests/testthat/test-specimen_handling.R

library(testthat)
library(shiny)
library(dplyr)

# Mock logger
MockLogger <- R6::R6Class("MockLogger",
                          public = list(
                            logs = list(),
                            log_action = function(user_email, user_name, session_id, action_type,
                                                  process_ids, metadata) {
                              self$logs[[length(self$logs) + 1]] <- list(
                                user_email = user_email,
                                user_name = user_name,
                                session_id = session_id,
                                action_type = action_type,
                                process_ids = process_ids,
                                metadata = metadata
                              )
                            },
                            get_specimen_history = function(processid) {
                              list(
                                action_history = list(
                                  data.frame(
                                    timestamp = Sys.time(),
                                    action = "test_action",
                                    user = "test_user",
                                    stringsAsFactors = FALSE
                                  )
                                )
                              )
                            }
                          )
)

# Mock specimen data
create_test_specimens <- function() {
  data.frame(
    processid = paste0("BOLD:", 1:10),
    species = rep(c("Species A", "Species B"), each = 5),
    specimen_rank = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    quality_score = c(14, 12, 10, 8, 6, 13, 11, 9, 7, 5),
    criteria_met = c(
      "SPECIES_ID; TYPE_SPECIMEN; SEQ_QUALITY",
      "SPECIES_ID; SEQ_QUALITY",
      "SPECIES_ID; HAS_IMAGE",
      "SPECIES_ID",
      "SEQ_QUALITY",
      "SPECIES_ID; TYPE_SPECIMEN; SEQ_QUALITY",
      "SPECIES_ID; SEQ_QUALITY",
      "SPECIES_ID; HAS_IMAGE",
      "SPECIES_ID",
      "SEQ_QUALITY"
    ),
    bin_uri = paste0("BIN:", 1:10),
    country.ocean = rep(c("Canada", "USA"), 5),
    stringsAsFactors = FALSE
  )
}

test_that("specimen handling UI module generates correctly", {
  ui <- mod_specimen_handling_ui("test")

  # Check UI elements exist
  expect_true(inherits(ui, "shiny.tag.list"))
  expect_true(grepl("rank_filter", as.character(ui)))
  expect_true(grepl("min_quality_score", as.character(ui)))
  expect_true(grepl("criteria_filter", as.character(ui)))
  expect_true(grepl("specimen_table", as.character(ui)))
})

test_that("specimen filtering works correctly", {
  testServer(mod_specimen_handling_server, args = list(
    state = initialize_state(MockSession$new()),
    logger = MockLogger$new()
  ), {
    state$set_specimen_data(create_test_specimens())

    # Test rank filter
    session$setInputs(rank_filter = "1")
    filtered <- filtered_data()
    expect_equal(nrow(filtered), 2)
    expect_true(all(filtered$specimen_rank == 1))

    # Test quality score filter
    session$setInputs(rank_filter = "All", min_quality_score = "12")
    filtered <- filtered_data()
    expect_equal(nrow(filtered), 3)
    expect_true(all(filtered$quality_score >= 12))

    # Test criteria filter
    session$setInputs(
      rank_filter = "All",
      min_quality_score = "0",
      criteria_filter = c("SPECIES_ID", "TYPE_SPECIMEN")
    )
    filtered <- filtered_data()
    expect_true(all(sapply(filtered$criteria_met, function(x) {
      all(c("SPECIES_ID", "TYPE_SPECIMEN") %in% strsplit(x, "; ")[[1]])
    })))
  })
})

test_that("specimen selection requires user info", {
  logger <- MockLogger$new()
  state <- initialize_state(MockSession$new())

  testServer(mod_specimen_handling_server, args = list(
    state = state,
    logger = logger
  ), {
    # Set specimen data
    state$set_specimen_data(create_test_specimens())

    # Try selecting without user info
    session$setInputs(specimen_table_rows_selected = 1)

    # Check no selection was made
    expect_equal(length(state$get_reactive_values()$selected_specimens), 0)
    expect_equal(length(logger$logs), 0)

    # Add user info and try again
    state$set_user_info(email = "test@example.com", name = "Test User")
    session$setInputs(specimen_table_rows_selected = 1)

    # Check selection was logged
    expect_equal(length(logger$logs), 1)
    expect_equal(logger$logs[[1]]$user_email, "test@example.com")
    expect_equal(logger$logs[[1]]$action_type, "specimen_selected")
  })
})

test_that("specimen selection logs quality information", {
  logger <- MockLogger$new()
  state <- initialize_state(MockSession$new())
  state$set_user_info(email = "test@example.com")

  testServer(mod_specimen_handling_server, args = list(
    state = state,
    logger = logger
  ), {
    state$set_specimen_data(create_test_specimens())
    session$setInputs(specimen_table_rows_selected = 1)

    log_entry <- logger$logs[[1]]
    expect_false(is.null(log_entry$metadata$quality_score))
    expect_false(is.null(log_entry$metadata$specimen_rank))
    expect_false(is.null(log_entry$metadata$criteria_met))
  })
})

test_that("specimen history modal shows data", {
  logger <- MockLogger$new()
  state <- initialize_state(MockSession$new())

  testServer(mod_specimen_handling_server, args = list(
    state = state,
    logger = logger
  ), {
    state$set_specimen_data(create_test_specimens())

    # Trigger history modal
    session$setInputs(specimen_table_row_last_clicked = 1)
    session$setInputs(show_history = 1)

    # Check that modal contains history data
    expect_true(!is.null(output$specimen_history))
  })
})

test_that("specimen selection chooses highest quality specimen", {
  logger <- MockLogger$new()
  state <- initialize_state(MockSession$new())
  state$set_user_info(email = "test@example.com")

  testServer(mod_specimen_handling_server, args = list(
    state = state,
    logger = logger
  ), {
    test_data <- create_test_specimens()
    state$set_specimen_data(test_data)

    # Select multiple specimens of same species
    session$setInputs(specimen_table_rows_selected = c(1, 2, 3))

    # Check that highest quality specimen was selected
    selected <- state$get_reactive_values()$selected_specimens
    expect_equal(selected[["Species A"]], "BOLD:1")
  })
})

test_that("get_specimen_columns returns correct columns", {
  columns <- get_specimen_columns()
  expected_columns <- c("species", "Valid_Name", "Search_Name", "Name_Status",
                        "quality_score", "specimen_rank", "bin_uri", "processid",
                        "identified_by", "country.ocean", "collection_date_start",
                        "voucher_type", "criteria_met")
  expect_equal(columns, expected_columns)
})

test_that("specimen table formatting works correctly", {
  state <- initialize_state(MockSession$new())
  logger <- MockLogger$new()

  testServer(mod_specimen_handling_server, args = list(
    state = state,
    logger = logger
  ), {
    state$set_specimen_data(create_test_specimens())

    # Check table output
    expect_true(!is.null(output$specimen_table))

    # Verify column ordering
    display_columns <- get_specimen_columns()
    expect_true(all(display_columns %in% names(filtered_data())))
  })
})

# Mock Session class for testing
MockSession <- R6::R6Class("MockSession",
                           public = list(
                             token = "test-token",
                             ns = function(x) paste0("test-", x),
                             sendCustomMessage = function(type, message) NULL,
                             sendInputMessage = function(inputId, message) NULL
                           )
)
