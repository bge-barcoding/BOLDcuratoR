library(testthat)
library(shiny)

# Create mock state management
mock_state <- function() {
  rv <- reactiveVal(list(
    specimen_data = NULL,
    bags_grades = NULL,
    selected_specimens = list(),
    user_info = list(
      email = NULL,
      name = NULL
    )
  ))

  list(
    get_reactive_values = function() rv(),
    set_specimen_data = function(data) {
      current <- rv()
      current$specimen_data <- data
      rv(current)
    },
    set_user_info = function(email = NULL, name = NULL) {
      current <- rv()
      current$user_info$email <- email
      current$user_info$name <- name
      rv(current)
    },
    validate_state = function(chain) {
      list(
        is_valid = !is.null(rv()$specimen_data),
        failed_validation = NULL
      )
    },
    update_grades = function(grades) {
      current <- rv()
      current$bags_grades <- grades
      rv(current)
    },
    update_selected_specimens = function(selections) {
      current <- rv()
      current$selected_specimens <- selections
      rv(current)
    },
    track_activity = function(action, details = NULL) {},
    set_error = function(message, details = NULL, source = NULL) {}
  )
}

test_that("BAGS grading module UI generates correctly", {
  ui <- mod_bags_grading_ui("test", "A")

  html <- as.character(ui)
  expect_true(grepl('id="test-grade_summary_box"', html))
  expect_true(grepl('id="test-selected_count_box"', html))
  expect_true(grepl('id="test-grade_content"', html))
  expect_true(grepl('id="test-selected_specimens_table"', html))
})

test_that("BAGS grading module handles specimen data correctly", {
  state <- mock_state()

  testServer(mod_bags_grading_server, args = list(
    state = state,
    grade = "A"
  ), {
    # Set specimen data
    specimen_data <- data.frame(
      species = c("Species A", "Species A", "Species B"),
      specimen_rank = c(1, 2, 1),
      quality_score = c(14, 12, 13),
      processid = c("BOLD:1", "BOLD:2", "BOLD:3"),
      bin_uri = c("BIN:1", "BIN:1", "BIN:2"),
      identified_by = c("John Doe", "Jane Doe", "John Doe"),
      country.ocean = c("Canada", "USA", "Mexico"),
      collection_date_start = c("2023-01-01", "2023-01-02", "2023-01-03"),
      stringsAsFactors = FALSE
    )

    state$set_specimen_data(specimen_data)

    # Verify grade calculation
    session$flushReact()
    expect_false(is.null(state$get_reactive_values()$bags_grades))

    # Verify grade filtering
    filtered <- grade_specimens()
    expect_equal(nrow(filtered), 2) # Species A specimens
    expect_equal(filtered$processid, c("BOLD:1", "BOLD:2"))
  })
})

test_that("BAGS grading module handles specimen selection correctly", {
  state <- mock_state()

  testServer(mod_bags_grading_server, args = list(
    state = state,
    grade = "A"
  ), {
    # Set user info and specimen data
    state$set_user_info(email = "test@example.com")
    state$set_specimen_data(data.frame(
      species = "Species A",
      specimen_rank = 1,
      quality_score = 14,
      processid = "BOLD:1",
      bin_uri = "BIN:1",
      identified_by = "John Doe",
      country.ocean = "Canada",
      collection_date_start = "2023-01-01",
      stringsAsFactors = FALSE
    ))

    # Simulate specimen selection
    session$setInputs(specimen_selection = '{"species":"Species A","processid":"BOLD:1"}')

    # Verify selection was recorded
    expect_equal(
      state$get_reactive_values()$selected_specimens[["Species A"]],
      "BOLD:1"
    )
  })
})

test_that("create_specimen_table generates correct structure", {
  specimens <- data.frame(
    processid = c("BOLD:1", "BOLD:2"),
    species = c("Species A", "Species A"),
    specimen_rank = c(1, 2),
    quality_score = c(14, 12),
    bin_uri = c("BIN:1", "BIN:1"),
    identified_by = c("John Doe", "Jane Doe"),
    country.ocean = c("Canada", "USA"),
    collection_date_start = c("2023-01-01", "2023-01-02"),
    stringsAsFactors = FALSE
  )

  selected_specimens <- list("Species A" = "BOLD:1")

  table <- create_specimen_table(
    function(x) paste0("test-", x),
    "Species A",
    specimens,
    selected_specimens
  )

  expect_s3_class(table, "datatables")
  expect_true("select" %in% names(table$x$data))
  expect_match(table$x$data$select[1], "checked", fixed = TRUE)
  expect_equal(nrow(table$x$data), 2)
})

test_that("BAGS grading handles invalid specimen data gracefully", {
  state <- mock_state()

  testServer(mod_bags_grading_server, args = list(
    state = state,
    grade = "A"
  ), {
    # Set invalid specimen data
    state$set_specimen_data(data.frame(
      species = character(0),
      specimen_rank = numeric(0),
      quality_score = numeric(0),
      processid = character(0),
      stringsAsFactors = FALSE
    ))

    # Verify empty grade specimens
    filtered <- grade_specimens()
    expect_equal(nrow(filtered), 0)
  })
})

test_that("BAGS grading handles user requirements", {
  state <- mock_state()

  testServer(mod_bags_grading_server, args = list(
    state = state,
    grade = "A"
  ), {
    specimen_data <- data.frame(
      species = "Species A",
      specimen_rank = 1,
      quality_score = 14,
      processid = "BOLD:1",
      bin_uri = "BIN:1",
      stringsAsFactors = FALSE
    )
    state$set_specimen_data(specimen_data)

    # Try selection without user info
    session$setInputs(specimen_selection = '{"species":"Species A","processid":"BOLD:1"}')
    expect_equal(length(state$get_reactive_values()$selected_specimens), 0)

    # Add user info and try again
    state$set_user_info(email = "test@example.com", name = "Test User")
    session$setInputs(specimen_selection = '{"species":"Species A","processid":"BOLD:1"}')
    expect_equal(length(state$get_reactive_values()$selected_specimens), 1)
  })
})

test_that("BAGS grading download handlers work correctly", {
  state <- mock_state()

  testServer(mod_bags_grading_server, args = list(
    state = state,
    grade = "A"
  ), {
    specimen_data <- data.frame(
      species = c("Species A", "Species B"),
      specimen_rank = c(1, 1),
      quality_score = c(14, 13),
      processid = c("BOLD:1", "BOLD:2"),
      bin_uri = c("BIN:1", "BIN:2"),
      stringsAsFactors = FALSE
    )
    state$set_specimen_data(specimen_data)

    # Test download handlers exist
    expect_true(!is.null(output$download_selected_full))
    expect_true(!is.null(output$download_selected_processids))
    expect_true(!is.null(output$download_grade))
  })
})
