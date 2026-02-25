# BOLDcuratoR/app.R

# debugging mode - delete later
options(shiny.error = browser)

# Source global configuration
source("global.R")

# Source required modules
source("R/config/constants.R")
source("R/config/column_definitions.R")

# Source utility functions
source("R/utils/ErrorBoundary.R")
source("R/utils/bags_grading.R")
source("R/utils/annotation_utils.R")
source("R/utils/table_utils.R")
source("R/utils/session_persistence.R")
source("R/utils/image_utils.R")

# Source core modules
source("R/modules/state/state_manager.R")
source("R/modules/logging/mod_logging.R")

# Source primary modules
source("R/modules/data_import/mod_data_import_ui.R")
source("R/modules/data_import/mod_data_import_server.R")
source("R/modules/data_import/mod_data_import_utils.R")

# Source analysis modules
source("R/modules/specimen_handling/specimen_validator.R")
source("R/modules/specimen_handling/specimen_processor.R")
source("R/modules/specimen_handling/specimen_scorer.R")
source("R/modules/specimen_handling/mod_specimen_handling_server.R")
source("R/modules/specimen_handling/mod_specimen_handling_ui.R")

source("R/modules/bin_analysis/mod_bin_analysis_ui.R")
source("R/modules/bin_analysis/mod_bin_analysis_server.R")
source("R/modules/bin_analysis/mod_bin_analysis_utils.R")

source("R/modules/bags_grading/mod_bags_grading_ui.R")
source("R/modules/bags_grading/mod_bags_grading_server.R")
source("R/modules/bags_grading/mod_bags_grading_utils.R")

source("R/modules/species_analysis/mod_species_analysis_ui.R")
source("R/modules/species_analysis/mod_species_analysis_server.R")
source("R/modules/species_analysis/mod_species_analysis_utils.R")

# Source export module
source("R/modules/export/mod_export.R")

# Source user modules
source("R/modules/user/mod_user_info_ui.R")
source("R/modules/user/mod_user_info_server.R")

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "BOLDcuratoR"),

  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("table")),
      menuItem("Species Analysis", tabName = "species_analysis", icon = icon("list-check")),
      menuItem("BIN Analysis", tabName = "bins", icon = icon("dna")),
      menuItem("BAGS Grade A", tabName = "bags_a", icon = icon("star")),
      menuItem("BAGS Grade B", tabName = "bags_b", icon = icon("star-half-alt")),
      menuItem("BAGS Grade C", tabName = "bags_c", icon = icon("exclamation-circle")),
      menuItem("BAGS Grade D", tabName = "bags_d", icon = icon("exclamation-triangle")),
      menuItem("BAGS Grade E", tabName = "bags_e", icon = icon("times-circle")),
      menuItem("Specimens", tabName = "specimens", icon = icon("microscope"))
    )
  ),

  dashboardBody(
    useShinyjs(),
    # Add DT dependency
    DT::datatable(NULL),
    DTOutput('dummy-dt'),
    tags$head(
      tags$style(HTML(paste(
        "
        /* Prevent page-level scroll — only tables scroll internally */
        html, body {
          overflow: hidden !important;
          height: 100vh !important;
          width: 100vw !important;
          margin: 0 !important;
          padding: 0 !important;
        }
        .wrapper {
          overflow: hidden !important;
          height: 100vh !important;
        }
        .content-wrapper {
          overflow: hidden !important;
          height: calc(100vh - 50px) !important;
          min-height: 0 !important;
          padding: 0 !important;
          max-width: 100% !important;
        }
        .content-wrapper > section,
        .content-wrapper > section.content,
        .content-wrapper > .content {
          height: 100% !important;
          min-height: 0 !important;
          overflow: hidden !important;
          max-width: 100% !important;
        }
        .content-wrapper > section > .tab-content,
        .content-wrapper > .content > .tab-content {
          height: calc(100vh - 100px) !important;
          min-height: 0 !important;
          overflow: hidden !important;
        }
        /* Default: allow tab-pane scroll for tabs that need it (data input, BAGS) */
        .tab-content > .tab-pane {
          max-height: calc(100vh - 100px) !important;
          overflow-y: auto !important;
          overflow-x: hidden !important;
        }
        /* Analysis tabs: no page scroll, tables scroll internally via DT scrollY */
        .tab-content > .tab-pane[data-value='species_analysis'],
        .tab-content > .tab-pane[data-value='bins'],
        .tab-content > .tab-pane[data-value='specimens'] {
          overflow: hidden !important;
        }
        .small-box {
          margin-bottom: 5px !important;
        }

        /* Hide sidebar toggle button */
        .sidebar-toggle {
          display: none !important;
        }

        /* User info top bar — compact inline form */
        .user-info-bar .form-group {
          margin-bottom: 0 !important;
          margin-top: 0 !important;
        }
        .user-info-bar .shiny-input-container {
          margin: 0 !important;
          width: 100% !important;
        }
        .user-info-bar .form-control {
          height: 28px !important;
          font-size: 12px !important;
          padding: 2px 6px !important;
        }
        .user-info-bar .validation-messages {
          font-size: 10px !important;
          margin: 0 !important;
          line-height: 1.2 !important;
        }

        /* DataTables Overrides */
        .dataTables_wrapper {
          padding: 0 !important;
          line-height: 1 !important;
          max-width: 100% !important;
          overflow: hidden !important;
        }
        .dataTables_wrapper .dataTables_scroll {
          padding: 0 !important;
          margin: 0 !important;
        }
        .dataTables_scrollBody {
          max-height: 60vh !important;
        }
        .dataTables_wrapper .row {
          margin: 0 !important;
          padding: 2px 0 !important;
        }

        /* Compact shinydashboard boxes */
        .content-wrapper .box {
          margin-bottom: 5px !important;
          max-width: 100% !important;
          overflow: hidden !important;
        }

        .content-wrapper .box-header {
          padding: 5px 5px !important;
        }

        .content-wrapper .box-body {
          padding: 5px !important;
          max-width: 100% !important;
          overflow: hidden !important;
        }

        /* Compact data input form */
        .content-wrapper .form-group {
          margin-bottom: 3px !important;
        }
        .content-wrapper .shiny-input-container label {
          margin-bottom: 1px !important;
        }

        /* Compact value boxes */
        .content-wrapper .small-box .inner {
          padding: 5px 10px !important;
        }
        .content-wrapper .small-box .icon-large,
        .content-wrapper .small-box .icon {
          font-size: 50px !important;
          top: 5px !important;
        }
        .content-wrapper .small-box h3 {
          font-size: 28px !important;
        }
        .content-wrapper .small-box p {
          font-size: 13px !important;
        }

        /* Reduce fluidRow margins */
        .content-wrapper .row {
          margin-bottom: 0px !important;
        }

        /* Ensure all content stays within viewport */
        .tab-pane .row {
          max-width: 100% !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
        .tab-pane .col-sm-12,
        .tab-pane .col-sm-14 {
          max-width: 100% !important;
          padding-left: 5px !important;
          padding-right: 5px !important;
        }

",
        get_table_css()
      ))),
    ),

    # User info bar along the top
    mod_user_info_ui("user_info"),

    tabItems(
      tabItem(tabName = "input",
              mod_data_import_ui("data_import")),

      tabItem(tabName = "species_analysis",
              mod_species_analysis_ui("species_analysis")),

      tabItem(tabName = "bins",
              mod_bin_analysis_ui("bin_analysis")),

      tabItem(tabName = "bags_a",
              mod_bags_grading_ui("bags_a", grade = "A")),

      tabItem(tabName = "bags_b",
              mod_bags_grading_ui("bags_b", grade = "B")),

      tabItem(tabName = "bags_c",
              mod_bags_grading_ui("bags_c", grade = "C")),

      tabItem(tabName = "bags_d",
              mod_bags_grading_ui("bags_d", grade = "D")),

      tabItem(tabName = "bags_e",
              mod_bags_grading_ui("bags_e", grade = "E")),

      tabItem(tabName = "specimens",
              mod_specimen_handling_ui("specimen_handling"))
    )
  )
)

# Define Processor classes
BagsProcessor <- R6::R6Class(
  "BagsProcessor",
  public = list(
    initialize = function(logger) {
      private$logger <- logger
    },
    process_grades = function(specimens) {
      return(calculate_bags_grade(specimens))
    }
  ),
  private = list(
    logger = NULL
  )
)

BinProcessor <- R6::R6Class(
  "BinProcessor",
  public = list(
    initialize = function(logger) {
      private$logger <- logger
    },
    analyze_bins = function(specimens) {
      if (!is.null(private$logger)) {
        private$logger$info("Starting BIN analysis")
      }
      tryCatch({
        analyze_bin_data(specimens)
      }, error = function(e) {
        if (!is.null(private$logger)) {
          private$logger$error(sprintf("BIN analysis failed: %s", e$message))
        }
        NULL
      })
    }
  ),
  private = list(
    logger = NULL
  )
)

# Server Definition
server <- function(input, output, session) {
  # Initialize session database (shared across all Shiny workers via WAL mode)
  session_db <- init_session_db("data/sessions.sqlite")
  cleanup_old_sessions(max_age_days = 30, con = session_db)
  onStop(function() { DBI::dbDisconnect(session_db) })

  # Initialize core components
  logging_manager <- LoggingManager$new()
  state <- StateManager$new(session, logging_manager)
  export_manager <- ExportManager$new(logging_manager, session$token)

  # Initialize processors
  bags_processor <- BagsProcessor$new(logging_manager)
  bin_processor <- BinProcessor$new(logging_manager)
  specimen_processor <- SpecimenProcessor$new(
    validator = SpecimenValidator$new(logging_manager),
    scorer = SpecimenScorer$new(logging_manager),
    logger = logging_manager
  )

  # Initialize modules
  user_info <- mod_user_info_server(
    "user_info",
    state = state,
    logger = logging_manager
  )

  data_import <- mod_data_import_server(
    "data_import",
    state = state,
    session_db = session_db,
    logger = logging_manager
  )

  specimen_handling <- mod_specimen_handling_server(
    "specimen_handling",
    state = state,
    processor = specimen_processor,
    logger = logging_manager
  )

  bin_analysis <- mod_bin_analysis_server(
    "bin_analysis",
    state = state,
    processor = function(specimens) bin_processor$analyze_bins(specimens),  # Pass as function
    logger = logging_manager
  )

  species_analysis <- mod_species_analysis_server(
    "species_analysis",
    state = state,
    logger = logging_manager
  )

  # Initialize BAGS grading modules with direct grade parameters
  bags_grading <- list(
    a = mod_bags_grading_server("bags_a", state, "A", logging_manager),
    b = mod_bags_grading_server("bags_b", state, "B", logging_manager),
    c = mod_bags_grading_server("bags_c", state, "C", logging_manager),
    d = mod_bags_grading_server("bags_d", state, "D", logging_manager),
    e = mod_bags_grading_server("bags_e", state, "E", logging_manager)
  )

  # Process specimens for analysis
  observe({
    store <- state$get_store()
    req(store$specimen_data)

    withProgress(
      message = 'Processing specimens',
      value = 0,
      {
        # Process specimens through pipeline
        incProgress(0.2, detail = "Analyzing specimens...")
        processed_specimens <- specimen_processor$process_specimens(store$specimen_data)

        if (!is.null(processed_specimens)) {
          # Calculate BAGS grades
          incProgress(0.2, detail = "Calculating BAGS grades...")
          grades <- calculate_bags_grade(processed_specimens)
          if (!is.null(grades)) {
            state$update_state("bags_grades", grades)
          }

          # Auto-select best specimen per BIN + country combination.
          # Only runs when there are no existing selections (i.e. fresh
          # data import), so manual user overrides are preserved.
          incProgress(0.2, detail = "Auto-selecting best specimens...")
          current_selections <- isolate(store$selected_specimens)
          if (is.null(current_selections) || length(current_selections) == 0) {
            auto_selections <- auto_select_best_specimens(processed_specimens)
            if (length(auto_selections) > 0) {
              state$update_state("selected_specimens", auto_selections)
              logging_manager$info("Auto-selected best specimens", list(
                count = length(auto_selections)
              ))
            }
          }

          # Update state with processed data
          incProgress(0.4, detail = "Updating data...")
          state$update_state("specimen_data", processed_specimens)
        }
      }
    )
  })

  # Auto-select the best specimen per BIN + country combination.
  # For each unique (bin_uri, country.ocean) pair, picks the specimen
  # with the highest quality_score. Returns a named list suitable for
  # storing in state$selected_specimens.
  auto_select_best_specimens <- function(specimens) {
    if (is.null(specimens) || nrow(specimens) == 0) return(list())

    # Only consider specimens with valid BIN and species
    valid <- specimens[
      !is.na(specimens$bin_uri) & specimens$bin_uri != "" &
      !is.na(specimens$species) & specimens$species != "",
    ]
    if (nrow(valid) == 0) return(list())

    # Fill missing countries with "Unknown" for grouping
    valid$country_group <- ifelse(
      is.na(valid$country.ocean) | valid$country.ocean == "",
      "Unknown", valid$country.ocean
    )

    # For each BIN + country combo, pick the highest quality_score
    selections <- list()
    combos <- unique(valid[, c("bin_uri", "country_group"), drop = FALSE])

    for (i in seq_len(nrow(combos))) {
      bin <- combos$bin_uri[i]
      country <- combos$country_group[i]

      subset <- valid[valid$bin_uri == bin & valid$country_group == country, ]
      if (nrow(subset) == 0) next

      # Best = highest quality_score, break ties by processid
      subset <- subset[order(-subset$quality_score, subset$processid), ]
      best <- subset[1, ]

      selections[[best$processid]] <- list(
        timestamp = Sys.time(),
        species = best$species,
        quality_score = best$quality_score,
        user = "auto",
        selected = TRUE,
        auto_selected = TRUE
      )
    }

    selections
  }

  # Handle specimen selection across modules.
  # Register one observeEvent per grade (stable IDs) instead of nesting
  # observeEvent inside observe() loops, which would create duplicate
  # handlers every time the outer observer invalidates.
  selection_observers_registered <- FALSE

  observe({
    store <- state$get_store()
    req(store$specimen_data, store$bags_grades)

    # Only register observers once
    if (selection_observers_registered) return()
    selection_observers_registered <<- TRUE

    grades <- c("a", "b", "c", "d", "e")

    for (grade in grades) {
      local({
        grade_local <- grade

        # Watch the BAGS module's selected_specimens reactive for this grade
        bags_module <- bags_grading[[grade_local]]
        if (is.null(bags_module)) return()

        observeEvent(bags_module$selected_specimens(), {
          # Handled via direct state updates in bags module
        }, ignoreInit = TRUE)
      })
    }
  })

  # Single observer for all specimen selections via a shared Shiny input.
  # BAGS grade tables use radio-button inputs whose IDs start with "select_".
  # Listen for any such input change.
  observe({
    store <- state$get_store()
    req(store$specimen_data, store$bags_grades)

    grades <- c("a", "b", "c", "d", "e")
    grade_species_map <- list()

    for (grade in grades) {
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == toupper(grade)]
      for (species in grade_species) {
        input_id <- paste0("select_", grade, "_", make.names(species))
        # Check if this input exists and has a value
        val <- input[[input_id]]
        if (!is.null(val) && nchar(val) > 0) {
          grade_species_map[[input_id]] <- list(
            processid = val,
            species = species,
            grade = grade
          )
        }
      }
    }

    # Process all current selections
    if (length(grade_species_map) > 0) {
      isolate({
        current_selections <- state$get_store()$selected_specimens
        if (is.null(current_selections)) current_selections <- list()

        for (info in grade_species_map) {
          # Remove previous representative for this species
          for (pid in names(current_selections)) {
            entry <- current_selections[[pid]]
            if (is.list(entry) && identical(entry$species, info$species)) {
              current_selections[[pid]] <- NULL
            }
          }

          # Get specimen metadata
          specimen_row <- store$specimen_data[store$specimen_data$processid == info$processid, ]

          current_selections[[info$processid]] <- list(
            timestamp = Sys.time(),
            species = info$species,
            quality_score = if (nrow(specimen_row) > 0) specimen_row$quality_score[1] else NA,
            user = store$user_info$name %||% store$user_info$email,
            selected = TRUE
          )
        }

        state$update_state("selected_specimens", current_selections)
      })
    }
  })

  # Watch for required user info and enable/disable UI accordingly
  observe({
    store <- state$get_store()
    user_info <- store$user_info

    # Check if API key and user info are provided
    has_api_key <- !is.null(user_info$bold_api_key) && nchar(user_info$bold_api_key) > 0
    has_user_info <- !is.null(user_info$email) || !is.null(user_info$name)

    # Check if any input data is present
    input_present <- !is.null(input$`data_import-taxa_input`) &&
      nchar(trimws(input$`data_import-taxa_input`)) > 0 ||
      !is.null(input$`data_import-dataset_codes`) &&
      nchar(trimws(input$`data_import-dataset_codes`)) > 0 ||
      !is.null(input$`data_import-project_codes`) &&
      nchar(trimws(input$`data_import-project_codes`)) > 0 ||
      !is.null(input$`data_import-countries`) &&
      nchar(trimws(input$`data_import-countries`)) > 0

    if (has_api_key && has_user_info && input_present) {
      shinyjs::enable("data_import-submit")
      shinyjs::enable("bin_analysis-analyze")
    } else {
      shinyjs::disable("data_import-submit")
      shinyjs::disable("bin_analysis-analyze")
    }
  })

  # Handle global error state
  observe({
    store <- state$get_store()
    if (store$error$has_error) {
      showNotification(
        store$error$message,
        type = "error",
        duration = NULL
      )
      logging_manager$log_action(
        session_id = session$token,
        action_type = "error",
        process_ids = character(0),
        metadata = list(
          message = store$error$message,
          details = store$error$details,
          source = store$error$source
        )
      )
    }
  })

  # Build a deterministic session ID from user info for persistence.
  # Uses a hash of the user's primary identifier so that:
  #   1. The same user always maps to the same session directory
  #      (saves overwrite rather than creating duplicates).
  #   2. Raw emails/names are not exposed in the filesystem (privacy).
  get_session_id <- function(store) {
    user_info <- store$user_info
    user_id <- NULL
    if (!is.null(user_info$email) && nchar(trimws(user_info$email)) > 0) {
      user_id <- tolower(trimws(user_info$email))
    } else if (!is.null(user_info$orcid) && nchar(trimws(user_info$orcid)) > 0) {
      user_id <- trimws(user_info$orcid)
    } else if (!is.null(user_info$name) && nchar(trimws(user_info$name)) > 0) {
      user_id <- tolower(trimws(user_info$name))
    }
    if (!is.null(user_id)) {
      # Deterministic hash — no timestamp, so repeated saves update
      # the same directory instead of creating duplicates.
      digest::digest(user_id, algo = "md5")
    } else {
      session$token
    }
  }

  # Plain (non-reactive) snapshot of state for use in onSessionEnded.
  # onSessionEnded fires AFTER the Shiny session is destroyed, so
  # isolate(state$get_store()) throws "Can't access reactive value
  # outside of reactive consumer".  This observer continuously
  # snapshots the state into a plain list.
  session_snapshot <- new.env(parent = emptyenv())
  session_snapshot$store <- NULL

  observe({
    store <- state$get_store()
    # Touch all keys we need so this observer re-runs on any change
    snapshot <- list(
      specimen_data = store$specimen_data,
      bags_grades = store$bags_grades,
      bin_analysis = store$bin_analysis,
      selected_specimens = store$selected_specimens,
      specimen_flags = store$specimen_flags,
      specimen_curator_notes = store$specimen_curator_notes,
      specimen_updated_ids = store$specimen_updated_ids,
      search_taxa = store$search_taxa,
      user_info = store$user_info
    )
    session_snapshot$store <- snapshot
  })

  # Periodic auto-save every 60 seconds so curator work isn't lost if
  # the browser crashes or the network drops (onSessionEnded is unreliable
  # in those cases).
  auto_save_timer <- reactiveTimer(60000)
  observe({
    auto_save_timer()
    store <- isolate(state$get_store())
    if (is.null(store$specimen_data)) return()

    # Only save if there are annotations worth saving
    has_annotations <- length(store$selected_specimens) > 0 ||
      length(store$specimen_flags) > 0 ||
      length(store$specimen_curator_notes) > 0 ||
      length(store$specimen_updated_ids) > 0
    if (!has_annotations) return()

    tryCatch({
      sid <- get_session_id(store)
      save_session_state(sid, store, con = session_db)
      logging_manager$log_action(
        session_id = sid,
        action_type = "auto_save",
        process_ids = character(0)
      )
    }, error = function(e) {
      logging_manager$log_action(
        session_id = session$token,
        action_type = "auto_save_error",
        process_ids = character(0),
        metadata = list(error = e$message)
      )
    })
  })

  # Save session state on exit using the plain snapshot (not reactive).
  # Do NOT call state$reset_state() — the session is already gone.
  session$onSessionEnded(function() {
    tryCatch({
      store <- session_snapshot$store
      if (!is.null(store) && !is.null(store$specimen_data)) {
        sid <- get_session_id(store)
        save_session_state(sid, store, con = session_db)
        logging_manager$log_action(
          session_id = sid,
          action_type = "session_saved",
          process_ids = character(0)
        )
      }
    }, error = function(e) {
      logging_manager$log_action(
        session_id = session$token,
        action_type = "session_save_error",
        process_ids = character(0),
        metadata = list(error = e$message)
      )
    })

    logging_manager$log_action(
      session_id = session$token,
      action_type = "session_ended",
      process_ids = character(0)
    )
  })

  # Handle session resume via the module's returned reactiveVal
  observeEvent(data_import$selected_session_id(), {
    session_id <- data_import$selected_session_id()
    req(session_id)

    tryCatch({
      saved <- load_session_state(session_id, con = session_db)
      if (!is.null(saved)) {
        for (key in names(saved)) {
          if (key != "metadata" && !is.null(saved[[key]])) {
            state$update_state(key, saved[[key]])
          }
        }

        logging_manager$log_action(
          session_id = session$token,
          action_type = "session_resumed",
          process_ids = character(0),
          metadata = list(
            restored_session = session_id,
            specimen_count = if (!is.null(saved$specimen_data)) nrow(saved$specimen_data) else 0
          )
        )

        showNotification(
          sprintf("Session restored: %d specimens loaded",
                  if (!is.null(saved$specimen_data)) nrow(saved$specimen_data) else 0),
          type = "message"
        )
      }
    }, error = function(e) {
      showNotification(
        sprintf("Failed to restore session: %s", e$message),
        type = "error"
      )
    })
  })
}

# Create required directories
for (dir in c("data", "logs", "output")) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
}

# Set global options
options(
  shiny.maxRequestSize = 30*1024^2,
  timeout = 300,
  scipen = 999,
  shiny.sanitize.errors = FALSE
)

# Run application
shinyApp(ui = ui, server = server)
