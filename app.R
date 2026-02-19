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
    mod_user_info_ui("user_info"),
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("table")),
      menuItem("Species Analysis", tabName = "species_analysis", icon = icon("list-check")),
      menuItem("BIN Analysis", tabName = "bins", icon = icon("dna")),
      menuItem("BAGS Grade A", tabName = "bags_a", icon = icon("star")),
      menuItem("BAGS Grade B", tabName = "bags_b", icon = icon("star-half-alt")),
      menuItem("BAGS Grade C", tabName = "bags_c", icon = icon("exclamation-circle")),
      menuItem("BAGS Grade D", tabName = "bags_d", icon = icon("exclamation-triangle")),
      menuItem("BAGS Grade E", tabName = "bags_e", icon = icon("times-circle")),
      menuItem("Specimens", tabName = "specimens", icon = icon("microscope")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
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
        /* Global Styles */
        .content-wrapper {
          overflow: auto !important;
          padding: 1 !important;
        }
        .small-box {
          margin-bottom: 5px !important;
        }

        /* Hide sidebar toggle button */
        .sidebar-toggle {
          display: none !important;
        }

        /* DataTables Overrides */
        .dataTables_wrapper {
          padding: 0 !important;
          line-height: 1 !important;
        }
        .dataTables_wrapper .dataTables_scroll {
          padding: 0 !important;
          margin: 0 !important;
        }
        .dataTables_scrollBody {
          max-height: 70vh !important;
        }
        .dataTables_wrapper .row {
          margin: 0 !important;
          padding: 2px 0 !important;
        }

        /* Reduce spacing in user info forms */
        .content-wrapper .user-info-container .form-group {
          margin-bottom: 1px !important;
          margin-top: 1px !important;
        }

        .content-wrapper .user-info-container .shiny-input-container {
          margin: 1px !important;
          padding: 1px !important;
        }

        .content-wrapper .user-info-container .form-control {
          margin: 1px !important;
          padding: 1px 1px !important;
          height: 1px !important;
        }

        .content-wrapper .user-info-container .box-body {
          padding: 1px !important;
        }

        /* Compact shinydashboard boxes */
        .content-wrapper .box {
          margin-bottom: 5px !important;
        }

        .content-wrapper .box-header {
          padding: 5px 5px !important;
        }

        .content-wrapper .box-body {
          padding: 5px !important;
        }
        ",
        get_table_css()
      ))),
    ),

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
              mod_specimen_handling_ui("specimen_handling")),

      tabItem(tabName = "about",
              box(title = "BOLDcuratoR",
                  width = 12,
                  includeMarkdown("about.md")))
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
        incProgress(0.3, detail = "Analyzing specimens...")
        processed_specimens <- specimen_processor$process_specimens(store$specimen_data)

        if (!is.null(processed_specimens)) {
          # Calculate BAGS grades
          incProgress(0.3, detail = "Calculating BAGS grades...")
          grades <- calculate_bags_grade(processed_specimens)
          if (!is.null(grades)) {
            state$update_state("bags_grades", grades)
          }

          # Update state with processed data
          incProgress(0.4, detail = "Updating data...")
          state$update_state("specimen_data", processed_specimens)
        }
      }
    )
  })

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
            user = store$user_info$email,
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
      length(input$`data_import-continents`) > 0

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

  # Build a session ID from user info for persistence.
  # Deterministic so sessions can be found by email/ORCID/name later.
  get_session_id <- function(store) {
    user_info <- store$user_info
    user_id <- NULL
    if (!is.null(user_info$email) && nchar(trimws(user_info$email)) > 0) {
      user_id <- tolower(trimws(user_info$email))
    } else if (!is.null(user_info$orcid) && nchar(trimws(user_info$orcid)) > 0) {
      user_id <- trimws(user_info$orcid)
    } else if (!is.null(user_info$name) && nchar(trimws(user_info$name)) > 0) {
      user_id <- make.names(trimws(user_info$name))
    }
    if (!is.null(user_id)) {
      safe_id <- gsub("[^A-Za-z0-9._@-]", "_", user_id)
      sprintf("%s_%s", safe_id, format(Sys.time(), "%Y%m%d_%H%M%S"))
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
      length(store$specimen_curator_notes) > 0
    if (!has_annotations) return()

    tryCatch({
      sid <- get_session_id(store)
      save_session_state(sid, store)
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
        save_session_state(sid, store)
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
      saved <- load_session_state(session_id)
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
