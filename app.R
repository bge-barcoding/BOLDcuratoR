# BOLDcuratoR/app.R

# Source required modules
source("R/config/constants.R")

# Source utility functions
source("R/utils/bags_grading.R")
source("R/utils/specimen_ranking.R")
source("R/utils/specimen_scoring.R")
source("R/utils/specimen_validation.R")
source("R/utils/table_utils.R")

# Source core modules
source("R/modules/interfaces.R")
source("R/modules/state/state_manager.R")
source("R/modules/logging/mod_logging.R")

# Source primary modules
source("R/modules/api_integration/mod_api_integration.R")
source("R/modules/data_import/mod_data_import_ui.R")
source("R/modules/data_import/mod_data_import_server.R")
source("R/modules/data_import/mod_data_import_utils.R")

# Source analysis modules
source("R/modules/specimen_handling/specimen_processor.R")
source("R/modules/specimen_handling/specimen_scorer.R")
source("R/modules/specimen_handling/specimen_validator.R")
source("R/modules/specimen_handling/mod_specimen_handling_server.R")
source("R/modules/specimen_handling/mod_specimen_handling_ui.R")

source("R/modules/bin_analysis/mod_bin_analysis_ui.R")
source("R/modules/bin_analysis/mod_bin_analysis_server.R")
source("R/modules/bin_analysis/mod_bin_analysis_utils.R")

source("R/modules/bags_grading/mod_bags_grading_ui.R")
source("R/modules/bags_grading/mod_bags_grading_server.R")
source("R/modules/bags_grading/mod_bags_grading_utils.R")

source("R/modules/haplotype_analysis/haplotype_manager.R")
source("R/modules/haplotype_analysis/sequence_aligner.R")
source("R/modules/haplotype_analysis/mod_haplotype_analysis_ui.R")
source("R/modules/haplotype_analysis/mod_haplotype_analysis_server.R")
source("R/modules/haplotype_analysis/mod_haplotype_analysis_utils.R")

# Source export and history modules
source("R/modules/export/mod_export.R")
source("R/modules/export_history/mod_export_history_ui.R")
source("R/modules/export_history/mod_export_history_server.R")

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
      menuItem("BIN Analysis", tabName = "bins", icon = icon("dna")),
      menuItem("Specimens", tabName = "specimens", icon = icon("microscope")),
      menuItem("BAGS Grade A", tabName = "bags_a", icon = icon("star")),
      menuItem("BAGS Grade B", tabName = "bags_b", icon = icon("star-half-alt")),
      menuItem("BAGS Grade C", tabName = "bags_c", icon = icon("exclamation-circle")),
      menuItem("BAGS Grade D", tabName = "bags_d", icon = icon("exclamation-triangle")),
      menuItem("BAGS Grade E", tabName = "bags_e", icon = icon("times-circle")),
      menuItem("Haplotype Analysis", tabName = "haplotypes", icon = icon("dna")),
      menuItem("Export History", tabName = "export_history", icon = icon("history")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { overflow: auto; }
        .table-wrapper { overflow-x: auto; }
        .small-box { margin-bottom: 15px; }
        .failed-query { background-color: #fff3cd !important; }
        .datatable .failed-query:hover { background-color: #ffe7b6 !important; }
        .datatable { width: 100% !important; font-size: 12px !important; }
        .datatable td { padding: 4px 8px !important; white-space: nowrap; }
        .datatable th { padding: 5px 8px !important; white-space: nowrap; font-size: 13px !important; }
      "))
    ),

    tabItems(
      tabItem(tabName = "input",
              mod_data_import_ui("data_import")),

      tabItem(tabName = "bins",
              mod_bin_analysis_ui("bin_analysis")),

      tabItem(tabName = "specimens",
              mod_specimen_handling_ui("specimen_handling")),

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

      tabItem(tabName = "haplotypes",
              mod_haplotype_analysis_ui("haplotype")),

      tabItem(tabName = "export_history",
              mod_export_history_ui("export_history")),

      tabItem(tabName = "about",
              box(title = "BOLDcuratoR",
                  width = 12,
                  includeMarkdown("about.md")))
    )
  )
)

# Server Definition
server <- function(input, output, session) {

  # Initialize core components
  logging_manager <- LoggingManager$new()
  state <- StateManager$new(session, logging_manager)
  export_manager <- ExportManager$new(logging_manager, session$token)

  # Initialize validator and processors
  validator <- SpecimenValidator$new(logging_manager)
  haplotype_manager <- HaplotypeManager$new(state, logging_manager)
  bags_processor <- BagsProcessor$new(validator, logging_manager)

  # Initialize modules
  api_integration <- mod_api_integration_server(
    "api",
    state = state
  )

  user_info <- mod_user_info_server(
    "user_info",
    state = state,
    logger = logging_manager
  )

  data_import <- mod_data_import_server(
    "data_import",
    api_integration = api_integration,
    state = state,
    logger = logging_manager
  )

  specimen_handling <- mod_specimen_handling_server(
    "specimen_handling",
    state = state,
    logger = logging_manager
  )

  bin_analysis <- mod_bin_analysis_server(
    "bin_analysis",
    state = state,
    logger = logging_manager
  )

  # Initialize BAGS grading modules
  bags_grading <- list(
    a = mod_bags_grading_server("bags_a", state, grade = "A"),
    b = mod_bags_grading_server("bags_b", state, grade = "B"),
    c = mod_bags_grading_server("bags_c", state, grade = "C"),
    d = mod_bags_grading_server("bags_d", state, grade = "D"),
    e = mod_bags_grading_server("bags_e", state, grade = "E")
  )

  haplotype <- mod_haplotype_analysis_server(
    "haplotype",
    state = state,
    logger = logging_manager
  )

  export_history <- mod_export_history_server(
    "export_history",
    state = state
  )

  # Process specimens and run analyses after data import
  observe({
    store <- state$get_store()
    req(store$specimen_data)

    # Process specimens through pipeline
    processed_specimens <- specimen_handling$processor$process_specimens(store$specimen_data)

    if (!is.null(processed_specimens)) {
      # Run BIN analysis
      bin_results <- bin_analysis$processor$analyze_bins(processed_specimens)
      if (!is.null(bin_results)) {
        state$update_state("bin_analysis", bin_results, validate_bin_analysis)
      }

      # Calculate BAGS grades
      grades <- bags_processor$process_grades(processed_specimens)
      if (!is.null(grades)) {
        state$update_state("bags_grades", grades, validate_bags_grades)
      }

      # Update state with processed data
      state$update_state("specimen_data", processed_specimens, validate_specimen_data)
    }
  })

  # Handle specimen selection across modules
  observe({
    store <- state$get_store()
    req(store$specimen_data, store$bags_grades)

    grades <- c("a", "b", "c", "d", "e")

    for(grade in grades) {
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == toupper(grade)]

      for(species in grade_species) {
        local({
          species_local <- species
          grade_local <- grade

          input_id <- paste0("select_", grade_local, "_", make.names(species_local))

          observeEvent(input[[input_id]], {
            selected_processid <- input[[input_id]]

            # Update selected specimens
            current_selections <- store$selected_specimens
            current_selections[[species_local]] <- selected_processid

            state$update_state(
              "selected_specimens",
              current_selections,
              validate_selected_specimens
            )

            # Log selection
            user_info <- store$user_info
            logging_manager$log_action(
              user_email = user_info$email,
              user_name = user_info$name,
              session_id = session$token,
              action_type = "specimen_selected",
              process_ids = selected_processid,
              metadata = list(
                species = species_local,
                grade = grade_local,
                quality_score = store$specimen_data[store$specimen_data$processid == selected_processid,]$quality_score,
                specimen_rank = store$specimen_data[store$specimen_data$processid == selected_processid,]$specimen_rank
              )
            )
          })
        })
      }
    }
  })

  # Watch for API key changes and update UI state
  observe({
    api_status <- api_integration$get_status()
    store <- state$get_store()

    # Check if at least email or name is provided
    has_user_info <- !is.null(store$user_info$email) || !is.null(store$user_info$name)

    # Check if any input data is present
    input_present <- !is.null(input$`data_import-taxa_input`) &&
      nchar(trimws(input$`data_import-taxa_input`)) > 0 ||
      !is.null(input$`data_import-dataset_codes`) &&
      nchar(trimws(input$`data_import-dataset_codes`)) > 0 ||
      !is.null(input$`data_import-project_codes`) &&
      nchar(trimws(input$`data_import-project_codes`)) > 0 ||
      length(input$`data_import-continents`) > 0

    if (api_status$is_set && has_user_info && input_present) {
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

  # Session cleanup
  session$onSessionEnded(function() {
    state$reset_state()
    logging_manager$log_action(
      session_id = session$token,
      action_type = "session_ended",
      process_ids = character(0)
    )
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
  shiny.maxRequestSize = 30*1024^2,  # 30MB max request size
  timeout = 300,                      # 5-minute timeout
  scipen = 999,                       # Avoid scientific notation
  shiny.sanitize.errors = FALSE       # Show detailed error messages in development
)

# Run application
shinyApp(ui = ui, server = server)
