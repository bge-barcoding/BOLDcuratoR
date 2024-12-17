# R/modules/data_import/mod_data_import_ui.R

#' UI Module for Data Import
#' @param id The module ID
#' @export
mod_data_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Error/Warning Alert Box
    uiOutput(ns("alert_box")),

    # Search Progress Section
    conditionalPanel(
      condition = sprintf("input['%s'] === true", ns("show_progress")),
      box(
        title = "Search Progress",
        status = "info",
        width = 12,
        solidHeader = TRUE,
        uiOutput(ns("progress_status"))
      )
    ),

    # Main Input Box
    fluidRow(
      column(12,
             box(
               title = "Data Input",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,

               # Input Controls
               fluidRow(
                 column(6,
                        textAreaInput(ns("taxa_input"),
                                      "Enter taxa (one per line, comma-separated for synonyms):",
                                      rows = 8,
                                      placeholder = "Valid name 1, Synonym A, Synonym B\nValid name 2, Synonym C"
                        )
                 ),
                 column(6,
                        textAreaInput(ns("dataset_codes"),
                                      "Dataset codes (one per line):",
                                      rows = 4,
                                      placeholder = "DS-EXAMPLE1\nDS-EXAMPLE2"
                        ),
                        textAreaInput(ns("project_codes"),
                                      "Project codes (one per line):",
                                      rows = 4,
                                      placeholder = "PROJECT1\nPROJECT2"
                        )
                 )
               ),

               # Geographic Filters
               box(
                 title = "Geographic Filters",
                 status = "info",
                 solidHeader = TRUE,
                 width = NULL,
                 collapsible = TRUE,

                 checkboxGroupInput(ns("continents"),
                                    "Select Continents:",
                                    choices = names(CONTINENT_COUNTRIES),
                                    inline = TRUE
                 ),

                 textAreaInput(ns("countries"),
                               "Additional Countries (one per line):",
                               rows = 3,
                               placeholder = "Enter additional countries not covered by continent selection"
                 ),

                 div(
                   id = ns("url_warning"),
                   style = "display: none;",
                   tags$p(
                     class = "text-warning",
                     icon("warning"),
                     "Warning: Query length approaching limit. Consider reducing selected regions."
                   )
                 ),

                 helpText("Note: Selecting multiple continents will include all countries from each selected continent.")
               ),

               # Action Buttons
               div(
                 style = "margin-top: 15px;",
                 actionButton(ns("submit"),
                              "Get Data",
                              class = "btn-primary",
                              icon = icon("search")
                 ),
                 actionButton(ns("clear_input"),
                              "Clear Input",
                              class = "btn-warning",
                              icon = icon("eraser")
                 )
               )
             )
      )
    ),

    # Results Section
    fluidRow(
      box(
        title = "Search Results",
        status = "info",
        solidHeader = TRUE,
        width = 12,

        # Results Controls
        div(
          style = "margin-bottom: 15px;",
          downloadButton(ns("download_csv"),
                         "Download Results",
                         class = "btn-success"
          ),
          actionButton(ns("clear_results"),
                       "Clear Results",
                       class = "btn-warning",
                       icon = icon("trash")
          )
        ),

        # Status boxes
        fluidRow(
          valueBoxOutput(ns("total_records_box"), width = 3),
          valueBoxOutput(ns("unique_taxa_box"), width = 3),
          valueBoxOutput(ns("unique_bins_box"), width = 3),
          valueBoxOutput(ns("countries_box"), width = 3)
        ),

        # Results table
        div(
          class = "table-wrapper",
          DTOutput(ns("results_table"))
        )
      )
    ),

    # Help Modal
    tags$div(
      id = ns("help_modal"),
      class = "modal fade",
      tags$div(
        class = "modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(
            class = "modal-header",
            tags$h4("Data Import Help", class = "modal-title"),
            tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×")
          ),
          tags$div(
            class = "modal-body",
            tags$ul(
              tags$li("Enter taxa names, one per line. Include synonyms separated by commas."),
              tags$li("Dataset codes should be in the format DS-XXXX"),
              tags$li("Select continents or enter specific countries for geographic filtering"),
              tags$li("Search results can be downloaded in CSV format"),
              tags$li("Clear results before starting a new search")
            )
          ),
          tags$div(
            class = "modal-footer",
            tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Close")
          )
        )
      )
    ),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        .table-wrapper {
          overflow-x: auto;
          margin-top: 15px;
        }
        .alert {
          margin-top: 15px;
        }
        .progress-status {
          margin: 10px 0;
        }
        .btn {
          margin-right: 5px;
        }
        .validation-message {
          color: #dc3545;
          margin-top: 5px;
        }
        #url_warning {
          margin-top: 10px;
          padding: 10px;
          border-radius: 4px;
          background-color: #fff3cd;
        }
      "))
    )
  )
}

#' Create Navigation Panel for Data Import
#' @param id The module ID
#' @export
mod_data_import_nav <- function(id) {
  ns <- NS(id)
  menuItem(
    "Data Input",
    tabName = "input",
    icon = icon("table")
  )
}
