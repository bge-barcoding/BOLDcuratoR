# R/modules/specimen_handling/mod_specimen_handling_ui.R

#' UI Module for Specimen Handling
#' @param id The module ID
#' @export
mod_specimen_handling_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Error/Alert Container
    tags$div(
      id = ns("error_container"),
      class = "shiny-notification-error",
      style = "display: none;"
    ),

    # Quality Summary Section
    fluidRow(
      box(
        title = "Specimen Quality Summary",
        status = "info",
        width = 12,
        solidHeader = TRUE,

        fluidRow(
          # Quality Distribution Boxes
          column(2, valueBoxOutput(ns("rank_1_box"), width = NULL)),
          column(2, valueBoxOutput(ns("rank_2_box"), width = NULL)),
          column(2, valueBoxOutput(ns("rank_3_box"), width = NULL)),
          column(2, valueBoxOutput(ns("rank_4_box"), width = NULL)),
          column(2, valueBoxOutput(ns("rank_5_box"), width = NULL)),
          column(2, valueBoxOutput(ns("rank_6_box"), width = NULL))
        )
      )
    ),

    # Filtering Controls
    fluidRow(
      box(
        title = "Quality Filters",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,

        fluidRow(
          # Rank Filter
          column(4,
                 selectInput(ns("rank_filter"),
                             "Filter by Rank:",
                             choices = c("All", "1", "2", "3", "4", "5", "6"),
                             selected = "All"
                 )
          ),
          # Quality Score Filter
          column(4,
                 numericInput(ns("min_quality_score"),
                              "Minimum Quality Score:",
                              value = 0,
                              min = 0,
                              max = 14,
                              step = 1
                 )
          ),
          # Criteria Filter
          column(4,
                 selectInput(ns("criteria_filter"),
                             "Filter by Required Criteria:",
                             choices = c(
                               "Species ID" = "SPECIES_ID",
                               "Type Specimen" = "TYPE_SPECIMEN",
                               "Sequence Quality" = "SEQ_QUALITY",
                               "Public Voucher" = "PUBLIC_VOUCHER",
                               "Has Image" = "HAS_IMAGE",
                               "Identifier" = "IDENTIFIER",
                               "Collection Date" = "COLLECTION_DATE",
                               "Country" = "COUNTRY",
                               "Coordinates" = "COORD"
                             ),
                             multiple = TRUE
                 )
          )
        ),

        # Filter Actions
        fluidRow(
          column(12,
                 div(
                   style = "margin-top: 15px;",
                   actionButton(ns("clear_filters"),
                                "Clear Filters",
                                class = "btn-warning",
                                icon = icon("eraser")
                   ),
                   tags$span(
                     class = "filter-status",
                     textOutput(ns("filter_status"), inline = TRUE)
                   )
                 )
          )
        )
      )
    ),

    # Specimen Records Section
    fluidRow(
      box(
        title = "Specimen Records",
        status = "primary",
        width = 12,
        solidHeader = TRUE,

        # Download Controls
        fluidRow(
          column(12,
                 div(
                   class = "download-controls",
                   style = "float: right; margin-bottom: 10px;",
                   downloadButton(ns("download_filtered"),
                                  "Download Filtered Data",
                                  class = "btn-success"
                   ),
                   downloadButton(ns("download_selected"),
                                  "Download Selected",
                                  class = "btn-info"
                   )
                 )
          )
        ),

        # Main Content
        div(
          class = "specimen-table-container",
          style = "overflow-x: auto;",

          # Processing Status
          conditionalPanel(
            condition = sprintf("input['%s'] === true", ns("show_processing")),
            div(
              class = "processing-status",
              style = "margin-bottom: 15px;",
              uiOutput(ns("processing_status"))
            )
          ),

          # Specimen Table
          DTOutput(ns("specimen_table")),

          # Selection History
          div(
            class = "selection-history",
            style = "margin-top: 20px;",
            h4("Selection History"),
            DTOutput(ns("selection_history"))
          )
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
            tags$h4("Specimen Handling Help", class = "modal-title"),
            tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×")
          ),
          tags$div(
            class = "modal-body",
            tags$ul(
              tags$li("Use filters to narrow down specimens based on rank, quality score, and specific criteria"),
              tags$li("Select specimens by clicking rows in the table"),
              tags$li("The system will automatically choose the highest quality specimen for each species"),
              tags$li("Download filtered or selected data using the buttons above the table"),
              tags$li("View your selection history at the bottom of the page")
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
        /* Table Styles */
        .specimen-table-container {
          margin-top: 15px;
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .datatable {
          width: 100% !important;
          font-size: 13px !important;
        }

        .datatable td {
          padding: 6px 8px !important;
          white-space: nowrap;
        }

        .datatable th {
          padding: 8px !important;
          font-weight: 600 !important;
          background-color: #f8f9fa;
        }

        /* Rank Styling */
        .specimen-rank {
          font-weight: bold;
          padding: 2px 6px;
          border-radius: 3px;
          text-align: center;
        }

        .rank-1, .rank-2 { background-color: #28a745; color: white; }
        .rank-3, .rank-4 { background-color: #17a2b8; color: white; }
        .rank-5, .rank-6 { background-color: #ffc107; color: black; }

        /* Quality Score Styling */
        .quality-score {
          position: relative;
          display: inline-block;
          min-width: 40px;
          text-align: center;
          padding: 2px 6px;
          border-radius: 3px;
        }

        .quality-score::after {
          content: '';
          position: absolute;
          left: 0;
          bottom: 0;
          height: 3px;
          width: var(--score-width);
          background-color: #28a745;
        }

        /* Selection History Styling */
        .selection-history {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 4px;
          margin-top: 20px;
        }

        .selection-history h4 {
          margin-top: 0;
          color: #495057;
        }

        /* Filter Status Styling */
        .filter-status {
          display: inline-block;
          margin-left: 15px;
          color: #6c757d;
          font-style: italic;
        }

        /* Processing Status Styling */
        .processing-status {
          padding: 10px;
          background-color: #e9ecef;
          border-radius: 4px;
          margin-bottom: 15px;
        }

        /* Button Styling */
        .btn {
          margin-right: 5px;
        }

        .download-controls {
          margin-bottom: 15px;
        }

        /* Modal Styling */
        .modal-content {
          border-radius: 6px;
        }

        .modal-header {
          background-color: #f8f9fa;
          border-radius: 6px 6px 0 0;
        }

        .modal-body ul {
          padding-left: 20px;
        }

        .modal-body li {
          margin-bottom: 8px;
        }
      "))
    )
  )
}

#' Create Navigation Panel for Specimen Handling
#' @param id The module ID
#' @export
mod_specimen_handling_nav <- function(id) {
  ns <- NS(id)
  menuItem(
    "Specimens",
    tabName = "specimens",
    icon = icon("microscope")
  )
}

#' Create Help Button Panel
#' @param id The module ID
#' @export
mod_specimen_handling_help <- function(id) {
  ns <- NS(id)
  absolutePanel(
    id = ns("help_panel"),
    class = "panel panel-default",
    style = "position: fixed; bottom: 10px; right: 10px; width: auto;",
    actionButton(ns("show_help"),
                 "Help",
                 icon = icon("question-circle"),
                 class = "btn-info"
    )
  )
}
