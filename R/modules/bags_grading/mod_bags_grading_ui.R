# R/modules/bags_grading/mod_bags_grading_ui.R

#' UI Module for BAGS Grading
#' @param id The module ID
#' @param grade BAGS grade to display (A-E)
#' @export
mod_bags_grading_ui <- function(id, grade) {
  ns <- NS(id)

  tagList(
    # Grade Summary Section
    fluidRow(
      column(12,
             box(
               title = paste("BAGS Grade", grade, "Overview"),
               status = "primary",
               width = NULL,
               solidHeader = TRUE,

               # Grade Description
               div(
                 class = "grade-description",
                 style = "margin-bottom: 15px;",
                 switch(grade,
                        "A" = "Species with >10 specimens and a single BIN",
                        "B" = "Species with 3-10 specimens and a single BIN",
                        "C" = "Species with multiple BINs",
                        "D" = "Species with <3 specimens and a single BIN",
                        "E" = "Species sharing BINs with other species"
                 )
               ),

               # Summary Metrics
               fluidRow(
                 column(3, valueBoxOutput(ns("grade_summary_box"), width = NULL)),
                 column(3, valueBoxOutput(ns("selected_count_box"), width = NULL)),
                 column(3, valueBoxOutput(ns("avg_quality_box"), width = NULL)),
                 column(3, valueBoxOutput(ns("countries_box"), width = NULL))
               )
             )
      )
    ),

    # Filtering Controls
    fluidRow(
      column(12,
             box(
               title = "Filter Options",
               status = "info",
               width = NULL,
               solidHeader = TRUE,
               collapsible = TRUE,

               fluidRow(
                 column(3,
                        selectInput(ns("rank_filter"),
                                    "Minimum Specimen Rank:",
                                    choices = c("All", "1", "2", "3", "4", "5", "6"),
                                    selected = "All"
                        )
                 ),
                 column(3,
                        sliderInput(ns("quality_filter"),
                                    "Minimum Quality Score:",
                                    min = 0, max = 14,
                                    value = 0, step = 1
                        )
                 ),
                 column(6,
                        checkboxGroupInput(ns("criteria_filter"),
                                           "Required Criteria:",
                                           choices = c(
                                             "Has Type Specimen" = "TYPE_SPECIMEN",
                                             "Has Image" = "HAS_IMAGE",
                                             "Has Sequence" = "SEQ_QUALITY",
                                             "Has Collection Data" = "COLLECTION_DATE"
                                           ),
                                           inline = TRUE
                        )
                 )
               )
             )
      )
    ),

    # Main Content
    fluidRow(
      column(12,
             box(
               title = "Species List",
               status = "primary",
               width = NULL,
               solidHeader = TRUE,
               DTOutput(ns("specimen_table"))
             )
      )
    ),

    # Help Modal
    tags$div(
      id = ns("help_modal"),
      class = "modal fade",
      div(
        class = "modal-dialog",
        div(
          class = "modal-content",
          div(
            class = "modal-header",
            tags$h4(class = "modal-title", paste("BAGS Grade", grade, "Help")),
            tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×")
          ),
          div(
            class = "modal-body",
            includeMarkdown(system.file("help", paste0("grade_", tolower(grade), "_help.md"),
                                        package = "BOLDcuratoR"))
          ),
          div(
            class = "modal-footer",
            tags$button(type = "button", class = "btn btn-default",
                        `data-dismiss` = "modal", "Close"
            )
          )
        )
      )
    ),

    # Help Button
    absolutePanel(
      id = ns("help_panel"),
      class = "panel panel-default",
      style = "position: fixed; bottom: 10px; right: 10px; width: auto;",
      actionButton(ns("show_help"),
                   "Help",
                   icon = icon("question-circle"),
                   class = "btn-info"
      )
    ),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        .table-wrapper {
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

        /* Selection Styling */
        .selected-row {
          background-color: #d4edda !important;
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

        /* Button Styling */
        .btn {
          margin-right: 5px;
        }

        /* Filter Box Styling */
        .filter-box {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 4px;
          margin-bottom: 15px;
        }
      "))
    )
  )
}
