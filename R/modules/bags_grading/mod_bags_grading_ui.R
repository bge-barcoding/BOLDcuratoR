# R/modules/bags_grading/mod_bags_grading_ui.R

#' UI Module for BAGS Grading
#' @param id The module ID
#' @param grade BAGS grade to display (A-E)
#' @export
mod_bags_grading_ui <- function(id, grade) {
  ns <- NS(id)

  tagList(
    # Error/Status Container
    uiOutput(ns("status_container")),

    # Grade Description Box
    fluidRow(
      column(12,
             box(
               title = paste("BAGS Grade", grade, "Overview"),
               status = switch(grade,
                               "A" = "success",
                               "B" = "info",
                               "C" = "warning",
                               "D" = "danger",
                               "E" = "danger"
               ),
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
                        "E" = "Species with discordant taxonomy in BINs"
                 )
               ),

               # Summary Metrics
               fluidRow(
                 valueBoxOutput(ns("species_count_box"), width = 3),
                 valueBoxOutput(ns("specimen_count_box"), width = 3),
                 valueBoxOutput(ns("bin_count_box"), width = 3),
                 valueBoxOutput(ns("selection_count_box"), width = 3)
               )
             )
      )
    ),

    # Filter Controls
    fluidRow(
      box(
        title = "Filter Controls",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,

        fluidRow(
          column(4,
                 selectInput(ns("rank_filter"),
                             "Filter by Rank:",
                             choices = c("All", "1", "2", "3", "4", "5", "6", "7"),
                             selected = "All"
                 )
          ),
          column(4,
                 numericInput(ns("min_quality_score"),
                              "Minimum Quality Score:",
                              value = 0,
                              min = 0,
                              max = 14,
                              step = 1
                 )
          ),
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

        fluidRow(
          column(12,
                 div(
                   style = "margin-top: 15px;",
                   actionButton(ns("reset_filters"),
                                "Reset Filters",
                                class = "btn-warning",
                                icon = icon("undo")
                   ),
                   downloadButton(ns("download_data"),
                                  "Download Data",
                                  class = "btn-success"
                   ),
                   actionButton(ns("show_help"),
                                "Help",
                                class = "btn-info",
                                icon = icon("question-circle")
                   )
                 )
          )
        )
      )
    ),

    # Main Content Section
    fluidRow(
      box(
        title = "Specimen Data",
        status = "primary",
        width = 12,
        solidHeader = TRUE,

        # Specimen Tables
        div(
          class = "specimen-tables",
          style = "margin-top: 20px;",
          uiOutput(ns("specimen_tables"))
        )
      )
    ),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Table Layout */
        .specimen-tables {
          margin-top: 20px;
        }

        .specimen-tables h3 {
          margin-top: 30px;
          margin-bottom: 15px;
          padding-bottom: 10px;
          border-bottom: 1px solid #dee2e6;
          color: #495057;
        }

        .specimen-tables h4 {
          margin-top: 20px;
          margin-bottom: 10px;
          color: #6c757d;
        }

        /* Table Styling */
        .datatable {
          width: 100% !important;
          margin-bottom: 20px !important;
          border: 1px solid #dee2e6;
          border-radius: 4px;
        }

        .datatable thead th {
          background-color: #f8f9fa;
          border-bottom: 2px solid #dee2e6;
          padding: 12px 8px !important;
          font-weight: 600;
        }

        .datatable tbody td {
          padding: 8px !important;
          vertical-align: middle !important;
        }

        /* Fixed Columns */
        .DTFC_LeftWrapper {
          border-right: 2px solid #dee2e6;
          background-color: #fff;
        }

        .DTFC_LeftHeadWrapper {
          border-bottom: 2px solid #dee2e6;
        }

        /* Selection and Flag Controls */
        .specimen-select {
          width: 20px;
          height: 20px;
          cursor: pointer;
        }

        .specimen-flag {
          width: 100%;
          min-width: 120px;
          padding: 4px;
          border: 1px solid #ced4da;
          border-radius: 4px;
        }

        /* Grade-specific Styling */
        .grade-description {
          padding: 15px;
          background-color: #f8f9fa;
          border-radius: 4px;
          margin-bottom: 20px;
        }

        /* Status Messages */
        .status-message {
          padding: 10px 15px;
          margin-bottom: 15px;
          border-radius: 4px;
        }

        .status-message.info {
          background-color: #d1ecf1;
          border-color: #bee5eb;
        }

        .status-message.error {
          background-color: #f8d7da;
          border-color: #f5c6cb;
        }

        /* Button Styling */
        .action-buttons {
          margin: 15px 0;
        }

        .action-buttons .btn {
          margin-right: 10px;
        }

        /* Value Boxes */
        .small-box {
          margin-bottom: 20px;
        }

        /* Responsive Design */
        @media (max-width: 768px) {
          .specimen-flag {
            min-width: 100px;
          }

          .value-box {
            text-align: center;
          }
        }
      "))
    )
  )
}

#' Create Navigation Panel for BAGS Grading
#' @param id The module ID
#' @param grade BAGS grade
#' @export
mod_bags_grading_nav <- function(id, grade) {
  ns <- NS(id)
  menuItem(
    paste("BAGS Grade", grade),
    tabName = paste0("bags_", tolower(grade)),
    icon = icon(switch(grade,
                       "A" = "trophy",
                       "B" = "medal",
                       "C" = "exclamation-circle",
                       "D" = "exclamation-triangle",
                       "E" = "times-circle"
    ))
  )
}
