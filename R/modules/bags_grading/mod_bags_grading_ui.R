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
                               "E" = "danger"),
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

    # Main Content Section
    fluidRow(
      column(12,
             box(
               title = "Specimen Data",
               status = "primary",
               width = NULL,
               solidHeader = TRUE,

               # Table Controls
               div(
                 class = "table-controls",
                 style = "margin-bottom: 15px;",

                 # Download Button
                 downloadButton(
                   ns("download_data"),
                   "Download Data",
                   class = "btn-success"
                 ),

                 # Help Button
                 actionButton(
                   ns("show_help"),
                   "Help",
                   class = "btn-info",
                   icon = icon("question-circle")
                 )
               ),

               # Specimen Tables
               div(
                 class = "specimen-tables",
                 uiOutput(ns("specimen_tables"))
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
            tags$h4(paste("BAGS Grade", grade, "Help"), class = "modal-title"),
            tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×")
          ),
          tags$div(
            class = "modal-body",
            switch(grade,
                   "A" = tagList(
                     tags$p("Grade A species have:"),
                     tags$ul(
                       tags$li("More than 10 specimens"),
                       tags$li("A single BIN"),
                       tags$li("No taxonomic discordance")
                     )
                   ),
                   "B" = tagList(
                     tags$p("Grade B species have:"),
                     tags$ul(
                       tags$li("3-10 specimens"),
                       tags$li("A single BIN"),
                       tags$li("No taxonomic discordance")
                     )
                   ),
                   "C" = tagList(
                     tags$p("Grade C species have:"),
                     tags$ul(
                       tags$li("Multiple BINs"),
                       tags$li("No taxonomic discordance within BINs")
                     )
                   ),
                   "D" = tagList(
                     tags$p("Grade D species have:"),
                     tags$ul(
                       tags$li("Fewer than 3 specimens"),
                       tags$li("A single BIN")
                     )
                   ),
                   "E" = tagList(
                     tags$p("Grade E species have:"),
                     tags$ul(
                       tags$li("Taxonomic discordance within BINs"),
                       tags$li("Color coding indicates different species within shared BINs")
                     )
                   )
            ),
            hr(),
            tags$p("Table Features:"),
            tags$ul(
              tags$li("Use checkboxes to select representative specimens"),
              tags$li("Use flag dropdown to mark potential issues"),
              tags$li("First two columns (selection and flags) are frozen"),
              tags$li("Scroll horizontally to see all specimen data")
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

        /* Value Boxes */
        .small-box {
          margin-bottom: 20px;
        }

        /* Modal Styling */
        .modal-content {
          border-radius: 6px;
        }

        .modal-header {
          background-color: #f8f9fa;
          border-radius: 6px 6px 0 0;
        }

        .modal-body {
          padding: 20px;
        }

        .modal-body ul {
          padding-left: 20px;
          margin-bottom: 15px;
        }

        .modal-body li {
          margin-bottom: 8px;
        }

        /* Table Controls */
        .table-controls {
          padding: 15px;
          background-color: #f8f9fa;
          border-radius: 4px;
          margin-bottom: 20px;
        }

        .table-controls .btn {
          margin-right: 10px;
        }

        /* Responsive Design */
        @media (max-width: 768px) {
          .value-box {
            text-align: center;
          }

          .specimen-flag {
            min-width: 100px;
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
