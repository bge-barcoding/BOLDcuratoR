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
                   ),
                   downloadButton(ns("download_annotated"),
                                  "Download Annotated Records",
                                  class = "btn-warning"
                   ),
                   downloadButton(ns("download_curation_report"),
                                  "Download BOLD Curation Report",
                                  class = "btn-primary"
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
          DTOutput(ns("specimen_table"))
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
