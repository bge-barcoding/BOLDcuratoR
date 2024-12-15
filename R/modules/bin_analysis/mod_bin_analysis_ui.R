# R/modules/bin_analysis/mod_bin_analysis_ui.R

#' UI Module for BIN Analysis
#' @param id The module ID
#' @export
mod_bin_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Processing Status Section
    uiOutput(ns("processing_status")),

    # Main Analysis Section
    fluidRow(
      box(
        title = "BIN Analysis Dashboard",
        status = "primary",
        width = 12,
        solidHeader = TRUE,

        # Summary Statistics
        fluidRow(
          valueBoxOutput(ns("total_bins_box"), width = 3),
          valueBoxOutput(ns("concordant_bins_box"), width = 3),
          valueBoxOutput(ns("discordant_bins_box"), width = 3),
          valueBoxOutput(ns("shared_bins_box"), width = 3)
        ),

        # Main Content Tabs
        tabBox(
          id = ns("analysis_tabs"),
          width = 12,

          tabPanel(
            "BIN Summary",
            div(
              class = "actions-bar",
              style = "margin-bottom: 15px;",
              downloadButton(
                ns("download_analysis"),
                "Download Analysis",
                class = "btn-success"
              )
            ),
            div(
              class = "table-responsive",
              DTOutput(ns("bin_summary_table"))
            )
          ),

          tabPanel(
            "BIN Content Details",
            div(
              class = "table-responsive",
              DTOutput(ns("bin_content_table"))
            )
          ),

          tabPanel(
            "BIN Statistics",
            div(
              class = "table-responsive",
              DTOutput(ns("bin_stats_table"))
            )
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
            tags$h4("BIN Analysis Help", class = "modal-title"),
            tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              "×"
            )
          ),
          tags$div(
            class = "modal-body",
            tags$ul(
              tags$li("BIN Summary shows overview statistics for each species"),
              tags$li("Content Details provides specimen-level BIN information"),
              tags$li("Statistics tab shows aggregate BIN metrics"),
              tags$li("Use Download Analysis to export all results")
            )
          ),
          tags$div(
            class = "modal-footer",
            tags$button(
              type = "button",
              class = "btn btn-default",
              `data-dismiss` = "modal",
              "Close"
            )
          )
        )
      )
    ),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        .table-responsive {
          overflow-x: auto;
          margin-top: 15px;
        }

        .dataTables_wrapper {
          padding: 15px 0;
        }

        .actions-bar {
          padding: 10px;
          background-color: #f8f9fa;
          border-radius: 4px;
        }

        .processing-status {
          margin-bottom: 15px;
          padding: 10px 15px;
          border-radius: 4px;
          background-color: #e9ecef;
        }

        .value-box {
          cursor: default;
        }

        .concordant {
          background-color: #d4edda !important;
        }

        .discordant {
          background-color: #f8d7da !important;
        }

        .btn {
          margin-right: 5px;
        }
      "))
    )
  )
}

#' Create Navigation Panel for BIN Analysis
#' @param id The module ID
#' @export
mod_bin_analysis_nav <- function(id) {
  ns <- NS(id)
  menuItem(
    "BIN Analysis",
    tabName = "bins",
    icon = icon("dna")
  )
}

#' Create Help Button Panel
#' @param id The module ID
#' @export
mod_bin_analysis_help <- function(id) {
  ns <- NS(id)
  absolutePanel(
    id = ns("help_panel"),
    class = "panel panel-default",
    style = "position: fixed; bottom: 10px; right: 10px; width: auto;",
    actionButton(
      ns("show_help"),
      "Help",
      icon = icon("question-circle"),
      class = "btn-info"
    )
  )
}
