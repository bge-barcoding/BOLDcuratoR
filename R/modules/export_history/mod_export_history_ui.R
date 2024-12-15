# R/modules/export_history/mod_export_history_ui.R

#' UI Module for Export History
#' @param id The module ID
#' @export
mod_export_history_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "Export Statistics",
        status = "primary",
        width = 12,
        solidHeader = TRUE,

        fluidRow(
          valueBoxOutput(ns("total_exports_box"), width = 3),
          valueBoxOutput(ns("success_rate_box"), width = 3),
          valueBoxOutput(ns("records_exported_box"), width = 3),
          valueBoxOutput(ns("avg_size_box"), width = 3)
        )
      )
    ),

    fluidRow(
      box(
        title = "Export History",
        status = "primary",
        width = 12,
        solidHeader = TRUE,

        fluidRow(
          column(3,
                 dateRangeInput(ns("date_range"),
                                "Date Range:",
                                start = Sys.Date() - 30,
                                end = Sys.Date()
                 )
          ),
          column(3,
                 selectInput(ns("export_type"),
                             "Export Type:",
                             choices = c("All", "excel", "tsv", "fasta"),
                             selected = "All"
                 )
          ),
          column(3,
                 selectInput(ns("format"),
                             "Format:",
                             choices = c("All", "xlsx", "tsv", "fasta"),
                             selected = "All"
                 )
          ),
          column(3,
                 div(class = "pull-right",
                     downloadButton(ns("download_history"),
                                    "Download History",
                                    class = "btn-info"
                     )
                 )
          )
        ),

        hr(),

        DTOutput(ns("history_table"))
      )
    )
  )
}
