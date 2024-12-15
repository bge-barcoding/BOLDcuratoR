# R/modules/haplotype_analysis/mod_haplotype_analysis_ui.R

#' UI Module for Haplotype Analysis
#' @param id The module ID
#' @export
mod_haplotype_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Error/Status Container
    uiOutput(ns("status_container")),

    # Analysis Summary Section
    fluidRow(
      column(12,
             box(
               title = "Analysis Summary",
               status = "info",
               width = NULL,
               solidHeader = TRUE,

               fluidRow(
                 valueBoxOutput(ns("total_species_box"), width = 3),
                 valueBoxOutput(ns("total_haplotypes_box"), width = 3),
                 valueBoxOutput(ns("avg_diversity_box"), width = 3),
                 valueBoxOutput(ns("unique_sequences_box"), width = 3)
               )
             )
      )
    ),

    # Main Analysis Section
    fluidRow(
      column(12,
             box(
               title = "Haplotype Analysis",
               status = "primary",
               width = NULL,
               solidHeader = TRUE,

               tabBox(
                 id = ns("analysis_tabs"),
                 width = NULL,

                 # Species Selection & Details Tab
                 tabPanel(
                   "Species Analysis",
                   fluidRow(
                     column(4,
                            selectInput(ns("selected_species"),
                                        "Select Species:",
                                        choices = NULL
                            )
                     ),
                     column(8,
                            actionButton(ns("analyze_species"),
                                         "Analyze Species",
                                         class = "btn-primary",
                                         icon = icon("dna")
                            ),
                            downloadButton(ns("download_analysis"),
                                           "Download Results",
                                           class = "btn-success"
                            )
                     )
                   ),
                   hr(),
                   DTOutput(ns("species_details"))
                 ),

                 # Haplotype Network Tab
                 tabPanel(
                   "Haplotype Network",
                   fluidRow(
                     column(12,
                            plotOutput(ns("haplotype_network"), height = "600px"),
                            hr(),
                            DTOutput(ns("haplotype_table"))
                     )
                   )
                 ),

                 # Geographic Distribution Tab
                 tabPanel(
                   "Geographic Distribution",
                   fluidRow(
                     column(12,
                            plotOutput(ns("geographic_dist"), height = "500px"),
                            hr(),
                            DTOutput(ns("geographic_table"))
                     )
                   )
                 ),

                 # Summary Statistics Tab
                 tabPanel(
                   "Statistics",
                   fluidRow(
                     column(12,
                            box(
                              title = "Diversity Metrics",
                              status = "info",
                              width = NULL,
                              DTOutput(ns("diversity_stats"))
                            ),
                            box(
                              title = "Sequence Analysis",
                              status = "info",
                              width = NULL,
                              DTOutput(ns("sequence_stats"))
                            )
                     )
                   )
                 )
               )
             )
      )
    ),

    # Download Section
    fluidRow(
      column(12,
             box(
               title = "Export Options",
               status = "success",
               width = NULL,
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,

               fluidRow(
                 column(12,
                        div(
                          class = "btn-group",
                          style = "margin-bottom: 15px;",
                          downloadButton(ns("download_summary"),
                                         "Download Summary (XLSX)",
                                         class = "btn-success"
                          ),
                          downloadButton(ns("download_sequences"),
                                         "Download Sequences (FASTA)",
                                         class = "btn-info"
                          ),
                          downloadButton(ns("download_networks"),
                                         "Download Networks (PDF)",
                                         class = "btn-warning"
                          )
                        )
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

        .value-box {
          cursor: default;
        }

        .btn-group .btn {
          margin-right: 5px;
        }

        .nav-tabs {
          margin-bottom: 15px;
        }

        .plot-container {
          background: white;
          padding: 15px;
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .status-message {
          padding: 10px 15px;
          margin-bottom: 15px;
          border-radius: 4px;
        }

        .status-message.info {
          background-color: #d1ecf1;
          border-color: #bee5eb;
        }

        .status-message.warning {
          background-color: #fff3cd;
          border-color: #ffeeba;
        }

        .status-message.error {
          background-color: #f8d7da;
          border-color: #f5c6cb;
        }
      "))
    )
  )
}

#' Create Navigation Panel for Haplotype Analysis
#' @param id The module ID
#' @export
mod_haplotype_analysis_nav <- function(id) {
  ns <- NS(id)
  menuItem(
    "Haplotype Analysis",
    tabName = "haplotypes",
    icon = icon("dna")
  )
}
