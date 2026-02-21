# R/modules/species_analysis/mod_species_analysis_ui.R

#' UI Module for Species Analysis
#' @param id The module ID
#' @export
mod_species_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Processing Status
    uiOutput(ns("processing_status")),

    # Main Analysis Section
    fluidRow(
      box(
        title = "Species Analysis Dashboard",
        status = "primary",
        width = 12,
        solidHeader = TRUE,

        # Summary Value Boxes
        fluidRow(
          valueBoxOutput(ns("total_species_box"), width = 3),
          valueBoxOutput(ns("species_with_bins_box"), width = 3),
          valueBoxOutput(ns("species_without_bins_box"), width = 3),
          valueBoxOutput(ns("input_taxa_found_box"), width = 3)
        ),

        # Content Tabs
        tabBox(
          id = ns("analysis_tabs"),
          width = 12,

          tabPanel(
            "Species Checklist",
            div(
              class = "table-responsive",
              DTOutput(ns("species_checklist_table"))
            )
          ),

          tabPanel(
            "Gap Analysis",
            uiOutput(ns("gap_analysis_panel"))
          ),

          tabPanel(
            "Summary Statistics",
            div(
              class = "table-responsive",
              DTOutput(ns("summary_stats_table"))
            )
          )
        )
      )
    )
  )
}

#' Create Navigation Panel for Species Analysis
#' @param id The module ID
#' @export
mod_species_analysis_nav <- function(id) {
  ns <- NS(id)
  menuItem(
    "Species Analysis",
    tabName = "species_analysis",
    icon = icon("list-check")
  )
}
