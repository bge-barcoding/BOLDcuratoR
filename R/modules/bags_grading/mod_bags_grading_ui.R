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
                 style = "margin-bottom: 10px;",
                 switch(grade,
                        "A" = "Species with >10 specimens and a single BIN",
                        "B" = "Species with 3-10 specimens and a single BIN",
                        "C" = "Species with multiple BINs",
                        "D" = "Species with <3 specimens and a single BIN",
                        "E" = "Species sharing a BIN with other species"
                 )
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
          column(6,
                 selectInput(ns("rank_filter"),
                             "Filter by Rank:",
                             choices = c("All", "1", "2", "3", "4", "5", "6", "7"),
                             selected = "All"
                 )
          ),
          column(6,
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
                   style = "margin-top: 10px;",
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
          style = "margin-top: 10px;",
          uiOutput(ns("specimen_tables"))
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
            tags$h4("BAGS Grade Help", class = "modal-title"),
            tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×")
          ),
          tags$div(
            class = "modal-body",
            tagList(
              switch(grade,
                     "A" = tagList(
                       tags$p("Grade A species have:"),
                       tags$ul(
                         tags$li("More than 10 specimens with valid BINs"),
                         tags$li("A single BIN"),
                         tags$li("No taxonomic discordance")
                       )
                     ),
                     "B" = tagList(
                       tags$p("Grade B species have:"),
                       tags$ul(
                         tags$li("3-10 specimens with valid BINs"),
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
                         tags$li("Fewer than 3 specimens with valid BINs"),
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
                tags$li("Sort columns by clicking headers"),
                tags$li("Filter data using the controls above")
              )
            )
          ),
          tags$div(
            class = "modal-footer",
            modalButton("Close")
          )
        )
      )
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
