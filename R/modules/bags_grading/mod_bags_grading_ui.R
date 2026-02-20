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
                               "D" = NULL,
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
