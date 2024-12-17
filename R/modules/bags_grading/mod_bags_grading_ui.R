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
                        "E" = "Species with discordant taxonomy in BINs"
                 )
               ),

               # Summary Metrics
               fluidRow(
                 valueBoxOutput(ns("grade_summary_box"), width = 12)
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

               # Filtering Controls
               fluidRow(
                 column(3,
                        selectInput(ns("rank_filter"),
                                    "Filter by Rank:",
                                    choices = c("All", "1", "2", "3", "4", "5", "6"),
                                    selected = "All")
                 ),
                 column(3,
                        numericInput(ns("quality_filter"),
                                     "Minimum Quality Score:",
                                     value = 0,
                                     min = 0,
                                     max = 14,
                                     step = 1)
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
                                           inline = TRUE)
                 )
               ),

               hr(),

               # Specimen Table
               DTOutput(ns("specimen_table"))
             )
      )
    )
  )
}
