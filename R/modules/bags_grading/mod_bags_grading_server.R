# R/modules/bags_grading/mod_bags_grading_server.R

#' Server Module for BAGS Grading
#' @param id The module ID
#' @param state State management instance
#' @param grade BAGS grade to display (A-E)
#' @export
mod_bags_grading_server <- function(id, state, grade) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filtered specimens for current grade
    grade_specimens <- reactive({
      store <- state$get_store()
      req(store$specimen_data, store$bags_grades)

      # Get specimens for current grade
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == grade]
      specimens <- store$specimen_data[store$specimen_data$species %in% grade_species, ]

      # Add ID quality flags and format for display
      formatted <- format_grade_data(specimens, grade)

      # Apply filters using module utilities
      filter_grade_specimens(
        specimens = formatted,
        grades = store$bags_grades,
        target_grade = grade,
        rank_filter = input$rank_filter,
        quality_filter = input$quality_filter,
        criteria_filter = input$criteria_filter
      )
    })

    # Grade metrics
    grade_metrics <- reactive({
      req(grade_specimens())
      calculate_grade_metrics(grade_specimens())
    })

    # Grade summary box
    output$grade_summary_box <- renderValueBox({
      req(grade_metrics())
      metrics <- grade_metrics()

      valueBox(
        metrics$total_species,
        paste("Grade", grade, "Species"),
        icon = icon(switch(grade,
                           "A" = "trophy",
                           "B" = "medal",
                           "C" = "exclamation-circle",
                           "D" = "exclamation-triangle",
                           "E" = "times-circle"
        )),
        color = switch(grade,
                       "A" = "green",
                       "B" = "blue",
                       "C" = "yellow",
                       "D" = "red",
                       "E" = "red"
        )
      )
    })

    # Specimen table
    output$specimen_table <- renderDT({
      req(grade_specimens())
      specimens <- grade_specimens()

      # Add species ID quality indicators
      datatable(specimens,
                options = list(
                  pageLength = 25,
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
                ),
                rownames = FALSE
      ) %>%
        formatStyle(
          'species_rank',
          backgroundColor = styleEqual(
            c("Species", "sp./spp.", "cf./aff.", "No ID"),
            c('#d4edda', '#fff3cd', '#f8d7da', '#e9ecef')
          )
        )
    })

    # Return module interface
    list(
      grade_specimens = grade_specimens,
      grade_metrics = grade_metrics
    )
  })
}
