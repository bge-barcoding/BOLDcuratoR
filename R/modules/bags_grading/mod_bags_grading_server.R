# R/modules/bags_grading/mod_bags_grading_server.R

#' BAGS Grading Processor R6 Class
#' @description Manages BAGS grade calculations and processing
#' @export
BagsProcessor <- R6::R6Class(
  "BagsProcessor",

  public = list(
    #' @description Initialize processor
    #' @param validator SpecimenValidator instance
    #' @param logger Logger instance
    initialize = function(validator, logger) {
      private$validator <- validator
      private$logger <- logger
      private$error_boundary <- ErrorBoundary$new()
    },

    #' @description Process specimens and calculate BAGS grades
    #' @param specimens Data frame of specimens
    #' @return Data frame with BAGS grades
    process_grades = function(specimens) {
      private$error_boundary$catch({
        # Validate input specimens
        validation <- private$validator$validate_specimens(specimens)
        if (!validation$valid) {
          private$logger$error("Invalid specimen data for BAGS grading", validation$messages)
          return(NULL)
        }

        # Calculate grades using existing utility function
        grades <- calculate_bags_grade(specimens)

        # Validate results
        if (!validate_bags_grades(grades)) {
          private$logger$error("Invalid BAGS grade calculation results")
          return(NULL)
        }

        private$logger$info(sprintf("BAGS grades calculated for %d species", nrow(grades)))
        grades
      })
    },

    #' @description Get metrics for a specific grade
    #' @param specimens Data frame of specimens
    #' @param grade Target BAGS grade
    #' @return List of metrics
    get_grade_metrics = function(specimens, grade) {
      private$error_boundary$catch({
        if (is.null(specimens) || nrow(specimens) == 0) {
          return(private$create_empty_metrics())
        }

        list(
          total_species = length(unique(specimens$species)),
          total_specimens = nrow(specimens),
          avg_quality = mean(specimens$quality_score, na.rm = TRUE),
          countries = length(unique(specimens$country.ocean))
        )
      })
    }
  ),

  private = list(
    validator = NULL,
    logger = NULL,
    error_boundary = NULL,

    create_empty_metrics = function() {
      list(
        total_species = 0,
        total_specimens = 0,
        avg_quality = 0,
        countries = 0
      )
    }
  )
)

#' Server Module for BAGS Grading
#' @param id The module ID
#' @param state State management instance
#' @param grade BAGS grade to filter for (A-E)
#' @export
mod_bags_grading_server <- function(id, state, grade) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize components
    processor <- BagsProcessor$new(
      validator = SpecimenValidator$new(state$logger),
      logger = state$logger
    )

    # Filtered specimens for current grade
    grade_specimens <- reactive({
      store <- state$get_store()
      req(store$specimen_data, store$bags_grades)

      # Get specimens for current grade
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == grade]
      specimens <- store$specimen_data[store$specimen_data$species %in% grade_species, ]

      # Apply filters using existing utility functions
      filter_grade_specimens(
        specimens = specimens,
        rank_filter = input$rank_filter,
        quality_filter = input$quality_filter,
        criteria_filter = input$criteria_filter
      )
    })

    # Grade metrics
    grade_metrics <- reactive({
      req(grade_specimens())
      processor$get_grade_metrics(grade_specimens(), grade)
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

    # Selected count box
    output$selected_count_box <- renderValueBox({
      store <- state$get_store()
      req(store$selected_specimens)

      selected_count <- length(intersect(
        names(store$selected_specimens),
        unique(grade_specimens()$species)
      ))

      valueBox(
        selected_count,
        "Selected Specimens",
        icon = icon("check-circle"),
        color = "blue"
      )
    })

    # Quality metrics boxes
    output$avg_quality_box <- renderValueBox({
      req(grade_metrics())
      metrics <- grade_metrics()

      valueBox(
        round(metrics$avg_quality, 1),
        "Average Quality Score",
        icon = icon("star"),
        color = "purple"
      )
    })

    output$countries_box <- renderValueBox({
      req(grade_metrics())
      metrics <- grade_metrics()

      valueBox(
        metrics$countries,
        "Countries",
        icon = icon("globe"),
        color = "orange"
      )
    })

    # Specimen table
    output$specimen_table <- renderDT({
      req(grade_specimens())
      format_bags_grade_table(grade_specimens())
    })

    # Handle specimen selection
    observeEvent(input$specimen_table_rows_selected, {
      selected_rows <- input$specimen_table_rows_selected
      if (length(selected_rows) == 0) return()

      specimens <- grade_specimens()
      if (is.null(specimens)) return()

      selected_specimens <- specimens[selected_rows, ]

      # Update state using existing functions
      current_selections <- state$get_store()$selected_specimens
      if (!is.list(current_selections)) {
        current_selections <- list()
      }

      # Select best specimen per species
      for (species in unique(selected_specimens$species)) {
        species_specimens <- selected_specimens[selected_specimens$species == species, ]
        best_specimen <- species_specimens[order(
          -species_specimens$quality_score,
          species_specimens$specimen_rank
        )[1], ]

        current_selections[[species]] <- best_specimen$processid

        # Log selection
        state$logger$info("Specimen selected", list(
          species = species,
          processid = best_specimen$processid,
          quality_score = best_specimen$quality_score,
          rank = best_specimen$specimen_rank,
          grade = grade
        ))
      }

      state$update_state(
        "selected_specimens",
        current_selections,
        validate_selected_specimens
      )
    })

    # Return module interface
    list(
      grade_specimens = grade_specimens,
      grade_metrics = grade_metrics
    )
  })
}
