# R/modules/bags_grading/mod_bags_grading_server.R

#' Server Module for BAGS Grading
#' @param id Module ID
#' @param state State management instance
#' @param grade BAGS grade (A-E)
#' @param logger Logger instance
#' @export
mod_bags_grading_server <- function(id, state, grade, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    rv <- reactiveValues(
      filtered_data = NULL,
      selected_specimens = list(),
      flagged_specimens = list(),
      curator_notes = list(),
      metrics = NULL,
      needs_refresh = FALSE, # New reactive value for tracking refresh needs
      processing_status = list(
        is_processing = FALSE,
        message = NULL,
        error = NULL
      )
    )

    # Initialize state
    observe({
      logger$info(sprintf("Initializing state for BAGS grade %s module", grade))
      store <- state$get_store()
      if (!is.null(store$specimen_curator_notes)) {
        isolate({
          rv$curator_notes <- store$specimen_curator_notes
          logger$info("Restored curator notes from state")
        })
      }
    })

    # Main data observer
    observe({
      logger$info(sprintf("Starting main data observer for grade %s", grade))
      store <- state$get_store()

      # Check required data
      if (is.null(store$specimen_data) || is.null(store$bags_grades)) {
        logger$warn("Missing required data", list(
          has_specimens = !is.null(store$specimen_data),
          has_grades = !is.null(store$bags_grades)
        ))
        return()
      }

      req(store$specimen_data, store$bags_grades)

      withProgress(message = 'Processing specimens', value = 0, {
        tryCatch({
          logger$info(sprintf("Processing BAGS grade %s data", grade), list(
            total_records = nrow(store$specimen_data),
            grade = grade
          ))

          # Get and validate grade species
          grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == grade]
          if (length(grade_species) == 0) {
            logger$warn(sprintf("No species found for grade %s", grade))
            rv$processing_status$error <- "No species found for this grade"
            return()
          }

          logger$info(sprintf("Found %d species for grade %s", length(grade_species), grade))

          # Filter specimens
          specimens <- store$specimen_data[
            store$specimen_data$species %in% grade_species &
              !is.na(store$specimen_data$bin_uri) &
              store$specimen_data$bin_uri != "" &
              store$specimen_data$identification_rank %in% c("species", "subspecies"),
          ]

          if (nrow(specimens) == 0) {
            logger$warn(sprintf("No specimens found for grade %s after filtering", grade))
            rv$processing_status$error <- "No specimens found for this grade"
            return()
          }

          logger$info(sprintf("Filtered to %d specimens for grade %s", nrow(specimens), grade))

          # Apply filters
          filtered <- filter_grade_specimens(
            specimens = specimens,
            grades = store$bags_grades,
            target_grade = grade,
            rank_filter = input$rank_filter,
            quality_filter = input$min_quality_score,
            criteria_filter = input$criteria_filter
          )

          logger$info(sprintf("Applied filters for grade %s", grade), list(
            rank_filter = as.character(input$rank_filter),
            quality_filter = input$min_quality_score,
            criteria_count = length(input$criteria_filter),
            filtered_count = nrow(filtered)
          ))

          # Update reactive values
          isolate({
            rv$filtered_data <- filtered
            rv$metrics <- calculate_grade_metrics(filtered)
            rv$processing_status$message <- "Data processed successfully"
          })

          logger$info(sprintf("Grade %s data processing complete", grade), list(
            filtered_count = nrow(filtered),
            species_count = length(unique(filtered$species))
          ))

        }, error = function(e) {
          logger$error(sprintf("Error processing grade %s data", grade), list(
            error = e$message,
            stack = e$call
          ))
          rv$processing_status$error <- sprintf("Processing error: %s", e$message)
        })
      })
    })

    # Specimen tables output
    output$specimen_tables <- renderUI({
      logger$info(sprintf("Rendering specimen tables for grade %s", grade))

      # Check if data exists and if a refresh is needed
      if (is.null(rv$filtered_data)) {
        logger$warn("No filtered data available")
        return(NULL)
      }

      req(rv$filtered_data)
      data <- isolate(rv$filtered_data)

      # Clear refresh flag if set
      if(isolate(rv$needs_refresh)) {
        isolate({
          rv$needs_refresh <- FALSE
        })
      }

      withProgress(message = 'Creating specimen tables', value = 0, {
        tryCatch({
          if (nrow(data) == 0) {
            logger$warn(sprintf("No data available for grade %s tables", grade))
            return(NULL)
          }

          logger$info(sprintf("Creating tables for %d specimens", nrow(data)))

          # Organize specimens by grade
          organized <- organize_grade_specimens(data, grade)
          logger$info(sprintf("Organized specimens into %d groups", length(organized)))

          # Create tables with state syncing
          tables <- create_grade_tables(
            organized_data = organized,
            grade = grade,
            ns = ns,
            current_sel = isolate(rv$selected_specimens),
            current_flags = isolate(rv$flagged_specimens),
            current_notes = isolate(rv$curator_notes),
            logger = logger
          )

          if (length(tables) == 0) {
            logger$warn("No tables created after organization")
            return(NULL)
          }

          logger$info(sprintf("Successfully created %d tables", length(tables)))

          div(class = "specimen-tables", tables)

        }, error = function(e) {
          logger$error(sprintf("Error rendering grade %s tables", grade),
                       list(error = e$message)
          )
          div(class = "alert alert-danger",
              "Error creating specimen tables. Please check the logs.")
        })
      })
    })

    # Status message output
    output$status_container <- renderUI({
      status <- rv$processing_status
      if (status$is_processing) {
        div(class = "status-message info",
            icon("spinner", class = "fa-spin"),
            status$message)
      } else if (!is.null(status$error)) {
        div(class = "status-message error",
            icon("exclamation-circle"),
            status$error)
      }
    })

    # Handle flag changes
    observeEvent(input$specimen_flag, {
      req(input$specimen_flag)
      flag_data <- input$specimen_flag

      if (!is.null(flag_data$processid)) {
        current_flags <- isolate(rv$flagged_specimens)
        user_info <- isolate(state$get_store()$user_info)

        if (!is.null(flag_data$flag) && nchar(flag_data$flag) > 0) {
          current_flags[[flag_data$processid]] <- list(
            flag = flag_data$flag,
            timestamp = Sys.time(),
            species = flag_data$species,
            user = list(
              email = user_info$email,
              name = user_info$name,
              orcid = user_info$orcid
            )
          )
        } else {
          current_flags[[flag_data$processid]] <- NULL
        }

        rv$flagged_specimens <- current_flags
        # Update state (will trigger database update)
        state$update_state("specimen_flags", current_flags)

        # Set refresh flag
        rv$needs_refresh <- TRUE
        invalidateLater(100)
      }
    })

    # Handle note changes
    observeEvent(input$specimen_notes, {
      req(input$specimen_notes)
      note_data <- input$specimen_notes

      if (!is.null(note_data$processid)) {
        current_notes <- isolate(rv$curator_notes)
        user_info <- isolate(state$get_store()$user_info)

        if (!is.null(note_data$notes) && nchar(note_data$notes) > 0) {
          current_notes[[note_data$processid]] <- list(
            text = note_data$notes,
            timestamp = Sys.time(),
            user = list(
              email = user_info$email,
              name = user_info$name,
              orcid = user_info$orcid
            )
          )
        } else {
          current_notes[[note_data$processid]] <- NULL
        }

        rv$curator_notes <- current_notes
        # Update state (will trigger database update)
        state$update_state("specimen_curator_notes", current_notes)

        # Set refresh flag
        rv$needs_refresh <- TRUE
        invalidateLater(100)
      }
    })

    # Session cleanup
    session$onSessionEnded(function() {
      logger$info(sprintf("Grade %s session ending", grade))

      final_metrics <- isolate({
        list(
          filtered_specimens = if (!is.null(rv$filtered_data)) nrow(rv$filtered_data) else 0,
          selected_count = length(rv$selected_specimens),
          flagged_count = length(rv$flagged_specimens),
          note_count = length(rv$curator_notes)
        )
      })

      logger$info("Final state logged", final_metrics)
    })

    # Return reactive values
    list(
      filtered_data = reactive({ rv$filtered_data }),
      selected_specimens = reactive({ rv$selected_specimens }),
      flagged_specimens = reactive({ rv$flagged_specimens }),
      curator_notes = reactive({ rv$curator_notes }),
      metrics = reactive({ rv$metrics }),
      processing_status = reactive({ rv$processing_status })
    )
  })
}
