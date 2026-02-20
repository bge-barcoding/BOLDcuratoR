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

    # Local reactive values for data that is truly local to this module.
    # Annotations (flags, notes, selections) live ONLY in the StateManager
    # — no local mirrors, no sync observers.
    rv <- reactiveValues(
      filtered_data = NULL,
      metrics = NULL,
      processing_status = list(
        is_processing = FALSE,
        message = NULL,
        error = NULL
      )
    )

    # Main data observer with detailed logging
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

          # Update reactive values
          isolate({
            rv$filtered_data <- specimens
            rv$metrics <- calculate_grade_metrics(specimens)
            rv$processing_status$message <- "Data processed successfully"
          })

          logger$info(sprintf("Grade %s data processing complete", grade), list(
            specimen_count = nrow(specimens),
            species_count = length(unique(specimens$species))
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

    # Specimen tables output.
    # Reactive only to rv$filtered_data (filter/data changes).
    # Annotations are read from StateManager via isolate() so flag/note
    # changes don't cause a full re-render (which would destroy DT state).
    output$specimen_tables <- renderUI({
      logger$info(sprintf("Rendering specimen tables for grade %s", grade))

      if (is.null(rv$filtered_data)) {
        logger$warn("No filtered data available for rendering tables")
        return(NULL)
      }

      req(rv$filtered_data)

      # Read annotations from the single source of truth (StateManager).
      # All accesses must be inside a single isolate() block — otherwise
      # store$specimen_flags etc. create reactive dependencies that cause
      # the tables to re-render on every flag/note change.
      annotations <- isolate({
        store <- state$get_store()
        list(
          sel   = store$selected_specimens,
          flags = store$specimen_flags,
          notes = store$specimen_curator_notes
        )
      })
      current_sel   <- annotations$sel
      current_flags <- annotations$flags
      current_notes <- annotations$notes

      withProgress(message = 'Creating specimen tables', value = 0, {
        tryCatch({
          data <- rv$filtered_data

          if (is.null(data) || nrow(data) == 0) {
            logger$warn(sprintf("No data available for grade %s tables", grade))
            return(NULL)
          }

          logger$info(sprintf("Creating tables for %d specimens", nrow(data)))

          # Create tables based on grade
          organized <- organize_grade_specimens(data, grade)
          logger$info(sprintf("Organized specimens into %d groups", length(organized)))

          # Annotations are merged by prepare_module_data() inside
          # create_grade_tables(), so no separate sync step is needed.
          tables <- create_grade_tables(
            organized = organized,
            grade = grade,
            ns = ns,
            current_sel = current_sel,
            current_flags = current_flags,
            current_notes = current_notes,
            logger = logger
          )

          if (length(tables) == 0) {
            logger$warn("No tables created after organization")
            return(NULL)
          }

          logger$info(sprintf("Successfully created %d tables", length(tables)))
          div(class = "specimen-tables", tables)

        }, error = function(e) {
          logger$error(sprintf("Error rendering grade %s tables", grade), list(
            error = e$message,
            stack = e$call
          ))
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

    # Value box outputs with logging
    output$species_count_box <- renderValueBox({
      logger$info(sprintf("Rendering species count box for grade %s", grade))
      data <- isolate(rv$filtered_data)
      count <- if (is.null(data)) 0 else length(unique(data$species))

      logger$info(sprintf("Species count for grade %s: %d", grade, count))

      valueBox(
        count,
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
          "D" = "black",
          "E" = "red"
        )
      )
    })

    # Watch for flag changes — write directly to StateManager
    observeEvent(input$specimen_flag, {
      req(input$specimen_flag)
      flag <- input$specimen_flag

      if (!is.null(flag$processid)) {
        store <- state$get_store()
        current_flags <- isolate(store$specimen_flags)
        if (is.null(current_flags)) current_flags <- list()

        if (!is.null(flag$flag) && nchar(flag$flag) > 0) {
          current_flags[[flag$processid]] <- list(
            flag = flag$flag,
            timestamp = Sys.time(),
            species = flag$species,
            user = store$user_info$email
          )
        } else {
          current_flags[[flag$processid]] <- NULL
        }

        state$update_state("specimen_flags", current_flags)
      }
    })

    # Watch for note changes — write directly to StateManager
    observeEvent(input$specimen_notes, {
      req(input$specimen_notes)
      note <- input$specimen_notes

      if (!is.null(note$processid)) {
        store <- state$get_store()
        current_notes <- isolate(store$specimen_curator_notes)
        if (is.null(current_notes)) current_notes <- list()

        # JS payload sends the value under "curator_notes" key
        note_text <- note$curator_notes %||% note$notes %||% ""
        if (nchar(note_text) > 0) {
          current_notes[[note$processid]] <- list(
            text = note_text,
            timestamp = Sys.time(),
            user = store$user_info$email
          )
        } else {
          current_notes[[note$processid]] <- NULL
        }

        state$update_state("specimen_curator_notes", current_notes)
      }
    })

    # Safe session cleanup with logging
    session$onSessionEnded(function() {
      logger$info(sprintf("Grade %s session ending", grade))

      # Log final state
      store <- isolate(state$get_store())
      final_metrics <- isolate({
        list(
          filtered_specimens = if (!is.null(rv$filtered_data)) nrow(rv$filtered_data) else 0,
          selected_count = length(store$selected_specimens),
          flagged_count = length(store$specimen_flags)
        )
      })

      logger$info("Final state logged", final_metrics)
    })

    # Return reactive endpoints — annotations come from StateManager
    list(
      filtered_data = reactive({ rv$filtered_data }),
      selected_specimens = reactive({ state$get_store()$selected_specimens }),
      flagged_specimens = reactive({ state$get_store()$specimen_flags }),
      curator_notes = reactive({ state$get_store()$specimen_curator_notes }),
      metrics = reactive({ rv$metrics }),
      processing_status = reactive({ rv$processing_status })
    )
  })
}
