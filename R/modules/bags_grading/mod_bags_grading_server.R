# R/modules/bags_grading/mod_bags_grading_server.R

#' Server Module for BAGS Grading
#' @param id The module ID
#' @param state State management instance
#' @param grade BAGS grade to display (A-E)
#' @param logger Logger instance
#' @export
mod_bags_grading_server <- function(id, state, grade, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive Values
    filtered_data <- reactiveVal(NULL)
    selected_specimens <- reactiveVal(list())
    flagged_specimens <- reactiveVal(list())
    notes <- reactiveVal(list())
    metrics <- reactiveVal(NULL)
    processing_status <- reactiveVal(list(
      is_processing = FALSE,
      message = NULL,
      error = NULL
    ))

    # Get specimens for current grade
    observe({
      store <- state$get_store()
      req(store$specimen_data, store$bags_grades)

      # Validate data consistency
      validation <- validate_bags_data(store$specimen_data, store$bags_grades)
      if (!validation$valid) {
        logger$error("Invalid BAGS data", validation$message)
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = validation$message
        ))
        return(NULL)
      }

      # Get species for current grade
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == grade]
      if (length(grade_species) == 0) {
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = "No species found for this grade"
        ))
        return(NULL)
      }

      # Filter specimens to those with BINs and species/subspecies rank
      specimens <- store$specimen_data[store$specimen_data$species %in% grade_species &
                                         !is.na(store$specimen_data$bin_uri) &
                                         store$specimen_data$bin_uri != "" &
                                         store$specimen_data$identification_rank %in% c("species", "subspecies"), ]

      if (nrow(specimens) == 0) {
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = "No specimens found for this grade"
        ))
        return(NULL)
      }

      # Calculate initial metrics
      metrics(calculate_metrics(specimens))

      # Update filtered data
      filtered_data(specimens)
      processing_status(list(
        is_processing = FALSE,
        message = "Data loaded successfully",
        error = NULL
      ))

      logger$info(sprintf("Loaded grade %s data: %d specimens, %d species",
                          grade, nrow(specimens), length(unique(specimens$species))))
    })

    # Summary box outputs
    output$species_count_box <- renderValueBox({
      data <- filtered_data()
      count <- if (is.null(data)) 0 else length(unique(data$species))

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
                       "D" = "red",
                       "E" = "red"
        )
      )
    })

    output$specimen_count_box <- renderValueBox({
      data <- filtered_data()
      count <- if (is.null(data)) 0 else nrow(data)

      valueBox(
        count,
        "Total Specimens",
        icon = icon("microscope"),
        color = "purple"
      )
    })

    output$bin_count_box <- renderValueBox({
      data <- filtered_data()
      count <- if (is.null(data)) 0 else length(unique(data$bin_uri))

      valueBox(
        count,
        "Unique BINs",
        icon = icon("dna"),
        color = "blue"
      )
    })

    output$selection_count_box <- renderValueBox({
      selections <- selected_specimens()
      total_metric <- metrics()$total_specimens
      percent <- if (!is.null(total_metric) && total_metric > 0) {
        sprintf(" (%.1f%%)", length(selections) / total_metric * 100)
      } else {
        ""
      }

      valueBox(
        paste0(length(selections), percent),
        "Selected Specimens",
        icon = icon("check-circle"),
        color = "green"
      )
    })

    # Specimen tables output
    output$specimen_tables <- renderUI({
      data <- filtered_data()
      if (is.null(data)) {
        logger$info("No filtered data available for BAGS grade", list(grade = grade))
        return(NULL)
      }

      withProgress(
        message = 'Creating specimen tables',
        detail = 'Processing data...',
        value = 0,
        {
          # Log data structure
          logger$info("Processing BAGS grade data", list(
            grade = grade,
            total_records = nrow(data),
            columns = names(data),
            unique_species = length(unique(data$species))
          ))

          # Get reactive values safely
          current_sel <- try(isolate(selected_specimens()), silent = TRUE)
          if (inherits(current_sel, "try-error")) {
            logger$warn("Could not access selected_specimens()")
            current_sel <- list()
          }

          current_flags <- try(isolate(flagged_specimens()), silent = TRUE)
          if (inherits(current_flags, "try-error")) {
            logger$warn("Could not access flagged_specimens()")
            current_flags <- list()
          }

          current_notes <- try(isolate(notes()), silent = TRUE)
          if (inherits(current_notes, "try-error")) {
            logger$warn("Could not access notes()")
            current_notes <- list()
          }

          logger$info("Current state for grade", list(
            grade = grade,
            selections = length(current_sel),
            flags = length(current_flags),
            notes = length(current_notes)
          ))

          organized <- organize_grade_specimens(data, grade)
          logger$info("Organized specimens by grade", list(
            grade = grade,
            groups = length(organized),
            group_names = names(organized)
          ))

          tables <- lapply(names(organized), function(group_name) {
            group_data <- organized[[group_name]]

            logger$info("Processing group", list(
              grade = grade,
              group = group_name,
              records = if (is.data.frame(group_data)) nrow(group_data) else length(group_data)
            ))

            if (grade == "C") {
              bin_tables <- lapply(names(group_data), function(bin) {
                bin_specimens <- group_data[[bin]]
                specimen_table <- format_specimen_table(
                  data = bin_specimens,
                  ns = ns,
                  current_selections = current_sel,
                  current_flags = current_flags,
                  current_notes = current_notes,
                  logger = logger
                )
                create_table_container(
                  table = specimen_table,
                  title = sprintf("BIN: %s", bin),
                  caption = sprintf("Species: %s", group_name)
                )
              })
              div(
                h3(sprintf("Species: %s", group_name)),
                bin_tables
              )
            } else if (grade == "E") {
              specimen_table <- format_specimen_table(
                data = group_data,
                ns = ns,
                color_by = "species",
                current_selections = current_sel,
                current_flags = current_flags,
                current_notes = current_notes,
                logger = logger
              )
              create_table_container(
                table = specimen_table,
                title = sprintf("Shared BIN: %s", group_name),
                caption = sprintf("Species Count: %d", length(unique(group_data$species)))
              )
            } else {
              specimen_table <- format_specimen_table(
                data = group_data,
                ns = ns,
                current_selections = current_sel,
                current_flags = current_flags,
                current_notes = current_notes,
                logger = logger
              )
              create_table_container(
                table = specimen_table,
                title = sprintf("Species: %s", group_name),
                caption = generate_table_caption(grade, list(species = group_name))
              )
            }
          })

          logger$info("Completed table generation", list(
            grade = grade,
            tables_created = length(tables)
          ))

          div(class = "specimen-tables", tables)
        }
      )
    })

    # Handle specimen selection
    observeEvent(input$specimen_selection, {
      req(input$specimen_selection)
      selection <- input$specimen_selection

      if (!is.null(selection$processid)) {
        current_selections <- selected_specimens()

        if (selection$selected) {
          # Validate selection
          validation <- validate_specimen_selection(
            selection$processid,
            filtered_data()
          )

          if (validation$valid) {
            specimen_data <- filtered_data()[
              filtered_data()$processid == selection$processid,
            ]

            current_selections[[selection$processid]] <- list(
              timestamp = Sys.time(),
              species = specimen_data$species[1],
              grade = grade,
              bin_uri = specimen_data$bin_uri[1],
              quality_score = specimen_data$quality_score[1]
            )

            logger$info("Specimen selected", list(
              processid = selection$processid,
              species = specimen_data$species[1],
              grade = grade,
              bin_uri = specimen_data$bin_uri[1]
            ))
          } else {
            logger$warn("Invalid specimen selection", validation$message)
          }
        } else {
          current_selections[[selection$processid]] <- NULL
          logger$info("Specimen deselected", list(
            processid = selection$processid,
            grade = grade
          ))
        }

        selected_specimens(current_selections)
        state$update_state("specimen_selections", current_selections)
      }
    })

    # Handle specimen flagging
    observeEvent(input$specimen_flag, {
      req(input$specimen_flag)
      flag <- input$specimen_flag

      if (!is.null(flag$processid)) {
        current_flags <- flagged_specimens()

        if (nchar(flag$flag) > 0) {
          specimen_data <- filtered_data()[
            filtered_data()$processid == flag$processid,
          ]

          if (nrow(specimen_data) > 0) {
            current_flags[[flag$processid]] <- list(
              flag = flag$flag,
              timestamp = Sys.time(),
              species = specimen_data$species[1],
              grade = grade,
              bin_uri = specimen_data$bin_uri[1]
            )

            logger$info("Specimen flagged", list(
              processid = flag$processid,
              flag = flag$flag,
              species = specimen_data$species[1],
              grade = grade,
              bin_uri = specimen_data$bin_uri[1]
            ))
          }
        } else {
          current_flags[[flag$processid]] <- NULL
          logger$info("Flag removed", list(
            processid = flag$processid,
            grade = grade
          ))
        }

        flagged_specimens(current_flags)
        state$update_state("specimen_flags", current_flags)
      }
    })

    # Handle specimen notes
    observeEvent(input$specimen_notes, {
      req(input$specimen_notes)
      note <- input$specimen_notes

      if (!is.null(note$processid)) {
        current_notes <- notes()
        if (nchar(note$notes) > 0) {
          current_notes[[note$processid]] <- note$notes
        } else {
          current_notes[[note$processid]] <- NULL
        }

        notes(current_notes)
        state$update_state("specimen_notes", current_notes)

        logger$info("Specimen note updated", list(
          processid = note$processid,
          grade = grade
        ))
      }
    })

    # Handle filter changes
    observeEvent(input$apply_filters, {
      data <- filtered_data()
      if (!is.null(data)) {
        filtered <- filter_grade_specimens(
          data,
          state$get_store()$bags_grades,
          grade,
          input$rank_filter,
          input$min_quality_score,
          input$criteria_filter
        )

        filtered_data(filtered)
        metrics(calculate_metrics(filtered))

        logger$info("Filters applied", list(
          grade = grade,
          filtered_count = nrow(filtered),
          rank_filter = input$rank_filter,
          quality_filter = input$min_quality_score,
          criteria_filter = input$criteria_filter
        ))
      }
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "rank_filter", selected = "All")
      updateNumericInput(session, "min_quality_score", value = 0)
      updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))

      # Reset to original filtered data
      store <- state$get_store()
      if (!is.null(store$specimen_data)) {
        original_filtered <- store$specimen_data[
          store$specimen_data$species %in% store$bags_grades$species[store$bags_grades$bags_grade == grade] &
            !is.na(store$specimen_data$bin_uri) &
            store$specimen_data$bin_uri != "" &
            store$specimen_data$identification_rank %in% c("species", "subspecies"),
        ]
        filtered_data(original_filtered)
        metrics(calculate_metrics(original_filtered))
      }

      logger$info("Filters reset", list(grade = grade))
    })

    # Handle help modal
    observeEvent(input$show_help, {
      showModal(modalDialog(
        title = paste("BAGS Grade", grade, "Help"),
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
                     tags$li("No taxonomic discordance within BINs"),
                     tags$li("All specimens must have valid BINs")
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
                     tags$li("Color coding indicates different species within shared BINs"),
                     tags$li("Only specimens with valid BINs are shown")
                   )
                 )
          ),
          hr(),
          tags$p("Table Features:"),
          tags$ul(
            tags$li("Use checkboxes to select representative specimens"),
            tags$li("Use flag dropdown to mark potential issues"),
            tags$li("Sort columns by clicking headers"),
            tags$li("Filter data using the controls above"),
            tags$li("Only specimens with valid BINs are included")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        create_download_filename(grade)
      },
      content = function(file) {
        withProgress(
          message = 'Preparing download',
          value = 0,
          {
            data <- filtered_data()
            if (is.null(data)) {
              stop("No data available for download")
            }

            # Prepare data for download
            download_data <- prepare_download_data(
              data,
              grade,
              selected_specimens(),
              flagged_specimens()
            )

            write.csv(download_data, file, row.names = FALSE)

            # Log successful download
            user_info <- state$get_store()$user_info
            logger$info("Downloaded grade data", list(
              grade = grade,
              user = user_info$email,
              records = nrow(download_data),
              species = length(unique(download_data$species)),
              bins = length(unique(download_data$bin_uri))
            ))
          }
        )
      }
    )

    # Error handling for specimen loading
    observeEvent(state$get_store()$error, {
      error_state <- state$get_store()$error
      if (error_state$has_error && error_state$source == "bags_grading") {
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = error_state$message
        ))
        logger$error("Error in BAGS grading", error_state$message)
      }
    })

    # Status message output
    output$status_container <- renderUI({
      status <- processing_status()
      if (status$is_processing) {
        div(
          class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          status$message
        )
      } else if (!is.null(status$error)) {
        div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          status$error
        )
      }
    })

    # Calculate metrics helper
    calculate_metrics <- function(data) {
      list(
        total_specimens = nrow(data),
        unique_species = length(unique(data$species)),
        unique_bins = length(unique(data$bin_uri)),
        avg_quality = mean(data$quality_score, na.rm = TRUE),
        bin_coverage = sum(!is.na(data$bin_uri) & data$bin_uri != "") / nrow(data) * 100
      )
    }

    # Watch for session timeout
    observe({
      # Simple check using session object directly
      if (!is.null(session$sessionTimeInSeconds) &&
          session$sessionTimeInSeconds > 3540) { # 59 minutes in seconds
        showNotification(
          "Session will expire in 1 minute. Please save your work.",
          type = "warning",
          duration = NULL
        )
      }
    })

    # Session cleanup
    session$onSessionEnded(function() {
      logger$info(sprintf("Grade %s session ended", grade))

      # Clean up reactive values
      filtered_data(NULL)
      selected_specimens(NULL)
      flagged_specimens(NULL)
      metrics(NULL)

      # Log final state
      logger$info("Final state logged", list(
        grade = grade,
        selections = length(selected_specimens()),
        flags = length(flagged_specimens())
      ))
    })

    # Return reactive values and functions
    list(
      filtered_data = filtered_data,
      selected_specimens = selected_specimens,
      flagged_specimens = flagged_specimens,
      notes = notes,
      metrics = metrics,
      processing_status = processing_status,

      # Helper functions
      reset_filters = function() {
        updateSelectInput(session, "rank_filter", selected = "All")
        updateNumericInput(session, "min_quality_score", value = 0)
        updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
      },

      update_filters = function(rank = NULL, quality = NULL, criteria = NULL) {
        if (!is.null(rank)) updateSelectInput(session, "rank_filter", selected = rank)
        if (!is.null(quality)) updateNumericInput(session, "min_quality_score", value = quality)
        if (!is.null(criteria)) updateCheckboxGroupInput(session, "criteria_filter", selected = criteria)
      },

      clear_selections = function() {
        selected_specimens(list())
        state$update_state("specimen_selections", list())
      },

      clear_flags = function() {
        flagged_specimens(list())
        state$update_state("specimen_flags", list())
      }
    )
  })
}
