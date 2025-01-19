# R/modules/specimen_handling/mod_specimen_handling_server.R

#' Server Module for Specimen Handling
#' @param id Module ID
#' @param state State management instance
#' @param processor Specimen processor instance
#' @param logger Logger instance
#' @export
mod_specimen_handling_server <- function(id, state, processor, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive Values
    filtered_data <- reactiveVal(NULL)
    selected_specimens <- reactiveVal(list())
    flagged_specimens <- reactiveVal(list())
    curator_notes <- reactiveVal(list())  # Changed from notes
    metrics <- reactiveVal(NULL)
    processing_status <- reactiveVal(list(
      is_processing = FALSE,
      message = NULL,
      error = NULL
    ))

    # Process new specimen data
    observe({
      store <- state$get_store()
      req(store$specimen_data)

      tryCatch({
        processing_status(list(
          is_processing = TRUE,
          message = "Processing specimens...",
          error = NULL
        ))

        processed_specimens <- processor$process_specimens(store$specimen_data)

        if (!is.null(processed_specimens) && nrow(processed_specimens) > 0) {
          # Update state with processed data
          state$update_state("specimen_data", processed_specimens)

          # Calculate and update metrics
          metrics <- processor$get_metrics()
          if (!is.null(metrics)) {
            state$update_state("specimen_metrics", metrics)
          }

          processing_status(list(
            is_processing = FALSE,
            message = "Processing complete",
            error = NULL
          ))

          logger$info("Specimen processing complete", list(
            total_specimens = nrow(processed_specimens),
            metrics = metrics
          ))
        }
      }, error = function(e) {
        error_msg <- sprintf("Error processing specimens: %s", e$message)
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = error_msg
        ))
        logger$error(error_msg)
        showNotification(error_msg, type = "error")
      })
    })

    # Update filtered data based on filters
    observe({
      store <- state$get_store()
      req(store$specimen_data)

      logger$info("Starting data filtering - Initial columns", list(
        initial_columns = names(store$specimen_data)
      ))

      data <- store$specimen_data

      # Apply rank filter
      if (!is.null(input$rank_filter) && input$rank_filter != "All") {
        data <- data[data$rank == as.numeric(input$rank_filter), ]
        logger$info("After rank filter", list(
          columns = names(data),
          filter_value = input$rank_filter,
          rows_remaining = nrow(data)
        ))
      }

      # Apply quality filter
      if (!is.null(input$min_quality_score) && input$min_quality_score > 0) {
        data <- data[data$quality_score >= input$min_quality_score, ]
        logger$info("After quality filter", list(
          columns = names(data),
          filter_value = input$min_quality_score,
          rows_remaining = nrow(data)
        ))
      }

      # Apply criteria filter
      if (!is.null(input$criteria_filter) && length(input$criteria_filter) > 0) {
        before_rows <- nrow(data)
        data <- data[sapply(data$criteria_met, function(x) {
          if (is.na(x) || x == "") return(FALSE)
          criteria_list <- strsplit(x, "; ")[[1]]
          all(input$criteria_filter %in% criteria_list)
        }), ]
        logger$info("After criteria filter", list(
          columns = names(data),
          filter_criteria = input$criteria_filter,
          rows_before = before_rows,
          rows_after = nrow(data)
        ))
      }

      filtered_data(data)
      metrics(calculate_metrics(data))

      logger$info("Complete filtered data info", list(
        total_records = nrow(store$specimen_data),
        filtered_records = nrow(data),
        initial_columns = names(store$specimen_data),
        final_columns = names(data),
        dropped_columns = setdiff(names(store$specimen_data), names(data))
      ))
    })


    # Initialize from state
    observe({
      store <- state$get_store()
      if (!is.null(store$specimen_curator_notes)) {
        curator_notes(store$specimen_curator_notes)
      }
    })

    # Specimen table output
    output$specimen_table <- renderDT({
      req(filtered_data())

      data <- filtered_data()
      logger$info("Starting specimen table preparation", list(
        initial_rows = nrow(data)
      ))

      tryCatch({
        # First prepare the data
        prepared_data <- prepare_module_data(
          data = data,
          current_selections = isolate(selected_specimens()),
          current_flags = isolate(flagged_specimens()),
          current_notes = isolate(curator_notes()),
          logger = logger
        )

        # Sync table states
        prepared_data <- sync_table_states(
          data = prepared_data,
          current_state = list(
            selections = isolate(selected_specimens()),
            flags = isolate(flagged_specimens()),
            notes = isolate(curator_notes())
          )
        )


        # Then create and format the table
        formatted_table <- format_specimen_table(
          data = prepared_data,
          ns = ns,
          buttons = c('copy', 'csv', 'excel'),
          page_length = 50,
          selection = 'multiple',
          current_selections = isolate(selected_specimens()),
          current_flags = isolate(flagged_specimens()),
          current_notes = isolate(curator_notes()),
          logger = logger
        )

        logger$info("Specimen table preparation complete", list(
          final_rows = nrow(prepared_data)
        ))

        formatted_table
      }, error = function(e) {
        logger$error("Error rendering specimen table", list(
          error = e$message,
          stack = e$call
        ))
        NULL
      })
    })

    # Handle specimen selection
    observeEvent(input$specimen_selection, {
      req(input$specimen_selection)
      selection <- input$specimen_selection

      logger$info("Selection event", list(
        processid = selection$processid,
        selected = selection$selected
      ))

      if (!is.null(selection$processid)) {
        current_selections <- selected_specimens()

        if (selection$selected) {
          specimen_data <- filtered_data()[
            filtered_data()$processid == selection$processid,
          ]

          if (nrow(specimen_data) > 0) {
            current_selections[[selection$processid]] <- list(
              timestamp = Sys.time(),
              species = specimen_data$species[1],
              quality_score = specimen_data$quality_score[1]
            )

            logger$info("Specimen selected", list(
              processid = selection$processid,
              species = specimen_data$species[1],
              quality_score = specimen_data$quality_score[1]
            ))
          } else {
            logger$warn("Selected specimen not found in filtered data", list(
              processid = selection$processid
            ))
          }
        } else {
          current_selections[[selection$processid]] <- NULL
          logger$info("Specimen deselected", list(
            processid = selection$processid
          ))
        }

        selected_specimens(current_selections)
        state$update_state("selected_specimens", current_selections)
      }
    })

    # Handle specimen flagging
    observeEvent(input$specimen_flag, {
      req(input$specimen_flag)
      flag <- input$specimen_flag

      if (!is.null(flag$processid)) {
        current_flags <- flagged_specimens()

        if (!is.null(flag$flag) && nchar(flag$flag) > 0) {
          current_flags[[flag$processid]] <- list(
            flag = flag$flag,
            timestamp = Sys.time(),
            species = flag$species,
            user = state$get_store()$user_info$email
          )
        } else {
          current_flags[[flag$processid]] <- NULL
        }

        flagged_specimens(current_flags)
        state$update_state("specimen_flags", current_flags)

        # Trigger table refresh to sync states
        invalidateLater(100)
      }
    })

    # Similar update for notes handler:
    observeEvent(input$specimen_notes, {
      req(input$specimen_notes)
      note <- input$specimen_notes

      if (!is.null(note$processid)) {
        current_notes <- curator_notes()

        if (!is.null(note$notes) && nchar(note$notes) > 0) {
          current_notes[[note$processid]] <- list(
            text = note$notes,
            timestamp = Sys.time(),
            user = state$get_store()$user_info$email
          )
        } else {
          current_notes[[note$processid]] <- NULL
        }

        curator_notes(current_notes)
        state$update_state("specimen_curator_notes", current_notes)

        # Trigger table refresh to sync states
        invalidateLater(100)
      }
    })

    # Value box outputs
    output$total_records_box <- renderValueBox({
      m <- metrics()
      if (is.null(m)) return(NULL)

      valueBox(
        m$total_specimens,
        "Total Records",
        icon = icon("table"),
        color = "blue"
      )
    })

    output$unique_taxa_box <- renderValueBox({
      m <- metrics()
      if (is.null(m)) return(NULL)

      valueBox(
        m$unique_species,
        "Unique Taxa",
        icon = icon("sitemap"),
        color = "green"
      )
    })

    output$unique_bins_box <- renderValueBox({
      m <- metrics()
      if (is.null(m)) return(NULL)

      valueBox(
        m$unique_bins,
        "Unique BINs",
        icon = icon("dna"),
        color = "purple"
      )
    })

    output$avg_quality_box <- renderValueBox({
      m <- metrics()
      if (is.null(m)) return(NULL)

      valueBox(
        sprintf("%.1f", m$avg_quality),
        "Average Quality Score",
        icon = icon("star"),
        color = "yellow"
      )
    })

    # Download handlers
    output$download_filtered <- downloadHandler(
      filename = function() {
        paste0("filtered_specimens_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
      },
      content = function(file) {
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) {
          return(NULL)
        }

        write.table(data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded filtered specimens", list(count = nrow(data)))
      }
    )

    output$download_selected <- downloadHandler(
      filename = function() {
        paste0("selected_specimens_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
      },
      content = function(file) {
        selected <- selected_specimens()
        if (is.null(selected) || length(selected) == 0) return(NULL)

        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) return(NULL)

        selected_data <- data[data$processid %in% names(selected), ]
        write.table(selected_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded selected specimens", list(count = nrow(selected_data)))
      }
    )

    # Calculate metrics helper
    calculate_metrics <- function(data) {
      list(
        total_specimens = nrow(data),
        unique_species = length(unique(data$species)),
        unique_bins = length(unique(data$bin_uri[!is.na(data$bin_uri)])),
        avg_quality = mean(data$quality_score, na.rm = TRUE),
        bin_coverage = sum(!is.na(data$bin_uri) & data$bin_uri != "") / nrow(data) * 100
      )
    }

    # Session cleanup
    session$onSessionEnded(function() {
      logger$info("Specimen handling session ended")
      filtered_data(NULL)
      selected_specimens(NULL)
      flagged_specimens(NULL)
      curator_notes(NULL)
      metrics(NULL)
    })

    # Return reactive values and functions
    list(
      filtered_data = filtered_data,
      selected_specimens = selected_specimens,
      flagged_specimens = flagged_specimens,
      curator_notes = curator_notes,
      metrics = metrics,
      processing_status = processing_status,

      reset_filters = function() {
        updateSelectInput(session, "rank_filter", selected = "All")
        updateNumericInput(session, "min_quality_score", value = 0)
        updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
      },

      clear_selections = function() {
        selected_specimens(list())
        state$update_state("selected_specimens", list())
      },

      clear_flags = function() {
        flagged_specimens(list())
        state$update_state("specimen_flags", list())
      }
    )
  })
}

#' Internal function to update filtered data
#' @keywords internal
update_filtered_data <- function(data, rank_filter, quality_filter,
                                 criteria_filter, filtered_data, logger) {
  tryCatch({
    filtered <- data

    # Apply rank filter
    if (!is.null(rank_filter) && rank_filter != "All") {
      filtered <- filtered[filtered$rank == as.numeric(rank_filter), ]
    }

    # Apply quality filter
    if (!is.null(quality_filter) && quality_filter > 0) {
      filtered <- filtered[filtered$quality_score >= quality_filter, ]
    }

    # Apply criteria filter
    if (!is.null(criteria_filter) && length(criteria_filter) > 0) {
      filtered <- filtered[sapply(filtered$criteria_met, function(x) {
        if (is.na(x) || x == "") return(FALSE)
        criteria_list <- strsplit(x, "; ")[[1]]
        all(criteria_filter %in% criteria_list)
      }), ]
    }

    filtered_data(filtered)
    logger$info("Updated filtered data", list(
      total_records = nrow(data),
      filtered_records = nrow(filtered)
    ))

  }, error = function(e) {
    logger$error(sprintf("Error filtering data: %s", e$message))
    filtered_data(data.frame())
  })
}

#' Validate specimen data
#' @keywords internal
validate_specimen_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, messages = "No specimen data"))
  }

  messages <- character()

  # Check required columns
  required_cols <- c("processid", "quality_score")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    messages <- c(messages, sprintf("Missing required columns: %s",
                                    paste(missing_cols, collapse = ", ")))
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}
