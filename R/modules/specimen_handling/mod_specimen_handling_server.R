# R/modules/specimen_handling/mod_specimen_handling_server.R

#' Server Module for Specimen Handling
#' @param id Module ID
#' @param state State management instance
#' @param processor Specimen processor instance
#' @param logger Logger instance
#' @export
mod_specimen_handling_server <- function(id, state, processor, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Reactive Values
    filtered_data <- reactiveVal(NULL)
    processing_status <- reactiveVal(list(
      is_processing = FALSE,
      message = NULL,
      error = NULL
    ))

    # Handle specimen data updates
    observe({
      store <- state$get_store()
      req(store$specimen_data)

      # Validate incoming data
      validation <- validate_specimen_data(store$specimen_data)
      if (!validation$valid) {
        logger$error("Invalid specimen data", validation$messages)
        showNotification(paste(validation$messages, collapse = "; "), type = "error")
        return()
      }

      # Apply filters and update filtered data
      update_filtered_data(
        data = store$specimen_data,
        rank_filter = input$rank_filter,
        quality_filter = input$min_quality_score,
        criteria_filter = input$criteria_filter,
        filtered_data = filtered_data,
        logger = logger
      )
    })

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

    # Update rank distribution boxes
    observe({
      req(filtered_data())
      data <- filtered_data()

      for (rank in 1:7) {
        local({
          r <- rank
          output[[paste0("rank_", r, "_box")]] <- renderValueBox({
            count <- sum(data$specimen_rank == r, na.rm = TRUE)
            valueBox(
              count,
              paste("Rank", r, "Specimens"),
              icon = icon(switch(as.character(r),
                                 "1" = "trophy",
                                 "2" = "medal",
                                 "3" = "award",
                                 "4" = "check",
                                 "5" = "circle-check",
                                 "6" = "circle",
                                 "7" = "circle-xmark"
              )),
              color = if (r <= 2) "green" else if (r <= 4) "blue" else "orange"
            )
          })
        })
      }
    })

    # Render specimen table
    output$specimen_table <- renderDT({
      req(filtered_data())
      data <- filtered_data()

      if (!is.data.frame(data) || nrow(data) == 0) {
        return(NULL)
      }

      format_specimen_table(
        data,
        buttons = c('copy', 'csv', 'excel'),
        selection = 'multiple'
      )
    })

    # Handle specimen selection
    observeEvent(input$specimen_table_rows_selected, {
      selected_rows <- input$specimen_table_rows_selected
      if (length(selected_rows) == 0) return()

      data <- filtered_data()
      if (is.null(data)) return()

      selected_specimens <- data[selected_rows, ]

      # Update state with new selections
      current_selections <- state$get_store()$selected_specimens
      if (!is.list(current_selections)) {
        current_selections <- list()
      }

      # Group by species
      species_groups <- split(selected_specimens, selected_specimens$species)

      for (species in names(species_groups)) {
        specimens <- species_groups[[species]]
        if (nrow(specimens) > 0) {
          # Select best specimen for each species
          best_specimen <- specimens[order(-specimens$quality_score, specimens$specimen_rank)[1], ]
          current_selections[[species]] <- best_specimen$processid

          # Log selection
          logger$info("Specimen selected", list(
            species = species,
            processid = best_specimen$processid,
            quality_score = best_specimen$quality_score,
            rank = best_specimen$specimen_rank
          ))
        }
      }

      state$update_state("selected_specimens", current_selections)
    })

    # Render selection history
    output$selection_history <- renderDT({
      store <- state$get_store()
      req(store$selected_specimens)

      if (length(store$selected_specimens) == 0) return(NULL)

      history_data <- data.frame(
        Species = names(store$selected_specimens),
        ProcessID = unlist(store$selected_specimens),
        stringsAsFactors = FALSE
      )

      specimen_data <- store$specimen_data
      if (!is.null(specimen_data)) {
        history_data <- merge(
          history_data,
          specimen_data[, c("processid", "quality_score", "specimen_rank", "country.ocean")],
          by.x = "ProcessID",
          by.y = "processid",
          all.x = TRUE
        )
      }

      format_history_table(history_data)
    })

    # Handle clear filters
    observeEvent(input$clear_filters, {
      updateSelectInput(session, "rank_filter", selected = "All")
      updateNumericInput(session, "min_quality_score", value = 0)
      updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))

      logger$info("Filters cleared")
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
        selected_rows <- input$specimen_table_rows_selected
        if (is.null(selected_rows)) return(NULL)

        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) return(NULL)

        selected_data <- data[selected_rows, ]
        write.table(selected_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded selected specimens", list(count = nrow(selected_data)))
      }
    )

    # Handle help modal
    observeEvent(input$show_help, {
      showModal(modalDialog(
        title = "Specimen Handling Help",
        tags$div(
          tags$h4("Working with Specimens"),
          tags$ul(
            tags$li("Use filters to narrow down specimens by rank, quality score, and criteria"),
            tags$li("Select specimens by clicking rows in the specimen table"),
            tags$li("The system automatically selects the highest quality specimen for each species"),
            tags$li("View selection history at the bottom to track your choices"),
            tags$li("Download filtered or selected specimens using the export buttons")
          ),
          tags$h4("Understanding Ranks"),
          tags$ul(
            tags$li("Rank 1-2: Highest quality specimens with type material"),
            tags$li("Rank 3-4: Well-documented specimens with good metadata"),
            tags$li("Rank 5-6: Basic specimens meeting minimal criteria"),
            tags$li("Rank 7: Specimens missing key information")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Return reactive values and functions
    list(
      filtered_data = filtered_data,
      processing_status = processing_status,
      processor = processor,

      # Helper functions
      update_filters = function(rank = NULL, quality = NULL, criteria = NULL) {
        if (!is.null(rank)) updateSelectInput(session, "rank_filter", selected = rank)
        if (!is.null(quality)) updateNumericInput(session, "min_quality_score", value = quality)
        if (!is.null(criteria)) updateCheckboxGroupInput(session, "criteria_filter", selected = criteria)
      },

      clear_selections = function() {
        selectRows(dataTableProxy("specimen_table"), NULL)
        state$update_state("selected_specimens", list())
      },

      get_selection_summary = function() {
        store <- state$get_store()
        selections <- store$selected_specimens
        if (is.null(selections)) return(NULL)

        list(
          total_selected = length(selections),
          species_covered = length(unique(names(selections))),
          avg_quality = mean(sapply(selections, function(pid) {
            specimen <- store$specimen_data[store$specimen_data$processid == pid, ]
            specimen$quality_score
          }))
        )
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
      filtered <- filtered[filtered$specimen_rank == as.numeric(rank_filter), ]
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
  required_cols <- c("processid", "species", "bin_uri", "quality_score", "specimen_rank")
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
