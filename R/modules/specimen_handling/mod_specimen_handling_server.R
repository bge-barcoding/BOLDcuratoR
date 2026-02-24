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

    # Initialize reactive values
    rv <- reactiveValues(
      filtered_data = NULL,
      metrics = NULL,
      processing_status = list(
        is_processing = FALSE,
        message = NULL,
        error = NULL
      )
    )

    # Proxy for updating specimen table in-place when annotations change
    specimen_proxy <- DT::dataTableProxy("specimen_table")

    # Watch for annotation changes AND data changes, then refresh the
    # specimen table via replaceData().  rv$filtered_data is reactive
    # (not isolated) so this fires on both annotation edits AND
    # filter/tab-switch changes — fixing the stale-data-on-tab-switch bug.
    observe({
      store <- state$get_store()

      # Reactive dependencies: annotation keys
      current_selections <- store$selected_specimens
      current_flags      <- store$specimen_flags
      current_notes      <- store$specimen_curator_notes
      current_uids       <- store$specimen_updated_ids

      # Reactive dependency: filtered data (NOT isolated)
      data <- rv$filtered_data
      if (is.null(data) || nrow(data) == 0) return()

      prepared <- prepare_module_data(
        data = data,
        current_selections = current_selections,
        current_flags = current_flags,
        current_notes = current_notes,
        current_updated_ids = current_uids,
        logger = logger
      )

      # order_columns() must match the column order used by the initial
      # renderDT (which calls format_specimen_table → order_columns).
      prepared <- order_columns(prepared)

      DT::replaceData(specimen_proxy, prepared, resetPaging = FALSE, rownames = FALSE)
      logger$info("Specimen table refreshed with updated annotations")
    })

    # Update specimen processing observer to use rv instead of processing_status function
    observe({
      store <- state$get_store()
      req(store$specimen_data)

      tryCatch({
        # Update processing status through rv
        isolate({
          rv$processing_status <- list(
            is_processing = TRUE,
            message = "Processing specimens...",
            error = NULL
          )
        })

        processed_specimens <- processor$process_specimens(store$specimen_data)

        if (!is.null(processed_specimens) && nrow(processed_specimens) > 0) {
          # Update state with processed data
          state$update_state("specimen_data", processed_specimens)

          # Calculate and update metrics
          metrics <- processor$get_metrics()
          if (!is.null(metrics)) {
            state$update_state("specimen_metrics", metrics)
            # Also update local metrics in rv for UI responsiveness
            isolate({
              rv$metrics <- metrics
            })
          }

          # Update processing status to complete
          isolate({
            rv$processing_status <- list(
              is_processing = FALSE,
              message = "Processing complete",
              error = NULL
            )
          })

          logger$info("Specimen processing complete", list(
            total_specimens = nrow(processed_specimens),
            metrics = metrics
          ))
        }
      }, error = function(e) {
        error_msg <- sprintf("Error processing specimens: %s", e$message)
        # Update error state through rv
        isolate({
          rv$processing_status <- list(
            is_processing = FALSE,
            message = NULL,
            error = error_msg
          )
        })
        logger$error(error_msg)
        showNotification(error_msg, type = "error")
      })
    })

    # Update the filtered data observer to use rv instead of reactiveVal
    observe({
      store <- state$get_store()

      # Clear local state when specimen data is removed (e.g. Clear Results)
      if (is.null(store$specimen_data)) {
        isolate({
          rv$filtered_data <- NULL
          rv$metrics <- NULL
        })
        return()
      }

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

      # Update local state with filtered data and metrics
      isolate({
        rv$filtered_data <- data

        # Calculate new metrics
        new_metrics <- calculate_metrics(data)
        rv$metrics <- new_metrics

        # Update global state with new metrics
        state$update_state("metrics", new_metrics)
      })

      # Log the final filtered data info
      logger$info("Complete filtered data info", list(
        total_records = nrow(store$specimen_data),
        filtered_records = nrow(data),
        initial_columns = names(store$specimen_data),
        final_columns = names(data),
        dropped_columns = setdiff(names(store$specimen_data), names(data))
      ))
    })

    # Render the specimen table (view-only).
    # Annotations are read reactively so the table re-renders when
    # flags/notes change in BAGS tabs.  Shiny's suspendWhenHidden
    # (default TRUE) defers the re-render until the Specimens tab is
    # active, so hidden tabs don't incur unnecessary work.
    output$specimen_table <- renderDT({
      req(rv$filtered_data)
      data <- rv$filtered_data

      logger$info("Rendering specimen table", list(rows = nrow(data)))

      store <- state$get_store()

      # Read annotations reactively so the table re-renders when they
      # change (e.g. after editing in BAGS grade tabs).
      prepared <- prepare_module_data(
        data = data,
        current_selections = store$selected_specimens,
        current_flags = store$specimen_flags,
        current_notes = store$specimen_curator_notes,
        current_updated_ids = store$specimen_updated_ids,
        logger = logger
      )

      format_specimen_table(
        data = prepared,
        ns = NULL,
        buttons = list(),
        page_length = 20,
        selection = 'none',
        logger = logger,
        dom = "frtip",
        read_only = TRUE,
        scroll_y = "400px"
      )
    })

    # Value box outputs
    output$total_records_box <- renderValueBox({
      # Change from metrics() to rv$metrics
      m <- rv$metrics
      if (is.null(m)) return(NULL)

      valueBox(
        m$total_specimens,
        "Total Records",
        icon = icon("table"),
        color = "blue"
      )
    })

    output$unique_taxa_box <- renderValueBox({
      m <- rv$metrics
      if (is.null(m)) return(NULL)

      valueBox(
        m$unique_species,
        "Unique Taxa",
        icon = icon("sitemap"),
        color = "green"
      )
    })

    output$unique_bins_box <- renderValueBox({
      m <- rv$metrics
      if (is.null(m)) return(NULL)

      valueBox(
        m$unique_bins,
        "Unique BINs",
        icon = icon("dna"),
        color = "purple"
      )
    })

    output$avg_quality_box <- renderValueBox({
      m <- rv$metrics
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
        data <- rv$filtered_data
        if (is.null(data) || nrow(data) == 0) {
          writeLines("No filtered specimens to download.", file)
          return()
        }

        store <- state$get_store()
        data <- merge_annotations_for_export(
          data,
          selections = store$selected_specimens,
          flags = store$specimen_flags,
          notes = store$specimen_curator_notes,
          updated_ids = store$specimen_updated_ids
        )

        write.table(data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded filtered specimens", list(count = nrow(data)))
      },
      contentType = "text/tab-separated-values"
    )

    # Download selected specimens with clean annotations
    output$download_selected <- downloadHandler(
      filename = function() {
        paste0("selected_specimens_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
      },
      content = function(file) {
        data <- rv$filtered_data
        store <- state$get_store()
        selected_ids <- names(store$selected_specimens)

        if (is.null(data) || nrow(data) == 0 ||
            length(selected_ids) == 0) {
          # Write an empty TSV so Shiny never serves an HTML error page
          writeLines("No selected specimens to download.", file)
          return()
        }

        selected_data <- data[data$processid %in% selected_ids, ]
        if (nrow(selected_data) == 0) {
          writeLines("No selected specimens to download.", file)
          return()
        }

        selected_data <- merge_annotations_for_export(
          selected_data,
          selections = store$selected_specimens,
          flags = store$specimen_flags,
          notes = store$specimen_curator_notes,
          updated_ids = store$specimen_updated_ids
        )

        write.table(selected_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded selected specimens", list(count = nrow(selected_data)))
      },
      contentType = "text/tab-separated-values"
    )

    # Download annotated (flagged or noted) specimens
    output$download_annotated <- downloadHandler(
      filename = function() {
        paste0("annotated_specimens_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
      },
      content = function(file) {
        data <- rv$filtered_data
        store <- state$get_store()
        flags <- store$specimen_flags
        notes <- store$specimen_curator_notes
        uids <- store$specimen_updated_ids

        # Get processids with any annotation (flag, note, or updated_id)
        annotated_ids <- unique(c(names(flags), names(notes), names(uids)))

        if (is.null(data) || nrow(data) == 0 ||
            length(annotated_ids) == 0) {
          writeLines("No annotated specimens to download.", file)
          return()
        }

        annotated_data <- data[data$processid %in% annotated_ids, ]
        if (nrow(annotated_data) == 0) {
          writeLines("No annotated specimens to download.", file)
          return()
        }

        annotated_data <- merge_annotations_for_export(
          annotated_data,
          selections = store$selected_specimens,
          flags = flags,
          notes = notes,
          updated_ids = store$specimen_updated_ids
        )

        write.table(annotated_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded annotated specimens", list(count = nrow(annotated_data)))
      },
      contentType = "text/tab-separated-values"
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
      isolate({
        rv$filtered_data <- NULL
        rv$metrics <- NULL
      })
    })

    # Return reactive values
    list(
      filtered_data = reactive({ rv$filtered_data }),
      metrics = reactive({ rv$metrics }),
      processing_status = reactive({ rv$processing_status }),

      reset_filters = function() {
        updateSelectInput(session, "rank_filter", selected = "All")
        updateNumericInput(session, "min_quality_score", value = 0)
        updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
      }
    )
  })
}
