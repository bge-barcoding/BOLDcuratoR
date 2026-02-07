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

    # Initialize reactive values with centralized state management
    rv <- reactiveValues(
      filtered_data = NULL,
      selected_specimens = list(),
      flagged_specimens = list(),
      curator_notes = list(),
      metrics = NULL,
      processing_status = list(
        is_processing = FALSE,
        message = NULL,
        error = NULL
      )
    )

    # Sync annotations from StateManager into local rv.
    # This fires whenever StateManager flags/notes/selections change
    # (e.g. when another module writes to state), keeping rv in sync.
    observe({
      store <- state$get_store()

      # Read state using correct StateManager key names
      state_selections <- store$selected_specimens
      state_flags      <- store$specimen_flags
      state_notes      <- store$specimen_curator_notes

      isolate({
        if (!is.null(state_selections) && !identical(rv$selected_specimens, state_selections)) {
          rv$selected_specimens <- state_selections
        }
        if (!is.null(state_flags) && !identical(rv$flagged_specimens, state_flags)) {
          rv$flagged_specimens <- state_flags
        }
        if (!is.null(state_notes) && !identical(rv$curator_notes, state_notes)) {
          rv$curator_notes <- state_notes
        }
      })
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

    # Helper: build the prepared data frame with current annotations injected.
    build_prepared_data <- function(data) {
      current_sel   <- rv$selected_specimens
      current_flags <- rv$flagged_specimens
      current_notes <- rv$curator_notes

      prepared <- prepare_module_data(
        data = data,
        current_selections = current_sel,
        current_flags = current_flags,
        current_notes = current_notes,
        logger = logger
      )

      sync_table_states(
        data = prepared,
        current_state = list(
          selections = current_sel,
          flags = current_flags,
          notes = current_notes
        )
      )
    }

    # Render the specimen table.
    # Only reactive to rv$filtered_data so that filter changes trigger a
    # full re-render. Annotation changes are pushed via DT proxy below.
    output$specimen_table <- renderDT({
      req(rv$filtered_data)
      data <- rv$filtered_data

      logger$info("Starting specimen table preparation", list(
        initial_rows = nrow(data)
      ))

      tryCatch({
        prepared_data <- isolate(build_prepared_data(data))

        formatted_table <- format_specimen_table(
          data = prepared_data,
          ns = ns,
          buttons = c('copy', 'csv', 'excel'),
          page_length = 50,
          selection = 'multiple',
          current_selections = isolate(rv$selected_specimens),
          current_flags = isolate(rv$flagged_specimens),
          current_notes = isolate(rv$curator_notes),
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

    # DT proxy: push annotation changes to the existing table without
    # a full widget replacement. Preserves search, scroll, and page state.
    specimen_proxy <- dataTableProxy("specimen_table")

    observe({
      # Reactive dependencies: re-fire when annotations change
      sel   <- rv$selected_specimens
      flags <- rv$flagged_specimens
      notes <- rv$curator_notes

      data <- isolate(rv$filtered_data)
      if (is.null(data) || nrow(data) == 0) return()

      prepared <- isolate(build_prepared_data(data))
      replaceData(specimen_proxy, prepared, resetPaging = FALSE, clearSelection = "none")
    })

    # Update specimen selection handler
    # This version ensures proper state synchronization and adds validation
    observeEvent(input$specimen_selection, {
      req(input$specimen_selection)
      selection <- input$specimen_selection

      logger$info("Processing selection event", list(
        processid = selection$processid,
        selected = selection$selected
      ))

      if (!is.null(selection$processid)) {
        # Isolate the current state update to prevent reactivity cycles
        isolate({
          # First get current selections from both local and global state
          current_selections <- rv$selected_specimens
          global_selections <- state$get_store()$selected_specimens

          # Merge global selections if they exist and differ from local
          if (!is.null(global_selections) && !identical(current_selections, global_selections)) {
            current_selections <- global_selections
            logger$info("Merged global selections with local state")
          }

          if (selection$selected) {
            # Get specimen data with proper error handling
            tryCatch({
              specimen_data <- rv$filtered_data[
                rv$filtered_data$processid == selection$processid,
              ]

              if (nrow(specimen_data) > 0) {
                # Create properly formatted selection entry
                current_selections[[selection$processid]] <- list(
                  timestamp = Sys.time(),
                  species = specimen_data$species[1],
                  quality_score = specimen_data$quality_score[1],
                  user = state$get_store()$user_info$email,
                  selected = TRUE
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
                return()
              }
            }, error = function(e) {
              logger$error("Error processing specimen selection", list(
                error = e$message,
                processid = selection$processid
              ))
              return()
            })
          } else {
            # Handle deselection
            current_selections[[selection$processid]] <- NULL
            logger$info("Specimen deselected", list(
              processid = selection$processid
            ))
          }

          # Update local state first
          rv$selected_specimens <- current_selections

          # Then update global state with validation
          state$update_state(
            "selected_specimens",
            current_selections,
            validation_fn = function(value) {
              if (!is.list(value)) {
                return(list(valid = FALSE, messages = "Selections must be a list"))
              }
              list(valid = TRUE, messages = NULL)
            }
          )

          # Log the sync status
          logger$info("Selection state synchronized", list(
            local_count = length(rv$selected_specimens),
            global_count = length(state$get_store()$selected_specimens)
          ))

        })
      }
    })

    # Handle specimen flagging
    observeEvent(input$specimen_flag, {
      req(input$specimen_flag)
      flag <- input$specimen_flag

      if (!is.null(flag$processid)) {
        # Change from flagged_specimens() to rv$flagged_specimens
        isolate({
          current_flags <- rv$flagged_specimens

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

          # Update rv instead of using flagged_specimens()
          rv$flagged_specimens <- current_flags
          state$update_state("specimen_flags", current_flags)

        })
      }
    })

    # Similar update for notes handler:
    observeEvent(input$specimen_notes, {
      req(input$specimen_notes)
      note <- input$specimen_notes

      if (!is.null(note$processid)) {
        # Change from curator_notes() to rv$curator_notes
        isolate({
          current_notes <- rv$curator_notes

          if (!is.null(note$notes) && nchar(note$notes) > 0) {
            current_notes[[note$processid]] <- list(
              text = note$notes,
              timestamp = Sys.time(),
              user = state$get_store()$user_info$email
            )
          } else {
            current_notes[[note$processid]] <- NULL
          }

          # Update rv instead of using curator_notes()
          rv$curator_notes <- current_notes
          state$update_state("specimen_curator_notes", current_notes)

        })
      }
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
        # Change from filtered_data() to rv$filtered_data
        data <- rv$filtered_data
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
        # Change from selected_specimens() and filtered_data() to rv
        selected <- rv$selected_specimens
        if (is.null(selected) || length(selected) == 0) return(NULL)

        data <- rv$filtered_data
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

    # Session cleanup with rv
    session$onSessionEnded(function() {
      logger$info("Specimen handling session ended")
      isolate({
        rv$filtered_data <- NULL
        rv$selected_specimens <- NULL
        rv$flagged_specimens <- NULL
        rv$curator_notes <- NULL
        rv$metrics <- NULL
      })
    })

    # Return reactive values and functions using rv
    # Update module return values
    list(
      filtered_data = reactive({ rv$filtered_data }),
      selected_specimens = reactive({ rv$selected_specimens }),
      flagged_specimens = reactive({ rv$flagged_specimens }),
      curator_notes = reactive({ rv$curator_notes }),
      metrics = reactive({ rv$metrics }),
      processing_status = reactive({ rv$processing_status }),

      # Helper functions for state management
      reset_filters = function() {
        updateSelectInput(session, "rank_filter", selected = "All")
        updateNumericInput(session, "min_quality_score", value = 0)
        updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
      },

      clear_selections = function() {
        rv$selected_specimens <- list()
        state$update_state("selected_specimens", list())
      },

      clear_flags = function() {
        rv$flagged_specimens <- list()
        state$update_state("specimen_flags", list())
      }
    )
  })
}
