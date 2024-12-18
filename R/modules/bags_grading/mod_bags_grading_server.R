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
    selected_specimens <- reactiveVal(list())
    flagged_specimens <- reactiveVal(list())
    processing_status <- reactiveVal(list(
      is_processing = FALSE,
      message = NULL,
      error = NULL
    ))

    # Get specimens for current grade with organization
    grade_specimens <- reactive({
      store <- state$get_store()
      req(store$specimen_data, store$bags_grades)

      # Validate BAGS data first
      validation <- validate_bags_data(store$specimen_data, store$bags_grades)
      if (!validation$valid) {
        logger$error("Invalid BAGS data", validation$message)
        return(NULL)
      }

      # Get specimens for current grade
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == grade]
      if (length(grade_species) == 0) {
        return(NULL)
      }

      specimens <- store$specimen_data[store$specimen_data$species %in% grade_species, ]
      if (nrow(specimens) == 0) {
        return(NULL)
      }

      # Load current selections and flags from state
      selections <- state$get_store()$specimen_selections
      flags <- state$get_store()$specimen_flags

      # Add selection and flag columns
      specimens$selected <- specimens$processid %in% names(selections)
      specimens$flag <- sapply(specimens$processid, function(pid) {
        flag_record <- flags[[pid]]
        if (!is.null(flag_record)) flag_record$flag else ""
      })

      # Sort specimens by quality score within groups
      specimens <- specimens[order(-specimens$quality_score), ]

      # Organize specimens based on grade
      organize_grade_specimens(specimens, grade)
    })

    # Initialize help modal handler
    observeEvent(input$show_help, {
      showModal(
        modalDialog(
          title = paste("BAGS Grade", grade, "Help"),
          switch(grade,
                 "A" = tagList(
                   tags$p("Grade A species have:"),
                   tags$ul(
                     tags$li("More than 10 specimens"),
                     tags$li("A single BIN"),
                     tags$li("No taxonomic discordance")
                   )
                 ),
                 "B" = tagList(
                   tags$p("Grade B species have:"),
                   tags$ul(
                     tags$li("3-10 specimens"),
                     tags$li("A single BIN"),
                     tags$li("No taxonomic discordance")
                   )
                 ),
                 "C" = tagList(
                   tags$p("Grade C species have:"),
                   tags$ul(
                     tags$li("Multiple BINs"),
                     tags$li("No taxonomic discordance within BINs")
                   )
                 ),
                 "D" = tagList(
                   tags$p("Grade D species have:"),
                   tags$ul(
                     tags$li("Fewer than 3 specimens"),
                     tags$li("A single BIN")
                   )
                 ),
                 "E" = tagList(
                   tags$p("Grade E species have:"),
                   tags$ul(
                     tags$li("Taxonomic discordance within BINs"),
                     tags$li("Color coding indicates different species within shared BINs")
                   )
                 )
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        )
      )
    })

    # Status message output
    output$processing_status <- renderUI({
      status <- processing_status()
      if(status$is_processing) {
        div(
          class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          status$message
        )
      } else if(!is.null(status$error)) {
        div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          status$error
        )
      }
    })

    # Summary box outputs
    output$grade_summary_box <- renderValueBox({
      specimens <- grade_specimens()
      if (is.null(specimens)) {
        count <- 0
      } else if (is.data.frame(specimens)) {
        count <- length(unique(specimens$species))
      } else {
        count <- length(specimens)
      }

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
      specimens <- grade_specimens()
      if (is.null(specimens)) {
        count <- 0
      } else if (is.data.frame(specimens)) {
        count <- nrow(specimens)
      } else {
        count <- sum(sapply(specimens, function(x) {
          if (is.data.frame(x)) nrow(x)
          else if (is.list(x)) sum(sapply(x, nrow))
          else 0
        }))
      }

      valueBox(
        count,
        "Total Specimens",
        icon = icon("microscope"),
        color = "purple"
      )
    })

    output$bin_count_box <- renderValueBox({
      specimens <- grade_specimens()
      if (is.null(specimens)) {
        count <- 0
      } else {
        if (is.data.frame(specimens)) {
          count <- length(unique(specimens$bin_uri))
        } else {
          count <- length(unique(unlist(lapply(specimens, function(x) {
            if (is.data.frame(x)) unique(x$bin_uri)
            else if (is.list(x)) unique(unlist(lapply(x, function(y) unique(y$bin_uri))))
          }))))
        }
      }

      valueBox(
        count,
        "Unique BINs",
        icon = icon("dna"),
        color = "blue"
      )
    })

    output$selection_count_box <- renderValueBox({
      selections <- selected_specimens()
      valueBox(
        length(selections),
        "Selected Specimens",
        icon = icon("check-circle"),
        color = "green"
      )
    })

    # Specimen tables output
    output$specimen_tables <- renderUI({
      specimens <- grade_specimens()
      if(is.null(specimens)) return(NULL)

      tables <- list()

      if (grade %in% c("A", "B", "D")) {
        # Simple species tables
        for(species in names(specimens)) {
          tables[[length(tables) + 1]] <- div(
            h3(sprintf("Species: %s", species)),
            create_specimen_table(ns, specimens[[species]], species)
          )
        }
      } else if (grade == "C") {
        # Nested tables by species and BIN
        for(species in names(specimens)) {
          species_tables <- list()
          for(bin in names(specimens[[species]])) {
            species_tables[[length(species_tables) + 1]] <- div(
              h4(sprintf("BIN: %s", bin)),
              create_specimen_table(ns, specimens[[species]][[bin]],
                                    paste(species, bin, sep = "_"))
            )
          }
          tables[[length(tables) + 1]] <- div(
            h3(sprintf("Species: %s", species)),
            species_tables
          )
        }
      } else if (grade == "E") {
        # Shared BIN tables with species coloring
        for(bin in names(specimens)) {
          tables[[length(tables) + 1]] <- div(
            h3(sprintf("Shared BIN: %s", bin)),
            create_specimen_table(ns, specimens[[bin]], bin, color_by = "species")
          )
        }
      }

      do.call(tagList, tables)
    })

    # Handle specimen selection
    observeEvent(input$specimen_selection, {
      req(input$specimen_selection)

      processid <- input$specimen_selection$processid
      is_selected <- input$specimen_selection$selected

      specimens <- grade_specimens()
      specimen_data <- find_specimen_in_groups(specimens, processid)

      if (!is.null(specimen_data)) {
        current_selections <- selected_specimens()

        if (is_selected) {
          current_selections[[processid]] <- list(
            timestamp = Sys.time(),
            species = specimen_data$species
          )
          logger$info(sprintf("Selected specimen %s", processid),
                      list(grade = grade, species = specimen_data$species))
        } else {
          current_selections[[processid]] <- NULL
          logger$info(sprintf("Deselected specimen %s", processid),
                      list(grade = grade, species = specimen_data$species))
        }

        selected_specimens(current_selections)
        state$update_state("specimen_selections", current_selections)
      }
    })

    # Handle specimen flagging
    observeEvent(input$specimen_flag, {
      req(input$specimen_flag)

      processid <- input$specimen_flag$processid
      flag_value <- input$specimen_flag$flag

      specimens <- grade_specimens()
      specimen_data <- find_specimen_in_groups(specimens, processid)

      if (!is.null(specimen_data)) {
        current_flags <- flagged_specimens()

        if (nchar(flag_value) > 0) {
          current_flags[[processid]] <- list(
            flag = flag_value,
            timestamp = Sys.time(),
            species = specimen_data$species
          )
          logger$info(sprintf("Flagged specimen %s as %s",
                              processid, flag_value),
                      list(grade = grade, species = specimen_data$species))
        } else {
          current_flags[[processid]] <- NULL
          logger$info(sprintf("Removed flag from specimen %s", processid),
                      list(grade = grade, species = specimen_data$species))
        }

        flagged_specimens(current_flags)
        state$update_state("specimen_flags", current_flags)
      }
    })

    # Handle filter changes
    observeEvent(input$apply_filters, {
      specimens <- grade_specimens()
      if (!is.null(specimens)) {
        filtered <- filter_grade_specimens(
          specimens,
          state$get_store()$bags_grades,
          grade,
          input$rank_filter,
          input$min_quality_score,
          input$criteria_filter
        )
        # Update tables with filtered data
        grade_specimens(filtered)
      }
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "rank_filter", selected = "All")
      updateNumericInput(session, "min_quality_score", value = 0)
      updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("grade_", grade, "_specimens_",
               format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        specimens <- grade_specimens()
        if (is.null(specimens)) return(NULL)

        # Get current selections and flags
        selections <- selected_specimens()
        flags <- flagged_specimens()

        # Prepare data for download
        download_data <- prepare_download_data(specimens, grade, selections, flags)
        write.csv(download_data, file, row.names = FALSE)

        # Log download
        user_info <- state$get_store()$user_info
        logger$info("Downloaded grade data",
                    list(grade = grade,
                         user = user_info$email,
                         records = nrow(download_data)))
      }
    )

    # Handle specimen detail modal
    observeEvent(input$view_specimen_details, {
      req(input$view_specimen_details)
      processid <- input$view_specimen_details
      specimens <- grade_specimens()
      specimen_data <- find_specimen_in_groups(specimens, processid)

      if (!is.null(specimen_data)) {
        showModal(modalDialog(
          title = "Specimen Details",
          renderUI({
            div(
              tags$h4(specimen_data$species),
              tags$table(
                class = "table table-striped",
                tags$tbody(
                  tags$tr(
                    tags$th("Process ID"),
                    tags$td(specimen_data$processid)
                  ),
                  tags$tr(
                    tags$th("BIN"),
                    tags$td(specimen_data$bin_uri)
                  ),
                  tags$tr(
                    tags$th("Quality Score"),
                    tags$td(specimen_data$quality_score)
                  ),
                  tags$tr(
                    tags$th("Specimen Rank"),
                    tags$td(specimen_data$specimen_rank)
                  ),
                  tags$tr(
                    tags$th("Criteria Met"),
                    tags$td(gsub("; ", "<br>", specimen_data$criteria_met),
                            HTML = TRUE)
                  )
                )
              )
            )
          }),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })

    # Load saved selections and flags on initialization
    observe({
      store <- state$get_store()
      req(store$user_info)

      withProgress(
        message = 'Loading specimen data',
        detail = 'Retrieving saved selections and flags...',
        value = 0,
        {
          tryCatch({
            # Load selections from state
            incProgress(0.4, detail = "Loading selections...")
            selections <- store$specimen_selections
            if(!is.null(selections)) {
              selected_specimens(selections)
            }

            # Load flags from state
            incProgress(0.4, detail = "Loading flags...")
            flags <- store$specimen_flags
            if(!is.null(flags)) {
              flagged_specimens(flags)
            }

            incProgress(0.2, detail = "Complete")
            logger$info("Loaded saved selections and flags")
          }, error = function(e) {
            logger$error("Failed to load saved selections and flags",
                         list(error = e$message))
          })
        }
      )
    })

    # Session cleanup
    session$onSessionEnded(function() {
      logger$info(sprintf("Grade %s session ended", grade))
    })

    # Return reactive values and functions
    list(
      grade_specimens = grade_specimens,
      selected_specimens = selected_specimens,
      flagged_specimens = flagged_specimens,
      processing_status = processing_status,

      # Helper functions
      get_specimen_metrics = reactive({
        specimens <- grade_specimens()
        if (is.null(specimens)) {
          return(list(
            total_specimens = 0,
            total_species = 0,
            total_bins = 0,
            selected_count = 0,
            flagged_count = 0
          ))
        }

        list(
          total_specimens = if (is.data.frame(specimens)) nrow(specimens)
          else sum(sapply(specimens, nrow)),
          total_species = if (is.data.frame(specimens)) length(unique(specimens$species))
          else length(specimens),
          total_bins = if (is.data.frame(specimens)) length(unique(specimens$bin_uri))
          else length(unique(unlist(lapply(specimens, function(x) {
            if (is.data.frame(x)) unique(x$bin_uri)
            else unique(unlist(lapply(x, function(y) unique(y$bin_uri))))
          })))),
          selected_count = length(selected_specimens()),
          flagged_count = length(flagged_specimens())
        )
      }),

      validate_specimen = function(processid) {
        specimens <- grade_specimens()
        if (is.null(specimens)) return(FALSE)

        specimen_data <- find_specimen_in_groups(specimens, processid)
        !is.null(specimen_data)
      },

      get_specimen_details = function(processid) {
        specimens <- grade_specimens()
        if (is.null(specimens)) return(NULL)

        find_specimen_in_groups(specimens, processid)
      },

      reset_selections = function() {
        selected_specimens(list())
        state$update_state("specimen_selections", list())
      },

      reset_flags = function() {
        flagged_specimens(list())
        state$update_state("specimen_flags", list())
      },

      export_data = function(format = c("csv", "excel")) {
        format <- match.arg(format)
        specimens <- grade_specimens()
        if (is.null(specimens)) return(NULL)

        # Get current selections and flags
        selections <- selected_specimens()
        flags <- flagged_specimens()

        # Prepare data
        export_data <- prepare_download_data(specimens, grade, selections, flags)

        if (format == "excel") {
          sheets <- list(
            Specimens = export_data,
            Metrics = as.data.frame(get_specimen_metrics()),
            Selections = as.data.frame(selections),
            Flags = as.data.frame(flags)
          )
          return(sheets)
        }

        export_data
      }
    )
  })
}
