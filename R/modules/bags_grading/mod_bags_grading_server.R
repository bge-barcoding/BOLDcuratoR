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

      # Get specimens for current grade
      grade_species <- store$bags_grades$species[store$bags_grades$bags_grade == grade]
      if (length(grade_species) == 0) {
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = "No species found for this grade"
        ))
        return(NULL)
      }

      specimens <- store$specimen_data[store$specimen_data$species %in% grade_species, ]
      if (nrow(specimens) == 0) {
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = "No specimens found for this grade"
        ))
        return(NULL)
      }

      # Update filtered data
      filtered_data(specimens)
      processing_status(list(
        is_processing = FALSE,
        message = "Data loaded successfully",
        error = NULL
      ))
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
      valueBox(
        length(selections),
        "Selected Specimens",
        icon = icon("check-circle"),
        color = "green"
      )
    })

    # Specimen tables output
    output$specimen_tables <- renderUI({
      data <- filtered_data()
      if (is.null(data)) return(NULL)

      withProgress(
        message = 'Creating specimen tables',
        detail = 'Processing data...',
        value = 0,
        {
          tryCatch({
            # Organize specimens based on grade
            incProgress(0.2, detail = "Organizing data...")
            organized <- organize_specimens_by_grade(data, grade)

            # Create tables based on organization
            incProgress(0.4, detail = "Creating tables...")
            tables <- create_grade_tables(organized, grade, ns)

            incProgress(0.4, detail = "Finalizing...")
            do.call(tagList, tables)

          }, error = function(e) {
            logger$error("Error creating specimen tables", e$message)
            div(class = "alert alert-danger",
                "Error creating tables. Please try refreshing the page.")
          })
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
          specimen_data <- filtered_data()[
            filtered_data()$processid == selection$processid,
          ]

          if (nrow(specimen_data) > 0) {
            current_selections[[selection$processid]] <- list(
              timestamp = Sys.time(),
              species = specimen_data$species[1],
              grade = grade
            )
            logger$info("Specimen selected", list(
              processid = selection$processid,
              species = specimen_data$species[1],
              grade = grade
            ))
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
              grade = grade
            )
            logger$info("Specimen flagged", list(
              processid = flag$processid,
              flag = flag$flag,
              species = specimen_data$species[1],
              grade = grade
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

    # Handle filter changes
    observeEvent(input$apply_filters, {
      data <- filtered_data()
      if (!is.null(data)) {
        withProgress(
          message = 'Applying filters',
          value = 0,
          {
            filtered <- data

            # Apply rank filter
            if (!is.null(input$rank_filter) && input$rank_filter != "All") {
              filtered <- filtered[filtered$specimen_rank == as.numeric(input$rank_filter), ]
            }

            # Apply quality score filter
            if (!is.null(input$min_quality_score) && input$min_quality_score > 0) {
              filtered <- filtered[filtered$quality_score >= input$min_quality_score, ]
            }

            # Apply criteria filter
            if (!is.null(input$criteria_filter) && length(input$criteria_filter) > 0) {
              filtered <- filtered[sapply(filtered$criteria_met, function(x) {
                if (is.na(x) || x == "") return(FALSE)
                criteria_list <- strsplit(x, "; ")[[1]]
                all(input$criteria_filter %in% criteria_list)
              }), ]
            }

            filtered_data(filtered)
            logger$info("Filters applied", list(
              grade = grade,
              filtered_count = nrow(filtered)
            ))
          }
        )
      }
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "rank_filter", selected = "All")
      updateNumericInput(session, "min_quality_score", value = 0)
      updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
      logger$info("Filters reset", list(grade = grade))
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("grade_", grade, "_specimens_",
               format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        withProgress(
          message = 'Preparing download',
          value = 0,
          {
            tryCatch({
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
                records = nrow(download_data)
              ))
            }, error = function(e) {
              logger$error("Download failed", e$message)
              stop(paste("Error downloading data:", e$message))
            })
          }
        )
      }
    )

    # Help modal
    observeEvent(input$show_help, {
      showModal(modalDialog(
        title = paste("BAGS Grade", grade, "Help"),
        tagList(
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
          hr(),
          tags$p("Table Features:"),
          tags$ul(
            tags$li("Use checkboxes to select representative specimens"),
            tags$li("Use flag dropdown to mark potential issues"),
            tags$li("Sort columns by clicking headers"),
            tags$li("Filter data using the controls above")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Session cleanup
    session$onSessionEnded(function() {
      logger$info(sprintf("Grade %s session ended", grade))
    })

    # Return reactive values and functions
    list(
      filtered_data = filtered_data,
      selected_specimens = selected_specimens,
      flagged_specimens = flagged_specimens,
      processing_status = processing_status,

      # Helper functions
      reset_filters = function() {
        updateSelectInput(session, "rank_filter", selected = "All")
        updateNumericInput(session, "min_quality_score", value = 0)
        updateCheckboxGroupInput(session, "criteria_filter", selected = character(0))
      },

      get_statistics = function() {
        data <- filtered_data()
        if (is.null(data)) {
          return(list(
            total_specimens = 0,
            unique_species = 0,
            unique_bins = 0,
            selected_count = length(selected_specimens()),
            flagged_count = length(flagged_specimens())
          ))
        }

        list(
          total_specimens = nrow(data),
          unique_species = length(unique(data$species)),
          unique_bins = length(unique(data$bin_uri)),
          selected_count = length(selected_specimens()),
          flagged_count = length(flagged_specimens())
        )
      }
    )
  })
}

#' Organize specimens by grade type
#' @param specimens Data frame of specimens
#' @param grade BAGS grade
#' @return Organized list of specimens
#' @keywords internal
organize_specimens_by_grade <- function(specimens, grade) {
  switch(grade,
         "A" = split(specimens, specimens$species),
         "B" = split(specimens, specimens$species),
         "D" = split(specimens, specimens$species),
         "C" = {
           species_groups <- split(specimens, specimens$species)
           lapply(species_groups, function(group) {
             split(group, group$bin_uri)
           })
         },
         "E" = {
           bins <- unique(specimens$bin_uri)
           bin_groups <- list()
           for (bin in bins) {
             bin_specimens <- specimens[specimens$bin_uri == bin,]
             if (length(unique(bin_specimens$species)) > 1) {
               bin_groups[[bin]] <- bin_specimens
             }
           }
           bin_groups
         }
  )
}

#' Create tables based on grade organization
#' @param organized Organized specimen data
#' @param grade BAGS grade
#' @param ns Namespace function
#' @return List of table UI elements
#' @keywords internal
create_grade_tables <- function(organized, grade, ns) {
  tables <- list()

  if (grade %in% c("A", "B", "D")) {
    for (species in names(organized)) {
      tables[[length(tables) + 1]] <- div(
        h3(sprintf("Species: %s", species)),
        create_specimen_table(ns, organized[[species]], species)
      )
    }
  } else if (grade == "C") {
    for (species in names(organized)) {
      species_tables <- list()
      for (bin in names(organized[[species]])) {
        species_tables[[length(species_tables) + 1]] <- div(
          h4(sprintf("BIN: %s", bin)),
          create_specimen_table(ns, organized[[species]][[bin]],
                                paste(species, bin, sep = "_"))
        )
      }
      tables[[length(tables) + 1]] <- div(
        h3(sprintf("Species: %s", species)),
        species_tables
      )
    }
  } else if (grade == "E") {
    for (bin in names(organized)) {
      tables[[length(tables) + 1]] <- div(
        h3(sprintf("Shared BIN: %s", bin)),
        create_specimen_table(ns, organized[[bin]], bin, color_by = "species")
      )
    }
  }

  tables
}

#' Prepare data for download
#' @param specimens Data frame of specimens
#' @param grade BAGS grade
#' @param selections List of selected specimens
#' @param flags List of specimen flags
#' @return Data frame prepared for download
#' @keywords internal
prepare_download_data <- function(specimens, grade, selections, flags) {
  if (is.null(specimens) || nrow(specimens) == 0) return(NULL)

  # Add selection and flag status
  specimens$selected <- specimens$processid %in% names(selections)
  specimens$flag <- sapply(specimens$processid, function(pid) {
    if (!is.null(flags[[pid]])) flags[[pid]]$flag else ""
  })

  # Order columns
  cols <- c(
    "processid", "species", "bin_uri", "quality_score",
    "specimen_rank", "criteria_met", "selected", "flag",
    setdiff(names(specimens), c(
      "processid", "species", "bin_uri", "quality_score",
      "specimen_rank", "criteria_met", "selected", "flag"
    ))
  )

  # Return ordered columns
  specimens[, cols]
}

#' Validate BAGS data
#' @param specimens Specimen data frame
#' @param grades BAGS grades data frame
#' @return List with validation results
#' @keywords internal
validate_bags_data <- function(specimens, grades) {
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(list(valid = FALSE, message = "No specimen data available"))
  }

  if (is.null(grades) || nrow(grades) == 0) {
    return(list(valid = FALSE, message = "No BAGS grades available"))
  }

  # Validate data consistency
  species_match <- all(grades$species %in% specimens$species)
  if (!species_match) {
    return(list(valid = FALSE, message = "Mismatch between specimens and BAGS grades"))
  }

  # Validate grade format
  valid_grades <- all(grades$bags_grade %in% c("A", "B", "C", "D", "E"))
  if (!valid_grades) {
    return(list(valid = FALSE, message = "Invalid BAGS grades detected"))
  }

  list(valid = TRUE, message = NULL)
}
