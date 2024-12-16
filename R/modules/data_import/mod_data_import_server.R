# R/modules/data_import/mod_data_import_server.R

#' Server Module for Data Import
#' @param id The module ID
#' @param state State management instance
#' @param logger Logger instance
#' @export
mod_data_import_server <- function(id, state, logger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive Values
    search_params <- reactiveVal(NULL)
    validation_status <- reactiveVal(list(valid = TRUE, messages = character()))

    # Track search progress
    progress <- reactive({
      store <- state$get_store()
      store$processing$progress
    })

    # Reactive for selected countries
    selected_countries <- reactive({
      continent_selected <- unlist(CONTINENT_COUNTRIES[input$continents])
      additional_countries <- NULL

      if (!is.null(input$countries) && nchar(input$countries) > 0) {
        additional_countries <- unlist(strsplit(input$countries, "\n"))
        additional_countries <- trimws(additional_countries[nchar(additional_countries) > 0])
      }

      unique(c(continent_selected, additional_countries))
    })

    # Validate search parameters
    observe({
      total_length <- sum(
        nchar(input$taxa_input),
        nchar(input$dataset_codes),
        nchar(input$project_codes),
        sum(nchar(selected_countries()))
      )

      if (total_length > 1800) {
        logger$warn("Query length exceeds recommended limit")
        shinyjs::show(id = "url_warning")
      } else {
        shinyjs::hide(id = "url_warning")
      }

      search_params(prepare_search_params(input, selected_countries()))
    })

    # Form submission handler
    observeEvent(input$submit, {
      # Validate API key from user info
      user_info <- state$get_store()$user_info
      if (is.null(user_info) || is.null(user_info$bold_api_key)) {
        logger$error("Attempted search without API key")
        showNotification("Please set your BOLD API key first", type = "error")
        return()
      }

      # Check input validation
      validation <- validation_status()
      if (!validation$valid) {
        logger$error("Input validation failed", validation$messages)
        showNotification(paste(validation$messages, collapse = "\n"), type = "error")
        return()
      }

      # Initialize progress
      state$update_state("processing", list(
        active = TRUE,
        progress = 0,
        message = "Starting data import..."
      ))

      # Log search attempt
      logger$info("Starting BOLD search", list(
        taxa = input$taxa_input,
        datasets = input$dataset_codes,
        projects = input$project_codes,
        countries = length(selected_countries())
      ))

      withProgress(message = "Searching BOLD database", {
        tryCatch({
          params <- search_params()

          # Reset error state
          state$update_state("error", list(
            has_error = FALSE,
            message = NULL,
            details = NULL,
            timestamp = NULL
          ))

          combined_specimens <- NULL
          total_steps <- sum(
            !is.null(params$dataset_codes),
            !is.null(params$project_codes),
            !is.null(params$taxonomy)
          )
          current_step <- 0

          # Process dataset codes
          if (!is.null(params$dataset_codes) && length(params$dataset_codes) > 0) {
            logger$info("Processing datasets", params$dataset_codes)
            current_step <- current_step + 1
            state$update_state("processing", list(
              active = TRUE,
              progress = (current_step / total_steps) * 100,
              message = "Processing datasets..."
            ))

            dataset_specimens <- fetch_specimens(params$dataset_codes, "datasets")
            if (!is.null(dataset_specimens)) {
              combined_specimens <- dataset_specimens
              logger$info("Dataset fetch successful",
                          list(records = nrow(dataset_specimens)))
            }
          }

          # Process project codes
          if (!is.null(params$project_codes) && length(params$project_codes) > 0) {
            logger$info("Processing projects", params$project_codes)
            current_step <- current_step + 1
            state$update_state("processing", list(
              active = TRUE,
              progress = (current_step / total_steps) * 100,
              message = "Processing projects..."
            ))

            project_specimens <- fetch_specimens(params$project_codes, "projects")
            if (!is.null(project_specimens)) {
              logger$info("Project fetch successful",
                          list(records = nrow(project_specimens)))
              combined_specimens <- merge_specimens(combined_specimens, project_specimens)
            }
          }

          # Process taxonomy search
          if (!is.null(params$taxonomy)) {
            logger$info("Processing taxonomy search", params$taxonomy)
            current_step <- current_step + 1
            state$update_state("processing", list(
              active = TRUE,
              progress = (current_step / total_steps) * 100,
              message = "Searching taxonomy..."
            ))

            taxonomy_specimens <- process_taxonomy_search(
              params$taxonomy,
              params$geography
            )

            if (!is.null(taxonomy_specimens)) {
              combined_specimens <- merge_specimens(combined_specimens, taxonomy_specimens)
            }
          }

          # Process final results
          if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
            state$update_state("processing", list(
              active = TRUE,
              progress = 90,
              message = "Processing results..."
            ))

            # Validate specimen data
            validation <- validate_specimen_data(combined_specimens)
            if (!validation$valid) {
              logger$error("Specimen validation failed", validation$messages)
              stop(paste("Invalid specimen data:",
                         paste(validation$messages, collapse = "; ")))
            }

            # Process and update state
            processed_data <- process_specimen_data(combined_specimens)
            state$update_state("specimen_data", processed_data, validate_specimen_data)

            # Clear previous analyses
            state$update_state("bin_analysis", NULL)
            state$update_state("bags_grades", NULL)
            state$update_state("selected_specimens", list())

            # Update completion status
            state$update_state("processing", list(
              active = FALSE,
              progress = 100,
              message = sprintf("Retrieved %d specimens successfully",
                                nrow(processed_data))
            ))

            logger$info("Search completed successfully", list(
              total_specimens = nrow(processed_data),
              unique_species = length(unique(processed_data$species)),
              unique_bins = length(unique(processed_data$bin_uri))
            ))

            showNotification(sprintf("Retrieved %d specimens", nrow(processed_data)),
                             type = "message")
          } else {
            logger$warn("No specimens found matching criteria")
            stop("No specimens found matching search criteria")
          }

        }, error = function(e) {
          # Update error state
          logger$error("Search failed", e$message)
          state$update_state("error", list(
            has_error = TRUE,
            message = paste("Error retrieving data:", e$message),
            details = list(
              search_params = params,
              timestamp = Sys.time()
            ),
            source = "data_import"
          ))

          # Reset processing state
          state$update_state("processing", list(
            active = FALSE,
            progress = 0,
            message = NULL
          ))

          showNotification(paste("Error retrieving data:", e$message),
                           type = "error")
        })
      })
    })

    # Helper function to fetch specimens using BOLDconnectR
    fetch_specimens <- function(codes, type = c("datasets", "projects")) {
      type <- match.arg(type)
      tryCatch({
        # Get API key from state
        user_info <- state$get_store()$user_info
        if (is.null(user_info) || is.null(user_info$bold_api_key)) {
          stop("No API key available")
        }

        # Set API key for request
        BOLDconnectR::bold.apikey(user_info$bold_api_key)

        # Make request based on type
        if (type == "datasets") {
          specimens <- BOLDconnectR::bold.fetch(
            get_by = "dataset_codes",
            identifiers = codes
          )
        } else {
          specimens <- BOLDconnectR::bold.fetch(
            get_by = "project_codes",
            identifiers = codes
          )
        }

        specimens
      }, error = function(e) {
        logger$error(sprintf("Error fetching %s: %s", type, e$message))
        NULL
      })
    }

    # Helper function to process taxonomy search
    process_taxonomy_search <- function(taxonomy, geography) {
      tryCatch({
        # Get API key from state
        user_info <- state$get_store()$user_info
        if (is.null(user_info) || is.null(user_info$bold_api_key)) {
          stop("No API key available")
        }

        # Set API key for request
        BOLDconnectR::bold.apikey(user_info$bold_api_key)

        # Search for specimens
        search_results <- BOLDconnectR::bold.public.search(taxonomy = taxonomy, geography = geography)
        if (!is.null(search_results) && nrow(search_results) > 0) {
          specimens <- BOLDconnectR::bold.fetch(
            get_by = "processid",
            identifiers = search_results$processid
          )
          return(specimens)
        }
        NULL
      }, error = function(e) {
        logger$error("Taxonomy search failed", e$message)
        NULL
      })
    }

    # Helper function to merge specimen results
    merge_specimens <- function(existing, new_specimens) {
      if (is.null(existing)) return(new_specimens)
      if (is.null(new_specimens)) return(existing)

      rbind(
        existing,
        new_specimens[!new_specimens$processid %in% existing$processid, ]
      )
    }

    # Clear input handler
    observeEvent(input$clear_input, {
      updateTextAreaInput(session, "taxa_input", value = "")
      updateTextAreaInput(session, "dataset_codes", value = "")
      updateTextAreaInput(session, "project_codes", value = "")
      updateTextAreaInput(session, "countries", value = "")
      updateCheckboxGroupInput(session, "continents",
                               choices = names(CONTINENT_COUNTRIES),
                               selected = character(0))

      # Log action
      logger$info("Input fields cleared")
      showNotification("Input fields cleared", type = "message")
    })

    # Clear results handler
    observeEvent(input$clear_results, {
      logger$info("Clearing search results")
      state$update_state("specimen_data", NULL)
      state$update_state("bin_analysis", NULL)
      state$update_state("bags_grades", NULL)
      state$update_state("selected_specimens", list())
      state$update_state("processing", list(
        active = FALSE,
        progress = 0,
        message = NULL
      ))

      showNotification("Results cleared", type = "message")
    })

    # Value box outputs
    output$total_records_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) nrow(specimen_data) else 0
      valueBox(
        count,
        "Total Records",
        icon = icon("table"),
        color = "blue"
      )
    })

    output$unique_taxa_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) {
        length(unique(specimen_data$species[!is.na(specimen_data$species)]))
      } else 0
      valueBox(
        count,
        "Unique Taxa",
        icon = icon("sitemap"),
        color = "green"
      )
    })

    output$unique_bins_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) {
        length(unique(specimen_data$bin_uri[!is.na(specimen_data$bin_uri)]))
      } else 0
      valueBox(
        count,
        "Unique BINs",
        icon = icon("dna"),
        color = "purple"
      )
    })

    output$countries_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) {
        length(unique(specimen_data$country.ocean[!is.na(specimen_data$country.ocean)]))
      } else 0
      valueBox(
        count,
        "Countries",
        icon = icon("globe"),
        color = "orange"
      )
    })

    # Results table output
    output$results_table <- renderDT({
      req(state$get_store()$specimen_data)
      format_specimen_table(state$get_store()$specimen_data)
    })

    # Download handler
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("bold_search_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        logger$info("Downloading search results", list(format = "csv"))
        write.csv(state$get_store()$specimen_data, file, row.names = FALSE)
      }
    )
  })
}
