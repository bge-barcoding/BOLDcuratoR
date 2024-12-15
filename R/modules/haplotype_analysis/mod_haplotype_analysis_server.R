# R/modules/haplotype_analysis/mod_haplotype_analysis_server.R

#' Server Module for Haplotype Analysis
#' @param id Module ID
#' @param state State management instance
#' @param logger Logger instance
#' @export
mod_haplotype_analysis_server <- function(id, state, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize components
    manager <- HaplotypeManager$new(state, logger)

    # Reactive Values
    analysis_results <- reactiveVal(NULL)
    selected_data <- reactiveVal(NULL)
    processing_status <- reactiveVal(list(
      is_processing = FALSE,
      message = NULL,
      error = NULL
    ))

    # Process new specimen data
    observe({
      # Validate state and required data
      validation <- state$validate_state(c("specimen_data"))
      if (!validation$valid) {
        logger$warn("Invalid state for haplotype analysis", validation$messages)
        return(NULL)
      }

      specimens <- state$get_store()$specimen_data
      if (is.null(specimens)) return(NULL)

      # Process specimens
      results <- manager$analyze_specimens(specimens)
      if (!is.null(results)) {
        analysis_results(results)
        updateSelectInput(session, "selected_species",
                          choices = sort(names(results)))
      }
    })

    # Handle species selection
    observeEvent(input$selected_species, {
      req(input$selected_species, analysis_results())

      species_data <- analysis_results()[[input$selected_species]]
      if (!is.null(species_data)) {
        selected_data(species_data)
      }
    })

    # Status message output
    output$status_container <- renderUI({
      status <- processing_status()
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

    # Summary box outputs
    output$total_species_box <- renderValueBox({
      req(analysis_results())
      valueBox(
        length(analysis_results()),
        "Total Species",
        icon = icon("dna"),
        color = "blue"
      )
    })

    output$total_haplotypes_box <- renderValueBox({
      req(analysis_results())
      total <- sum(sapply(analysis_results(), function(x) x$n_haplotypes))
      valueBox(
        total,
        "Total Haplotypes",
        icon = icon("project-diagram"),
        color = "green"
      )
    })

    output$avg_diversity_box <- renderValueBox({
      req(analysis_results())
      avg <- mean(sapply(analysis_results(), function(x) x$diversity))
      valueBox(
        sprintf("%.3f", avg),
        "Average Diversity",
        icon = icon("chart-bar"),
        color = "purple"
      )
    })

    output$unique_sequences_box <- renderValueBox({
      req(analysis_results())
      total <- sum(sapply(analysis_results(), function(x) x$total_specimens))
      valueBox(
        total,
        "Unique Sequences",
        icon = icon("fingerprint"),
        color = "orange"
      )
    })

    # Table outputs
    output$species_details <- renderDT({
      req(selected_data())
      format_species_details(selected_data())
    })

    output$haplotype_table <- renderDT({
      req(selected_data())
      format_haplotype_table(selected_data())
    })

    output$geographic_table <- renderDT({
      req(selected_data())
      format_geographic_table(selected_data())
    })

    output$diversity_stats <- renderDT({
      req(analysis_results())
      format_diversity_stats(analysis_results())
    })

    output$sequence_stats <- renderDT({
      req(analysis_results())
      format_sequence_stats(analysis_results())
    })

    # Plot outputs
    output$haplotype_network <- renderPlot({
      req(selected_data())
      plot_haplotype_network(selected_data())
    })

    output$geographic_dist <- renderPlot({
      req(selected_data())
      plot_geographic_distribution(selected_data())
    })

    # Download handlers
    output$download_summary <- downloadHandler(
      filename = function() {
        paste0("haplotype_analysis_",
               format(Sys.time(), "%Y%m%d_%H%M"),
               ".xlsx")
      },
      content = function(file) {
        req(analysis_results())

        tryCatch({
          processing_status(list(
            is_processing = TRUE,
            message = "Preparing summary export...",
            error = NULL
          ))

          sheets <- create_summary_sheets(analysis_results())
          writexl::write_xlsx(sheets, file)

          # Log export
          user_info <- state$get_store()$user_info
          logger$log_action(
            user_email = user_info$email,
            user_name = user_info$name,
            session_id = session$token,
            action_type = "haplotype_analysis_exported",
            metadata = list(
              format = "xlsx",
              sheets = names(sheets)
            )
          )

          processing_status(list(
            is_processing = FALSE,
            message = NULL,
            error = NULL
          ))
        }, error = function(e) {
          processing_status(list(
            is_processing = FALSE,
            message = NULL,
            error = sprintf("Export failed: %s", e$message)
          ))
        })
      }
    )

    output$download_sequences <- downloadHandler(
      filename = function() {
        paste0("haplotype_sequences_",
               format(Sys.time(), "%Y%m%d_%H%M"),
               ".fasta")
      },
      content = function(file) {
        req(selected_data())

        tryCatch({
          processing_status(list(
            is_processing = TRUE,
            message = "Preparing sequence export...",
            error = NULL
          ))

          write_fasta_file(selected_data(), file)

          processing_status(list(
            is_processing = FALSE,
            message = NULL,
            error = NULL
          ))
        }, error = function(e) {
          processing_status(list(
            is_processing = FALSE,
            message = NULL,
            error = sprintf("Export failed: %s", e$message)
          ))
        })
      }
    )

    output$download_networks <- downloadHandler(
      filename = function() {
        paste0("haplotype_networks_",
               format(Sys.time(), "%Y%m%d_%H%M"),
               ".pdf")
      },
      content = function(file) {
        req(analysis_results())

        tryCatch({
          processing_status(list(
            is_processing = TRUE,
            message = "Preparing network plots...",
            error = NULL
          ))

          generate_network_pdf(analysis_results(), file)

          processing_status(list(
            is_processing = FALSE,
            message = NULL,
            error = NULL
          ))
        }, error = function(e) {
          processing_status(list(
            is_processing = FALSE,
            message = NULL,
            error = sprintf("Export failed: %s", e$message)
          ))
        })
      }
    )

    # Return reactive values
    list(
      analysis_results = analysis_results,
      selected_data = selected_data,
      processing_status = processing_status
    )
  })
}
