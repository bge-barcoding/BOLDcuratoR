# R/modules/bin_analysis/mod_bin_analysis_server.R

#' Server Module for BIN Analysis
#' @param id The module ID
#' @param state State management instance
#' @param logger Logger instance
#' @export
mod_bin_analysis_server <- function(id, state, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    analysis_results <- reactiveVal(NULL)
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
        logger$warn("Invalid state for BIN analysis", validation$messages)
        return(NULL)
      }

      specimen_data <- state$get_store()$specimen_data
      if (is.null(specimen_data)) return(NULL)

      tryCatch({
        processing_status(list(
          is_processing = TRUE,
          message = "Starting BIN analysis...",
          error = NULL
        ))

        logger$info("Starting BIN analysis", list(
          total_records = nrow(specimen_data),
          records_with_bins = sum(!is.na(specimen_data$bin_uri))
        ))

        # Process BIN analysis
        results <- analyze_bin_data(specimen_data)

        # Validate results
        validation <- validate_bin_analysis(results)
        if (!validation$valid) {
          stop(paste("Invalid BIN analysis results:",
                     paste(validation$messages, collapse = "; ")))
        }

        # Update state and reactive values
        state$update_state("bin_analysis", results, validate_bin_analysis)
        analysis_results(results)

        processing_status(list(
          is_processing = FALSE,
          message = "BIN analysis completed successfully",
          error = NULL
        ))

        logger$info("BIN analysis completed", list(
          total_bins = results$stats$total_bins,
          concordant_bins = results$stats$concordant_bins,
          discordant_bins = results$stats$discordant_bins
        ))

      }, error = function(e) {
        error_msg <- sprintf("BIN analysis error: %s", e$message)

        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = error_msg
        ))

        logger$error(error_msg)
        analysis_results(NULL)

        # Update error state
        state$update_state("error", list(
          has_error = TRUE,
          message = error_msg,
          details = list(
            source = "bin_analysis",
            timestamp = Sys.time()
          ),
          source = "bin_analysis"
        ))
      })
    })

    # Summary table output
    output$bin_summary_table <- renderDT({
      req(!is.null(analysis_results()))
      format_bin_summary_table(analysis_results()$summary)
    })

    # Content table output
    output$bin_content_table <- renderDT({
      req(!is.null(analysis_results()))
      format_bin_content_table(analysis_results()$content)
    })

    # Statistics table output
    output$bin_stats_table <- renderDT({
      req(!is.null(analysis_results()))
      format_bin_stats_table(analysis_results()$stats)
    })

    # Summary box outputs
    output$total_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      valueBox(
        analysis_results()$stats$total_bins,
        "Total BINs",
        icon = icon("dna"),
        color = "blue"
      )
    })

    output$concordant_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      valueBox(
        analysis_results()$stats$concordant_bins,
        "Concordant BINs",
        icon = icon("check"),
        color = "green"
      )
    })

    output$discordant_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      discordant <- analysis_results()$stats$discordant_bins
      valueBox(
        discordant,
        "Discordant BINs",
        icon = icon("exclamation-triangle"),
        color = if(discordant > 0) "red" else "yellow"
      )
    })

    output$shared_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      shared <- analysis_results()$stats$shared_bins
      valueBox(
        shared,
        "Shared BINs",
        icon = icon("share-alt"),
        color = if(shared > 0) "red" else "green"
      )
    })

    # Download handler
    output$download_analysis <- downloadHandler(
      filename = function() {
        paste0("bin_analysis_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
      },
      content = function(file) {
        req(!is.null(analysis_results()))
        results <- analysis_results()

        sheets <- list(
          "Summary" = results$summary,
          "Content" = results$content,
          "Statistics" = as.data.frame(t(unlist(results$stats)))
        )

        writexl::write_xlsx(sheets, file)

        # Log export
        user_info <- state$get_store()$user_info
        logger$log_action(
          user_email = user_info$email,
          user_name = user_info$name,
          session_id = session$token,
          action_type = "bin_analysis_exported",
          metadata = list(
            format = "xlsx",
            sheets = names(sheets),
            total_bins = results$stats$total_bins
          )
        )
      }
    )

    # Return reactive values
    list(
      analysis_results = analysis_results,
      processing_status = processing_status
    )
  })
}
