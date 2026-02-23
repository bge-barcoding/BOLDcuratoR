# R/modules/bin_analysis/mod_bin_analysis_server.R

#' Server Module for BIN Analysis
#' @param id The module ID
#' @param state State management instance
#' @param processor BIN processor instance
#' @param logger Logger instance
#' @export
mod_bin_analysis_server <- function(id, state, processor, logger) {
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
      store <- state$get_store()

      # Clear local state when specimen data is removed (e.g. Clear Results)
      if (is.null(store$specimen_data)) {
        analysis_results(NULL)
        processing_status(list(
          is_processing = FALSE,
          message = NULL,
          error = NULL
        ))
        return()
      }

      req(store$specimen_data)

      tryCatch({
        processing_status(list(
          is_processing = TRUE,
          message = "Starting BIN analysis...",
          error = NULL
        ))

        # Filter to specimens with BINs for analysis
        specimens_with_bins <- store$specimen_data[!is.na(store$specimen_data$bin_uri) &
                                                     store$specimen_data$bin_uri != "", ]

        logger$info("Starting BIN analysis", list(
          total_records = nrow(store$specimen_data),
          records_with_bins = nrow(specimens_with_bins)
        ))

        # Process BIN analysis
        results <- processor(specimens_with_bins) # removed processor$analyze_bins(specimens_with_bins)

        # Convert table objects to data frames
        if (!is.null(results)) {
          if ("summary" %in% names(results)) {
            results$summary <- as.data.frame(results$summary)
          }
          if ("content" %in% names(results)) {
            results$content <- as.data.frame(results$content)
          }
          if ("stats" %in% names(results) && !is.data.frame(results$stats)) {
            results$stats <- as.data.frame(as.list(results$stats))
          }
        }

        # Validate results
        if (!is.null(results) && validate_bin_results(results)) {
          # Update state and reactive values
          state$update_state("bin_analysis", results)
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
        } else {
          stop("Invalid BIN analysis results")
        }

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
          )
        ))
      })
    })

    # BIN validation internal function
    validate_bin_results <- function(results) {
      if (is.null(results)) return(FALSE)
      if (!("content" %in% names(results))) return(FALSE)
      if (!is.data.frame(results$content)) return(FALSE)
      TRUE
    }

    # Content table output
    output$bin_content_table <- renderDT({
      req(!is.null(analysis_results()))
      results <- analysis_results()

      # Ensure required columns exist
      required_cols <- c("bin_uri", "total_records", "unique_species", "species_list", "countries", "concordance")

      if(!is.null(results$content)) {
        content_data <- results$content[, intersect(names(results$content), required_cols)]
        format_bin_content_table(content_data)
      }
    })

    # Summary box outputs with enhanced metrics
    output$total_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      content <- analysis_results()$content
      if(is.null(content)) return(NULL)

      # Ensure we calculate complete metrics for display
      content$total_records <- sapply(content$species_list, function(x) {
        length(unlist(strsplit(x, "; ")))
      })
      content$unique_species <- sapply(content$species_list, function(x) {
        length(unique(unlist(strsplit(x, "; "))))
      })
      content$bin_coverage <- content$total_records / max(content$total_records)

      valueBox(
        nrow(content), "Total BINs",
        icon = icon("dna"),
        color = "blue"
      )
    })

    output$concordant_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      req(!is.null(analysis_results()$content))
      concordant <- if (!is.null(analysis_results()$content$concordance)) {
        sum(analysis_results()$content$concordance == "Concordant", na.rm = TRUE)
      } else {
        0
      }
      valueBox(
        concordant,
        "Concordant BINs",
        icon = icon("check"),
        color = "green"
      )
    })

    output$discordant_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      req(!is.null(analysis_results()$content))
      discordant <- sum(analysis_results()$content$concordance == "Discordant")
      valueBox(
        discordant,
        "Discordant BINs",
        icon = icon("exclamation-triangle"),
        color = if(discordant > 0) "red" else "yellow"
      )
    })

    output$shared_bins_box <- renderValueBox({
      req(!is.null(analysis_results()))
      req(!is.null(analysis_results()$content))
      shared <- sum(analysis_results()$content$unique_species > 1)
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
          "Statistics" = results$stats
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
