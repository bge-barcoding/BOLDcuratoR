# R/modules/species_analysis/mod_species_analysis_server.R

#' Server Module for Species Analysis
#' @param id The module ID
#' @param state State management instance
#' @param logger Logger instance
#' @export
mod_species_analysis_server <- function(id, state, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      checklist = NULL,
      gap_analysis = NULL,
      summary_stats = NULL
    )

    # Main analysis observer
    observe({
      store <- state$get_store()
      req(store$specimen_data)

      tryCatch({
        data <- store$specimen_data
        grades <- store$bags_grades

        logger$info("Building species checklist", list(
          specimens = nrow(data),
          has_grades = !is.null(grades)
        ))

        rv$checklist <- build_species_checklist(data, grades)
        rv$summary_stats <- build_summary_stats(data)

        # Gap analysis if input taxa are available
        search_taxa <- store$search_taxa
        if (!is.null(search_taxa) && length(search_taxa) > 0) {
          logger$info("Performing gap analysis", list(
            input_taxa_count = length(search_taxa)
          ))
          rv$gap_analysis <- perform_gap_analysis(search_taxa, data)
        } else {
          rv$gap_analysis <- NULL
        }

        logger$info("Species analysis complete", list(
          species_count = nrow(rv$checklist),
          gap_analysis = !is.null(rv$gap_analysis)
        ))

      }, error = function(e) {
        logger$error("Species analysis error", list(error = e$message))
      })
    })

    # Value boxes
    output$total_species_box <- renderValueBox({
      count <- if (!is.null(rv$checklist)) nrow(rv$checklist) else 0
      valueBox(count, "Total Species", icon = icon("list"), color = "blue")
    })

    output$species_with_bins_box <- renderValueBox({
      count <- if (!is.null(rv$checklist)) {
        sum(rv$checklist$bin_count > 0)
      } else 0
      valueBox(count, "Species with BINs", icon = icon("dna"), color = "green")
    })

    output$species_without_bins_box <- renderValueBox({
      count <- if (!is.null(rv$checklist)) {
        sum(rv$checklist$bin_count == 0)
      } else 0
      valueBox(count, "Species without BINs", icon = icon("exclamation-triangle"),
               color = if (count > 0) "yellow" else "green")
    })

    output$input_taxa_found_box <- renderValueBox({
      store <- state$get_store()
      if (!is.null(rv$gap_analysis)) {
        found <- sum(rv$gap_analysis$status %in% c("Found", "Found (higher taxon)"))
        total <- nrow(rv$gap_analysis)
        valueBox(
          sprintf("%d / %d", found, total),
          "Input Taxa Found",
          icon = icon("check-circle"),
          color = if (found == total) "green" else "yellow"
        )
      } else {
        valueBox("N/A", "Input Taxa Found", icon = icon("minus-circle"), color = "aqua")
      }
    })

    # Species checklist table
    output$species_checklist_table <- renderDT({
      req(rv$checklist)
      DT::datatable(
        rv$checklist,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          order = list(list(1, 'desc'))
        ),
        rownames = FALSE,
        extensions = c('Buttons')
      ) %>%
        formatStyle(
          'bags_grade',
          backgroundColor = styleEqual(
            c("A", "B", "C", "D", "E"),
            c('#28a745', '#17a2b8', '#ffc107', '#dc3545', '#6c757d')
          ),
          color = styleEqual(
            c("A", "B", "C", "D", "E"),
            rep('white', 5)
          ),
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'mean_quality_score',
          background = styleColorBar(c(0, 14), '#28a745'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # Gap analysis panel (conditional)
    output$gap_analysis_panel <- renderUI({
      if (is.null(rv$gap_analysis)) {
        div(
          class = "alert alert-info",
          icon("info-circle"),
          "Gap analysis requires a taxa input list. Enter taxa in the Data Input tab before searching."
        )
      } else {
        tagList(
          div(
            style = "margin-bottom: 10px;",
            downloadButton(ns("download_gap_analysis"),
                           "Download Gap Analysis",
                           class = "btn-success")
          ),
          DTOutput(ns("gap_analysis_table"))
        )
      }
    })

    # Gap analysis table
    output$gap_analysis_table <- renderDT({
      req(rv$gap_analysis)
      DT::datatable(
        rv$gap_analysis,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        extensions = c('Buttons')
      ) %>%
        formatStyle(
          'status',
          backgroundColor = styleEqual(
            c("Found", "Found (higher taxon)", "Partial (genus match)", "Missing"),
            c('#d4edda', '#d1ecf1', '#fff3cd', '#f8d7da')
          ),
          fontWeight = 'bold'
        )
    })

    # Summary statistics table
    output$summary_stats_table <- renderDT({
      req(rv$summary_stats)
      DT::datatable(
        rv$summary_stats,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          order = list(list(2, 'desc'))
        ),
        rownames = FALSE,
        extensions = c('Buttons')
      )
    })

    # Download handlers
    output$download_checklist <- downloadHandler(
      filename = function() {
        paste0("species_checklist_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
      },
      content = function(file) {
        req(rv$checklist)
        write.table(rv$checklist, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded species checklist", list(count = nrow(rv$checklist)))
      }
    )

    output$download_gap_analysis <- downloadHandler(
      filename = function() {
        paste0("gap_analysis_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv")
      },
      content = function(file) {
        req(rv$gap_analysis)
        write.table(rv$gap_analysis, file, sep = "\t", row.names = FALSE, quote = FALSE)
        logger$info("Downloaded gap analysis", list(count = nrow(rv$gap_analysis)))
      }
    )

    # Return reactive endpoints
    list(
      checklist = reactive({ rv$checklist }),
      gap_analysis = reactive({ rv$gap_analysis }),
      summary_stats = reactive({ rv$summary_stats })
    )
  })
}
