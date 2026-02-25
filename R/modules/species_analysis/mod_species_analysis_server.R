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

      # Clear local state when specimen data is removed (e.g. Clear Results)
      if (is.null(store$specimen_data)) {
        rv$checklist <- NULL
        rv$gap_analysis <- NULL
        rv$summary_stats <- NULL
        return()
      }

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
        found <- sum(rv$gap_analysis$status == "Found")
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

      # Build columnDefs: truncate bin_uris & countries to keep table narrow
      checklist_cols <- names(rv$checklist)
      truncate_targets <- which(checklist_cols %in% c("bin_uris", "countries")) - 1
      col_defs <- list()
      if (length(truncate_targets) > 0) {
        col_defs <- c(col_defs, list(list(
          targets = truncate_targets,
          width = "120px",
          render = JS("
            function(data, type, row) {
              if (type === 'display' && data && data.length > 30) {
                return '<span title=\"' + data.replace(/\"/g, '&quot;') + '\">' +
                       data.substr(0, 30) + '...</span>';
              }
              return data;
            }
          ")
        )))
      }

      # Add clickable link for species column
      species_target <- which(checklist_cols == "species") - 1
      if (length(species_target) > 0) {
        col_defs <- c(col_defs, list(list(
          targets = species_target,
          render = JS("
            function(data, type, row) {
              if (type === 'display' && data) {
                var s = data.toString();
                return '<a href=\"https://portal.boldsystems.org/result?query=' +
                       encodeURIComponent('\"' + s + '\"') + '[tax]\" target=\"_blank\" rel=\"noopener\">' +
                       s + '</a>';
              }
              return data;
            }
          ")
        )))
      }

      checklist_filename <- paste0("species_checklist_", format(Sys.time(), "%Y%m%d_%H%M"))

      DT::datatable(
        rv$checklist,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          autoWidth = FALSE,
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            list(extend = 'csv', title = checklist_filename, filename = checklist_filename),
            list(extend = 'excel', title = checklist_filename, filename = checklist_filename)
          ),
          order = list(list(1, 'desc')),
          columnDefs = col_defs
        ),
        rownames = FALSE,
        escape = FALSE,
        extensions = c('Buttons')
      ) %>%
        formatStyle(
          'bags_grade',
          backgroundColor = styleEqual(
            c("A", "B", "C", "D", "E"),
            c('#28a745', '#17a2b8', '#ffc107', '#6c757d', '#dc3545')
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
          DTOutput(ns("gap_analysis_table"))
        )
      }
    })

    # Gap analysis table
    output$gap_analysis_table <- renderDT({
      req(rv$gap_analysis)
      gap_filename <- paste0("gap_analysis_", format(Sys.time(), "%Y%m%d_%H%M"))

      DT::datatable(
        rv$gap_analysis,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            list(extend = 'csv', title = gap_filename, filename = gap_filename),
            list(extend = 'excel', title = gap_filename, filename = gap_filename)
          )
        ),
        rownames = FALSE,
        extensions = c('Buttons')
      ) %>%
        formatStyle(
          'status',
          backgroundColor = styleEqual(
            c("Found", "Missing"),
            c('#d4edda', '#f8d7da')
          ),
          fontWeight = 'bold'
        )
    })

    # Summary statistics table
    output$summary_stats_table <- renderDT({
      req(rv$summary_stats)
      stats_filename <- paste0("summary_statistics_", format(Sys.time(), "%Y%m%d_%H%M"))

      DT::datatable(
        rv$summary_stats,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            list(extend = 'csv', title = stats_filename, filename = stats_filename),
            list(extend = 'excel', title = stats_filename, filename = stats_filename)
          ),
          order = list(list(2, 'desc'))
        ),
        rownames = FALSE,
        extensions = c('Buttons')
      )
    })

    # Return reactive endpoints
    list(
      checklist = reactive({ rv$checklist }),
      gap_analysis = reactive({ rv$gap_analysis }),
      summary_stats = reactive({ rv$summary_stats })
    )
  })
}
