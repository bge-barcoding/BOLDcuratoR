# R/modules/export_history/mod_export_history_server.R

#' Server Module for Export History
#' @param id The module ID
#' @param state State management instance
#' @export
mod_export_history_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Get history data
    history_data <- reactive({
      req(input$date_range)

      export_type <- if (input$export_type != "All") input$export_type else NULL
      format <- if (input$format != "All") input$format else NULL

      state$logger$get_export_history(
        from_date = format(input$date_range[1], "%Y-%m-%d"),
        to_date = format(input$date_range[2], "%Y-%m-%d"),
        export_type = export_type,
        format = format
      )
    })

    # Get statistics
    stats_data <- reactive({
      req(input$date_range)

      state$logger$get_export_stats(
        from_date = format(input$date_range[1], "%Y-%m-%d"),
        to_date = format(input$date_range[2], "%Y-%m-%d")
      )
    })

    # Render value boxes
    output$total_exports_box <- renderValueBox({
      req(stats_data())
      stats <- stats_data()
      valueBox(
        stats$total_exports,
        "Total Exports",
        icon = icon("file-export"),
        color = "blue"
      )
    })

    output$success_rate_box <- renderValueBox({
      req(stats_data())
      stats <- stats_data()
      success_rate <- round(stats$successful_exports / stats$total_exports * 100, 1)
      valueBox(
        paste0(success_rate, "%"),
        "Success Rate",
        icon = icon("check-circle"),
        color = if (success_rate >= 90) "green" else if (success_rate >= 75) "yellow" else "red"
      )
    })

    output$records_exported_box <- renderValueBox({
      req(stats_data())
      stats <- stats_data()
      valueBox(
        format(stats$total_records_exported, big.mark = ","),
        "Records Exported",
        icon = icon("database"),
        color = "purple"
      )
    })

    output$avg_size_box <- renderValueBox({
      req(stats_data())
      stats <- stats_data()
      avg_size_mb <- round(stats$total_size_exported / stats$total_exports / 1024 / 1024, 2)
      valueBox(
        paste0(avg_size_mb, " MB"),
        "Average File Size",
        icon = icon("file"),
        color = "orange"
      )
    })

    # Render history table
    output$history_table <- renderDT({
      req(history_data())

      data <- history_data()
      data$file_size <- paste0(round(data$file_size / 1024 / 1024, 2), " MB")
      data$timestamp <- format(as.POSIXct(data$timestamp), "%Y-%m-%d %H:%M:%S")

      datatable(
        data,
        options = list(
          pageLength = 25,
          order = list(list(1, 'desc')),
          scrollX = TRUE
        ),
        selection = 'none',
        rownames = FALSE
      ) %>%
        formatStyle(
          'success',
          backgroundColor = styleEqual(
            c(TRUE, FALSE),
            c('#d4edda', '#f8d7da')
          )
        )
    })

    # Download handler
    output$download_history <- downloadHandler(
      filename = function() {
        paste0("export_history_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        write.csv(history_data(), file, row.names = FALSE)
      }
    )
  })
}
