# R/utils/table_utils.R

#' Format specimen table with standardized styling
#' @param data Data frame of specimen data
#' @param buttons Vector of button types to include
#' @param page_length Number of rows per page
#' @param selection Selection type
#' @export
format_specimen_table <- function(data, buttons = c('copy', 'csv', 'excel'),
                                  page_length = 25, selection = 'none') {
  # Get available columns from data
  available_cols <- intersect(names(data), c(
    "specimen_rank", "quality_score", "criteria_met",  # Custom columns
    "processid", "bin_uri", "species", "genus", "family",  # Core taxonomy
    "country.ocean", "coord", "collection_date_start",  # Geography/collection
    "identified_by", "voucher_type", "institution"  # Attribution
  ))

  dt <- datatable(
    data[, available_cols, drop = FALSE],
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = buttons
    ),
    selection = selection,
    rownames = FALSE
  )

  # Apply styling if columns exist
  if ("specimen_rank" %in% available_cols) {
    dt <- dt %>% formatStyle(
      'specimen_rank',
      backgroundColor = styleInterval(
        c(1.5, 2.5, 3.5, 4.5, 5.5),
        c('#28a745', '#28a745', '#17a2b8', '#17a2b8', '#ffc107', '#ffc107')
      )
    )
  }

  if ("quality_score" %in% available_cols) {
    dt <- dt %>% formatStyle(
      'quality_score',
      background = styleColorBar(c(0, 14), '#28a745'),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
  }

  return(dt)
}

#' Format BIN summary table
#' @param summary_data Data frame of BIN summary data
#' @export
format_bin_summary_table <- function(summary_data) {
  datatable(
    summary_data,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'concordant_bins',
      backgroundColor = styleInterval(c(0), c('#f8d7da', '#d4edda'))
    ) %>%
    formatPercentage('bin_coverage', digits = 1)
}

#' Format BIN content table
#' @param content_data Data frame of BIN content data
#' @export
format_bin_content_table <- function(content_data) {
  datatable(
    content_data,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'concordance',
      backgroundColor = styleEqual(
        c("Concordant", "Discordant"),
        c('#d4edda', '#f8d7da')
      )
    )
}

#' Format BIN statistics table
#' @param stats_data Data frame of BIN statistics
#' @export
format_bin_stats_table <- function(stats_data) {
  # Convert stats list to data frame if needed
  if (is.list(stats_data) && !is.data.frame(stats_data)) {
    stats_df <- as.data.frame(t(unlist(stats_data)))
  } else {
    stats_df <- stats_data
  }

  # Format percentage columns
  percent_cols <- c("bin_coverage")
  for (col in intersect(percent_cols, names(stats_df))) {
    stats_df[[col]] <- paste0(round(stats_df[[col]], 1), "%")
  }

  datatable(
    stats_df,
    options = list(
      dom = 't',
      ordering = FALSE
    ),
    rownames = FALSE
  ) %>%
    formatRound(
      columns = c("avg_species_per_bin"),
      digits = 2
    )
}

#' Format history table
#' @param history_data Data frame of selection history
#' @export
format_history_table <- function(history_data) {
  datatable(
    history_data,
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      dom = 't',
      ordering = FALSE
    ),
    selection = 'none',
    rownames = FALSE
  ) %>%
    formatStyle(
      'quality_score',
      background = styleColorBar(c(0, 14), '#28a745')
    ) %>%
    formatStyle(
      'specimen_rank',
      backgroundColor = styleInterval(
        c(1.5, 2.5, 3.5, 4.5, 5.5),
        c('#28a745', '#28a745', '#17a2b8', '#17a2b8', '#ffc107', '#ffc107')
      )
    )
}

#' Format BAGS grade table
#' @param grade_data Data frame of BAGS grades
#' @export
format_bags_grade_table <- function(grade_data) {
  datatable(
    grade_data,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'bags_grade',
      backgroundColor = styleEqual(
        c("A", "B", "C", "D", "E"),
        c('#28a745', '#17a2b8', '#ffc107', '#dc3545', '#6c757d')
      ),
      color = 'white'
    ) %>%
    formatStyle(
      'bin_coverage',
      background = styleColorBar(c(0, 100), '#28a745')
    )
}

#' Format export history table
#' @param history_data Data frame of export history
#' @export
format_export_history_table <- function(history_data) {
  datatable(
    history_data,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
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
    ) %>%
    formatDate('timestamp', method = "toLocaleString") %>%
    formatBytes('file_size')
}

#' Format haplotype summary table
#' @param summary_data Data frame of haplotype summary data
#' @export
format_haplotype_summary_table <- function(summary_data) {
  datatable(
    summary_data,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'haplotype_diversity',
      background = styleColorBar(c(0, 1), '#28a745')
    ) %>%
    formatRound('haplotype_diversity', digits = 3) %>%
    formatStyle(
      'unique_haplotypes',
      background = styleColorBar(c(0, max(summary_data$unique_haplotypes)), '#17a2b8')
    )
}
