# R/modules/haplotype_analysis/mod_haplotype_analysis_utils.R

#' Format species details table
#' @param data Species analysis data
#' @return Formatted datatable
#' @export
format_species_details <- function(data) {
  if (is.null(data)) return(NULL)

  details_df <- data.frame(
    total_specimens = data$total_specimens,
    unique_haplotypes = data$n_haplotypes,
    diversity = data$diversity,
    countries = data$n_countries,
    alignment_coverage = data$alignment_coverage,
    stringsAsFactors = FALSE
  )

  # Include species count when showing BIN-level results
  if (!is.null(data$n_species)) {
    details_df$species_in_group <- data$n_species
    if (!is.null(data$species_list)) {
      details_df$species <- paste(sort(data$species_list), collapse = "; ")
    }
  }

  datatable(
    details_df,
    options = list(
      dom = 't',
      ordering = FALSE
    ),
    rownames = FALSE
  ) %>%
    formatRound(c('diversity', 'alignment_coverage'), 3) %>%
    formatStyle(
      'diversity',
      background = styleColorBar(c(0,1), '#28a745')
    )
}

#' Format haplotype table
#' @param data Haplotype analysis data
#' @return Formatted datatable
#' @export
format_haplotype_table <- function(data) {
  if (is.null(data) || is.null(data$haplotype_assignments)) return(NULL)

  haplotype_df <- data.frame(
    haplotype = as.factor(data$haplotype_assignments),
    specimens = seq_along(data$haplotype_assignments),
    distance = rowSums(data$distance_matrix),
    stringsAsFactors = FALSE
  )

  datatable(
    haplotype_df,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'distance',
      background = styleColorBar(c(0, max(haplotype_df$distance)), '#17a2b8')
    )
}

#' Format geographic distribution table
#' @param data Analysis data with geographic info
#' @return Formatted datatable
#' @export
format_geographic_table <- function(data) {
  if (is.null(data)) return(NULL)

  # Group haplotypes by country
  unique_haps <- unique(data$haplotype_assignments)
  countries <- unique(data$country.ocean)

  dist_matrix <- matrix(0, nrow = length(countries), ncol = length(unique_haps))
  rownames(dist_matrix) <- countries
  colnames(dist_matrix) <- paste("Haplotype", unique_haps)

  for (i in seq_along(countries)) {
    for (j in seq_along(unique_haps)) {
      dist_matrix[i,j] <- sum(data$haplotype_assignments == unique_haps[j] &
                                data$country.ocean == countries[i])
    }
  }

  dist_df <- as.data.frame(dist_matrix)
  dist_df$Country <- rownames(dist_matrix)
  dist_df <- dist_df[, c("Country", paste("Haplotype", unique_haps))]

  datatable(
    dist_df,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      paste("Haplotype", unique_haps),
      background = styleColorBar(c(0, max(dist_matrix)), '#28a745')
    )
}

#' Format diversity statistics table
#' @param results Analysis results list
#' @return Formatted datatable
#' @export
format_diversity_stats <- function(results) {
  if (is.null(results)) return(NULL)

  stats_df <- do.call(rbind, lapply(names(results), function(group_name) {
    result <- results[[group_name]]
    row <- data.frame(
      group = group_name,
      specimens = result$total_specimens,
      haplotypes = result$n_haplotypes,
      diversity = result$diversity,
      countries = result$n_countries,
      stringsAsFactors = FALSE
    )
    # Add species count for BIN-level results
    if (!is.null(result$n_species)) {
      row$species_in_bin <- result$n_species
    }
    row
  }))

  datatable(
    stats_df,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'diversity',
      background = styleColorBar(c(0,1), '#28a745')
    ) %>%
    formatRound('diversity', 3)
}

#' Format sequence statistics table
#' @param results Analysis results list
#' @return Formatted datatable
#' @export
format_sequence_stats <- function(results) {
  if (is.null(results)) return(NULL)

  stats_df <- do.call(rbind, lapply(names(results), function(group_name) {
    result <- results[[group_name]]
    upper <- result$distance_matrix[upper.tri(result$distance_matrix)]
    data.frame(
      group = group_name,
      coverage = result$alignment_coverage * 100,
      avg_distance = if (length(upper) > 0) mean(upper) else 0,
      max_distance = if (length(upper) > 0) max(upper) else 0,
      stringsAsFactors = FALSE
    )
  }))

  datatable(
    stats_df,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    rownames = FALSE
  ) %>%
    formatRound(c('coverage', 'avg_distance', 'max_distance'), 2) %>%
    formatStyle(
      'coverage',
      background = styleColorBar(c(0,100), '#28a745')
    )
}

#' Plot haplotype network
#' @param data Analysis data
#' @return ggplot object
#' @export
plot_haplotype_network <- function(data) {
  if (is.null(data) || is.null(data$distance_matrix)) return(NULL)

  # Create network layout using igraph
  g <- graph_from_adjacency_matrix(
    data$distance_matrix,
    mode = "undirected",
    weighted = TRUE
  )

  layout <- layout_with_fr(g)

  # Create plot with ggplot2
  plot_df <- data.frame(
    x = layout[,1],
    y = layout[,2],
    haplotype = as.factor(data$haplotype_assignments),
    size = table(data$haplotype_assignments)[data$haplotype_assignments]
  )

  ggplot(plot_df, aes(x = x, y = y, color = haplotype, size = size)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(5, 15)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(title = "Haplotype Network",
         color = "Haplotype",
         size = "Frequency")
}

#' Plot geographic distribution
#' @param data Analysis data
#' @return ggplot object
#' @export
plot_geographic_distribution <- function(data) {
  if (is.null(data)) return(NULL)

  # Create distribution data
  dist_df <- data.frame(
    country = data$country.ocean,
    haplotype = as.factor(data$haplotype_assignments)
  )

  # Count frequencies
  freq_df <- as.data.frame(table(dist_df))
  freq_df$Freq <- as.numeric(freq_df$Freq)

  ggplot(freq_df, aes(x = country, y = Freq, fill = haplotype)) +
    geom_col(position = "stack") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Geographic Distribution of Haplotypes",
         x = "Country",
         y = "Frequency",
         fill = "Haplotype")
}

#' Create summary sheets for export
#' @param results Analysis results
#' @return List of data frames
#' @keywords internal
create_summary_sheets <- function(results) {
  if (is.null(results)) return(NULL)

  list(
    Summary = format_diversity_stats(results)$data,
    Sequences = format_sequence_stats(results)$data,
    Details = do.call(rbind, lapply(names(results), function(species) {
      result <- results[[species]]
      data.frame(
        species = species,
        specimen = seq_len(result$total_specimens),
        haplotype = result$haplotype_assignments,
        country = result$country.ocean,
        stringsAsFactors = FALSE
      )
    }))
  )
}

#' Write sequences to FASTA file
#' @param data Analysis data
#' @param file Output file path
#' @keywords internal
write_fasta_file <- function(data, file) {
  if (is.null(data) || is.null(data$sequences)) return(NULL)

  con <- file(file, "w")
  on.exit(close(con))

  for (i in seq_along(data$sequences)) {
    writeLines(c(
      sprintf(">Haplotype_%d", data$haplotype_assignments[i]),
      as.character(data$sequences[i])
    ), con)
  }
}

#' Generate network plots PDF
#' @param results Analysis results
#' @param file Output file path
#' @keywords internal
generate_network_pdf <- function(results, file) {
  if (is.null(results)) return(NULL)

  pdf(file)
  on.exit(dev.off())

  for (species in names(results)) {
    result <- results[[species]]
    plot <- plot_haplotype_network(result)
    print(plot)
  }
}
