# BOLDcuratoR/R/modules/interfaces.R

#' API Integration Module Interface
#' @export
api_interface <- list(
  set_api_key = function(key) NULL,
  validate_api_key = function(key) NULL,
  fetch_specimens = function(params) NULL,
  fetch_bin_data = function(bin_uris) NULL
)

#' Data Import Module Interface
#' @export
data_import_interface <- list(
  parse_taxa_input = function(input_text) NULL,
  parse_dataset_codes = function(input_text) NULL,
  parse_project_codes = function(input_text) NULL,
  parse_geography = function(countries, continents) NULL
)

#' Specimen Handling Module Interface
#' @export
specimen_interface <- list(
  process_specimens = function(specimens) NULL,
  rank_specimens = function(specimens) NULL,
  select_representative = function(specimens, criteria) NULL,
  validate_specimens = function(specimens) NULL
)

#' BIN Analysis Module Interface
#' @export
bin_interface <- list(
  analyze_bins = function(specimens) NULL,
  check_concordance = function(bin_data) NULL,
  summarize_bins = function(bin_analysis) NULL
)

#' BAGS Grading Module Interface
#' @export
bags_interface <- list(
  calculate_grades = function(specimens) NULL,
  validate_grades = function(grades) NULL,
  summarize_grades = function(grades) NULL
)

#' Haplotype Analysis Module Interface
#' @export
haplotype_interface <- list(
  analyze_haplotypes = function(specimens) NULL,
  identify_unique = function(haplotype_data) NULL,
  calculate_diversity = function(haplotype_data) NULL
)

#' Export Module Interface
#' @export
export_interface <- list(
  export_specimens = function(data, format) NULL,
  export_analysis = function(data, format) NULL,
  export_summary = function(data, format) NULL
)

#' Logging Module Interface
#' @export
logging_interface <- list(
  log_message = function(message, type, details) NULL,
  log_error = function(error, details) NULL,
  get_logs = function(type, timeframe) NULL
)

#' Required Module Events
#' @export
module_events <- list(
  api_key_set = "api_key_set",
  api_error = "api_error",
  processing_start = "processing_start",
  processing_complete = "processing_complete",
  processing_error = "processing_error",
  analysis_complete = "analysis_complete",
  grades_complete = "grades_complete",
  specimen_selected = "specimen_selected",
  specimen_deselected = "specimen_deselected",
  export_start = "export_start",
  export_complete = "export_complete",
  export_error = "export_error"
)

#' Module Response Structure
#' @export
module_response <- list(
  success = function(data = NULL, message = NULL) {
    list(
      status = "success",
      data = data,
      message = message,
      timestamp = Sys.time()
    )
  },
  error = function(message, details = NULL) {
    list(
      status = "error",
      message = message,
      details = details,
      timestamp = Sys.time()
    )
  }
)
