# R/modules/export/mod_export.R

#' Export Manager class using R6
#' @export
ExportManager <- R6::R6Class("ExportManager",
                             public = list(
                               initialize = function(logger, session_id = NULL) {
                                 private$logger <- logger
                                 private$session_id <- session_id
                               },

                               export_excel = function(data, filename, user_info = NULL,
                                                        annotations = NULL) {
                                 tryCatch({
                                   sheets <- list()
                                   record_count <- 0

                                   if (!is.null(data$summary_data)) {
                                     sheets$Summary <- data$summary_data
                                     record_count <- record_count + nrow(data$summary_data)
                                   }

                                   if (!is.null(data$specimen_data)) {
                                     specimen_export <- data$specimen_data
                                     if (!is.null(annotations)) {
                                       specimen_export <- merge_annotations_for_export(
                                         specimen_export,
                                         annotations$selections,
                                         annotations$flags,
                                         annotations$notes
                                       )
                                     }
                                     cols <- intersect(private$get_specimen_columns(), names(specimen_export))
                                     sheets$Specimens <- specimen_export[, cols]
                                     record_count <- record_count + nrow(data$specimen_data)
                                   }

                                   if (!is.null(data$bin_analysis)) {
                                     sheets$BIN_Summary <- private$prepare_bin_summary(data$bin_analysis)
                                     sheets$BIN_Content <- private$prepare_bin_content(data$bin_analysis)
                                     record_count <- record_count + nrow(sheets$BIN_Content)
                                   }

                                   if (!is.null(data$bags_grades)) {
                                     sheets$BAGS_Summary <- data$bags_grades
                                     record_count <- record_count + nrow(data$bags_grades)
                                   }

                                   if (!is.null(data$quality_summary)) {
                                     sheets$Quality_Summary <- data$quality_summary
                                     record_count <- record_count + nrow(data$quality_summary)
                                   }

                                   writexl::write_xlsx(sheets, filename)

                                   # Log successful export
                                   private$log_export(
                                     export_type = "excel",
                                     file_name = basename(filename),
                                     record_count = record_count,
                                     file_size = file.size(filename),
                                     format = "xlsx",
                                     success = TRUE,
                                     user_info = user_info,
                                     metadata = list(sheet_names = names(sheets))
                                   )

                                   return(TRUE)
                                 }, error = function(e) {
                                   # Log failed export
                                   private$log_export(
                                     export_type = "excel",
                                     file_name = basename(filename),
                                     record_count = 0,
                                     file_size = 0,
                                     format = "xlsx",
                                     success = FALSE,
                                     error_message = e$message,
                                     user_info = user_info
                                   )
                                   return(FALSE)
                                 })
                               },

                               export_tsv = function(data, filename, include_sequences = FALSE,
                                                      user_info = NULL, annotations = NULL) {
                                 tryCatch({
                                   specimen_data <- data$specimen_data
                                   if (!is.null(specimen_data)) {
                                     if (!is.null(annotations)) {
                                       specimen_data <- merge_annotations_for_export(
                                         specimen_data,
                                         annotations$selections,
                                         annotations$flags,
                                         annotations$notes
                                       )
                                     }
                                     cols <- private$get_specimen_columns()
                                     if (!include_sequences) {
                                       cols <- setdiff(cols, "nuc")
                                     }
                                     cols <- intersect(cols, names(specimen_data))

                                     write.table(specimen_data[, cols], filename,
                                                 sep = "\t", row.names = FALSE, quote = FALSE)

                                     # Log successful export
                                     private$log_export(
                                       export_type = "tsv",
                                       file_name = basename(filename),
                                       record_count = nrow(specimen_data),
                                       file_size = file.size(filename),
                                       format = "tsv",
                                       success = TRUE,
                                       user_info = user_info,
                                       metadata = list(
                                         include_sequences = include_sequences,
                                         columns = cols
                                       )
                                     )
                                     return(TRUE)
                                   }
                                   return(FALSE)
                                 }, error = function(e) {
                                   # Log failed export
                                   private$log_export(
                                     export_type = "tsv",
                                     file_name = basename(filename),
                                     record_count = 0,
                                     file_size = 0,
                                     format = "tsv",
                                     success = FALSE,
                                     error_message = e$message,
                                     user_info = user_info
                                   )
                                   return(FALSE)
                                 })
                               },

                               export_fasta = function(data, filename, user_info = NULL) {
                                 tryCatch({
                                   if (is.null(data$specimen_data) || !"nuc" %in% names(data$specimen_data)) {
                                     return(FALSE)
                                   }

                                   sequences <- data$specimen_data[!is.na(data$specimen_data$nuc) &
                                                                     data$specimen_data$nuc != "", ]
                                   if (nrow(sequences) == 0) return(FALSE)

                                   con <- file(filename, "w")
                                   on.exit(close(con))

                                   for (i in 1:nrow(sequences)) {
                                     header <- private$create_fasta_header(sequences[i, ])
                                     writeLines(c(header, sequences$nuc[i]), con)
                                   }

                                   # Log successful export
                                   private$log_export(
                                     export_type = "fasta",
                                     file_name = basename(filename),
                                     record_count = nrow(sequences),
                                     file_size = file.size(filename),
                                     format = "fasta",
                                     success = TRUE,
                                     user_info = user_info
                                   )
                                   return(TRUE)
                                 }, error = function(e) {
                                   # Log failed export
                                   private$log_export(
                                     export_type = "fasta",
                                     file_name = basename(filename),
                                     record_count = 0,
                                     file_size = 0,
                                     format = "fasta",
                                     success = FALSE,
                                     error_message = e$message,
                                     user_info = user_info
                                   )
                                   return(FALSE)
                                 })
                               },

                               get_export_history = function(from_date = NULL, to_date = NULL, limit = 100) {
                                 private$logger$get_export_history(from_date, to_date, limit = limit)
                               },

                               get_export_stats = function(from_date = NULL, to_date = NULL) {
                                 private$logger$get_export_stats(from_date, to_date)
                               }
                             ),

                             private = list(
                               logger = NULL,
                               session_id = NULL,

                               log_export = function(export_type, file_name, record_count, file_size,
                                                     format, success, error_message = NULL,
                                                     user_info = NULL, metadata = NULL) {
                                 user_email <- if (!is.null(user_info)) user_info$email else NULL
                                 user_name <- if (!is.null(user_info)) user_info$name else NULL

                                 private$logger$log_export(
                                   session_id = private$session_id,
                                   user_email = user_email,
                                   user_name = user_name,
                                   export_type = export_type,
                                   file_name = file_name,
                                   record_count = record_count,
                                   file_size = file_size,
                                   format = format,
                                   success = success,
                                   error_message = error_message,
                                   metadata = metadata
                                 )
                               },

                               get_specimen_columns = function() {
                                 c("processid", "species", "genus", "family", "order",
                                   "bin_uri", "identified_by", "identification_method",
                                   "collectors", "collection_date_start", "country.ocean",
                                   "coord", "institution", "voucher_type", "quality_score",
                                   "criteria_met", "nuc",
                                   "selected", "flag", "curator_notes")
                               },

                               prepare_bin_summary = function(bin_analysis) {
                                 summaries <- lapply(names(bin_analysis), function(name) {
                                   if (!is.null(bin_analysis[[name]]$bin_summary)) {
                                     summary <- bin_analysis[[name]]$bin_summary
                                     name_parts <- strsplit(name, "_")[[1]]
                                     summary$Valid_Name <- name_parts[1]
                                     summary$Search_Name <- name_parts[2]
                                     return(summary)
                                   }
                                   return(NULL)
                                 })
                                 do.call(rbind, summaries[!sapply(summaries, is.null)])
                               },

                               prepare_bin_content = function(bin_analysis) {
                                 contents <- lapply(names(bin_analysis), function(name) {
                                   if (!is.null(bin_analysis[[name]]$bin_content)) {
                                     content <- bin_analysis[[name]]$bin_content
                                     name_parts <- strsplit(name, "_")[[1]]
                                     content$Valid_Name <- name_parts[1]
                                     content$Search_Name <- name_parts[2]
                                     return(content)
                                   }
                                   return(NULL)
                                 })
                                 do.call(rbind, contents[!sapply(contents, is.null)])
                               },

                               create_fasta_header = function(specimen) {
                                 sprintf(">%s|%s|%s|%s|%s",
                                         specimen$processid,
                                         specimen$species,
                                         specimen$bin_uri,
                                         specimen$country.ocean,
                                         specimen$collection_date_start)
                               }
                             )
)
