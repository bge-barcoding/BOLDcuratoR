# R/modules/data_import/mod_data_import_server.R

#' Server Module for Data Import
#' @param id The module ID
#' @param state State management instance
#' @param session_db DBI connection to the session SQLite database
#' @param logger Logger instance
#' @export
mod_data_import_server <- function(id, state, session_db, logger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper to get sessions filtered by the current user's identifiers.
    # Returns the user's own sessions if any identifier matches, otherwise
    # falls back to showing all sessions for backward compatibility.
    get_user_sessions <- reactive({
      user_info <- state$get_store()$user_info
      user_sessions <- filter_sessions_by_user(
        user_email = user_info$email,
        user_orcid = user_info$orcid,
        user_name  = user_info$name,
        con        = session_db
      )
      user_sessions
    })

    # Render saved sessions list
    output$saved_sessions_ui <- renderUI({
      sessions <- get_user_sessions()
      if (nrow(sessions) == 0) {
        div(class = "text-muted", "No saved sessions found for your account. Enter your details in the User Info panel to find previous sessions.")
      } else {
        tagList(
          DTOutput(ns("saved_sessions_table")),
          div(
            style = "margin-top: 10px;",
            actionButton(ns("resume_session"),
                         "Resume Selected Session",
                         class = "btn-primary",
                         icon = icon("redo"))
          )
        )
      }
    })

    output$saved_sessions_table <- renderDT({
      sessions <- get_user_sessions()
      if (nrow(sessions) == 0) return(NULL)

      # Show user-friendly columns; hide internal session_id
      display <- sessions[, c("created_at", "updated_at", "user_email",
                               "user_name", "specimen_count", "species_count"),
                           drop = FALSE]
      names(display) <- c("Created", "Updated", "Email", "Name",
                           "Specimens", "Species")

      DT::datatable(
        display,
        options = list(pageLength = 5, dom = 'tip'),
        selection = 'single',
        rownames = FALSE
      )
    })

    # ReactiveVal to communicate the selected session ID to app.R.
    # sendInputMessage doesn't work for module→parent communication because
    # it triggers a click count, not the session ID string.
    selected_session_id <- reactiveVal(NULL)

    observeEvent(input$resume_session, {
      sessions <- get_user_sessions()
      selected_row <- input$saved_sessions_table_rows_selected
      req(selected_row)
      session_id <- sessions$session_id[selected_row]
      selected_session_id(session_id)
    })

    # Reactive Values
    search_params     <- reactiveVal(NULL)
    validation_status <- reactiveVal(list(valid = TRUE, messages = character()))

    # Intermediate state held between Phase 1-3.5 and Phase 4-5 when the
    # size check requires user confirmation before BIN expansion proceeds.
    pending_specimens <- reactiveVal(NULL)
    pending_params    <- reactiveVal(NULL)

    # Track search progress
    progress <- reactive({
      store <- state$get_store()
      store$processing$progress
    })

    # Countries derived from the continent checkbox selection
    selected_continent_countries <- reactive({
      continents <- input$continent_filter
      if (is.null(continents) || length(continents) == 0) return(NULL)
      unique(unlist(CONTINENT_COUNTRIES[continents]))
    })

    # Validate search parameters
    observe({
      total_length <- sum(
        nchar(input$taxa_input),
        nchar(input$dataset_codes),
        nchar(input$project_codes)
        # continent_countries is applied post-download; adds no URL length
      )

      if (total_length > 1800) {
        logger$warn("Query length exceeds recommended limit")
        shinyjs::show(id = "url_warning")
      } else {
        shinyjs::hide(id = "url_warning")
      }

      search_params(prepare_search_params(input, selected_continent_countries()))
    })

    # Form submission handler
    observeEvent(input$submit, {
      # Validate API key from user info
      user_info <- state$get_store()$user_info
      if (is.null(user_info) || is.null(user_info$bold_api_key)) {
        logger$error("Attempted search without API key")
        showNotification("Please set your BOLD API key first", type = "error")
        return()
      }

      # Check input validation
      validation <- validation_status()
      if (!validation$valid) {
        logger$error("Input validation failed", validation$messages)
        showNotification(paste(validation$messages, collapse = "\n"), type = "error")
        return()
      }

      # Clear any pending state from a previous run that was not confirmed
      pending_specimens(NULL)
      pending_params(NULL)

      # Initialize progress
      state$update_state("processing", list(
        active = TRUE,
        progress = 0,
        message = "Starting data import..."
      ))

      # Log search attempt
      logger$info("Starting BOLD search", list(
        taxa       = input$taxa_input,
        datasets   = input$dataset_codes,
        projects   = input$project_codes,
        continents = input$continent_filter
      ))

      withProgress(message = "Searching BOLD database", value = 0, {
        tryCatch({
          params <- search_params()

          # Reset error state
          state$update_state("error", list(
            has_error = FALSE,
            message = NULL,
            details = NULL,
            timestamp = NULL
          ))

          combined_specimens <- NULL

          # --- Phase 1: Dataset codes (0% – 20%) ---
          if (!is.null(params$dataset_codes) && length(params$dataset_codes) > 0) {
            logger$info("Processing datasets", params$dataset_codes)
            setProgress(0.01, detail = sprintf(
              "Fetching %d dataset(s)...", length(params$dataset_codes)))

            dataset_specimens <- fetch_specimens(
              params$dataset_codes, "datasets",
              progress_range = c(0.0, 0.2))
            if (!is.null(dataset_specimens)) {
              combined_specimens <- dataset_specimens
              logger$info("Dataset fetch successful",
                          list(records = nrow(dataset_specimens)))
            }
          }

          # --- Phase 2: Project codes (20% – 40%) ---
          if (!is.null(params$project_codes) && length(params$project_codes) > 0) {
            logger$info("Processing projects", params$project_codes)
            setProgress(0.20, detail = sprintf(
              "Fetching %d project(s)...", length(params$project_codes)))

            project_specimens <- fetch_specimens(
              params$project_codes, "projects",
              progress_range = c(0.2, 0.4))
            if (!is.null(project_specimens)) {
              logger$info("Project fetch successful",
                          list(records = nrow(project_specimens)))
              combined_specimens <- merge_specimens(combined_specimens, project_specimens)
            }
          }

          # --- Phase 3: Taxonomy search (40% – 70%) ---
          if (!is.null(params$taxonomy)) {
            logger$info("Processing taxonomy search", params$taxonomy)
            setProgress(0.40, detail = sprintf(
              "Searching %d taxon/taxa...", length(params$taxonomy)))

            taxonomy_specimens <- process_taxonomy_search(params$taxonomy)

            if (!is.null(taxonomy_specimens)) {
              combined_specimens <- merge_specimens(combined_specimens, taxonomy_specimens)
            }
          }

          # --- Phase 3.5: Continent filtering (pre-BIN expansion) ---
          # Filter downloaded records to selected continents before deriving the
          # BIN set, so BIN expansion only fetches BINs observed in the target region.
          # BIN expansion itself remains global (needed for concordance analysis).
          if (!is.null(params$continent_countries) && length(params$continent_countries) > 0) {
            if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
              before_count <- nrow(combined_specimens)

              setProgress(0.70, detail = sprintf(
                "Filtering %d records to %s...",
                before_count,
                paste(params$continents, collapse = ", ")
              ))

              combined_specimens <- filter_specimens_by_continent(
                combined_specimens,
                params$continent_countries
              )

              after_count <- nrow(combined_specimens)

              logger$info("Continent filtering applied", list(
                continents          = params$continents,
                countries_in_filter = length(params$continent_countries),
                records_before      = before_count,
                records_after       = after_count,
                records_removed     = before_count - after_count
              ))

              if (nrow(combined_specimens) == 0) {
                stop(sprintf(
                  "No specimens found in the selected continent(s) (%s) after filtering. %d records were removed. Try broadening your geographic selection.",
                  paste(params$continents, collapse = ", "),
                  before_count
                ))
              }

              showNotification(
                sprintf(
                  "Continent filter applied: %d of %d records retained",
                  after_count, before_count
                ),
                type = "message"
              )
            }
          }

          # --- Phase 3.75: Size check (pre-BIN expansion) ---
          # Count records and unique BINs in the filtered set. If either
          # exceeds the configured threshold, pause and show a modal before
          # starting BIN expansion (which can be very slow for large sets).
          if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
            n_records <- nrow(combined_specimens)
            n_bins    <- length(unique(combined_specimens$bin_uri[
              !is.na(combined_specimens$bin_uri) & combined_specimens$bin_uri != ""
            ]))

            user_info  <- state$get_store()$user_info
            shared     <- is_shared_key(user_info)
            over_limit <- (shared && (n_records > DOWNLOAD_LIMITS$SHARED_MAX_RECORDS ||
                                        n_bins    > DOWNLOAD_LIMITS$SHARED_MAX_BINS)) ||
                          (!shared && (n_records > DOWNLOAD_LIMITS$WARN_RECORDS ||
                                         n_bins    > DOWNLOAD_LIMITS$WARN_BINS))

            if (over_limit) {
              logger$warn("Download size check triggered", list(
                records = n_records, bins = n_bins, shared_key = shared
              ))

              # Park intermediate results so Phase 4-5 can resume after confirmation
              pending_specimens(combined_specimens)
              pending_params(params)

              state$update_state("processing", list(
                active  = FALSE,
                progress = 70,
                message = sprintf(
                  "Awaiting confirmation — %s records, %s BINs",
                  format(n_records, big.mark = ","),
                  format(n_bins,    big.mark = ",")
                )
              ))

              if (shared) {
                showModal(large_dataset_blocked_modal(n_records, n_bins))
              } else {
                showModal(large_dataset_confirm_modal(n_records, n_bins, ns))
              }
              return()  # Exit the tryCatch body cleanly; modal handles next step
            }
          }

          # Under the size limit (or no data yet): run Phase 4+5 now.
          do_expansion_and_processing(combined_specimens, params)

        }, error = function(e) {
          # Update error state
          logger$error("Search failed", e$message)
          state$update_state("error", list(
            has_error = TRUE,
            message = paste("Error retrieving data:", e$message),
            details = list(
              search_params = params,
              timestamp = Sys.time()
            ),
            source = "data_import"
          ))

          # Reset processing state
          state$update_state("processing", list(
            active = FALSE,
            progress = 0,
            message = NULL
          ))

          showNotification(paste("Error retrieving data:", e$message),
                           type = "error")
        })
      })
    })

    # ── Size-check confirmation handlers ────────────────────────────────────────

    # User confirmed they want to proceed despite the large dataset.
    observeEvent(input$confirm_large_download, {
      removeModal()

      specimens     <- pending_specimens()
      stored_params <- pending_params()

      # Clear pending state immediately so a stale confirm cannot re-trigger
      pending_specimens(NULL)
      pending_params(NULL)

      if (is.null(specimens)) {
        showNotification(
          "Session data expired. Please run the search again.",
          type = "error"
        )
        state$update_state("processing", list(active = FALSE, progress = 0, message = NULL))
        return()
      }

      logger$info("User confirmed large download", list(
        records = nrow(specimens),
        bins    = length(unique(specimens$bin_uri[!is.na(specimens$bin_uri)]))
      ))

      withProgress(message = "Expanding BINs", value = 0.70, {
        tryCatch({
          do_expansion_and_processing(specimens, stored_params)
        }, error = function(e) {
          logger$error("Expansion failed after confirmation", e$message)
          state$update_state("error", list(
            has_error = TRUE,
            message   = paste("Error during BIN expansion:", e$message),
            details   = list(timestamp = Sys.time()),
            source    = "data_import"
          ))
          state$update_state("processing", list(active = FALSE, progress = 0, message = NULL))
          showNotification(paste("Error during BIN expansion:", e$message), type = "error")
        })
      })
    })

    # User cancelled the large-download confirmation modal.
    observeEvent(input$cancel_large_download, {
      removeModal()
      pending_specimens(NULL)
      pending_params(NULL)
      state$update_state("processing", list(active = FALSE, progress = 0, message = NULL))
      logger$info("Large download cancelled by user")
      showNotification("Download cancelled.", type = "message")
    })

    # ── Size-check helper functions ──────────────────────────────────────────────

    # Returns TRUE if user_info$bold_api_key matches the shared/fallback key.
    is_shared_key <- function(user_info) {
      if (is.null(user_info$bold_api_key)) return(FALSE)
      fallback <- get_fallback_api_key()
      if (is.null(fallback)) return(FALSE)
      identical(user_info$bold_api_key, fallback)
    }

    # Modal shown to personal-key users: explains the size and lets them confirm.
    large_dataset_confirm_modal <- function(n_records, n_bins, ns) {
      modalDialog(
        title = tags$span(icon("exclamation-triangle"), " Large dataset — confirm to continue"),
        tags$p(sprintf(
          "The filtered dataset contains %s records across %s unique BINs.",
          format(n_records, big.mark = ","),
          format(n_bins,    big.mark = ",")
        )),
        tags$p("BIN expansion fetches all global records for each BIN and may take a very long time with this many BINs."),
        tags$ul(
          tags$li(sprintf("Records: %s  (warn threshold: %s)",
                          format(n_records, big.mark = ","),
                          format(DOWNLOAD_LIMITS$WARN_RECORDS, big.mark = ","))),
          tags$li(sprintf("BINs to expand: %s  (warn threshold: %s)",
                          format(n_bins, big.mark = ","),
                          format(DOWNLOAD_LIMITS$WARN_BINS, big.mark = ",")))
        ),
        tags$p(tags$em("Consider narrowing your search (continent filter, fewer taxa) if this is unexpectedly large.")),
        footer = tagList(
          actionButton(ns("cancel_large_download"),  "Cancel",           class = "btn-default"),
          actionButton(ns("confirm_large_download"), "Continue anyway",  class = "btn-warning")
        ),
        easyClose = FALSE
      )
    }

    # Modal shown to shared-key users: hard stop, no confirm button.
    large_dataset_blocked_modal <- function(n_records, n_bins) {
      modalDialog(
        title = tags$span(icon("ban"), " Download not permitted with shared key"),
        tags$p(sprintf(
          "The filtered dataset contains %s records across %s unique BINs.",
          format(n_records, big.mark = ","),
          format(n_bins,    big.mark = ",")
        )),
        tags$p(tags$strong(
          "Large downloads cannot be run using the shared API key."
        )),
        tags$p("To proceed, either:"),
        tags$ul(
          tags$li("Use your own personal BOLD API key (enter it in the User Info panel), or"),
          tags$li("Narrow your search — add a continent filter, fewer taxa, or use dataset/project codes.")
        ),
        tags$ul(
          tags$li(sprintf("Records limit: %s",
                          format(DOWNLOAD_LIMITS$SHARED_MAX_RECORDS, big.mark = ","))),
          tags$li(sprintf("BINs limit: %s",
                          format(DOWNLOAD_LIMITS$SHARED_MAX_BINS, big.mark = ",")))
        ),
        footer = modalButton("Close"),
        easyClose = FALSE
      )
    }

    # ── Phase 4+5 helper: BIN expansion → processing ────────────────────────────
    # Called from the submit handler (under-limit path) and from the confirm
    # handler (over-limit path after user approves).  Must be invoked from
    # inside an active withProgress() block so setProgress() works correctly.

    do_expansion_and_processing <- function(combined_specimens, params) {

      # --- Phase 4: BIN expansion (70% – 90%) ---
      if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
        bin_uris <- unique(combined_specimens$bin_uri[
          !is.na(combined_specimens$bin_uri) & combined_specimens$bin_uri != ""
        ])

        if (length(bin_uris) > 0) {
          logger$info("Starting BIN expansion", list(
            initial_specimens = nrow(combined_specimens),
            bins_to_expand    = length(bin_uris)
          ))

          setProgress(0.70, detail = sprintf(
            "Expanding BIN coverage (%d BINs, %d specimens so far)...",
            length(bin_uris), nrow(combined_specimens)))

          # Ensure API key is set
          user_info <- state$get_store()$user_info
          if (!is.null(user_info) && !is.null(user_info$bold_api_key)) {
            BOLDconnectR::bold.apikey(user_info$bold_api_key)
          }

          # Fetch in batches to avoid API limits
          batch_size  <- 50
          bin_batches <- split(bin_uris, ceiling(seq_along(bin_uris) / batch_size))

          for (batch_idx in seq_along(bin_batches)) {
            batch <- bin_batches[[batch_idx]]

            setProgress(
              0.70 + (batch_idx / length(bin_batches)) * 0.20,
              detail = sprintf(
                "Expanding BINs (batch %d/%d, %d specimens so far)...",
                batch_idx, length(bin_batches), nrow(combined_specimens)))

            bin_specimens <- tryCatch({
              bold_fetch_with_retry(
                get_by      = "bin_uris",
                identifiers = paste(batch, collapse = ",")
              )
            }, error = function(e) {
              logger$warn("BIN batch fetch failed", list(batch = batch_idx, error = e$message))
              NULL
            })

            if (!is.null(bin_specimens) && nrow(bin_specimens) > 0) {
              combined_specimens <- merge_specimens(combined_specimens, bin_specimens)
            }
          }

          logger$info("BIN expansion complete", list(
            bins_queried    = length(bin_uris),
            total_specimens = nrow(combined_specimens)
          ))
        }
      }

      # --- Phase 5: Processing (90% – 100%) ---
      if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
        setProgress(0.90, detail = sprintf(
          "Processing %d specimens...", nrow(combined_specimens)))

        # Validate specimen data
        validation <- validate_specimen_data(combined_specimens)
        if (!validation$valid) {
          logger$error("Specimen validation failed", validation$messages)
          stop(paste("Invalid specimen data:",
                     paste(validation$messages, collapse = "; ")))
        }

        # Process and update state
        processed_data <- process_specimen_data(combined_specimens)
        state$update_state("specimen_data", processed_data, validate_specimen_data)

        # Persist the original taxa input groups for gap analysis.
        if (!is.null(params$taxonomy_groups)) {
          state$update_state("search_taxa", params$taxonomy_groups)
        } else {
          state$update_state("search_taxa", as.list(params$taxonomy))
        }

        # Clear previous analyses
        state$update_state("bin_analysis", NULL)
        state$update_state("bags_grades", NULL)
        state$update_state("selected_specimens", list())

        # Update completion status
        setProgress(1.0, detail = sprintf(
          "Done — %d specimens, %d species, %d BINs",
          nrow(processed_data),
          length(unique(processed_data$species[!is.na(processed_data$species)])),
          length(unique(processed_data$bin_uri[!is.na(processed_data$bin_uri)]))))

        state$update_state("processing", list(
          active   = FALSE,
          progress = 100,
          message  = sprintf("Retrieved %d specimens successfully", nrow(processed_data))
        ))

        logger$info("Search completed successfully", list(
          total_specimens = nrow(processed_data),
          unique_species  = length(unique(processed_data$species)),
          unique_bins     = length(unique(processed_data$bin_uri))
        ))

        showNotification(
          sprintf("Retrieved %d specimens", nrow(processed_data)),
          type = "message"
        )
      } else {
        logger$warn("No specimens found matching criteria")
        stop("No specimens found matching search criteria")
      }
    }

    # Helper function to fetch specimens using BOLDconnectR.
    # Fetches each code individually so one failing code (e.g. private
    # dataset returning 401) doesn't prevent the others from loading.
    # progress_range = c(start, end) maps to the Shiny progress bar.
    fetch_specimens <- function(codes, type = c("datasets", "projects"),
                                progress_range = c(0, 1)) {
      type <- match.arg(type)
      get_by <- if (type == "datasets") "dataset_codes" else "project_codes"
      type_label <- if (type == "datasets") "dataset" else "project"

      # Get API key from state
      user_info <- state$get_store()$user_info
      if (is.null(user_info) || is.null(user_info$bold_api_key)) {
        logging_manager$error("No API key available for dataset/project fetch")
        showNotification("Please set your BOLD API key first", type = "error")
        return(NULL)
      }

      # Set API key for request
      BOLDconnectR::bold.apikey(user_info$bold_api_key)

      combined <- NULL
      failed_codes <- character()
      n_codes <- length(codes)

      for (i in seq_along(codes)) {
        code <- codes[i]
        pct <- progress_range[1] +
          ((i - 1) / n_codes) * (progress_range[2] - progress_range[1])
        setProgress(pct, detail = sprintf(
          "Fetching %s %d/%d: %s", type_label, i, n_codes, code))

        result <- tryCatch({
          logger$info(sprintf("Fetching %s: %s", type, code))
          specimens <- bold_fetch_with_retry(
            get_by = get_by,
            identifiers = code
          )

          if (!is.null(specimens) && nrow(specimens) > 0) {
            logger$info(sprintf("Fetched %d records from %s", nrow(specimens), code))
          }
          specimens
        }, error = function(e) {
          logging_manager$error(sprintf("Error fetching %s '%s': %s", type, code, e$message))
          failed_codes <<- c(failed_codes, code)
          showNotification(
            sprintf("Failed to fetch %s '%s': %s", type, code, e$message),
            type = "warning",
            duration = 10
          )
          NULL
        })

        if (!is.null(result) && nrow(result) > 0) {
          combined <- merge_specimens(combined, result)
        }
      }

      if (length(failed_codes) > 0) {
        logger$warn(sprintf("Failed to fetch %d %s: %s",
                            length(failed_codes), type,
                            paste(failed_codes, collapse = ", ")))
      }

      if (!is.null(combined) && nrow(combined) > 0) {
        logging_manager$info("Columns after BOLD fetch", list(
          num_columns = length(names(combined)),
          specimen_count = nrow(combined)
        ))
      }

      combined
    }

    # Helper function to process taxonomy search.
    # Each taxon is searched individually so that one invalid name
    # (e.g. a typo or a name absent from BOLD) does not cause the
    # entire batch to fail.  Genus names are title-cased because the
    # BOLD preprocessor does an exact-match comparison.
    #
    # No geography filter is passed to the API — continent filtering is
    # applied post-download (Phase 3.5) to avoid HTTP 422 errors that
    # occur when any country in the list has no records for the taxon.
    process_taxonomy_search <- function(taxonomy) {
      tryCatch({
        # Get API key from state
        user_info <- state$get_store()$user_info
        if (is.null(user_info) || is.null(user_info$bold_api_key)) {
          stop("No API key available")
        }

        # Set API key for request
        BOLDconnectR::bold.apikey(user_info$bold_api_key)

        # Search each taxon independently, collecting process IDs.
        # Progress: 40% – 60% for the public search, 60% – 70% for full fetch.
        n_taxa <- length(taxonomy)
        all_search_results <- lapply(seq_along(taxonomy), function(idx) {
          taxon <- taxonomy[idx]
          # Title-case the genus (first word) to satisfy BOLD's exact-match check
          taxon <- sub("^(\\w)", "\\U\\1", trimws(taxon), perl = TRUE)

          pct <- 0.40 + ((idx - 1) / n_taxa) * 0.20
          setProgress(pct, detail = sprintf(
            "Searching taxon %d/%d: %s", idx, n_taxa, taxon))

          tryCatch({
            res <- BOLDconnectR::bold.public.search(taxonomy = list(taxon))
            if (!is.null(res) && nrow(res) > 0) {
              logger$info(sprintf("Found %d records for '%s'", nrow(res), taxon))
              res[!duplicated(res$processid), ]
            } else {
              logger$warn(sprintf("No records for taxon: %s", taxon))
              NULL
            }
          }, error = function(e) {
            logger$warn(sprintf("Taxon search failed for '%s': %s", taxon, e$message))
            NULL
          })
        })

        # Combine results, dropping NULLs
        valid <- Filter(Negate(is.null), all_search_results)
        if (length(valid) == 0) return(NULL)

        search_results <- do.call(rbind, valid)
        search_results <- search_results[!duplicated(search_results$processid), ]

        if (nrow(search_results) > 0) {
          logger$info(sprintf("Total unique records from taxonomy search: %d",
                              nrow(search_results)))
          setProgress(0.60, detail = sprintf(
            "Downloading full data for %d records...", nrow(search_results)))
          specimens <- bold_fetch_with_retry(
            get_by = "processid",
            identifiers = search_results$processid
          )
          return(specimens)
        }
        NULL
      }, error = function(e) {
        logger$error("Taxonomy search failed", e$message)
        NULL
      })
    }

    # Helper: retry a BOLD API call with exponential backoff.
    # Retries on transient errors (timeouts, 5xx) up to max_retries times.
    bold_fetch_with_retry <- function(..., max_retries = 3, base_delay = 2) {
      for (attempt in seq_len(max_retries + 1)) {
        result <- tryCatch(
          BOLDconnectR::bold.fetch(...),
          error = function(e) e
        )
        if (!inherits(result, "error")) return(result)

        is_transient <- grepl(
          "timeout|timed out|Gateway|503|504|Connection reset",
          result$message, ignore.case = TRUE
        )
        if (!is_transient || attempt > max_retries) stop(result)

        delay <- base_delay * (2 ^ (attempt - 1))
        logger$warn(sprintf(
          "BOLD API error (attempt %d/%d), retrying in %ds: %s",
          attempt, max_retries + 1, delay, result$message
        ))
        Sys.sleep(delay)
      }
    }

    # Helper function to merge specimen results
    merge_specimens <- function(existing, new_specimens) {
      if (is.null(existing)) return(new_specimens)
      if (is.null(new_specimens)) return(existing)

      rbind(
        existing,
        new_specimens[!new_specimens$processid %in% existing$processid, ]
      )
    }

    # Clear input handler
    observeEvent(input$clear_input, {
      updateTextAreaInput(session, "taxa_input", value = "")
      updateTextAreaInput(session, "dataset_codes", value = "")
      updateTextAreaInput(session, "project_codes", value = "")
      updateCheckboxGroupInput(session, "continent_filter", selected = character(0))

      # Log action
      logger$info("Input fields cleared")
      showNotification("Input fields cleared", type = "message")
    })

    # Clear results handler - show confirmation dialog first
    observeEvent(input$clear_results, {
      showModal(modalDialog(
        title = "Clear All Results",
        tags$p("This will clear all data and analyses including:"),
        tags$ul(
          tags$li("All specimen data"),
          tags$li("BIN analysis results"),
          tags$li("BAGS grades"),
          tags$li("Species analysis and gap analysis"),
          tags$li("All specimen selections, flags, and curator notes")
        ),
        tags$p(tags$strong("This action cannot be undone.")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_clear_results"), "Clear All Results",
                       class = "btn-danger", icon = icon("trash"))
        )
      ))
    })

    # Confirmed clear results - clear ALL state
    observeEvent(input$confirm_clear_results, {
      removeModal()
      logger$info("Clearing all results and analyses")

      state$update_state("specimen_data", NULL)
      state$update_state("bin_analysis", NULL)
      state$update_state("bags_grades", NULL)
      state$update_state("search_taxa", NULL)
      state$update_state("selected_specimens", list())
      state$update_state("specimen_flags", list())
      state$update_state("specimen_curator_notes", list())
      state$update_state("specimen_updated_ids", list())
      state$update_state("specimen_metrics", NULL)
      state$update_state("specimen_history", list())
      state$update_state("processing", list(
        active = FALSE,
        progress = 0,
        message = NULL
      ))

      showNotification("All results and analyses cleared", type = "message")
    })

    # Value box outputs
    output$total_records_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) nrow(specimen_data) else 0
      valueBox(
        count,
        "Total Records",
        icon = icon("table"),
        color = "blue"
      )
    })

    output$unique_taxa_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) {
        length(unique(specimen_data$species[!is.na(specimen_data$species)]))
      } else 0
      valueBox(
        count,
        "Unique Taxa",
        icon = icon("sitemap"),
        color = "green"
      )
    })

    output$unique_bins_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) {
        length(unique(specimen_data$bin_uri[!is.na(specimen_data$bin_uri)]))
      } else 0
      valueBox(
        count,
        "Unique BINs",
        icon = icon("dna"),
        color = "purple"
      )
    })

    output$countries_box <- renderValueBox({
      specimen_data <- state$get_store()$specimen_data
      count <- if (!is.null(specimen_data)) {
        length(unique(specimen_data$country.ocean[!is.na(specimen_data$country.ocean)]))
      } else 0
      valueBox(
        count,
        "Countries",
        icon = icon("globe"),
        color = "orange"
      )
    })

    # FASTA download handler
    output$download_fasta <- downloadHandler(
      filename = function() {
        paste0("bold_sequences_", format(Sys.time(), "%Y%m%d_%H%M"), ".fasta")
      },
      content = function(file) {
        logger$info("Downloading FASTA sequences", list(format = "fasta"))
        data <- state$get_store()$specimen_data
        if (is.null(data) || nrow(data) == 0) {
          writeLines("", file)
          return()
        }

        # Filter to specimens with sequences
        if (!"nuc" %in% names(data)) {
          writeLines("", file)
          showNotification("No sequence data available", type = "warning")
          return()
        }

        seq_data <- data[!is.na(data$nuc) & data$nuc != "", ]
        if (nrow(seq_data) == 0) {
          writeLines("", file)
          showNotification("No sequences found", type = "warning")
          return()
        }

        # Write FASTA with header format >processid|identification
        con <- file(file, "w")
        on.exit(close(con))
        for (i in seq_len(nrow(seq_data))) {
          pid <- seq_data$processid[i]
          ident <- if ("identification" %in% names(seq_data)) {
            seq_data$identification[i]
          } else {
            seq_data$species[i]
          }
          ident <- ifelse(is.na(ident) || ident == "", "Unknown", ident)
          header <- sprintf(">%s|%s", pid, ident)
          writeLines(c(header, seq_data$nuc[i]), con)
        }

        logger$info("Downloaded FASTA file", list(sequences = nrow(seq_data)))
      },
      contentType = "text/plain"
    )

    # Download handler
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("bold_search_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        logger$info("Downloading search results", list(format = "csv"))
        data <- state$get_store()$specimen_data
        if (is.null(data) || nrow(data) == 0) return(NULL)

        # Coerce list/factor columns to plain character for clean CSV
        for (col in names(data)) {
          if (is.list(data[[col]])) {
            data[[col]] <- vapply(data[[col]], function(x) {
              if (is.null(x) || length(x) == 0) return(NA_character_)
              paste(x, collapse = "; ")
            }, character(1))
          } else if (is.factor(data[[col]])) {
            data[[col]] <- as.character(data[[col]])
          }
        }

        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    # Return reactive endpoints for parent module communication
    list(
      selected_session_id = selected_session_id
    )
  })
}
