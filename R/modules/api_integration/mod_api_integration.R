# R/modules/api_integration/mod_api_integration.R

#' API Integration Module
#' @description Manages BOLD API interactions with rate limiting and error handling
#' @importFrom shiny moduleServer NS
#' @importFrom R6 R6Class
#' @importFrom BOLDconnectR bold.fetch bold.public.search bold.apikey

#' Rate Limiter R6 Class
#' @description Manages API request timing to prevent overload
#' @export
RateLimiter <- R6::R6Class("RateLimiter",
                           public = list(
                             #' @description Initialize rate limiter
                             #' @param requests_per_minute Maximum requests per minute
                             initialize = function(requests_per_minute = 1000) {
                               private$min_interval <- 60 / requests_per_minute
                               private$reset_counts()
                             },

                             #' @description Wait appropriate time before next request
                             wait_for_request = function() {
                               current_time <- Sys.time()
                               time_diff <- difftime(current_time, private$last_request, units = "secs")

                               if (time_diff < private$min_interval) {
                                 Sys.sleep(private$min_interval - as.numeric(time_diff))
                               }

                               private$last_request <- Sys.time()
                               private$counts$total <- private$counts$total + 1
                             },

                             #' @description Record successful request
                             record_success = function() {
                               private$counts$successful <- private$counts$successful + 1
                             },

                             #' @description Record failed request
                             record_failure = function() {
                               private$counts$failed <- private$counts$failed + 1
                             },

                             #' @description Get request metrics
                             get_metrics = function() {
                               private$counts
                             },

                             #' @description Reset metrics counters
                             reset_metrics = function() {
                               private$reset_counts()
                             }
                           ),

                           private = list(
                             min_interval = NULL,
                             last_request = NULL,
                             counts = NULL,

                             reset_counts = function() {
                               private$last_request <- Sys.time()
                               private$counts <- list(
                                 total = 0,
                                 successful = 0,
                                 failed = 0
                               )
                             }
                           )
)

#' BOLD API Manager R6 Class
#' @description Manages BOLD API authentication and requests
#' @export
BOLDAPIManager <- R6::R6Class("BOLDAPIManager",
                              public = list(
                                #' @description Initialize API manager
                                #' @param rate_limiter RateLimiter instance
                                #' @param logger Logger instance
                                initialize = function(rate_limiter, logger) {
                                  private$rate_limiter <- rate_limiter
                                  private$logger <- logger
                                },

                                #' @description Set and validate API key
                                #' @param key BOLD API key
                                #' @return Boolean indicating success
                                set_api_key = function(key) {
                                  if (!private$validate_key_format(key)) {
                                    private$logger$error("Invalid API key format")
                                    return(FALSE)
                                  }

                                  tryCatch({
                                    BOLDconnectR::bold.apikey(key)
                                    private$api_key <- key
                                    private$logger$info("API key set successfully")
                                    TRUE
                                  }, error = function(e) {
                                    private$logger$error(sprintf("Failed to set API key: %s", e$message))
                                    FALSE
                                  })
                                },

                                #' @description Clear stored API key
                                clear_api_key = function() {
                                  private$api_key <- NULL
                                  private$logger$info("API key cleared")
                                  TRUE
                                },

                                #' @description Get API connection status
                                get_status = function() {
                                  list(
                                    is_set = !is.null(private$api_key),
                                    metrics = private$rate_limiter$get_metrics()
                                  )
                                },

                                #' @description Fetch specimens from BOLD
                                #' @param params List of search parameters
                                #' @return Data frame of specimens or NULL on error
                                fetch_specimens = function(params) {
                                  if (!private$has_valid_key()) {
                                    private$logger$error("No valid API key set")
                                    return(NULL)
                                  }

                                  tryCatch({
                                    private$rate_limiter$wait_for_request()

                                    result <- if (!is.null(params$processids)) {
                                      BOLDconnectR::bold.fetch(
                                        get_by = "processid",
                                        identifiers = params$processids
                                      )
                                    } else if (!is.null(params$dataset_codes)) {
                                      BOLDconnectR::bold.fetch(
                                        get_by = "dataset_codes",
                                        identifiers = params$dataset_codes
                                      )
                                    } else if (!is.null(params$project_codes)) {
                                      BOLDconnectR::bold.fetch(
                                        get_by = "project_codes",
                                        identifiers = params$project_codes
                                      )
                                    } else {
                                      BOLDconnectR::bold.public.search(
                                        taxonomy = params$taxonomy,
                                        geography = params$geography
                                      )
                                    }

                                    if (!is.null(result) && nrow(result) > 0) {
                                      private$rate_limiter$record_success()
                                      private$logger$info(sprintf("Successfully fetched %d specimens", nrow(result)))
                                      result
                                    } else {
                                      private$rate_limiter$record_failure()
                                      private$logger$warn("No specimens found matching criteria")
                                      NULL
                                    }
                                  }, error = function(e) {
                                    private$rate_limiter$record_failure()
                                    private$logger$error(sprintf("Failed to fetch specimens: %s", e$message))
                                    NULL
                                  })
                                },

                                #' @description Search BOLD public data
                                #' @param params Search parameters
                                #' @return Search results or NULL on error
                                search_public = function(params) {
                                  if (!private$has_valid_key()) {
                                    private$logger$error("No valid API key set")
                                    return(NULL)
                                  }

                                  tryCatch({
                                    private$rate_limiter$wait_for_request()

                                    result <- BOLDconnectR::bold.public.search(
                                      taxonomy = params$taxonomy,
                                      geography = params$geography,
                                      institutions = params$institutions,
                                      researchers = params$researchers,
                                      bio_id = params$bio_id
                                    )

                                    if (!is.null(result) && nrow(result) > 0) {
                                      private$rate_limiter$record_success()
                                      private$logger$info(sprintf("Public search returned %d results", nrow(result)))
                                      result
                                    } else {
                                      private$rate_limiter$record_failure()
                                      private$logger$warn("No results found for public search")
                                      NULL
                                    }
                                  }, error = function(e) {
                                    private$rate_limiter$record_failure()
                                    private$logger$error(sprintf("Public search failed: %s", e$message))
                                    NULL
                                  })
                                }
                              ),

                              private = list(
                                api_key = NULL,
                                rate_limiter = NULL,
                                logger = NULL,

                                validate_key_format = function(key) {
                                  !is.null(key) &&
                                    nchar(trimws(key)) == 36 &&
                                    grepl("^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$",
                                          key, ignore.case = TRUE)
                                },

                                has_valid_key = function() {
                                  !is.null(private$api_key)
                                }
                              )
)

#' API Integration Server Module
#' @param id Module ID
#' @param state State management instance
#' @export
mod_api_integration_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize components
    rate_limiter <- RateLimiter$new()
    api_manager <- BOLDAPIManager$new(rate_limiter, state$logger)

    # API Status output
    output$api_status <- renderUI({
      status <- api_manager$get_status()
      metrics <- status$metrics

      if (status$is_set) {
        div(
          class = "api-status success",
          icon("check-circle"),
          sprintf("API Connected | Requests: %d | Success: %d | Failed: %d",
                  metrics$total, metrics$successful, metrics$failed)
        )
      } else {
        div(
          class = "api-status warning",
          icon("exclamation-circle"),
          "API Not Connected"
        )
      }
    })

    # Set API Key handler
    observeEvent(input$set_api_key, {
      req(input$bold_api_key)

      result <- api_manager$set_api_key(input$bold_api_key)
      if (result) {
        state$update_state("api_key_status", list(
          is_set = TRUE,
          key = input$bold_api_key,
          last_validated = Sys.time()
        ))

        updateTextInput(session, "bold_api_key", value = input$bold_api_key)
        shinyjs::enable("data_import-submit")

        showNotification("API key set successfully", type = "message")
      } else {
        showNotification("Failed to set API key", type = "error")
      }
    })

    # Clear API Key handler
    observeEvent(input$clear_api_key, {
      if (api_manager$clear_api_key()) {
        state$update_state("api_key_status", list(
          is_set = FALSE,
          key = NULL,
          last_validated = NULL
        ))

        updateTextInput(session, "bold_api_key", value = "")
        showNotification("API key cleared", type = "message")
      }
    })

    # Reset metrics handler
    observeEvent(input$reset_metrics, {
      rate_limiter$reset_metrics()
    })

    # Return API interface for other modules
    list(
      rate_limiter = rate_limiter,
      get_status = api_manager$get_status,
      fetch_specimens = api_manager$fetch_specimens,
      search_public = api_manager$search_public
    )
  })
}

#' API Integration UI Module
#' @param id Module ID
#' @export
mod_api_integration_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "api-integration",

    box(
      title = "BOLD API Connection",
      status = "primary",
      width = NULL,
      solidHeader = TRUE,

      div(
        class = "api-key-input",
        textInput(ns("bold_api_key"),
                  "API Key:",
                  placeholder = "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX")
      ),

      div(
        class = "api-controls",
        style = "margin-bottom: 15px;",
        actionButton(ns("set_api_key"),
                     "Connect API",
                     class = "btn-primary",
                     icon = icon("plug")),
        actionButton(ns("clear_api_key"),
                     "Disconnect",
                     class = "btn-warning",
                     icon = icon("power-off")),
        actionButton(ns("reset_metrics"),
                     "Reset Metrics",
                     class = "btn-info",
                     icon = icon("refresh"))
      ),

      uiOutput(ns("api_status")),

      div(
        class = "api-help",
        helpText(
          "Contact ",
          tags$a(href = "mailto:support@boldsystems.org",
                 "support@boldsystems.org"),
          " to obtain an API key"
        )
      )
    )
  )
}
