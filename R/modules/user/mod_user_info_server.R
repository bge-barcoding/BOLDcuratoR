# R/modules/user/mod_user_info_server.R

#' Server Module for User Information Management
#' @param id Module ID
#' @param state State management instance
#' @param logger Logger instance
#' @export
mod_user_info_server <- function(id, state, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Validation message output
    output$validation_message <- renderUI({
      messages <- validate_inputs()

      if (length(messages) > 0) {
        div(
          class = "validation-messages",
          style = "color: red; margin-top: 10px;",
          lapply(messages, function(msg) {
            div(icon("exclamation-circle"), msg)
          })
        )
      }
    })

    # Input validation
    validate_inputs <- function() {
      messages <- character()

      # Validate email if provided
      if (!is.null(input$email) && nchar(input$email) > 0) {
        if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$email)) {
          messages <- c(messages, "Invalid email format")
        }
      }

      # Validate ORCID if provided
      if (!is.null(input$orcid) && nchar(input$orcid) > 0) {
        if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{4}$|\\d{4}-\\d{4}-\\d{4}-\\d{3}X$", input$orcid)) {
          messages <- c(messages, "Invalid ORCID format (should be 0000-0000-0000-0000 or 0000-0000-0000-000X)")
        }
      }

      # Validate API key if provided
      if (!is.null(input$bold_api_key) && nchar(input$bold_api_key) > 0) {
        if (!grepl("^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$",
                   input$bold_api_key, ignore.case = TRUE)) {
          messages <- c(messages, "Invalid API key format")
        }
      }

      # Require at least email/name and API key
      if ((is.null(input$email) || nchar(input$email) == 0) &&
          (is.null(input$name) || nchar(input$name) == 0)) {
        messages <- c(messages, "Please provide email or name")
      }

      if (is.null(input$bold_api_key) || nchar(input$bold_api_key) == 0) {
        messages <- c(messages, "BOLD API key is required")
      }

      messages
    }

    # Save user information
    observeEvent(input$save, {
      messages <- validate_inputs()

      if (length(messages) > 0) {
        showNotification(paste(messages, collapse = "\n"), type = "error")
        return()
      }

      # Update state with new user info
      tryCatch({
        # First try to set the API key
        if (!is.null(input$bold_api_key) && nchar(input$bold_api_key) > 0) {
          BOLDconnectR::bold.apikey(input$bold_api_key)
        }

        # If API key set successfully, update state
        state$update_state("user_info", list(
          email = input$email,
          name = input$name,
          orcid = input$orcid,
          bold_api_key = input$bold_api_key
        ), validate_user_info)

        showNotification("Information saved successfully", type = "message")
        logger$info("User information updated",
                    list(email = input$email,
                         name = input$name,
                         bold_api_key = "SET"))  # Don't log actual key

        # Enable data import submit button
        shinyjs::enable("data_import-submit")

      }, error = function(e) {
        showNotification(sprintf("Error saving user information: %s", e$message),
                         type = "error")
        logger$error("Failed to save user information", e$message)
      })
    })

    # Initialize user info from state if available
    observe({
      user_info <- state$get_store()$user_info

      if (!is.null(user_info)) {
        if (!is.null(user_info$email)) {
          updateTextInput(session, "email", value = user_info$email)
        }
        if (!is.null(user_info$name)) {
          updateTextInput(session, "name", value = user_info$name)
        }
        if (!is.null(user_info$orcid)) {
          updateTextInput(session, "orcid", value = user_info$orcid)
        }
        if (!is.null(user_info$bold_api_key)) {
          updateTextInput(session, "bold_api_key", value = user_info$bold_api_key)
          # Re-set API key in BOLDconnectR if available
          tryCatch({
            BOLDconnectR::bold.apikey(user_info$bold_api_key)
            shinyjs::enable("data_import-submit")
          }, error = function(e) {
            logger$warn("Failed to restore API key", e$message)
          })
        }
      }
    })
  })
}

#' Validate user info
#' @param info User info list
#' @return List with validation results
#' @keywords internal
validate_user_info <- function(info) {
  if (!is.list(info)) {
    return(list(valid = FALSE, messages = "User info must be a list"))
  }

  messages <- character()

  # Check required fields
  required_fields <- c("email", "name", "bold_api_key")
  if (!all(required_fields %in% names(info))) {
    missing <- setdiff(required_fields, names(info))
    messages <- c(messages, sprintf("Missing required fields: %s",
                                    paste(missing, collapse = ", ")))
  }

  # Validate email format if provided
  if (!is.null(info$email) && nchar(info$email) > 0) {
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", info$email)) {
      messages <- c(messages, "Invalid email format")
    }
  }

  # Validate API key format
  if (!is.null(info$bold_api_key) && nchar(info$bold_api_key) > 0) {
    if (!grepl("^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$",
               info$bold_api_key, ignore.case = TRUE)) {
      messages <- c(messages, "Invalid API key format")
    }
  }

  list(
    valid = length(messages) == 0,
    messages = messages
  )
}
