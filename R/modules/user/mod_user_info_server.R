# User Information Server Module
# R/modules/user/mod_user_info_server.R

#' Server Module for User Information Management
#' @param id Module ID
#' @param state State management instance
#' @param logger Logger instance
#' @export
mod_user_info_server <- function(id, state, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validation message output
    output$validation_message <- renderUI({
      # Show nothing if no validation issues
      if (is.null(input$email) && is.null(input$orcid)) {
        return(NULL)
      }

      messages <- character()

      # Validate email if provided
      if (!is.null(input$email) && nchar(input$email) > 0) {
        if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$email)) {
          messages <- c(messages, "Invalid email format")
        }
      }

      # Validate ORCID if provided
      if (!is.null(input$orcid) && nchar(input$orcid) > 0) {
        # Updated regex to match ORCID format (4 groups of 4 digits, last char optional X)
        if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{4}$|\\d{4}-\\d{4}-\\d{4}-\\d{3}X$", input$orcid)) {
          messages <- c(messages, "Invalid ORCID format (should be 0000-0000-0000-0000 or 0000-0000-0000-000X)")
        }
      }

      # Return validation messages if any
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

    # Save user information
    observeEvent(input$save, {
      # Validate inputs
      if (!is.null(input$email) && nchar(input$email) > 0) {
        if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$email)) {
          showNotification("Invalid email format", type = "error")
          return()
        }
      }

      if (!is.null(input$orcid) && nchar(input$orcid) > 0) {
        # Updated regex to match ORCID format with optional X
        if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{4}$|\\d{4}-\\d{4}-\\d{4}-\\d{3}X$", input$orcid)) {
          showNotification("Invalid ORCID format", type = "error")
          return()
        }
      }

      # Require at least email or name
      if ((is.null(input$email) || nchar(input$email) == 0) &&
          (is.null(input$name) || nchar(input$name) == 0)) {
        showNotification("Please provide either email or name", type = "error")
        return()
      }

      # Update state
      tryCatch({
        state$update_state("user_info", list(
          email = input$email,
          name = input$name,
          orcid = input$orcid
        ), validate_user_info)

        showNotification("User information saved", type = "message")
        logger$info("User information updated",
                    list(email = input$email, name = input$name))

      }, error = function(e) {
        showNotification(sprintf("Error saving user information: %s", e$message),
                         type = "error")
        logger$error("Failed to save user information", e$message)
      })
    })

    # Clear user information
    observeEvent(input$clear, {
      # Clear inputs
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "orcid", value = "")

      # Clear state
      state$clear_user_info()

      showNotification("User information cleared", type = "message")
      logger$info("User information cleared")
    })

    # Initialize user info from state if available
    observe({
      user_info <- state$get_store()$user_info

      if (!is.null(user_info$email)) {
        updateTextInput(session, "email", value = user_info$email)
      }
      if (!is.null(user_info$name)) {
        updateTextInput(session, "name", value = user_info$name)
      }
      if (!is.null(user_info$orcid)) {
        updateTextInput(session, "orcid", value = user_info$orcid)
      }
    })
  })
}
