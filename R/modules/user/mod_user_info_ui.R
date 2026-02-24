# R/modules/user/mod_user_info_ui.R

#' UI Module for User Information Management
#' @param id The module ID
#' @export
mod_user_info_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "user-info-bar",
    style = "background: #f4f4f4; border-bottom: 1px solid #d2d6de; padding: 6px 15px; margin: 0;",
    fluidRow(
      style = "margin: 0; display: flex; align-items: center; flex-wrap: nowrap;",

      # Email field
      column(3, style = "padding: 0 5px;",
        div(style = "display: flex; align-items: center; gap: 5px;",
          tags$label("Email", style = "margin: 0; font-size: 11px; font-weight: 600; white-space: nowrap;"),
          textInput(ns("email"), label = NULL,
                    placeholder = "Enter your email address",
                    width = "100%")
        )
      ),

      # Name field
      column(2, style = "padding: 0 5px;",
        div(style = "display: flex; align-items: center; gap: 5px;",
          tags$label("Name", style = "margin: 0; font-size: 11px; font-weight: 600; white-space: nowrap;"),
          textInput(ns("name"), label = NULL,
                    placeholder = "First Last",
                    width = "100%")
        )
      ),

      # API key field (password input for security)
      column(3, style = "padding: 0 5px;",
        div(style = "display: flex; align-items: center; gap: 5px;",
          tags$label("API Key", style = "margin: 0; font-size: 11px; font-weight: 600; white-space: nowrap;",
                     title = "Your BOLD API key from your user profile. Can also be set via BOLD_API_KEY env var or .bold_api_key file."),
          passwordInput(ns("bold_api_key"), label = NULL,
                        placeholder = "Not saved by app",
                        width = "100%")
        )
      ),

      # Buttons
      column(2, style = "padding: 0 5px;",
        div(style = "display: flex; align-items: center; gap: 4px; padding-top: 0px;",
          actionButton(ns("save"),
                       "Save",
                       class = "btn-primary btn-sm",
                       icon = icon("save")),
          actionButton(ns("use_shared_key"),
                       "Shared key",
                       class = "btn-default btn-sm",
                       icon = icon("key"),
                       title = "Use a pre-configured shared API key",
                       style = "display: none;")
        )
      ),

      # Validation messages (compact inline)
      column(2, style = "padding: 0 5px;",
        uiOutput(ns("validation_message"))
      )
    )
  )
}
