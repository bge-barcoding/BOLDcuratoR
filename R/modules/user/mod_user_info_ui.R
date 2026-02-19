# R/modules/user/mod_user_info_ui.R

#' UI Module for User Information Management
#' @param id The module ID
#' @export
mod_user_info_ui <- function(id) {
  ns <- NS(id)

  # Compact CSS for user info panel
  tags$head(
    tags$style(HTML("
      #user_info .form-group {
        margin-bottom: 2px !important;
        margin-top: 2px !important;
      }

      #user_info .shiny-input-container {
        margin-bottom: 2px !important;
        margin-top: 2px !important;
        padding-bottom: 0px !important;
        padding-top: 0px !important;
      }

      #user_info .form-control {
        margin-bottom: 0px !important;
        margin-top: 0px !important;
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        height: 28px !important;
        font-size: 12px !important;
      }

      #user_info .box-body {
        padding: 5px !important;
      }

      #user_info .box-header {
        padding-bottom: 2px !important;
      }

      #user_info .control-label {
        margin-bottom: 0px !important;
        font-size: 11px !important;
      }
    "))
  )

  div(class = "user-info-container",
      box(
        title = "User Information",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,

        div(
          class = "tight-form",
          style = "padding: 0px; color: black;",

          textInput(ns("email"),
                    "EMAIL",
                    placeholder = "Enter your email address"),

          textInput(ns("name"),
                    "NAME",
                    placeholder = "Enter name as First Last"),

          # Updated API key input with tooltip
          div(
            title = "Your api key can be found in your BOLD user profile",
            textInput(ns("bold_api_key"),
                      "BOLD API KEY",
                      placeholder = "Not saved by app")
          ),

          div(
            style = "margin-top: 4px;",
            actionButton(ns("save"),
                         "Save Information",
                         class = "btn-primary btn-sm",
                         icon = icon("save")),
            actionButton(ns("use_shared_key"),
                         "Use shared key",
                         class = "btn-default btn-sm",
                         icon = icon("key"),
                         title = "Use a pre-configured shared API key for testing",
                         style = "display: none;")
          ),

          uiOutput(ns("validation_message")),

          div(
            class = "info-note",
            style = "margin-top: 1px; font-size: 1.0em; color: black;",
            tags$p(
              icon("info-circle"),
              "We collect this information to enable authorship in publications.
              Your data is handled as per our ",
              tags$a(
                href = "https://www.nhm.ac.uk/about-us/privacy-notice.html",
                target = "_blank",
                style = "color: black; text-decoration: none;",
                "privacy notice"
              ),
              "."
            )
          )
        )
      )
  )
}
