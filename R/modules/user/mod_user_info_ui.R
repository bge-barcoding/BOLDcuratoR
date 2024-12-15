# User Information UI Module
# R/modules/user/mod_user_info_ui.R

#' UI Module for User Information Management
#' @param id The module ID
#' @export
mod_user_info_ui <- function(id) {
  ns <- NS(id)

  div(class = "user-info-container",
      box(
        title = "User Information",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,

        textInput(ns("email"),
                  "Email:",
                  placeholder = "Enter your email address"),

        textInput(ns("name"),
                  "Name:",
                  placeholder = "Enter your full name"),

        textInput(ns("orcid"),
                  "ORCID:",
                  placeholder = "0000-0000-0000-0000"),

        div(
          class = "button-group",
          actionButton(ns("save"),
                       "Save Information",
                       class = "btn-primary",
                       icon = icon("save")),
          actionButton(ns("clear"),
                       "Clear Information",
                       class = "btn-warning",
                       icon = icon("eraser"))
        ),

        uiOutput(ns("validation_message"))
      ))
}
