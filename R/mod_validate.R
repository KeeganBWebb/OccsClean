#' Validate step UI
#' @param id Module id.
#' @noRd
mod_validate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Validate"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Verifies that the imported file has the structure OccsClean needs for",
      "downstream cleaning by looking for expected fields (coordinates, dates,",
      "species names, etc.)."
    ),
    shiny::verbatimTextOutput(ns("report"))
  )
}

#' Validate step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_validate_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      if (isTRUE(app_state$session$has_data())) {
        return(NULL)
      }
      workflow_warning_ui("Validate")
    })

    output$report <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded. Import a file first.")
      }
      validation <- s$get_validation()
      if (is.null(validation)) {
        return("Validation has not run yet. Import a file to validate.")
      }
      format_validation_report(validation)
    })
  })
}
