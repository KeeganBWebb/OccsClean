#' Import step UI
#' @param id Module id.
#' @noRd
mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Import"),
    shiny::p("Load occurrence data from a CSV or tab-delimited file (e.g. GBIF downloads)."),
    shiny::fileInput(
      ns("file"),
      label = "Occurrence file",
      accept = c(
        ".csv",
        ".txt",
        ".tsv",
        "text/csv",
        "text/tab-separated-values",
        "text/plain"
      )
    ),
    shiny::verbatimTextOutput(ns("status"))
  )
}

#' Import step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_import_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$file, {
      path <- input$file$datapath
      shiny::req(path)
      tryCatch(
        {
          app_state$session$import_csv(
            path = path,
            source_name = input$file$name
          )
          bump_app_state(app_state)
        },
        error = function(e) {
          shiny::showNotification(
            paste("Import failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    })

    output$status <- shiny::renderText({
      rev <- app_state$rev
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded yet.")
      }
      meta <- s$get_meta()
      paste0(
        "File: ", meta$source_name, "\n",
        "Rows: ", meta$n_rows, "\n",
        "Columns: ", meta$n_cols, "\n",
        "Delimiter: ",
        if (is.null(meta$delimiter)) "unknown" else meta$delimiter, "\n",
        "Imported: ", format(meta$imported_at, usetz = TRUE)
      )
    })
  })
}
