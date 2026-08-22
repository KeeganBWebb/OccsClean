#' Warning banner when a workflow step is opened too early
#' @noRd
workflow_warning_ui <- function() {
  shiny::div(
    class = "alert alert-warning",
    role = "alert",
    "No occurrence data is loaded yet. Import a file"
  )
}
