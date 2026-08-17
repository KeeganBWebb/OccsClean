#' Warning banner when a workflow step is opened too early
#' @param step_label Short step name.
#' @noRd
workflow_warning_ui <- function(step_label = NULL) {
  lead <- if (!is.null(step_label) && nzchar(step_label)) {
    paste0(step_label, ": ")
  } else {
    ""
  }

  body <- if (identical(as.character(step_label), "Validate")) {
    "No occurrence data is loaded yet. Import a file on Import."
  } else {
    paste0(
      "No occurrence data is loaded yet. Import a file on Import, then confirm ",
      "structure on Validate before conducting this step."
    )
  }

  shiny::div(
    class = "alert alert-warning",
    role = "alert",
    paste0(lead, body)
  )
}
