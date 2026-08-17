#' Launch the OccsClean Shiny application
#'
#' @examples
#' if (interactive()) {
#'   OccsClean::run_app()
#' }
#' @export
run_app <- function(...) {
  max_bytes <- 50 * 1024^2
  current <- getOption("shiny.maxRequestSize")
  if (is.null(current) || !is.finite(current) || current < max_bytes) {
    options(shiny.maxRequestSize = max_bytes)
  }

  shiny::shinyApp(ui = app_ui(), server = app_server)
}
