#' Launch the OccsClean Shiny application
#'
#' @examples
#' if (interactive()) {
#'   OccsClean::run_app()
#' }
#' @export
run_app <- function(launch.browser = TRUE, stop_on_close = TRUE) {
  max_bytes <- 50 * 1024^2
  current <- getOption("shiny.maxRequestSize")
  if (is.null(current) || !is.finite(current) || current < max_bytes) {
    options(shiny.maxRequestSize = max_bytes)
  }

  server_fn <- function(input, output, session) {
    if (isTRUE(stop_on_close)) {
      session$onSessionEnded(shiny::stopApp)
    }
    app_server(input, output, session)
  }

  app <- shiny::shinyApp(ui = app_ui(), server = server_fn)
  if (isTRUE(launch.browser)) {
    shiny::runApp(app, launch.browser = TRUE)
  }
  invisible(app)
}
