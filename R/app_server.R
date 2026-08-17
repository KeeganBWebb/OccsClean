#' OccsClean application server
#' @param input,output,session Standard Shiny server arguments.
#' @noRd
app_server <- function(input, output, session) {
  app_state <- shiny::reactiveValues(
    session = OccSession$new(),
    rev = 0L
  )

  mod_import_server("import", app_state = app_state)
  mod_validate_server("validate", app_state = app_state)
  mod_inspect_server("inspect", app_state = app_state)
  mod_assess_server("assess", app_state = app_state)
  mod_review_server("review", app_state = app_state)
  mod_mapping_server("mapping", app_state = app_state)
  mod_visualize_server("visualize", app_state = app_state)
  mod_export_server("export", app_state = app_state)
  mod_about_server("about")
}
