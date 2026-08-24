#' About step UI
#' @param id Module id.
#' @noRd
mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("About OccsClean"),
    shiny::p(
      "OccsClean is a data cleaning tool designed to help researchers with",
      "little coding experience clean their species occurrence data. It",
      "integrates established R packages behind a guided workflow and common",
      "cleaning steps. You should exercise discretion for which checks you",
      "utilize and their respective parameters as the quality of your cleaning",
      "heavily depends on specific factors tied to your dataset, study species,",
      "and study question."
    ),
    shiny::p(
      class = "text-muted",
      "When publishing analyses using data cleaned with OccsClean, please cite",
      "OccsClean and the backend packages that performed the checks or maps you",
      "relied on. You can export a list of recommended citations on the Export",
      "tab."
    ),
    shiny::verbatimTextOutput(ns("version")),
    shiny::hr(),
    shiny::h4("Packages used"),
    shiny::p(
      "Installed versions on this machine. Roles describe how OccsClean uses each package."
    ),
    shiny::tableOutput(ns("packages")),
    shiny::hr(),
    shiny::h4("Suggested citations (key backend packages)"),
    shiny::verbatimTextOutput(ns("citations"))
  )
}

#' About step server
#' @param id Module id.
#' @noRd
mod_about_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$version <- shiny::renderText({
      ver <- tryCatch(
        as.character(utils::packageVersion("OccsClean")),
        error = function(e) "0.3.0"
      )
      paste0("OccsClean version: ", ver)
    })

    output$packages <- shiny::renderTable({
      pkgs <- occsclean_package_versions()
      data.frame(
        Package = pkgs$package,
        Version = pkgs$version,
        Role = pkgs$role,
        `Used for` = pkgs$used_for,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    output$citations <- shiny::renderText({
      occsclean_package_citations()
    })
  })
}
