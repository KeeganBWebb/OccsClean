#' Packages OccsClean uses
#' @export
occsclean_used_packages <- function() {
  tibble::tibble(
    package = c(
      "CoordinateCleaner",
      "sf",
      "countrycode",
      "leaflet",
      "ggplot2",
      "shiny",
      "bslib",
      "DT",
      "htmlwidgets",
      "htmltools",
      "dplyr",
      "tibble",
      "readr",
      "jsonlite",
      "R6",
      "rlang"
    ),
    role = c(
      "Quality checks",
      "Quality checks / Mapping",
      "Quality checks",
      "Mapping",
      "Visualize",
      "App",
      "App",
      "Review",
      "Mapping",
      "App",
      "Engine",
      "Engine",
      "Import / Export",
      "Assess settings",
      "Engine",
      "Engine"
    ),
    used_for = c(
      "Suspicious-coordinate flags (sea, land, centroids, capitals, institutions, GBIF headquarters, equal lon/lat, country mismatch)",
      "Study-area polygons, geometry repair, nearest-distance outside-area check, map overlays",
      "Country name / ISO code handling for country-coordinate checks",
      "Interactive occurrence map and basemap tiles",
      "Decision summary charts",
      "Guided Shiny workflow UI and reactivity",
      "Bootstrap 5 layout and navbar",
      "Review findings tables",
      "Leaflet map widgets",
      "Safe HTML in map popups and UI helpers",
      "Tabular joins, grouping, and summaries",
      "Tibble data frames throughout the engine",
      "CSV import and export",
      "Assess settings save / reload (JSON)",
      "Session and decision registry objects",
      "Consistent error messages and tidy evaluation helpers"
    )
  )
}

#' Used packages with installed versions
#' @export
occsclean_package_versions <- function() {
  pkgs <- occsclean_used_packages()
  pkgs$version <- vapply(
    pkgs$package,
    function(pkg) {
      tryCatch(
        as.character(utils::packageVersion(pkg)),
        error = function(e) "(not installed)"
      )
    },
    character(1)
  )
  pkgs[c("package", "version", "role", "used_for")]
}

#' Citation text for packages used in an assessment
#'
#' @param packages Character vector of package names.
#' @export
occsclean_package_citations <- function(
    packages = c("CoordinateCleaner", "sf", "countrycode", "leaflet", "ggplot2")
) {
  packages <- as.character(packages)
  packages <- packages[nzchar(packages)]
  if (length(packages) < 1) {
    return("No packages selected for citation text.")
  }

  blocks <- lapply(packages, function(pkg) {
    cite <- tryCatch(
      utils::capture.output(print(utils::citation(pkg), style = "text")),
      error = function(e) {
        paste0("(Could not retrieve citation for ", pkg, ": ", conditionMessage(e), ")")
      }
    )
    paste0(
      "── ", pkg, " ──\n",
      paste(cite, collapse = "\n")
    )
  })

  paste(
    c(
      "Suggested citations for key backend packages used by OccsClean.",
      "Also credit OccsClean itself and any data providers (e.g. GBIF).",
      "",
      blocks
    ),
    collapse = "\n\n"
  )
}
