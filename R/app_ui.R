#' Path to packaged www assets
#' @noRd
occsclean_www_path <- function() {
  local <- file.path("inst", "www")
  if (dir.exists(local)) {
    local <- normalizePath(local, winslash = "/", mustWork = FALSE)
  } else {
    local <- ""
  }

  installed <- system.file("www", package = "OccsClean", mustWork = FALSE)
  if (!nzchar(installed) || !dir.exists(installed)) {
    installed <- ""
  }

  logo <- "occsclean-logo.png"
  if (nzchar(local) && file.exists(file.path(local, logo))) {
    return(local)
  }
  if (nzchar(installed) && file.exists(file.path(installed, logo))) {
    return(installed)
  }
  if (nzchar(local)) {
    return(local)
  }
  installed
}

#' OccsClean application UI
#' @noRd
app_ui <- function() {
  www <- occsclean_www_path()
  if (nzchar(www)) {
    shiny::addResourcePath("occsclean-assets", www)
  }

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .navbar .navbar-brand {
        display: inline-flex;
        align-items: center;
        gap: 0.3rem;
        margin-right: 0.4rem !important;
        padding-top: 0.35rem;
        padding-bottom: 0.35rem;
        font-size: 1.35rem;
      }
      .navbar .navbar-brand img {
        height: 72px !important;
        width: auto !important;
        max-height: none !important;
        margin-right: 0 !important;
      }
      .navbar .navbar-nav {
        margin-left: 0.15rem;
      }
      .oc-review-type-actions {
        display: flex;
        flex-direction: column;
        gap: 0.45rem;
        width: 100%;
      }
      .oc-review-type-actions .oc-review-type-btn {
        width: 100%;
        margin: 0;
        white-space: normal;
        line-height: 1.25;
        text-align: center;
      }
      .oc-review-type-btn {
        width: 100%;
        white-space: normal;
        line-height: 1.25;
        text-align: center;
      }
      .oc-review-section-label {
        font-size: 1rem;
        font-weight: 600;
        margin: 0 0 0.5rem 0;
      }
      .oc-review-batch-slot {
        min-height: 6.75rem;
      }
      .oc-review-eq-bar {
        display: flex;
        width: 100%;
        max-width: 36rem;
      }
      .oc-review-eq-bar > .btn {
        flex: 1 1 0;
        width: auto;
        white-space: normal;
        line-height: 1.25;
        text-align: center;
      }
    ")),
    bslib::page_navbar(
      title = shiny::tagList(
        shiny::tags$img(
          src = "occsclean-assets/occsclean-logo.png",
          alt = "OccsClean"
        ),
        shiny::span("OccsClean")
      ),
      theme = bslib::bs_theme(version = 5),
      navbar_options = bslib::navbar_options(
        bg = "#ADD8E6",
        theme = "light"
      ),
      id = "main_nav",
      bslib::nav_panel(
        title = "Import",
        mod_import_ui("import")
      ),
      bslib::nav_panel(
        title = "Validate",
        mod_validate_ui("validate")
      ),
      bslib::nav_panel(
        title = "Inspect",
        mod_inspect_ui("inspect")
      ),
      bslib::nav_panel(
        title = "Assess",
        mod_assess_ui("assess")
      ),
      bslib::nav_panel(
        title = "Review",
        mod_review_ui("review")
      ),
      bslib::nav_panel(
        title = "Mapping",
        mod_mapping_ui("mapping")
      ),
      bslib::nav_panel(
        title = "Visualize",
        mod_visualize_ui("visualize")
      ),
      bslib::nav_panel(
        title = "Export",
        mod_export_ui("export")
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "About",
        mod_about_ui("about")
      )
    )
  )
}
