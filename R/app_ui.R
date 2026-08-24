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
      .oc-review-batch-bar {
        display: flex;
        flex-wrap: wrap;
        align-items: flex-end;
        gap: 0.35rem 0.75rem;
      }
      .oc-review-batch-check {
        flex: 0 1 15rem;
        min-width: 12.5rem;
        max-width: 18rem;
      }
      .oc-review-batch-check .shiny-input-container {
        margin-bottom: 0;
      }
      .oc-review-batch-check select {
        padding-right: 1.75rem;
      }
      .oc-review-batch-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.35rem;
        align-items: center;
        margin-left: 0.25rem;
      }
      .oc-review-batch-actions .btn {
        white-space: normal;
        line-height: 1.2;
        text-align: center;
        max-width: 14rem;
      }
      .oc-review-batch-spacer {
        flex: 1 1 auto;
        min-width: 0.25rem;
      }
      .oc-review-root {
        display: flex;
        flex-direction: column;
        gap: 0.35rem;
        height: calc(100dvh - 7rem);
        min-height: 28rem;
        overflow: hidden;
      }
      .oc-review-intro {
        flex: 0 0 auto;
      }
      .oc-review-intro p {
        margin-bottom: 0.35rem;
        font-size: 0.95rem;
      }
      .oc-review-table-toolbar {
        flex: 0 0 auto;
        margin-bottom: 0.35rem;
      }
      .oc-review-table-wrap .dataTables_scrollBody {
        overflow-y: auto !important;
      }
      .oc-review-workspace {
        display: flex;
        gap: 0.75rem;
        align-items: stretch;
        width: 100%;
        flex: 1 1 0;
        min-height: 0;
        overflow: hidden;
      }
      .oc-review-table-pane {
        flex: 0 0 50%;
        min-width: 0;
        height: 100%;
        min-height: 0;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      .oc-review-table-pane .oc-review-table-wrap {
        flex: 1 1 0;
        min-height: 0;
        overflow: hidden;
      }
      .oc-review-table-pane .dataTables_wrapper {
        height: 100%;
      }
      .oc-review-table-pane .dataTables_info,
      .oc-review-table-pane .dataTables_paginate {
        margin-top: 0.25rem;
      }
      .oc-review-table-wrap table.dataTable tbody tr.selected > * {
        background-color: #0d6efd !important;
        color: #fff !important;
        box-shadow: inset 0 0 0 9999px #0d6efd;
      }
      .oc-review-map-pane {
        flex: 1 1 0;
        min-width: 0;
        height: 100%;
        min-height: 0;
        display: flex;
        flex-direction: column;
        gap: 0.35rem;
        overflow: hidden;
      }
      .oc-review-map {
        flex: 1 1 0;
        width: 100%;
        min-height: 14rem;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        overflow: hidden;
      }
      .oc-review-map .leaflet,
      .oc-review-map .leaflet-container {
        height: 100% !important;
        width: 100% !important;
        background: #f0f0f0;
      }
      .oc-review-selected {
        padding: 0.55rem 0.75rem;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        background: #f8f9fa;
        flex: 0 1 42%;
        min-height: 9rem;
        max-height: 42%;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      .oc-review-selected-actions {
        flex: 0 0 auto;
        margin-top: 0.35rem;
      }
      .oc-review-selected-help {
        flex: 0 0 auto;
      }
      .oc-review-record-view {
        flex: 1 1 0;
        min-height: 0;
        overflow: auto;
        margin-top: 0.35rem;
      }
      .oc-review-map-pane .checkbox {
        margin: 0;
        flex: 0 0 auto;
      }
      .oc-review-status-panel {
        flex: 0 0 auto;
        margin: 0;
        padding: 0.35rem 0.65rem;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        background: #f8f9fa;
        white-space: pre-wrap;
        line-height: 1.3;
        font-size: 0.875rem;
      }
      .oc-review-status-panel .shiny-text-output,
      .oc-review-status-panel pre {
        margin: 0;
        padding: 0;
        border: 0;
        background: transparent;
        font-size: inherit;
        line-height: inherit;
        white-space: pre-wrap;
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
