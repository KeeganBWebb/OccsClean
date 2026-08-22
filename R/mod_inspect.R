#' Inspect step UI
#' @param id Module id.
#' @noRd
mod_inspect_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .oc-inspect-status {
        width: 100%;
        padding: 0.75rem 1rem;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        margin-bottom: 1rem;
      }
      .oc-inspect-status p {
        margin: 0 0 0.35rem 0;
      }
      .oc-inspect-status-label {
        margin: 0.5rem 0 0.35rem 0;
      }
      .oc-inspect-column-names {
        display: flex;
        flex-wrap: wrap;
        gap: 0.35rem;
      }
      .oc-inspect-column-name {
        display: inline-block;
        padding: 0.1rem 0.45rem;
        border-radius: 0.2rem;
        background: #e9ecef;
        font-family: SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-size: 0.88rem;
        line-height: 1.35;
        word-break: break-word;
      }
      .oc-missing-chart {
        width: 100%;
        max-width: 960px;
        margin: 0.5rem 0 0.75rem 0;
      }
      .oc-missing-chart .shiny-plot-output {
        width: 100% !important;
      }
      .oc-missing-table-wrap {
        display: flex;
        flex-wrap: nowrap;
        gap: 1rem;
        align-items: flex-start;
        width: 100%;
        margin-top: 0.5rem;
        margin-bottom: 1.5rem;
      }
      .oc-missing-table-chunk {
        flex: 1 1 0;
        min-width: 0;
      }
      .oc-missing-table-single {
        margin-top: 0.5rem;
        margin-bottom: 1.5rem;
      }
      .oc-missing-table-wrap .table,
      .oc-missing-table-single .table {
        margin-bottom: 0;
      }
    ")),
    shiny::h3("Inspect"),
    shiny::uiOutput(ns("warning")),
    shiny::p("Overview of records, columns, and missing data."),
    shiny::uiOutput(ns("status")),
    shiny::h4("Missing data"),
    shiny::p(
      "Columns with any missing values, sorted from most to least incomplete.",
      "The table below lists every column."
    ),
    shiny::uiOutput(ns("missing_chart")),
    shiny::h5("All columns"),
    shiny::uiOutput(ns("missing"))
  )
}

#' Inspect step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_inspect_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      if (isTRUE(app_state$session$has_data())) {
        return(NULL)
      }
      workflow_warning_ui()
    })

    output$status <- shiny::renderUI({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(shiny::p("No data loaded. Import a file first."))
      }
      inspect_status_ui(s$get_occ_raw())
    })

    missing_summary <- shiny::reactive({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(summarize_missing_columns(NULL))
      }
      summarize_missing_columns(s$get_occ_raw())
    })

    output$missing_chart <- shiny::renderUI({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(NULL)
      }
      sm <- missing_summary()
      n_bars <- sum(sm$n_missing > 0)
      if (n_bars < 1L) {
        return(NULL)
      }
      h <- missing_percent_plot_height(n_bars)
      shiny::div(
        class = "oc-missing-chart",
        shiny::plotOutput(
          session$ns("missing_bars"),
          height = paste0(h, "px")
        )
      )
    })

    output$missing_bars <- shiny::renderPlot({
      invisible(app_state$rev)
      plot_missing_percent_bars(missing_summary())
    }, res = 96, alt = "Horizontal bar chart of percent missing by column")

    output$missing <- shiny::renderUI({
      invisible(app_state$rev)
      s <- app_state$session
      if (!isTRUE(s$has_data())) {
        return(NULL)
      }
      missing_summary_layout_ui(missing_summary())
    })
  })
}
