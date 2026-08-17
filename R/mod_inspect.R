#' Inspect step UI
#' @param id Module id.
#' @noRd
mod_inspect_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .oc-missing-chart {
        width: 100%;
        max-width: 960px;
        margin: 0.5rem 0 0.75rem 0;
      }
      .oc-missing-chart .shiny-plot-output {
        width: 100% !important;
      }
      .oc-missing-table {
        margin-top: 0.5rem;
        margin-bottom: 1.5rem;
      }
    ")),
    shiny::h3("Inspect"),
    shiny::uiOutput(ns("warning")),
    shiny::p("Overview of records, columns, and missing data."),
    shiny::verbatimTextOutput(ns("status")),
    shiny::h4("Missing data"),
    shiny::p(
      "Columns with any missing values, sorted from most to least incomplete.",
      "The table below lists every column."
    ),
    shiny::uiOutput(ns("missing_chart")),
    shiny::h5("All columns"),
    shiny::div(
      class = "oc-missing-table",
      shiny::tableOutput(ns("missing"))
    )
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
      workflow_warning_ui("Inspect")
    })

    output$status <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded. Import a file first.")
      }
      occ <- s$get_occ_raw()
      paste0(
        "Records: ", nrow(occ), "\n",
        "Columns: ", ncol(occ), "\n",
        "Column names: ", paste(names(occ), collapse = ", ")
      )
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

    output$missing <- shiny::renderTable({
      invisible(app_state$rev)
      s <- app_state$session
      if (!isTRUE(s$has_data())) {
        return(NULL)
      }
      missing_summary()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
