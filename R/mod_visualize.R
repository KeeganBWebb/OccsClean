#' Visualize step UI (summary graphics)
#' @param id Module id.
#' @noRd
mod_visualize_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .oc-decision-chart {
        width: 100%;
        max-width: 960px;
        margin: 0.5rem auto 0 auto;
      }
      .oc-decision-chart .shiny-plot-output {
        width: 100% !important;
      }
    ")),
    shiny::h3("Visualize"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Summary graphics for review decisions."
    ),
    shiny::h4("Decisions by flag category"),
    shiny::p(
      "For each check with findings (not counting manually flagged records), compare",
      "Passed, In Review, and Failed counts. Counts are findings, not unique",
      "occurrences, meaning one record can appear under several checks."
    ),
    shiny::div(
      class = "oc-decision-chart",
      shiny::plotOutput(ns("decision_bars"), height = "720px")
    ),
    shiny::br(),
    shiny::tableOutput(ns("decision_table")),
    shiny::br(),
    shiny::downloadButton(ns("dl_decision_bars"), "Download chart PNG")
  )
}

#' Visualize step server (summary graphics)
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_visualize_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(workflow_warning_ui())
      }
      if (length(s$get_assessment()) < 1) {
        return(shiny::div(
          class = "alert alert-warning",
          role = "alert",
          "No assessment results yet. Run checks under Assess, then decide in Review to build graphics."
        ))
      }
      NULL
    })

    decision_summary <- shiny::reactive({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data() || length(s$get_assessment()) < 1) {
        return(build_flag_decision_summary(NULL))
      }
      findings <- findings_with_decisions(
        s$get_findings_table(),
        s$get_decisions()
      )
      build_flag_decision_summary(findings)
    })

    output$decision_bars <- shiny::renderPlot({
      plot_flag_decision_bars(decision_summary())
    }, res = 96, alt = "Grouped bar chart of review decisions by flag category")

    output$decision_table <- shiny::renderTable({
      summary <- decision_summary()
      shiny::req(nrow(summary) > 0)
      wide <- tidyr_pivot_decisions(summary)
      wide
    })

    output$dl_decision_bars <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "occsclean_flag_decisions_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".png"
        )
      },
      content = function(file) {
        s <- app_state$session
        shiny::req(s$has_data())
        summary <- decision_summary()
        shiny::req(nrow(summary) > 0)
        n_cat <- length(unique(as.character(summary$label)))
        ggplot2::ggsave(
          filename = file,
          plot = plot_flag_decision_bars(summary),
          width = if (n_cat >= 8L) 11 else 9,
          height = 7.5,
          dpi = 150,
          bg = "white"
        )
      }
    )
  })
}

#' Wide table of Passed / In Review / Failed by category
#' @noRd
tidyr_pivot_decisions <- function(summary) {
  labels <- levels(summary$label)
  if (is.null(labels) || length(labels) < 1) {
    labels <- unique(as.character(summary$label))
  }
  rows <- lapply(labels, function(lab) {
    hit <- summary[as.character(summary$label) == lab, , drop = FALSE]
    data.frame(
      Category = lab,
      Passed = sum(hit$n[hit$status == "Passed"]),
      `In Review` = sum(hit$n[hit$status == "In Review"]),
      Failed = sum(hit$n[hit$status == "Failed"]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}
