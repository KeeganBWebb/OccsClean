#' Export step UI
#' @param id Module id.
#' @noRd
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Export"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Download the original CSV, the cleaned CSV,",
      "flagged occurrences (with decisions),",
      "and a processing log of which checks were run.",
      "You can also download a text file that holds the recommended citations",
      "for OccsClean and the utilized packages based on the checks you selected",
      "for data cleaning."
    ),
    shiny::verbatimTextOutput(ns("status")),
    shiny::downloadButton(ns("dl_original"), "Download original CSV"),
    shiny::downloadButton(ns("dl_cleaned"), "Download cleaned CSV"),
    shiny::downloadButton(ns("dl_flagged"), "Download flagged occurrences CSV"),
    shiny::downloadButton(ns("dl_log"), "Download processing log (TXT)"),
    shiny::downloadButton(ns("dl_citations"), "Download recommended citations (TXT)")
  )
}

#' Export step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_export_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      if (isTRUE(app_state$session$has_data())) {
        return(NULL)
      }
      workflow_warning_ui("Export")
    })

    output$status <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("Nothing to export yet.")
      }
      counts <- export_counts(
        s$get_occ_raw(),
        s$get_decisions(),
        findings = s$get_findings_table()
      )
      flagged <- export_flagged_occurrences(s)
      paste0(
        "Original records: ", counts$n_original, "\n",
        "Excluded from cleaned (failed or still unreviewed flags): ",
        counts$n_removed, "\n",
        "Cleaned records: ", counts$n_cleaned, "\n",
        "Flagged finding rows: ", nrow(flagged), "\n",
        "Checks in current assessment: ", length(s$get_assessment())
      )
    })

    output$dl_original <- shiny::downloadHandler(
      filename = function() {
        paste0("occsclean_original_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        s <- app_state$session
        shiny::req(s$has_data())
        readr::write_csv(strip_occsclean_columns(s$get_occ_raw()), file)
      }
    )

    output$dl_cleaned <- shiny::downloadHandler(
      filename = function() {
        paste0("occsclean_cleaned_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        s <- app_state$session
        shiny::req(s$has_data())
        cleaned <- build_cleaned_occurrences(
          s$get_occ_raw(),
          s$get_decisions(),
          findings = s$get_findings_table()
        )
        readr::write_csv(strip_occsclean_columns(cleaned), file)
      }
    )

    output$dl_flagged <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "occsclean_flagged_occurrences_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        s <- app_state$session
        shiny::req(s$has_data())
        readr::write_csv(
          strip_occsclean_columns(export_flagged_occurrences(s)),
          file
        )
      }
    )

    output$dl_log <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "occsclean_processing_log_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".txt"
        )
      },
      content = function(file) {
        s <- app_state$session
        shiny::req(s$has_data())
        writeLines(build_processing_log(s), file, useBytes = TRUE)
      }
    )

    output$dl_citations <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "occsclean_recommended_citations_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".txt"
        )
      },
      content = function(file) {
        s <- app_state$session
        shiny::req(s$has_data())
        writeLines(build_session_citations(s), file, useBytes = TRUE)
      }
    )
  })
}

#' All assessment flags with decisions, slimmed for export
#' @param session An [OccSession].
#' @noRd
export_flagged_occurrences <- function(session) {
  flagged <- findings_with_decisions(
    session$get_findings_table(),
    session$get_decisions()
  )
  slim_flag_columns(flagged, for_export = TRUE)
}

#' Drop redundant flag columns from review or export tables
#' @param df Findings tibble.
#' @param for_export If `TRUE`, also drop internal `check_id`.
#' @noRd
slim_flag_columns <- function(df, for_export = FALSE) {
  drop <- c(
    "evidence",
    "recommended_action",
    "severity",
    "category",
    "species"
  )
  if (isTRUE(for_export)) {
    drop <- c(drop, "check_id", "occsclean_id")
  }
  out <- df[setdiff(names(df), drop)]
  if ("check_label" %in% names(out)) {
    names(out)[names(out) == "check_label"] <- "check"
  }
  out
}
