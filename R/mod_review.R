#' Review step UI
#' @param id Module id.
#' @noRd
mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Review Flags"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Review flagged occurrences and mark them as pass or fail.",
      "Fail rejects the occurrence record and will exclude it from the cleaned",
      "CSV file in export. Pass accepts the occurrence. You can choose to fail",
      "all occurrences in review but this should only be utilized if you are",
      "confident that your filters do not contain erroneous flags. Any remaining",
      "flagged occurrences that are not reviewed are also omitted from the",
      "cleaned CSV file. You will have an opportunity to Pass/Fail flagged",
      "occurrences on the Mapping section."
    ),
    shiny::verbatimTextOutput(ns("status")),
    shiny::tabsetPanel(
      id = ns("review_tabs"),
      shiny::tabPanel(
        title = "Review",
        value = "in_review",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            7,
            shiny::tags$h5(class = "oc-review-section-label", "Manual Review"),
            shiny::div(
              class = "btn-group oc-review-eq-bar",
              role = "group",
              shiny::actionButton(
                ns("mark_delete"),
                "Fail selected",
                class = "btn-danger"
              ),
              shiny::actionButton(
                ns("mark_keep"),
                "Pass selected",
                class = "btn-success"
              )
            ),
            shiny::br(),
            shiny::actionButton(
              ns("fail_all_review"),
              "Fail all occurrences in Review…",
              class = "btn-outline-danger oc-review-type-btn",
              width = "100%"
            )
          ),
          shiny::column(
            5,
            shiny::selectInput(
              ns("finding_type"),
              label = "Finding code",
              choices = c("Choose a finding code" = ""),
              selected = ""
            ),
            shiny::tags$h5(class = "oc-review-section-label", "Batch Review"),
            shiny::uiOutput(ns("batch_actions_review"))
          )
        ),
        shiny::br(),
        DT::DTOutput(ns("tbl_in_review"))
      ),
      shiny::tabPanel(
        title = "Pass",
        value = "keep",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            7,
            shiny::tags$h5(class = "oc-review-section-label", "Manual Review"),
            shiny::div(
              class = "btn-group oc-review-eq-bar",
              role = "group",
              shiny::actionButton(
                ns("keep_to_delete"),
                "Fail selected",
                class = "btn-danger"
              ),
              shiny::actionButton(
                ns("keep_to_review"),
                "Return selected to Review",
                class = "btn-secondary"
              )
            )
          ),
          shiny::column(
            5,
            shiny::selectInput(
              ns("finding_type_keep"),
              label = "Finding code",
              choices = c("Choose a finding code" = ""),
              selected = ""
            ),
            shiny::tags$h5(class = "oc-review-section-label", "Batch Review"),
            shiny::uiOutput(ns("batch_actions_keep"))
          )
        ),
        shiny::br(),
        DT::DTOutput(ns("tbl_keep"))
      ),
      shiny::tabPanel(
        title = "Fail",
        value = "remove",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            7,
            shiny::tags$h5(class = "oc-review-section-label", "Manual Review"),
            shiny::div(
              class = "btn-group oc-review-eq-bar",
              role = "group",
              shiny::actionButton(
                ns("delete_to_keep"),
                "Pass selected",
                class = "btn-success"
              ),
              shiny::actionButton(
                ns("delete_to_review"),
                "Return selected to Review",
                class = "btn-secondary"
              )
            )
          ),
          shiny::column(
            5,
            shiny::selectInput(
              ns("finding_type_delete"),
              label = "Finding code",
              choices = c("Choose a finding code" = ""),
              selected = ""
            ),
            shiny::tags$h5(class = "oc-review-section-label", "Batch Review"),
            shiny::uiOutput(ns("batch_actions_delete"))
          )
        ),
        shiny::br(),
        DT::DTOutput(ns("tbl_delete"))
      )
    )
  )
}

#' Review step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_review_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    batch_actions_ui <- function(show, buttons) {
      shiny::div(
        class = "oc-review-batch-slot",
        shiny::div(
          class = "oc-review-type-actions",
          style = if (isTRUE(show)) {
            NULL
          } else {
            "visibility: hidden; pointer-events: none;"
          },
          buttons
        )
      )
    }

    output$batch_actions_review <- shiny::renderUI({
      show <- nzchar(as.character(input$finding_type %||% ""))
      batch_actions_ui(
        show,
        list(
          shiny::actionButton(
            session$ns("keep_by_type"),
            "Pass all with only this finding",
            class = "btn-success oc-review-type-btn"
          ),
          shiny::actionButton(
            session$ns("delete_by_type"),
            "Fail all that have this finding",
            class = "btn-danger oc-review-type-btn"
          )
        )
      )
    })

    output$batch_actions_keep <- shiny::renderUI({
      show <- nzchar(as.character(input$finding_type_keep %||% ""))
      batch_actions_ui(
        show,
        list(
          shiny::actionButton(
            session$ns("keep_type_to_review_only"),
            "Return all with only this finding to Review",
            class = "btn-secondary oc-review-type-btn"
          ),
          shiny::actionButton(
            session$ns("keep_type_to_review"),
            "Return all that have this finding to Review",
            class = "btn-secondary oc-review-type-btn"
          )
        )
      )
    })

    output$batch_actions_delete <- shiny::renderUI({
      show <- nzchar(as.character(input$finding_type_delete %||% ""))
      batch_actions_ui(
        show,
        list(
          shiny::actionButton(
            session$ns("delete_type_to_review_only"),
            "Return all with only this finding to Review",
            class = "btn-secondary oc-review-type-btn"
          ),
          shiny::actionButton(
            session$ns("delete_type_to_review"),
            "Return all that have this finding to Review",
            class = "btn-secondary oc-review-type-btn"
          )
        )
      )
    })

    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(workflow_warning_ui("Review"))
      }
      if (length(s$get_assessment()) < 1) {
        return(shiny::div(
          class = "alert alert-warning",
          role = "alert",
          "No assessment results yet. Run checks under Assess first."
        ))
      }
      NULL
    })

    raw_findings <- shiny::reactive({
      invisible(app_state$rev)
      s <- app_state$session
      shiny::req(s$has_data())
      shiny::req(length(s$get_assessment()) > 0)
      s$get_findings_table()
    })

    occurrences <- shiny::reactive({
      prepare_review_occurrence_table(
        findings = raw_findings(),
        decisions = app_state$session$get_decisions()
      )
    })

    subset_status <- function(status) {
      df <- occurrences()
      hit <- as.character(df$review_status) == status
      df[hit, , drop = FALSE]
    }

    in_review <- shiny::reactive(subset_status("review"))
    passed <- shiny::reactive(subset_status("pass"))
    failed <- shiny::reactive(subset_status("fail"))

    update_finding_choices <- function(input_id, occ_df) {
      if (is.null(occ_df) || nrow(occ_df) < 1) {
        shiny::updateSelectInput(
          session,
          input_id,
          choices = c("Choose a finding code" = ""),
          selected = ""
        )
        return(invisible(NULL))
      }
      codes <- finding_codes_for_records(
        raw_findings(),
        as.character(occ_df$occsclean_id)
      )
      shiny::updateSelectInput(
        session,
        input_id,
        choices = c("Choose a finding code" = "", stats::setNames(codes, codes)),
        selected = shiny::isolate(input[[input_id]])
      )
    }

    shiny::observe({
      df <- tryCatch(in_review(), error = function(e) NULL)
      update_finding_choices("finding_type", df)
    })
    shiny::observe({
      df <- tryCatch(passed(), error = function(e) NULL)
      update_finding_choices("finding_type_keep", df)
    })
    shiny::observe({
      df <- tryCatch(failed(), error = function(e) NULL)
      update_finding_choices("finding_type_delete", df)
    })

    output$status <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded. Import a file first.")
      }
      if (length(s$get_assessment()) < 1) {
        return("No assessment cached. Run checks under Assess.")
      }
      ir <- in_review()
      kp <- passed()
      del <- failed()
      n_original <- nrow(s$get_occ_raw())
      n_cleaned <- n_original - nrow(ir) - nrow(del)
      paste0(
        "Occurrences in review: ", nrow(ir), "\n",
        "Occurrences passed: ", nrow(kp), "\n",
        "Occurrences failed: ", nrow(del), "\n",
        "Cleaned export would have: ", n_cleaned, " of ",
        n_original, " records"
      )
    })

    render_occ_dt <- function(df) {
      show <- df
      if ("occsclean_id" %in% names(show)) {
        show$occsclean_id <- NULL
      }
      if ("review_status" %in% names(show)) {
        show$review_status <- NULL
      }
      DT::datatable(
        show,
        selection = "multiple",
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE
        )
      )
    }

    output$tbl_in_review <- DT::renderDT({
      render_occ_dt(in_review())
    })
    output$tbl_keep <- DT::renderDT({
      render_occ_dt(passed())
    })
    output$tbl_delete <- DT::renderDT({
      render_occ_dt(failed())
    })

    selected_occsclean_ids <- function(occ_df, selected_rows) {
      shiny::req(nrow(occ_df) > 0)
      shiny::req(length(selected_rows) > 0)
      as.character(occ_df$occsclean_id[selected_rows])
    }

    occsclean_ids_for_finding_in_tab <- function(occ_df, code, only = FALSE) {
      if (is.null(occ_df) || nrow(occ_df) < 1) {
        return(character())
      }
      if (is.null(code) || !nzchar(as.character(code)[[1]])) {
        return(character())
      }
      tab_ids <- as.character(occ_df$occsclean_id)
      with_code <- if (isTRUE(only)) {
        occsclean_ids_with_only_finding(raw_findings(), code)
      } else {
        occsclean_ids_with_finding(raw_findings(), code)
      }
      intersect(tab_ids, with_code)
    }

    apply_occurrence_action <- function(record_ids, action) {
      ids <- unique(as.character(record_ids))
      ids <- ids[nzchar(ids)]
      shiny::req(length(ids) > 0)
      s <- app_state$session
      findings <- s$get_findings_table()
      n <- length(ids)
      label <- switch(
        action,
        fail = "failed",
        pass = "passed",
        review = "returned to Review",
        action
      )

      shiny::withProgress(
        message = "Updating occurrence decisions",
        detail = paste0(format(n, big.mark = ","), " records"),
        value = 0.2,
        {
          if (identical(action, "fail")) {
            fail_records(s$get_decisions(), ids, findings = findings)
          } else if (identical(action, "pass")) {
            pass_records(s$get_decisions(), ids, findings = findings)
          } else if (identical(action, "review")) {
            return_records_to_review(
              s$get_decisions(),
              ids,
              findings = findings
            )
          }
          shiny::setProgress(value = 0.85, detail = "Refreshing tables")
          s$touch()
          bump_app_state(app_state)
        }
      )

      shiny::showNotification(
        paste0(n, " occurrence(s) ", label, "."),
        type = "message"
      )
    }

    shiny::observeEvent(input$mark_delete, {
      ids <- selected_occsclean_ids(in_review(), input$tbl_in_review_rows_selected)
      apply_occurrence_action(ids, "fail")
    })
    shiny::observeEvent(input$mark_keep, {
      ids <- selected_occsclean_ids(in_review(), input$tbl_in_review_rows_selected)
      apply_occurrence_action(ids, "pass")
    })
    shiny::observeEvent(input$delete_by_type, {
      code <- input$finding_type
      shiny::req(!is.null(code), nzchar(code))
      ids <- occsclean_ids_for_finding_in_tab(
        in_review(),
        code,
        only = FALSE
      )
      if (length(ids) < 1) {
        shiny::showNotification(
          "No occurrences in Review carry that finding.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "fail")
    })
    shiny::observeEvent(input$keep_by_type, {
      code <- input$finding_type
      shiny::req(!is.null(code), nzchar(code))
      ids <- occsclean_ids_for_finding_in_tab(
        in_review(),
        code,
        only = TRUE
      )
      if (length(ids) < 1) {
        shiny::showNotification(
          paste(
            "No occurrences in Review have only that finding.",
            "Records that also have other flags were left in Review."
          ),
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "pass")
    })
    shiny::observeEvent(input$keep_type_to_review_only, {
      code <- input$finding_type_keep
      shiny::req(!is.null(code), nzchar(code))
      ids <- occsclean_ids_for_finding_in_tab(passed(), code, only = TRUE)
      if (length(ids) < 1) {
        shiny::showNotification(
          paste(
            "No passed occurrences have only that finding.",
            "Records that also have other flags were left on Pass."
          ),
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "review")
    })
    shiny::observeEvent(input$keep_type_to_review, {
      code <- input$finding_type_keep
      shiny::req(!is.null(code), nzchar(code))
      ids <- occsclean_ids_for_finding_in_tab(passed(), code, only = FALSE)
      if (length(ids) < 1) {
        shiny::showNotification(
          "No passed occurrences carry that finding.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "review")
    })
    shiny::observeEvent(input$delete_type_to_review_only, {
      code <- input$finding_type_delete
      shiny::req(!is.null(code), nzchar(code))
      ids <- occsclean_ids_for_finding_in_tab(failed(), code, only = TRUE)
      if (length(ids) < 1) {
        shiny::showNotification(
          paste(
            "No failed occurrences have only that finding.",
            "Records that also have other flags were left on Fail."
          ),
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "review")
    })
    shiny::observeEvent(input$delete_type_to_review, {
      code <- input$finding_type_delete
      shiny::req(!is.null(code), nzchar(code))
      ids <- occsclean_ids_for_finding_in_tab(failed(), code, only = FALSE)
      if (length(ids) < 1) {
        shiny::showNotification(
          "No failed occurrences carry that finding.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "review")
    })
    shiny::observeEvent(input$fail_all_review, {
      rows <- tryCatch(in_review(), error = function(e) NULL)
      n <- if (is.null(rows)) 0L else nrow(rows)
      if (n < 1) {
        shiny::showNotification(
          "No occurrences currently in Review.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      shiny::showModal(shiny::modalDialog(
        title = "Fail all occurrences in Review?",
        shiny::p(
          paste0(
            "This will fail ", format(n, big.mark = ","),
            " occurrence(s) still in Review and exclude them from the cleaned export."
          )
        ),
        shiny::p(
          class = "text-muted",
          "Only use this when you trust that everything left should be rejected.",
          "This can only be undone by returning records from the Fail tab or on",
          "the Mapping section."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            session$ns("fail_all_review_confirm"),
            "Fail all in Review",
            class = "btn-danger"
          )
        ),
        easyClose = TRUE
      ))
    })
    shiny::observeEvent(input$fail_all_review_confirm, {
      shiny::removeModal()
      rows <- tryCatch(in_review(), error = function(e) NULL)
      shiny::req(!is.null(rows), nrow(rows) > 0)
      apply_occurrence_action(as.character(rows$occsclean_id), "fail")
    })
    shiny::observeEvent(input$keep_to_delete, {
      ids <- selected_occsclean_ids(passed(), input$tbl_keep_rows_selected)
      apply_occurrence_action(ids, "fail")
    })
    shiny::observeEvent(input$keep_to_review, {
      ids <- selected_occsclean_ids(passed(), input$tbl_keep_rows_selected)
      apply_occurrence_action(ids, "review")
    })
    shiny::observeEvent(input$delete_to_keep, {
      ids <- selected_occsclean_ids(failed(), input$tbl_delete_rows_selected)
      apply_occurrence_action(ids, "pass")
    })
    shiny::observeEvent(input$delete_to_review, {
      ids <- selected_occsclean_ids(failed(), input$tbl_delete_rows_selected)
      apply_occurrence_action(ids, "review")
    })
  })
}

#' Prepare findings for the Review DT
#' @param findings Tibble from session findings.
#' @param decisions A [DecisionRegistry].
#' @noRd
prepare_review_table <- function(findings, decisions) {
  out <- slim_flag_columns(
    findings_with_decisions(findings, decisions),
    for_export = FALSE
  )

  dec <- as.character(out$decision)
  dec[dec == "keep"] <- "pass"
  dec[dec == "remove"] <- "fail"
  out$decision <- dec

  skip_factor <- c("occsclean_id", "occurrence_date")
  for (col in setdiff(names(out), skip_factor)) {
    vals <- as.character(out[[col]])
    vals[is.na(vals) | !nzchar(vals)] <- "(blank)"
    levels <- sort(unique(vals))
    out[[col]] <- factor(vals, levels = levels)
  }

  preferred <- c(
    "occsclean_id",
    "check",
    "finding",
    "reason",
    "decision",
    "scientificName",
    "decimalLongitude",
    "decimalLatitude",
    "occurrence_date",
    "check_id"
  )
  ordered <- c(intersect(preferred, names(out)), setdiff(names(out), preferred))
  out[ordered]
}

#' Join effective decisions onto a findings table
#'
#' @param findings Findings tibble with `occsclean_id`, `check_id`, `finding`.
#' @param decisions A [DecisionRegistry].
#' @export
findings_with_decisions <- function(findings, decisions) {
  if (nrow(findings) < 1) {
    findings$decision <- character()
    return(findings)
  }

  eff <- decisions$effective()
  if (nrow(eff) < 1) {
    findings$decision <- rep("unreviewed", nrow(findings))
    return(findings)
  }

  key <- eff[c("occsclean_id", "check_id", "finding", "action")]
  names(key)[names(key) == "action"] <- "decision"
  key$finding_key <- ifelse(is.na(key$finding), "", as.character(key$finding))

  out <- findings
  out$finding_key <- ifelse(is.na(out$finding), "", as.character(out$finding))
  out <- dplyr::left_join(
    out,
    key[c("occsclean_id", "check_id", "finding_key", "decision")],
    by = c("occsclean_id", "check_id", "finding_key")
  )
  out$finding_key <- NULL
  out$decision[is.na(out$decision)] <- "unreviewed"
  out
}
