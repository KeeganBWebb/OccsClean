#' Validate step UI
#' @param id Module id.
#' @noRd
mod_validate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .oc-column-map {
        max-width: 28rem;
        margin: 0.75rem 0 1rem 0;
      }
      .oc-column-map .form-group { margin-bottom: 0.65rem; }
      .oc-column-map-note {
        font-size: 0.9rem;
        color: #664d03;
        margin: 0.35rem 0 0 0;
      }
    ")),
    shiny::h3("Validate"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Verifies that the imported file has the structure OccsClean needs for",
      "downstream cleaning by looking for expected fields (coordinates, dates,",
      "species names, etc.)."
    ),
    shiny::uiOutput(ns("mapping")),
    shiny::verbatimTextOutput(ns("report"))
  )
}

#' Validate step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_validate_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      if (isTRUE(app_state$session$has_data())) {
        return(NULL)
      }
      workflow_warning_ui()
    })

    output$mapping <- shiny::renderUI({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(NULL)
      }
      validation <- s$get_validation()
      if (validation_mapping_panel_hidden(validation)) {
        return(NULL)
      }

      occ <- s$get_occ_working()
      occ_cols <- setdiff(names(occ), "occsclean_id")
      cm <- s$get_column_map()
      skipped <- s$get_skipped_fields()
      ns <- session$ns
      specs <- occurrence_column_map_specs()
      current_map <- stats::setNames(
        lapply(specs, function(spec) cm[[spec$key]]),
        vapply(specs, function(spec) spec$key, character(1))
      )

      inputs <- lapply(specs, function(spec) {
        choices <- column_mapping_choices(occ_cols, current_map, spec$key)
        selected <- selected_column_mapping_value(
          cm[[spec$key]],
          spec$key %in% skipped,
          choices
        )
        shiny::tagList(
          shiny::selectInput(
            inputId = ns(paste0("map_", spec$key)),
            label = spec$label,
            choices = choices,
            selected = selected
          ),
          shiny::uiOutput(ns(paste0("map_note_", spec$key)))
        )
      })

      shiny::div(
        class = "oc-column-map",
        shiny::div(
          class = "alert alert-warning",
          role = "alert",
          "Some expected fields were not detected automatically. Choose a column or Skip for each field below. Skipped fields disable related Assess checks."
        ),
        inputs,
        shiny::actionButton(
          ns("apply_mapping"),
          "Apply mapping",
          class = "btn-primary"
        )
      )
    })

    specs <- occurrence_column_map_specs()
    keys <- vapply(specs, function(spec) spec$key, character(1))
    lapply(specs, function(spec) {
      output[[paste0("map_note_", spec$key)]] <- shiny::renderUI({
        val <- input[[paste0("map_", spec$key)]]
        if (!is_column_map_skip_value(val)) {
          return(NULL)
        }
        shiny::p(
          class = "oc-column-map-note",
          paste0("Skipped: ", column_map_field_skip_message(spec$field), ".")
        )
      })
    })

    shiny::observe({
      s <- app_state$session
      if (!s$has_data()) {
        return()
      }
      validation <- s$get_validation()
      if (validation_mapping_panel_hidden(validation)) {
        return()
      }

      input_ids <- paste0("map_", keys)
      if (!all(vapply(input_ids, function(id) !is.null(input[[id]]), logical(1)))) {
        return()
      }

      occ_cols <- setdiff(names(s$get_occ_working()), "occsclean_id")
      current_map <- stats::setNames(
        lapply(keys, function(key) input[[paste0("map_", key)]]),
        keys
      )

      for (spec in specs) {
        key <- spec$key
        choices <- column_mapping_choices(occ_cols, current_map, key)
        selected <- selected_column_mapping_value(
          if (is_column_map_skip_value(current_map[[key]])) {
            NULL
          } else {
            current_map[[key]]
          },
          is_column_map_skip_value(current_map[[key]]),
          choices
        )
        shiny::updateSelectInput(
          session,
          inputId = paste0("map_", key),
          choices = choices,
          selected = selected
        )
      }
    })

    shiny::observeEvent(input$apply_mapping, {
      s <- app_state$session
      shiny::req(s$has_data())
      map <- list(
        lon = input$map_lon,
        lat = input$map_lat,
        date = input$map_date,
        taxon = input$map_taxon,
        basis_of_record = input$map_basis_of_record,
        country = input$map_country
      )
      tryCatch(
        {
          s$apply_column_mapping(map)
          bump_app_state(app_state)
          validation <- s$get_validation()
          if (identical(validation$overall, "ready")) {
            msg <- if (isTRUE(validation$manually_mapped)) {
              "Column mapping applied. Structure OK - manually mapped."
            } else {
              "Column mapping applied. Structure looks OK."
            }
            shiny::showNotification(msg, type = "message")
          } else if (identical(validation$overall, "ready_with_warnings")) {
            shiny::showNotification(
              "Column mapping applied. Review skipped fields and related checks in the report.",
              type = "warning"
            )
          } else {
            shiny::showNotification(
              "Column mapping applied. Review the validation report.",
              type = "warning"
            )
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Mapping failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    })

    output$report <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded. Import a file first.")
      }
      validation <- s$get_validation()
      if (is.null(validation)) {
        return("Validation has not run yet. Import a file to validate.")
      }
      format_validation_report(validation)
    })
  })
}
