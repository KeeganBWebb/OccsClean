#' Default checks selected on Assess
#' @noRd
default_assess_checks <- function() {
  c("occ_duplicates", "coord_missing", "coord_invalid")
}

#' Checks that expose custom settings in the Assess UI
#' @noRd
assess_checks_with_settings <- function() {
  c(
    "date_out_of_range",
    "occ_basis_of_record",
    "taxon_allowed_species",
    "coord_outside_area",
    "coord_capital",
    "coord_centroid",
    "coord_institution",
    "coord_gbif",
    "coord_country"
  )
}

#' Build tooltip text for an Assess check
#' @noRd
assess_check_tooltip <- function(description, method_via = NA_character_) {
  tip <- as.character(description)
  if (!is.null(method_via) && length(method_via) >= 1 &&
        !is.na(method_via[[1]]) && nzchar(method_via[[1]])) {
    tip <- paste0(tip, " Method via ", method_via[[1]], ".")
  }
  tip
}

#' Help icon shown next to each Assess check
#' @noRd
assess_help_icon <- function(description) {
  shiny::tags$span(
    "?",
    class = "oc-check-help",
    `data-tip` = description,
    tabindex = "0",
    role = "img",
    `aria-label` = description
  )
}

#' Settings cog shown for checks with custom parameters
#' @noRd
assess_settings_icon <- function() {
  tip <- "This check has custom parameters you can set."
  shiny::tags$span(
    class = "oc-check-help oc-check-settings",
    `data-tip` = tip,
    tabindex = "0",
    role = "img",
    `aria-label` = tip,
    "\u2699"
  )
}

#' Choice label with help (and optional settings) icons on the same line
#' @noRd
assess_check_choice_label <- function(label, tip, has_settings = FALSE) {
  shiny::tags$span(
    class = "oc-check-choice",
    shiny::tags$span(class = "oc-check-choice-text", label),
    assess_help_icon(tip),
    if (isTRUE(has_settings)) assess_settings_icon()
  )
}

#' CSS for Assess help icon tooltips
#' @noRd
assess_help_css <- function() {
  shiny::tags$style(shiny::HTML("
    .oc-check-choice {
      display: inline-flex;
      align-items: center;
      flex-wrap: nowrap;
      white-space: nowrap;
      max-width: 100%;
    }
    .oc-check-choice-text {
      white-space: nowrap;
    }
    .shiny-input-checkboxgroup .checkbox,
    .shiny-input-checkboxgroup .form-check {
      white-space: nowrap;
    }
    .shiny-input-checkboxgroup label.checkbox-inline,
    .shiny-input-checkboxgroup .form-check-label {
      display: inline-flex;
      align-items: center;
      white-space: nowrap;
    }
    .oc-check-help {
      position: relative;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      flex: 0 0 auto;
      width: 1.15em;
      height: 1.15em;
      margin-left: 0.35rem;
      border-radius: 50%;
      border: 1px solid #6c757d;
      color: #6c757d;
      font-size: 0.75rem;
      font-weight: 600;
      line-height: 1;
      cursor: help;
      vertical-align: middle;
      user-select: none;
      white-space: nowrap;
    }
    .oc-check-help::after {
      content: attr(data-tip);
      position: absolute;
      left: 50%;
      bottom: calc(100% + 6px);
      transform: translateX(-50%);
      width: max-content;
      max-width: 16rem;
      padding: 0.4rem 0.55rem;
      border-radius: 0.35rem;
      background: #212529;
      color: #fff;
      font-size: 0.75rem;
      font-weight: 400;
      line-height: 1.3;
      text-align: left;
      white-space: normal;
      opacity: 0;
      visibility: hidden;
      transition: opacity 0.05s linear;
      pointer-events: none;
      z-index: 1000;
      box-shadow: 0 2px 8px rgba(0,0,0,0.2);
    }
    .oc-check-help:hover,
    .oc-check-help:focus {
      color: #212529;
      border-color: #212529;
      outline: none;
      box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.25);
    }
    .oc-check-help:hover::after,
    .oc-check-help:focus::after {
      opacity: 1;
      visibility: visible;
    }
    .oc-check-settings {
      border: 1px solid #ffffff !important;
      background: #ffffff;
      color: #6c757d;
      border-radius: 50%;
      overflow: visible;
      font-size: 0.95rem;
      line-height: 1;
    }
    .oc-check-settings:hover,
    .oc-check-settings:focus {
      border: 1px solid #ffffff !important;
      background: #ffffff;
      color: #212529;
      box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.25);
    }
    .oc-assess-group summary {
      cursor: pointer;
      margin-bottom: 0.5rem;
    }
    .oc-assess-groups {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(18rem, 1fr));
      gap: 0.75rem 1.5rem;
      margin-bottom: 1rem;
      align-items: start;
    }
    .oc-assess-group-block {
      min-width: 0;
    }
    .oc-assess-group-block > h4 {
      margin-top: 0;
      margin-bottom: 0.5rem;
    }
    .oc-assess-checks .shiny-options-group,
    .oc-assess-checks .shiny-input-checkboxgroup {
      margin-bottom: 0;
    }
    .oc-assess-checks .form-group {
      display: block;
      margin-bottom: 0;
    }
    .oc-assess-checks .form-group > .shiny-input-checkboxgroup,
    .oc-assess-checks .form-group > .shiny-input-container {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(14rem, 1fr));
      gap: 0.2rem 1rem;
      align-items: start;
    }
    .oc-assess-checks .shiny-options-group {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(14rem, 1fr));
      gap: 0.2rem 1rem;
      align-items: start;
    }
    .oc-assess-checks .checkbox,
    .oc-assess-checks .form-check {
      margin-top: 0.15rem;
      margin-bottom: 0.15rem;
    }
    .oc-assess-settings {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(20rem, 1fr));
      gap: 1rem 1.5rem;
      align-items: start;
      margin-bottom: 0.5rem;
    }
    .oc-assess-settings-panel {
      min-width: 0;
    }
    .oc-assess-settings-wide {
      grid-column: 1 / -1;
    }
    .oc-assess-settings-panel h4 {
      margin-top: 0;
    }
    .oc-assess-buffer-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(16rem, 1fr));
      gap: 0.35rem 1rem;
    }
    .oc-assess-settings .selectize-control {
      width: 100%;
    }
    .oc-assess-settings .selectize-dropdown {
      max-height: 16rem;
    }
  "))
}

#' Build grouped checkbox UI for one Assess section
#' @noRd
assess_group_ui <- function(ns, catalog, group_id, input_id, open = TRUE) {
  rows <- catalog[catalog$ui_group == group_id, , drop = FALSE]
  if (nrow(rows) < 1) {
    return(NULL)
  }

  selected <- intersect(default_assess_checks(), rows$check_id)
  group_label <- rows$ui_group_label[[1]]
  settings_ids <- assess_checks_with_settings()

  choice_names <- lapply(seq_len(nrow(rows)), function(i) {
    tip <- assess_check_tooltip(rows$description[[i]], rows$method_via[[i]])
    assess_check_choice_label(
      rows$label[[i]],
      tip,
      has_settings = rows$check_id[[i]] %in% settings_ids
    )
  })

  body <- shiny::div(
    class = "oc-assess-checks",
    shiny::checkboxGroupInput(
      ns(input_id),
      label = NULL,
      choiceNames = choice_names,
      choiceValues = as.list(rows$check_id),
      selected = selected
    )
  )

  if (isTRUE(open)) {
    shiny::div(
      class = "oc-assess-group-block",
      shiny::h4(group_label),
      body
    )
  } else {
    shiny::div(
      class = "oc-assess-group-block",
      shiny::tags$details(
        class = "oc-assess-group",
        shiny::tags$summary(shiny::tags$strong(group_label)),
        body
      )
    )
  }
}


#' Wrapped settings panel for Assess grid layout
#' @noRd
assess_settings_panel <- function(..., wide = FALSE) {
  cls <- "oc-assess-settings-panel"
  if (isTRUE(wide)) {
    cls <- paste(cls, "oc-assess-settings-wide")
  }
  shiny::div(class = cls, ...)
}

#' Assess step UI
#' @param id Module id.
#' @noRd
mod_assess_ui <- function(id) {
  ns <- shiny::NS(id)
  catalog <- list_quality_checks()

  shiny::tagList(
    assess_help_css(),
    shiny::h3("Assess quality"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Choose which cleaning checks to run. Hover over ? next to each check for help tips."
    ),
    shiny::div(
      class = "oc-assess-groups",
      assess_group_ui(ns, catalog, "basics", "checks_basics", open = TRUE),
      assess_group_ui(ns, catalog, "dates", "checks_dates", open = FALSE),
      assess_group_ui(
        ns, catalog, "suspicious_locations", "checks_locations",
        open = FALSE
      ),
      assess_group_ui(
        ns, catalog, "taxonomic", "checks_taxonomic",
        open = FALSE
      )
    ),
    shiny::uiOutput(ns("check_settings")),
    shiny::br(),
    shiny::actionButton(ns("run"), "Run selected checks", class = "btn-primary"),
    shiny::hr(),
    shiny::h4("Load Settings"),
    shiny::p(
      "Load a saved settings file from a previous run to restore checks and",
      "specified filters. Does not include previously loaded shapefiles or data."
    ),
    shiny::fileInput(
      ns("settings_file"),
      "Load settings",
      accept = c(".json", "application/json"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    shiny::downloadButton(ns("dl_settings"), "Download settings"),
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput(ns("status")),
    shiny::tableOutput(ns("summary"))
  )
}

#' Assess step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_assess_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      if (isTRUE(app_state$session$has_data())) {
        return(NULL)
      }
      workflow_warning_ui()
    })

    pending_filters <- shiny::reactiveVal(NULL)
    loaded_area_source <- shiny::reactiveVal(NULL)

    selected_checks <- shiny::reactive({
      unique(c(
        input$checks_basics %||% character(),
        input$checks_dates %||% character(),
        input$checks_locations %||% character(),
        input$checks_taxonomic %||% character()
      ))
    })

    output$check_settings <- shiny::renderUI({
      invisible(app_state$rev)
      selected <- selected_checks()
      panels <- list()
      pending <- pending_filters()

      pick_text <- function(key, current) {
        if (!is.null(pending) && !is.null(pending[[key]])) {
          return(as.character(pending[[key]]))
        }
        as.character(current %||% "")
      }
      pick_multi <- function(key, current, choices) {
        vals <- if (!is.null(pending) && !is.null(pending[[key]])) {
          unlist_character(pending[[key]])
        } else {
          as.character(current %||% character())
        }
        intersect(vals, choices)
      }

      if ("date_out_of_range" %in% selected) {
        panels[[length(panels) + 1L]] <- assess_settings_panel(
          wide = TRUE,
          shiny::h4("Date range"),
          shiny::p('Required for "Dates outside range". Use YYYY-MM-DD.'),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                session$ns("date_min"),
                "Earliest allowed (YYYY-MM-DD)",
                value = pick_text("date_min", shiny::isolate(input$date_min)),
                placeholder = "e.g. 1900-01-01"
              )
            ),
            shiny::column(
              6,
              shiny::textInput(
                session$ns("date_max"),
                "Latest allowed (YYYY-MM-DD)",
                value = pick_text("date_max", shiny::isolate(input$date_max)),
                placeholder = "e.g. 2024-12-31"
              )
            )
          )
        )
      }

      if ("occ_basis_of_record" %in% selected) {
        s <- app_state$session
        choices <- character()
        if (isTRUE(s$has_data())) {
          cm <- s$get_column_map()
          choices <- basis_of_record_values_in_data(
            s$get_occ_working(),
            basis_col = cm$basis_of_record
          )
        }
        current <- pick_multi(
          "allowed_basis",
          shiny::isolate(input$basis_allowed),
          choices
        )

        if (length(choices) < 1) {
          panels[[length(panels) + 1L]] <- assess_settings_panel(
            shiny::h4("Basis of record"),
            shiny::p(
              "No non-blank basisOfRecord values were found in the uploaded file.",
              "Import data with a basisOfRecord column first, or leave this check unchecked."
            )
          )
        } else {
          panels[[length(panels) + 1L]] <- assess_settings_panel(
            shiny::h4("Basis of record"),
            shiny::p(
              'Required for "Basis Of Record". Options are the unique values',
              "from your uploaded file. Records that are blank or not among the",
              "selected values will be flagged."
            ),
            shiny::selectInput(
              session$ns("basis_allowed"),
              "Allowed basisOfRecord values",
              choices = choices,
              selected = current,
              multiple = TRUE,
              selectize = TRUE
            )
          )
        }
      }

      if ("taxon_allowed_species" %in% selected) {
        s <- app_state$session
        choices <- character()
        if (isTRUE(s$has_data())) {
          cm <- s$get_column_map()
          choices <- scientific_name_values_in_data(
            s$get_occ_working(),
            taxon_col = cm$taxon
          )
        }
        current <- pick_multi(
          "allowed_species",
          shiny::isolate(input$species_allowed),
          choices
        )

        if (length(choices) < 1) {
          panels[[length(panels) + 1L]] <- assess_settings_panel(
            shiny::h4("Allowed species"),
            shiny::p(
              "No non-blank scientific names were found in the uploaded file.",
              "Import data with a scientific name column first, or leave this check unchecked."
            )
          )
        } else {
          panels[[length(panels) + 1L]] <- assess_settings_panel(
            shiny::h4("Allowed species"),
            shiny::p(
              'Required for "Allowed species". Options are the unique scientific',
              "names from your uploaded file. Records that are blank or not among",
              "the selected names will be flagged."
            ),
            shiny::selectInput(
              session$ns("species_allowed"),
              "Allowed scientific names",
              choices = choices,
              selected = current,
              multiple = TRUE,
              selectize = TRUE
            )
          )
        }
      }

      if ("coord_outside_area" %in% selected) {
        area_note <- NULL
        prev_src <- loaded_area_source()
        if (!is.null(prev_src) && nzchar(as.character(prev_src))) {
          area_note <- shiny::p(
            paste0(
              "Previously used shapefile: ", prev_src,
              ". Re-upload it before running this check (polygons are not stored in settings)."
            )
          )
        }
        panels[[length(panels) + 1L]] <- assess_settings_panel(
          wide = TRUE,
          shiny::h4("Study area / range"),
          shiny::p(
            'Required for "Outside study area". Upload a polygon shapefile',
            "(.zip of the shapefile, or select .shp + .shx + .dbf together).",
            "Shapefiles are checked for valid geometry, but in the case of",
            "invalid geometry (such as self-intersecting rings) OccsClean will",
            "attempt to temporarily repair it for this session. OccsClean will",
            "show a clear warning when this happens."
          ),
          area_note,
          shiny::fileInput(
            session$ns("area_shapefile"),
            "Shapefile upload",
            multiple = TRUE,
            accept = c(
              ".zip", ".shp", ".shx", ".dbf", ".prj", ".cpg",
              "application/zip", "application/x-zip-compressed"
            )
          ),
          shiny::textInput(
            session$ns("area_outside_distance_m"),
            paste0(
              "Only flag if farther than this many meters from the polygon ",
              "(default: blank = flag any point outside)"
            ),
            value = pick_text(
              "outside_distance_m",
              shiny::isolate(input$area_outside_distance_m)
            ),
            placeholder = "Leave blank to flag any point outside the polygon"
          )
        )
      }

      buffer_defaults <- coordinatecleaner_buffer_defaults_m()
      buffer_checks <- intersect(
        names(buffer_defaults),
        selected
      )
      if (length(buffer_checks) > 0 || "coord_country" %in% selected) {
        buffer_fields <- list()
        if ("coord_capital" %in% selected) {
          buffer_fields[[length(buffer_fields) + 1L]] <- shiny::textInput(
            session$ns("buffer_capital_m"),
            paste0(
              "Near a capital city buffer (m) (default: ",
              buffer_defaults$coord_capital, ")"
            ),
            value = pick_text(
              "buffer_capital_m",
              shiny::isolate(input$buffer_capital_m) %||%
                as.character(buffer_defaults$coord_capital)
            )
          )
        }
        if ("coord_centroid" %in% selected) {
          buffer_fields[[length(buffer_fields) + 1L]] <- shiny::textInput(
            session$ns("buffer_centroid_m"),
            paste0(
              "Near country or province center buffer (m) (default: ",
              buffer_defaults$coord_centroid, ")"
            ),
            value = pick_text(
              "buffer_centroid_m",
              shiny::isolate(input$buffer_centroid_m) %||%
                as.character(buffer_defaults$coord_centroid)
            )
          )
        }
        if ("coord_institution" %in% selected) {
          buffer_fields[[length(buffer_fields) + 1L]] <- shiny::textInput(
            session$ns("buffer_institution_m"),
            paste0(
              "Near a museum or collection buffer (m) (default: ",
              buffer_defaults$coord_institution, ")"
            ),
            value = pick_text(
              "buffer_institution_m",
              shiny::isolate(input$buffer_institution_m) %||%
                as.character(buffer_defaults$coord_institution)
            )
          )
        }
        if ("coord_gbif" %in% selected) {
          buffer_fields[[length(buffer_fields) + 1L]] <- shiny::textInput(
            session$ns("buffer_gbif_m"),
            paste0(
              "Near GBIF headquarters buffer (m) (default: ",
              buffer_defaults$coord_gbif, ")"
            ),
            value = pick_text(
              "buffer_gbif_m",
              shiny::isolate(input$buffer_gbif_m) %||%
                as.character(buffer_defaults$coord_gbif)
            )
          )
        }
        if ("coord_country" %in% selected) {
          buffer_fields[[length(buffer_fields) + 1L]] <- shiny::textInput(
            session$ns("buffer_country_m"),
            "Outside reported country buffer (m) (default: none)",
            value = pick_text(
              "buffer_country_m",
              shiny::isolate(input$buffer_country_m)
            ),
            placeholder = "Leave blank for no buffer"
          )
        }
        panels[[length(panels) + 1L]] <- assess_settings_panel(
          wide = TRUE,
          shiny::h4("Location buffers"),
          shiny::p(
            "Distance thresholds in meters for selected location checks.",
            "Blank fields use the defaults shown in parentheses.",
            "Make sure to decide these distances based on the range of your",
            "study species."
          ),
          shiny::div(
            class = "oc-assess-buffer-grid",
            do.call(shiny::tagList, buffer_fields)
          )
        )
      }

      if (length(panels) < 1) {
        return(NULL)
      }
      do.call(shiny::div, c(list(class = "oc-assess-settings"), panels))
    })

    shiny::observeEvent(input$settings_file, {
      path <- input$settings_file$datapath
      shiny::req(path)
      tryCatch(
        {
          parsed <- read_assess_settings(path)
          preset <- parsed$settings
          selected <- unlist_character(preset$selected_checks)
          by_g <- assess_checks_by_ui_group(selected)
          pending_filters(preset$settings %||% list())
          src <- preset$settings$area_source %||% NULL
          if (!is.null(src) && length(src) > 0 && any(nzchar(as.character(src)))) {
            loaded_area_source(paste(as.character(src), collapse = ", "))
          } else {
            loaded_area_source(NULL)
          }

          shiny::updateCheckboxGroupInput(
            session, "checks_basics", selected = by_g$basics
          )
          shiny::updateCheckboxGroupInput(
            session, "checks_dates", selected = by_g$dates
          )
          shiny::updateCheckboxGroupInput(
            session, "checks_locations", selected = by_g$suspicious_locations
          )
          shiny::updateCheckboxGroupInput(
            session, "checks_taxonomic", selected = by_g$taxonomic
          )

          for (w in parsed$warnings) {
            shiny::showNotification(w, type = "warning", duration = NULL)
          }
          if ("coord_outside_area" %in% selected) {
            shiny::showNotification(
              "Study-area shapefile must be re-uploaded before running Outside study area.",
              type = "message",
              duration = NULL
            )
          }
          shiny::showNotification(
            "Assess settings loaded. Review filters, then Run selected checks.",
            type = "message"
          )

          session$onFlushed(function() {
            pending_filters(NULL)
          }, once = TRUE)
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not load Assess settings:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    })

    output$dl_settings <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "occsclean_assess_settings_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        settings <- collect_assess_settings_from_inputs(input)
        src <- loaded_area_source()
        if (!is.null(src) && nzchar(as.character(src))) {
          settings$area_source <- as.character(src)
        }
        upload <- input$area_shapefile
        if (!is.null(upload) && nrow(upload) > 0) {
          settings$area_source <- paste(upload$name, collapse = ", ")
        } else {
          assess <- app_state$session$get_assessment()
          if ("coord_outside_area" %in% names(assess)) {
            from_run <- assess$coord_outside_area$params_used$area_source
            if (!is.null(from_run) && any(nzchar(as.character(from_run)))) {
              settings$area_source <- paste(as.character(from_run), collapse = ", ")
            }
          }
        }
        write_assess_settings(
          path = file,
          selected_checks = selected_checks(),
          settings = settings
        )
      }
    )

    shiny::observeEvent(input$run, {
      s <- app_state$session
      if (!s$has_data()) {
        shiny::showNotification("Import data before running checks.", type = "warning")
        return(invisible(NULL))
      }
      checks <- selected_checks()
      if (length(checks) < 1) {
        shiny::showNotification("Select at least one check.", type = "warning")
        return(invisible(NULL))
      }

      range_params <- list()
      range_ok <- TRUE
      if ("date_out_of_range" %in% checks) {
        min_txt <- trimws(input$date_min %||% "")
        max_txt <- trimws(input$date_max %||% "")
        if (!nzchar(min_txt) && !nzchar(max_txt)) {
          shiny::showNotification(
            'Set an earliest and/or latest date for "Dates outside range".',
            type = "warning"
          )
          return(invisible(NULL))
        }
        tryCatch(
          {
            if (nzchar(min_txt)) {
              range_params$min_date <- parse_param_date(min_txt)
            }
            if (nzchar(max_txt)) {
              range_params$max_date <- parse_param_date(max_txt)
            }
          },
          error = function(e) {
            range_ok <<- FALSE
            shiny::showNotification(
              paste("Date range error:", conditionMessage(e)),
              type = "error",
              duration = NULL
            )
          }
        )
        if (!isTRUE(range_ok)) {
          return(invisible(NULL))
        }
      }

      basis_params <- list()
      if ("occ_basis_of_record" %in% checks) {
        allowed <- input$basis_allowed %||% character()
        allowed <- allowed[nzchar(allowed)]
        if (length(allowed) < 1) {
          shiny::showNotification(
            'Select at least one allowed value for "Basis Of Record".',
            type = "warning"
          )
          return(invisible(NULL))
        }
        basis_params$allowed_basis <- as.character(allowed)
      }

      species_params <- list()
      if ("taxon_allowed_species" %in% checks) {
        allowed <- input$species_allowed %||% character()
        allowed <- allowed[nzchar(allowed)]
        if (length(allowed) < 1) {
          shiny::showNotification(
            'Select at least one allowed scientific name for "Allowed species".',
            type = "warning"
          )
          return(invisible(NULL))
        }
        species_params$allowed_species <- as.character(allowed)
      }

      area_params <- list()
      if ("coord_outside_area" %in% checks) {
        upload <- input$area_shapefile
        if (is.null(upload) || nrow(upload) < 1) {
          shiny::showNotification(
            'Upload a study-area shapefile for "Outside study area".',
            type = "warning"
          )
          return(invisible(NULL))
        }

        distance_ok <- TRUE
        outside_distance_m <- tryCatch(
          parse_outside_distance_m(input$area_outside_distance_m),
          error = function(e) {
            distance_ok <<- FALSE
            shiny::showNotification(
              paste("Study area distance error:", conditionMessage(e)),
              type = "error",
              duration = NULL
            )
            NULL
          }
        )
        if (!isTRUE(distance_ok)) {
          return(invisible(NULL))
        }
        if (!is.null(outside_distance_m)) {
          area_params$outside_distance_m <- outside_distance_m
        }

        area_ok <- TRUE
        prepared <- tryCatch(
          {
            out <- read_area_polygon_upload(upload$datapath, upload$name)
            area_params$area_source <- paste(upload$name, collapse = ", ")
            area_params$area_geom <- out$geom
            area_params$geometry_repaired <- isTRUE(out$geometry_repaired)
            if (isTRUE(out$geometry_repaired)) {
              area_params$repair_message <- out$repair_message
            }
            out
          },
          error = function(e) {
            area_ok <<- FALSE
            shiny::showNotification(
              paste("Study area shapefile error:", conditionMessage(e)),
              type = "error",
              duration = NULL
            )
            NULL
          }
        )
        if (!isTRUE(area_ok)) {
          return(invisible(NULL))
        }
        if (isTRUE(prepared$geometry_repaired)) {
          shiny::showNotification(
            prepared$repair_message,
            type = "warning",
            duration = NULL
          )
        }
        s$set_study_area(prepared$geom, area_params$area_source)
      } else {
        s$clear_study_area()
      }

      buffer_defaults <- coordinatecleaner_buffer_defaults_m()
      buffer_ok <- TRUE
      capital_params <- list()
      centroid_params <- list()
      institution_params <- list()
      gbif_params <- list()
      country_params <- list()

      if ("coord_capital" %in% checks) {
        capital_params$buffer <- tryCatch(
          parse_buffer_distance_m(
            input$buffer_capital_m,
            buffer_defaults$coord_capital,
            label = "Capital city buffer"
          ),
          error = function(e) {
            buffer_ok <<- FALSE
            shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
            NULL
          }
        )
        capital_params$geod <- TRUE
      }
      if (!isTRUE(buffer_ok)) {
        return(invisible(NULL))
      }
      if ("coord_centroid" %in% checks) {
        centroid_params$buffer <- tryCatch(
          parse_buffer_distance_m(
            input$buffer_centroid_m,
            buffer_defaults$coord_centroid,
            label = "Centroid buffer"
          ),
          error = function(e) {
            buffer_ok <<- FALSE
            shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
            NULL
          }
        )
        centroid_params$geod <- TRUE
      }
      if (!isTRUE(buffer_ok)) {
        return(invisible(NULL))
      }
      if ("coord_institution" %in% checks) {
        institution_params$buffer <- tryCatch(
          parse_buffer_distance_m(
            input$buffer_institution_m,
            buffer_defaults$coord_institution,
            label = "Institution buffer"
          ),
          error = function(e) {
            buffer_ok <<- FALSE
            shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
            NULL
          }
        )
        institution_params$geod <- TRUE
      }
      if (!isTRUE(buffer_ok)) {
        return(invisible(NULL))
      }
      if ("coord_gbif" %in% checks) {
        gbif_params$buffer <- tryCatch(
          parse_buffer_distance_m(
            input$buffer_gbif_m,
            buffer_defaults$coord_gbif,
            label = "GBIF headquarters buffer"
          ),
          error = function(e) {
            buffer_ok <<- FALSE
            shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
            NULL
          }
        )
        gbif_params$geod <- TRUE
      }
      if (!isTRUE(buffer_ok)) {
        return(invisible(NULL))
      }
      if ("coord_country" %in% checks) {
        country_buf <- tryCatch(
          parse_optional_buffer_distance_m(
            input$buffer_country_m,
            label = "Country buffer"
          ),
          error = function(e) {
            buffer_ok <<- FALSE
            shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
            NULL
          }
        )
        if (!isTRUE(buffer_ok)) {
          return(invisible(NULL))
        }
        if (!is.null(country_buf)) {
          country_params$buffer <- country_buf
          country_params$geod <- TRUE
        }
      }

      params <- list(
        date_out_of_range = range_params,
        occ_basis_of_record = basis_params,
        taxon_allowed_species = species_params,
        coord_outside_area = area_params,
        coord_capital = capital_params,
        coord_centroid = centroid_params,
        coord_institution = institution_params,
        coord_gbif = gbif_params,
        coord_country = country_params
      )

      tryCatch(
        {
          shiny::withProgress(
            message = "Running checks",
            detail = "Starting...",
            value = 0,
            {
              s$run_checks(
                check_ids = checks,
                params = params,
                progress = function(i, n, check_id) {
                  lab <- tryCatch(
                    get_quality_check_def(check_id)$label,
                    error = function(e) check_id
                  )
                  shiny::setProgress(
                    value = (i - 1) / max(n, 1),
                    detail = paste0(i, " / ", n, ": ", lab)
                  )
                }
              )
              shiny::setProgress(value = 1, detail = "Finishing...")
            }
          )
          bump_app_state(app_state)
          shiny::showNotification("Checks finished.", type = "message")
        },
        error = function(e) {
          shiny::showNotification(
            paste("Checks failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    })

    output$status <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded.")
      }
      assessment <- s$get_assessment()
      if (length(assessment) < 1) {
        return("No checks run yet.")
      }
      n_flagged <- sum(vapply(
        assessment,
        function(r) as.integer(r$summary$n_flagged %||% 0L),
        integer(1)
      ))
      paste0(
        "Cached checks: ", length(assessment), "\n",
        "Total flags: ", n_flagged
      )
    })

    output$summary <- shiny::renderTable({
      invisible(app_state$rev)
      s <- app_state$session
      shiny::req(s$has_data())
      assessment <- s$get_assessment()
      shiny::req(length(assessment) > 0)
      tibble::tibble(
        check = vapply(assessment, `[[`, character(1), "label"),
        category = vapply(assessment, `[[`, character(1), "category"),
        check_status = vapply(
          assessment,
          function(r) format_check_run_outcome(r$status),
          character(1)
        ),
        flagged = vapply(
          assessment,
          function(r) as.integer(r$summary$n_flagged %||% 0L),
          integer(1)
        ),
        checked = vapply(
          assessment,
          function(r) as.integer(r$summary$n_checked %||% 0L),
          integer(1)
        )
      )
    })
  })
}
