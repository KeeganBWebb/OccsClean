#' Review step UI
#' @param id Module id.
#' @noRd
mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Review"),
    shiny::div(
      class = "oc-review-root",
      shiny::div(
        class = "oc-review-intro",
        shiny::uiOutput(ns("warning")),
        shiny::p(
          "Checks flagged records based on checks from the Assess tab.",
          "Flagged and failed records are excluded from the cleaned CSV unless you pass them.",
          "Use the Check flag box to select which check you want to review, and use the",
          "Status box to search the table by current decision (In review, Passed, Failed,",
          "Batch Failed, or Batch Passed)."
        )
      ),
      shiny::div(
        class = "oc-review-workspace",
        shiny::div(
          class = "oc-review-table-pane",
          shiny::div(
            class = "oc-review-table-toolbar",
            shiny::div(
              class = "oc-review-batch-bar",
              shiny::div(
                class = "oc-review-batch-check",
                shiny::selectInput(
                  ns("check_flag"),
                  "Check flag",
                  choices = stats::setNames(
                    review_check_flag_all_value(),
                    review_check_flag_all_label
                  ),
                  selected = review_check_flag_all_value(),
                  width = "100%"
                )
              ),
              shiny::uiOutput(ns("batch_actions_review")),
              shiny::div(class = "oc-review-batch-spacer"),
              shiny::actionButton(
                ns("fail_all_review"),
                "Fail all in review…",
                class = "btn-sm btn-outline-secondary"
              )
            )
          ),
          shiny::div(
            class = "oc-review-table-wrap",
            DT::DTOutput(ns("tbl_review"))
          )
        ),
        shiny::div(
          class = "oc-review-map-pane",
          shiny::div(
            class = "oc-review-selected",
            shiny::tags$strong("Selected record"),
            shiny::uiOutput(ns("selected_actions")),
            shiny::div(
              class = "oc-review-selected-help text-muted",
              style = "font-size: 0.85rem;",
              shiny::textOutput(ns("selected_help"))
            ),
            shiny::uiOutput(ns("selected_record"))
          ),
          shiny::div(
            class = "oc-review-map",
            leaflet::leafletOutput(ns("map"), height = "100%")
          ),
          shiny::p(
            class = "text-muted oc-review-map-disclaimer",
            style = "font-size: 0.85rem; margin: 0;",
            "Points are plotted from decimalLongitude / decimalLatitude as WGS84",
            "(EPSG:4326) geographic coordinates. The basemap is for context only",
            "(Web Mercator display); it does not reproject or alter your coordinates."
          )
        )
      ),
      shiny::div(
        class = "oc-review-status-panel",
        shiny::verbatimTextOutput(ns("status"))
      )
    )
  )
}

review_check_flag_all_label <- "All Checks"

#' Whether coordinates are exactly Null Island (0, 0)
#' @noRd
review_map_is_null_island <- function(lng, lat) {
  lng <- as.numeric(lng)[[1]]
  lat <- as.numeric(lat)[[1]]
  is.finite(lng) &&
    is.finite(lat) &&
    abs(lng) < 1e-12 &&
    abs(lat) < 1e-12
}

#' Map view center for setView (nudges off exact 0, 0)
#'
#' Leaflet with worldCopyJump unsettles on a perfect 0, 0 center. Markers and
#' popups still use the true coordinates.
#' @noRd
review_map_view_lnglat <- function(lng, lat) {
  lng <- as.numeric(lng)[[1]]
  lat <- as.numeric(lat)[[1]]
  if (review_map_is_null_island(lng, lat)) {
    eps <- 0.0001
    return(list(lng = eps, lat = eps))
  }
  list(lng = lng, lat = lat)
}

#' Map status label for the review selected-record panel
#' @noRd
review_panel_map_status <- function(review_status, n_flags) {
  rs <- as.character(review_status)[[1]]
  nf <- as.integer(n_flags)[[1]]
  if (is.na(nf)) {
    nf <- 0L
  }
  if (rs %in% c("fail", "batch_fail")) {
    return("Failed")
  }
  if (rs %in% c("pass", "batch_pass")) {
    return("Passed")
  }
  if (identical(rs, "review")) {
    return("Flagged")
  }
  "OK"
}

review_map_follow_js <- function(handler_name) {
  paste0(
    "var ocReviewLastFollow = null;",
    "function followRecordOnMap(data) {",
    "  if (!data || !data.id) { return; }",
    "  var zoom = data.zoom || 7;",
    "  var vLng = (typeof data.viewLng === 'number') ? data.viewLng : data.lng;",
    "  var vLat = (typeof data.viewLat === 'number') ? data.viewLat : data.lat;",
    "  var followKey = [data.id, vLng, vLat, zoom, !!data.openPopup].join('|');",
    "  if (ocReviewLastFollow === followKey) { return; }",
    "  ocReviewLastFollow = followKey;",
    "  if (typeof vLng === 'number' && typeof vLat === 'number') {",
    "    map.setView([vLat, vLng], zoom, {animate: false, reset: true});",
    "  }",
    "  if (!data.openPopup) { return; }",
    "  var prefix = data.id + '||';",
    "  var target = null;",
    "  if (typeof data.lat === 'number' && typeof data.lng === 'number') {",
    "    target = L.latLng(data.lat, data.lng);",
    "  }",
    "  setTimeout(function() {",
    "    map.closePopup();",
    "    var best = null;",
    "    var bestDist = Infinity;",
    "    map.eachLayer(function(layer) {",
    "      if (!layer.options || !layer.options.layerId) { return; }",
    "      if (layer.options.layerId.indexOf(prefix) !== 0) { return; }",
    "      if (typeof layer.openPopup !== 'function') { return; }",
    "      var ll = layer.getLatLng();",
    "      if (!ll) { return; }",
    "      var d = target ? map.distance(target, ll) : map.distance(map.getCenter(), ll);",
    "      if (d < bestDist) {",
    "        bestDist = d;",
    "        best = layer;",
    "      }",
    "    });",
    "    if (best) { best.openPopup(); }",
    "  }, 120);",
    "}",
    "window._ocReviewFollowRecord = followRecordOnMap;",
    "Shiny.addCustomMessageHandler('", handler_name, "', followRecordOnMap);"
  )
}

#' Build id-keyed map follow payloads for rows in the review table
#' @noRd
review_table_coord_map <- function(df, coords) {
  if (is.null(df) || nrow(df) < 1 || is.null(coords) || nrow(coords) < 1) {
    return(list())
  }
  out <- list()
  ids <- as.character(df$occsclean_id)
  mappable <- coords[coords$mappable, , drop = FALSE]
  if (nrow(mappable) < 1) {
    return(out)
  }
  for (id in ids) {
    hit <- mappable[as.character(mappable$occsclean_id) == id, , drop = FALSE]
    if (nrow(hit) < 1) {
      next
    }
    lng <- as.numeric(hit$longitude[[1]])
    lat <- as.numeric(hit$latitude[[1]])
    if (!is.finite(lng) || !is.finite(lat)) {
      next
    }
    view <- review_map_view_lnglat(lng, lat)
    out[[id]] <- list(
      id = id,
      lng = lng,
      lat = lat,
      viewLng = view$lng,
      viewLat = view$lat
    )
  }
  out
}

review_dt_init_js <- function(check_filter_choices, coord_map = list()) {
  choices_json <- jsonlite::toJSON(
    as.character(check_filter_choices),
    auto_unbox = TRUE
  )
  coord_json <- jsonlite::toJSON(
    coord_map,
    auto_unbox = TRUE,
    null = "null"
  )
  DT::JS(paste0(
    "function() {",
    "  var api = this.api();",
    "  var table = api.table();",
    "  var $wrap = $(api.table().container()).closest('.oc-review-table-wrap');",
    "  var fitTimer = null;",
    "  var lastFitH = null;",
    "  var fitReviewTable = function() {",
    "    if (fitTimer) { clearTimeout(fitTimer); }",
    "    fitTimer = setTimeout(function() {",
    "      fitTimer = null;",
    "      if (!$wrap.length) { return; }",
    "      var $c = $(api.table().container());",
    "      var used = 0;",
    "      $c.children(':visible').not('.dataTables_scroll').each(function() {",
    "        used += $(this).outerHeight(true);",
    "      });",
    "      used += $c.find('.dataTables_scrollHead').outerHeight(true) || 0;",
    "      var h = Math.max(160, $wrap.innerHeight() - used - 12);",
    "      if (lastFitH === h) { return; }",
    "      lastFitH = h;",
    "      $c.find('.dataTables_scrollBody').css({",
    "        'max-height': h + 'px',",
    "        'height': h + 'px'",
    "      });",
    "    }, 80);",
    "  };",
    "  fitReviewTable();",
    "  setTimeout(fitReviewTable, 0);",
    "  setTimeout(fitReviewTable, 250);",
    "  $(window).on('resize.ocReviewTbl', fitReviewTable);",
    "  var checkChoices = ", choices_json, ";",
    "  var coordMap = ", coord_json, ";",
    "  var idColIdx = null;",
    "  api.columns().every(function(i) {",
    "    if ($(api.column(i).header()).text().trim() === 'occsclean_id') {",
    "      idColIdx = i;",
    "    }",
    "  });",
    "  table.off('select.ocReviewMap');",
    "  table.off('deselect.ocReviewMap');",
    "  table.on('deselect.ocReviewMap', function() { ocReviewLastFollow = null; });",
    "  table.on('select.ocReviewMap', function(e, dt, type, indexes) {",
    "    if (type !== 'row' || !indexes || indexes.length < 1) { return; }",
    "    if (idColIdx === null) { return; }",
    "    var id = api.row(indexes[0]).data()[idColIdx];",
    "    if (!id || !coordMap[id]) { return; }",
    "    if (typeof window._ocReviewFollowRecord !== 'function') { return; }",
    "    var src = coordMap[id];",
    "    window._ocReviewFollowRecord({",
    "      id: src.id,",
    "      lng: src.lng,",
    "      lat: src.lat,",
    "      viewLng: src.viewLng,",
    "      viewLat: src.viewLat,",
    "      openPopup: false,",
    "      zoom: 7",
    "    });",
    "  });",
    "  var setupChecksFilter = function() {",
    "    var $scrollHead = $wrap.find('.dataTables_scrollHead');",
    "    if (!$scrollHead.length) { return; }",
    "    var checksColIdx = null;",
    "    $scrollHead.find('thead tr').first().find('th').each(function(i) {",
    "      if ($(this).text().trim() === 'checks') { checksColIdx = i; }",
    "    });",
    "    if (checksColIdx === null || checkChoices.length < 1) { return; }",
    "    var $filterRow = $scrollHead.find('thead tr').eq(1);",
    "    if (!$filterRow.length) { return; }",
    "    var $filterCell = $filterRow.find('th, td').eq(checksColIdx);",
    "    if (!$filterCell.length) { return; }",
    "    $filterCell.empty();",
    "    var $sel = $('<select class=\"form-control oc-review-checks-filter\">",
    "      <option value=\"\">All</option></select>');",
    "    checkChoices.forEach(function(label) {",
    "      $sel.append($('<option></option>').val(label).text(label));",
    "    });",
    "    $filterCell.append($sel);",
    "    $sel.on('change.ocReviewChecks', function() {",
    "      api.column(checksColIdx).search(this.value || '', false, false).draw();",
    "    });",
    "  };",
    "  setupChecksFilter();",
    "  setTimeout(setupChecksFilter, 0);",
    "  setTimeout(setupChecksFilter, 100);",
    "  setTimeout(setupChecksFilter, 300);",
    "}"
  ))
}

#' Review step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_review_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_occsclean_id <- shiny::reactiveVal(NULL)
    should_fit_bounds <- shiny::reactiveVal(TRUE)
    marker_layer_ids <- shiny::reactiveVal(character())
    map_ready <- shiny::reactiveVal(FALSE)
    review_table_page_length <- 15L
    recenter_map_on_selected <- shiny::reactiveVal(FALSE)
    review_table_proxy <- DT::dataTableProxy("tbl_review", session = session)

    batch_actions_ui <- function(buttons) {
      shiny::div(
        class = "oc-review-batch-actions",
        buttons
      )
    }

    output$batch_actions_review <- shiny::renderUI({
      if (review_check_flag_is_all(input$check_flag)) {
        return(NULL)
      }
      batch_actions_ui(
        list(
          shiny::actionButton(
            session$ns("keep_by_type"),
            "Pass all records with only this check in review",
            class = "btn-sm btn-success"
          ),
          shiny::actionButton(
            session$ns("delete_by_type"),
            "Fail all records with this check present in review",
            class = "btn-sm btn-outline-danger"
          )
        )
      )
    })

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
      s <- app_state$session
      prepare_review_occurrence_table(
        findings = raw_findings(),
        decisions = s$get_decisions(),
        occ = s$get_occ_working(),
        column_map = s$get_column_map()
      )
    })

    subset_status <- function(status) {
      df <- occurrences()
      hit <- as.character(df$review_status) == status
      df[hit, , drop = FALSE]
    }

    in_review <- shiny::reactive(subset_status("review"))
    passed <- shiny::reactive({
      df <- occurrences()
      hit <- as.character(df$review_status) %in% c("pass", "batch_pass")
      df[hit, , drop = FALSE]
    })
    failed <- shiny::reactive({
      df <- occurrences()
      hit <- as.character(df$review_status) %in% c("fail", "batch_fail")
      df[hit, , drop = FALSE]
    })

    active_table_df <- shiny::reactive({
      filter_review_occurrences_by_check_label(
        occurrences(),
        input$check_flag
      )
    })

    update_check_flag_choices <- function(occ_df) {
      all_val <- review_check_flag_all_value()
      labels <- review_checks_column_filter_choices(occ_df)
      choices <- c(
        stats::setNames(all_val, review_check_flag_all_label),
        stats::setNames(labels, labels)
      )
      current <- as.character(shiny::isolate(input$check_flag %||% all_val))[[1]]
      if (review_check_flag_is_all(current)) {
        current <- all_val
      }
      if (current == all_val || current %in% labels) {
        shiny::updateSelectInput(
          session,
          "check_flag",
          choices = choices,
          selected = current
        )
      } else {
        shiny::updateSelectInput(
          session,
          "check_flag",
          choices = choices,
          selected = all_val
        )
      }
    }

    shiny::observe({
      df <- tryCatch(occurrences(), error = function(e) NULL)
      update_check_flag_choices(df)
    })

    map_records <- shiny::reactive({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return(empty_visualize_points())
      }
      findings <- NULL
      if (length(s$get_assessment()) > 0) {
        findings <- s$get_findings_table()
      }
      build_visualize_records(
        occ = s$get_occ_working(),
        findings = findings,
        decisions = s$get_decisions(),
        column_map = s$get_column_map()
      )
    })

    map_display_points <- shiny::reactive({
      rec <- map_records()
      if (nrow(rec) < 1) {
        return(empty_visualize_points())
      }
      rec[rec$mappable, , drop = FALSE]
    })

    selected_panel_info <- shiny::reactive({
      rid <- selected_occsclean_id()
      if (is.null(rid) || !nzchar(as.character(rid))) {
        return(NULL)
      }
      df <- occurrences()
      hit <- df[as.character(df$occsclean_id) == as.character(rid), , drop = FALSE]
      if (nrow(hit) >= 1) {
        row <- hit[1, , drop = FALSE]
        return(list(
          occsclean_id = as.character(row$occsclean_id[[1]]),
          map_status = review_panel_map_status(
            row$review_status[[1]],
            row$n_flags[[1]]
          ),
          flags = as.character(row$checks[[1]] %||% "")
        ))
      }
      rec <- map_records()
      map_hit <- rec[as.character(rec$occsclean_id) == as.character(rid), , drop = FALSE]
      if (nrow(map_hit) < 1) {
        return(NULL)
      }
      row <- map_hit[1, , drop = FALSE]
      list(
        occsclean_id = as.character(row$occsclean_id[[1]]),
        map_status = as.character(row$map_status[[1]]),
        flags = as.character(row$flag_labels[[1]] %||% "")
      )
    })

    review_panel_mode <- shiny::reactive({
      info <- selected_panel_info()
      if (is.null(info)) {
        return("none")
      }
      switch(
        info$map_status,
        Flagged = "pass_fail",
        Passed = "flag_only",
        Failed = "pass_flag",
        OK = "flag_only",
        "none"
      )
    })

    data_fit_key <- shiny::reactive({
      invisible(app_state$rev)
      s <- app_state$session
      if (!isTRUE(s$has_data())) {
        return(NA_character_)
      }
      meta <- s$get_meta()
      paste(
        as.character(meta$source_name %||% ""),
        as.character(meta$n_rows %||% 0L),
        as.character(meta$imported_at %||% ""),
        sep = "\r"
      )
    })

    shiny::observeEvent(
      data_fit_key(),
      {
        should_fit_bounds(TRUE)
        selected_occsclean_id(NULL)
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$tbl_review_rows_selected,
      {
        rows <- input$tbl_review_rows_selected
        if (is.null(rows) || length(rows) < 1) {
          return(invisible(NULL))
        }
        df <- shiny::isolate(active_table_df())
        if (is.null(df) || nrow(df) < 1) {
          return(invisible(NULL))
        }
        idx <- as.integer(rows)[[1]]
        if (is.na(idx) || idx < 1L || idx > nrow(df)) {
          return(invisible(NULL))
        }
        rid <- as.character(df$occsclean_id[[idx]])
        if (identical(rid, shiny::isolate(selected_occsclean_id()))) {
          return(invisible(NULL))
        }
        selected_occsclean_id(rid)
      },
      ignoreNULL = FALSE
    )

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

    render_occ_dt <- function(df, coords) {
      show <- df
      status_vals <- NULL
      if ("review_status" %in% names(show)) {
        status_vals <- occurrence_review_status_label(as.character(show$review_status))
      }
      drop_cols <- c(
        "review_status", "check_ids",
        "decimalLongitude", "decimalLatitude"
      )
      for (col in drop_cols) {
        if (col %in% names(show)) {
          show[[col]] <- NULL
        }
      }
      if (!is.null(status_vals)) {
        show$status <- factor(
          status_vals,
          levels = c(
            "In review", "Passed", "Failed", "Batch Failed", "Batch Passed"
          )
        )
      }
      show_cols <- c(
        "status", "checks", "scientificName", "occurrence_date", "occsclean_id"
      )
      show <- show[, intersect(show_cols, names(show)), drop = FALSE]
      if ("scientificName" %in% names(show)) {
        sci_vals <- as.character(show$scientificName)
        sci_vals[is.na(sci_vals) | !nzchar(sci_vals)] <- "(blank)"
        show$scientificName <- factor(
          sci_vals,
          levels = sort(unique(sci_vals))
        )
      }
      check_filter_choices <- review_checks_column_filter_choices(df)
      coord_map <- review_table_coord_map(df, coords)
      init_js <- review_dt_init_js(check_filter_choices, coord_map)
      id_col_idx <- which(names(show) == "occsclean_id") - 1L
      col_defs <- list()
      if (length(id_col_idx) == 1L && id_col_idx >= 0L) {
        col_defs <- list(list(visible = FALSE, targets = id_col_idx))
      }
      opts <- list(
        pageLength = review_table_page_length,
        scrollY = "240px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        dom = "lrtip",
        orderCellsTop = TRUE,
        processing = FALSE,
        columnDefs = col_defs,
        initComplete = init_js
      )
      DT::datatable(
        show,
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = opts
      )
    }

    output$tbl_review <- DT::renderDT({
      render_occ_dt(active_table_df(), shiny::isolate(map_records()))
    })

    output$selected_record <- shiny::renderUI({
      rid <- selected_occsclean_id()
      info <- selected_panel_info()
      if (is.null(rid) || !nzchar(as.character(rid)) || is.null(info)) {
        return(shiny::tags$div(
          class = "text-muted oc-review-record-view",
          "Select a table row or map point to view record fields."
        ))
      }
      s <- app_state$session
      shiny::req(s$has_data())
      occ <- s$get_occ_working()
      hit <- occ[as.character(occ$occsclean_id) == as.character(rid), , drop = FALSE]
      if (nrow(hit) < 1) {
        return(shiny::tags$div(
          class = "text-muted oc-review-record-view",
          "Record not found."
        ))
      }
      show <- strip_occsclean_columns(hit)
      flags <- info$flags
      review_rows <- list(
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong("Review status")),
          shiny::tags$td(visualize_status_label(info$map_status))
        ),
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong("Flags")),
          shiny::tags$td(
            if (!is.na(flags) && nzchar(flags)) flags else "(none)"
          )
        )
      )
      field_rows <- lapply(names(show), function(col) {
        val <- as.character(show[[col]][[1]])
        if (is.na(val) || !nzchar(val)) {
          val <- "(blank)"
        }
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$strong(col)),
          shiny::tags$td(val)
        )
      })
      shiny::tags$div(
        class = "oc-review-record-view",
        shiny::tags$table(
          class = "table table-sm table-striped",
          shiny::tags$tbody(c(review_rows, field_rows))
        )
      )
    }) |> shiny::bindEvent(
      selected_occsclean_id(),
      app_state$rev,
      ignoreNULL = FALSE
    )

    output$selected_help <- shiny::renderText({
      info <- selected_panel_info()
      if (is.null(info)) {
        return("")
      }
      switch(
        info$map_status,
        Failed = paste(
          "This record is excluded from export.",
          "Pass it to include it again, or flag it to return to review."
        ),
        Passed = paste(
          "This record is kept in the cleaned export.",
          "Flag it to return to review."
        ),
        Flagged = paste(
          "Pass to keep this record in the cleaned export,",
          "or fail to exclude it."
        ),
        OK = paste(
          "This record has no check flags.",
          "You can manually flag it for review, but you should only do",
          "so if you have a defendable rationale for the decision."
        ),
        "This record has no check flags. Fail to exclude it from export."
      )
    })

    output$selected_actions <- shiny::renderUI({
      mode <- review_panel_mode()
      buttons <- switch(
        mode,
        pass_fail = list(
          shiny::actionButton(
            session$ns("sel_pass"),
            "Pass",
            class = "btn-success"
          ),
          shiny::actionButton(
            session$ns("sel_fail"),
            "Fail",
            class = "btn-outline-danger"
          )
        ),
        flag_only = list(
          shiny::actionButton(
            session$ns("sel_flag_record"),
            "Flag record",
            class = "btn-outline-warning"
          )
        ),
        pass_flag = list(
          shiny::actionButton(
            session$ns("sel_pass"),
            "Pass",
            class = "btn-success"
          ),
          shiny::actionButton(
            session$ns("sel_flag_record"),
            "Flag record",
            class = "btn-outline-warning"
          )
        ),
        list()
      )
      shiny::div(
        class = "oc-review-selected-actions",
        if (length(buttons) > 0) {
          shiny::div(class = "btn-group", role = "group", buttons)
        }
      )
    }) |> shiny::bindEvent(
      review_panel_mode(),
      ignoreNULL = FALSE
    )

    send_map_follow_payload <- function(id, lng, lat, zoom = 7L, open_popup = TRUE) {
      if (!isTRUE(map_ready())) {
        return(invisible(NULL))
      }
      lng <- as.numeric(lng)[[1]]
      lat <- as.numeric(lat)[[1]]
      if (!is.finite(lng) || !is.finite(lat)) {
        return(invisible(NULL))
      }
      view <- review_map_view_lnglat(lng, lat)
      session$sendCustomMessage(
        session$ns("review_map_follow"),
        list(
          id = as.character(id),
          lng = lng,
          lat = lat,
          viewLng = view$lng,
          viewLat = view$lat,
          zoom = as.integer(zoom),
          openPopup = isTRUE(open_popup)
        )
      )
      invisible(NULL)
    }

    output$map <- leaflet::renderLeaflet({
      follow_handler <- session$ns("review_map_follow")
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          preferCanvas = TRUE,
          worldCopyJump = TRUE,
          minZoom = 2
        )
      ) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldGrayCanvas,
          group = "Light"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("Study area"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::setView(lng = 0, lat = 20, zoom = 2) |>
        htmlwidgets::onRender(paste0(
          "function(el, x) {
             var map = this;
             var resizeTimer = null;
             var lastW = 0;
             var lastH = 0;
             function updateMinZoom() {
               var w = el.clientWidth || 1;
               var minZ = Math.max(2, Math.ceil(Math.log(w / 256) / Math.LN2));
               minZ = Math.min(minZ, 4);
               map.setMinZoom(minZ);
               if (map.getZoom() < minZ) {
                 map.setZoom(minZ);
               }
             }
             function refreshSize() {
               if (resizeTimer) { clearTimeout(resizeTimer); }
               resizeTimer = setTimeout(function() {
                 resizeTimer = null;
                 var w = el.clientWidth || 0;
                 var h = el.clientHeight || 0;
                 if (w === lastW && h === lastH) { return; }
                 lastW = w;
                 lastH = h;
                 updateMinZoom();
                 map.invalidateSize({pan: false});
               }, 100);
             }
             refreshSize();
             setTimeout(refreshSize, 0);
             setTimeout(refreshSize, 300);
             ", review_map_follow_js(follow_handler), "
           }"
        ))
    })

    shiny::observeEvent(
      input$map_zoom,
      {
        map_ready(TRUE)
      },
      ignoreNULL = TRUE,
      once = TRUE
    )

    shiny::observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      rid <- mapping_layer_id_record(click$id)
      if (!is.na(rid) && nzchar(rid)) {
        DT::selectRows(review_table_proxy, NULL)
        selected_occsclean_id(rid)
      }
    })

    selected_panel_actionable <- function(want) {
      info <- selected_panel_info()
      if (is.null(info)) {
        return(FALSE)
      }
      status <- info$map_status
      if (identical(want, "fail")) {
        return(status == "Flagged")
      }
      if (identical(want, "pass")) {
        return(status %in% c("Flagged", "Failed"))
      }
      if (identical(want, "flag")) {
        return(status %in% c("Passed", "Failed", "OK"))
      }
      FALSE
    }

    shiny::observe({
      shiny::req(isTRUE(map_ready()))
      invisible(app_state$rev)
      s <- app_state$session
      pts <- map_display_points()
      colors <- visualize_status_colors()
      do_fit <- isTRUE(shiny::isolate(should_fit_bounds()))
      old_ids <- shiny::isolate(marker_layer_ids())
      full_redraw <- isTRUE(do_fit) || length(old_ids) < 1L
      sel_id <- shiny::isolate(selected_occsclean_id())

      proxy <- leaflet::leafletProxy("map", session = session)
      if (isTRUE(full_redraw)) {
        proxy <- proxy |>
          leaflet::clearMarkers() |>
          leaflet::clearShapes()
      } else if (length(old_ids) > 0) {
        proxy <- leaflet::removeMarker(proxy, layerId = old_ids)
      }

      study <- if (isTRUE(s$has_data())) s$get_study_area() else NULL
      if (isTRUE(full_redraw) && !is.null(study) && !is.null(study$geom) &&
            length(study$geom) > 0) {
        geom_draw <- expand_study_area_for_wrap(study$geom)
        poly_sf <- sf::st_sf(geometry = geom_draw)
        proxy <- proxy |>
          leaflet::addPolygons(
            data = poly_sf,
            fillColor = "#4FC3F7",
            fillOpacity = 0.28,
            color = "#0288D1",
            weight = 1.5,
            opacity = 0.85,
            group = "Study area",
            options = leaflet::pathOptions(clickable = FALSE)
          )
      }

      if (nrow(pts) < 1) {
        marker_layer_ids(character())
        if (isTRUE(do_fit)) {
          should_fit_bounds(FALSE)
        }
        return(invisible(NULL))
      }

      draw <- expand_visualize_points_for_wrap(pts)
      pal <- unname(colors[draw$map_status])
      pal[is.na(pal)] <- "#6c757d"
      sel_chr <- as.character(sel_id %||% "")
      is_sel <- nzchar(sel_chr) &
        as.character(draw$occsclean_id) == sel_chr
      radii <- ifelse(is_sel, 9, 5)
      strokes <- ifelse(is_sel, "#111111", "#212529")
      stroke_w <- ifelse(is_sel, 2.5, 0.5)

      true_lon <- ((as.numeric(draw$longitude) + 180) %% 360) - 180
      coord_txt <- paste0(
        "Longitude: ", format(true_lon, scientific = FALSE, trim = TRUE),
        "<br/>Latitude: ", format(draw$latitude, scientific = FALSE, trim = TRUE)
      )
      popup <- paste0(
        "<strong>",
        htmltools::htmlEscape(
          ifelse(
            !is.na(draw$taxon) & nzchar(as.character(draw$taxon)),
            as.character(draw$taxon),
            "(no scientific name)"
          )
        ),
        "</strong>",
        "<br/>", coord_txt,
        "<br/>Review: ",
        htmltools::htmlEscape(visualize_status_label(draw$map_status)),
        ifelse(
          draw$n_flags > 0L,
          paste0(
            "<br/>Flags (", draw$n_flags, "): ",
            htmltools::htmlEscape(draw$flag_labels)
          ),
          ""
        )
      )

      layer_ids <- paste0(as.character(draw$occsclean_id), "||", seq_len(nrow(draw)))

      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = draw$longitude,
          lat = draw$latitude,
          layerId = layer_ids,
          radius = radii,
          color = strokes,
          weight = stroke_w,
          fillColor = pal,
          fillOpacity = 0.85,
          popup = popup
        )

      marker_layer_ids(layer_ids)

      recenter <- isTRUE(shiny::isolate(recenter_map_on_selected()))
      if (recenter) {
        recenter_map_on_selected(FALSE)
        sel_id <- shiny::isolate(selected_occsclean_id())
        if (!is.null(sel_id) && nzchar(as.character(sel_id))) {
          sel_hit <- pts[
            as.character(pts$occsclean_id) == as.character(sel_id),
            ,
            drop = FALSE
          ]
          if (nrow(sel_hit) > 0) {
            send_map_follow_payload(
              sel_id,
              sel_hit$longitude[[1]],
              sel_hit$latitude[[1]]
            )
          }
        }
      }

      if (isTRUE(do_fit)) {
        lng1 <- min(pts$longitude, na.rm = TRUE)
        lat1 <- min(pts$latitude, na.rm = TRUE)
        lng2 <- max(pts$longitude, na.rm = TRUE)
        lat2 <- max(pts$latitude, na.rm = TRUE)
        span_lng <- abs(lng2 - lng1)
        span_lat <- abs(lat2 - lat1)
        fit_max_zoom <- if (span_lng < 25 && span_lat < 20) {
          4L
        } else if (span_lng < 90 && span_lat < 60) {
          3L
        } else {
          2L
        }
        proxy <- proxy |>
          leaflet::fitBounds(
            lng1 = lng1,
            lat1 = lat1,
            lng2 = lng2,
            lat2 = lat2,
            options = list(maxZoom = fit_max_zoom, padding = c(40, 40))
          )
        should_fit_bounds(FALSE)
      }

      invisible(NULL)
    })

    selected_occsclean_ids <- function(occ_df, selected_rows) {
      shiny::req(nrow(occ_df) > 0)
      shiny::req(length(selected_rows) > 0)
      as.character(occ_df$occsclean_id[selected_rows])
    }

    shiny::observeEvent(input$delete_by_type, {
      check_label <- input$check_flag
      shiny::req(!review_check_flag_is_all(check_label))
      ids <- occsclean_ids_for_check_in_review(
        in_review(),
        raw_findings(),
        check_label,
        only = FALSE
      )
      if (length(ids) < 1) {
        shiny::showNotification(
          "No occurrences in Review carry that check flag.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "fail", batch = TRUE)
    })
    shiny::observeEvent(input$keep_by_type, {
      check_label <- input$check_flag
      shiny::req(!review_check_flag_is_all(check_label))
      ids <- occsclean_ids_for_check_in_review(
        in_review(),
        raw_findings(),
        check_label,
        only = TRUE
      )
      if (length(ids) < 1) {
        shiny::showNotification(
          paste(
            "No occurrences in Review have only that check flag.",
            "Records that also have other flags were left in Review."
          ),
          type = "warning"
        )
        return(invisible(NULL))
      }
      apply_occurrence_action(ids, "pass", batch = TRUE)
    })

    apply_occurrence_action <- function(record_ids, action, batch = FALSE) {
      ids <- unique(as.character(record_ids))
      ids <- ids[nzchar(ids)]
      shiny::req(length(ids) > 0)
      s <- app_state$session
      findings <- s$get_findings_table()
      n <- length(ids)
      label <- switch(
        action,
        fail = if (isTRUE(batch)) "batch failed" else "failed",
        pass = if (isTRUE(batch)) "batch passed" else "passed",
        review = "returned to Review",
        action
      )

      shiny::withProgress(
        message = "Updating occurrence decisions",
        detail = paste0(format(n, big.mark = ","), " records"),
        value = 0.2,
        {
          if (identical(action, "fail")) {
            fail_records(
              s$get_decisions(),
              ids,
              findings = findings,
              batch = isTRUE(batch)
            )
          } else if (identical(action, "pass")) {
            pass_records(
              s$get_decisions(),
              ids,
              findings = findings,
              batch = isTRUE(batch)
            )
          } else if (identical(action, "review")) {
            info <- selected_panel_info()
            if (!is.null(info) && identical(info$map_status, "OK")) {
              flag_unflagged_for_manual_review(s$get_decisions(), ids)
            } else {
              return_records_to_review(
                s$get_decisions(),
                ids,
                findings = findings
              )
            }
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

    apply_selected_panel_action <- function(action) {
      rid <- selected_occsclean_id()
      shiny::req(!is.null(rid), nzchar(as.character(rid)))
      recenter_map_on_selected(TRUE)
      apply_occurrence_action(as.character(rid), action)
    }

    shiny::observeEvent(input$sel_pass, {
      shiny::req(selected_panel_actionable("pass"))
      apply_selected_panel_action("pass")
    })

    shiny::observeEvent(input$sel_fail, {
      shiny::req(selected_panel_actionable("fail"))
      apply_selected_panel_action("fail")
    })

    shiny::observeEvent(input$sel_flag_record, {
      shiny::req(selected_panel_actionable("flag"))
      apply_selected_panel_action("review")
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
          "Filter Status to Batch Failed to see these records again after changes."
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
      apply_occurrence_action(as.character(rows$occsclean_id), "fail", batch = TRUE)
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
