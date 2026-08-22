#' Mapping step UI
#' @param id Module id.
#' @noRd
mod_mapping_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .oc-mapping-map {
        width: 100%;
        height: 75vh;
        min-height: 600px;
        margin-top: 0.5rem;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        overflow: hidden;
      }
      .oc-mapping-map .leaflet,
      .oc-mapping-map .leaflet-container {
        height: 100% !important;
        width: 100% !important;
        background: #f0f0f0;
      }
      .oc-mapping-selected {
        margin-top: 0.75rem;
        margin-bottom: 0.25rem;
        padding: 0.75rem 1rem;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        background: #f8f9fa;
        min-height: 12.5rem;
        box-sizing: border-box;
      }
      .oc-mapping-selected-details {
        min-height: 5.5rem;
      }
      .oc-mapping-selected-actions {
        min-height: 2.4rem;
        margin-top: 0.5rem;
      }
    ")),
    shiny::h3("Mapping"),
    shiny::uiOutput(ns("warning")),
    shiny::p(
      "Click a plotted occurrence that has a flag associated to pass or fail.",
      "Failed records will be removed from the cleaned CSV export."
    ),
    shiny::p(
      class = "text-muted",
      style = "font-size: 0.9rem;",
      "Points are plotted from decimalLongitude / decimalLatitude as WGS84",
      "(EPSG:4326) geographic coordinates. The basemap is for context only",
      "(Web Mercator display); it does not reproject or alter your coordinates."
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("view_mode"),
          "Show points",
          choices = c(
            "All records" = "all",
            "Flagged records" = "flagged",
            "Failed records" = "removed",
            "Passed records" = "kept",
            "Unflagged records" = "ok",
            "Unflagged + passed records" = "ok_kept"
          ),
          selected = "all"
        ),
        shiny::checkboxInput(
          ns("cluster_points"),
          "Collapse nearby points",
          value = FALSE
        )
      ),
      shiny::column(
        8,
        shiny::verbatimTextOutput(ns("status"))
      )
    ),
    shiny::div(
      class = "oc-mapping-selected",
      shiny::tags$strong("Selected record"),
      shiny::uiOutput(ns("selected_details")),
      shiny::uiOutput(ns("selected_actions")),
      shiny::uiOutput(ns("selected_help"))
    ),
    shiny::div(
      class = "oc-mapping-map",
      leaflet::leafletOutput(ns("map"), height = "100%")
    ),
    shiny::br(),
    shiny::h4("Mapped points by review category"),
    shiny::tableOutput(ns("legend_counts"))
  )
}

#' Mapping step server
#' @param id Module id.
#' @param app_state Shared reactiveValues (session + rev).
#' @noRd
mod_mapping_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_occsclean_id <- shiny::reactiveVal(NULL)
    should_fit_bounds <- shiny::reactiveVal(TRUE)
    marker_layer_ids <- shiny::reactiveVal(character())
    last_cluster_mode <- shiny::reactiveVal(NULL)

    output$warning <- shiny::renderUI({
      invisible(app_state$rev)
      if (isTRUE(app_state$session$has_data())) {
        return(NULL)
      }
      workflow_warning_ui()
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

    view_mode <- shiny::reactive({
      input$view_mode %||% "all"
    })

    display_records <- shiny::reactive({
      filter_visualize_points(map_records(), view_mode())
    })

    display_points <- shiny::reactive({
      rec <- display_records()
      if (nrow(rec) < 1) {
        return(rec)
      }
      rec[rec$mappable, , drop = FALSE]
    })

    selected_row <- shiny::reactive({
      rid <- selected_occsclean_id()
      if (is.null(rid) || !nzchar(as.character(rid))) {
        return(NULL)
      }
      rec <- map_records()
      hit <- rec[as.character(rec$occsclean_id) == as.character(rid), , drop = FALSE]
      if (nrow(hit) < 1) {
        return(NULL)
      }
      hit[1, , drop = FALSE]
    })

    shiny::observeEvent(view_mode(), {
      should_fit_bounds(TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cluster_points, {
      should_fit_bounds(TRUE)
    }, ignoreInit = TRUE)

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

    output$status <- shiny::renderText({
      invisible(app_state$rev)
      s <- app_state$session
      if (!s$has_data()) {
        return("No data loaded. Import a file first.")
      }

      mode <- view_mode()
      mode_label <- switch(
        mode,
        flagged = "Flagged records",
        removed = "Failed records",
        kept = "Passed records",
        ok = "Unflagged records",
        ok_kept = "Unflagged + passed records",
        "All records"
      )
      summary <- summarize_visualize_view(map_records(), mode)

      lines <- c(
        paste0("View: ", mode_label),
        paste0("Records in this view: ", summary$n_records),
        paste0("Mapped on screen: ", summary$n_mapped),
        paste0("Missing coordinates (this view): ", summary$n_missing_coords),
        paste0("Cached checks: ", length(s$get_assessment()))
      )

      if (summary$n_mapped < 1 && summary$n_missing_coords > 0) {
        lines <- c(
          lines,
          "",
          "No points to plot for this view because these records lack mappable coordinates."
        )
      } else if (summary$n_mapped < 1) {
        lines <- c(lines, "", "No records match this view.")
      }

      paste(lines, collapse = "\n")
    })

    output$selected_details <- shiny::renderUI({
      invisible(app_state$rev)
      row <- selected_row()
      shiny::div(
        class = "oc-mapping-selected-details",
        if (is.null(row)) {
          shiny::tagList(
            shiny::tags$div(class = "text-muted", "No point selected."),
            shiny::tags$div(class = "text-muted", "Click a map point to inspect it.")
          )
        } else {
          taxon <- row$taxon[[1]]
          taxon_txt <- if (!is.na(taxon) && nzchar(taxon)) {
            taxon
          } else {
            "(no scientific name)"
          }
          lon <- row$longitude[[1]]
          lat <- row$latitude[[1]]
          flags <- row$flag_labels[[1]]
          shiny::tagList(
            shiny::tags$div(paste0("Taxon: ", taxon_txt)),
            shiny::tags$div(paste0(
              "Coordinates: ",
              format(lon, scientific = FALSE, trim = TRUE), ", ",
              format(lat, scientific = FALSE, trim = TRUE)
            )),
            shiny::tags$div(paste0(
              "Review status: ",
              visualize_status_label(row$map_status[[1]])
            )),
            shiny::tags$div(paste0(
              "Flags: ",
              if (!is.na(flags) && nzchar(flags)) flags else "(none)"
            ))
          )
        }
      )
    })

    output$selected_actions <- shiny::renderUI({
      invisible(app_state$rev)
      row <- selected_row()
      status <- if (is.null(row)) {
        NA_character_
      } else {
        as.character(row$map_status[[1]])
      }

      buttons <- list()
      if (identical(status, "Flagged")) {
        buttons <- list(
          shiny::actionButton(
            session$ns("map_mark_delete"),
            "Fail",
            class = "btn-danger"
          ),
          shiny::actionButton(
            session$ns("map_keep"),
            "Pass",
            class = "btn-success"
          )
        )
      } else if (identical(status, "Passed")) {
        buttons <- list(
          shiny::actionButton(
            session$ns("map_mark_delete"),
            "Fail",
            class = "btn-danger"
          )
        )
      } else if (identical(status, "Failed")) {
        buttons <- list(
          shiny::actionButton(
            session$ns("map_keep"),
            "Pass",
            class = "btn-success"
          )
        )
      }

      shiny::div(
        class = "oc-mapping-selected-actions",
        if (length(buttons) > 0) {
          shiny::div(
            class = "btn-group",
            role = "group",
            buttons
          )
        }
      )
    })

    output$selected_help <- shiny::renderUI({
      invisible(app_state$rev)
      row <- selected_row()
      help_txt <- if (is.null(row)) {
        "Select a flagged point on the map to Pass or Fail it."
      } else {
        status <- as.character(row$map_status[[1]])
        if (identical(status, "Failed")) {
          "This record failed review. Use Pass to recover it."
        } else if (identical(status, "Passed")) {
          "This record passed review. Use Fail if you need to exclude it."
        } else if (identical(status, "Flagged")) {
          "Use Pass to accept this flagged record, or Fail to exclude it."
        } else {
          "Unflagged points are for context only. Pass/Fail is for flagged records."
        }
      }
      shiny::tags$p(
        class = "text-muted",
        style = "margin: 0.5rem 0 0 0; font-size: 0.9rem;",
        help_txt
      )
    })

    output$legend_counts <- shiny::renderTable({
      pts <- map_records()
      shiny::req(nrow(pts) > 0)
      mapped <- pts[pts$mappable, , drop = FALSE]
      shiny::req(nrow(mapped) > 0)
      counts <- as.data.frame(
        table(Category = visualize_status_label(mapped$map_status)),
        stringsAsFactors = FALSE
      )
      names(counts) <- c("Category", "Mapped points")
      order <- c(
        "Unflagged",
        "Flagged",
        "Passed",
        "Failed"
      )
      counts <- counts[order(match(counts$Category, order)), , drop = FALSE]
      total <- data.frame(
        Category = "Total",
        `Mapped points` = sum(as.integer(counts[["Mapped points"]])),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      rbind(counts, total)
    })

    output$map <- leaflet::renderLeaflet({
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
        htmlwidgets::onRender(
          "function(el, x) {
             var map = this;
             function updateMinZoom() {
               var w = el.clientWidth || 1;
               var minZ = Math.max(2, Math.ceil(Math.log(w / 256) / Math.LN2));
               map.setMinZoom(minZ);
               if (map.getZoom() < minZ) {
                 map.setZoom(minZ);
               }
             }
             updateMinZoom();
             map.on('resize', updateMinZoom);
           }"
        )
    })

    map_ready <- shiny::reactiveVal(FALSE)
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
        selected_occsclean_id(rid)
      }
    })

    selected_is_actionable <- function(want) {
      row <- selected_row()
      if (is.null(row)) {
        return(FALSE)
      }
      status <- as.character(row$map_status[[1]])
      if (identical(want, "fail")) {
        return(status %in% c("Flagged", "Passed"))
      }
      if (identical(want, "pass")) {
        return(status %in% c("Flagged", "Failed"))
      }
      FALSE
    }

    shiny::observeEvent(input$map_mark_delete, {
      shiny::req(selected_is_actionable("fail"))
      rid <- selected_occsclean_id()
      shiny::req(!is.null(rid), nzchar(as.character(rid)))
      s <- app_state$session
      shiny::req(s$has_data())

      if (as.character(rid) %in% s$get_decisions()$removed_occsclean_ids()) {
        shiny::showNotification(
          paste0(rid, " is already failed."),
          type = "warning"
        )
        return(invisible(NULL))
      }

      findings <- NULL
      if (length(s$get_assessment()) > 0) {
        findings <- s$get_findings_table()
      }
      mark_record_for_deletion(s$get_decisions(), rid, findings = findings)
      s$touch()
      bump_app_state(app_state)
      shiny::showNotification(
        paste0(rid, " failed."),
        type = "message"
      )
    })

    shiny::observeEvent(input$map_keep, {
      shiny::req(selected_is_actionable("pass"))
      rid <- selected_occsclean_id()
      shiny::req(!is.null(rid), nzchar(as.character(rid)))
      s <- app_state$session
      shiny::req(s$has_data())

      findings <- NULL
      if (length(s$get_assessment()) > 0) {
        findings <- s$get_findings_table()
      }
      n <- keep_record_from_mapping(
        s$get_decisions(),
        rid,
        findings = findings
      )
      if (n < 1) {
        shiny::showNotification(
          paste0(rid, " could not be passed."),
          type = "warning"
        )
        return(invisible(NULL))
      }
      s$touch()
      bump_app_state(app_state)
      shiny::showNotification(
        paste0(rid, " passed."),
        type = "message"
      )
    })

    shiny::observe({
      shiny::req(isTRUE(map_ready()))
      invisible(app_state$rev)
      s <- app_state$session
      pts <- display_points()
      cluster <- isTRUE(input$cluster_points)
      colors <- visualize_status_colors()
      do_fit <- isTRUE(shiny::isolate(should_fit_bounds()))
      old_ids <- shiny::isolate(marker_layer_ids())
      prev_cluster <- shiny::isolate(last_cluster_mode())
      cluster_changed <- !identical(prev_cluster, cluster)
      full_redraw <- isTRUE(do_fit) || isTRUE(cluster_changed) ||
        length(old_ids) < 1L

      proxy <- leaflet::leafletProxy("map", session = session)
      if (isTRUE(full_redraw)) {
        proxy <- proxy |>
          leaflet::clearMarkers() |>
          leaflet::clearMarkerClusters() |>
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
        last_cluster_mode(cluster)
        if (isTRUE(do_fit)) {
          should_fit_bounds(FALSE)
        }
        return(invisible(NULL))
      }

      # Repeat markers at lon +/- 360 for dateline panning.
      draw <- expand_visualize_points_for_wrap(pts)

      pal <- unname(colors[draw$map_status])
      pal[is.na(pal)] <- "#6c757d"

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
        ),
        "<br/><em>Flagged points can be Passed or Failed in the panel above.</em>"
      )

      cluster_opts <- if (isTRUE(cluster)) {
        leaflet::markerClusterOptions(showCoverageOnHover = FALSE)
      } else {
        NULL
      }

      layer_ids <- paste0(as.character(draw$occsclean_id), "||", seq_len(nrow(draw)))

      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = draw$longitude,
          lat = draw$latitude,
          layerId = layer_ids,
          radius = 5,
          color = "#212529",
          weight = 0.5,
          fillColor = pal,
          fillOpacity = 0.85,
          popup = popup,
          clusterOptions = cluster_opts
        )

      marker_layer_ids(layer_ids)
      last_cluster_mode(cluster)

      if (isTRUE(do_fit)) {
        proxy <- proxy |>
          leaflet::fitBounds(
            lng1 = min(pts$longitude, na.rm = TRUE),
            lat1 = min(pts$latitude, na.rm = TRUE),
            lng2 = max(pts$longitude, na.rm = TRUE),
            lat2 = max(pts$latitude, na.rm = TRUE),
            options = list(maxZoom = 8, padding = c(20, 20))
          )
        should_fit_bounds(FALSE)
      }

      invisible(NULL)
    })
  })
}
