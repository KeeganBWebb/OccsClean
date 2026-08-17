#' Read an uploaded study-area polygon (shapefile or zip)
#'
#' @param datapaths Character vector of local temp paths from Shiny.
#' @param names Optional original file names.
#' @noRd
read_area_polygon_upload <- function(datapaths, names = NULL) {
  datapaths <- as.character(datapaths)
  datapaths <- datapaths[nzchar(datapaths) & file.exists(datapaths)]
  if (length(datapaths) < 1) {
    rlang::abort("No shapefile upload was provided.", call = NULL)
  }
  if (is.null(names) || length(names) != length(datapaths)) {
    names <- basename(datapaths)
  }
  names <- as.character(names)

  td <- tempfile("oc_area_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  is_zip <- grepl("\\.zip$", names, ignore.case = TRUE) |
    grepl("\\.zip$", datapaths, ignore.case = TRUE)

  if (any(is_zip)) {
    zip_path <- datapaths[is_zip][[1]]
    utils::unzip(zip_path, exdir = td)
  } else {
    for (i in seq_along(datapaths)) {
      dest <- file.path(td, names[[i]])
      subdir <- dirname(dest)
      if (!identical(subdir, td) && !dir.exists(subdir)) {
        dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
      }
      ok <- file.copy(datapaths[[i]], dest, overwrite = TRUE)
      if (!isTRUE(ok)) {
        rlang::abort(
          paste0("Could not stage uploaded file: ", names[[i]]),
          call = NULL
        )
      }
    }
  }

  shp_files <- list.files(td, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp_files) < 1) {
    rlang::abort(
      "No .shp file found. Upload a zipped shapefile or the .shp/.shx/.dbf set.",
      call = NULL
    )
  }
  if (length(shp_files) > 1) {
    rlang::abort(
      "Multiple .shp files were found. Upload a single shapefile (or one zip).",
      call = NULL
    )
  }

  area <- tryCatch(
    sf::st_read(shp_files[[1]], quiet = TRUE),
    error = function(e) {
      rlang::abort(
        paste0("Could not read shapefile: ", conditionMessage(e)),
        call = NULL
      )
    }
  )

  prepare_area_polygon(area)
}

#' Validate and normalize an area polygon to WGS84
#' @param area An sf or sfc object.
#' @noRd
prepare_area_polygon <- function(area) {
  if (inherits(area, "sf")) {
    geom <- sf::st_geometry(area)
  } else if (inherits(area, "sfc")) {
    geom <- area
  } else {
    rlang::abort("`area` must be an sf or sfc object.", call = NULL)
  }

  geom <- sf::st_zm(geom, drop = TRUE, what = "ZM")

  crs <- sf::st_crs(geom)
  if (is.na(crs)) {
    sf::st_crs(geom) <- 4326
  }

  geographic <- isTRUE(sf::st_is_longlat(geom))

  s2_was_on <- FALSE
  if (isTRUE(geographic) && isTRUE(sf::sf_use_s2())) {
    s2_was_on <- TRUE
    suppressMessages(sf::sf_use_s2(FALSE))
  }
  on.exit({
    if (isTRUE(s2_was_on)) {
      suppressMessages(sf::sf_use_s2(TRUE))
    }
  }, add = TRUE)

  validity_reasons <- tryCatch(
    as.character(sf::st_is_valid(geom, reason = TRUE)),
    error = function(e) {
      paste0("Could not evaluate validity: ", conditionMessage(e))
    }
  )
  was_invalid <- tryCatch(
    {
      v <- sf::st_is_valid(geom)
      !all(as.logical(v), na.rm = TRUE)
    },
    error = function(e) TRUE
  )

  repair_message <- NULL
  if (isTRUE(was_invalid)) {
    reason_txt <- validity_reasons
    if (length(reason_txt) > 1) {
      reason_txt <- reason_txt[!grepl("^Valid Geometry$", reason_txt, ignore.case = TRUE)]
      reason_txt <- paste(unique(reason_txt), collapse = "; ")
    }
    if (!nzchar(reason_txt)) {
      reason_txt <- "self-intersecting or otherwise invalid polygon rings"
    }

    geom <- tryCatch(
      sf::st_make_valid(geom),
      error = function(e) {
        rlang::abort(
          paste0(
            "Uploaded shapefile has invalid polygon geometry that could not be repaired. ",
            "Detail: ", conditionMessage(e),
            " Fix the shapefile in a GIS (e.g. 'Fix geometries') and re-upload."
          ),
          call = NULL
        )
      }
    )

    still_invalid <- tryCatch(
      !all(sf::st_is_valid(geom), na.rm = TRUE),
      error = function(e) TRUE
    )
    if (isTRUE(still_invalid)) {
      rlang::abort(
        paste0(
          "Uploaded shapefile has invalid polygon geometry that could not be repaired ",
          "(original issue: ", reason_txt, "). ",
          "Fix the shapefile in a GIS and re-upload."
        ),
        call = NULL
      )
    }

    repair_message <- paste0(
      "Uploaded shapefile had INVALID geometry (", reason_txt, "). ",
      "OccsClean temporarily repaired it with sf::st_make_valid() for this ",
      "session only; your file on disk was not changed. ",
      "Distance / outside-area flags use the repaired shape. ",
      "For a definitive analysis, fix the shapefile in a GIS and re-upload."
    )
  }

  geom <- extract_polygonal_geom(geom)

  types <- unique(as.character(sf::st_geometry_type(geom, by_geometry = TRUE)))
  ok_types <- c("POLYGON", "MULTIPOLYGON")
  if (!any(types %in% ok_types)) {
    rlang::abort(
      paste0(
        "Study area must contain polygon geometries after validation. Found: ",
        paste(types, collapse = ", ")
      ),
      call = NULL
    )
  }

  dissolved <- dissolve_area_geom(geom)
  geom <- dissolved$geom
  if (isTRUE(dissolved$used_fallback)) {
    fallback_note <- paste0(
      "Uploaded shapefile had topology problems when building one study-area ",
      "polygon (", dissolved$detail, "). OccsClean kept a multipolygon study ",
      "area for this session only (overlaps were not dissolved); your file on ",
      "disk was not changed. Distance / outside-area flags use this shape. ",
      "For a definitive analysis, fix the shapefile in a GIS and re-upload."
    )
    if (is.null(repair_message)) {
      repair_message <- fallback_note
    } else {
      repair_message <- paste(repair_message, fallback_note, sep = " ")
    }
    was_invalid <- TRUE
  }

  if (!isTRUE(sf::st_is_longlat(geom))) {
    geom <- sf::st_transform(geom, 4326)
  } else {
    epsg <- suppressWarnings(as.integer(sf::st_crs(geom)$epsg))
    if (!isTRUE(identical(epsg, 4326L))) {
      geom <- sf::st_transform(geom, 4326)
    }
  }

  geom <- wrap_area_dateline(geom)

  list(
    geom = geom,
    geometry_repaired = isTRUE(was_invalid),
    repair_message = repair_message
  )
}

#' Keep polygonal parts only from make_valid / collection results
#' @noRd
extract_polygonal_geom <- function(geom) {
  tryCatch(
    {
      types <- unique(as.character(sf::st_geometry_type(geom, by_geometry = TRUE)))
      if (any(types %in% c("GEOMETRYCOLLECTION", "MULTISURFACE"))) {
        sf::st_collection_extract(geom, "POLYGON")
      } else {
        geom
      }
    },
    error = function(e) geom
  )
}

#' Does a geographic polygon appear to cross the antimeridian?
#' @noRd
area_crosses_dateline <- function(geom) {
  if (!isTRUE(sf::st_is_longlat(geom))) {
    return(FALSE)
  }
  bb <- sf::st_bbox(geom)
  span <- as.numeric(bb["xmax"]) - as.numeric(bb["xmin"])
  if (is.finite(span) && span > 180) {
    return(TRUE)
  }
  xy <- tryCatch(sf::st_coordinates(geom)[, 1], error = function(e) NULL)
  if (is.null(xy) || length(xy) < 1) {
    return(FALSE)
  }
  any(xy > 170, na.rm = TRUE) && any(xy < -170, na.rm = TRUE)
}

#' Split antimeridian-crossing WGS84 polygons
#' @noRd
wrap_area_dateline <- function(geom) {
  wrapped <- tryCatch(
    sf::st_wrap_dateline(
      geom,
      options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")
    ),
    error = function(e) geom
  )

  s2_was_on <- FALSE
  if (isTRUE(sf::sf_use_s2())) {
    s2_was_on <- TRUE
    suppressMessages(sf::sf_use_s2(FALSE))
  }
  on.exit({
    if (isTRUE(s2_was_on)) {
      suppressMessages(sf::sf_use_s2(TRUE))
    }
  }, add = TRUE)

  wrapped <- tryCatch(
    suppressWarnings(sf::st_make_valid(wrapped)),
    error = function(e) wrapped
  )
  wrapped <- extract_polygonal_geom(wrapped)
  parts <- tryCatch(
    suppressWarnings(sf::st_cast(wrapped, "POLYGON")),
    error = function(e) wrapped
  )

  if (length(parts) > 1) {
    keep <- vapply(seq_along(parts), function(i) {
      bb <- sf::st_bbox(parts[i])
      span <- as.numeric(bb["xmax"]) - as.numeric(bb["xmin"])
      is.finite(span) && span < 180
    }, logical(1))
    if (any(keep) && !all(keep)) {
      parts <- parts[keep]
    }
  }

  tryCatch(
    suppressWarnings(sf::st_make_valid(sf::st_combine(parts))),
    error = function(e) parts
  )
}

#' Dissolve study-area parts into one geometry
#' @param geom sfc POLYGON or MULTIPOLYGON.
#' @noRd
dissolve_area_geom <- function(geom) {
  detail <- NULL

  try_union <- function(x) {
    tryCatch(
      suppressWarnings(sf::st_make_valid(sf::st_union(x))),
      error = function(e) e
    )
  }

  geographic <- isTRUE(sf::st_is_longlat(geom))

  if (!isTRUE(geographic)) {
    u <- try_union(geom)
    if (!inherits(u, "error")) {
      return(list(geom = u, used_fallback = FALSE, detail = NULL))
    }
    detail <- conditionMessage(u)
    buffered <- try_union(sf::st_buffer(geom, 0))
    if (!inherits(buffered, "error")) {
      return(list(geom = buffered, used_fallback = TRUE, detail = detail))
    }
    combined <- tryCatch(
      suppressWarnings(sf::st_make_valid(sf::st_combine(geom))),
      error = function(e) e
    )
    if (inherits(combined, "error")) {
      rlang::abort(
        paste0(
          "Could not build a study-area polygon from the upload. ",
          "Detail: ", detail, ". ",
          "Fix the shapefile topology in a GIS (e.g. 'Fix geometries') and re-upload."
        ),
        call = NULL
      )
    }
    return(list(geom = combined, used_fallback = TRUE, detail = detail))
  }

  work <- geom
  if (isTRUE(area_crosses_dateline(geom))) {
    work <- tryCatch(sf::st_shift_longitude(geom), error = function(e) geom)
  }

  projected <- tryCatch(
    {
      crs_aeqd <- aeqd_crs_for_geom(work)
      try_union(sf::st_transform(work, crs_aeqd))
    },
    error = function(e) e
  )
  if (!inherits(projected, "error")) {
    out <- tryCatch(
      sf::st_make_valid(sf::st_transform(projected, 4326)),
      error = function(e) e
    )
    if (!inherits(out, "error")) {
      return(list(geom = out, used_fallback = FALSE, detail = NULL))
    }
    detail <- conditionMessage(out)
  } else {
    detail <- conditionMessage(projected)
  }

  geographic_u <- try_union(work)
  if (!inherits(geographic_u, "error")) {
    return(list(geom = geographic_u, used_fallback = FALSE, detail = NULL))
  }
  detail <- conditionMessage(geographic_u)

  buffered <- tryCatch(
    {
      crs_aeqd <- aeqd_crs_for_geom(work)
      g_p <- sf::st_transform(work, crs_aeqd)
      u <- try_union(sf::st_buffer(g_p, 0))
      if (inherits(u, "error")) {
        u
      } else {
        sf::st_make_valid(sf::st_transform(u, 4326))
      }
    },
    error = function(e) e
  )
  if (!inherits(buffered, "error")) {
    return(list(geom = buffered, used_fallback = TRUE, detail = detail))
  }

  combined <- tryCatch(
    {
      cgeom <- sf::st_make_valid(sf::st_combine(work))
      cgeom <- extract_polygonal_geom(cgeom)
      if (!all(as.logical(sf::st_is_valid(cgeom)), na.rm = TRUE)) {
        rlang::abort("Combined study-area geometry is still invalid.", call = NULL)
      }
      cgeom
    },
    error = function(e) e
  )
  if (inherits(combined, "error")) {
    rlang::abort(
      paste0(
        "Could not build a study-area polygon from the upload. ",
        "Detail: ", detail, ". ",
        "Fix the shapefile topology in a GIS (e.g. 'Fix geometries') and re-upload."
      ),
      call = NULL
    )
  }

  list(geom = combined, used_fallback = TRUE, detail = detail)
}

#' Local azimuthal equidistant CRS centered on a geometry
#' @noRd
aeqd_crs_for_geom <- function(geom) {
  if (isTRUE(sf::st_is_longlat(geom))) {
    xy <- tryCatch(
      sf::st_coordinates(geom),
      error = function(e) NULL
    )
    if (!is.null(xy) && nrow(xy) > 0) {
      lon_rad <- xy[, 1] * pi / 180
      mid_lon <- atan2(mean(sin(lon_rad)), mean(cos(lon_rad))) * 180 / pi
      mid_lat <- mean(range(xy[, 2], na.rm = TRUE))
    } else {
      bb <- sf::st_bbox(geom)
      mid_lon <- mean(c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])))
      mid_lat <- mean(c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])))
    }
  } else {
    cen <- tryCatch(
      sf::st_transform(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(geom))), 4326),
      error = function(e) NULL
    )
    if (!is.null(cen)) {
      xy <- sf::st_coordinates(cen)
      mid_lon <- xy[1, 1]
      mid_lat <- xy[1, 2]
    } else {
      mid_lon <- 0
      mid_lat <- 0
    }
  }
  paste0(
    "+proj=aeqd +lat_0=", mid_lat,
    " +lon_0=", mid_lon,
    " +datum=WGS84 +units=m +no_defs"
  )
}


#' Nearest distance from points to a polygon, in meters
#' @param pts sf POINT geometries (WGS84).
#' @param area_geom sfc polygon (WGS84).
#' @noRd
distance_to_polygon_meters <- function(pts, area_geom) {
  crs_aeqd <- aeqd_crs_for_geom(area_geom)
  pts_p <- sf::st_transform(pts, crs_aeqd)
  area_p <- sf::st_transform(area_geom, crs_aeqd)
  d <- sf::st_distance(pts_p, area_p)
  as.numeric(d[, 1])
}

#' Parse optional outside-distance threshold in meters
#' @param x Raw value (character or numeric).
#' @noRd
parse_outside_distance_m <- function(x) {
  if (is.null(x) || length(x) < 1 || (length(x) == 1 && is.na(x[[1]]))) {
    return(NULL)
  }
  if (is.character(x)) {
    txt <- trimws(x[[1]])
    if (!nzchar(txt)) {
      return(NULL)
    }
    val <- suppressWarnings(as.numeric(txt))
  } else {
    val <- suppressWarnings(as.numeric(x[[1]]))
  }
  if (!is.finite(val)) {
    rlang::abort(
      "Outside-distance threshold must be a number (meters), or left blank.",
      call = NULL
    )
  }
  if (val < 0) {
    rlang::abort("Outside-distance threshold cannot be negative.", call = NULL)
  }
  if (val == 0) {
    return(NULL)
  }
  val
}

#' Flag coordinates outside an uploaded study area
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_outside_area <- function(occ, params = list()) {
  check_id <- "coord_outside_area"
  label <- "Outside study area"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  raw_threshold <- params$outside_distance_m %||% params$buffer_m
  threshold_m <- tryCatch(
    parse_outside_distance_m(raw_threshold),
    error = function(e) e
  )
  if (inherits(threshold_m, "error")) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "error",
      findings = empty_findings(),
      params_used = list(
        lon_col = req$lon_col,
        lat_col = req$lat_col,
        area_source = params$area_source %||% NA_character_,
        outside_distance_m = raw_threshold %||% NA
      ),
      engine = "sf",
      summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = nrow(occ)),
      messages = conditionMessage(threshold_m)
    ))
  }

  area_geom <- params$area_geom %||% NULL
  geometry_repaired <- isTRUE(params$geometry_repaired)
  repair_message <- params$repair_message %||% NULL
  if (is.null(area_geom) && !is.null(params$area_sf)) {
    area_ok <- TRUE
    prepared <- tryCatch(
      prepare_area_polygon(params$area_sf),
      error = function(e) {
        area_ok <<- FALSE
        conditionMessage(e)
      }
    )
    if (!isTRUE(area_ok)) {
      return(new_occ_check_result(
        check_id = check_id,
        label = label,
        category = category,
        status = "error",
        findings = empty_findings(),
        params_used = list(
          lon_col = req$lon_col,
          lat_col = req$lat_col,
          area_source = params$area_source %||% NA_character_,
          outside_distance_m = threshold_m %||% NA_real_,
          geometry_repaired = FALSE
        ),
        engine = "sf",
        summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = nrow(occ)),
        messages = prepared
      ))
    }
    area_geom <- prepared$geom
    geometry_repaired <- isTRUE(prepared$geometry_repaired)
    repair_message <- prepared$repair_message
  }

  if (is.null(area_geom)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Upload a study-area / range shapefile to run this check.",
      params_used = params[setdiff(names(params), c("area_sf", "area_geom"))]
    ))
  }

  lon <- as_numeric_silent(occ[[req$lon_col]])
  lat <- as_numeric_silent(occ[[req$lat_col]])
  usable <- is.finite(lon) & is.finite(lat) &
    lon >= -180 & lon <= 180 &
    lat >= -90 & lat <= 90

  params_meta <- list(
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    area_source = params$area_source %||% NA_character_,
    outside_distance_m = threshold_m %||% NA_real_,
    geometry_repaired = geometry_repaired
  )

  result_messages <- character()
  if (isTRUE(geometry_repaired) && !is.null(repair_message) && nzchar(repair_message)) {
    result_messages <- repair_message
  }

  if (!any(usable)) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "ok",
      findings = empty_findings(),
      params_used = params_meta,
      engine = "sf",
      summary = list(
        n_checked = 0L,
        n_flagged = 0L,
        n_skipped_rows = as.integer(nrow(occ))
      ),
      messages = c(result_messages, "No finite coordinates available to evaluate.")
    ))
  }

  pts <- sf::st_as_sf(
    data.frame(
      lon = lon[usable],
      lat = lat[usable]
    ),
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  dist_m <- tryCatch(
    distance_to_polygon_meters(pts, area_geom),
    error = function(e) e
  )
  if (inherits(dist_m, "error")) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "error",
      findings = empty_findings(),
      params_used = params_meta,
      engine = "sf",
      summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = nrow(occ)),
      messages = paste0("Could not compute distances: ", conditionMessage(dist_m))
    ))
  }

  limit_m <- if (is.null(threshold_m)) 0 else threshold_m
  flag_local <- is.finite(dist_m) & dist_m > limit_m
  idx_local <- which(flag_local)
  idx <- which(usable)[idx_local]
  dist_flagged <- dist_m[idx_local]

  reason <- if (is.null(threshold_m)) {
    "Coordinate falls outside the uploaded study area / range polygon."
  } else {
    paste0(
      "Nearest distance to the study area / range polygon exceeds ",
      threshold_m, " m."
    )
  }

  evidence <- paste0(
    req$lon_col, "=", format_evidence_value(occ[[req$lon_col]][idx]), ";",
    req$lat_col, "=", format_evidence_value(occ[[req$lat_col]][idx]), ";",
    "distance_m=", format(round(dist_flagged, 1), scientific = FALSE, trim = TRUE)
  )

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "COORD_OUTSIDE_AREA",
    reason = reason,
    evidence = evidence,
    recommended_action = "remove",
    severity = "medium"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = params_meta,
    engine = "sf",
    summary = list(
      n_checked = as.integer(sum(usable)),
      n_flagged = length(idx),
      n_skipped_rows = as.integer(sum(!usable))
    ),
    messages = result_messages
  )
}

#' Shift study-area geometry onto adjacent world copies for map display
#' @noRd
expand_study_area_for_wrap <- function(geom) {
  g <- sf::st_geometry(geom)
  if (length(g) < 1) {
    return(g)
  }
  crs <- sf::st_crs(g)
  pieces <- lapply(c(-360, 0, 360), function(off) {
    shifted <- g + c(off, 0)
    sf::st_set_crs(shifted, crs)
  })
  do.call(c, pieces)
}
