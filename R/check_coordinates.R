#' Flag records with missing coordinates
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_missing_coordinates <- function(occ, params = list()) {
  check_id <- "coord_missing"
  label <- "Missing coordinates"
  category <- "coordinate"

  cols <- resolve_occurrence_columns(occ)
  lon_col <- params$lon_col %||% cols$lon
  lat_col <- params$lat_col %||% cols$lat

  if (is.null(lon_col) || is.null(lat_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find longitude and/or latitude columns.",
      params_used = params
    ))
  }

  lon <- occ[[lon_col]]
  lat <- occ[[lat_col]]
  missing <- is_blank_coord(lon) | is_blank_coord(lat)
  idx <- which(missing)

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "COORD_MISSING",
    reason = "Longitude and/or latitude is missing.",
    evidence = paste0(
      lon_col, "=", format_evidence_value(lon[idx]), ";",
      lat_col, "=", format_evidence_value(lat[idx])
    ),
    recommended_action = "remove",
    severity = "high"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(
      params,
      list(lon_col = lon_col, lat_col = lat_col)
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}

#' Flag invalid coordinates (non-numeric or out of range)
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_invalid_coordinates <- function(occ, params = list()) {
  check_id <- "coord_invalid"
  label <- "Invalid coordinates"
  category <- "coordinate"

  cols <- resolve_occurrence_columns(occ)
  lon_col <- params$lon_col %||% cols$lon
  lat_col <- params$lat_col %||% cols$lat

  if (is.null(lon_col) || is.null(lat_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find longitude and/or latitude columns.",
      params_used = params
    ))
  }

  lon_raw <- occ[[lon_col]]
  lat_raw <- occ[[lat_col]]
  lon <- as_numeric_silent(lon_raw)
  lat <- as_numeric_silent(lat_raw)

  present <- !is_blank_coord(lon_raw) & !is_blank_coord(lat_raw)
  non_numeric <- present & (is.na(lon) | is.na(lat))
  out_of_range <- present & !is.na(lon) & !is.na(lat) &
    (lon < -180 | lon > 180 | lat < -90 | lat > 90)

  bad <- non_numeric | out_of_range
  idx <- which(bad)

  reason <- rep("Coordinates are invalid.", length(idx))
  if (length(idx) > 0) {
    reason[non_numeric[idx]] <- "Coordinate values are non-numeric."
    reason[out_of_range[idx]] <-
      "Coordinates are outside valid longitude [-180, 180] or latitude [-90, 90] ranges."
  }

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "COORD_INVALID",
    reason = reason,
    evidence = paste0(
      lon_col, "=", format_evidence_value(lon_raw[idx]), ";",
      lat_col, "=", format_evidence_value(lat_raw[idx])
    ),
    recommended_action = "remove",
    severity = "high"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(
      params,
      list(lon_col = lon_col, lat_col = lat_col)
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}

#' Flag coordinates at (0, 0)
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_zero_coordinates <- function(occ, params = list()) {
  check_id <- "coord_zero"
  label <- "Coordinates at (0, 0)"
  category <- "coordinate"

  cols <- resolve_occurrence_columns(occ)
  lon_col <- params$lon_col %||% cols$lon
  lat_col <- params$lat_col %||% cols$lat

  if (is.null(lon_col) || is.null(lat_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find longitude and/or latitude columns.",
      params_used = params
    ))
  }

  lon <- as_numeric_silent(occ[[lon_col]])
  lat <- as_numeric_silent(occ[[lat_col]])
  is_zero <- !is.na(lon) & !is.na(lat) & lon == 0 & lat == 0
  idx <- which(is_zero)

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "COORD_ZERO",
    reason = "Longitude and latitude are both 0 (often a missing-value placeholder).",
    evidence = paste0(lon_col, "=0;", lat_col, "=0"),
    recommended_action = "remove",
    severity = "high"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(
      params,
      list(lon_col = lon_col, lat_col = lat_col)
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}

#' True when a coordinate cell is missing / blank
#' @noRd
is_blank_coord <- function(x) {
  if (is.character(x) || is.factor(x)) {
    xc <- trimws(as.character(x))
    return(is.na(x) | !nzchar(xc))
  }
  is.na(x)
}

#' Format evidence values for findings
#' @noRd
format_evidence_value <- function(x) {
  ifelse(is.na(x), "NA", as.character(x))
}
