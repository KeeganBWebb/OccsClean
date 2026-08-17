#' Flag coordinates in the ocean
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_sea <- function(occ, params = list()) {
  check_id <- "coord_sea"
  label <- "In the ocean"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_sea,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_SEA",
    reason = "Coordinate falls outside the landmass reference (ocean / non-terrestrial).",
    engine = "CoordinateCleaner::cc_sea",
    cc_args = cc_args
  )
}

#' Flag coordinates on land
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_land <- function(occ, params = list()) {
  check_id <- "coord_land"
  label <- "On land"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_sea,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_LAND",
    reason = "Coordinate falls on land (often a georeferencing error for marine taxa).",
    engine = "CoordinateCleaner::cc_sea (inverted)",
    cc_args = cc_args,
    invert = TRUE
  )
}

#' Flag coordinates near country or province centroids
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_centroid <- function(occ, params = list()) {
  check_id <- "coord_centroid"
  label <- "Near country or province center"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  if (is.null(cc_args$geod)) {
    cc_args$geod <- TRUE
  }
  if (is.null(cc_args$buffer)) {
    cc_args$buffer <- coordinatecleaner_buffer_defaults_m()$coord_centroid
  }

  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_cen,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_CENTROID",
    reason = "Coordinate is near a country or province centroid (often a georeferencing artifact).",
    engine = "CoordinateCleaner::cc_cen",
    cc_args = cc_args
  )
}

#' Flag coordinates near capital cities
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_capital <- function(occ, params = list()) {
  check_id <- "coord_capital"
  label <- "Near a capital city"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  if (is.null(cc_args$geod)) {
    cc_args$geod <- TRUE
  }
  if (is.null(cc_args$buffer)) {
    cc_args$buffer <- coordinatecleaner_buffer_defaults_m()$coord_capital
  }

  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_cap,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_CAPITAL",
    reason = "Coordinate is near a capital city (often a georeferencing artifact).",
    engine = "CoordinateCleaner::cc_cap",
    cc_args = cc_args
  )
}

#' Flag coordinates near biodiversity institutions
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_institution <- function(occ, params = list()) {
  check_id <- "coord_institution"
  label <- "Near a museum or collection"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  if (is.null(cc_args$geod)) {
    cc_args$geod <- TRUE
  }
  if (is.null(cc_args$buffer)) {
    cc_args$buffer <- coordinatecleaner_buffer_defaults_m()$coord_institution
  }

  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_inst,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_INSTITUTION",
    reason = "Coordinate is near a biodiversity institution (museum/herbarium artifact risk).",
    engine = "CoordinateCleaner::cc_inst",
    cc_args = cc_args
  )
}

#' Flag records with equal longitude and latitude
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_equal <- function(occ, params = list()) {
  check_id <- "coord_equal"
  label <- "Equal longitude and latitude"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  if (is.null(cc_args$test)) {
    cc_args$test <- "absolute"
  }

  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_equ,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_EQUAL",
    reason = "Longitude and latitude are equal (common data-entry artifact).",
    engine = "CoordinateCleaner::cc_equ",
    cc_args = cc_args
  )
}

#' Flag coordinates near GBIF headquarters
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_gbif <- function(occ, params = list()) {
  check_id <- "coord_gbif"
  label <- "Near GBIF headquarters"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cc_args <- params[setdiff(names(params), c("lon_col", "lat_col"))]
  if (is.null(cc_args$geod)) {
    cc_args$geod <- TRUE
  }
  if (is.null(cc_args$buffer)) {
    cc_args$buffer <- coordinatecleaner_buffer_defaults_m()$coord_gbif
  }

  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_gbif,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_GBIF",
    reason = "Coordinate is near GBIF headquarters (often a placeholder locality).",
    engine = "CoordinateCleaner::cc_gbif",
    cc_args = cc_args
  )
}

#' Flag coordinates outside the reported country
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_coord_country <- function(occ, params = list()) {
  check_id <- "coord_country"
  label <- "Outside reported country"
  category <- "coordinate"

  req <- cc_require_lon_lat(occ, params, check_id, label, category)
  if (!isTRUE(req$ok)) {
    return(req$result)
  }

  cols <- resolve_occurrence_columns(occ)
  country_col <- params$country_col %||% cols$country
  if (is.null(country_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find a country or countryCode column.",
      params_used = params
    ))
  }

  iso3 <- to_iso3c(occ[[country_col]])
  row_mask <- !is.na(iso3)
  if (!any(row_mask)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = paste0(
        "No resolvable ISO3 country codes in column \"", country_col, "\"."
      ),
      params_used = utils::modifyList(params, list(country_col = country_col))
    ))
  }

  cc_args <- params[setdiff(
    names(params),
    c("lon_col", "lat_col", "country_col")
  )]
  cc_args$iso3 <- "countrycode"

  adapter_coordinatecleaner_flag(
    occ = occ,
    lon_col = req$lon_col,
    lat_col = req$lat_col,
    cc_fun = CoordinateCleaner::cc_coun,
    check_id = check_id,
    label = label,
    category = category,
    finding = "COORD_COUNTRY",
    reason = "Coordinate falls outside the reported country polygon.",
    engine = "CoordinateCleaner::cc_coun",
    cc_args = cc_args,
    extra_cols = list(countrycode = iso3),
    row_mask = row_mask,
    evidence_fun = function(idx) {
      paste0(
        req$lon_col, "=", format_evidence_value(occ[[req$lon_col]][idx]), ";",
        req$lat_col, "=", format_evidence_value(occ[[req$lat_col]][idx]), ";",
        country_col, "=", format_evidence_value(occ[[country_col]][idx]),
        " (ISO3=", format_evidence_value(iso3[idx]), ")"
      )
    }
  )
}
