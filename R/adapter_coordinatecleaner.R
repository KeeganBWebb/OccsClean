#' Map CoordinateCleaner flags into an occ_check_result
#' @noRd
adapter_coordinatecleaner_flag <- function(occ,
                                           lon_col,
                                           lat_col,
                                           cc_fun,
                                           check_id,
                                           label,
                                           category,
                                           finding,
                                           reason,
                                           engine,
                                           cc_args = list(),
                                           extra_cols = NULL,
                                           row_mask = NULL,
                                           evidence_fun = NULL,
                                           invert = FALSE) {
  n <- nrow(occ)
  lon <- as_numeric_silent(occ[[lon_col]])
  lat <- as_numeric_silent(occ[[lat_col]])
  usable <- is.finite(lon) & is.finite(lat)
  if (!is.null(row_mask)) {
    if (length(row_mask) != n) {
      rlang::abort("`row_mask` must have length nrow(occ).", call = NULL)
    }
    usable <- usable & as.logical(row_mask) & !is.na(row_mask)
  }

  passed <- rep(TRUE, n)
  messages <- character()
  params_used <- utils::modifyList(
    cc_args,
    list(lon_col = lon_col, lat_col = lat_col)
  )

  if (!any(usable)) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "ok",
      findings = empty_findings(),
      params_used = params_used,
      engine = engine,
      summary = list(
        n_checked = 0L,
        n_flagged = 0L,
        n_skipped_rows = as.integer(n)
      ),
      messages = "No eligible rows available to evaluate."
    ))
  }

  cc_df <- data.frame(
    decimalLongitude = lon[usable],
    decimalLatitude = lat[usable],
    stringsAsFactors = FALSE
  )
  if (!is.null(extra_cols)) {
    for (nm in names(extra_cols)) {
      cc_df[[nm]] <- extra_cols[[nm]][usable]
    }
  }

  args <- utils::modifyList(
    list(
      x = cc_df,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      value = "flagged",
      verbose = FALSE
    ),
    cc_args
  )

  flags <- tryCatch(
    do.call(cc_fun, args),
    error = function(e) {
      messages <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(flags)) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "error",
      findings = empty_findings(),
      params_used = params_used,
      engine = engine,
      summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = as.integer(n)),
      messages = messages
    ))
  }

  if (!is.logical(flags) || length(flags) != sum(usable)) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "error",
      findings = empty_findings(),
      params_used = params_used,
      engine = engine,
      summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = as.integer(n)),
      messages = "CoordinateCleaner returned an unexpected flag vector."
    ))
  }

  passed[usable] <- flags
  if (isTRUE(invert)) {
    problematic <- usable & passed
  } else {
    problematic <- usable & !passed
  }
  idx <- which(problematic)

  if (is.null(evidence_fun)) {
    evidence <- paste0(
      lon_col, "=", format_evidence_value(occ[[lon_col]][idx]), ";",
      lat_col, "=", format_evidence_value(occ[[lat_col]][idx])
    )
  } else {
    evidence <- evidence_fun(idx)
  }

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = finding,
    reason = reason,
    evidence = evidence,
    recommended_action = "remove",
    severity = NA_character_
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = params_used,
    engine = engine,
    summary = list(
      n_checked = as.integer(sum(usable)),
      n_flagged = length(idx),
      n_skipped_rows = as.integer(sum(!usable))
    ),
    messages = messages
  )
}

#' Shared lon/lat resolution or skipped result for CC checks
#' @noRd
cc_require_lon_lat <- function(occ, params, check_id, label, category) {
  cols <- resolve_occurrence_columns(occ)
  lon_col <- params$lon_col %||% cols$lon
  lat_col <- params$lat_col %||% cols$lat
  if (is.null(lon_col) || is.null(lat_col)) {
    return(list(
      ok = FALSE,
      result = skipped_check_result(
        check_id, label, category,
        messages = "Could not find longitude and/or latitude columns.",
        params_used = params
      )
    ))
  }
  list(ok = TRUE, lon_col = lon_col, lat_col = lat_col)
}

#' Package-level cache for CoordinateCleaner land polygons
#' @noRd
.cc_land_ref_cache <- new.env(parent = emptyenv())

#' Load Natural Earth land polygons for ocean/land checks
#' @param scale Natural Earth scale (10, 50, or 110).
#' @noRd
load_coordinatecleaner_land_ref <- function(scale = 110) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE) ||
        !requireNamespace("terra", quietly = TRUE)) {
    return(NULL)
  }

  scale <- as.integer(scale)[[1]]
  if (!scale %in% c(10L, 50L, 110L)) {
    scale <- 110L
  }
  key <- as.character(scale)
  if (exists(key, envir = .cc_land_ref_cache, inherits = FALSE)) {
    return(.cc_land_ref_cache[[key]])
  }

  ref <- suppressWarnings(
    terra::vect(
      rnaturalearth::ne_download(
        scale = scale,
        type = "land",
        category = "physical",
        load = TRUE,
        returnclass = "sf"
      )
    )
  )
  .cc_land_ref_cache[[key]] <- ref
  ref
}

