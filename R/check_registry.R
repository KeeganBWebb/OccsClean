#' Built-in quality check catalog
#' @export
list_quality_checks <- function() {
  defs <- quality_check_definitions()
  tibble::tibble(
    check_id = vapply(defs, `[[`, character(1), "check_id"),
    label = vapply(defs, `[[`, character(1), "label"),
    category = vapply(defs, `[[`, character(1), "category"),
    description = vapply(defs, `[[`, character(1), "description"),
    ui_group = vapply(defs, `[[`, character(1), "ui_group"),
    ui_group_label = vapply(defs, `[[`, character(1), "ui_group_label"),
    method_via = vapply(defs, function(d) {
      mv <- d$method_via
      if (is.null(mv) || length(mv) < 1 || is.na(mv[[1]])) {
        NA_character_
      } else {
        as.character(mv[[1]])
      }
    }, character(1))
  )
}

#' UI group order for Assess
#' @noRd
assess_ui_group_order <- function() {
  c("basics", "dates", "suspicious_locations", "taxonomic")
}

#' Internal check definitions
#' @noRd
quality_check_definitions <- function() {
  list(
    list(
      check_id = "occ_duplicates",
      label = "Duplicate records",
      category = "occurrence",
      ui_group = "basics",
      ui_group_label = "General checks",
      description = "Flags rows that match on every field except OccsClean's internal row key.",
      fun = check_duplicates
    ),
    list(
      check_id = "coord_missing",
      label = "Missing coordinates",
      category = "coordinate",
      ui_group = "basics",
      ui_group_label = "General checks",
      description = "Longitude and/or latitude is blank.",
      fun = check_missing_coordinates
    ),
    list(
      check_id = "coord_invalid",
      label = "Invalid coordinates",
      category = "coordinate",
      ui_group = "basics",
      ui_group_label = "General checks",
      description = "Non-numeric or outside lon [-180, 180] / lat [-90, 90].",
      fun = check_invalid_coordinates
    ),
    list(
      check_id = "occ_basis_of_record",
      label = "Basis Of Record",
      category = "occurrence",
      ui_group = "basics",
      ui_group_label = "General checks",
      description = paste(
        "Flag records with blank or unwanted basisOfRecord values based on the",
        "selected filters. Useful for ensuring the remaining data has been",
        "collected a certain way."
      ),
      fun = check_basis_of_record
    ),
    list(
      check_id = "taxon_allowed_species",
      label = "Allowed species",
      category = "taxonomic",
      ui_group = "taxonomic",
      ui_group_label = "Taxonomic",
      description = paste(
        "Flag records with missing scientific names or names outside those",
        "that are selected from this file."
      ),
      fun = check_allowed_species
    ),
    list(
      check_id = "date_invalid",
      label = "Unparseable dates",
      category = "temporal",
      ui_group = "dates",
      ui_group_label = "Dates",
      description = "Flag records that have dates that were not interpreted by OccsClean.",
      fun = check_invalid_dates
    ),
    list(
      check_id = "date_future",
      label = "Future dates",
      category = "temporal",
      ui_group = "dates",
      ui_group_label = "Dates",
      description = "Parsed date is after today.",
      fun = check_future_dates
    ),
    list(
      check_id = "date_out_of_range",
      label = "Dates outside range",
      category = "temporal",
      ui_group = "dates",
      ui_group_label = "Dates",
      description = "Parsed occurrence date falls outside the min/max you designate.",
      fun = check_date_out_of_range
    ),
    list(
      check_id = "coord_zero",
      label = "At coordinates (0, 0)",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = "Both longitude and latitude are exactly 0 (common placeholder).",
      fun = check_zero_coordinates
    ),
    list(
      check_id = "coord_equal",
      label = "Equal longitude and latitude",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = "Lon and lat are the same number (often a data-entry error).",
      method_via = "CoordinateCleaner",
      fun = check_coord_equal
    ),
    list(
      check_id = "coord_sea",
      label = "In the ocean",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = "Point falls outside land. Review carefully for marine taxa.",
      method_via = "CoordinateCleaner",
      fun = check_coord_sea
    ),
    list(
      check_id = "coord_land",
      label = "On land",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = "Point falls on land. Intended for marine taxa; review coastal records carefully.",
      method_via = "CoordinateCleaner",
      fun = check_coord_land
    ),
    list(
      check_id = "coord_centroid",
      label = "Near country or province center",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = paste(
        "Near political centroids, which are often a faulty georeference",
        "(default buffer: 1000 m)."
      ),
      method_via = "CoordinateCleaner",
      fun = check_coord_centroid
    ),
    list(
      check_id = "coord_capital",
      label = "Near a capital city",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = paste(
        "Near capital cities, which are often a faulty georeference",
        "(default buffer: 10000 m)."
      ),
      method_via = "CoordinateCleaner",
      fun = check_coord_capital
    ),
    list(
      check_id = "coord_institution",
      label = "Near a museum or collection",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = paste(
        "Near biodiversity institutions, which can be a possible collection",
        "locality artifact (default buffer: 100 m)."
      ),
      method_via = "CoordinateCleaner",
      fun = check_coord_institution
    ),
    list(
      check_id = "coord_gbif",
      label = "Near GBIF headquarters",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = paste(
        "Near the GBIF office in Copenhagen, which is often a placeholder",
        "locality (default buffer: 1000 m)."
      ),
      method_via = "CoordinateCleaner",
      fun = check_coord_gbif
    ),
    list(
      check_id = "coord_outside_area",
      label = "Outside study area",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = paste(
        "Occurrences that fall outside of a provided shapefile's extent by a",
        "specified distance (default: blank = flag any point outside) are",
        "flagged using nearest-distance measurement."
      ),
      method_via = "sf",
      fun = check_outside_area
    ),
    list(
      check_id = "coord_country",
      label = "Outside reported country",
      category = "coordinate",
      ui_group = "suspicious_locations",
      ui_group_label = "Suspicious locations",
      description = paste(
        "Occurrence point falls outside the country specified in",
        "countryCode/country column. Be wary of misinputted country data or",
        "occurrences that just fall outside of a country",
        "(default buffer: none)."
      ),
      method_via = "CoordinateCleaner",
      fun = check_coord_country
    )
  )
}

#' Look up one check definition
#' @param check_id Check id.
#' @noRd
get_quality_check_def <- function(check_id) {
  defs <- quality_check_definitions()
  ids <- vapply(defs, `[[`, character(1), "check_id")
  hit <- defs[ids == check_id]
  if (length(hit) < 1) {
    rlang::abort(paste0("Unknown check_id: ", check_id), call = NULL)
  }
  hit[[1]]
}

#' Run one quality check
#'
#' @param check_id Check identifier from [list_quality_checks()].
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional parameters forwarded to the check.
#' @export
run_quality_check <- function(check_id, occ, params = list()) {
  def <- get_quality_check_def(check_id)
  def$fun(occ, params = params)
}

#' Run multiple quality checks
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param check_ids Character vector of check ids. Default: all registered.
#' @param params Optional per-check parameters.
#' @param progress Optional progress callback `function(i, n, check_id)`.
#' @export
run_quality_checks <- function(occ,
                               check_ids = NULL,
                               params = list(),
                               progress = NULL) {
  catalog <- list_quality_checks()
  if (is.null(check_ids)) {
    check_ids <- catalog$check_id
  }
  unknown <- setdiff(check_ids, catalog$check_id)
  if (length(unknown) > 0) {
    rlang::abort(
      paste0("Unknown check_id(s): ", paste(unknown, collapse = ", ")),
      call = NULL
    )
  }

  n <- length(check_ids)
  results <- vector("list", n)
  names(results) <- check_ids

  # Reuse one land polygon for sea/land checks in a run.
  shared_land_ref <- NULL
  needs_land <- any(check_ids %in% c("coord_sea", "coord_land"))
  if (isTRUE(needs_land)) {
    shared_land_ref <- tryCatch(
      load_coordinatecleaner_land_ref(),
      error = function(e) NULL
    )
  }

  for (i in seq_len(n)) {
    id <- check_ids[[i]]
    if (is.function(progress)) {
      progress(i = i, n = n, check_id = id)
    }

    params_by_check <- !is.null(names(params)) &&
      any(names(params) %in% catalog$check_id)
    if (isTRUE(params_by_check)) {
      check_params <- params[[id]]
      if (is.null(check_params)) {
        check_params <- list()
      }
    } else {
      check_params <- params
    }

    if (!is.null(shared_land_ref) &&
          id %in% c("coord_sea", "coord_land") &&
          is.null(check_params$ref)) {
      check_params$ref <- shared_land_ref
    }

    results[[i]] <- run_quality_check(id, occ, params = check_params)
  }

  results
}

#' Combine flagged findings from an assessment list for review
#'
#' @param assessment Named list of [occ_check_result] objects.
#' @param occ Optional occurrence tibble to join context columns.
#' @param column_map Optional resolved column map.
#' @export
assessment_findings_table <- function(assessment, occ = NULL, column_map = NULL) {
  if (length(assessment) < 1) {
    return(tibble::tibble(
      check_id = character(),
      check_label = character(),
      category = character(),
      occsclean_id = character(),
      finding = character(),
      reason = character(),
      evidence = character(),
      recommended_action = character(),
      severity = character()
    ))
  }

  rows <- lapply(assessment, function(res) {
    if (!is_occ_check_result(res)) {
      return(NULL)
    }
    f <- res$findings
    if (nrow(f) < 1) {
      return(NULL)
    }
    flagged <- f[f$flag %in% TRUE, , drop = FALSE]
    if (nrow(flagged) < 1) {
      return(NULL)
    }
    tibble::tibble(
      check_id = res$check_id,
      check_label = res$label,
      category = res$category,
      occsclean_id = flagged$occsclean_id,
      finding = flagged$finding,
      reason = flagged$reason,
      evidence = flagged$evidence,
      recommended_action = flagged$recommended_action,
      severity = flagged$severity
    )
  })

  out <- dplyr::bind_rows(rows)
  if (nrow(out) < 1) {
    return(out)
  }

  if (!is.null(occ) && "occsclean_id" %in% names(occ)) {
    cols <- if (is.list(column_map) && length(column_map) > 0) {
      column_map
    } else {
      resolve_occurrence_columns(occ)
    }
    keep <- c(
      "occsclean_id",
      intersect(c("scientificName", "acceptedScientificName"), names(occ)),
      cols$lon,
      cols$lat,
      cols$date,
      cols$basis_of_record,
      cols$country
    )
    keep <- unique(stats::na.omit(keep))
    ctx <- occ[keep]
    if (!is.null(cols$date) && cols$date %in% names(ctx)) {
      names(ctx)[names(ctx) == cols$date] <- "occurrence_date"
    }
    out <- dplyr::left_join(out, ctx, by = "occsclean_id")
  }

  out
}
