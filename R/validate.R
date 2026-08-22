#' Validate occurrence table structure and expected fields
#'
#' Checks columns and parseability before Assess.
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param column_map Optional manual column map.
#' @param skipped_fields Optional character vector of skipped map keys.
#' @param manually_mapped Whether columns were mapped manually.
#' @return An object of class `occ_validation`.
#' @export
validate_occurrence_dataset <- function(occ,
                                        column_map = NULL,
                                        skipped_fields = NULL,
                                        manually_mapped = FALSE) {
  if (!is.data.frame(occ)) {
    rlang::abort("`occ` must be a data frame.", call = NULL)
  }

  issues <- list()
  add_issue <- function(level, code, message) {
    issues[[length(issues) + 1L]] <<- list(
      level = level,
      code = code,
      message = message
    )
  }

  if (!"occsclean_id" %in% names(occ)) {
    add_issue(
      "error",
      "missing_occsclean_id",
      "Internal OccsClean row key (occsclean_id) is missing."
    )
  } else {
    ids <- as.character(occ$occsclean_id)
    if (anyNA(ids) || any(!nzchar(ids)) || any(duplicated(ids))) {
      add_issue(
        "error",
        "invalid_occsclean_id",
        "Internal OccsClean row key must be unique and non-missing."
      )
    }
  }

  skipped_fields <- skipped_fields %||% character()

  cols <- resolve_occurrence_columns(
    occ,
    overrides = column_map,
    skipped = skipped_fields
  )
  field_keys <- c("lon", "lat", "date", "taxon", "basis_of_record", "country")
  field_skipped <- field_keys %in% skipped_fields
  fields <- tibble::tibble(
    field = c(
      "longitude", "latitude", "occurrence_date", "scientific_name",
      "basis_of_record", "country"
    ),
    expected_role = c(
      "decimalLongitude",
      "decimalLatitude",
      "eventDate",
      "scientificName",
      "basisOfRecord",
      "countryCode / country"
    ),
    column_found = c(
      cols$lon %||% NA_character_,
      cols$lat %||% NA_character_,
      cols$date %||% NA_character_,
      cols$taxon %||% NA_character_,
      cols$basis_of_record %||% NA_character_,
      cols$country %||% NA_character_
    ),
    present = c(
      !is.null(cols$lon),
      !is.null(cols$lat),
      !is.null(cols$date),
      !is.null(cols$taxon),
      !is.null(cols$basis_of_record),
      !is.null(cols$country)
    ),
    skipped = c(
      field_skipped[[1]] || field_skipped[[2]],
      field_skipped[[1]] || field_skipped[[2]],
      field_skipped[[3]],
      field_skipped[[4]],
      field_skipped[[5]],
      field_skipped[[6]]
    )
  )

  coords_skipped <- "lon" %in% skipped_fields || "lat" %in% skipped_fields
  if (!isTRUE(fields$present[fields$field == "longitude"]) ||
        !isTRUE(fields$present[fields$field == "latitude"])) {
    if (coords_skipped) {
      add_issue(
        "warning",
        "skipped_coordinates_columns",
        "Coordinates were skipped. Coordinate and mapping checks will not run."
      )
    } else {
      add_issue(
        "warning",
        "missing_coordinates_columns",
        "Longitude and/or latitude columns were not found. Coordinate checks will be skipped."
      )
    }
  }
  if (!isTRUE(fields$present[fields$field == "occurrence_date"])) {
    if ("date" %in% skipped_fields) {
      add_issue(
        "warning",
        "skipped_date_column",
        "Occurrence date was skipped. Temporal checks will not run."
      )
    } else {
      add_issue(
        "warning",
        "missing_date_column",
        "An occurrence date column was not found. Temporal checks will be skipped."
      )
    }
  }
  if (!isTRUE(fields$present[fields$field == "scientific_name"])) {
    if ("taxon" %in% skipped_fields) {
      add_issue(
        "warning",
        "skipped_taxon_column",
        "Scientific name was skipped. Allowed species checks will not run."
      )
    } else {
      add_issue(
        "warning",
        "missing_taxon_column",
        "A scientific name column was not found. Taxonomic checks (future) will be unavailable."
      )
    }
  }
  if (!isTRUE(fields$present[fields$field == "basis_of_record"]) &&
        "basis_of_record" %in% skipped_fields) {
    add_issue(
      "warning",
      "skipped_basis_of_record_column",
      "Basis of record was skipped. Basis of record checks will not run."
    )
  }
  if (!isTRUE(fields$present[fields$field == "country"]) &&
        "country" %in% skipped_fields) {
    add_issue(
      "warning",
      "skipped_country_column",
      "Country was skipped. Country mismatch checks will not run."
    )
  }

  n_expected_fields <- sum(fields$present)
  if (n_expected_fields < 1L) {
    add_issue(
      "error",
      "no_expected_fields",
      paste0(
        "No expected occurrence columns were found (coordinates, occurrence date, ",
        "scientific name, basis of record, or country). ",
        "This file does not look like occurrence data. Re-import a CSV/TSV of records."
      )
    )
  }

  parse_summary <- list(
    n_rows = nrow(occ),
    n_cols = ncol(occ),
    lon_nonblank = NA_integer_,
    lon_numeric = NA_integer_,
    lon_in_range = NA_integer_,
    lat_nonblank = NA_integer_,
    lat_numeric = NA_integer_,
    lat_in_range = NA_integer_,
    date_nonblank = NA_integer_,
    date_parseable = NA_integer_
  )

  if (!is.null(cols$lon)) {
    lon_raw <- occ[[cols$lon]]
    lon_num <- as_numeric_silent(lon_raw)
    present <- !is_blank_coord(lon_raw)
    parse_summary$lon_nonblank <- sum(present)
    parse_summary$lon_numeric <- sum(present & !is.na(lon_num))
    parse_summary$lon_in_range <- sum(
      present & !is.na(lon_num) & lon_num >= -180 & lon_num <= 180
    )
  }
  if (!is.null(cols$lat)) {
    lat_raw <- occ[[cols$lat]]
    lat_num <- as_numeric_silent(lat_raw)
    present <- !is_blank_coord(lat_raw)
    parse_summary$lat_nonblank <- sum(present)
    parse_summary$lat_numeric <- sum(present & !is.na(lat_num))
    parse_summary$lat_in_range <- sum(
      present & !is.na(lat_num) & lat_num >= -90 & lat_num <= 90
    )
  }
  if (!is.null(cols$date)) {
    date_raw <- occ[[cols$date]]
    parsed <- parse_occurrence_dates(date_raw)
    present <- !is_blank_coord(date_raw)
    parse_summary$date_nonblank <- sum(present)
    parse_summary$date_parseable <- sum(present & !is.na(parsed))
  }

  readiness <- list(
    coordinate_checks = isTRUE(fields$present[fields$field == "longitude"]) &&
      isTRUE(fields$present[fields$field == "latitude"]),
    temporal_checks = isTRUE(fields$present[fields$field == "occurrence_date"]),
    taxonomic_checks = isTRUE(fields$present[fields$field == "scientific_name"]),
    basis_of_record_checks = isTRUE(
      fields$present[fields$field == "basis_of_record"]
    ),
    country_checks = isTRUE(fields$present[fields$field == "country"])
  )

  n_errors <- sum(vapply(issues, function(x) identical(x$level, "error"), logical(1)))
  n_warnings <- sum(vapply(issues, function(x) identical(x$level, "warning"), logical(1)))
  overall <- if (n_errors > 0) {
    "failed"
  } else if (n_warnings > 0) {
    "ready_with_warnings"
  } else {
    "ready"
  }

  structure(
    list(
      overall = overall,
      fields = fields,
      readiness = readiness,
      parse_summary = parse_summary,
      issues = issues,
      column_map = cols,
      skipped_fields = skipped_fields,
      manually_mapped = isTRUE(manually_mapped),
      validated_at = Sys.time()
    ),
    class = "occ_validation"
  )
}

#' @export
print.occ_validation <- function(x, ...) {
  cat(
    "<occ_validation>\n",
    "  overall: ", x$overall, "\n",
    "  fields present: ",
    paste(x$fields$field[x$fields$present], collapse = ", "), "\n",
    "  issues: ", length(x$issues), "\n",
    sep = ""
  )
  invisible(x)
}

#' Test whether an object is an `occ_validation`
#' @param x Object.
#' @return Logical.
#' @export
is_occ_validation <- function(x) {
  inherits(x, "occ_validation")
}

#' Format validation results for display
#' @param validation An [occ_validation] object.
#' @return Character string.
#' @export
format_validation_report <- function(validation) {
  if (!is_occ_validation(validation)) {
    rlang::abort("`validation` must be an occ_validation.", call = NULL)
  }

  ps <- validation$parse_summary
  lines <- c(
    "Dataset validation",
    "------------------",
    paste0("Overall: ", format_validation_overall(
      validation$overall,
      validation$manually_mapped %||% FALSE
    )),
    paste0("Validated: ", format(validation$validated_at, usetz = TRUE)),
    paste0("Rows: ", ps$n_rows, "  Columns: ", ps$n_cols),
    "",
    "Expected fields",
    "---------------"
  )

  for (i in seq_len(nrow(validation$fields))) {
    row <- validation$fields[i, , drop = FALSE]
    if (isTRUE(row$skipped[[1]])) {
      lines <- c(
        lines,
        paste0(
          "- ", row$field, ": skipped (",
          column_map_field_skip_message(row$field),
          ")"
        )
      )
    } else if (isTRUE(row$present[[1]])) {
      lines <- c(
        lines,
        paste0("- ", row$field, ": found as \"", row$column_found, "\"")
      )
    } else {
      lines <- c(
        lines,
        paste0("- ", row$field, ": not found")
      )
    }
  }

  lines <- c(lines, "", "Value checks", "------------")
  n_rows <- ps$n_rows
  if (!is.na(ps$lon_nonblank)) {
    lines <- c(
      lines,
      paste0(
        "- Longitude: ", ps$lon_nonblank, "/", n_rows, " non-blank, ",
        ps$lon_numeric, " numeric, ",
        ps$lon_in_range, " in [-180, 180]"
      )
    )
  }
  if (!is.na(ps$lat_nonblank)) {
    lines <- c(
      lines,
      paste0(
        "- Latitude: ", ps$lat_nonblank, "/", n_rows, " non-blank, ",
        ps$lat_numeric, " numeric, ",
        ps$lat_in_range, " in [-90, 90]"
      )
    )
  }
  if (!is.na(ps$date_nonblank)) {
    lines <- c(
      lines,
      paste0(
        "- Date: ", ps$date_nonblank, "/", n_rows, " non-blank, ",
        ps$date_parseable, " parseable"
      )
    )
  }
  if (is.na(ps$lon_nonblank) && is.na(ps$lat_nonblank) && is.na(ps$date_nonblank)) {
    lines <- c(lines, "- (No coordinate or date columns to summarize.)")
  }

  if (length(validation$issues) > 0) {
    lines <- c(lines, "", "Issues", "------")
    for (issue in validation$issues) {
      lines <- c(
        lines,
        paste0("- [", issue$level, "] ", issue$message)
      )
    }
  } else {
    lines <- c(lines, "", "Issues", "------", "- None")
  }

  paste(lines, collapse = "\n")
}

#' @noRd
format_validation_overall <- function(overall, manually_mapped = FALSE) {
  suffix <- if (isTRUE(manually_mapped)) {
    " - manually mapped"
  } else {
    ""
  }
  switch(
    as.character(overall),
    ready = paste0("structure OK", suffix),
    ready_with_warnings = paste0("structure OK with warnings", suffix),
    failed = "structure check failed",
    as.character(overall)
  )
}
