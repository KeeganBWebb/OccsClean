#' Sentinel for skipped column mapping in UI
#' @noRd
COLUMN_MAP_SKIP <- "__skip__"

#' Candidate column names for longitude
#' @noRd
lon_column_candidates <- function() {
  c(
    "decimalLongitude", "decimal_longitude", "decimallongitude",
    "longitude", "Longitude", "lon", "lng", "x"
  )
}

#' Candidate column names for latitude
#' @noRd
lat_column_candidates <- function() {
  c(
    "decimalLatitude", "decimal_latitude", "decimallatitude",
    "latitude", "Latitude", "lat", "y"
  )
}

#' Candidate column names for event / occurrence date
#' @noRd
date_column_candidates <- function() {
  c(
    "eventDate", "event_date", "eventdate",
    "occurrenceDate", "occurrence_date",
    "date", "Date", "verbatimEventDate"
  )
}

#' Candidate column names for scientific / taxon name
#' @noRd
taxon_column_candidates <- function() {
  c(
    "scientificName", "acceptedScientificName", "species",
    "scientific_name", "taxonName", "taxon_name", "name"
  )
}

#' Candidate column names for basis of record
#' @noRd
basis_of_record_column_candidates <- function() {
  c(
    "basisOfRecord", "basis_of_record", "basisofrecord",
    "BasisOfRecord", "basisOfRecordType"
  )
}

#' Candidate column names for country / country code
#' @noRd
country_column_candidates <- function() {
  c(
    "countryCode", "countrycode", "country_code",
    "isoCountryCodeAlpha3", "isoCountryCodeAlpha2",
    "country", "Country"
  )
}

#' Resolve the first matching column name
#' @param occ A data frame.
#' @param candidates Character vector of preferred names.
#' @noRd
resolve_column <- function(occ, candidates) {
  hit <- candidates[candidates %in% names(occ)]
  if (length(hit) < 1) {
    return(NULL)
  }
  hit[[1]]
}

#' Resolve standard occurrence field columns
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param overrides Optional named list of manual column choices.
#' @param skipped Optional character vector of skipped map keys.
#' @export
resolve_occurrence_columns <- function(occ, overrides = NULL, skipped = NULL) {
  keys <- c("lon", "lat", "date", "taxon", "basis_of_record", "country")
  skipped <- skipped %||% character()
  auto <- list(
    lon = resolve_column(occ, lon_column_candidates()),
    lat = resolve_column(occ, lat_column_candidates()),
    date = resolve_column(occ, date_column_candidates()),
    taxon = resolve_column(occ, taxon_column_candidates()),
    basis_of_record = resolve_column(occ, basis_of_record_column_candidates()),
    country = resolve_column(occ, country_column_candidates())
  )
  if (is.null(overrides)) {
    return(auto)
  }

  out <- auto
  for (k in keys) {
    if (k %in% skipped) {
      out[[k]] <- NULL
      next
    }
    if (!k %in% names(overrides)) {
      next
    }
    ov <- overrides[[k]]
    if (identical(ov, COLUMN_MAP_SKIP)) {
      out[[k]] <- NULL
      next
    }
    if (is.null(ov) || length(ov) < 1L || !nzchar(as.character(ov)[1])) {
      out[[k]] <- NULL
    } else {
      col <- as.character(ov)[1]
      out[[k]] <- if (col %in% names(occ)) col else NULL
    }
  }
  out
}

#' Occurrence column map field specs for UI
#' @noRd
occurrence_column_map_specs <- function() {
  list(
    list(
      key = "lon",
      label = "Longitude",
      field = "longitude",
      skip_note = "coordinate and mapping checks unavailable"
    ),
    list(
      key = "lat",
      label = "Latitude",
      field = "latitude",
      skip_note = "coordinate and mapping checks unavailable"
    ),
    list(
      key = "date",
      label = "Occurrence date",
      field = "occurrence_date",
      skip_note = "temporal checks unavailable"
    ),
    list(
      key = "taxon",
      label = "Scientific name",
      field = "scientific_name",
      skip_note = "allowed species checks unavailable"
    ),
    list(
      key = "basis_of_record",
      label = "Basis of record",
      field = "basis_of_record",
      skip_note = "basis of record checks unavailable"
    ),
    list(
      key = "country",
      label = "Country",
      field = "country",
      skip_note = "country mismatch checks unavailable"
    )
  )
}

#' @noRd
column_map_spec_for_key <- function(key) {
  specs <- occurrence_column_map_specs()
  for (spec in specs) {
    if (identical(spec$key, key)) {
      return(spec)
    }
  }
  NULL
}

#' @noRd
column_map_skip_choice_label <- function(key) {
  spec <- column_map_spec_for_key(key)
  if (is.null(spec)) {
    return("Skip")
  }
  paste0("Skip (", spec$skip_note, ")")
}

#' @noRd
column_map_field_skip_message <- function(field) {
  spec <- NULL
  for (candidate in occurrence_column_map_specs()) {
    if (identical(candidate$field, field)) {
      spec <- candidate
      break
    }
  }
  if (is.null(spec)) {
    return("related checks unavailable")
  }
  spec$skip_note
}

#' @noRd
is_column_map_skip_value <- function(value) {
  identical(as.character(value %||% ""), COLUMN_MAP_SKIP)
}

#' @noRd
mapping_value_uses_column <- function(value) {
  val <- as.character(value %||% "")
  nzchar(val) && !identical(val, COLUMN_MAP_SKIP)
}

#' Column choices for one mapping field
#' @noRd
column_mapping_choices <- function(all_cols, current_map, for_key) {
  used <- character()
  for (spec in occurrence_column_map_specs()) {
    key <- spec$key
    if (identical(key, for_key)) {
      next
    }
    val <- as.character(current_map[[key]] %||% "")
    if (mapping_value_uses_column(val)) {
      used <- c(used, val)
    }
  }
  avail <- setdiff(all_cols, unique(used))
  stats::setNames(
    c("", COLUMN_MAP_SKIP, avail),
    c(
      "Select column...",
      column_map_skip_choice_label(for_key),
      avail
    )
  )
}

#' @noRd
validation_mapping_panel_hidden <- function(validation) {
  if (is.null(validation)) {
    return(TRUE)
  }
  if (identical(validation$overall, "ready")) {
    return(TRUE)
  }
  isTRUE(validation$manually_mapped) &&
    !identical(validation$overall, "failed")
}

#' @noRd
selected_column_mapping_value <- function(column_name, skipped, choices) {
  if (isTRUE(skipped)) {
    if (COLUMN_MAP_SKIP %in% choices) {
      return(COLUMN_MAP_SKIP)
    }
    return("")
  }
  selected_mapping_value(column_name, choices)
}

#' @noRd
selected_mapping_value <- function(value, choices) {
  val <- as.character(value %||% "")
  if (!nzchar(val) || !val %in% choices) {
    return("")
  }
  val
}

#' Normalize manual column map input from UI
#' @noRd
normalize_column_map_input <- function(map, occ_names) {
  specs <- occurrence_column_map_specs()
  out <- list()
  skipped <- character()
  for (spec in specs) {
    key <- spec$key
    val <- map[[key]]
    if (is.null(val) || length(val) < 1L || !nzchar(as.character(val)[1])) {
      rlang::abort(
        paste0("Choose a column or Skip for ", spec$label, "."),
        call = NULL
      )
    }
    if (is_column_map_skip_value(val)) {
      out[[key]] <- NULL
      skipped <- c(skipped, key)
    } else {
      col <- as.character(val)[1]
      if (!col %in% occ_names) {
        rlang::abort(
          paste0("Unknown column for ", key, ": ", col),
          call = NULL
        )
      }
      out[[key]] <- col
    }
  }

  mapped <- unlist(out, use.names = FALSE)
  mapped <- mapped[vapply(
    mapped,
    function(x) !is.null(x) && nzchar(as.character(x)),
    logical(1)
  )]
  if (any(duplicated(mapped))) {
    dups <- unique(mapped[duplicated(mapped)])
    rlang::abort(
      paste0("Each column can only be mapped once: ", paste(dups, collapse = ", ")),
      call = NULL
    )
  }

  list(map = out, skipped = skipped)
}

#' Check params derived from a resolved column map
#' @noRd
check_params_from_column_map <- function(column_map) {
  if (is.null(column_map) || length(column_map) < 1) {
    return(list())
  }
  out <- list()
  if (!is.null(column_map$lon)) {
    out$lon_col <- column_map$lon
  }
  if (!is.null(column_map$lat)) {
    out$lat_col <- column_map$lat
  }
  if (!is.null(column_map$date)) {
    out$date_col <- column_map$date
  }
  if (!is.null(column_map$taxon)) {
    out$taxon_col <- column_map$taxon
  }
  if (!is.null(column_map$basis_of_record)) {
    out$basis_col <- column_map$basis_of_record
  }
  if (!is.null(column_map$country)) {
    out$country_col <- column_map$country
  }
  out
}

#' Merge column params into check params
#' @noRd
merge_check_column_params <- function(params, col_params) {
  if (length(col_params) < 1) {
    return(params)
  }
  if (is.null(params)) {
    params <- list()
  }
  catalog <- list_quality_checks()
  if (!is.null(names(params)) && any(names(params) %in% catalog$check_id)) {
    for (id in names(params)) {
      check_params <- params[[id]]
      if (is.null(check_params)) {
        check_params <- list()
      }
      params[[id]] <- utils::modifyList(col_params, check_params)
    }
    return(params)
  }
  utils::modifyList(col_params, params)
}

#' Normalize country values to ISO3
#' @param x Character vector of country codes or names.
#' @noRd
to_iso3c <- function(x) {
  raw <- trimws(as.character(x))
  out <- rep(NA_character_, length(raw))
  idx <- which(!is.na(raw) & nzchar(raw))
  if (length(idx) < 1) {
    return(out)
  }

  vals <- raw[idx]
  res <- rep(NA_character_, length(vals))

  is3 <- nchar(vals) == 3L & grepl("^[A-Za-z]{3}$", vals)
  res[is3] <- toupper(vals[is3])

  is2 <- is.na(res) & nchar(vals) == 2L & grepl("^[A-Za-z]{2}$", vals)
  if (any(is2)) {
    res[is2] <- countrycode::countrycode(
      vals[is2],
      origin = "iso2c",
      destination = "iso3c",
      warn = FALSE
    )
  }

  still <- is.na(res)
  if (any(still)) {
    res[still] <- countrycode::countrycode(
      vals[still],
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  }

  out[idx] <- res
  out
}

#' Build a findings tibble for flagged rows only
#' @noRd
findings_from_flags <- function(occsclean_id,
                                finding,
                                reason,
                                evidence,
                                recommended_action = "remove",
                                severity = NA_character_,
                                confidence = NA_real_) {
  n <- length(occsclean_id)
  if (n < 1) {
    return(empty_findings())
  }
  tibble::tibble(
    occsclean_id = as.character(occsclean_id),
    flag = rep(TRUE, n),
    finding = rep(as.character(finding), length.out = n),
    reason = as.character(reason),
    evidence = as.character(evidence),
    confidence = rep(as.numeric(confidence), length.out = n),
    recommended_action = rep(as.character(recommended_action), length.out = n),
    severity = rep(as.character(severity), length.out = n)
  )
}

#' Skipped check result when required fields are missing
#' @noRd
skipped_check_result <- function(check_id,
                                 label,
                                 category,
                                 messages,
                                 params_used = list()) {
  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "skipped",
    findings = empty_findings(),
    params_used = params_used,
    engine = "native",
    summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = 0L),
    messages = messages
  )
}

#' Coerce a column to numeric, preserving non-numeric as NA
#' @noRd
as_numeric_silent <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  suppressWarnings(as.numeric(as.character(x)))
}

#' Null-coalesce (internal)
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
