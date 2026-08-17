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
#' @export
resolve_occurrence_columns <- function(occ) {
  list(
    lon = resolve_column(occ, lon_column_candidates()),
    lat = resolve_column(occ, lat_column_candidates()),
    date = resolve_column(occ, date_column_candidates()),
    taxon = resolve_column(occ, taxon_column_candidates()),
    basis_of_record = resolve_column(occ, basis_of_record_column_candidates()),
    country = resolve_column(occ, country_column_candidates())
  )
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
