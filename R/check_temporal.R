#' Flag records with unparseable dates
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_invalid_dates <- function(occ, params = list()) {
  check_id <- "date_invalid"
  label <- "Unparseable dates"
  category <- "temporal"

  cols <- resolve_occurrence_columns(occ)
  date_col <- params$date_col %||% cols$date

  if (is.null(date_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find a date column.",
      params_used = params
    ))
  }

  raw <- occ[[date_col]]
  parsed <- parse_occurrence_dates(raw)
  present <- !is_blank_coord(raw)
  invalid <- present & is.na(parsed)
  idx <- which(invalid)

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "DATE_UNPARSEABLE",
    reason = "Date value could not be parsed (not an age/range filter).",
    evidence = paste0(date_col, "=", format_evidence_value(raw[idx])),
    recommended_action = "remove",
    severity = "medium"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(params, list(date_col = date_col)),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}

#' Flag records with dates in the future
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_future_dates <- function(occ, params = list()) {
  check_id <- "date_future"
  label <- "Future dates"
  category <- "temporal"

  cols <- resolve_occurrence_columns(occ)
  date_col <- params$date_col %||% cols$date
  as_of <- params$as_of %||% Sys.Date()
  as_of <- as.Date(as_of)

  if (is.null(date_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find a date column.",
      params_used = params
    ))
  }

  raw <- occ[[date_col]]
  parsed <- parse_occurrence_dates(raw)
  future <- !is.na(parsed) & parsed > as_of
  idx <- which(future)

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "DATE_FUTURE",
    reason = paste0("Date is after ", as.character(as_of), "."),
    evidence = paste0(date_col, "=", format_evidence_value(raw[idx])),
    recommended_action = "remove",
    severity = "medium"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(
      params,
      list(date_col = date_col, as_of = as.character(as_of))
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}

#' Flag dates outside a user-defined range
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_date_out_of_range <- function(occ, params = list()) {
  check_id <- "date_out_of_range"
  label <- "Dates outside range"
  category <- "temporal"

  cols <- resolve_occurrence_columns(occ)
  date_col <- params$date_col %||% cols$date
  min_date <- parse_param_date(params$min_date)
  max_date <- parse_param_date(params$max_date)

  if (is.null(min_date) && is.null(max_date)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "No min/max date set; skipped. Provide a date range in Assess.",
      params_used = params
    ))
  }

  if (is.null(date_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find a date column.",
      params_used = params
    ))
  }

  if (!is.null(min_date) && !is.null(max_date) && min_date > max_date) {
    return(new_occ_check_result(
      check_id = check_id,
      label = label,
      category = category,
      status = "error",
      findings = empty_findings(),
      params_used = params,
      engine = "native",
      summary = list(n_checked = 0L, n_flagged = 0L, n_skipped_rows = 0L),
      messages = "min_date is after max_date."
    ))
  }

  raw <- occ[[date_col]]
  parsed <- parse_occurrence_dates(raw)

  too_early <- rep(FALSE, length(parsed))
  too_late <- rep(FALSE, length(parsed))
  if (!is.null(min_date)) {
    too_early <- !is.na(parsed) & parsed < min_date
  }
  if (!is.null(max_date)) {
    too_late <- !is.na(parsed) & parsed > max_date
  }

  bad <- too_early | too_late
  idx <- which(bad)

  reason <- rep("Date is outside the allowed range.", length(idx))
  if (length(idx) > 0) {
    if (!is.null(min_date)) {
      reason[too_early[idx]] <- paste0(
        "Date is before earliest allowed date (", as.character(min_date), ")."
      )
    }
    if (!is.null(max_date)) {
      reason[too_late[idx]] <- paste0(
        "Date is after latest allowed date (", as.character(max_date), ")."
      )
    }
  }

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "DATE_OUT_OF_RANGE",
    reason = reason,
    evidence = paste0(date_col, "=", format_evidence_value(raw[idx])),
    recommended_action = "remove",
    severity = "medium"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(
      params,
      list(
        date_col = date_col,
        min_date = if (is.null(min_date)) NULL else as.character(min_date),
        max_date = if (is.null(max_date)) NULL else as.character(max_date)
      )
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}

#' Parse occurrence date values to Date
#' @param x Atomic vector.
#' @noRd
parse_occurrence_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  xc <- trimws(as.character(x))
  xc[is.na(x)] <- NA_character_
  parsed <- rep(as.Date(NA), length(xc))

  ok <- !is.na(xc) & nzchar(xc)
  if (!any(ok)) {
    return(parsed)
  }

  vals <- xc
  has_slash <- ok & grepl("/", vals, fixed = TRUE)
  if (any(has_slash)) {
    vals[has_slash] <- sub("/.*$", "", vals[has_slash])
    vals[has_slash] <- trimws(vals[has_slash])
  }

  has_time <- ok & grepl("T", vals, fixed = TRUE)
  if (any(has_time)) {
    vals[has_time] <- sub("T.*$", "", vals[has_time])
  }

  year_only <- ok & grepl("^[0-9]{4}$", vals)
  if (any(year_only)) {
    parsed[year_only] <- safe_as_date(
      paste0(vals[year_only], "-01-01"),
      format = "%Y-%m-%d"
    )
  }

  year_month <- ok & is.na(parsed) & grepl("^[0-9]{4}-[0-9]{2}$", vals)
  if (any(year_month)) {
    parsed[year_month] <- safe_as_date(
      paste0(vals[year_month], "-01"),
      format = "%Y-%m-%d"
    )
  }

  still <- ok & is.na(parsed)
  if (any(still)) {
    parsed[still] <- safe_as_date(vals[still], format = "%Y-%m-%d")
  }

  still <- ok & is.na(parsed)
  if (any(still)) {
    parsed[still] <- safe_as_date(vals[still], format = "%Y/%m/%d")
  }

  still <- ok & is.na(parsed)
  if (any(still)) {
    parsed[still] <- safe_as_date(vals[still], format = "%m/%d/%Y")
  }

  still <- ok & is.na(parsed)
  if (any(still)) {
    parsed[still] <- safe_as_date(vals[still], format = "%d/%m/%Y")
  }

  parsed
}

#' as.Date that returns NA on failure instead of erroring
#' @noRd
safe_as_date <- function(x, format = NULL) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))
  if (length(x) < 1) {
    return(out)
  }
  if (is.null(format)) {
    for (i in seq_along(x)) {
      if (is.na(x[[i]]) || !nzchar(x[[i]])) {
        next
      }
      out[[i]] <- tryCatch(
        as.Date(x[[i]]),
        error = function(e) as.Date(NA),
        warning = function(w) as.Date(NA)
      )
    }
    return(out)
  }
  suppressWarnings(as.Date(x, format = format))
}

#' Parse an optional date parameter
#' @param x NULL, Date, or character.
#' @noRd
parse_param_date <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    if (length(x) != 1 || is.na(x)) {
      return(NULL)
    }
    return(x)
  }
  xc <- trimws(as.character(x))
  if (length(xc) != 1 || is.na(xc) || !nzchar(xc)) {
    return(NULL)
  }
  d <- safe_as_date(xc, format = "%Y-%m-%d")
  if (is.na(d)) {
    d <- safe_as_date(xc)
  }
  if (is.na(d)) {
    rlang::abort(
      paste0("Could not parse date parameter: ", xc),
      call = NULL
    )
  }
  d
}
