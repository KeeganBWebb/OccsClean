#' Occurrence-level review status from findings and decisions
#' @param findings Findings tibble.
#' @param decisions A [DecisionRegistry].
#' @noRd
build_review_occurrences <- function(findings, decisions) {
  empty <- tibble::tibble(
    occsclean_id = character(),
    review_status = character(),
    n_flags = integer(),
    findings = character(),
    checks = character(),
    scientificName = character(),
    decimalLongitude = character(),
    decimalLatitude = character(),
    occurrence_date = character(),
    basisOfRecord = character()
  )

  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1) {
    return(empty)
  }
  if (!("occsclean_id" %in% names(findings))) {
    return(empty)
  }

  f <- findings
  if ("check_id" %in% names(f)) {
    f <- f[
      as.character(f$check_id) != map_review_check_id(),
      ,
      drop = FALSE
    ]
  }
  if (nrow(f) < 1) {
    return(empty)
  }

  joined <- findings_with_decisions(f, decisions)
  joined$occsclean_id <- as.character(joined$occsclean_id)
  joined$finding <- as.character(joined$finding)
  joined$finding[is.na(joined$finding)] <- ""
  joined$decision <- as.character(joined$decision)

  check_col <- if ("check_label" %in% names(joined)) {
    "check_label"
  } else if ("check" %in% names(joined)) {
    "check"
  } else {
    "check_id"
  }
  joined$check_display <- as.character(joined[[check_col]])
  joined$check_display[is.na(joined$check_display)] <- ""

  for (col in c(
    "scientificName", "decimalLongitude", "decimalLatitude",
    "occurrence_date", "basisOfRecord"
  )) {
    if (!col %in% names(joined)) {
      joined[[col]] <- NA_character_
    } else {
      joined[[col]] <- as.character(joined[[col]])
    }
  }

  first_nonblank <- function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) < 1) NA_character_ else x[[1]]
  }

  out <- dplyr::summarise(
    dplyr::group_by(joined, .data$occsclean_id),
    review_status = {
      d <- .data$decision
      if (any(d == "remove", na.rm = TRUE)) {
        "fail"
      } else if (length(d) > 0L && all(d == "keep")) {
        "pass"
      } else {
        "review"
      }
    },
    n_flags = dplyr::n(),
    findings = paste(unique(.data$finding[nzchar(.data$finding)]), collapse = "; "),
    checks = paste(
      unique(.data$check_display[nzchar(.data$check_display)]),
      collapse = "; "
    ),
    scientificName = first_nonblank(.data$scientificName),
    decimalLongitude = first_nonblank(.data$decimalLongitude),
    decimalLatitude = first_nonblank(.data$decimalLatitude),
    occurrence_date = first_nonblank(.data$occurrence_date),
    basisOfRecord = first_nonblank(.data$basisOfRecord),
    .groups = "drop"
  )

  if (nrow(out) < 1) {
    return(empty)
  }
  out[order(out$occsclean_id), , drop = FALSE]
}

#' Prepare occurrence review table for DT
#' @param findings Findings tibble.
#' @param decisions A [DecisionRegistry].
#' @noRd
prepare_review_occurrence_table <- function(findings, decisions) {
  out <- build_review_occurrences(findings, decisions)
  if (nrow(out) < 1) {
    return(out)
  }

  # Factor columns for DT dropdown filters (not numeric sliders).
  factor_cols <- c(
    "review_status", "n_flags", "findings", "checks", "scientificName",
    "basisOfRecord"
  )
  for (col in intersect(factor_cols, names(out))) {
    vals <- as.character(out[[col]])
    vals[is.na(vals) | !nzchar(vals)] <- "(blank)"
    if (identical(col, "n_flags")) {
      levels <- as.character(sort(unique(as.integer(vals[vals != "(blank)"]))))
      if (any(vals == "(blank)")) {
        levels <- c(levels, "(blank)")
      }
      out[[col]] <- factor(vals, levels = levels)
    } else {
      out[[col]] <- factor(vals, levels = sort(unique(vals)))
    }
  }
  out
}

#' Finding codes present on a set of occurrence ids
#' @noRd
finding_codes_for_records <- function(findings, record_ids) {
  if (is.null(findings) || nrow(findings) < 1 || length(record_ids) < 1) {
    return(character())
  }
  hit <- findings[
    as.character(findings$occsclean_id) %in% as.character(record_ids),
    ,
    drop = FALSE
  ]
  codes <- sort(unique(as.character(hit$finding)))
  codes[!is.na(codes) & nzchar(codes) & codes != "(blank)"]
}

#' Occurrence ids that carry a given finding code
#' @noRd
occsclean_ids_with_finding <- function(findings, finding_code) {
  code <- as.character(finding_code %||% "")[[1]]
  if (!nzchar(code) || is.null(findings) || nrow(findings) < 1) {
    return(character())
  }
  unique(as.character(findings$occsclean_id[
    as.character(findings$finding) == code
  ]))
}

#' Occurrence ids whose only finding is the given code
#' @noRd
occsclean_ids_with_only_finding <- function(findings, finding_code) {
  code <- as.character(finding_code %||% "")[[1]]
  if (!nzchar(code) || is.null(findings) || nrow(findings) < 1) {
    return(character())
  }
  f <- findings
  if ("check_id" %in% names(f)) {
    f <- f[
      as.character(f$check_id) != map_review_check_id(),
      ,
      drop = FALSE
    ]
  }
  f$occsclean_id <- as.character(f$occsclean_id)
  f$finding <- as.character(f$finding)
  f <- f[!is.na(f$finding) & nzchar(f$finding), , drop = FALSE]
  if (nrow(f) < 1) {
    return(character())
  }

  n_codes <- tapply(f$finding, f$occsclean_id, function(x) length(unique(x)))
  only <- names(n_codes)[n_codes == 1L]
  if (length(only) < 1) {
    return(character())
  }
  has_code <- unique(f$occsclean_id[f$finding == code])
  intersect(only, has_code)
}

#' Assess findings for a set of record ids (excludes mapping synthetic rows)
#' @noRd
assess_findings_for_records <- function(findings, record_ids) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  empty <- tibble::tibble(
    occsclean_id = character(),
    check_id = character(),
    finding = character()
  )
  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1 ||
        length(ids) < 1) {
    return(empty)
  }
  rows <- findings[as.character(findings$occsclean_id) %in% ids, , drop = FALSE]
  if ("check_id" %in% names(rows)) {
    rows <- rows[
      as.character(rows$check_id) != map_review_check_id(),
      ,
      drop = FALSE
    ]
  }
  rows
}

#' Mapping-level decision stub rows for many records
#' @noRd
map_decision_rows <- function(record_ids) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  tibble::tibble(
    occsclean_id = ids,
    check_id = rep(map_review_check_id(), length(ids)),
    finding = rep(map_review_finding(), length(ids))
  )
}

#' Pass many occurrences in batched writes
#' @noRd
pass_records <- function(decisions, record_ids, findings = NULL) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(0L)
  }

  n <- 0L
  rows <- assess_findings_for_records(findings, ids)
  if (nrow(rows) > 0) {
    n <- n + as.integer(decisions$record_many(rows, action = "keep"))
  }

  eff <- decisions$effective()
  if (nrow(eff) > 0) {
    pending <- eff[
      as.character(eff$occsclean_id) %in% ids &
        as.character(eff$action) == "remove",
      ,
      drop = FALSE
    ]
    if (nrow(pending) > 0) {
      n <- n + as.integer(decisions$record_many(pending, action = "keep"))
    }
  }

  n <- n + as.integer(
    decisions$record_many(map_decision_rows(ids), action = "keep")
  )
  n
}

#' Fail many occurrences in batched writes
#' @noRd
fail_records <- function(decisions, record_ids, findings = NULL) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(0L)
  }

  n <- 0L
  rows <- assess_findings_for_records(findings, ids)
  if (nrow(rows) > 0) {
    n <- n + as.integer(decisions$record_many(rows, action = "remove"))
  }
  n <- n + as.integer(
    decisions$record_many(map_decision_rows(ids), action = "remove")
  )
  n
}

#' Return many occurrences to Review in batched writes
#' @noRd
return_records_to_review <- function(decisions, record_ids, findings = NULL) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(0L)
  }

  n <- 0L
  rows <- assess_findings_for_records(findings, ids)
  if (nrow(rows) > 0) {
    n <- n + as.integer(decisions$record_many(rows, action = "unreviewed"))
  }

  eff <- decisions$effective()
  if (nrow(eff) > 0) {
    pending <- eff[
      as.character(eff$occsclean_id) %in% ids &
        as.character(eff$action) %in% c("keep", "remove"),
      ,
      drop = FALSE
    ]
    if (nrow(pending) > 0) {
      n <- n + as.integer(decisions$record_many(pending, action = "unreviewed"))
    }
  }
  n
}

#' Return one occurrence to Review
#' @noRd
return_record_to_review <- function(decisions, occsclean_id, findings = NULL) {
  return_records_to_review(decisions, occsclean_id, findings = findings)
}
