#' Occurrence-level review status from findings and decisions
#' @param findings Findings tibble.
#' @param decisions A [DecisionRegistry].
#' @param occ Optional occurrence tibble for manual-review metadata.
#' @param column_map Optional resolved column map for `occ`.
#' @noRd
build_review_occurrences <- function(findings,
                                     decisions,
                                     occ = NULL,
                                     column_map = NULL) {
  empty <- tibble::tibble(
    occsclean_id = character(),
    review_status = character(),
    n_flags = integer(),
    check_ids = character(),
    checks = character(),
    scientificName = character(),
    decimalLongitude = character(),
    decimalLatitude = character(),
    occurrence_date = character(),
    basisOfRecord = character()
  )

  out <- empty
  if (!is.null(findings) && is.data.frame(findings) && nrow(findings) > 0 &&
        "occsclean_id" %in% names(findings)) {
    f <- findings
    if ("check_id" %in% names(f)) {
      f <- f[
        as.character(f$check_id) != map_review_check_id(),
        ,
        drop = FALSE
      ]
    }
    if (nrow(f) > 0) {
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
      joined$check_id <- as.character(joined$check_id)
      joined$check_id[is.na(joined$check_id)] <- ""

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

      batch_fail_ids <- batch_failed_occsclean_ids(decisions)
      batch_pass_ids <- batch_passed_occsclean_ids(decisions)

      out <- dplyr::summarise(
        dplyr::group_by(joined, .data$occsclean_id),
        review_status = {
          d <- .data$decision
          occ_id <- as.character(.data$occsclean_id[[1]])
          if (any(d == "remove", na.rm = TRUE)) {
            if (occ_id %in% batch_fail_ids) {
              "batch_fail"
            } else {
              "fail"
            }
          } else if (length(d) > 0L && all(d == "keep")) {
            if (occ_id %in% batch_pass_ids) {
              "batch_pass"
            } else {
              "pass"
            }
          } else {
            "review"
          }
        },
        n_flags = dplyr::n(),
        check_ids = paste(
          sort(unique(.data$check_id[nzchar(.data$check_id)])),
          collapse = ","
        ),
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
    }
  }

  out <- append_manual_review_occurrences(
    out,
    findings = findings,
    decisions = decisions,
    occ = occ,
    column_map = column_map
  )
  out <- enrich_manual_review_occurrences(out, decisions)
  if (nrow(out) < 1) {
    return(empty)
  }
  out[order(out$occsclean_id), , drop = FALSE]
}

#' Occurrence ids with assessment findings (excluding map-review stubs)
#' @noRd
assessment_flagged_occsclean_ids <- function(findings) {
  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1 ||
        !"occsclean_id" %in% names(findings)) {
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
  if (nrow(f) < 1) {
    return(character())
  }
  unique(as.character(f$occsclean_id))
}

#' Read one occurrence field for the review table
#' @noRd
read_occurrence_review_field <- function(row, col) {
  if (is.null(col) || length(col) < 1L || !col %in% names(row)) {
    return(NA_character_)
  }
  val <- as.character(row[[col]])[[1]]
  if (is.na(val) || !nzchar(val)) NA_character_ else val
}

#' Occurrence metadata for manually reviewed unflagged records
#' @noRd
occurrence_metadata_for_review <- function(occ, occsclean_id, column_map = NULL) {
  empty <- list(
    scientificName = NA_character_,
    decimalLongitude = NA_character_,
    decimalLatitude = NA_character_,
    occurrence_date = NA_character_,
    basisOfRecord = NA_character_
  )
  if (is.null(occ) || !is.data.frame(occ) || nrow(occ) < 1) {
    return(empty)
  }
  id <- as.character(occsclean_id)[[1]]
  hit <- occ[as.character(occ$occsclean_id) == id, , drop = FALSE]
  if (nrow(hit) < 1) {
    return(empty)
  }
  row <- hit[1, , drop = FALSE]
  cols <- if (is.list(column_map) && length(column_map) > 0) {
    column_map
  } else {
    resolve_occurrence_columns(occ)
  }

  taxon_col <- cols$taxon %||% "scientificName"
  lon_col <- cols$lon %||% "decimalLongitude"
  lat_col <- cols$lat %||% "decimalLatitude"
  date_col <- cols$date %||% "occurrence_date"
  basis_col <- cols$basis_of_record %||% "basisOfRecord"

  taxon <- read_occurrence_review_field(row, taxon_col)
  if (is.na(taxon)) {
    taxon <- read_occurrence_review_field(row, "scientificName")
  }

  list(
    scientificName = taxon,
    decimalLongitude = read_occurrence_review_field(row, lon_col),
    decimalLatitude = read_occurrence_review_field(row, lat_col),
    occurrence_date = read_occurrence_review_field(row, date_col),
    basisOfRecord = read_occurrence_review_field(row, basis_col)
  )
}

#' Add manually reviewed unflagged records to the review table
#' @noRd
append_manual_review_occurrences <- function(out,
                                             findings,
                                             decisions,
                                             occ = NULL,
                                             column_map = NULL) {
  if (is.null(decisions) || !inherits(decisions, "DecisionRegistry")) {
    return(out)
  }
  eff <- decisions$effective()
  if (nrow(eff) < 1) {
    return(out)
  }

  map_id <- map_review_check_id()
  map_eff <- eff[
    as.character(eff$check_id) == map_id &
      as.character(eff$action) %in% c("keep", "remove", "unreviewed"),
    ,
    drop = FALSE
  ]
  if (nrow(map_eff) < 1) {
    return(out)
  }

  flagged_ids <- assessment_flagged_occsclean_ids(findings)
  existing_ids <- if (!is.null(out) && nrow(out) > 0) {
    as.character(out$occsclean_id)
  } else {
    character()
  }

  map_eff$occsclean_id <- as.character(map_eff$occsclean_id)
  map_eff$action <- as.character(map_eff$action)
  manual_ids <- unique(map_eff$occsclean_id)
  manual_ids <- manual_ids[
    !manual_ids %in% flagged_ids & !manual_ids %in% existing_ids
  ]
  if (length(manual_ids) < 1) {
    return(out)
  }

  manual_review_status <- function(action, finding = NA_character_) {
    switch(
      action,
      remove = if (identical(as.character(finding), map_review_batch_finding())) {
        "batch_fail"
      } else {
        "fail"
      },
      keep = if (identical(as.character(finding), map_review_batch_pass_finding())) {
        "batch_pass"
      } else {
        "pass"
      },
      unreviewed = "review",
      NULL
    )
  }

  rows <- lapply(manual_ids, function(id) {
    idx <- match(id, map_eff$occsclean_id)
    action <- map_eff$action[idx]
    finding <- map_eff$finding[idx]
    status <- manual_review_status(action, finding)
    if (is.null(status)) {
      return(NULL)
    }
    meta <- occurrence_metadata_for_review(occ, id, column_map = column_map)
    has_manual <- id %in% manual_review_display_occsclean_ids(decisions)
    tibble::tibble(
      occsclean_id = id,
      review_status = status,
      n_flags = if (has_manual) 1L else 0L,
      check_ids = if (has_manual) manual_review_check_id() else "",
      checks = if (has_manual) manual_review_check_label() else "",
      scientificName = meta$scientificName,
      decimalLongitude = meta$decimalLongitude,
      decimalLatitude = meta$decimalLatitude,
      occurrence_date = meta$occurrence_date,
      basisOfRecord = meta$basisOfRecord
    )
  })
  dplyr::bind_rows(out, dplyr::bind_rows(rows))
}

#' Fill manual-review flag columns on occurrence review rows
#' @noRd
enrich_manual_review_occurrences <- function(out, decisions) {
  if (is.null(out) || nrow(out) < 1) {
    return(out)
  }
  ids <- manual_review_display_occsclean_ids(decisions)
  if (length(ids) < 1) {
    return(out)
  }
  hit <- as.character(out$occsclean_id) %in% ids
  if (!any(hit)) {
    return(out)
  }
  out$n_flags[hit] <- pmax(as.integer(out$n_flags[hit]), 1L)
  out$check_ids[hit] <- manual_review_check_id()
  out$checks[hit] <- manual_review_check_label()
  out
}

#' Add manual-review finding rows for export and decision joins
#' @noRd
append_manual_review_findings <- function(findings,
                                          decisions,
                                          occ = NULL,
                                          column_map = NULL) {
  if (is.null(decisions) || !inherits(decisions, "DecisionRegistry")) {
    return(findings)
  }
  eff <- decisions$effective()
  manual_eff <- eff[
    as.character(eff$check_id) == manual_review_check_id() &
      as.character(eff$finding) == manual_review_finding(),
    ,
    drop = FALSE
  ]
  if (nrow(manual_eff) < 1) {
    return(findings)
  }

  existing_keys <- character()
  if (!is.null(findings) && is.data.frame(findings) && nrow(findings) > 0 &&
        all(c("occsclean_id", "check_id", "finding") %in% names(findings))) {
    existing_keys <- paste(
      as.character(findings$occsclean_id),
      as.character(findings$check_id),
      ifelse(is.na(findings$finding), "", as.character(findings$finding)),
      sep = "\r"
    )
  }

  manual_eff$occsclean_id <- as.character(manual_eff$occsclean_id)
  new_keys <- paste(
    manual_eff$occsclean_id,
    manual_review_check_id(),
    manual_review_finding(),
    sep = "\r"
  )
  manual_eff <- manual_eff[!new_keys %in% existing_keys, , drop = FALSE]
  if (nrow(manual_eff) < 1) {
    return(findings)
  }

  ids <- unique(as.character(manual_eff$occsclean_id))
  rows <- lapply(ids, function(id) {
    meta <- occurrence_metadata_for_review(occ, id, column_map = column_map)
    tibble::tibble(
      check_id = manual_review_check_id(),
      check_label = manual_review_check_label(),
      category = "manual",
      occsclean_id = id,
      finding = manual_review_finding(),
      reason = manual_review_reason(),
      evidence = NA_character_,
      recommended_action = "review",
      severity = NA_character_,
      scientificName = meta$scientificName,
      decimalLongitude = meta$decimalLongitude,
      decimalLatitude = meta$decimalLatitude,
      occurrence_date = meta$occurrence_date,
      basisOfRecord = meta$basisOfRecord
    )
  })
  manual_rows <- dplyr::bind_rows(rows)

  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1) {
    return(manual_rows)
  }
  align_finding_rows_to_template(manual_rows, findings)
}

#' Coerce manual-review rows to match an existing findings table
#' @noRd
align_finding_rows_to_template <- function(rows, template) {
  if (nrow(rows) < 1) {
    return(rows)
  }
  out <- rows
  all_cols <- union(names(template), names(out))
  for (col in all_cols) {
    if (!col %in% names(out)) {
      out[[col]] <- recycle_na_like(template[[col]], nrow(out))
      next
    }
    if (col %in% names(template)) {
      out[[col]] <- coerce_like_column(out[[col]], template[[col]])
    }
  }
  dplyr::bind_rows(template, out[names(template)])
}

#' @noRd
recycle_na_like <- function(template_col, n) {
  if (is.numeric(template_col)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(template_col)) {
    return(rep(NA, n))
  }
  if (inherits(template_col, "POSIXct")) {
    return(rep(as.POSIXct(NA, tz = "UTC"), n))
  }
  if (inherits(template_col, "Date")) {
    return(rep(as.Date(NA), n))
  }
  rep(NA_character_, n)
}

#' @noRd
coerce_like_column <- function(values, template_col) {
  if (is.numeric(template_col)) {
    return(suppressWarnings(as.numeric(values)))
  }
  if (is.logical(template_col)) {
    return(as.logical(values))
  }
  if (inherits(template_col, "POSIXct")) {
    return(suppressWarnings(as.POSIXct(values, tz = "UTC")))
  }
  if (inherits(template_col, "Date")) {
    return(suppressWarnings(as.Date(values)))
  }
  as.character(values)
}

#' Flag an unflagged occurrence for manual review
#' @noRd
flag_unflagged_for_manual_review <- function(decisions, record_ids) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(0L)
  }
  n <- as.integer(
    decisions$record_many(manual_review_decision_rows(ids), action = "unreviewed")
  )
  n <- n + as.integer(
    decisions$record_many(map_decision_rows(ids), action = "unreviewed")
  )
  n
}

#' Prepare occurrence review table for DT
#' @param findings Findings tibble.
#' @param decisions A [DecisionRegistry].
#' @param occ Optional occurrence tibble for manual-review metadata.
#' @param column_map Optional resolved column map for `occ`.
#' @noRd
prepare_review_occurrence_table <- function(findings,
                                            decisions,
                                            occ = NULL,
                                            column_map = NULL) {
  out <- build_review_occurrences(
    findings,
    decisions,
    occ = occ,
    column_map = column_map
  )
  if (nrow(out) < 1) {
    return(out)
  }

  factor_cols <- c(
    "review_status", "n_flags", "basisOfRecord"
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

#' Distinct column values for occurrence review table filters
#' @noRd
review_column_filter_choices <- function(occ_df, column) {
  if (is.null(occ_df) || nrow(occ_df) < 1 || !column %in% names(occ_df)) {
    return(character())
  }
  vals <- as.character(occ_df[[column]])
  vals[is.na(vals) | !nzchar(vals)] <- "(blank)"
  sort(unique(vals))
}

#' Distinct check labels from occurrence checks column text
#' @noRd
review_checks_column_filter_choices <- function(occ_df) {
  if (is.null(occ_df) || nrow(occ_df) < 1 || !"checks" %in% names(occ_df)) {
    return(character())
  }
  vals <- as.character(occ_df$checks)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) < 1) {
    return(character())
  }
  parts <- unique(trimws(unlist(strsplit(vals, ";", fixed = TRUE))))
  parts <- parts[nzchar(parts)]
  sort(parts)
}

#' Display labels for occurrence review_status values
#' @noRd
occurrence_review_status_label <- function(status) {
  out <- as.character(status)
  out[out == "review"] <- "In review"
  out[out == "pass"] <- "Passed"
  out[out == "fail"] <- "Failed"
  out[out == "batch_fail"] <- "Batch Failed"
  out[out == "batch_pass"] <- "Batch Passed"
  out
}

#' Check filter choices for occurrence review tables
#' @noRd
review_check_filter_choices <- function(occ_df, findings) {
  if (is.null(occ_df) || nrow(occ_df) < 1 ||
        is.null(findings) || nrow(findings) < 1) {
    return(list())
  }
  hit <- findings[
    as.character(findings$occsclean_id) %in% as.character(occ_df$occsclean_id),
    ,
    drop = FALSE
  ]
  if (!("check_id" %in% names(hit))) {
    return(list())
  }
  hit <- hit[
    as.character(hit$check_id) != map_review_check_id(),
    ,
    drop = FALSE
  ]
  ids <- sort(unique(as.character(hit$check_id)))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(list())
  }
  label_col <- if ("check_label" %in% names(hit)) {
    "check_label"
  } else if ("check" %in% names(hit)) {
    "check"
  } else {
    NULL
  }
  labels <- vapply(ids, function(id) {
    rows <- hit[as.character(hit$check_id) == id, , drop = FALSE]
    if (!is.null(label_col)) {
      val <- as.character(rows[[label_col]][[1]])
      if (!is.na(val) && nzchar(val)) {
        return(val)
      }
    }
    id
  }, character(1))
  stats::setNames(as.list(ids), labels)
}

#' Sentinel value for the review Check flag "show all" option
#' @noRd
review_check_flag_all_value <- function() {
  "__all__"
}

#' Whether the review Check flag shows all checks
#' @noRd
review_check_flag_is_all <- function(check_flag) {
  val <- as.character(check_flag %||% "")[[1]]
  identical(val, review_check_flag_all_value()) ||
    identical(val, "") ||
    !nzchar(val)
}

#' Keep occurrences whose checks column includes a check label
#' @noRd
filter_review_occurrences_by_check_label <- function(occ_df, check_label) {
  if (review_check_flag_is_all(check_label)) {
    return(occ_df)
  }
  label <- as.character(check_label %||% "")[[1]]
  if (is.null(occ_df) || nrow(occ_df) < 1 || !"checks" %in% names(occ_df)) {
    return(occ_df)
  }
  keep <- vapply(
    occ_df$checks,
    function(checks_text) {
      parts <- trimws(strsplit(as.character(checks_text), ";", fixed = TRUE)[[1]])
      label %in% parts
    },
    logical(1)
  )
  occ_df[keep, , drop = FALSE]
}

#' Keep occurrences flagged by all selected checks
#' @noRd
filter_review_occurrences_by_checks <- function(occ_df, selected_check_ids) {
  selected <- unique(as.character(selected_check_ids))
  selected <- selected[nzchar(selected)]
  if (length(selected) < 1) {
    return(occ_df)
  }
  if (is.null(occ_df) || nrow(occ_df) < 1 || !"check_ids" %in% names(occ_df)) {
    return(occ_df)
  }
  keep <- vapply(
    strsplit(as.character(occ_df$check_ids), ",", fixed = TRUE),
    function(ids) all(selected %in% ids),
    logical(1)
  )
  occ_df[keep, , drop = FALSE]
}

#' Occurrence ids flagged by a check label
#' @noRd
occsclean_ids_with_check_label <- function(findings, check_label) {
  label <- as.character(check_label %||% "")[[1]]
  if (!nzchar(label) || is.null(findings) || nrow(findings) < 1) {
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
  label_col <- if ("check_label" %in% names(f)) {
    "check_label"
  } else if ("check" %in% names(f)) {
    "check"
  } else {
    return(character())
  }
  unique(as.character(f$occsclean_id[
    as.character(f[[label_col]]) == label
  ]))
}

#' Occurrence ids flagged only by a given check label
#' @noRd
occsclean_ids_with_only_check_label <- function(findings, check_label) {
  label <- as.character(check_label %||% "")[[1]]
  if (!nzchar(label) || is.null(findings) || nrow(findings) < 1) {
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
  label_col <- if ("check_label" %in% names(f)) {
    "check_label"
  } else if ("check" %in% names(f)) {
    "check"
  } else {
    return(character())
  }
  f$occsclean_id <- as.character(f$occsclean_id)
  f$check_display <- as.character(f[[label_col]])
  f <- f[!is.na(f$check_display) & nzchar(f$check_display), , drop = FALSE]
  if (nrow(f) < 1) {
    return(character())
  }
  labels_per_occ <- tapply(f$check_display, f$occsclean_id, function(x) {
    unique(x)
  })
  only_one <- names(labels_per_occ)[
    vapply(labels_per_occ, length, integer(1)) == 1L
  ]
  has_label <- unique(f$occsclean_id[f$check_display == label])
  intersect(only_one, has_label)
}

#' Occurrence ids in a review subset that carry a check label
#' @noRd
occsclean_ids_for_check_in_review <- function(
    occ_df,
    findings,
    check_label,
    only = FALSE) {
  if (is.null(occ_df) || nrow(occ_df) < 1) {
    return(character())
  }
  label <- as.character(check_label %||% "")[[1]]
  if (!nzchar(label)) {
    return(character())
  }
  tab_ids <- as.character(occ_df$occsclean_id)
  with_check <- if (isTRUE(only)) {
    occsclean_ids_with_only_check_label(findings, label)
  } else {
    occsclean_ids_with_check_label(findings, label)
  }
  intersect(tab_ids, with_check)
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

#' Review map decision stub rows for many records
#' @noRd
map_decision_rows <- function(record_ids,
                              batch = FALSE,
                              outcome = c("pass", "fail")) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  outcome <- match.arg(outcome)
  finding <- if (isTRUE(batch)) {
    if (outcome == "pass") {
      map_review_batch_pass_finding()
    } else {
      map_review_batch_finding()
    }
  } else {
    map_review_finding()
  }
  tibble::tibble(
    occsclean_id = ids,
    check_id = rep(map_review_check_id(), length(ids)),
    finding = rep(finding, length(ids))
  )
}

#' Pass many occurrences in batched writes
#' @noRd
pass_records <- function(decisions, record_ids, findings = NULL, batch = FALSE) {
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
    decisions$record_many(
      map_decision_rows(ids, batch = batch, outcome = "pass"),
      action = "keep"
    )
  )
  manual_clear <- intersect(
    ids,
    manual_review_display_occsclean_ids(decisions)
  )
  if (length(manual_clear) > 0) {
    n <- n + as.integer(
      decisions$record_many(
        manual_review_decision_rows(manual_clear),
        action = "keep"
      )
    )
  }
  n
}

#' Fail many occurrences in batched writes
#' @noRd
fail_records <- function(decisions, record_ids, findings = NULL, batch = FALSE) {
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
    decisions$record_many(
      map_decision_rows(ids, batch = batch, outcome = "fail"),
      action = "remove"
    )
  )
  manual_hit <- intersect(ids, manual_review_occsclean_ids(decisions))
  if (length(manual_hit) > 0) {
    n <- n + as.integer(
      decisions$record_many(
        manual_review_decision_rows(manual_hit),
        action = "remove"
      )
    )
  }
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
  n <- n + as.integer(
    decisions$record_many(map_decision_rows(ids), action = "unreviewed")
  )
  manual_hit <- intersect(ids, manual_review_occsclean_ids(decisions))
  if (length(manual_hit) > 0) {
    n <- n + as.integer(
      decisions$record_many(
        manual_review_decision_rows(manual_hit),
        action = "unreviewed"
      )
    )
  }
  n
}

#' Return one occurrence to Review
#' @noRd
return_record_to_review <- function(decisions, occsclean_id, findings = NULL) {
  return_records_to_review(decisions, occsclean_id, findings = findings)
}
