#' Record ids excluded from the cleaned export
#'
#' @param decisions A [DecisionRegistry].
#' @param findings Optional findings tibble.
#' @export
excluded_from_cleaned_ids <- function(decisions, findings = NULL) {
  failed <- as.character(decisions$removed_occsclean_ids())

  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1) {
    return(unique(failed))
  }
  if (!("occsclean_id" %in% names(findings))) {
    return(unique(failed))
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
    return(unique(failed))
  }

  joined <- findings_with_decisions(f, decisions)
  not_passed <- unique(as.character(joined$occsclean_id[
    as.character(joined$decision) != "keep"
  ]))
  unique(c(failed, not_passed))
}

#' Build the cleaned occurrence table from session decisions
#'
#' @param occ_raw Original occurrence tibble.
#' @param decisions A [DecisionRegistry].
#' @param findings Optional findings tibble.
#' @export
build_cleaned_occurrences <- function(occ_raw, decisions, findings = NULL) {
  if (is.null(occ_raw) || nrow(occ_raw) < 1) {
    rlang::abort("No occurrence data to clean.", call = NULL)
  }
  excluded <- excluded_from_cleaned_ids(decisions, findings = findings)
  if (length(excluded) < 1) {
    return(tibble::as_tibble(occ_raw))
  }
  tibble::as_tibble(occ_raw[!occ_raw$occsclean_id %in% excluded, , drop = FALSE])
}

#' Summarize export contents for the UI
#'
#' @param occ_raw Original tibble.
#' @param decisions A [DecisionRegistry].
#' @param findings Optional findings tibble.
#' @export
export_counts <- function(occ_raw, decisions, findings = NULL) {
  n_raw <- if (is.null(occ_raw)) 0L else nrow(occ_raw)
  if (n_raw < 1) {
    return(list(n_original = 0L, n_removed = 0L, n_cleaned = 0L))
  }
  excluded <- excluded_from_cleaned_ids(decisions, findings = findings)
  n_removed <- as.integer(sum(as.character(occ_raw$occsclean_id) %in% excluded))
  list(
    n_original = as.integer(n_raw),
    n_removed = n_removed,
    n_cleaned = as.integer(n_raw - n_removed)
  )
}

#' Packages to cite based on checks in an assessment
#'
#' @param assessment Named list of [occ_check_result] objects.
#' @export
packages_used_by_assessment <- function(assessment) {
  if (is.null(assessment) || length(assessment) < 1) {
    return(character())
  }
  ids <- unique(as.character(names(assessment)))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(character())
  }

  catalog <- list_quality_checks()
  via <- catalog$method_via[match(ids, catalog$check_id)]
  unique(as.character(via[!is.na(via) & nzchar(as.character(via))]))
}

#' Recommended citations for this cleaning session
#'
#' @param session An [OccSession].
#' @export
build_session_citations <- function(session) {
  if (!session$has_data()) {
    rlang::abort("No occurrence data loaded.", call = NULL)
  }

  assessment <- session$get_assessment()
  ver <- tryCatch(
    as.character(utils::packageVersion("OccsClean")),
    error = function(e) "development"
  )

  lines <- c(
    "Recommended citations for this OccsClean cleaning session",
    "=========================================================",
    paste0("Generated: ", format(Sys.time(), usetz = TRUE)),
    "",
    "OccsClean",
    "---------",
    "OccsClean citation WIP",
    paste0("Version: ", ver),
    "Also credit any data providers for the occurrence records (e.g. GBIF).",
    "",
    "Checks used in this session",
    "---------------------------"
  )

  if (length(assessment) < 1) {
    lines <- c(
      lines,
      "(No checks have been run yet. Package citations below will be empty.)"
    )
  } else {
    catalog <- list_quality_checks()
    for (res in assessment) {
      via <- catalog$method_via[match(res$check_id, catalog$check_id)]
      via_txt <- if (!is.na(via) && nzchar(as.character(via))) {
        paste0(" (via ", via, ")")
      } else {
        ""
      }
      lines <- c(
        lines,
        paste0("- ", res$label, " [", res$check_id, "]", via_txt)
      )
    }
  }

  pkgs <- packages_used_by_assessment(assessment)
  lines <- c(lines, "")
  if (length(pkgs) < 1) {
    lines <- c(
      lines,
      "Suggested citations for key backend packages used by OccsClean",
      "--------------------------------------------------------------",
      "(No method_via packages to cite for the checks run so far.)"
    )
    return(paste(lines, collapse = "\n"))
  }

  cite_txt <- occsclean_package_citations(packages = pkgs)
  paste(c(lines, cite_txt), collapse = "\n")
}

#' Build a plain-text processing log of checks and filtering
#'
#' @param session An [OccSession].
#' @export
build_processing_log <- function(session) {
  if (!session$has_data()) {
    rlang::abort("No occurrence data loaded.", call = NULL)
  }

  meta <- session$get_meta()
  assessment <- session$get_assessment()
  counts <- export_counts(
    session$get_occ_raw(),
    session$get_decisions(),
    findings = session$get_findings_table()
  )
  ver <- tryCatch(
    as.character(utils::packageVersion("OccsClean")),
    error = function(e) "development"
  )

  lines <- c(
    "OccsClean processing log",
    "========================",
    paste0("OccsClean version: ", ver),
    paste0("Generated: ", format(Sys.time(), usetz = TRUE)),
    "",
    "Data",
    "----",
    paste0("Source file: ", meta$source_name %||% "(unknown)"),
    paste0("Imported: ", format(meta$imported_at, usetz = TRUE)),
    paste0("Delimiter detected: ", meta$delimiter %||% "(unknown)"),
    paste0("Original records: ", counts$n_original),
    paste0("Failed (excluded from cleaned): ", counts$n_removed),
    paste0("Cleaned records: ", counts$n_cleaned),
    "",
    "Checks conducted",
    "----------------"
  )

  if (length(assessment) < 1) {
    lines <- c(lines, "(No checks have been run yet.)")
  } else {
    flagged_all <- findings_with_decisions(
      session$get_findings_table(),
      session$get_decisions()
    )

    run_times <- lapply(assessment, function(res) res$timestamp)
    run_times <- run_times[!vapply(run_times, is.null, logical(1))]
    if (length(run_times) > 0) {
      lines <- c(
        lines,
        paste0("Assessed: ", format(run_times[[1]], usetz = TRUE)),
        ""
      )
    }
    for (res in assessment) {
      n_flagged <- res$summary$n_flagged %||% NA_integer_
      n_checked <- res$summary$n_checked %||% NA_integer_
      decision_counts <- flag_decision_counts(flagged_all, res$check_id)
      params <- res$params_used
      param_txt <- format_params_for_log(params)
      lines <- c(
        lines,
        paste0("- ", res$label, " [", res$check_id, "]"),
        paste0("    check: ", format_check_run_outcome(res$status)),
        paste0("    flagged/checked: ", n_flagged, "/", n_checked)
      )
      if (!is.na(decision_counts$n_flagged) && decision_counts$n_flagged > 0) {
        lines <- c(
          lines,
          paste0(
            "    failed: ",
            decision_counts$n_removed,
            " of ",
            decision_counts$n_flagged,
            " flagged"
          ),
          paste0(
            "    passed: ",
            decision_counts$n_kept,
            " of ",
            decision_counts$n_flagged,
            " flagged"
          )
        )
        if (decision_counts$n_unreviewed > 0) {
          lines <- c(
            lines,
            paste0(
              "    in review: ",
              decision_counts$n_unreviewed,
              " of ",
              decision_counts$n_flagged,
              " flagged"
            )
          )
        }
      }
      if (!is.null(param_txt) && nzchar(param_txt)) {
        lines <- c(lines, paste0("    params: ", param_txt))
      }
      if (length(res$messages) > 0) {
        lines <- c(
          lines,
          paste0("    messages: ", paste(res$messages, collapse = " | "))
        )
      }
    }
  }

  paste(lines, collapse = "\n")
}

#' Format check params for the processing log
#' @param params Named list from an [occ_check_result].
#' @noRd
format_params_for_log <- function(params) {
  if (is.null(params) || length(params) < 1) {
    return(NULL)
  }
  nms <- names(params)
  if (is.null(nms)) {
    nms <- as.character(seq_along(params))
  }
  parts <- vapply(seq_along(params), function(i) {
    nm <- nms[[i]]
    val <- params[[i]]
    formatted <- format_param_value_for_log(nm, val)
    if (is.null(formatted) || !nzchar(formatted)) {
      return(NA_character_)
    }
    paste0(nm, "=", formatted)
  }, character(1))
  parts <- parts[!is.na(parts)]
  if (length(parts) < 1) {
    return(NULL)
  }
  paste(parts, collapse = "; ")
}

#' Format one param value for the processing log
#' @noRd
format_param_value_for_log <- function(name, val) {
  if (is.null(val)) {
    return(NULL)
  }
  if (identical(name, "area_geom") || identical(name, "area_sf") ||
        inherits(val, c("sf", "sfc", "sfg"))) {
    return(NULL)
  }
  if (is.list(val) && !is.data.frame(val)) {
    if (length(val) < 1) {
      return(NULL)
    }
    if ("area_source" %in% names(val) || "area_geom" %in% names(val) ||
          "area_sf" %in% names(val)) {
      bits <- character()
      src <- val$area_source
      if (!is.null(src) && length(src) > 0 && any(nzchar(as.character(src)))) {
        bits <- c(bits, paste(as.character(src), collapse = ", "))
      } else {
        bits <- c(bits, "(shapefile)")
      }
      dist <- val$outside_distance_m
      if (!is.null(dist) && length(dist) > 0 && any(is.finite(as.numeric(dist)))) {
        bits <- c(bits, paste0("outside_distance_m=", dist[[1]]))
      }
      if (isTRUE(val$geometry_repaired)) {
        bits <- c(bits, "geometry_repaired=TRUE")
      }
      return(paste(bits, collapse = ", "))
    }
    inner <- format_params_for_log(val)
    if (is.null(inner) || !nzchar(inner)) {
      return(NULL)
    }
    return(paste0("{", inner, "}"))
  }
  txt <- paste(as.character(val), collapse = ",")
  if (!nzchar(txt)) {
    return(NULL)
  }
  if (nchar(txt) > 200) {
    return(paste0(substr(txt, 1, 197), "..."))
  }
  txt
}

#' Count pass, fail, and unreviewed decisions for one check
#' @param flagged_all Findings tibble with `check_id` and `decision`.
#' @param check_id Check id.
#' @noRd
flag_decision_counts <- function(flagged_all, check_id) {
  if (is.null(flagged_all) || nrow(flagged_all) < 1 ||
        !"check_id" %in% names(flagged_all)) {
    return(list(
      n_flagged = 0L,
      n_removed = 0L,
      n_kept = 0L,
      n_unreviewed = 0L
    ))
  }
  rows <- flagged_all[as.character(flagged_all$check_id) == check_id, , drop = FALSE]
  decisions <- as.character(rows$decision)
  list(
    n_flagged = nrow(rows),
    n_removed = sum(decisions == "remove", na.rm = TRUE),
    n_kept = sum(decisions == "keep", na.rm = TRUE),
    n_unreviewed = sum(
      is.na(decisions) | decisions == "unreviewed" | decisions == "(blank)",
      na.rm = TRUE
    )
  )
}

#' Map check status to a short label
#' @param status One of `ok`, `skipped`, `error`.
#' @noRd
format_check_run_outcome <- function(status) {
  switch(
    as.character(status),
    ok = "successful",
    skipped = "skipped",
    error = "failed",
    as.character(status)
  )
}
