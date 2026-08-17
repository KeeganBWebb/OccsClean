#' Allowed status values for an [occ_check_result]
#' @noRd
check_result_statuses <- function() {
  c("ok", "skipped", "error")
}

#' Allowed user decision actions
#' @export
decision_actions <- function() {
  c("unreviewed", "keep", "remove", "correct")
}

#' Valid `recommended_action` values on findings
#' @export
recommended_actions <- function() {
  c("keep", "remove")
}

#' Empty findings tibble with the standard schema
#' @export
empty_findings <- function() {
  tibble::tibble(
    occsclean_id = character(),
    flag = logical(),
    finding = character(),
    reason = character(),
    evidence = character(),
    confidence = numeric(),
    recommended_action = character(),
    severity = character()
  )
}

#' Validate a findings tibble
#' @param findings A data frame or tibble.
#' @noRd
validate_findings <- function(findings) {
  required <- names(empty_findings())
  if (!is.data.frame(findings)) {
    rlang::abort("`findings` must be a data frame.", call = NULL)
  }
  missing <- setdiff(required, names(findings))
  if (length(missing) > 0) {
    rlang::abort(
      paste0("`findings` missing columns: ", paste(missing, collapse = ", ")),
      call = NULL
    )
  }
  actions <- findings$recommended_action
  bad <- !is.na(actions) & !actions %in% recommended_actions()
  if (any(bad)) {
    rlang::abort(
      paste0(
        "`recommended_action` contains invalid values: ",
        paste(unique(actions[bad]), collapse = ", ")
      ),
      call = NULL
    )
  }
  tibble::as_tibble(findings[required])
}

#' Create a standardized quality-check result
#'
#' @param check_id Stable machine id.
#' @param label User-facing check name.
#' @param category Check category (e.g. `"coordinate"`, `"temporal"`).
#' @param status One of `"ok"`, `"skipped"`, `"error"`.
#' @param findings Tibble of per-record findings.
#' @param params_used Named list of parameters used for this run.
#' @param engine Backend label.
#' @param summary Optional named list of counts.
#' @param messages Character vector of notes or warnings.
#' @param timestamp POSIXct time of the result.
#' @param result_schema_version Integer schema version.
#' @export
new_occ_check_result <- function(check_id,
                                 label,
                                 category,
                                 status = "ok",
                                 findings = empty_findings(),
                                 params_used = list(),
                                 engine = "native",
                                 summary = NULL,
                                 messages = character(),
                                 timestamp = Sys.time(),
                                 result_schema_version = 1L) {
  if (!is.character(check_id) || length(check_id) != 1 || !nzchar(check_id)) {
    rlang::abort("`check_id` must be a non-empty string.", call = NULL)
  }
  if (!status %in% check_result_statuses()) {
    rlang::abort(
      paste0(
        "`status` must be one of: ",
        paste(check_result_statuses(), collapse = ", ")
      ),
      call = NULL
    )
  }

  findings <- validate_findings(findings)

  if (is.null(summary)) {
    summary <- list(
      n_checked = nrow(findings),
      n_flagged = sum(findings$flag, na.rm = TRUE),
      n_skipped_rows = 0L
    )
  }

  structure(
    list(
      check_id = check_id,
      label = label,
      category = category,
      status = status,
      findings = findings,
      params_used = params_used,
      engine = engine,
      summary = summary,
      messages = as.character(messages),
      timestamp = timestamp,
      result_schema_version = as.integer(result_schema_version)
    ),
    class = "occ_check_result"
  )
}

#' @export
print.occ_check_result <- function(x, ...) {
  n_flagged <- x$summary$n_flagged
  if (is.null(n_flagged)) {
    n_flagged <- sum(x$findings$flag, na.rm = TRUE)
  }
  n_checked <- x$summary$n_checked
  if (is.null(n_checked)) {
    n_checked <- "?"
  }
  cat(
    "<occ_check_result>\n",
    "  check_id: ", x$check_id, "\n",
    "  label:    ", x$label, "\n",
    "  category: ", x$category, "\n",
    "  status:   ", x$status, "\n",
    "  engine:   ", x$engine, "\n",
    "  flagged:  ", n_flagged, " / ", n_checked, "\n",
    sep = ""
  )
  invisible(x)
}

#' Test whether an object is an `occ_check_result`
#' @param x Object to test.
#' @export
is_occ_check_result <- function(x) {
  inherits(x, "occ_check_result")
}
