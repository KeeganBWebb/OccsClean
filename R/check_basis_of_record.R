#' Unique non-blank basisOfRecord values in a dataset
#' @param occ Occurrence data frame.
#' @param basis_col Optional column name.
#' @noRd
basis_of_record_values_in_data <- function(occ, basis_col = NULL) {
  if (is.null(occ) || !is.data.frame(occ) || nrow(occ) < 1) {
    return(character())
  }
  if (is.null(basis_col)) {
    basis_col <- resolve_occurrence_columns(occ)$basis_of_record
  }
  if (is.null(basis_col) || !basis_col %in% names(occ)) {
    return(character())
  }

  raw <- trimws(as.character(occ[[basis_col]]))
  raw <- raw[!is.na(raw) & nzchar(raw)]
  if (length(raw) < 1) {
    return(character())
  }
  sort(unique(raw))
}

#' Flag basisOfRecord values outside the allowed set
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_basis_of_record <- function(occ, params = list()) {
  check_id <- "occ_basis_of_record"
  label <- "Basis Of Record"
  category <- "occurrence"

  cols <- resolve_occurrence_columns(occ)
  basis_col <- params$basis_col %||% cols$basis_of_record
  if (is.null(basis_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find a basisOfRecord column.",
      params_used = params
    ))
  }

  allowed_raw <- params$allowed_basis %||% character()
  allowed_raw <- as.character(allowed_raw)
  allowed_raw <- trimws(allowed_raw)
  allowed_raw <- allowed_raw[!is.na(allowed_raw) & nzchar(allowed_raw)]
  if (length(allowed_raw) < 1) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Select at least one allowed basisOfRecord value from the file.",
      params_used = utils::modifyList(params, list(basis_col = basis_col))
    ))
  }

  raw <- occ[[basis_col]]
  blank <- is_blank_coord(raw)
  present <- trimws(as.character(raw))
  disallowed <- !blank & !(present %in% allowed_raw)
  flag <- blank | disallowed
  idx <- which(flag)

  reason <- rep(NA_character_, length(idx))
  if (length(idx) > 0) {
    reason[blank[idx]] <- "basisOfRecord is missing."
    reason[disallowed[idx]] <- paste0(
      "basisOfRecord is outside the selected allowed values (",
      paste(allowed_raw, collapse = ", "),
      ")."
    )
  }

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = ifelse(blank[idx], "BASIS_MISSING", "BASIS_DISALLOWED"),
    reason = reason,
    evidence = paste0(basis_col, "=", format_evidence_value(raw[idx])),
    recommended_action = "remove",
    severity = ifelse(blank[idx], "medium", "low")
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = utils::modifyList(
      params,
      list(basis_col = basis_col, allowed_basis = allowed_raw)
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}
