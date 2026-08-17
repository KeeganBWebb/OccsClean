#' Unique non-blank scientific names in a dataset
#' @param occ Occurrence data frame.
#' @param taxon_col Optional column name.
#' @noRd
scientific_name_values_in_data <- function(occ, taxon_col = NULL) {
  if (is.null(occ) || !is.data.frame(occ) || nrow(occ) < 1) {
    return(character())
  }
  if (is.null(taxon_col)) {
    taxon_col <- resolve_occurrence_columns(occ)$taxon
  }
  if (is.null(taxon_col) || !taxon_col %in% names(occ)) {
    return(character())
  }

  raw <- trimws(as.character(occ[[taxon_col]]))
  raw <- raw[!is.na(raw) & nzchar(raw)]
  if (length(raw) < 1) {
    return(character())
  }
  sort(unique(raw))
}

#' Flag scientific names outside the allowed set
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_allowed_species <- function(occ, params = list()) {
  check_id <- "taxon_allowed_species"
  label <- "Allowed species"
  category <- "taxonomic"

  cols <- resolve_occurrence_columns(occ)
  taxon_col <- params$taxon_col %||% cols$taxon
  if (is.null(taxon_col)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Could not find a scientific name column.",
      params_used = params
    ))
  }

  allowed_raw <- params$allowed_species %||% character()
  allowed_raw <- as.character(allowed_raw)
  allowed_raw <- trimws(allowed_raw)
  allowed_raw <- allowed_raw[!is.na(allowed_raw) & nzchar(allowed_raw)]
  if (length(allowed_raw) < 1) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Select at least one allowed scientific name from the file.",
      params_used = utils::modifyList(params, list(taxon_col = taxon_col))
    ))
  }

  raw <- occ[[taxon_col]]
  blank <- is_blank_coord(raw)
  present <- trimws(as.character(raw))
  disallowed <- !blank & !(present %in% allowed_raw)
  flag <- blank | disallowed
  idx <- which(flag)

  reason <- rep(NA_character_, length(idx))
  if (length(idx) > 0) {
    reason[blank[idx]] <- "Scientific name is missing."
    reason[disallowed[idx]] <- paste0(
      "Scientific name is outside the selected allowed species (",
      paste(allowed_raw, collapse = ", "),
      ")."
    )
  }

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = ifelse(blank[idx], "TAXON_MISSING", "TAXON_DISALLOWED"),
    reason = reason,
    evidence = paste0(taxon_col, "=", format_evidence_value(raw[idx])),
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
      list(taxon_col = taxon_col, allowed_species = allowed_raw)
    ),
    engine = "native",
    summary = list(
      n_checked = nrow(occ),
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}
