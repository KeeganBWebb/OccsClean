#' Flag duplicate occurrence records
#'
#' Matches on all columns except `occsclean_id`.
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param params Optional check parameters.
#' @export
check_duplicates <- function(occ, params = list()) {
  check_id <- "occ_duplicates"
  label <- "Duplicate records"
  category <- "occurrence"

  if (!"occsclean_id" %in% names(occ)) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "Missing required column: occsclean_id",
      params_used = params
    ))
  }

  key_cols <- setdiff(names(occ), "occsclean_id")
  if (length(key_cols) < 1) {
    return(skipped_check_result(
      check_id, label, category,
      messages = "No columns available to compare for duplicates.",
      params_used = params
    ))
  }

  key <- occ[key_cols]
  is_dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  n_checked <- nrow(occ)
  idx <- which(is_dup)

  findings <- findings_from_flags(
    occsclean_id = occ$occsclean_id[idx],
    finding = "DUPLICATE_EXACT",
    reason = "Record matches another row on all fields except OccsClean's internal id.",
    evidence = paste0("duplicate_group_size_marker=exact;", paste(key_cols, collapse = ",")),
    recommended_action = "remove",
    severity = "medium"
  )

  new_occ_check_result(
    check_id = check_id,
    label = label,
    category = category,
    status = "ok",
    findings = findings,
    params_used = params,
    engine = "native",
    summary = list(
      n_checked = n_checked,
      n_flagged = length(idx),
      n_skipped_rows = 0L
    )
  )
}
