#' Synthetic check id for Review map record decisions
#' @noRd
map_review_check_id <- function() {
  "map_review"
}

#' Synthetic finding code for Review map record decisions
#' @noRd
map_review_finding <- function() {
  "MAP_DECISION"
}

#' Synthetic finding code for batch fail actions in Review
#' @noRd
map_review_batch_finding <- function() {
  "MAP_BATCH_FAIL"
}

#' Synthetic finding code for batch pass actions in Review
#' @noRd
map_review_batch_pass_finding <- function() {
  "MAP_BATCH_PASS"
}

#' Synthetic check id for user-initiated manual review flags
#' @noRd
manual_review_check_id <- function() {
  "manual_review"
}

#' Synthetic finding code for user-initiated manual review flags
#' @noRd
manual_review_finding <- function() {
  "MANUAL"
}

#' Display label for user-initiated manual review flags
#' @noRd
manual_review_check_label <- function() {
  "MANUAL"
}

#' Reason text for manual review finding rows
#' @noRd
manual_review_reason <- function() {
  "Flagged manually for review"
}

#' Stub rows for manual review flags
#' @noRd
manual_review_decision_rows <- function(record_ids) {
  ids <- unique(as.character(record_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) < 1) {
    return(tibble::tibble(
      occsclean_id = character(),
      check_id = character(),
      finding = character()
    ))
  }
  tibble::tibble(
    occsclean_id = ids,
    check_id = rep(manual_review_check_id(), length(ids)),
    finding = rep(manual_review_finding(), length(ids))
  )
}

#' Occurrence ids with an active manual review flag
#' @noRd
manual_review_occsclean_ids <- function(decisions, actions = c("unreviewed", "keep", "remove")) {
  if (is.null(decisions) || !inherits(decisions, "DecisionRegistry")) {
    return(character())
  }
  eff <- decisions$effective()
  if (nrow(eff) < 1) {
    return(character())
  }
  hit <- eff[
    as.character(eff$check_id) == manual_review_check_id() &
      as.character(eff$finding) == manual_review_finding() &
      as.character(eff$action) %in% actions,
    ,
    drop = FALSE
  ]
  unique(as.character(hit$occsclean_id))
}

#' Occurrence ids manually flagged and still in review
#' @noRd
manually_flagged_in_review_occsclean_ids <- function(decisions) {
  manual_review_occsclean_ids(decisions, actions = "unreviewed")
}

#' Occurrence ids that should show the MANUAL flag label
#' @noRd
manual_review_display_occsclean_ids <- function(decisions) {
  manual_review_occsclean_ids(decisions, actions = c("unreviewed", "remove"))
}

#' Occurrence ids failed via a batch Review action
#' @noRd
batch_failed_occsclean_ids <- function(decisions) {
  if (is.null(decisions) || !inherits(decisions, "DecisionRegistry")) {
    return(character())
  }
  eff <- decisions$effective()
  if (nrow(eff) < 1) {
    return(character())
  }
  hit <- eff[
    as.character(eff$check_id) == map_review_check_id() &
      as.character(eff$action) == "remove" &
      as.character(eff$finding) == map_review_batch_finding(),
    ,
    drop = FALSE
  ]
  unique(as.character(hit$occsclean_id))
}

#' Occurrence ids passed via a batch Review action
#' @noRd
batch_passed_occsclean_ids <- function(decisions) {
  if (is.null(decisions) || !inherits(decisions, "DecisionRegistry")) {
    return(character())
  }
  eff <- decisions$effective()
  if (nrow(eff) < 1) {
    return(character())
  }
  hit <- eff[
    as.character(eff$check_id) == map_review_check_id() &
      as.character(eff$action) == "keep" &
      as.character(eff$finding) == map_review_batch_pass_finding(),
    ,
    drop = FALSE
  ]
  unique(as.character(hit$occsclean_id))
}

#' Mark one occurrence as failed from the Review map
#' @param decisions A [DecisionRegistry].
#' @param occsclean_id Occurrence id.
#' @param findings Optional findings tibble.
#' @noRd
mark_record_for_deletion <- function(decisions, occsclean_id, findings = NULL) {
  fail_records(decisions, occsclean_id, findings = findings)
}

#' Pass one occurrence from the Review map
#' @param decisions A [DecisionRegistry].
#' @param occsclean_id Occurrence id.
#' @param findings Optional findings tibble.
#' @noRd
keep_record_from_mapping <- function(decisions, occsclean_id, findings = NULL) {
  pass_records(decisions, occsclean_id, findings = findings)
}

#' Parse Review map marker layerId back to occsclean_id
#' @noRd
mapping_layer_id_record <- function(layer_id) {
  id <- as.character(layer_id %||% "")[[1]]
  if (!nzchar(id)) {
    return(NA_character_)
  }
  # layerId format: "<occsclean_id>||<row>"
  if (grepl("\\|\\|", id, perl = TRUE)) {
    return(sub("\\|\\|.*$", "", id, perl = TRUE))
  }
  id
}
