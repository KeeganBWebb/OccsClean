#' Synthetic check id for Mapping-tab record decisions
#' @noRd
map_review_check_id <- function() {
  "map_review"
}

#' Synthetic finding code for Mapping-tab record decisions
#' @noRd
map_review_finding <- function() {
  "MAP_DECISION"
}

#' Mark one occurrence as failed from Mapping
#' @param decisions A [DecisionRegistry].
#' @param occsclean_id Occurrence id.
#' @param findings Optional findings tibble.
#' @noRd
mark_record_for_deletion <- function(decisions, occsclean_id, findings = NULL) {
  fail_records(decisions, occsclean_id, findings = findings)
}

#' Pass one occurrence from Mapping
#' @param decisions A [DecisionRegistry].
#' @param occsclean_id Occurrence id.
#' @param findings Optional findings tibble.
#' @noRd
keep_record_from_mapping <- function(decisions, occsclean_id, findings = NULL) {
  pass_records(decisions, occsclean_id, findings = findings)
}

#' Parse Mapping marker layerId back to occsclean_id
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
