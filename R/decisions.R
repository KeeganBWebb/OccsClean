#' Decision registry for user actions on quality findings
#'
#' Stores pass, fail, and correct decisions (`keep`, `remove`, `correct`).
#'
#' @export
DecisionRegistry <- R6::R6Class(
  "DecisionRegistry",
  public = list(
    #' @description Create an empty registry.
    initialize = function() {
      private$entries_ <- private$empty_entries()
      private$counter_ <- 0L
      invisible(self)
    },

    #' @description Record a user decision.
    #' @param occsclean_id Occurrence record id.
    #' @param check_id Check id.
    #' @param action One of `keep`, `remove`, or `correct`.
    record = function(occsclean_id,
                      check_id,
                      action,
                      finding = NA_character_,
                      note = NA_character_,
                      fields_affected = character(),
                      old_values = list(),
                      new_values = list(),
                      assessment_run_id = NA_character_,
                      decided_by = "user") {
      if (!action %in% decision_actions()) {
        rlang::abort(
          paste0(
            "`action` must be one of: ",
            paste(decision_actions(), collapse = ", ")
          ),
          call = NULL
        )
      }
      if (identical(action, "correct")) {
        if (length(fields_affected) < 1) {
          rlang::abort(
            "`correct` decisions require `fields_affected`.",
            call = NULL
          )
        }
      }

      key_finding <- if (is.null(finding) || length(finding) == 0) {
        NA_character_
      } else {
        as.character(finding)[[1]]
      }

      current <- self$effective()
      same <- current$occsclean_id == occsclean_id &
        current$check_id == check_id &
        (
          (is.na(current$finding) & is.na(key_finding)) |
            (!is.na(current$finding) & !is.na(key_finding) &
              current$finding == key_finding)
        )
      superseded_ids <- current$decision_id[same]

      private$counter_ <- private$counter_ + 1L
      decision_id <- sprintf("dec_%06d", private$counter_)

      new_row <- tibble::tibble(
        decision_id = decision_id,
        occsclean_id = as.character(occsclean_id),
        check_id = as.character(check_id),
        finding = key_finding,
        action = as.character(action),
        fields_affected = list(as.character(fields_affected)),
        old_values = list(old_values),
        new_values = list(new_values),
        note = if (is.null(note)) NA_character_ else as.character(note),
        decided_at = Sys.time(),
        decided_by = as.character(decided_by),
        assessment_run_id = if (is.null(assessment_run_id)) {
          NA_character_
        } else {
          as.character(assessment_run_id)
        },
        superseded_by = NA_character_
      )

      private$entries_ <- dplyr::bind_rows(private$entries_, new_row)

      if (length(superseded_ids) > 0) {
        idx <- private$entries_$decision_id %in% superseded_ids
        private$entries_$superseded_by[idx] <- decision_id
      }

      invisible(decision_id)
    },

    #' @description Record the same action for many findings.
    #' @param findings_df Data frame with `occsclean_id`, `check_id`, and `finding`.
    #' @param action Decision action.
    record_many = function(findings_df, action) {
      if (nrow(findings_df) < 1) {
        return(0L)
      }
      if (!action %in% decision_actions()) {
        rlang::abort(
          paste0(
            "`action` must be one of: ",
            paste(decision_actions(), collapse = ", ")
          ),
          call = NULL
        )
      }
      if (identical(action, "correct")) {
        rlang::abort(
          "`record_many` does not support `correct` actions.",
          call = NULL
        )
      }
      if (!all(c("occsclean_id", "check_id") %in% names(findings_df))) {
        rlang::abort(
          "`findings_df` must include occsclean_id and check_id columns.",
          call = NULL
        )
      }

      n <- nrow(findings_df)
      record_ids <- as.character(findings_df$occsclean_id)
      check_ids <- as.character(findings_df$check_id)
      findings <- if ("finding" %in% names(findings_df)) {
        as.character(findings_df$finding)
      } else {
        rep(NA_character_, n)
      }
      findings[is.na(findings) | !nzchar(findings)] <- NA_character_

      current <- self$effective()
      superseded_map <- rep(NA_character_, n)

      if (nrow(current) > 0) {
        cur_key <- paste(
          as.character(current$occsclean_id),
          as.character(current$check_id),
          ifelse(
            is.na(current$finding),
            "",
            as.character(current$finding)
          ),
          sep = "\r"
        )
        new_key <- paste(
          record_ids,
          check_ids,
          ifelse(is.na(findings), "", findings),
          sep = "\r"
        )
        hit <- match(new_key, cur_key)
        superseded_map <- ifelse(
          is.na(hit),
          NA_character_,
          as.character(current$decision_id[hit])
        )
      }

      ids <- private$counter_ + seq_len(n)
      private$counter_ <- private$counter_ + n
      decision_ids <- sprintf("dec_%06d", ids)
      now <- Sys.time()

      new_rows <- tibble::tibble(
        decision_id = decision_ids,
        occsclean_id = record_ids,
        check_id = check_ids,
        finding = findings,
        action = rep(as.character(action), n),
        fields_affected = replicate(n, character(), simplify = FALSE),
        old_values = replicate(n, list(), simplify = FALSE),
        new_values = replicate(n, list(), simplify = FALSE),
        note = rep(NA_character_, n),
        decided_at = rep(now, n),
        decided_by = rep("user", n),
        assessment_run_id = rep(NA_character_, n),
        superseded_by = rep(NA_character_, n)
      )

      private$entries_ <- dplyr::bind_rows(private$entries_, new_rows)

      hit_old <- which(!is.na(superseded_map) & nzchar(superseded_map))
      if (length(hit_old) > 0) {
        old_ids <- superseded_map[hit_old]
        new_ids <- decision_ids[hit_old]
        idx <- match(old_ids, private$entries_$decision_id)
        ok <- !is.na(idx)
        if (any(ok)) {
          private$entries_$superseded_by[idx[ok]] <- new_ids[ok]
        }
      }

      as.integer(n)
    },

    #' @description Occurrence ids with an effective remove decision.
    removed_occsclean_ids = function() {
      eff <- self$effective()
      if (nrow(eff) < 1) {
        return(character())
      }
      unique(eff$occsclean_id[eff$action == "remove"])
    },

    #' @description Full decision log.
    log = function() {
      tibble::as_tibble(private$entries_)
    },

    #' @description Current effective decisions.
    effective = function() {
      keep <- is.na(private$entries_$superseded_by)
      tibble::as_tibble(private$entries_[keep, , drop = FALSE])
    },

    #' @description Number of log entries.
    n_entries = function() {
      nrow(private$entries_)
    },

    #' @description Clear all decisions.
    clear = function() {
      private$entries_ <- private$empty_entries()
      private$counter_ <- 0L
      invisible(self)
    }
  ),
  private = list(
    entries_ = NULL,
    counter_ = 0L,
    empty_entries = function() {
      tibble::tibble(
        decision_id = character(),
        occsclean_id = character(),
        check_id = character(),
        finding = character(),
        action = character(),
        fields_affected = list(),
        old_values = list(),
        new_values = list(),
        note = character(),
        decided_at = as.POSIXct(character()),
        decided_by = character(),
        assessment_run_id = character(),
        superseded_by = character()
      )
    }
  )
)
