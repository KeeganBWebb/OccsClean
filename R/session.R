#' OccsClean analysis session
#'
#' Holds uploaded data, validation, cached checks, and decisions for one run.
#'
#' @export
OccSession <- R6::R6Class(
  "OccSession",
  public = list(
    #' @description Create an empty session.
    initialize = function() {
      private$occ_raw_ <- NULL
      private$occ_working_ <- NULL
      private$column_map_ <- list()
      private$column_map_override_ <- NULL
      private$column_map_skipped_ <- character()
      private$validation_ <- NULL
      private$assessment_ <- list()
      private$decisions_ <- DecisionRegistry$new()
      private$run_log_ <- list()
      private$meta_ <- list()
      private$study_area_ <- NULL
      private$revision_ <- 0L
      invisible(self)
    },

    #' @description Import CSV/TSV via [import_occurrences_csv()].
    #' @param path Path to a delimited text file.
    #' @param source_name Optional display name.
    import_csv = function(path, source_name = NULL) {
      imported <- import_occurrences_csv(
        path = path,
        source_name = source_name
      )
      private$occ_raw_ <- imported$occ_raw
      private$occ_working_ <- copy_tibble(imported$occ_raw)
      private$column_map_ <- list()
      private$column_map_override_ <- NULL
      private$column_map_skipped_ <- character()
      private$validation_ <- validate_occurrence_dataset(private$occ_working_)
      private$column_map_ <- private$validation_$column_map
      private$assessment_ <- list()
      private$decisions_$clear()
      private$run_log_ <- list()
      private$study_area_ <- NULL
      private$meta_ <- imported$meta
      private$bump()
      invisible(self)
    },

    #' @description Whether occurrence data have been imported.
    has_data = function() {
      !is.null(private$occ_raw_)
    },

    #' @description Original uploaded data.
    get_occ_raw = function() {
      copy_tibble(private$occ_raw_)
    },

    #' @description Working copy of occurrence data.
    get_occ_working = function() {
      copy_tibble(private$occ_working_)
    },

    #' @description Import metadata.
    get_meta = function() {
      private$meta_
    },

    #' @description Latest validation result.
    get_validation = function() {
      private$validation_
    },

    #' @description Re-run validation on the working copy.
    run_validation = function() {
      if (is.null(private$occ_working_)) {
        rlang::abort("No occurrence data loaded.", call = NULL)
      }
      private$validation_ <- validate_occurrence_dataset(
        private$occ_working_,
        column_map = private$column_map_override_,
        skipped_fields = private$column_map_skipped_,
        manually_mapped = !is.null(private$column_map_override_)
      )
      private$column_map_ <- private$validation_$column_map
      private$column_map_skipped_ <- private$validation_$skipped_fields %||% character()
      private$bump()
      invisible(self)
    },

    #' @description Apply manual column mapping and re-run validation.
    #' @param map Named list of column choices.
    apply_column_mapping = function(map) {
      if (is.null(private$occ_working_)) {
        rlang::abort("No occurrence data loaded.", call = NULL)
      }
      normalized <- normalize_column_map_input(
        map,
        names(private$occ_working_)
      )
      private$column_map_override_ <- normalized$map
      private$column_map_skipped_ <- normalized$skipped
      private$validation_ <- validate_occurrence_dataset(
        private$occ_working_,
        column_map = normalized$map,
        skipped_fields = normalized$skipped,
        manually_mapped = TRUE
      )
      private$column_map_ <- private$validation_$column_map
      private$column_map_skipped_ <- private$validation_$skipped_fields %||% character()
      private$bump()
      invisible(self)
    },

    #' @description Column map from validation.
    get_column_map = function() {
      private$column_map_
    },

    #' @description Skipped column map keys from manual mapping.
    get_skipped_fields = function() {
      private$column_map_skipped_
    },

    #' @description Decision registry.
    get_decisions = function() {
      private$decisions_
    },

    #' @description Cached check results by `check_id`.
    get_assessment = function() {
      private$assessment_
    },

    #' @description Store one check result.
    #' @param result An [occ_check_result].
    set_check_result = function(result) {
      if (!is_occ_check_result(result)) {
        rlang::abort("`result` must be an occ_check_result.", call = NULL)
      }
      private$assessment_[[result$check_id]] <- result
      private$run_log_[[length(private$run_log_) + 1L]] <- list(
        check_id = result$check_id,
        status = result$status,
        timestamp = result$timestamp,
        engine = result$engine
      )
      private$bump()
      invisible(self)
    },

    #' @description Store multiple check results.
    #' @param results List of [occ_check_result] objects.
    set_check_results = function(results) {
      if (!is.list(results)) {
        rlang::abort("`results` must be a list.", call = NULL)
      }
      for (result in results) {
        if (!is_occ_check_result(result)) {
          rlang::abort("Each element of `results` must be an occ_check_result.", call = NULL)
        }
        private$assessment_[[result$check_id]] <- result
        private$run_log_[[length(private$run_log_) + 1L]] <- list(
          check_id = result$check_id,
          status = result$status,
          timestamp = result$timestamp,
          engine = result$engine
        )
      }
      private$bump()
      invisible(self)
    },

    #' @description Run quality checks and cache results.
    #' @param check_ids Optional character vector of check ids. Default: all.
    #' @param params Optional params forwarded to [run_quality_checks()].
    #' @param progress Optional progress callback `function(i, n, check_id)`.
    run_checks = function(check_ids = NULL, params = list(), progress = NULL) {
      if (is.null(private$occ_working_)) {
        rlang::abort("No occurrence data loaded.", call = NULL)
      }
      col_params <- check_params_from_column_map(private$column_map_)
      params <- merge_check_column_params(params, col_params)
      results <- run_quality_checks(
        occ = private$occ_working_,
        check_ids = check_ids,
        params = params,
        progress = progress
      )
      self$set_check_results(results)
      invisible(self)
    },

    #' @description Findings table for Review.
    get_findings_table = function() {
      assessment_findings_table(
        assessment = private$assessment_,
        occ = private$occ_working_,
        column_map = private$column_map_
      )
    },

    #' @description Clear cached assessment.
    clear_assessment = function() {
      private$assessment_ <- list()
      private$run_log_ <- list()
      private$bump()
      invisible(self)
    },

    #' @description Store study-area polygon.
    #' @param geom An `sfc` polygon geometry (WGS84).
    #' @param source Optional source filename(s).
    set_study_area = function(geom, source = NULL) {
      if (is.null(geom)) {
        private$study_area_ <- NULL
      } else {
        private$study_area_ <- list(
          geom = geom,
          source = if (is.null(source)) NA_character_ else as.character(source)
        )
      }
      private$bump()
      invisible(self)
    },

    #' @description Stored study-area polygon, if any.
    get_study_area = function() {
      private$study_area_
    },

    #' @description Clear study-area geometry.
    clear_study_area = function() {
      private$study_area_ <- NULL
      private$bump()
      invisible(self)
    },

    #' @description Check run log.
    get_run_log = function() {
      private$run_log_
    },

    #' @description Session revision counter.
    get_revision = function() {
      private$revision_
    },

    #' @description Bump revision after external updates.
    touch = function() {
      private$bump()
      invisible(self)
    }
  ),
  private = list(
    occ_raw_ = NULL,
    occ_working_ = NULL,
    column_map_ = NULL,
    column_map_override_ = NULL,
    column_map_skipped_ = NULL,
    validation_ = NULL,
    assessment_ = NULL,
    decisions_ = NULL,
    run_log_ = NULL,
    study_area_ = NULL,
    meta_ = NULL,
    revision_ = 0L,
    bump = function() {
      private$revision_ <- private$revision_ + 1L
    }
  )
)

#' Sync app `rev` with session revision.
#' @param app_state Shiny reactiveValues with `session` and `rev`.
#' @noRd
bump_app_state <- function(app_state) {
  app_state$rev <- app_state$session$get_revision()
}
