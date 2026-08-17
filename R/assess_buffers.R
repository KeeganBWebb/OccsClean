#' Default CoordinateCleaner buffer distances (meters)
#' @noRd
coordinatecleaner_buffer_defaults_m <- function() {
  list(
    coord_capital = 10000L,
    coord_centroid = 1000L,
    coord_institution = 100L,
    coord_gbif = 1000L
  )
}

#' Parse a buffer-distance text input (meters)
#' @param x Raw input.
#' @param default_m Default meters when blank.
#' @param label Field label for error messages.
#' @noRd
parse_buffer_distance_m <- function(x, default_m, label = "Buffer") {
  default_m <- as.integer(default_m)[1]
  if (is.na(default_m) || default_m < 0L) {
    rlang::abort("Internal default buffer is invalid.", call = NULL)
  }
  txt <- trimws(as.character(x %||% ""))
  if (!nzchar(txt)) {
    return(default_m)
  }
  val <- suppressWarnings(as.numeric(txt))
  if (length(val) != 1 || is.na(val) || !is.finite(val) || val < 0) {
    rlang::abort(
      paste0(label, " must be a non-negative number of meters."),
      call = NULL
    )
  }
  as.integer(round(val))
}

#' Optional country-buffer parse (blank = no buffer / package default NULL)
#' @noRd
parse_optional_buffer_distance_m <- function(x, label = "Buffer") {
  txt <- trimws(as.character(x %||% ""))
  if (!nzchar(txt)) {
    return(NULL)
  }
  val <- suppressWarnings(as.numeric(txt))
  if (length(val) != 1 || is.na(val) || !is.finite(val) || val < 0) {
    rlang::abort(
      paste0(label, " must be a non-negative number of meters, or blank."),
      call = NULL
    )
  }
  as.integer(round(val))
}
