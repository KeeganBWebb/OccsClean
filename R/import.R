#' Import occurrence records from a delimited text file
#'
#' Reads CSV or TSV into a tibble and adds `occsclean_id`.
#'
#' @param path Path to a delimited text file.
#' @param source_name Optional display name; defaults to `basename(path)`.
#' @return A list with `occ_raw` (tibble) and `meta` (import metadata).
#' @export
import_occurrences_csv <- function(path, source_name = NULL) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    rlang::abort("`path` must be a non-empty string.", call = NULL)
  }
  if (!file.exists(path)) {
    rlang::abort(paste0("File not found: ", path), call = NULL)
  }

  parsed <- read_occurrence_table(path)
  occ <- parsed$data
  delim_label <- parsed$delim_label

  if (nrow(occ) < 1) {
    rlang::abort("Imported file has zero rows.", call = NULL)
  }

  occ$occsclean_id <- generate_occsclean_ids(nrow(occ))
  occ <- dplyr::relocate(occ, "occsclean_id")

  display_name <- source_name
  if (is.null(display_name) || !nzchar(display_name)) {
    display_name <- basename(path)
  }

  meta <- list(
    read_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    source_name = display_name,
    n_rows = nrow(occ),
    n_cols = ncol(occ),
    imported_at = Sys.time(),
    occsclean_id_source = "generated",
    delimiter = delim_label,
    original_file_untouched = TRUE
  )

  list(occ_raw = occ, meta = meta)
}

#' Read occurrence table, detecting comma vs tab delimiters
#' @param path File path.
#' @noRd
read_occurrence_table <- function(path) {
  header <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (length(header) < 1 || !nzchar(header[[1]])) {
    header <- ""
  }
  line <- header[[1]]
  n_tab <- nchar(gsub("[^\t]", "", line))
  n_comma <- nchar(gsub("[^,]", "", line))

  use_tsv <- n_tab > n_comma
  if (use_tsv) {
    occ <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
    delim_label <- "tab"
  } else {
    occ <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    delim_label <- "comma"
  }

  list(data = tibble::as_tibble(occ), delim_label = delim_label)
}

#' Generate OccsClean row identifiers
#' @param n Number of ids.
#' @noRd
generate_occsclean_ids <- function(n) {
  sprintf("oc_%06d", seq_len(n))
}

#' Drop OccsClean-only columns from user-facing tables
#' @param df A data frame or tibble.
#' @noRd
strip_occsclean_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }
  drop <- intersect(c("occsclean_id"), names(df))
  if (length(drop) < 1) {
    return(tibble::as_tibble(df))
  }
  tibble::as_tibble(df[setdiff(names(df), drop)])
}

#' Copy a tibble defensively
#' @param x A data frame or tibble, or `NULL`.
#' @noRd
copy_tibble <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  tibble::as_tibble(data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
}
