#' Build an Assess settings preset
#'
#' @param selected_checks Character vector of check ids.
#' @param settings Optional filter settings list.
#' @export
build_assess_settings <- function(selected_checks = character(),
                                  settings = list()) {
  known <- list_quality_checks()$check_id
  selected_checks <- as.character(selected_checks %||% character())
  selected_checks <- unique(trimws(selected_checks))
  selected_checks <- selected_checks[nzchar(selected_checks)]
  selected_checks <- intersect(selected_checks, known)

  settings <- sanitize_assess_settings_payload(settings)
  if (length(settings) < 1) {
    settings <- structure(list(), names = character())
  }

  ver <- tryCatch(
    as.character(utils::packageVersion("OccsClean")),
    error = function(e) "development"
  )

  list(
    schema = "occsclean_assess_settings",
    schema_version = 1L,
    created_at = format(Sys.time(), usetz = TRUE),
    occsclean_version = ver,
    selected_checks = as.list(selected_checks),
    settings = settings
  )
}

#' Write Assess settings to a JSON file
#'
#' @param path Output path.
#' @param selected_checks Character vector of check ids.
#' @param settings Optional filter settings list.
#' @param x Optional pre-built list from [build_assess_settings()].
#' @export
write_assess_settings <- function(path,
                                  selected_checks = character(),
                                  settings = list(),
                                  x = NULL) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    rlang::abort("`path` must be a non-empty string.", call = NULL)
  }
  if (is.null(x)) {
    x <- build_assess_settings(selected_checks, settings)
  } else {
    x <- validate_assess_settings_object(x, drop_unknown = TRUE)$settings
  }
  jsonlite::write_json(
    x,
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  invisible(path)
}

#' Read Assess settings from a JSON file
#'
#' @param path Path to a JSON settings file.
#' @export
read_assess_settings <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    rlang::abort("`path` must be a non-empty string.", call = NULL)
  }
  if (!file.exists(path)) {
    rlang::abort(paste0("File not found: ", path), call = NULL)
  }

  raw <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      rlang::abort(
        paste0("Could not parse Assess settings JSON: ", conditionMessage(e)),
        call = NULL
      )
    }
  )

  validate_assess_settings_object(raw, drop_unknown = TRUE)
}

#' Validate and normalize an Assess settings list
#' @param x Parsed JSON or list.
#' @param drop_unknown If `TRUE`, drop unknown check ids.
#' @noRd
validate_assess_settings_object <- function(x, drop_unknown = TRUE) {
  warnings <- character()
  if (!is.list(x)) {
    rlang::abort("Assess settings must be a JSON object.", call = NULL)
  }

  schema <- x$schema %||% NA_character_
  if (!identical(as.character(schema), "occsclean_assess_settings")) {
    rlang::abort(
      'Assess settings file must have schema \"occsclean_assess_settings\".',
      call = NULL
    )
  }

  ver <- suppressWarnings(as.integer(x$schema_version %||% NA_integer_))
  if (!isTRUE(identical(ver, 1L))) {
    rlang::abort(
      paste0(
        "Unsupported Assess settings schema_version: ",
        x$schema_version %||% "missing",
        " (supported: 1)."
      ),
      call = NULL
    )
  }

  known <- list_quality_checks()$check_id
  selected <- unlist_character(x$selected_checks)
  unknown <- setdiff(selected, known)
  if (length(unknown) > 0) {
    msg <- paste0(
      "Dropped unknown check id(s): ",
      paste(unknown, collapse = ", "),
      "."
    )
    if (isTRUE(drop_unknown)) {
      warnings <- c(warnings, msg)
      selected <- intersect(selected, known)
    } else {
      rlang::abort(msg, call = NULL)
    }
  }

  settings <- sanitize_assess_settings_payload(x$settings %||% list())

  out <- list(
    schema = "occsclean_assess_settings",
    schema_version = 1L,
    created_at = as.character(x$created_at %||% NA_character_),
    occsclean_version = as.character(x$occsclean_version %||% NA_character_),
    selected_checks = as.list(selected),
    settings = settings
  )

  list(settings = out, warnings = warnings)
}

#' Keep only serializable, known Assess filter keys
#' @noRd
sanitize_assess_settings_payload <- function(settings) {
  if (is.null(settings)) {
    return(structure(list(), names = character()))
  }
  if (!is.list(settings) || (length(settings) < 1 && is.null(names(settings)))) {
    return(structure(list(), names = character()))
  }
  settings$area_geom <- NULL
  settings$area_sf <- NULL
  settings$repair_message <- NULL

  keep <- c(
    "allowed_basis",
    "allowed_species",
    "date_min",
    "date_max",
    "outside_distance_m",
    "buffer_capital_m",
    "buffer_centroid_m",
    "buffer_institution_m",
    "buffer_gbif_m",
    "buffer_country_m",
    "area_source"
  )
  out <- list()
  for (nm in keep) {
    if (!nm %in% names(settings)) {
      next
    }
    val <- settings[[nm]]
    if (is.null(val)) {
      next
    }
    if (inherits(val, c("sf", "sfc", "sfg"))) {
      next
    }
    if (nm %in% c("allowed_basis", "allowed_species")) {
      val <- unlist_character(val)
      val <- val[nzchar(val)]
      if (length(val) < 1) {
        next
      }
      out[[nm]] <- as.list(val)
    } else if (nm %in% c(
      "outside_distance_m",
      "buffer_capital_m",
      "buffer_centroid_m",
      "buffer_institution_m",
      "buffer_gbif_m",
      "buffer_country_m"
    )) {
      if (is.list(val) && length(val) == 1) {
        val <- val[[1]]
      }
      txt <- trimws(as.character(val))
      if (!nzchar(txt) || identical(txt, "NA")) {
        next
      }
      out[[nm]] <- txt
    } else {
      if (is.list(val) && length(val) == 1) {
        val <- val[[1]]
      }
      txt <- trimws(as.character(val))
      if (!nzchar(txt) || identical(txt, "NA")) {
        next
      }
      out[[nm]] <- txt
    }
  }
  if (length(out) < 1) {
    return(structure(list(), names = character()))
  }
  out
}

#' Flatten JSON list/vector to character
#' @noRd
unlist_character <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  as.character(unlist(x, use.names = FALSE))
}

#' Split selected check ids by Assess UI checkbox group
#' @noRd
assess_checks_by_ui_group <- function(selected_checks) {
  selected_checks <- as.character(selected_checks %||% character())
  catalog <- list_quality_checks()
  groups <- c("basics", "dates", "suspicious_locations", "taxonomic")
  out <- stats::setNames(vector("list", length(groups)), groups)
  for (g in groups) {
    ids <- catalog$check_id[catalog$ui_group == g]
    out[[g]] <- intersect(selected_checks, ids)
  }
  out
}

#' Collect filter settings from Assess UI input values
#' @noRd
collect_assess_settings_from_inputs <- function(input) {
  settings <- list()
  basis <- input$basis_allowed %||% character()
  basis <- as.character(basis)
  basis <- basis[nzchar(basis)]
  if (length(basis) > 0) {
    settings$allowed_basis <- basis
  }
  species <- input$species_allowed %||% character()
  species <- as.character(species)
  species <- species[nzchar(species)]
  if (length(species) > 0) {
    settings$allowed_species <- species
  }
  date_min <- trimws(as.character(input$date_min %||% ""))
  if (nzchar(date_min)) {
    settings$date_min <- date_min
  }
  date_max <- trimws(as.character(input$date_max %||% ""))
  if (nzchar(date_max)) {
    settings$date_max <- date_max
  }
  dist <- trimws(as.character(input$area_outside_distance_m %||% ""))
  if (nzchar(dist)) {
    settings$outside_distance_m <- dist
  }
  for (nm in c(
    "buffer_capital_m",
    "buffer_centroid_m",
    "buffer_institution_m",
    "buffer_gbif_m",
    "buffer_country_m"
  )) {
    txt <- trimws(as.character(input[[nm]] %||% ""))
    if (nzchar(txt)) {
      settings[[nm]] <- txt
    }
  }
  settings
}
