#' Empty visualize-points tibble
#' @noRd
empty_visualize_points <- function() {
  tibble::tibble(
    occsclean_id = character(),
    longitude = double(),
    latitude = double(),
    map_status = character(),
    n_flags = integer(),
    flag_labels = character(),
    taxon = character(),
    mappable = logical()
  )
}

#' Build per-record visualize index
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param findings Optional findings tibble.
#' @param decisions Optional [DecisionRegistry].
#' @param column_map Optional resolved column map.
#' @noRd
build_visualize_records <- function(occ,
                                    findings = NULL,
                                    decisions = NULL,
                                    column_map = NULL) {
  if (is.null(occ) || !is.data.frame(occ) || nrow(occ) < 1) {
    return(empty_visualize_points())
  }

  cols <- if (is.list(column_map) && length(column_map) > 0) {
    column_map
  } else {
    resolve_occurrence_columns(occ)
  }
  n <- nrow(occ)
  lon <- rep(NA_real_, n)
  lat <- rep(NA_real_, n)
  mappable <- rep(FALSE, n)

  if (!is.null(cols$lon) && !is.null(cols$lat)) {
    lon <- as_numeric_silent(occ[[cols$lon]])
    lat <- as_numeric_silent(occ[[cols$lat]])
    mappable <- is.finite(lon) & is.finite(lat) &
      lon >= -180 & lon <= 180 &
      lat >= -90 & lat <= 90
  }

  taxon <- rep(NA_character_, n)
  if (!is.null(cols$taxon) && cols$taxon %in% names(occ)) {
    taxon <- as.character(occ[[cols$taxon]])
  }

  rec <- tibble::tibble(
    occsclean_id = as.character(occ$occsclean_id),
    longitude = lon,
    latitude = lat,
    map_status = rep("OK", n),
    n_flags = rep(0L, n),
    flag_labels = rep("", n),
    taxon = taxon,
    mappable = mappable
  )

  if (!is.null(findings) && is.data.frame(findings) && nrow(findings) > 0 &&
        "occsclean_id" %in% names(findings)) {
    f <- findings
    if ("check_id" %in% names(f)) {
      f <- f[
        as.character(f$check_id) != map_review_check_id(),
        ,
        drop = FALSE
      ]
    }
    f$occsclean_id <- as.character(f$occsclean_id)
    if (nrow(f) > 0) {
      counts <- as.integer(table(f$occsclean_id))
      count_ids <- names(table(f$occsclean_id))
      rec$n_flags <- as.integer(counts[match(rec$occsclean_id, count_ids)])
      rec$n_flags[is.na(rec$n_flags)] <- 0L

      if ("check_label" %in% names(f)) {
        label_map <- tapply(
          as.character(f$check_label),
          f$occsclean_id,
          function(x) paste(unique(x), collapse = "; ")
        )
        rec$flag_labels <- as.character(label_map[rec$occsclean_id])
        rec$flag_labels[is.na(rec$flag_labels)] <- ""
      }

      rec$map_status[rec$n_flags > 0L] <- "Flagged"
    }
  }

  if (!is.null(decisions) && inherits(decisions, "DecisionRegistry")) {
    if (!is.null(findings) && is.data.frame(findings) && nrow(findings) > 0) {
      f <- findings
      if ("check_id" %in% names(f)) {
        f <- f[
          as.character(f$check_id) != map_review_check_id(),
          ,
          drop = FALSE
        ]
      }
      if (nrow(f) > 0) {
        joined <- findings_with_decisions(f, decisions)
        decs <- as.character(joined$decision)
        rid <- as.character(joined$occsclean_id)
        all_passed <- vapply(
          split(decs, rid),
          function(d) length(d) > 0L && all(d == "keep"),
          logical(1)
        )
        passed_ids <- names(all_passed)[all_passed]
        if (length(passed_ids) > 0) {
          rec$map_status[rec$occsclean_id %in% passed_ids] <- "Passed"
        }
      }
    }

    eff <- decisions$effective()
    if (nrow(eff) > 0) {
      map_pass <- unique(as.character(eff$occsclean_id[
        as.character(eff$action) == "keep" &
          as.character(eff$check_id) == map_review_check_id()
      ]))
      if (length(map_pass) > 0) {
        hit <- rec$occsclean_id %in% map_pass & rec$n_flags < 1L
        rec$map_status[hit] <- "Passed"
      }
    }

    removed <- decisions$removed_occsclean_ids()
    if (length(removed) > 0) {
      rec$map_status[rec$occsclean_id %in% as.character(removed)] <- "Failed"
    }

    rec <- apply_manual_review_map_status(rec, decisions)
  }

  rec
}

#' Apply manual review flag status to visualize records
#' @noRd
apply_manual_review_map_status <- function(rec, decisions) {
  if (is.null(rec) || nrow(rec) < 1) {
    return(rec)
  }
  eff <- decisions$effective()
  if (nrow(eff) < 1) {
    return(rec)
  }
  manual_eff <- eff[
    as.character(eff$check_id) == manual_review_check_id() &
      as.character(eff$finding) == manual_review_finding(),
    ,
    drop = FALSE
  ]
  if (nrow(manual_eff) < 1) {
    return(rec)
  }
  label <- manual_review_check_label()
  for (id in unique(as.character(manual_eff$occsclean_id))) {
    hit <- manual_eff[
      as.character(manual_eff$occsclean_id) == id,
      ,
      drop = FALSE
    ]
    action <- as.character(hit$action[[1]])
    idx <- rec$occsclean_id == id
    if (!any(idx)) {
      next
    }
    if (identical(action, "unreviewed")) {
      rec$map_status[idx] <- "Flagged"
    }
    if (action %in% c("unreviewed", "remove")) {
      rec$n_flags[idx] <- pmax(as.integer(rec$n_flags[idx]), 1L)
      existing <- as.character(rec$flag_labels[idx])
      if (!nzchar(existing)) {
        rec$flag_labels[idx] <- label
      } else if (!grepl(label, existing, fixed = TRUE)) {
        rec$flag_labels[idx] <- paste(existing, label, sep = "; ")
      }
    }
  }
  rec
}

#' Build occurrence points for the Visualize map
#'
#' @param occ Occurrence tibble with `occsclean_id`.
#' @param findings Optional findings tibble.
#' @param decisions Optional [DecisionRegistry].
#' @export
build_visualize_points <- function(occ, findings = NULL, decisions = NULL) {
  rec <- build_visualize_records(occ, findings = findings, decisions = decisions)
  if (nrow(rec) < 1) {
    return(empty_visualize_points())
  }
  rec[rec$mappable, , drop = FALSE]
}

#' Color palette for visualize map statuses
#' @noRd
visualize_status_colors <- function() {
  c(
    "OK" = "#2E7D32",
    "Flagged" = "#EF6C00",
    "Passed" = "#1565C0",
    "Failed" = "#C62828"
  )
}

#' Filter visualize records/points by display mode
#' @noRd
filter_visualize_points <- function(pts, mode = "all") {
  if (is.null(pts) || nrow(pts) < 1) {
    return(pts)
  }
  mode <- as.character(mode %||% "all")
  switch(
    mode,
    flagged = pts[pts$map_status == "Flagged", , drop = FALSE],
    removed = pts[pts$map_status == "Failed", , drop = FALSE],
    kept = pts[pts$map_status == "Passed", , drop = FALSE],
    ok = pts[pts$map_status == "OK", , drop = FALSE],
    ok_kept = pts[pts$map_status %in% c("OK", "Passed"), , drop = FALSE],
    pts
  )
}

#' Summarize mapped vs missing-coordinate counts
#' @param records Full visualize record index from [build_visualize_records()].
#' @param mode View mode passed to [filter_visualize_points()].
#' @noRd
summarize_visualize_view <- function(records, mode = "all") {
  view <- filter_visualize_points(records, mode)
  list(
    n_records = nrow(view),
    n_mapped = as.integer(sum(view$mappable)),
    n_missing_coords = as.integer(sum(!view$mappable))
  )
}

#' Duplicate points at lon +/- 360 for wrapped basemaps
#' @param pts Mappable visualize points.
#' @noRd
expand_visualize_points_for_wrap <- function(pts) {
  if (is.null(pts) || nrow(pts) < 1) {
    return(pts)
  }
  offsets <- c(-360, 0, 360)
  pieces <- lapply(offsets, function(off) {
    out <- pts
    out$longitude <- as.numeric(out$longitude) + off
    out
  })
  dplyr::bind_rows(pieces)
}
