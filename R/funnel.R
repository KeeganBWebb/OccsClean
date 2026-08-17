#' Build a removal funnel summary
#'
#' @param findings Findings tibble with decisions.
#' @param n_total Total occurrence records in the dataset.
#' @export
build_removal_funnel <- function(findings, n_total) {
  n_total <- as.integer(n_total %||% 0L)
  empty <- tibble::tibble(
    step = integer(),
    check_id = character(),
    label = character(),
    n_removed = integer(),
    n_remaining = integer()
  )
  if (n_total < 1L) {
    return(empty)
  }

  start <- tibble::tibble(
    step = 0L,
    check_id = NA_character_,
    label = "Imported records",
    n_removed = 0L,
    n_remaining = n_total
  )

  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1) {
    return(start)
  }

  df <- findings
  if (!"decision" %in% names(df)) {
    return(start)
  }
  removed <- df[as.character(df$decision) == "remove", , drop = FALSE]
  if (nrow(removed) < 1) {
    return(start)
  }

  label_col <- if ("check_label" %in% names(removed)) {
    "check_label"
  } else if ("check" %in% names(removed)) {
    "check"
  } else {
    "check_id"
  }

  removed$check_id <- as.character(removed$check_id)
  removed$occsclean_id <- as.character(removed$occsclean_id)
  removed$label <- as.character(removed[[label_col]])

  per_check <- dplyr::summarise(
    dplyr::group_by(removed, .data$check_id, .data$label),
    n_removed = dplyr::n_distinct(.data$occsclean_id),
    .groups = "drop"
  )
  per_check <- dplyr::arrange(per_check, dplyr::desc(.data$n_removed), .data$label)

  remaining_n <- n_total
  rows <- list(start)
  covered <- character()

  for (i in seq_len(nrow(per_check))) {
    ids <- unique(removed$occsclean_id[removed$check_id == per_check$check_id[[i]]])
    new_ids <- setdiff(ids, covered)
    covered <- c(covered, new_ids)
    remaining_n <- n_total - length(covered)
    rows[[length(rows) + 1L]] <- tibble::tibble(
      step = as.integer(i),
      check_id = per_check$check_id[[i]],
      label = paste0("After removals: ", per_check$label[[i]]),
      n_removed = as.integer(length(new_ids)),
      n_remaining = as.integer(remaining_n)
    )
  }

  dplyr::bind_rows(rows)
}

#' Whether a funnel has any removal steps
#' @param funnel Tibble from [build_removal_funnel()].
#' @noRd
funnel_has_removals <- function(funnel) {
  is.data.frame(funnel) && nrow(funnel) > 1L
}

#' Placeholder ggplot for an empty funnel
#' @param message Character message.
#' @noRd
funnel_placeholder_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = message,
      size = 4.2,
      color = "black",
      lineheight = 1.2
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}

#' Color ramp for funnel bands
#' @param n Number of bands.
#' @noRd
funnel_band_colors <- function(n) {
  n <- as.integer(n)
  if (n < 1L) {
    return(character())
  }
  grDevices::colorRampPalette(
    c("#A8DADC", "#457B9D", "#1D3557")
  )(n)
}

#' Draw a removal funnel with ggplot2
#'
#' @param funnel Tibble from [build_removal_funnel()].
#' @export
plot_removal_funnel <- function(funnel) {
  if (is.null(funnel) || nrow(funnel) < 1) {
    return(funnel_placeholder_plot(
      "No fail decisions yet.\nFail findings in Review."
    ))
  }

  if (!funnel_has_removals(funnel)) {
    n <- funnel$n_remaining[[1]]
    return(funnel_placeholder_plot(
      paste0(
        "Imported records: ", format(n, big.mark = ","), "\n",
        "No failed occurrences yet.\n",
        "Fail findings in Review to build this funnel."
      )
    ))
  }

  df <- funnel
  df$y <- rev(seq_len(nrow(df)))
  max_n <- max(df$n_remaining, na.rm = TRUE)
  if (!is.finite(max_n) || max_n < 1) {
    max_n <- 1
  }
  df$xmin <- -df$n_remaining / (2 * max_n)
  df$xmax <- df$n_remaining / (2 * max_n)
  df$ymin <- df$y - 0.4
  df$ymax <- df$y + 0.4
  df$fill <- funnel_band_colors(nrow(df))

  ggplot2::ggplot(df) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      fill = df$fill,
      color = "white",
      linewidth = 0.8
    ) +
    ggplot2::coord_cartesian(xlim = c(-0.55, 0.55), clip = "off") +
    ggplot2::labs(title = "Records remaining after deletions") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        color = "black",
        size = 14
      ),
      plot.margin = ggplot2::margin(16, 16, 16, 16)
    )
}

#' Display label for visualize map_status values
#' @noRd
visualize_status_label <- function(status) {
  out <- as.character(status)
  out[out == "OK"] <- "Unflagged"
  out[out == "Passed"] <- "Passed"
  out[out == "Failed"] <- "Failed"
  out[out == "Kept"] <- "Passed"
  out[out == "Marked for deletion"] <- "Failed"
  out
}
