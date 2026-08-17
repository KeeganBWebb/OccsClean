#' Summarize review decisions by flag category
#'
#' @param findings Findings tibble with decisions.
#' @export
build_flag_decision_summary <- function(findings) {
  empty <- tibble::tibble(
    check_id = character(),
    label = character(),
    status = factor(
      character(),
      levels = c("Passed", "In Review", "Failed")
    ),
    n = integer()
  )

  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) < 1) {
    return(empty)
  }
  if (!all(c("check_id", "decision") %in% names(findings))) {
    return(empty)
  }

  df <- findings
  if ("check_id" %in% names(df)) {
    df <- df[
      as.character(df$check_id) != map_review_check_id(),
      ,
      drop = FALSE
    ]
  }
  if (nrow(df) < 1) {
    return(empty)
  }

  label_col <- if ("check_label" %in% names(df)) {
    "check_label"
  } else if ("check" %in% names(df)) {
    "check"
  } else {
    "check_id"
  }

  df$check_id <- as.character(df$check_id)
  df$label <- as.character(df[[label_col]])
  df$label[is.na(df$label) | !nzchar(df$label)] <- df$check_id[
    is.na(df$label) | !nzchar(df$label)
  ]

  decision <- as.character(df$decision)
  status <- rep(NA_character_, length(decision))
  status[decision %in% c("keep", "pass")] <- "Passed"
  status[decision %in% c("unreviewed", "(blank)")] <- "In Review"
  status[is.na(decision) | !nzchar(decision)] <- "In Review"
  status[decision %in% c("remove", "fail")] <- "Failed"
  df <- df[!is.na(status), , drop = FALSE]
  df$status <- status[!is.na(status)]

  if (nrow(df) < 1) {
    return(empty)
  }

  check_ids <- unique(df$check_id)
  if (length(check_ids) < 1) {
    return(empty)
  }

  labels <- dplyr::summarise(
    dplyr::group_by(df, .data$check_id),
    label = {
      labs <- unique(.data$label)
      labs <- labs[!is.na(labs) & nzchar(labs)]
      if (length(labs) < 1) {
        .data$check_id[[1]]
      } else {
        labs[[1]]
      }
    },
    .groups = "drop"
  )

  counts <- dplyr::summarise(
    dplyr::group_by(df, .data$check_id, .data$status),
    n = dplyr::n(),
    .groups = "drop"
  )

  grid <- expand.grid(
    check_id = check_ids,
    status = c("Passed", "In Review", "Failed"),
    stringsAsFactors = FALSE
  )
  grid <- tibble::as_tibble(grid)

  out <- dplyr::left_join(grid, counts, by = c("check_id", "status"))
  out$n[is.na(out$n)] <- 0L
  out <- dplyr::left_join(out, labels, by = "check_id")
  out$status <- factor(
    out$status,
    levels = c("Passed", "In Review", "Failed")
  )

  totals <- dplyr::summarise(
    dplyr::group_by(out, .data$check_id, .data$label),
    n_failed = sum(.data$n[.data$status == "Failed"]),
    n_review = sum(.data$n[.data$status == "In Review"]),
    .groups = "drop"
  )
  totals <- dplyr::arrange(
    totals,
    dplyr::desc(.data$n_failed),
    dplyr::desc(.data$n_review),
    .data$label
  )
  out$label <- factor(out$label, levels = totals$label)
  out <- dplyr::arrange(out, .data$label, .data$status)
  out$n <- as.integer(out$n)
  out
}

#' Grouped bar chart of Passed / In Review / Failed by flag category
#'
#' @param summary Tibble from [build_flag_decision_summary()].
#' @export
plot_flag_decision_bars <- function(summary) {
  if (is.null(summary) || nrow(summary) < 1) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = "No flagged findings yet.\nRun Assess, then decide in Review.",
          size = 4.2,
          color = "black",
          lineheight = 1.2
        ) +
        ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
    )
  }

  fills <- c(
    "Passed" = "#2A9D8F",
    "In Review" = "#E9C46A",
    "Failed" = "#E76F51"
  )

  labels <- unique(as.character(summary$label))
  n_cat <- length(labels)
  x_expand <- if (n_cat <= 4L) 0.35 else 0.12
  max_lab <- max(nchar(labels), na.rm = TRUE)
  if (!is.finite(max_lab) || max_lab < 1) {
    max_lab <- 12L
  }
  left_pad <- max(28, min(140, as.numeric(max_lab) * 3.4))
  bottom_pad <- max(28, min(96, as.numeric(max_lab) * 2.2))
  x_expand_left <- x_expand + if (n_cat >= 8L) 0.35 else if (n_cat >= 5L) 0.2 else 0.05

  ggplot2::ggplot(
    summary,
    ggplot2::aes(x = .data$label, y = .data$n, fill = .data$status)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.75),
      width = 0.68,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(values = fills, drop = FALSE) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(add = c(x_expand_left, x_expand))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.06)),
      breaks = scales_pretty_breaks_safe,
      labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
    ) +
    ggplot2::labs(
      title = "Review decisions by flag category",
      x = NULL,
      y = "Findings",
      fill = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        color = "black"
      ),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1, size = 11),
      legend.position = "top",
      legend.text = ggplot2::element_text(color = "black"),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(16, 24, bottom_pad, left_pad)
    )
}

#' Integer-friendly y breaks without requiring scales
#' @noRd
scales_pretty_breaks_safe <- function(x) {
  pretty(x, n = 6)
}
