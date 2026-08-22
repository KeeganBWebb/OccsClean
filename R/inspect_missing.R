#' Summarize missing values by column
#'
#' @param occ Occurrence data frame.
#' @export
summarize_missing_columns <- function(occ) {
  empty <- tibble::tibble(
    column = character(),
    n_missing = integer(),
    pct_missing = numeric()
  )
  if (is.null(occ) || !is.data.frame(occ) || ncol(occ) < 1) {
    return(empty)
  }

  n_rows <- nrow(occ)
  counts <- vapply(occ, function(col) sum(is.na(col)), integer(1))
  out <- tibble::tibble(
    column = names(counts),
    n_missing = as.integer(counts),
    pct_missing = if (n_rows < 1) {
      rep(0, length(counts))
    } else {
      round(100 * as.numeric(counts) / n_rows, 1)
    }
  )
  out[order(-out$pct_missing, out$column), , drop = FALSE]
}

#' @noRd
missing_table_chunk_size <- function() {
  20L
}

#' @noRd
missing_table_max_chunks <- function() {
  3L
}

#' Whether to use side-by-side missing table chunks
#' @noRd
use_wrapped_missing_table <- function(summary) {
  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) < 1) {
    return(FALSE)
  }
  n <- nrow(summary)
  n <= missing_table_chunk_size() * missing_table_max_chunks()
}

#' Split missing summary into table chunks
#' @noRd
missing_summary_table_chunks <- function(summary,
                                         chunk_size = missing_table_chunk_size()) {
  n <- nrow(summary)
  if (n < 1) {
    return(list())
  }
  chunk_size <- as.integer(chunk_size)[1]
  if (is.na(chunk_size) || chunk_size < 1L) {
    chunk_size <- missing_table_chunk_size()
  }
  indices <- split(seq_len(n), (seq_len(n) - 1L) %/% chunk_size)
  lapply(indices, function(idx) summary[idx, , drop = FALSE])
}

#' HTML table for missing summary rows
#' @noRd
missing_summary_table_ui <- function(summary) {
  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) < 1) {
    return(NULL)
  }
  rows <- lapply(seq_len(nrow(summary)), function(i) {
    shiny::tags$tr(
      shiny::tags$td(summary$column[i]),
      shiny::tags$td(summary$n_missing[i]),
      shiny::tags$td(summary$pct_missing[i])
    )
  })
  shiny::tags$table(
    class = "table table-striped table-bordered table-hover",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("column"),
        shiny::tags$th("n_missing"),
        shiny::tags$th("pct_missing")
      )
    ),
    shiny::tags$tbody(rows)
  )
}

#' Missing table UI with optional side-by-side chunks
#' @noRd
missing_summary_layout_ui <- function(summary) {
  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) < 1) {
    return(NULL)
  }
  if (!use_wrapped_missing_table(summary)) {
    return(shiny::div(
      class = "oc-missing-table oc-missing-table-single",
      missing_summary_table_ui(summary)
    ))
  }

  chunks <- missing_summary_table_chunks(summary)
  shiny::div(
    class = "oc-missing-table-wrap",
    lapply(chunks, function(chunk) {
      shiny::div(
        class = "oc-missing-table-chunk",
        missing_summary_table_ui(chunk)
      )
    })
  )
}

#' Inspect status summary UI
#' @noRd
inspect_status_ui <- function(occ) {
  if (is.null(occ) || !is.data.frame(occ) || ncol(occ) < 1) {
    return(NULL)
  }
  col_tags <- lapply(names(occ), function(name) {
    shiny::tags$span(class = "oc-inspect-column-name", name)
  })
  shiny::div(
    class = "oc-inspect-status",
    shiny::tags$p(
      shiny::tags$strong("Records: "),
      nrow(occ)
    ),
    shiny::tags$p(
      shiny::tags$strong("Columns: "),
      ncol(occ)
    ),
    shiny::tags$div(
      class = "oc-inspect-status-label",
      shiny::tags$strong("Column names:")
    ),
    shiny::div(class = "oc-inspect-column-names", col_tags)
  )
}

#' Horizontal bar chart of percent missing by column
#'
#' @param summary Output of [summarize_missing_columns()].
#' @param max_bars Maximum number of incomplete columns to show.
#' @export
plot_missing_percent_bars <- function(summary, max_bars = 30L) {
  max_bars <- as.integer(max_bars)[1]
  if (is.na(max_bars) || max_bars < 1L) {
    max_bars <- 30L
  }

  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) < 1) {
    return(missing_percent_empty_plot("No columns to summarize yet."))
  }
  if (!all(c("column", "pct_missing") %in% names(summary))) {
    return(missing_percent_empty_plot("Missing-data summary is incomplete."))
  }

  plot_df <- summary[summary$n_missing > 0, , drop = FALSE]
  if (nrow(plot_df) < 1) {
    return(missing_percent_empty_plot("No missing values in any column."))
  }

  plot_df <- plot_df[order(-plot_df$pct_missing, plot_df$column), , drop = FALSE]
  n_total <- nrow(plot_df)
  plot_df <- utils::head(plot_df, max_bars)
  plot_df$column <- factor(plot_df$column, levels = rev(plot_df$column))

  subtitle <- NULL
  if (n_total > max_bars) {
    subtitle <- paste0(
      "Showing top ", max_bars, " of ", n_total,
      " incomplete columns (sorted by % missing)."
    )
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$pct_missing, y = .data$column, fill = .data$pct_missing)
  ) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradientn(
      colours = c("#2A9D8F", "#E9C46A", "#F4A261", "#E76F51"),
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      name = "% missing"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      title = "Missing data by column",
      subtitle = subtitle,
      x = "Percent missing",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        color = "black"
      ),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black"),
      legend.position = "top",
      legend.title = ggplot2::element_text(color = "black"),
      legend.text = ggplot2::element_text(color = "black"),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(12, 16, 12, 12)
    )
}

#' Empty-state plot for missingness chart
#' @noRd
missing_percent_empty_plot <- function(message) {
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

#' Plot height in px for a missingness bar chart
#' @noRd
missing_percent_plot_height <- function(n_bars) {
  n <- as.integer(n_bars)[1]
  if (is.na(n) || n < 1L) {
    return(220L)
  }
  as.integer(max(220L, min(720L, 80L + n * 22L)))
}
