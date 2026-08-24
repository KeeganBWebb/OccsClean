test_that("summarize_missing_columns orders by percent missing", {
  occ <- tibble::tibble(
    a = c(1, 2, 3),
    b = c(NA, NA, 1),
    c = c(NA, NA, NA),
    d = c("x", "y", "z")
  )
  sm <- summarize_missing_columns(occ)
  expect_equal(sm$column, c("c", "b", "a", "d"))
  expect_equal(sm$n_missing, c(3L, 2L, 0L, 0L))
  expect_equal(sm$pct_missing, c(100, 66.7, 0, 0))
})

test_that("missing summary layout uses chunks for moderate column counts", {
  cols <- paste0("col_", seq_len(45))
  occ <- tibble::tibble(occsclean_id = "oc_1")
  for (nm in cols) {
    occ[[nm]] <- NA
  }
  sm <- summarize_missing_columns(occ)
  expect_true(OccsClean:::use_wrapped_missing_table(sm))
  chunks <- OccsClean:::missing_summary_table_chunks(sm)
  expect_length(chunks, 3L)
  expect_equal(nrow(chunks[[1]]), 20L)
  expect_equal(nrow(chunks[[2]]), 20L)
  expect_equal(nrow(chunks[[3]]), 5L)
})

test_that("missing summary layout falls back for large column counts", {
  cols <- paste0("col_", seq_len(61))
  occ <- tibble::tibble(occsclean_id = "oc_1")
  for (nm in cols) {
    occ[[nm]] <- NA
  }
  sm <- summarize_missing_columns(occ)
  expect_false(OccsClean:::use_wrapped_missing_table(sm))
})

test_that("plot_missing_percent_bars handles empty and complete data", {
  empty_plot <- plot_missing_percent_bars(summarize_missing_columns(NULL))
  expect_s3_class(empty_plot, "ggplot")

  complete <- tibble::tibble(a = 1:3, b = letters[1:3])
  complete_plot <- plot_missing_percent_bars(summarize_missing_columns(complete))
  expect_s3_class(complete_plot, "ggplot")

  partial <- tibble::tibble(a = c(1, NA), b = c(NA, NA), c = c(2, 3))
  partial_plot <- plot_missing_percent_bars(summarize_missing_columns(partial))
  expect_s3_class(partial_plot, "ggplot")
  expect_equal(missing_percent_plot_height(0), 220L)
  expect_true(missing_percent_plot_height(10) > 220L)
  expect_true(missing_percent_plot_height(40) <= 720L)
})
