test_that("build_removal_funnel orders by most removed and avoids double-counting", {
  findings <- tibble::tibble(
    check_id = c("a", "a", "a", "b", "b", "c"),
    check_label = c("Check A", "Check A", "Check A", "Check B", "Check B", "Check C"),
    occsclean_id = c("r1", "r2", "r3", "r1", "r4", "r5"),
    decision = c("remove", "remove", "remove", "remove", "remove", "keep")
  )

  funnel <- build_removal_funnel(findings, n_total = 10L)
  expect_equal(funnel$step, 0:2)
  expect_equal(funnel$label[[1]], "Imported records")
  expect_equal(funnel$n_remaining[[1]], 10L)
  expect_equal(funnel$check_id[-1], c("a", "b"))
  expect_equal(funnel$n_removed[-1], c(3L, 1L))
  expect_equal(funnel$n_remaining[-1], c(7L, 6L))
})

test_that("build_flag_decision_summary counts Passed / In Review / Failed", {
  findings <- tibble::tibble(
    check_id = c("a", "a", "a", "b", "b"),
    check_label = c("Check A", "Check A", "Check A", "Check B", "Check B"),
    occsclean_id = c("r1", "r2", "r3", "r4", "r5"),
    decision = c("remove", "keep", "unreviewed", "remove", "remove")
  )
  summary <- build_flag_decision_summary(findings)
  expect_equal(levels(summary$status), c("Passed", "In Review", "Failed"))
  a <- summary[summary$check_id == "a", ]
  expect_equal(a$n[a$status == "Passed"], 1L)
  expect_equal(a$n[a$status == "In Review"], 1L)
  expect_equal(a$n[a$status == "Failed"], 1L)
  b <- summary[summary$check_id == "b", ]
  expect_equal(b$n[b$status == "Failed"], 2L)
  expect_equal(b$n[b$status == "Passed"], 0L)
  # Most-failed category first (B has 2, A has 1)
  expect_equal(as.character(levels(summary$label)[[1]]), "Check B")
})

test_that("build_flag_decision_summary includes categories without failures", {
  findings <- tibble::tibble(
    check_id = c("a", "a", "b"),
    check_label = c("Only passed", "Only passed", "Has fail"),
    occsclean_id = c("r1", "r2", "r3"),
    decision = c("keep", "unreviewed", "remove")
  )
  summary <- build_flag_decision_summary(findings)
  expect_true("Only passed" %in% as.character(summary$label))
  expect_true("Has fail" %in% as.character(summary$label))
})

test_that("build_flag_decision_summary accepts pass/fail display labels", {
  findings <- tibble::tibble(
    check_id = c("a", "a"),
    check = c("Check A", "Check A"),
    occsclean_id = c("r1", "r2"),
    decision = c("pass", "fail")
  )
  summary <- build_flag_decision_summary(findings)
  expect_equal(summary$n[summary$status == "Passed"], 1L)
  expect_equal(summary$n[summary$status == "Failed"], 1L)
})

test_that("plot_flag_decision_bars returns a ggplot", {
  summary <- build_flag_decision_summary(
    tibble::tibble(
      check_id = "a",
      check = "Check A",
      occsclean_id = c("r1", "r2"),
      decision = c("remove", "keep")
    )
  )
  expect_s3_class(plot_flag_decision_bars(summary), "ggplot")
  expect_s3_class(plot_flag_decision_bars(NULL), "ggplot")
})

test_that("visualize_status_label renames OK for display", {
  expect_equal(
    OccsClean:::visualize_status_label(
      c("OK", "Flagged", "Passed", "Failed", "Kept", "Marked for deletion")
    ),
    c("Unflagged", "In review", "Passed", "Failed", "Passed", "Failed")
  )
})
