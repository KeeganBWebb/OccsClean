fixture_occ <- function() {
  import_occurrences_csv(
    testthat::test_path("fixtures", "example_occurrences.csv")
  )$occ_raw
}

test_that("v0.1 checks flag expected fixture issues", {
  occ <- fixture_occ()

  dups <- check_duplicates(occ)
  expect_equal(dups$status, "ok")
  expect_equal(dups$summary$n_flagged, 2)

  missing <- check_missing_coordinates(occ)
  expect_equal(missing$summary$n_flagged, 1)
  expect_equal(missing$findings$occsclean_id, "oc_000005")

  invalid <- check_invalid_coordinates(occ)
  expect_equal(invalid$summary$n_flagged, 1)
  expect_equal(invalid$findings$occsclean_id, "oc_000004")

  zero <- check_zero_coordinates(occ)
  expect_equal(zero$summary$n_flagged, 1)
  expect_equal(zero$findings$occsclean_id, "oc_000003")

  future <- check_future_dates(occ, params = list(as_of = as.Date("2026-01-01")))
  expect_equal(future$summary$n_flagged, 1)
  expect_equal(future$findings$occsclean_id, "oc_000004")

  invalid_dates <- check_invalid_dates(occ)
  expect_equal(invalid_dates$summary$n_flagged, 0)
})

test_that("year-only dates parse and are not unparseable", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    eventDate = c("1812", "1812-06", "not-a-date")
  )
  parsed <- parse_occurrence_dates(occ$eventDate)
  expect_equal(parsed[[1]], as.Date("1812-01-01"))
  expect_equal(parsed[[2]], as.Date("1812-06-01"))
  expect_true(is.na(parsed[[3]]))

  res <- check_invalid_dates(occ)
  expect_equal(res$summary$n_flagged, 1)
  expect_equal(res$findings$occsclean_id, "oc_3")
})

test_that("date out of range uses user bounds", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    eventDate = c("1812", "1950-01-01", "2020-01-01")
  )

  skipped <- check_date_out_of_range(occ, params = list())
  expect_equal(skipped$status, "skipped")

  ranged <- check_date_out_of_range(
    occ,
    params = list(min_date = "1900-01-01", max_date = "2010-12-31")
  )
  expect_equal(ranged$status, "ok")
  expect_equal(ranged$summary$n_flagged, 2)
  expect_setequal(ranged$findings$occsclean_id, c("oc_1", "oc_3"))
})

test_that("run_quality_checks returns all registered results", {
  occ <- fixture_occ()
  results <- run_quality_checks(occ)
  expect_equal(length(results), nrow(list_quality_checks()))
  expect_true(all(vapply(results, is_occ_check_result, logical(1))))
  expect_equal(results$date_out_of_range$status, "skipped")
})

test_that("OccSession run_checks caches findings for review", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  ids <- setdiff(list_quality_checks()$check_id, "date_out_of_range")
  s$run_checks(check_ids = ids)
  expect_equal(length(s$get_assessment()), length(ids))

  findings <- s$get_findings_table()
  expect_gt(nrow(findings), 0)
  expect_true(all(c("check_id", "occsclean_id", "reason") %in% names(findings)))
})
