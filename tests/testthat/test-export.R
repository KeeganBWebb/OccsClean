test_that("append_manual_review_findings matches coordinate column types", {
  findings <- tibble::tibble(
    check_id = "coord_zero",
    check_label = "Zero",
    category = "coordinate",
    occsclean_id = "oc_1",
    finding = "ZERO",
    reason = "x",
    evidence = "a",
    recommended_action = "review",
    severity = "high",
    decimalLongitude = -105.5,
    decimalLatitude = 40.0
  )
  reg <- DecisionRegistry$new()
  flag_unflagged_for_manual_review(reg, "oc_2")
  occ <- tibble::tibble(
    occsclean_id = "oc_2",
    scientificName = "Test",
    decimalLongitude = -106.1,
    decimalLatitude = 41.2
  )

  out <- append_manual_review_findings(findings, reg, occ = occ)
  expect_equal(nrow(out), 2)
  expect_type(out$decimalLongitude, "double")
  expect_type(out$decimalLatitude, "double")
})

test_that("export flagged includes manually flagged records with decisions", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  s$run_checks(check_ids = "coord_equal")

  occ <- s$get_occ_working()
  findings <- s$get_findings_table()
  flagged_ids <- unique(as.character(findings$occsclean_id))
  clean_id <- setdiff(as.character(occ$occsclean_id), flagged_ids)[[1]]

  flag_unflagged_for_manual_review(s$get_decisions(), clean_id)
  s$touch()

  manual_rows <- function(flagged) {
    flagged[
      as.character(flagged$check) == "MANUAL" &
        as.character(flagged$finding) == "MANUAL",
      ,
      drop = FALSE
    ]
  }

  flagged <- export_flagged_occurrences(s)
  manual <- manual_rows(flagged)
  expect_equal(nrow(manual), 1)
  expect_equal(manual$decision[[1]], "unreviewed")
  expect_equal(manual$reason[[1]], manual_review_reason())

  fail_records(s$get_decisions(), clean_id, findings = findings)
  flagged <- export_flagged_occurrences(s)
  manual <- manual_rows(flagged)
  expect_equal(manual$decision[[1]], "remove")

  pass_records(s$get_decisions(), clean_id, findings = findings)
  flagged <- export_flagged_occurrences(s)
  manual <- manual_rows(flagged)
  expect_equal(manual$decision[[1]], "keep")
})

test_that("cleaned export excludes manually flagged unreviewed records", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  s$run_checks(check_ids = "coord_equal")

  occ <- s$get_occ_working()
  findings <- s$get_findings_table()
  flagged_ids <- unique(as.character(findings$occsclean_id))
  clean_id <- setdiff(as.character(occ$occsclean_id), flagged_ids)[[1]]

  flag_unflagged_for_manual_review(s$get_decisions(), clean_id)
  s$touch()

  cleaned <- build_cleaned_occurrences(
    s$get_occ_raw(),
    s$get_decisions(),
    findings = s$get_findings_table()
  )
  expect_false(clean_id %in% as.character(cleaned$occsclean_id))

  pass_records(
    s$get_decisions(),
    clean_id,
    findings = s$get_findings_table()
  )
  cleaned <- build_cleaned_occurrences(
    s$get_occ_raw(),
    s$get_decisions(),
    findings = s$get_findings_table()
  )
  expect_true(clean_id %in% as.character(cleaned$occsclean_id))
})

test_that("export flagged omits redundant check columns", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  s$run_checks(check_ids = c("coord_missing", "coord_zero"))

  flagged <- export_flagged_occurrences(s)
  expect_gt(nrow(flagged), 0)
  expect_true("check" %in% names(flagged))
  expect_false("check_id" %in% names(flagged))
  expect_false("category" %in% names(flagged))
  expect_false("evidence" %in% names(flagged))
  expect_false("severity" %in% names(flagged))
  expect_false("occsclean_id" %in% names(flagged))
  expect_true("occurrence_date" %in% names(flagged))
})

test_that("processing log lists conducted checks", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  s$run_checks(check_ids = c("coord_missing", "coord_zero"))

  log <- build_processing_log(s)
  expect_true(grepl("Checks conducted", log, fixed = TRUE))
  expect_true(grepl("Missing coordinates", log, fixed = TRUE))
  expect_true(grepl("coord_missing", log, fixed = TRUE))
  expect_true(grepl("check: successful", log, fixed = TRUE))
  expect_true(grepl("Assessed:", log, fixed = TRUE))
  expect_true(grepl("failed:", log, fixed = TRUE))
  expect_true(grepl("passed:", log, fixed = TRUE))
  expect_false(grepl("run at:", log, fixed = TRUE))
  expect_false(grepl("engine:", log, fixed = TRUE))
  expect_false(grepl("Notes\n-----", log, fixed = TRUE))
  expect_true(grepl("Coordinates at (0, 0)", log, fixed = TRUE))
})

test_that("session citations include OccsClean and method_via packages", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  s$run_checks(check_ids = c("coord_missing", "coord_equal"))

  pkgs <- packages_used_by_assessment(s$get_assessment())
  expect_true("CoordinateCleaner" %in% pkgs)
  expect_false("sf" %in% pkgs)

  txt <- build_session_citations(s)
  expect_true(grepl("OccsClean", txt, fixed = TRUE))
  expect_true(grepl("OccsClean citation WIP", txt, fixed = TRUE))
  expect_true(grepl("coord_equal", txt, fixed = TRUE))
  expect_true(grepl("CoordinateCleaner", txt, fixed = TRUE))
  expect_true(grepl("Equal longitude and latitude", txt, ignore.case = TRUE))
})

test_that("packages_used_by_assessment follows method_via only", {
  expect_equal(packages_used_by_assessment(list()), character())
  fake <- list(coord_country = list(check_id = "coord_country", label = "x"))
  names(fake) <- "coord_country"
  pkgs <- packages_used_by_assessment(fake)
  expect_true("CoordinateCleaner" %in% pkgs)
  expect_false("countrycode" %in% pkgs)
})

test_that("processing log reflects per-check keep/remove decisions", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  s$run_checks(check_ids = "coord_missing")

  flagged <- findings_with_decisions(
    s$get_findings_table(),
    s$get_decisions()
  )
  expect_equal(nrow(flagged), 1)
  s$get_decisions()$record_many(flagged, action = "keep")
  s$touch()

  log <- build_processing_log(s)
  expect_true(grepl("failed: 0 of 1 flagged", log, fixed = TRUE))
  expect_true(grepl("passed: 1 of 1 flagged", log, fixed = TRUE))
  expect_false(grepl("still in review", log, fixed = TRUE))
})

test_that("cleaned export requires every flag passed", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    scientificName = c("a", "b", "c")
  )
  findings <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_1", "oc_2"),
    check_id = c("coord_sea", "coord_zero", "coord_sea"),
    finding = c("SEA", "ZERO", "SEA")
  )
  reg <- DecisionRegistry$new()

  # Unreviewed flags exclude the record; unflagged oc_3 is kept
  cleaned <- build_cleaned_occurrences(occ, reg, findings = findings)
  expect_equal(cleaned$occsclean_id, "oc_3")

  reg$record("oc_1", "coord_sea", "keep", finding = "SEA")
  cleaned <- build_cleaned_occurrences(occ, reg, findings = findings)
  expect_false("oc_1" %in% cleaned$occsclean_id)

  reg$record("oc_1", "coord_zero", "keep", finding = "ZERO")
  cleaned <- build_cleaned_occurrences(occ, reg, findings = findings)
  expect_true("oc_1" %in% cleaned$occsclean_id)

  reg$record("oc_2", "coord_sea", "remove", finding = "SEA")
  cleaned <- build_cleaned_occurrences(occ, reg, findings = findings)
  expect_false("oc_2" %in% cleaned$occsclean_id)
  expect_equal(export_counts(occ, reg, findings)$n_cleaned, 2L)
})

test_that("processing log names shapefile instead of dumping coordinates", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
    ))),
    crs = 4326
  )
  params <- list(
    area_source = "nz.shp, nz.shx, nz.dbf",
    area_geom = poly,
    outside_distance_m = 500,
    geometry_repaired = FALSE
  )
  txt <- format_params_for_log(params)
  expect_true(grepl("nz.shp", txt, fixed = TRUE))
  expect_true(grepl("outside_distance_m=500", txt, fixed = TRUE))
  expect_false(grepl("area_geom", txt, fixed = TRUE))
  expect_false(grepl("POLYGON", txt, fixed = TRUE))
  expect_false(grepl("0,0", txt, fixed = TRUE))
})

test_that("run_quality_checks does not pass sibling check params around", {
  occ <- import_occurrences_csv(
    testthat::test_path("fixtures", "example_occurrences.csv")
  )$occ_raw
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
    ))),
    crs = 4326
  )
  res <- run_quality_checks(
    occ,
    check_ids = c("occ_duplicates", "coord_outside_area"),
    params = list(
      coord_outside_area = list(
        area_geom = poly,
        area_source = "unit-test.shp"
      )
    )
  )
  expect_false("area_geom" %in% names(res$occ_duplicates$params_used))
  expect_false("coord_outside_area" %in% names(res$occ_duplicates$params_used))
  expect_equal(res$coord_outside_area$params_used$area_source, "unit-test.shp")
  expect_false("area_geom" %in% names(res$coord_outside_area$params_used))
})
