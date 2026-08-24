test_that("findings_with_decisions joins effective actions", {
  findings <- tibble::tibble(
    check_id = c("coord_missing", "coord_zero"),
    check_label = c("Missing coordinates", "Coordinates at (0, 0)"),
    category = c("coordinate", "coordinate"),
    occsclean_id = c("oc_000001", "oc_000002"),
    finding = c("COORD_MISSING", "COORD_ZERO"),
    reason = c("x", "y"),
    evidence = c("a", "b"),
    recommended_action = c("remove", "remove"),
    severity = c("high", "high")
  )
  reg <- DecisionRegistry$new()
  reg$record(
    occsclean_id = "oc_000001",
    check_id = "coord_missing",
    finding = "COORD_MISSING",
    action = "remove"
  )

  out <- findings_with_decisions(findings, reg)
  expect_equal(out$decision[[1]], "remove")
  expect_equal(out$decision[[2]], "unreviewed")

  prepared <- prepare_review_table(findings, reg)
  expect_true(is.factor(prepared$finding))
  expect_true(is.factor(prepared$check))
  expect_true(is.factor(prepared$reason))
  expect_true(is.factor(prepared$decision))
  expect_false(is.factor(prepared$occsclean_id))
  expect_true("COORD_MISSING" %in% levels(prepared$finding))
  expect_true("check" %in% names(prepared))
  expect_true("check_id" %in% names(prepared))
  expect_false("category" %in% names(prepared))
  expect_false("severity" %in% names(prepared))
  expect_false("evidence" %in% names(prepared))
})

test_that("review table keeps occsclean_id and occurrence_date as type filters", {
  findings <- tibble::tibble(
    check_id = "coord_missing",
    check_label = "Missing coordinates",
    category = "coordinate",
    occsclean_id = c("oc_1", "oc_2"),
    finding = c("COORD_MISSING", "COORD_MISSING"),
    reason = c("blank", "blank"),
    evidence = c("a", "b"),
    recommended_action = c("remove", "remove"),
    severity = c("high", "high"),
    scientificName = c("Pinus ponderosa", "Quercus alba"),
    occurrence_date = c("2020-01-01", "2021-06-15"),
    basisOfRecord = c("HumanObservation", "PreservedSpecimen")
  )
  prepared <- prepare_review_table(findings, DecisionRegistry$new())
  expect_false(is.factor(prepared$occsclean_id))
  expect_false(is.factor(prepared$occurrence_date))
  expect_true(is.factor(prepared$scientificName))
  expect_true(is.factor(prepared$basisOfRecord))
  expect_true("Pinus ponderosa" %in% levels(prepared$scientificName))
})

test_that("build_cleaned_occurrences drops failed records", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    scientificName = c("a", "b", "c")
  )
  reg <- DecisionRegistry$new()
  reg$record(
    occsclean_id = "oc_2",
    check_id = "coord_missing",
    finding = "COORD_MISSING",
    action = "remove"
  )
  cleaned <- build_cleaned_occurrences(occ, reg)
  expect_equal(cleaned$occsclean_id, c("oc_1", "oc_3"))
  expect_equal(export_counts(occ, reg)$n_cleaned, 2)
})

test_that("prepare_review_table uses pass/fail labels", {
  findings <- tibble::tibble(
    check_id = "coord_missing",
    check_label = "Missing coordinates",
    category = "coordinate",
    occsclean_id = "oc_1",
    finding = "COORD_MISSING",
    reason = "blank",
    evidence = "a",
    recommended_action = "remove",
    severity = "high"
  )
  reg <- DecisionRegistry$new()
  reg$record(
    occsclean_id = "oc_1",
    check_id = "coord_missing",
    finding = "COORD_MISSING",
    action = "remove"
  )
  prepared <- prepare_review_table(findings, reg)
  expect_true("fail" %in% as.character(prepared$decision))
})

test_that("occurrence review collapses flags and supports pass/fail/return", {
  findings <- tibble::tibble(
    check_id = c("coord_sea", "coord_zero", "coord_sea"),
    check_label = c("In the ocean", "At (0,0)", "In the ocean"),
    occsclean_id = c("oc_1", "oc_1", "oc_2"),
    finding = c("SEA", "ZERO", "SEA"),
    scientificName = c("a", "a", "b")
  )
  reg <- DecisionRegistry$new()

  occ <- build_review_occurrences(findings, reg)
  expect_equal(nrow(occ), 2)
  expect_equal(occ$n_flags[occ$occsclean_id == "oc_1"], 2L)
  expect_true(grepl("In the ocean", occ$checks[occ$occsclean_id == "oc_1"], fixed = TRUE))
  expect_true(grepl("At (0,0)", occ$checks[occ$occsclean_id == "oc_1"], fixed = TRUE))
  expect_true(all(occ$review_status == "review"))

  pass_records(reg, "oc_1", findings = findings)
  occ <- build_review_occurrences(findings, reg)
  expect_equal(occ$review_status[occ$occsclean_id == "oc_1"], "pass")
  expect_equal(occ$review_status[occ$occsclean_id == "oc_2"], "review")

  fail_records(reg, "oc_2", findings = findings)
  occ <- build_review_occurrences(findings, reg)
  expect_equal(occ$review_status[occ$occsclean_id == "oc_2"], "fail")

  return_records_to_review(reg, c("oc_1", "oc_2"), findings = findings)
  occ <- build_review_occurrences(findings, reg)
  expect_true(all(occ$review_status == "review"))

  expect_equal(
    sort(occsclean_ids_with_finding(findings, "SEA")),
    c("oc_1", "oc_2")
  )
  expect_equal(occsclean_ids_with_only_finding(findings, "SEA"), "oc_2")
  expect_equal(occsclean_ids_with_only_finding(findings, "ZERO"), character())
  expect_equal(finding_codes_for_records(findings, "oc_1"), c("SEA", "ZERO"))

  choices <- OccsClean:::review_column_filter_choices(occ, "scientificName")
  expect_equal(choices, c("a", "b"))

  check_parts <- OccsClean:::review_checks_column_filter_choices(occ)
  expect_equal(check_parts, c("At (0,0)", "In the ocean"))

  expect_equal(
    OccsClean:::occurrence_review_status_label(
      c("review", "pass", "fail", "batch_fail", "batch_pass")
    ),
    c("In review", "Passed", "Failed", "Batch Failed", "Batch Passed")
  )

  prepared <- prepare_review_occurrence_table(findings, reg)
  expect_false(is.factor(prepared$occsclean_id))
  expect_false(is.factor(prepared$scientificName))
  expect_false("findings" %in% names(prepared))
  expect_true(is.factor(prepared$review_status))
  expect_true(is.factor(prepared$n_flags))
  expect_equal(levels(prepared$n_flags), c("1", "2"))
})

test_that("build_review_occurrences includes manually reviewed unflagged records", {
  findings <- tibble::tibble(
    check_id = "coord_zero",
    check_label = "At (0,0)",
    occsclean_id = "oc_flag",
    finding = "ZERO",
    scientificName = "Flagged sp"
  )
  occ <- tibble::tibble(
    occsclean_id = c("oc_flag", "oc_clean"),
    scientificName = c("Flagged sp", "Clean sp"),
    decimalLongitude = c("-104", "-105"),
    decimalLatitude = c("40", "41"),
    basisOfRecord = c("HumanObservation", "HumanObservation")
  )
  reg <- DecisionRegistry$new()

  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_equal(out$occsclean_id, "oc_flag")
  expect_true(all(out$review_status == "review"))

  fail_records(reg, "oc_clean", findings = findings)
  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_setequal(out$occsclean_id, c("oc_clean", "oc_flag"))
  expect_equal(out$review_status[out$occsclean_id == "oc_clean"], "fail")
  expect_equal(out$n_flags[out$occsclean_id == "oc_clean"], 0L)
  expect_equal(out$scientificName[out$occsclean_id == "oc_clean"], "Clean sp")

  return_records_to_review(reg, "oc_clean", findings = findings)
  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_setequal(out$occsclean_id, c("oc_clean", "oc_flag"))
  expect_equal(out$review_status[out$occsclean_id == "oc_clean"], "review")

  pass_records(reg, "oc_clean", findings = findings)
  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_equal(out$review_status[out$occsclean_id == "oc_clean"], "pass")
})

test_that("return_records_to_review adds unflagged map-only records to review table", {
  findings <- tibble::tibble(
    check_id = "coord_zero",
    check_label = "At (0,0)",
    occsclean_id = "oc_flag",
    finding = "ZERO",
    scientificName = "Flagged sp"
  )
  occ <- tibble::tibble(
    occsclean_id = c("oc_flag", "oc_clean"),
    scientificName = c("Flagged sp", "Clean sp"),
    decimalLongitude = c("-104", "-105"),
    decimalLatitude = c("40", "41")
  )
  reg <- DecisionRegistry$new()

  flag_unflagged_for_manual_review(reg, "oc_clean")
  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_setequal(out$occsclean_id, c("oc_clean", "oc_flag"))
  expect_equal(out$review_status[out$occsclean_id == "oc_clean"], "review")
  expect_equal(out$n_flags[out$occsclean_id == "oc_clean"], 1L)
  expect_equal(out$checks[out$occsclean_id == "oc_clean"], "MANUAL")
  expect_equal(
    occurrence_review_status_label(
      out$review_status[out$occsclean_id == "oc_clean"]
    ),
    "In review"
  )

  pts <- build_visualize_records(occ, findings = findings, decisions = reg)
  clean_pt <- pts[pts$occsclean_id == "oc_clean", , drop = FALSE]
  expect_equal(clean_pt$map_status, "Flagged")
  expect_equal(clean_pt$n_flags, 1L)
  expect_equal(clean_pt$flag_labels, "MANUAL")

  fail_records(reg, "oc_clean", findings = findings)
  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_equal(out$review_status[out$occsclean_id == "oc_clean"], "fail")
  expect_equal(out$checks[out$occsclean_id == "oc_clean"], "MANUAL")
  pts <- build_visualize_records(occ, findings = findings, decisions = reg)
  clean_pt <- pts[pts$occsclean_id == "oc_clean", , drop = FALSE]
  expect_equal(clean_pt$map_status, "Failed")
  expect_equal(clean_pt$flag_labels, "MANUAL")

  pass_records(reg, "oc_clean", findings = findings)
  out <- build_review_occurrences(findings, reg, occ = occ)
  expect_equal(out$review_status[out$occsclean_id == "oc_clean"], "pass")
  expect_equal(out$checks[out$occsclean_id == "oc_clean"], "")
  pts <- build_visualize_records(occ, findings = findings, decisions = reg)
  clean_pt <- pts[pts$occsclean_id == "oc_clean", , drop = FALSE]
  expect_equal(clean_pt$map_status, "Passed")
  expect_equal(clean_pt$flag_labels, "")
})

test_that("filter_review_occurrences_by_check_label matches check labels", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    checks = c("Missing coordinates", "Basis Of Record", "Missing coordinates; Basis Of Record")
  )
  filtered <- OccsClean:::filter_review_occurrences_by_check_label(
    occ,
    "Missing coordinates"
  )
  expect_equal(filtered$occsclean_id, c("oc_1", "oc_3"))
  expect_equal(
    nrow(OccsClean:::filter_review_occurrences_by_check_label(occ, "")),
    3L
  )
  expect_equal(
    nrow(
      OccsClean:::filter_review_occurrences_by_check_label(
        occ,
        OccsClean:::review_check_flag_all_value()
      )
    ),
    3L
  )
  expect_true(OccsClean:::review_check_flag_is_all(""))
  expect_true(
    OccsClean:::review_check_flag_is_all(OccsClean:::review_check_flag_all_value())
  )
  expect_false(OccsClean:::review_check_flag_is_all("Missing coordinates"))
})

test_that("review_map_view_lnglat nudges null island off exact zero", {
  view <- OccsClean:::review_map_view_lnglat(0, 0)
  expect_false(view$lng == 0 && view$lat == 0)
  expect_equal(view$lng, 0.0001)
  expect_equal(view$lat, 0.0001)
  other <- OccsClean:::review_map_view_lnglat(-105, 40)
  expect_equal(other$lng, -105)
  expect_equal(other$lat, 40)
})

test_that("batch fail actions mark occurrences as Batch Failed in review table", {
  findings <- tibble::tibble(
    check_id = c("coord_zero", "coord_zero", "coord_zero"),
    check_label = c("At (0,0)", "At (0,0)", "At (0,0)"),
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    finding = c("ZERO", "ZERO", "ZERO"),
    scientificName = c("a", "b", "c")
  )
  reg <- DecisionRegistry$new()
  fail_records(reg, c("oc_1", "oc_2"), findings = findings, batch = TRUE)

  occ <- build_review_occurrences(findings, reg)
  expect_equal(occ$review_status[occ$occsclean_id == "oc_1"], "batch_fail")
  expect_equal(occ$review_status[occ$occsclean_id == "oc_2"], "batch_fail")
  expect_equal(
    occurrence_review_status_label("batch_fail"),
    "Batch Failed"
  )
  expect_setequal(
    OccsClean:::batch_failed_occsclean_ids(reg),
    c("oc_1", "oc_2")
  )

  fail_records(reg, "oc_3", findings = findings, batch = FALSE)
  occ <- build_review_occurrences(findings, reg)
  expect_equal(occ$review_status[occ$occsclean_id == "oc_3"], "fail")
})

test_that("batch pass actions mark occurrences as Batch Passed in review table", {
  findings <- tibble::tibble(
    check_id = c("coord_zero", "coord_zero", "coord_zero"),
    check_label = c("At (0,0)", "At (0,0)", "At (0,0)"),
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    finding = c("ZERO", "ZERO", "ZERO"),
    scientificName = c("a", "b", "c")
  )
  reg <- DecisionRegistry$new()
  pass_records(reg, c("oc_1", "oc_2"), findings = findings, batch = TRUE)

  occ <- build_review_occurrences(findings, reg)
  expect_equal(occ$review_status[occ$occsclean_id == "oc_1"], "batch_pass")
  expect_equal(occ$review_status[occ$occsclean_id == "oc_2"], "batch_pass")
  expect_equal(
    occurrence_review_status_label("batch_pass"),
    "Batch Passed"
  )
  expect_setequal(
    OccsClean:::batch_passed_occsclean_ids(reg),
    c("oc_1", "oc_2")
  )

  pass_records(reg, "oc_3", findings = findings, batch = FALSE)
  occ <- build_review_occurrences(findings, reg)
  expect_equal(occ$review_status[occ$occsclean_id == "oc_3"], "pass")
})

test_that("review_panel_map_status maps batch_fail to Failed for map panel", {
  expect_equal(
    OccsClean:::review_panel_map_status("batch_fail", 1L),
    "Failed"
  )
  expect_equal(
    OccsClean:::review_panel_map_status("batch_pass", 1L),
    "Passed"
  )
})

test_that("review_panel_map_status maps occurrence review rows to map statuses", {
  expect_equal(
    OccsClean:::review_panel_map_status("review", 2L),
    "Flagged"
  )
  expect_equal(
    OccsClean:::review_panel_map_status("review", 0L),
    "Flagged"
  )
  expect_equal(
    OccsClean:::review_panel_map_status("pass", 0L),
    "Passed"
  )
  expect_equal(
    OccsClean:::review_panel_map_status("fail", 3L),
    "Failed"
  )
})

test_that("review_table_coord_map embeds follow payloads for mappable rows", {
  df <- tibble::tibble(
    occsclean_id = c("oc_zero", "oc_missing"),
    checks = c("At (0,0)", "Missing coordinates")
  )
  coords <- tibble::tibble(
    occsclean_id = c("oc_zero", "oc_missing"),
    longitude = c(0, NA_real_),
    latitude = c(0, NA_real_),
    mappable = c(TRUE, FALSE)
  )
  out <- OccsClean:::review_table_coord_map(df, coords)
  expect_length(out, 1L)
  expect_equal(out$oc_zero$id, "oc_zero")
  expect_equal(out$oc_zero$lng, 0)
  expect_equal(out$oc_zero$lat, 0)
  expect_equal(out$oc_zero$viewLng, 0.0001)
  expect_equal(out$oc_zero$viewLat, 0.0001)
  expect_null(out$oc_missing)
})

test_that("filter_review_occurrences_by_checks requires all selected checks", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3"),
    check_ids = c("coord_sea,coord_zero", "coord_sea", "coord_zero"),
    checks = c("a; b", "a", "b")
  )
  filtered <- OccsClean:::filter_review_occurrences_by_checks(
    occ,
    c("coord_sea", "coord_zero")
  )
  expect_equal(filtered$occsclean_id, "oc_1")
  expect_equal(
    nrow(OccsClean:::filter_review_occurrences_by_checks(occ, character())),
    3L
  )
})
