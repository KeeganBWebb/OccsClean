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
  expect_true(grepl("At \\(0,0\\)", occ$checks[occ$occsclean_id == "oc_1"], fixed = TRUE))
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

  prepared <- prepare_review_occurrence_table(findings, reg)
  expect_false(is.factor(prepared$occsclean_id))
  expect_false(is.factor(prepared$scientificName))
  expect_false("findings" %in% names(prepared))
  expect_true(is.factor(prepared$review_status))
  expect_true(is.factor(prepared$n_flags))
  expect_equal(levels(prepared$n_flags), c("1", "2"))
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
