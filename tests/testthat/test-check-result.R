test_that("new_occ_check_result builds a valid object", {
  findings <- empty_findings()
  findings <- tibble::add_row(
    findings,
    occsclean_id = "oc_000001",
    flag = TRUE,
    finding = "ZERO_COORD",
    reason = "Longitude and latitude are both 0",
    evidence = "lon=0;lat=0",
    confidence = NA_real_,
    recommended_action = "keep",
    severity = "high"
  )

  res <- new_occ_check_result(
    check_id = "coord_zero",
    label = "Coordinates at (0,0)",
    category = "coordinate",
    findings = findings
  )

  expect_true(is_occ_check_result(res))
  expect_equal(res$status, "ok")
  expect_equal(res$summary$n_flagged, 1)
  expect_equal(res$summary$n_checked, 1)
})

test_that("new_occ_check_result rejects bad status and actions", {
  expect_error(
    new_occ_check_result("x", "X", "occurrence", status = "nope"),
    "status"
  )

  bad <- empty_findings()
  bad <- tibble::add_row(
    bad,
    occsclean_id = "oc_1",
    flag = TRUE,
    finding = "X",
    reason = "x",
    evidence = "",
    confidence = NA_real_,
    recommended_action = "delete_forever",
    severity = NA_character_
  )
  expect_error(
    new_occ_check_result("x", "X", "occurrence", findings = bad),
    "recommended_action"
  )
})
