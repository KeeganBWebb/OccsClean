test_that("basisOfRecord check skips when column is missing", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    scientificName = c("a", "b")
  )
  res <- check_basis_of_record(
    occ,
    params = list(allowed_basis = "HumanObservation")
  )
  expect_equal(res$status, "skipped")
})

test_that("basisOfRecord check skips without allowed values", {
  occ <- tibble::tibble(
    occsclean_id = "oc_1",
    basisOfRecord = "HumanObservation"
  )
  res <- check_basis_of_record(occ)
  expect_equal(res$status, "skipped")
})

test_that("basisOfRecord values are taken from the file", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3", "oc_4"),
    basisOfRecord = c("HumanObservation", "PreservedSpecimen", "", "HumanObservation")
  )
  expect_equal(
    basis_of_record_values_in_data(occ),
    c("HumanObservation", "PreservedSpecimen")
  )
})

test_that("basisOfRecord check flags blank and disallowed values", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_ok", "oc_blank", "oc_spec"),
    basisOfRecord = c("HumanObservation", "", "PreservedSpecimen")
  )

  res <- check_basis_of_record(
    occ,
    params = list(allowed_basis = c("HumanObservation"))
  )
  expect_equal(res$status, "ok")
  expect_equal(res$engine, "native")
  expect_setequal(res$findings$occsclean_id, c("oc_blank", "oc_spec"))
  expect_false("oc_ok" %in% res$findings$occsclean_id)

  blank <- res$findings[res$findings$occsclean_id == "oc_blank", , drop = FALSE]
  disallowed <- res$findings[res$findings$occsclean_id == "oc_spec", , drop = FALSE]
  expect_equal(blank$finding, "BASIS_MISSING")
  expect_equal(disallowed$finding, "BASIS_DISALLOWED")
})

test_that("basisOfRecord check is registered under basics after defaults", {
  catalog <- list_quality_checks()
  basics <- catalog$check_id[catalog$ui_group == "basics"]
  expect_equal(
    basics,
    c("occ_duplicates", "coord_missing", "coord_invalid", "occ_basis_of_record")
  )
  row <- catalog[catalog$check_id == "occ_basis_of_record", , drop = FALSE]
  expect_equal(row$label, "Basis Of Record")
  expect_true(is.na(row$method_via))
})
