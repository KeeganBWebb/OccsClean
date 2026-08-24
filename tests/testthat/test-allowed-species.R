test_that("allowed species check skips when column is missing", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    basisOfRecord = c("HumanObservation", "HumanObservation")
  )
  res <- check_allowed_species(
    occ,
    params = list(allowed_species = "Pinus ponderosa")
  )
  expect_equal(res$status, "skipped")
})

test_that("allowed species check skips without selected names", {
  occ <- tibble::tibble(
    occsclean_id = "oc_1",
    scientificName = "Pinus ponderosa"
  )
  res <- check_allowed_species(occ)
  expect_equal(res$status, "skipped")
})

test_that("scientific name values are taken from the file", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_3", "oc_4"),
    scientificName = c("Pinus ponderosa", "Quercus alba", "", "Pinus ponderosa")
  )
  expect_equal(
    scientific_name_values_in_data(occ),
    c("Pinus ponderosa", "Quercus alba")
  )
})

test_that("allowed species check flags blank and disallowed names", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_ok", "oc_blank", "oc_other"),
    scientificName = c("Pinus ponderosa", "", "Quercus alba")
  )

  res <- check_allowed_species(
    occ,
    params = list(allowed_species = c("Pinus ponderosa"))
  )
  expect_equal(res$status, "ok")
  expect_equal(res$engine, "native")
  expect_setequal(res$findings$occsclean_id, c("oc_blank", "oc_other"))
  expect_false("oc_ok" %in% res$findings$occsclean_id)

  blank <- res$findings[res$findings$occsclean_id == "oc_blank", , drop = FALSE]
  disallowed <- res$findings[res$findings$occsclean_id == "oc_other", , drop = FALSE]
  expect_equal(blank$finding, "TAXON_MISSING")
  expect_equal(disallowed$finding, "TAXON_DISALLOWED")
})

test_that("allowed species check is registered under taxonomic", {
  catalog <- list_quality_checks()
  tax <- catalog$check_id[catalog$ui_group == "taxonomic"]
  expect_equal(tax, "taxon_allowed_species")
  row <- catalog[catalog$check_id == "taxon_allowed_species", , drop = FALSE]
  expect_equal(row$label, "Allowed species")
  expect_equal(row$category, "taxonomic")
  expect_true(is.na(row$method_via))
})
