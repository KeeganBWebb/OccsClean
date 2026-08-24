test_that("equal lon/lat check flags identical absolute values", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_ok", "oc_eq", "oc_abs"),
    decimalLongitude = c(-105.5, 10, -20),
    decimalLatitude = c(40.0, 10, 20),
    scientificName = letters[1:3]
  )

  res <- check_coord_equal(occ)
  expect_equal(res$status, "ok")
  expect_equal(res$engine, "CoordinateCleaner::cc_equ")
  expect_setequal(res$findings$occsclean_id, c("oc_eq", "oc_abs"))
  expect_false("oc_ok" %in% res$findings$occsclean_id)
})

test_that("GBIF headquarters check flags Copenhagen office area", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_gbif", "oc_elsewhere"),
    decimalLongitude = c(12.58, -105.5),
    decimalLatitude = c(55.67, 40.0),
    scientificName = c("a", "b")
  )

  res <- check_coord_gbif(occ)
  expect_true(is_occ_check_result(res))
  expect_true(res$status %in% c("ok", "error"))
  if (identical(res$status, "ok")) {
    expect_equal(res$engine, "CoordinateCleaner::cc_gbif")
    expect_true("oc_gbif" %in% res$findings$occsclean_id)
    expect_false("oc_elsewhere" %in% res$findings$occsclean_id)
  }
})

test_that("country mismatch check skips without country column", {
  occ <- tibble::tibble(
    occsclean_id = "oc_1",
    decimalLongitude = -105.5,
    decimalLatitude = 40.0
  )
  res <- check_coord_country(occ)
  expect_equal(res$status, "skipped")
})

test_that("country mismatch check flags wrong country when available", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_match", "oc_mismatch"),
    decimalLongitude = c(-105.5, -105.5),
    decimalLatitude = c(40.0, 40.0),
    countryCode = c("US", "BR")
  )

  res <- check_coord_country(occ)
  expect_true(is_occ_check_result(res))
  expect_true(res$status %in% c("ok", "error", "skipped"))
  if (identical(res$status, "ok")) {
    expect_equal(res$engine, "CoordinateCleaner::cc_coun")
    expect_true("oc_mismatch" %in% res$findings$occsclean_id)
    expect_false("oc_match" %in% res$findings$occsclean_id)
  }
})

test_that("CoordinateCleaner ocean check flags open-ocean points", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_land", "oc_sea"),
    decimalLongitude = c(-105.5, -40),
    decimalLatitude = c(40.0, 0),
    scientificName = c("Pinus ponderosa", "Fake oceanus")
  )

  res <- check_coord_sea(occ)
  expect_true(is_occ_check_result(res))
  expect_equal(res$status, "ok")
  expect_equal(res$engine, "CoordinateCleaner::cc_sea")
  expect_true("oc_sea" %in% res$findings$occsclean_id)
  expect_false("oc_land" %in% res$findings$occsclean_id)
})

test_that("on-land check flags terrestrial points for marine review", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_land", "oc_sea"),
    decimalLongitude = c(-105.5, -40),
    decimalLatitude = c(40.0, 0),
    scientificName = c("Fake terrestrius", "Fake oceanus")
  )

  res <- check_coord_land(occ)
  expect_true(is_occ_check_result(res))
  expect_equal(res$status, "ok")
  expect_equal(res$engine, "CoordinateCleaner::cc_sea (inverted)")
  expect_true("oc_land" %in% res$findings$occsclean_id)
  expect_false("oc_sea" %in% res$findings$occsclean_id)
})

test_that("CoordinateCleaner checks are registered with method attribution", {
  catalog <- list_quality_checks()
  cc_ids <- c(
    "coord_sea", "coord_land", "coord_centroid", "coord_capital",
    "coord_institution", "coord_equal", "coord_gbif", "coord_country"
  )
  expect_true(all(cc_ids %in% catalog$check_id))
  cc_rows <- catalog[catalog$check_id %in% cc_ids, ]
  expect_true(all(cc_rows$method_via == "CoordinateCleaner"))
  tip <- assess_check_tooltip(cc_rows$description[[1]], cc_rows$method_via[[1]])
  expect_true(grepl("Method via CoordinateCleaner\\.$", tip))
})

test_that("CoordinateCleaner centroid check returns a structured result", {
  occ <- tibble::tibble(
    occsclean_id = sprintf("oc_%02d", 1:5),
    decimalLongitude = c(-105.5, -47.92, 10, -70, 120),
    decimalLatitude = c(40.0, -15.78, 50, -30, 30),
    scientificName = letters[1:5]
  )
  res <- check_coord_centroid(occ)
  expect_true(is_occ_check_result(res))
  expect_true(res$status %in% c("ok", "error"))
  if (identical(res$status, "ok")) {
    expect_true(res$summary$n_checked >= 1)
  }
})
