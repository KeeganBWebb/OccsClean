test_that("validate_occurrence_dataset finds expected fixture fields", {
  occ <- import_occurrences_csv(
    testthat::test_path("fixtures", "example_occurrences.csv")
  )$occ_raw

  v <- validate_occurrence_dataset(occ)
  expect_true(is_occ_validation(v))
  expect_equal(v$overall, "ready")
  expect_true(v$readiness$coordinate_checks)
  expect_true(v$readiness$temporal_checks)
  expect_true(v$readiness$taxonomic_checks)
  expect_equal(v$column_map$lon, "decimalLongitude")
  expect_equal(v$column_map$date, "eventDate")
  expect_equal(v$column_map$taxon, "scientificName")

  report <- format_validation_report(v)
  expect_true(grepl("Expected fields", report, fixed = TRUE))
  expect_true(grepl("decimalLongitude", report, fixed = TRUE))
  expect_true(grepl("Value checks", report, fixed = TRUE))
  expect_true(grepl("non-blank", report, fixed = TRUE))
  expect_false(grepl("Assess check availability", report, fixed = TRUE))
  expect_false(grepl("Column presence only", report, fixed = TRUE))
})

test_that("validation warns when coordinate columns are missing", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    scientificName = c("a", "b")
  )
  v <- validate_occurrence_dataset(occ)
  expect_equal(v$overall, "ready_with_warnings")
  expect_false(v$readiness$coordinate_checks)
  expect_false(v$readiness$temporal_checks)
  expect_true(v$readiness$taxonomic_checks)
})

test_that("validation fails when no expected occurrence columns exist", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    notes = c("README line one", "README line two")
  )
  v <- validate_occurrence_dataset(occ)
  expect_equal(v$overall, "failed")
  expect_false(v$readiness$coordinate_checks)
  expect_false(v$readiness$temporal_checks)
  expect_false(v$readiness$taxonomic_checks)
  expect_true(any(vapply(
    v$issues,
    function(x) identical(x$code, "no_expected_fields"),
    logical(1)
  )))

  report <- format_validation_report(v)
  expect_true(grepl("failed", report, fixed = TRUE))
  expect_true(grepl("does not look like", report, fixed = TRUE))
  expect_true(grepl("Value checks", report, fixed = TRUE))
  expect_true(grepl("No coordinate or date columns", report, fixed = TRUE))
  expect_false(grepl("Assess check availability", report, fixed = TRUE))
  expect_false(grepl("or alias", report, fixed = TRUE))
  expect_false(grepl("cannot run yet", report, fixed = TRUE))
  expect_false(grepl("Duplicate checks:", report, fixed = TRUE))
  expect_false(grepl("Occurrence checks:", report, fixed = TRUE))
})

test_that("manual column map resolves custom headers", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    my_lon = c(-105, -106),
    my_lat = c(40, 39),
    my_date = c("2020-01-01", "2021-02-02"),
    my_taxon = c("Pinus ponderosa", "Abies lasiocarpa")
  )
  v <- validate_occurrence_dataset(
    occ,
    column_map = list(
      lon = "my_lon",
      lat = "my_lat",
      date = "my_date",
      taxon = "my_taxon",
      basis_of_record = NULL,
      country = NULL
    ),
    skipped_fields = c("basis_of_record", "country"),
    manually_mapped = TRUE
  )
  expect_equal(v$overall, "ready_with_warnings")
  expect_true(v$manually_mapped)
  expect_equal(v$column_map$lon, "my_lon")
  expect_equal(v$column_map$lat, "my_lat")
  expect_equal(v$readiness$coordinate_checks, TRUE)
  expect_equal(v$readiness$taxonomic_checks, TRUE)
  report <- format_validation_report(v)
  expect_true(grepl("structure OK with warnings - manually mapped", report, fixed = TRUE))
  expect_true(grepl("basis_of_record: skipped", report, fixed = TRUE))
})

test_that("duplicate column mapping is rejected", {
  expect_error(
    normalize_column_map_input(
      list(
        lon = "x_coord",
        lat = "x_coord",
        date = COLUMN_MAP_SKIP,
        taxon = COLUMN_MAP_SKIP,
        basis_of_record = COLUMN_MAP_SKIP,
        country = COLUMN_MAP_SKIP
      ),
      c("occsclean_id", "x_coord", "y_coord")
    ),
    "Each column can only be mapped once"
  )
})

test_that("skipped fields are reported in validation output", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    my_lon = c(-105, -106),
    my_lat = c(40, 39)
  )
  v <- validate_occurrence_dataset(
    occ,
    column_map = list(
      lon = "my_lon",
      lat = "my_lat",
      date = NULL,
      taxon = NULL,
      basis_of_record = NULL,
      country = NULL
    ),
    skipped_fields = c("date", "taxon", "basis_of_record", "country"),
    manually_mapped = TRUE
  )
  expect_equal(v$overall, "ready_with_warnings")
  report <- format_validation_report(v)
  expect_true(grepl("occurrence_date: skipped", report, fixed = TRUE))
  expect_true(grepl("temporal checks unavailable", report, fixed = TRUE))
})

test_that("OccSession apply_column_mapping updates validation", {
  occ_path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "x_coord,y_coord,species_name,when",
      "1,2,Alpha,2020-01-01"
    ),
    occ_path
  )
  on.exit(unlink(occ_path), add = TRUE)

  s <- OccSession$new()
  s$import_csv(occ_path)
  expect_equal(s$get_validation()$overall, "failed")

  s$apply_column_mapping(list(
    lon = "x_coord",
    lat = "y_coord",
    taxon = "species_name",
    date = "when",
    basis_of_record = COLUMN_MAP_SKIP,
    country = COLUMN_MAP_SKIP
  ))
  v <- s$get_validation()
  expect_equal(v$overall, "ready_with_warnings")
  expect_true(v$manually_mapped)
  expect_equal(s$get_column_map()$lon, "x_coord")
  expect_equal(s$get_column_map()$taxon, "species_name")
})

test_that("OccSession stores validation on import", {
  s <- OccSession$new()
  s$import_csv(testthat::test_path("fixtures", "example_occurrences.csv"))
  v <- s$get_validation()
  expect_true(is_occ_validation(v))
  expect_equal(v$overall, "ready")
})
