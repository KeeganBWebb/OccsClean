fixture_path <- function() {
  testthat::test_path("fixtures", "example_occurrences.csv")
}

fixture_tsv_path <- function() {
  testthat::test_path("fixtures", "example_occurrences.tsv")
}

test_that("import_occurrences_csv adds occsclean_id and preserves rows", {
  imported <- import_occurrences_csv(fixture_path())
  occ <- imported$occ_raw

  expect_true("occsclean_id" %in% names(occ))
  expect_equal(nrow(occ), 5)
  expect_equal(occ$occsclean_id, sprintf("oc_%06d", 1:5))
  expect_equal(names(occ)[[1]], "occsclean_id")
  expect_equal(imported$meta$n_rows, 5)
  expect_equal(imported$meta$delimiter, "comma")
  expect_equal(imported$meta$occsclean_id_source, "generated")
  expect_true(imported$meta$original_file_untouched)
})

test_that("import never adopts or renames a user record_id column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c(
      "record_id,scientificName,decimalLongitude,decimalLatitude",
      "gbif-111,Pinus ponderosa,-105.5,40.0",
      "gbif-222,Abies lasiocarpa,-106.1,39.2"
    ),
    path
  )
  occ <- import_occurrences_csv(path)$occ_raw
  expect_true("record_id" %in% names(occ))
  expect_equal(occ$record_id, c("gbif-111", "gbif-222"))
  expect_equal(occ$occsclean_id, c("oc_000001", "oc_000002"))
  expect_false(identical(occ$record_id, occ$occsclean_id))
})

test_that("strip_occsclean_columns removes internal key from downloads", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2"),
    record_id = c("user-a", "user-b"),
    scientificName = c("a", "b")
  )
  stripped <- strip_occsclean_columns(occ)
  expect_false("occsclean_id" %in% names(stripped))
  expect_equal(stripped$record_id, c("user-a", "user-b"))
  expect_equal(stripped$scientificName, c("a", "b"))
})

test_that("import detects tab-delimited GBIF-style files", {
  imported <- import_occurrences_csv(
    fixture_tsv_path(),
    source_name = "occurrence.txt"
  )
  expect_equal(imported$meta$delimiter, "tab")
  expect_equal(imported$meta$source_name, "occurrence.txt")
  expect_equal(nrow(imported$occ_raw), 2)
  expect_gt(ncol(imported$occ_raw), 2)
  expect_true("decimalLongitude" %in% names(imported$occ_raw))
})

test_that("import does not alter the source file contents", {
  path <- fixture_path()
  before <- readLines(path)
  invisible(import_occurrences_csv(path))
  after <- readLines(path)
  expect_identical(before, after)
})

test_that("OccSession stores immutable raw copy semantics", {
  s <- OccSession$new()
  expect_false(s$has_data())

  s$import_csv(fixture_path())
  expect_true(s$has_data())
  expect_equal(s$get_meta()$n_rows, 5)

  raw1 <- s$get_occ_raw()
  raw1$scientificName[[1]] <- "MUTATED"
  raw2 <- s$get_occ_raw()
  expect_false(identical(raw1$scientificName[[1]], raw2$scientificName[[1]]))
  expect_equal(raw2$scientificName[[1]], "Pinus ponderosa")

  work <- s$get_occ_working()
  expect_equal(nrow(work), nrow(raw2))
})

test_that("OccSession caches check results and clears on re-import", {
  s <- OccSession$new()
  s$import_csv(fixture_path())

  res <- new_occ_check_result(
    check_id = "coord_zero",
    label = "Coordinates at (0,0)",
    category = "coordinate",
    findings = empty_findings()
  )
  s$set_check_result(res)
  expect_equal(names(s$get_assessment()), "coord_zero")

  s$get_decisions()$record(
    occsclean_id = "oc_000001",
    check_id = "coord_zero",
    action = "keep"
  )
  expect_equal(s$get_decisions()$n_entries(), 1)

  s$import_csv(fixture_path())
  expect_equal(length(s$get_assessment()), 0)
  expect_equal(s$get_decisions()$n_entries(), 0)
})
