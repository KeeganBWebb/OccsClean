test_that("occsclean_used_packages lists DESCRIPTION Imports of interest", {
  pkgs <- occsclean_used_packages()
  expect_true(all(c("package", "role", "used_for") %in% names(pkgs)))
  expect_true("CoordinateCleaner" %in% pkgs$package)
  expect_true("sf" %in% pkgs$package)
  expect_true("shiny" %in% pkgs$package)
  expect_equal(anyDuplicated(pkgs$package), 0L)
})

test_that("occsclean_package_versions adds version column", {
  vers <- occsclean_package_versions()
  expect_true("version" %in% names(vers))
  expect_true(all(nzchar(vers$version)))
})

test_that("occsclean_package_citations returns text for key backends", {
  txt <- occsclean_package_citations(c("CoordinateCleaner", "sf"))
  expect_type(txt, "character")
  expect_true(grepl(
    "Suggested citations for key backend packages used by OccsClean",
    txt,
    fixed = TRUE
  ))
  expect_true(grepl("CoordinateCleaner", txt, fixed = TRUE))
  expect_true(grepl("sf", txt, fixed = TRUE))
})
