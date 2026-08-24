test_that("buffer distance parsing uses defaults and validates", {
  defaults <- coordinatecleaner_buffer_defaults_m()
  expect_equal(defaults$coord_capital, 10000L)
  expect_equal(defaults$coord_centroid, 1000L)
  expect_equal(defaults$coord_institution, 100L)
  expect_equal(defaults$coord_gbif, 1000L)

  expect_equal(parse_buffer_distance_m("", 1000L), 1000L)
  expect_equal(parse_buffer_distance_m("2500", 1000L), 2500L)
  expect_error(parse_buffer_distance_m("nope", 1000L), "non-negative")
  expect_null(parse_optional_buffer_distance_m(""))
  expect_equal(parse_optional_buffer_distance_m("50"), 50L)
})

test_that("assess settings round-trip includes location buffers", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  write_assess_settings(
    path = path,
    selected_checks = c("coord_capital", "coord_country"),
    settings = list(
      buffer_capital_m = "5000",
      buffer_country_m = "100"
    )
  )

  parsed <- read_assess_settings(path)
  expect_equal(parsed$settings$settings$buffer_capital_m, "5000")
  expect_equal(parsed$settings$settings$buffer_country_m, "100")
})
