test_that("assess settings round-trip through JSON", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  write_assess_settings(
    path = path,
    selected_checks = c(
      "occ_duplicates",
      "taxon_allowed_species",
      "coord_outside_area"
    ),
    settings = list(
      allowed_species = c("Pinus ponderosa", "Quercus alba"),
      outside_distance_m = "500",
      area_source = "nz.shp, nz.shx, nz.dbf",
      area_geom = sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
        ))),
        crs = 4326
      )
    )
  )

  raw_txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("POLYGON", raw_txt, fixed = TRUE))
  expect_false(grepl("area_geom", raw_txt, fixed = TRUE))
  expect_true(grepl("nz.shp", raw_txt, fixed = TRUE))
  expect_true(grepl("Pinus ponderosa", raw_txt, fixed = TRUE))

  parsed <- read_assess_settings(path)
  expect_equal(parsed$warnings, character())
  expect_equal(
    unlist_character(parsed$settings$selected_checks),
    c("occ_duplicates", "taxon_allowed_species", "coord_outside_area")
  )
  expect_equal(
    unlist_character(parsed$settings$settings$allowed_species),
    c("Pinus ponderosa", "Quercus alba")
  )
  expect_equal(parsed$settings$settings$outside_distance_m, "500")
  expect_equal(
    parsed$settings$settings$area_source,
    "nz.shp, nz.shx, nz.dbf"
  )
  expect_null(parsed$settings$settings$area_geom)
})

test_that("assess settings drops unknown check ids with a warning", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  payload <- build_assess_settings(
    selected_checks = c("occ_duplicates", "not_a_real_check"),
    settings = list(date_min = "2000-01-01")
  )
  # Force an unknown id into the written file
  payload$selected_checks <- as.list(c("occ_duplicates", "not_a_real_check"))
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)

  parsed <- read_assess_settings(path)
  expect_equal(unlist_character(parsed$settings$selected_checks), "occ_duplicates")
  expect_true(any(grepl("not_a_real_check", parsed$warnings, fixed = TRUE)))
  expect_equal(parsed$settings$settings$date_min, "2000-01-01")
})

test_that("build_assess_settings rejects geometry silently", {
  built <- build_assess_settings(
    selected_checks = "coord_outside_area",
    settings = list(
      area_source = "test.shp",
      area_geom = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326),
      area_sf = "should_drop"
    )
  )
  expect_equal(built$schema, "occsclean_assess_settings")
  expect_equal(built$schema_version, 1L)
  expect_equal(built$settings$area_source, "test.shp")
  expect_null(built$settings$area_geom)
  expect_null(built$settings$area_sf)
})

test_that("assess_checks_by_ui_group partitions check ids", {
  by_g <- assess_checks_by_ui_group(c(
    "occ_duplicates",
    "date_future",
    "coord_sea",
    "taxon_allowed_species"
  ))
  expect_equal(by_g$basics, "occ_duplicates")
  expect_equal(by_g$dates, "date_future")
  expect_equal(by_g$suspicious_locations, "coord_sea")
  expect_equal(by_g$taxonomic, "taxon_allowed_species")
})

test_that("empty settings serialize as a JSON object", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_assess_settings(
    path = path,
    selected_checks = c("occ_duplicates", "coord_missing"),
    settings = list()
  )
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl('"settings": \\{\\}', txt) || grepl('"settings":{}', txt))
  expect_false(grepl('"settings": \\[\\]', txt))
})
