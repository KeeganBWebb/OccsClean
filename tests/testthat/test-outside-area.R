test_that("outside-area check flags points beyond the polygon", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(0, 1),
      c(1, 1),
      c(1, 0),
      c(0, 0)
    ))),
    crs = 4326
  )

  occ <- tibble::tibble(
    occsclean_id = c("oc_in", "oc_out", "oc_miss"),
    decimalLongitude = c(0.5, 10, NA_real_),
    decimalLatitude = c(0.5, 10, NA_real_)
  )

  res <- check_outside_area(
    occ,
    params = list(area_geom = poly, area_source = "unit-test")
  )
  expect_equal(res$status, "ok")
  expect_equal(res$engine, "sf")
  expect_equal(res$summary$n_checked, 2)
  expect_equal(res$findings$occsclean_id, "oc_out")
  expect_false("oc_in" %in% res$findings$occsclean_id)
  expect_false("oc_miss" %in% res$findings$occsclean_id)
  expect_true(grepl("distance_m=", res$findings$evidence[[1]], fixed = TRUE))
})

test_that("outside-area distance threshold uses nearest meters", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(0, 0.01),
      c(0.01, 0.01),
      c(0.01, 0),
      c(0, 0)
    ))),
    crs = 4326
  )

  # ~0.002 deg east of the east edge (~220 m); within 1000 m, beyond hard edge
  occ <- tibble::tibble(
    occsclean_id = c("oc_near", "oc_far"),
    decimalLongitude = c(0.012, 0.05),
    decimalLatitude = c(0.005, 0.005)
  )

  hard <- check_outside_area(occ, params = list(area_geom = poly))
  expect_setequal(hard$findings$occsclean_id, c("oc_near", "oc_far"))

  soft <- check_outside_area(
    occ,
    params = list(area_geom = poly, outside_distance_m = 1000)
  )
  expect_equal(soft$status, "ok")
  expect_equal(soft$params_used$outside_distance_m, 1000)
  expect_false("oc_near" %in% soft$findings$occsclean_id)
  expect_true("oc_far" %in% soft$findings$occsclean_id)
  expect_true(grepl("distance_m=", soft$findings$evidence[[1]], fixed = TRUE))
})

test_that("outside-area check skips without a polygon", {
  occ <- tibble::tibble(
    occsclean_id = "oc_1",
    decimalLongitude = 0.5,
    decimalLatitude = 0.5
  )
  res <- check_outside_area(occ)
  expect_equal(res$status, "skipped")
})

test_that("outside-area check is registered with sf attribution", {
  catalog <- list_quality_checks()
  row <- catalog[catalog$check_id == "coord_outside_area", , drop = FALSE]
  expect_equal(nrow(row), 1)
  expect_equal(row$method_via, "sf")
  tip <- assess_check_tooltip(row$description[[1]], row$method_via[[1]])
  expect_true(grepl("Method via sf\\.$", tip))
})

test_that("parse_outside_distance_m treats blank as hard boundary", {
  expect_null(parse_outside_distance_m(""))
  expect_null(parse_outside_distance_m(NULL))
  expect_null(parse_outside_distance_m(0))
  expect_equal(parse_outside_distance_m("500"), 500)
  expect_error(parse_outside_distance_m("abc"), "number")
})

test_that("OccSession stores study area for visualize", {
  s <- OccSession$new()
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
    ))),
    crs = 4326
  )
  s$set_study_area(poly, source = "test.shp")
  got <- s$get_study_area()
  expect_false(is.null(got))
  expect_equal(got$source, "test.shp")
  expect_true(inherits(got$geom, "sfc"))
  s$clear_study_area()
  expect_null(s$get_study_area())
})

test_that("invalid shapefile geometry is repaired with a clear note", {
  # Self-intersecting bow-tie polygon
  bad <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(1, 1),
      c(0, 1),
      c(1, 0),
      c(0, 0)
    ))),
    crs = 4326
  )
  s2_was_on <- isTRUE(sf::sf_use_s2())
  if (s2_was_on) {
    sf::sf_use_s2(FALSE)
  }
  expect_false(all(sf::st_is_valid(bad)))
  if (s2_was_on) {
    sf::sf_use_s2(TRUE)
  }

  prepared <- prepare_area_polygon(bad)
  expect_true(prepared$geometry_repaired)
  expect_true(all(sf::st_is_valid(prepared$geom)))
  expect_true(grepl("INVALID geometry", prepared$repair_message, fixed = TRUE))
  expect_true(grepl("st_make_valid", prepared$repair_message, fixed = TRUE))
  expect_true(grepl("file on disk was not changed", prepared$repair_message, fixed = TRUE))

  # Valid polygons must not claim a repair happened
  good <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
    ))),
    crs = 4326
  )
  ok <- prepare_area_polygon(good)
  expect_false(ok$geometry_repaired)
  expect_null(ok$repair_message)
})

test_that("dissolve fallback keeps a usable multipolygon study area", {
  polys <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
    ))),
    sf::st_polygon(list(rbind(
      c(2, 2), c(2, 3), c(3, 3), c(3, 2), c(2, 2)
    ))),
    crs = 4326
  )
  dissolved <- dissolve_area_geom(polys)
  expect_false(dissolved$used_fallback)
  expect_true(all(sf::st_is_valid(dissolved$geom)))
  expect_equal(length(dissolved$geom), 1L)

  # Undissolved multipolygon (combine fallback shape) still works for distances
  mp <- sf::st_sfc(
    sf::st_multipolygon(list(
      list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))),
      list(rbind(c(2, 2), c(2, 3), c(3, 3), c(3, 2), c(2, 2)))
    )),
    crs = 4326
  )
  occ <- tibble::tibble(
    occsclean_id = c("in", "out"),
    decimalLongitude = c(0.5, 10),
    decimalLatitude = c(0.5, 10)
  )
  res <- check_outside_area(
    occ,
    params = list(area_geom = mp, area_source = "multipolygon-fallback")
  )
  expect_equal(res$status, "ok")
  expect_equal(res$findings$occsclean_id, "out")
  expect_false("in" %in% res$findings$occsclean_id)
})

test_that("yellow-eyed penguin sample shapefile prepares across the dateline", {
  path <- file.path(
    "C:/Users/bubby/Desktop/OccsClean/data-raw/shapefile",
    "Yellow-Eyed_Penguin_-_Annual_Distribution.shp"
  )
  skip_if_not(file.exists(path))
  area <- sf::st_read(path, quiet = TRUE)
  prepared <- prepare_area_polygon(area)
  expect_true(inherits(prepared$geom, "sfc"))
  expect_equal(length(prepared$geom), 1L)
  expect_true(all(sf::st_is_valid(prepared$geom)))

  # Must split into Pacific parts — not one long-way-around world polygon
  parts <- suppressWarnings(sf::st_cast(prepared$geom, "POLYGON"))
  spans <- vapply(seq_along(parts), function(i) {
    bb <- sf::st_bbox(parts[i])
    as.numeric(bb["xmax"]) - as.numeric(bb["xmin"])
  }, numeric(1))
  expect_true(all(spans < 180))

  # Wellington / Chatham inside; Sydney outside
  pts <- sf::st_sfc(
    sf::st_point(c(174.78, -41.29)),
    sf::st_point(c(-176.5, -43.9)),
    sf::st_point(c(151.21, -33.87)),
    crs = 4326
  )
  hit <- lengths(sf::st_intersects(pts, prepared$geom)) > 0
  expect_true(hit[[1]])
  expect_true(hit[[2]])
  expect_false(hit[[3]])
})
