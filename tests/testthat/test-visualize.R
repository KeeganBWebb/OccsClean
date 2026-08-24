test_that("build_visualize_points returns empty without coordinates", {
  occ <- tibble::tibble(occsclean_id = c("a", "b"), scientificName = c("x", "y"))
  pts <- build_visualize_points(occ)
  expect_equal(nrow(pts), 0)
  rec <- build_visualize_records(occ)
  expect_equal(nrow(rec), 2)
  expect_true(all(!rec$mappable))
})

test_that("build_visualize_points maps status from findings, keep, and removals", {
  occ <- tibble::tibble(
    occsclean_id = c("oc_ok", "oc_flag", "oc_keep", "oc_del", "oc_bad"),
    decimalLongitude = c(-105.5, -104, -103.5, -103, NA_real_),
    decimalLatitude = c(40, 39, 38.5, 38, 37),
    scientificName = c("a", "b", "c", "d", "e")
  )
  findings <- tibble::tibble(
    check_id = c("coord_zero", "coord_zero", "coord_zero", "coord_missing"),
    check_label = c(
      "At coordinates (0, 0)",
      "At coordinates (0, 0)",
      "At coordinates (0, 0)",
      "Missing coordinates"
    ),
    occsclean_id = c("oc_flag", "oc_keep", "oc_del", "oc_bad"),
    finding = c("COORD_ZERO", "COORD_ZERO", "COORD_ZERO", "COORD_MISSING")
  )
  decisions <- DecisionRegistry$new()
  decisions$record(
    occsclean_id = "oc_keep",
    check_id = "coord_zero",
    action = "keep",
    finding = "COORD_ZERO"
  )
  decisions$record(
    occsclean_id = "oc_del",
    check_id = "coord_zero",
    action = "remove",
    finding = "COORD_ZERO"
  )

  pts <- build_visualize_points(occ, findings = findings, decisions = decisions)
  expect_equal(nrow(pts), 4)
  expect_equal(pts$map_status[pts$occsclean_id == "oc_ok"], "OK")
  expect_equal(pts$map_status[pts$occsclean_id == "oc_flag"], "Flagged")
  expect_equal(pts$map_status[pts$occsclean_id == "oc_keep"], "Passed")
  expect_equal(pts$map_status[pts$occsclean_id == "oc_del"], "Failed")
  expect_equal(pts$n_flags[pts$occsclean_id == "oc_flag"], 1L)

  rec <- build_visualize_records(occ, findings = findings, decisions = decisions)
  expect_equal(nrow(rec), 5)
  expect_false(rec$mappable[rec$occsclean_id == "oc_bad"])
  expect_equal(rec$map_status[rec$occsclean_id == "oc_bad"], "Flagged")
})

test_that("summarize_visualize_view counts missing coords by selection", {
  occ <- tibble::tibble(
    occsclean_id = c("ok", "flag_map", "flag_miss", "del_miss"),
    decimalLongitude = c(-105, -104, NA_real_, NA_real_),
    decimalLatitude = c(40, 39, NA_real_, NA_real_)
  )
  findings <- tibble::tibble(
    check_id = c("coord_zero", "coord_missing", "coord_missing"),
    check_label = c("zero", "missing", "missing"),
    occsclean_id = c("flag_map", "flag_miss", "del_miss"),
    finding = c("Z", "M", "M")
  )
  decisions <- DecisionRegistry$new()
  decisions$record(
    occsclean_id = "del_miss",
    check_id = "coord_missing",
    action = "remove",
    finding = "M"
  )
  rec <- build_visualize_records(occ, findings, decisions)

  all_sum <- summarize_visualize_view(rec, "all")
  expect_equal(all_sum$n_records, 4)
  expect_equal(all_sum$n_mapped, 2)
  expect_equal(all_sum$n_missing_coords, 2)

  flagged <- summarize_visualize_view(rec, "flagged")
  expect_equal(flagged$n_records, 2)
  expect_equal(flagged$n_mapped, 1)
  expect_equal(flagged$n_missing_coords, 1)

  removed <- summarize_visualize_view(rec, "removed")
  expect_equal(removed$n_records, 1)
  expect_equal(removed$n_mapped, 0)
  expect_equal(removed$n_missing_coords, 1)
})

test_that("filter_visualize_points respects view modes", {
  pts <- tibble::tibble(
    occsclean_id = c("a", "b", "c", "d"),
    longitude = c(1, 2, 3, 4),
    latitude = c(1, 2, 3, 4),
    map_status = c("OK", "Flagged", "Passed", "Failed"),
    n_flags = c(0L, 1L, 1L, 1L),
    flag_labels = c("", "x", "x", "x"),
    taxon = c("a", "b", "c", "d"),
    mappable = c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(nrow(filter_visualize_points(pts, "all")), 4)
  expect_equal(nrow(filter_visualize_points(pts, "ok")), 1)
  expect_equal(nrow(filter_visualize_points(pts, "flagged")), 1)
  expect_equal(nrow(filter_visualize_points(pts, "kept")), 1)
  expect_equal(nrow(filter_visualize_points(pts, "removed")), 1)
  expect_equal(nrow(filter_visualize_points(pts, "ok_kept")), 2)
})

test_that("wrap expansion duplicates longitudes for display only", {
  pts <- tibble::tibble(
    occsclean_id = "a",
    longitude = 174,
    latitude = -41,
    map_status = "OK",
    n_flags = 0L,
    flag_labels = "",
    taxon = "x",
    mappable = TRUE
  )
  drawn <- expand_visualize_points_for_wrap(pts)
  expect_equal(nrow(drawn), 3)
  expect_setequal(drawn$longitude, c(174 - 360, 174, 174 + 360))
  expect_true(all(drawn$occsclean_id == "a"))
})
