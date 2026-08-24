test_that("mark and keep from review map update removed_occsclean_ids and keep", {
  reg <- DecisionRegistry$new()
  findings <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_1", "oc_2"),
    check_id = c("coord_zero", "coord_sea", "coord_zero"),
    finding = c("Z", "SEA", "Z")
  )

  n <- mark_record_for_deletion(reg, "oc_1", findings)
  expect_true(n >= 2L)
  expect_equal(reg$removed_occsclean_ids(), "oc_1")

  # Unflagged record can still be failed from the Review map
  mark_record_for_deletion(reg, "oc_ok", findings = NULL)
  expect_true("oc_ok" %in% reg$removed_occsclean_ids())

  n_keep <- keep_record_from_mapping(reg, "oc_1", findings = findings)
  expect_true(n_keep >= 1L)
  expect_false("oc_1" %in% reg$removed_occsclean_ids())
  expect_true("oc_ok" %in% reg$removed_occsclean_ids())

  # Flagged record can be marked keep without first deleting
  n_flag_keep <- keep_record_from_mapping(reg, "oc_2", findings = findings)
  expect_true(n_flag_keep >= 1L)
  eff <- reg$effective()
  expect_true(any(
    eff$occsclean_id == "oc_2" & eff$action == "keep"
  ))

  expect_equal(mapping_layer_id_record("oc_000012||3"), "oc_000012")
  expect_equal(mapping_layer_id_record("plain_id"), "plain_id")
})
