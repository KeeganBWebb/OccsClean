test_that("DecisionRegistry records and supersedes decisions", {
  reg <- DecisionRegistry$new()
  id1 <- reg$record(
    occsclean_id = "oc_000001",
    check_id = "coord_zero",
    action = "keep"
  )
  expect_equal(reg$n_entries(), 1)
  expect_equal(nrow(reg$effective()), 1)

  id2 <- reg$record(
    occsclean_id = "oc_000001",
    check_id = "coord_zero",
    action = "remove",
    note = "Confirmed bad"
  )
  expect_equal(reg$n_entries(), 2)
  eff <- reg$effective()
  expect_equal(nrow(eff), 1)
  expect_equal(eff$action, "remove")
  expect_equal(eff$decision_id, id2)

  log <- reg$log()
  expect_equal(log$superseded_by[log$decision_id == id1], id2)
  expect_equal(reg$removed_occsclean_ids(), "oc_000001")
})

test_that("record_many batch records and supersedes", {
  reg <- DecisionRegistry$new()
  findings <- tibble::tibble(
    occsclean_id = c("oc_1", "oc_2", "oc_1"),
    check_id = c("coord_zero", "coord_zero", "coord_sea"),
    finding = c("Z", "Z", "SEA")
  )
  expect_equal(reg$record_many(findings, "remove"), 3L)
  expect_equal(sort(reg$removed_occsclean_ids()), c("oc_1", "oc_2"))
  expect_equal(nrow(reg$effective()), 3L)

  expect_equal(reg$record_many(findings[1, , drop = FALSE], "keep"), 1L)
  # oc_1 still removed via the remaining coord_sea remove
  expect_equal(sort(reg$removed_occsclean_ids()), c("oc_1", "oc_2"))

  expect_equal(
    reg$record_many(findings[findings$occsclean_id == "oc_1", , drop = FALSE], "keep"),
    2L
  )
  expect_equal(reg$removed_occsclean_ids(), "oc_2")
})

test_that("correct requires fields_affected", {
  reg <- DecisionRegistry$new()
  expect_error(
    reg$record(
      occsclean_id = "oc_1",
      check_id = "x",
      action = "correct"
    ),
    "fields_affected"
  )
})
