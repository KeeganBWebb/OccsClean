test_that("run_app returns a shiny app object", {
  app <- OccsClean::run_app(launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
})
