context("Loading data into a jwmodel")

library(jwmodel)

test_that("test parameter file exists", {
  filepath <- system.file("extdata", "test_parameter_dataset.xlsx", package = "jwmodel")
  expect_true(filepath != "")
})

test_that("file loads without warnings", {
  filepath <- system.file("extdata", "test_parameter_dataset.xlsx", package = "jwmodel")
  expect_silent(myobj <- jwmodel::jwmodel())
  expect_silent(jwmodel::load_from_file(myobj, filepath))
})

# TODO ADD MORE TESTS