context("Initialise and optimise jwmodel without warnings")

library(jwmodel)

test_that("file initialises without warnings", {
  filepath <- system.file("extdata", "test_parameter_dataset.xlsx", package = "jwmodel")
  myobj <- jwmodel::jwmodel()
  myobj <- jwmodel::load_from_file(myobj, filepath)
  expect_silent(myobj <- jwmodel::initialise(myobj))
})

test_that("file optimises without warnings", {
  filepath <- system.file("extdata", "test_parameter_dataset.xlsx", package = "jwmodel")
  myobj <- jwmodel::jwmodel()
  myobj <- jwmodel::load_from_file(myobj, filepath)
  myobj <- jwmodel::initialise(myobj)
  expect_silent(myobj <- jwmodel::optimise(myobj))
})

# TODO ADD MORE TESTS