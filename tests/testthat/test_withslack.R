context("Run with option to use 'slack' contraints")

library(jwmodel)

test_that("file optimises without warnings when slack_constraints option = TRUE", {
  filepath <- system.file("extdata", "test_parameter_dataset.xlsx", package = "jwmodel")
  myobj <- jwmodel::jwmodel()
  myobj <- jwmodel::load_from_file(myobj, filepath)
  myobj <- jwmodel::initialise(myobj)
  expect_silent(
    myobj <- jwmodel::optimise(myobj, slack_constraints = TRUE)
  )
  
})

test_that("file optimises identically with & without slack option", {
  filepath <- system.file("extdata", "test_parameter_dataset.xlsx", package = "jwmodel")
  myobj <- jwmodel::jwmodel()
  myobj <- jwmodel::load_from_file(myobj, filepath)
  myobj <- jwmodel::initialise(myobj)
  
  myobj <- jwmodel::optimise(myobj, slack_constraints = FALSE)
  resource_outputs_without_slack <- myobj$outputs$resource_output
  alloc_outputs_without_slack <- myobj$outputs$allocation_output
  
  myobj <- jwmodel::optimise(myobj, slack_constraints = TRUE)
  resource_outputs_with_slack <- myobj$outputs$resource_output
  alloc_outputs_with_slack <- myobj$outputs$allocation_output
  
  # example dataset is solvable without using slack, therefore the solution
  # should be identical whether slack is enabled or not
  
  # compare resource outputs (check each field individually)
  expect_equal(resource_outputs_without_slack$Year, resource_outputs_with_slack$Year)
  expect_equal(resource_outputs_without_slack$Judge, resource_outputs_with_slack$Judge)
  expect_equal(resource_outputs_without_slack$status, resource_outputs_with_slack$status)
  expect_equal(resource_outputs_without_slack$FTE, resource_outputs_with_slack$FTE)
  
  # compare allocation outputs (check each field individually)
  expect_equal(alloc_outputs_without_slack$Year, alloc_outputs_with_slack$Year)
  expect_equal(alloc_outputs_without_slack$Jurisdiction, alloc_outputs_with_slack$Jurisdiction)
  expect_equal(alloc_outputs_without_slack$Judge, alloc_outputs_with_slack$Judge)
  expect_equal(alloc_outputs_without_slack$Category, alloc_outputs_with_slack$Category)
  expect_equal(alloc_outputs_without_slack$Description, alloc_outputs_with_slack$Description)
  expect_equal(alloc_outputs_without_slack$Allocated, alloc_outputs_with_slack$Allocated)
  expect_equal(alloc_outputs_without_slack$`Avg Sitting Day Cost`, alloc_outputs_with_slack$`Avg Sitting Day Cost`)
  expect_equal(alloc_outputs_without_slack$`Avg Sitting Days`, alloc_outputs_with_slack$`Avg Sitting Days`)
  expect_equal(alloc_outputs_without_slack$`Min Sitting Days`, alloc_outputs_with_slack$`Min Sitting Days`)
  expect_equal(alloc_outputs_without_slack$`Max Sitting Days`, alloc_outputs_with_slack$`Max Sitting Days`)
  expect_equal(alloc_outputs_without_slack$`Avg Sitting Days`, alloc_outputs_with_slack$`Avg Sitting Days`)
  expect_equal(alloc_outputs_without_slack$`Total Sitting Days`, alloc_outputs_with_slack$`Total Sitting Days`)
  expect_equal(alloc_outputs_without_slack$`Total Fee Cost`, alloc_outputs_with_slack$`Total Fee Cost`)
  
})

# TODO add test using example where slack IS required to solve the optimisation