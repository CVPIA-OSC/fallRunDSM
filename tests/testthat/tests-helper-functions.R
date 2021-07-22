library(testthat)
library(fallRunDSM)
source("../../R/utils.R")
# Tests util functions and other helper functions
# Tests territory function
expected_territory <- c(0.0498944803729702, 0.138941944739835, 0.471083652829798, 0)

test_that('The territory_by_size funciton returns the expected output', {
  expect_equal(territory_by_size(), expected_territory)
})
