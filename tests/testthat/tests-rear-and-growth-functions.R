library(testthat)
library(fallRunDSM)
# Data to test
survival_rate <- structure(c(0.970687769248644, 0.926162679369688, 0.0292171120559602,
                             0.993172867577862, 0.982175316206257, 0.116773215187387, 0.996740770796031,
                             0.991441080750038, 0.217490944447593, 1, 1, 1), .Dim = 3:4)

juveniles <- structure(c(0, 11573957.5840594, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 3:4)
growth <- structure(c(0.0030986844080139, 0, 0, 0, 0.996809996864753, 0.508311476301429,
                      0, 0, 9.13187272335581e-05, 0.491688523698443, 0.813818359404653,
                      0, 0, 1.27897692436818e-13, 0.186181640595347, 1),
                    .Dim = c(4L,4L),
                    .Dimnames = list(c("s", "m", "l", "vl"), c("s", "m", "l",  "vl")))

# Tests rear fucntion
expected_rearing_output <- structure(c(0, 33217, 0, 0, 10685535, 0, 0, 979, 0, 0, 0, 0),
                                     .Dim = 3:4,
                                     .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The rearing function returns the expected values for year 1', {
  set.seed(2021)
  rearing <- rear(juveniles = juveniles,
                  survival_rate = survival_rate,
                  growth = growth)
  expect_equal(rearing,
               expected_rearing_output)
})

