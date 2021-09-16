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

# Tests rear function ----------------------------------------------------------
# Stochastic
expected_rearing_output <- structure(c(0, 33217, 0, 0, 10685535, 0, 0, 979, 0, 0, 0, 0),
                                     .Dim = 3:4,
                                     .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The rearing function returns the expected values for year 1', {
  set.seed(2021)
  rearing <- rear(juveniles = juveniles,
                  survival_rate = survival_rate,
                  growth = growth,
                  stochastic = TRUE)
  expect_equal(rearing,
               expected_rearing_output)
})
# Deterministic
expected_rearing_output_det <- structure(c(0, 33216, 0, 0, 10685173, 0, 0, 979, 0, 0, 0, 0),
                                         .Dim = 3:4,
                                         .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The rearing function returns the expected values for year 1 stochastic = FALSE', {
  rearing <- rear(juveniles = juveniles,
                  survival_rate = survival_rate,
                  growth = growth,
                  stochastic = FALSE)
  expect_equal(rearing,
               expected_rearing_output_det)
})

# Tests growth functions
# In channel growth ------------------------------------------------------------
expected_growth <- structure(c(0.0030986844080139, 0, 0, 0, 0.996809996864753, 0.508311476301429,
                               0, 0, 9.13187272335581e-05, 0.491688523698443, 0.813818359404653,
                               0, 0, 1.27897692436818e-13, 0.186181640595347, 1),
                             .Dim = c(4L,4L),
                             .Dimnames = list(c("s", "m", "l", "vl"), c("s", "m", "l",
                                                                                                                                           "vl")))
test_that('The growth() function returns the expected value', {
  growth <- growth()
  expect_equal(growth, expected_growth)
})

# floodplain growth
expected_growth_floodplain <- structure(c(0.0023943318247171, 0, 0, 0, 0.89449258142548, 0.389276740027636,
                                          0, 0, 0.103074557012702, 0.606275800235655, 0.63188508414161,
                                          0, 3.85297371013615e-05, 0.00444745973670987, 0.36811491585839,
                                          1, 0.00168997924142031, 0, 0, 0, 0.792175165986207, 0.270242003753842,
                                          0, 0, 0.20605779529817, 0.720863076772866, 0.449951808878567,
                                          0, 7.70594742027231e-05, 0.00889491947329184, 0.550048191121433,
                                          1, 0.000985626658123508, 0, 0, 0, 0.689857750546934, 0.151207267480049,
                                          0, 0, 0.309041033583638, 0.835450353310077, 0.268018533615524,
                                          0, 0.000115589211304085, 0.0133423792098738, 0.731981466384476,
                                          1, 0.00028127407482671, 0, 0, 0, 0.587540335107661, 0.0321725312062555,
                                          0, 0, 0.412024271869107, 0.950037629847289, 0.0860852583524811,
                                          0, 0.000154118948405446, 0.0177898389464558, 0.913914741647519,
                                          1), .Dim = c(4L, 4L, 4L), .Dimnames = list(c("s", "m", "l", "vl"
                                          ), c("s", "m", "l", "vl"), c("1 week flooded", "2 weeks flooded",
                                                                       "3 weeks flooded", "4 weeks flooded")))

test_that('The growth_floodplain() function returns the expected value', {
  growth_floodplain <- growth_floodplain()
  expect_equal(growth_floodplain, expected_growth_floodplain)
})
