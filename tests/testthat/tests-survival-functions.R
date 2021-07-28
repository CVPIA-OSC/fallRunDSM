library(testthat)
library(springRunDSM)
# tests for survival functions
# Lists inputs to use in testing
year <- 1
month <- 9
aveT20 <- c(0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L,
            0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 1L)
maxT25 <- c(0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L,
            1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L)
aveT20D <- c(1L, 1L)
maxT25D <- 0:1
high_predation <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L)
ws_strand <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
               0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L)


# Tests surv_juv_rear survival function
expected_surv_juv_rear <- list(inchannel = structure(c(0.12800137398749, 0.392039610266673,
                                                       0.575481904004287, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL,  c("s", "m", "l", "vl"))),
                               floodplain = structure(c(0.279466957899942, 0.585031205768225, 0.733651503532796, 1),
                                                      .Dim = c(1L, 4L),
                                                      .Dimnames = list("Upper Sacramento River", c("s", "m", "l", "vl"))))
test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = springRunDSM::params$contact_points[1],
                             prop_diversions = springRunDSM::params$proportion_diverted[1],
                             total_diversions = springRunDSM::params$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = springRunDSM::params$weeks_flooded[, month, year][1],
                             ..surv_juv_rear_int = .1,
                             ..surv_juv_rear_contact_points = -0.0067662,
                             ..surv_juv_rear_prop_diversions = -0.1755,
                             ..surv_juv_rear_total_diversions = -0.0004515),
               expected_surv_juv_rear)
})

# Tests surv_juv_delta survival function
expected_delta_juv_surv <- structure(c(0.0932457862245425, 1e-04, 0.0932457862245425, 1e-04,
                                       0.0932457862245425, 1e-04, 0.0932457862245425, 1),
                                     .Dim = c(2L, 4L),
                                     .Dimnames = list(c("North Delta", "South Delta"), c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(avg_temp = springRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                              max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = springRunDSM::params$delta_prop_high_predation,
                              contact_points = springRunDSM::params$delta_contact_points,
                              prop_diverted = springRunDSM::params$delta_proportion_diverted,
                              total_diverted = springRunDSM::params$delta_total_diverted),
               expected_delta_juv_surv)
})

# Tests surv_juv_bypass survival function
expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1), .Dim = c(1L, 4L),
                                      .Dimnames = list( NULL, c("s", "m", "l", "vl")))

test_that('The bypass_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0,
                               ..surv_juv_bypass_int = .1),
               expected_bypass_juv_surv)
})

# Tests migratory survival for lower mid sac fish survival function
expected_lms_mig_surv <- c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189)

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = springRunDSM::params$upper_sacramento_flows[month, year])
,               expected_lms_mig_surv)
})


# Tests migratory survival for san joaquin fish survival function
expected_lms_mig_surv <- structure(c(0.0293122307513563, 0.117118990875781, 0.218061322644411,
                                     0.218061322644411), .Dim = c(1L, 4L),
                                   .Dimnames = list(NULL,
                                                    c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_lms_mig_surv)
})

# Tests migratory survival for sac delta outmigration survival function
expected_sac_delta_mig_surv <- structure(c(0.361381001327841, 0.440065384516238, 0.521492554744754,
                                           0.521492554744754),
                                         .Dim = c(1L, 4L),
                                         .Dimnames = list(NULL, c("s", "m", "l", "vl")))
test_that('The migratory_juv_surv function for sac delta returns the expected values for row one of year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = springRunDSM::params$delta_inflow[month, year, "North Delta"],
                                               avg_temp = springRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                                               perc_diversions = springRunDSM::params$delta_proportion_diverted[month, year, "North Delta"] * 100),
               expected_sac_delta_mig_surv)
})
