library(testthat)
library(fallRunDSM)
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
betas <- as.matrix(survival_betas[, 3:16])
bp_survival_betas <- as.matrix(survival_betas[c(17, 22), c(3, 4, 5, 13, 14, 15)])

# Tests surv_juv_rear survival function
expected_surv_juv_rear <- list(inchannel = structure(c(0.373145308705724, 0.723372636446464,
                                                       0.846089404751765, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                               floodplain = structure(c(0.567765169512671, 0.835659795386719, 0.912083864792188, 1),
                                                      .Dim = c(1L, 4L),
                                                      .Dimnames = list( "Upper Sacramento River", c("s", "m", "l", "vl"))))

test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = fallRunDSM::params$contact_points[1],
                             prop_diversions = fallRunDSM::params$proportion_diverted[1],
                             total_diversions = fallRunDSM::params$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = fallRunDSM::params$weeks_flooded[, month, year][1],
                             ..surv_juv_rear_int = betas[1, 1],
                             ..surv_juv_rear_contact_points = -0.0067662,
                             ..surv_juv_rear_prop_diversions = -0.1755,
                             ..surv_juv_rear_total_diversions = -0.0004515),
               expected_surv_juv_rear)
})

# Tests surv_juv_delta survival function
expected_delta_juv_surv <- structure(c(0.0932457862245425, 1e-04, 0.0932457862245425, 1e-04,
                                       0.0932457862245425, 1e-04, 0.0932457862245425, 1),
                                     .Dim = c(2L, 4L),
                                     .Dimnames = list(c("North Delta", "South Delta"),
                                                      c("s",  "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(avg_temp = fallRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                              max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = fallRunDSM::params$delta_prop_high_predation,
                              contact_points = fallRunDSM::params$delta_contact_points,
                              prop_diverted = fallRunDSM::params$delta_proportion_diverted,
                              total_diverted = fallRunDSM::params$delta_total_diverted),
               expected_delta_juv_surv)
})

# Tests surv_juv_bypass survival function
expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1), .Dim = c(1L, 4L),
                                      .Dimnames = list( NULL, c("s", "m", "l", "vl")))

test_that('The bypass_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0,
                               ..surv_juv_bypass_int = bp_survival_betas[1, ]),
               expected_bypass_juv_surv)
})

# Tests migratory survival for lower mid sac fish survival function
expected_lms_mig_surv <- structure(c(0.991560590050647, 0.998053508596856,
                                     0.999072190870173, 0.999072190870173),
                                   .Dim = c(1L, 4L), .Dimnames = list(NULL,
                                                                   c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = test_data$upper_sacramento_flows[month, year],
                                         avg_temp = test_data$avg_temp[21, month, year],
                                         total_diversions = test_data$total_diverted[21],
                                         prop_diversions = test_data$proportion_diverted[21]),
               expected_lms_mig_surv)
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
expected_sac_delta_mig_surv <- c(s = 0.362285441652534, m = 0.44305372307621, l = 0.526441379341886,
                                 vl = 0.526441379341886)
test_that('The migratory_juv_surv function for sac delta returns the expected values for row one of year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = test_data$delta_inflow[month, year, ],
                                               avg_temp = test_data$avg_temp_delta[month, year, ],
                                               perc_diversions = test_data$delta_proportion_diverted * 100)[1,],
               expected_sac_delta_mig_surv)
})
