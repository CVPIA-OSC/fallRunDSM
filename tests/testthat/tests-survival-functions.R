library(testthat)
library(fallRunDSM)
# tests for survival functions
# Lists inputs to use in testing
test_data <- fallRunDSM::load_baseline_data()
year <- 1
month <- 9
aveT20 <- rbinom(31, 1, boot::inv.logit(-14.32252 + 0.72102 * test_data$avg_temp[ , month , year]))
maxT25 <- rbinom(31, 1, boot::inv.logit(-23.1766 + 1.4566 * test_data$avg_temp[ , month, year]))
aveT20D <- rbinom(2, 1, boot::inv.logit(-18.30017 + 0.96991 * test_data$avg_temp_delta[month, year, ]))
maxT25D <- rbinom(2, 1, boot::inv.logit(-157.537 + 6.998 * test_data$avg_temp_delta[month, year, ]))
high_predation <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L)
ws_strand <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
               0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L)
betas <- as.matrix(survival_betas[, 3:16])
bp_survival_betas <- as.matrix(survival_betas[c(17, 22), c(3, 4, 5, 13, 14, 15)])

# Tests surv_juv_rear survival function
expected_surv_juv_rear <- list(inchannel = cbind(s = 0.313393973985376,
                                                 m = 0.667233816948951,
                                                 l = 0.808253327118087,
                                                 vl = 1),
                               floodplain = structure(c(0.520325456943001, 0.802578450626017,
                                                        0.89145699095444, 1),
                                                      .Dim = c(1L, 4L),
                                                      .Dimnames = list("Upper Sacramento River",
                                                                       c("s", "m", "l", "vl"))))
test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = test_data$contact_points[1],
                             prop_diversions = test_data$proportion_diverted[1],
                             total_diversions = test_data$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = test_data$weeks_flooded[, month, year][1],
                             ..surv_juv_rear_int = betas[1, 1],
                             ..surv_juv_rear_contact_points = -0.0067662,
                             ..surv_juv_rear_prop_diversions = -0.1755,
                             ..surv_juv_rear_total_diversions = -0.0004515),
               expected_surv_juv_rear)
})

# Tests surv_juv_delta survival function
expected_delta_juv_surv <- structure(c(0.0133399021276623, 1e-04, 0.0560639245646203, 1e-04,
                                       0.111000112182202, 1e-04, 1, 1),
                                     .Dim = c(2L, 4L),
                                     .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = test_data$delta_prop_high_predation,
                              contact_points = test_data$delta_contact_points,
                              prop_diverted = test_data$delta_proportion_diverted,
                              total_diverted = test_data$delta_total_diverted),
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
