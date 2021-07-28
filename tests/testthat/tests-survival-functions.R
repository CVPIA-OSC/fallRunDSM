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

# tests the surv_juv_outmigration_delta function
expected_surv_juv_outmigration <- structure(c(0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25,
                                              3.67469661043849e-14, 0.266668614822945, 2.26283033759458e-26,
                                              1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945,
                                              2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14,
                                              0.373914118050784, 4.49218800782043e-26, 2.2667851513676e-25,
                                              8.17576203365024e-14), .Dim = c(4L, 4L),
                                            .Dimnames = list(c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"),
                                                             c("s", "m", "l", "vl")))
test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = springRunDSM::params$cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = springRunDSM::params$freeport_flows[month, year],
                                           vernalis_flow = springRunDSM::params$vernalis_flows[month, year],
                                           stockton_flow = springRunDSM::params$stockton_flows[month, year],
                                           vernalis_temperature = springRunDSM::params$vernalis_temps[month, year],
                                           prisoners_point_temperature = springRunDSM::params$prisoners_point_temps[month, year],
                                           CVP_exp = springRunDSM::params$CVP_exports[month, year],
                                           SWP_exp = springRunDSM::params$SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})
# Tests the rearing survival rates function
expected_survival <- list(inchannel = structure(c(0.679337871242492, 0.0924445862652292,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.922791610478644, 1e-04, 1e-04,
                                                  1e-04, 0.0921446093597646, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.00968670833448966, 0.022411042055873, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 0.00297628777270874, 1e-04, 1e-04, 1e-04,
                                                  0.0016806093525492, 1e-04, 1e-04, 0.902975385868383, 0.309139513259462,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.98130991781418, 1e-04, 1e-04, 1e-04,
                                                  0.308375299813447, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  0.0411991138853027, 0.0914933839603649, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.012943957479448, 1e-04, 1e-04, 1e-04, 0.0073409658220353,
                                                  1e-04, 1e-04, 0.95137315370464, 0.484718453686442, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.991021428036703, 1e-04, 1e-04, 1e-04, 0.483824166758786,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0828479190458482,
                                                  0.174720363823946, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0268284432709071,
                                                  1e-04, 1e-04, 1e-04, 0.0153085504402503, 1e-04, 1e-04, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L)),
                          floodplain = structure(c(0.766077428417244,
                                                   0.115450727109286, 1e-04, 1e-04, 1e-04, 1e-04, 0.940480620472687,
                                                   1e-04, 1e-04, 1e-04, 0.0921446093597646, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 0.00968670833448966, 0.44862428487003, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.00297628777270874, 1e-04, 1e-04,
                                                   1e-04, 0.0016806093525492, 1e-04, 1e-04, 0.933654997345289, 0.362605353742648,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.98576600217467, 1e-04, 1e-04, 1e-04,
                                                   0.308375299813447, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                   0.0411991138853027, 0.781387307076476, 1e-04, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 0.012943957479448, 1e-04, 1e-04, 1e-04, 0.0073409658220353,
                                                   1e-04, 1e-04, 0.96716838257346, 0.54320137265084, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.993175648918326, 1e-04, 1e-04, 1e-04, 0.483824166758786,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0828479190458482,
                                                   0.882546534408116, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0268284432709071,
                                                   1e-04, 1e-04, 1e-04, 0.0153085504402503, 1e-04, 1e-04, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1, 1),
                                                 .Dim = c(31L, 4L)),
                          sutter = structure(c(0.01,  0.01, 0.01, 1),
                                             .Dim = c(1L, 4L),
                                             .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          yolo = structure(c(0.01, 0.01, 0.01, 1),
                                           .Dim = c(1L,  4L),
                                           .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          delta = structure(c(0.0932457862245425, 7.04958049133285e-14, 0.0932457862245425, 3.09684241716824e-13, 0.0932457862245425, 6.51028358869752e-13, 0.0932457862245425, 1),
                                            .Dim = c(2L, 4L),
                                            .Dimnames = list(c("North Delta", "South Delta"), c("s", "m", "l", "vl"))))

habitats <- list(
  spawning_habitat = springRunDSM::params$spawning_habitat,
  inchannel_habitat_fry = springRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = springRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat = springRunDSM::params$floodplain_habitat,
  weeks_flooded = springRunDSM::params$weeks_flooded
)

scenario_data <- DSMscenario::load_scenario(scenario = DSMscenario::scenarios$ONE,
                                            habitat_inputs = habitats,
                                            species = DSMscenario::species$spring_RUN)
test_that("get_rearing_survival returns the expected result", {
  set.seed(2021)
  survival <- get_rearing_survival(year = year, month = month,
                                   survival_adjustment = scenario_data$survival_adjustment,
                                   mode = "simulate",
                                   avg_temp = springRunDSM::params$avg_temp,
                                   avg_temp_delta = springRunDSM::params$avg_temp_delta,
                                   prob_strand_early = springRunDSM::params$prob_strand_early,
                                   prob_strand_late = springRunDSM::params$prob_strand_late,
                                   proportion_diverted = springRunDSM::params$proportion_diverted,
                                   total_diverted = springRunDSM::params$total_diverted,
                                   delta_proportion_diverted = springRunDSM::params$delta_proportion_diverted,
                                   delta_total_diverted = springRunDSM::params$delta_total_diverted,
                                   weeks_flooded = springRunDSM::params$weeks_flooded,
                                   prop_high_predation = springRunDSM::params$prop_high_predation,
                                   contact_points = springRunDSM::params$contact_points,
                                   delta_contact_points = springRunDSM::params$delta_contact_points,
                                   delta_prop_high_predation = springRunDSM::params$delta_prop_high_predation,
                                   ..surv_juv_rear_int = springRunDSM::params$..surv_juv_rear_int,
                                   ..surv_juv_rear_contact_points = springRunDSM::params$..surv_juv_rear_contact_points,
                                   ..surv_juv_rear_prop_diversions = springRunDSM::params$..surv_juv_rear_prop_diversions,
                                   ..surv_juv_rear_total_diversions = springRunDSM::params$..surv_juv_rear_total_diversions,
                                   ..surv_juv_bypass_int = springRunDSM::params$..surv_juv_bypass_int,
                                   ..surv_juv_delta_int = springRunDSM::params$..surv_juv_delta_int,
                                   ..surv_juv_delta_contact_points = springRunDSM::params$..surv_juv_delta_contact_points,
                                   .surv_juv_rear_contact_points = springRunDSM::params$.surv_juv_rear_contact_points,
                                   ..surv_juv_delta_total_diverted = springRunDSM::params$..surv_juv_delta_total_diverted,
                                   .surv_juv_delta_contact_points = springRunDSM::params$.surv_juv_delta_contact_points,
                                   .surv_juv_delta_total_diverted = springRunDSM::params$.surv_juv_delta_total_diverted,
                                   .surv_juv_rear_prop_diversions = springRunDSM::params$.surv_juv_rear_prop_diversions,
                                   .surv_juv_rear_total_diversions = springRunDSM::params$.surv_juv_rear_total_diversions,
                                   .surv_juv_rear_avg_temp_thresh = springRunDSM::params$.surv_juv_rear_avg_temp_thresh,
                                   .surv_juv_rear_high_predation = springRunDSM::params$.surv_juv_rear_high_predation,
                                   .surv_juv_rear_stranded = springRunDSM::params$.surv_juv_rear_stranded,
                                   .surv_juv_rear_medium = springRunDSM::params$.surv_juv_rear_medium,
                                   .surv_juv_rear_large = springRunDSM::params$.surv_juv_rear_large,
                                   .surv_juv_rear_floodplain = springRunDSM::params$.surv_juv_rear_floodplain,
                                   .surv_juv_bypass_avg_temp_thresh = springRunDSM::params$.surv_juv_bypass_avg_temp_thresh,
                                   .surv_juv_bypass_high_predation = springRunDSM::params$.surv_juv_bypass_high_predation,
                                   .surv_juv_bypass_medium = springRunDSM::params$.surv_juv_bypass_medium,
                                   .surv_juv_bypass_large = springRunDSM::params$.surv_juv_bypass_large,
                                   .surv_juv_bypass_floodplain = springRunDSM::params$.surv_juv_bypass_floodplain,
                                   .surv_juv_delta_avg_temp_thresh = springRunDSM::params$.surv_juv_delta_avg_temp_thresh,
                                   .surv_juv_delta_high_predation = springRunDSM::params$.surv_juv_delta_high_predation,
                                   .surv_juv_delta_prop_diverted = springRunDSM::params$.surv_juv_delta_prop_diverted,
                                   .surv_juv_delta_medium = springRunDSM::params$.surv_juv_delta_medium,
                                   .surv_juv_delta_large = springRunDSM::params$.surv_juv_delta_large,
                                   min_survival_rate = springRunDSM::params$min_survival_rate)
  expect_equal(survival, expected_survival)
})
