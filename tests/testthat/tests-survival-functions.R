library(testthat)
library(winterRunDSM)
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
                                                       0.846089404751765, 1), .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL,   c("s", "m", "l", "vl"))),
                               floodplain = structure(c(0.567765169512671, 0.835659795386719, 0.912083864792188, 1),
                                                      .Dim = c(1L, 4L),  .Dimnames = list( "Upper Sacramento River", c("s", "m", "l", "vl"))))
test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = winterRunDSM::params$contact_points[1],
                             prop_diversions = winterRunDSM::params$proportion_diverted[1],
                             total_diversions = winterRunDSM::params$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = winterRunDSM::params$weeks_flooded[, month, year][1],
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
  expect_equal(surv_juv_delta(avg_temp = winterRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                              max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = winterRunDSM::params$delta_prop_high_predation,
                              contact_points = winterRunDSM::params$delta_contact_points,
                              prop_diverted = winterRunDSM::params$delta_proportion_diverted,
                              total_diverted = winterRunDSM::params$delta_total_diverted),
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
expected_lms_mig_surv <- c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189)

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = winterRunDSM::params$upper_sacramento_flows[month, year]),
               expected_lms_mig_surv)
})


# Tests migratory survival for san joaquin fish survival function
expected_sj_mig_surv <- structure(c(0.0465823740391211, 0.176705306337067, 0.310918056813447,
                                    0.310918056813447),
                                  .Dim = c(1L, 4L),
                                  .Dimnames = list(NULL,   c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_sj_mig_surv)
})

# Tests migratory survival for sac delta outmigration survival function
expected_sac_delta_mig_surv <- structure(c(0.361381001327841, 0.440065384516238, 0.521492554744754,
                                           0.521492554744754),
                                         .Dim = c(1L, 4L), .Dimnames = list(NULL,  c("s", "m", "l", "vl")))
test_that('The migratory_juv_surv function for sac delta returns the expected values for row one of year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = fallRunDSM::params$delta_inflow[month, year, "North Delta"],
                                               avg_temp = fallRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                                               perc_diversions = fallRunDSM::params$delta_proportion_diverted[month, year, "North Delta"] * 100),
               expected_sac_delta_mig_surv)
})

# tests the surv_juv_outmigration_delta function'
expected_surv_juv_outmigration <- structure(c(0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25,
                                              3.67469661043849e-14, 0.266668614822945, 2.26283033759458e-26,
                                              1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945,
                                              2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14,
                                              0.373914118050784, 4.49218800782043e-26, 2.2667851513676e-25,
                                              8.17576203365024e-14), .Dim = c(4L, 4L),
                                            .Dimnames = list(c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"),
                                                             c("s", "m", "l", "vl")))
test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = winterRunDSM::params$cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = winterRunDSM::params$freeport_flows[month, year],
                                           vernalis_flow = winterRunDSM::params$vernalis_flows[month, year],
                                           stockton_flow = winterRunDSM::params$stockton_flows[month, year],
                                           vernalis_temperature = winterRunDSM::params$vernalis_temps[month, year],
                                           prisoners_point_temperature = winterRunDSM::params$prisoners_point_temps[month, year],
                                           CVP_exp = winterRunDSM::params$CVP_exports[month, year],
                                           SWP_exp = winterRunDSM::params$SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})

# Tests the rearing survival rates function
expected_survival <- list(inchannel = structure(c(0.281941427795232, 0.289265768923124,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.306865131650589, 1e-04, 1e-04,
                                                  1e-04, 0.267061580554748, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.0555620446157733, 0.0753773402305201, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 0.142227776211696, 1e-04, 1e-04, 1e-04,
                                                  0.0403182701024075, 1e-04, 1e-04, 0.633009206393106, 0.641308358389236,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.660423881715229, 1e-04, 1e-04,
                                                  1e-04, 0.615482588137185, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.205365706447436, 0.263689619149164, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.421429290782037, 1e-04, 1e-04, 1e-04, 0.15580254116033,
                                                  1e-04, 1e-04, 0.783833588554346, 0.789854286522123, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.80347931448211, 1e-04, 1e-04, 1e-04, 0.770903092464039,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.352038741654704,
                                                  0.429503041159465, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.604939907282548,
                                                  1e-04, 1e-04, 1e-04, 0.279529431258522, 1e-04, 1e-04, 1, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1),
                                                .Dim = c(31L, 4L)),
                          floodplain = structure(c(0.345588240726284,
                                                   0.36224393899662, 1e-04, 1e-04, 1e-04, 1e-04, 0.36053985581436,
                                                   1e-04, 1e-04, 1e-04, 0.267061580554748, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 0.0555620446157733, 0.423602991097244, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.142227776211696, 1e-04, 1e-04,
                                                   1e-04, 0.0403182701024075, 1e-04, 1e-04, 0.695202516608632, 0.709446154041349,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.710097047857016, 1e-04, 1e-04,
                                                   1e-04, 0.615482588137185, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 0.205365706447436, 0.76350633484916, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.421429290782037, 1e-04, 1e-04, 1e-04, 0.15580254116033,
                                                   1e-04, 1e-04, 0.826543130315782, 0.835898781072232, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.836837119603856, 1e-04, 1e-04, 1e-04, 0.770903092464039,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.352038741654704,
                                                   0.87157983052527, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.604939907282548,
                                                   1e-04, 1e-04, 1e-04, 0.279529431258522, 1e-04, 1e-04, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L)),
                          sutter = structure(c(0.01, 0.01, 0.01, 1),
                                             .Dim = c(1L, 4L),
                                             .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          yolo = structure(c(0.01, 0.01, 0.01, 1),
                                           .Dim = c(1L,  4L),
                                           .Dimnames = list(NULL,
                                                            c("s", "m", "l", "vl"))),
                          delta = structure(c(0.0932457862245425,
                                              0.000254726339391766, 0.0932457862245425, 0.00111803268739273,
                                              0.0932457862245425, 0.00234747207906635, 0.0932457862245425,
                                              1),
                                            .Dim = c(2L, 4L),
                                            .Dimnames = list(c("North Delta", "South Delta"),
                                                             c("s", "m", "l", "vl"))))

habitats <- list(
  spawning_habitat = winterRunDSM::params$spawning_habitat,
  inchannel_habitat_fry = winterRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = winterRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat = winterRunDSM::params$floodplain_habitat,
  weeks_flooded = winterRunDSM::params$weeks_flooded
)

scenario_data <- DSMscenario::load_scenario(scenario = DSMscenario::scenarios$ONE,
                                            habitat_inputs = habitats,
                                            species = DSMscenario::species$winter_RUN)
test_that("get_rearing_survival returns the expected result", {
  set.seed(2021)
  survival <- get_rearing_survival(year = year, month = month,
                                   survival_adjustment = scenario_data$survival_adjustment,
                                   mode = "simulate",
                                   avg_temp = winterRunDSM::params$avg_temp,
                                   avg_temp_delta = winterRunDSM::params$avg_temp_delta,
                                   prob_strand_early = winterRunDSM::params$prob_strand_early,
                                   prob_strand_late = winterRunDSM::params$prob_strand_late,
                                   proportion_diverted = winterRunDSM::params$proportion_diverted,
                                   total_diverted = winterRunDSM::params$total_diverted,
                                   delta_proportion_diverted = winterRunDSM::params$delta_proportion_diverted,
                                   delta_total_diverted = winterRunDSM::params$delta_total_diverted,
                                   weeks_flooded = winterRunDSM::params$weeks_flooded,
                                   prop_high_predation = winterRunDSM::params$prop_high_predation,
                                   contact_points = winterRunDSM::params$contact_points,
                                   delta_contact_points = winterRunDSM::params$delta_contact_points,
                                   delta_prop_high_predation = winterRunDSM::params$delta_prop_high_predation,
                                   ..surv_juv_rear_int = winterRunDSM::params$..surv_juv_rear_int,
                                   ..surv_juv_rear_contact_points = winterRunDSM::params$..surv_juv_rear_contact_points,
                                   ..surv_juv_rear_prop_diversions = winterRunDSM::params$..surv_juv_rear_prop_diversions,
                                   ..surv_juv_rear_total_diversions = winterRunDSM::params$..surv_juv_rear_total_diversions,
                                   ..surv_juv_bypass_int = winterRunDSM::params$..surv_juv_bypass_int,
                                   ..surv_juv_delta_int = winterRunDSM::params$..surv_juv_delta_int,
                                   ..surv_juv_delta_contact_points = winterRunDSM::params$..surv_juv_delta_contact_points,
                                   .surv_juv_rear_contact_points = winterRunDSM::params$.surv_juv_rear_contact_points,
                                   ..surv_juv_delta_total_diverted = winterRunDSM::params$..surv_juv_delta_total_diverted,
                                   .surv_juv_delta_contact_points = winterRunDSM::params$.surv_juv_delta_contact_points,
                                   .surv_juv_delta_total_diverted = winterRunDSM::params$.surv_juv_delta_total_diverted,
                                   .surv_juv_rear_prop_diversions = winterRunDSM::params$.surv_juv_rear_prop_diversions,
                                   .surv_juv_rear_total_diversions = winterRunDSM::params$.surv_juv_rear_total_diversions,
                                   .surv_juv_rear_avg_temp_thresh = winterRunDSM::params$.surv_juv_rear_avg_temp_thresh,
                                   .surv_juv_rear_high_predation = winterRunDSM::params$.surv_juv_rear_high_predation,
                                   .surv_juv_rear_stranded = winterRunDSM::params$.surv_juv_rear_stranded,
                                   .surv_juv_rear_medium = winterRunDSM::params$.surv_juv_rear_medium,
                                   .surv_juv_rear_large = winterRunDSM::params$.surv_juv_rear_large,
                                   .surv_juv_rear_floodplain = winterRunDSM::params$.surv_juv_rear_floodplain,
                                   .surv_juv_bypass_avg_temp_thresh = winterRunDSM::params$.surv_juv_bypass_avg_temp_thresh,
                                   .surv_juv_bypass_high_predation = winterRunDSM::params$.surv_juv_bypass_high_predation,
                                   .surv_juv_bypass_medium = winterRunDSM::params$.surv_juv_bypass_medium,
                                   .surv_juv_bypass_large = winterRunDSM::params$.surv_juv_bypass_large,
                                   .surv_juv_bypass_floodplain = winterRunDSM::params$.surv_juv_bypass_floodplain,
                                   .surv_juv_delta_avg_temp_thresh = winterRunDSM::params$.surv_juv_delta_avg_temp_thresh,
                                   .surv_juv_delta_high_predation = winterRunDSM::params$.surv_juv_delta_high_predation,
                                   .surv_juv_delta_prop_diverted = winterRunDSM::params$.surv_juv_delta_prop_diverted,
                                   .surv_juv_delta_medium = winterRunDSM::params$.surv_juv_delta_medium,
                                   .surv_juv_delta_large = winterRunDSM::params$.surv_juv_delta_large,
                                   min_survival_rate = winterRunDSM::params$min_survival_rate)
  expect_equal(survival, expected_survival)
})

# Adds test for migratory surv function
expected_migratory_survival <- list(delta = structure(c(0.266668614822945, 4.23117924253791e-24,
                                                        2.79838300757807e-23, 2.25682449302165e-13, 0.266668614822945,
                                                        4.23117924253791e-24, 2.79838300757807e-23, 2.25682449302165e-13,
                                                        0.266668614822945, 4.23117924253791e-24, 2.79838300757807e-23,
                                                        2.25682449302165e-13, 0.373914118050784, 8.39976923434414e-24,
                                                        4.2385741964001e-23, 5.04188928377011e-13),
                                                      .Dim = c(4L, 4L),
                                                      .Dimnames = list(
                                                          c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish",
                                                            "southern_fish"), c("s", "m", "l", "vl"))),
                                    san_joaquin = structure(c(0.0465823740391211, 0.176705306337067, 0.310918056813447, 0.310918056813447),
                                                            .Dim = c(1L, 4L),
                                                            .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    uppermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    lowermid_sac = c(s = 0.189,  m = 0.189, l = 0.189, vl = 0.189),
                                    lower_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    sutter = structure(c(0.01, 0.01, 0.01, 1),
                                                       .Dim = c(1L, 4L),
                                                       .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    yolo = structure(c(0.01, 0.01, 0.01, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    sac_delta = structure(c(0.709190977181413,  0.82945620000403, 0.955358125560114, 0.955358125560114),
                                                          .Dim = c(1L, 4L),
                                                          .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    bay_delta = 0.358)

test_that("get_migratory_survival returns the expected result", {
  set.seed(2021)
  migratory_survival <- get_migratory_survival(year = year, month = month,
                                               cc_gates_prop_days_closed = winterRunDSM::params$cc_gates_prop_days_closed,
                                               freeport_flows = winterRunDSM::params$freeport_flows,
                                               vernalis_flows = winterRunDSM::params$vernalis_flows,
                                               stockton_flows = winterRunDSM::params$stockton_flows,
                                               vernalis_temps = winterRunDSM::params$vernalis_temps,
                                               prisoners_point_temps = winterRunDSM::params$prisoners_point_temps,
                                               CVP_exports = winterRunDSM::params$CVP_exports,
                                               SWP_exports = winterRunDSM::params$SWP_exports,
                                               upper_sacramento_flows = winterRunDSM::params$upper_sacramento_flows,
                                               delta_inflow = winterRunDSM::params$delta_inflow,
                                               avg_temp_delta = winterRunDSM::params$avg_temp_delta,
                                               avg_temp = winterRunDSM::params$avg_temp,
                                               delta_proportion_diverted = winterRunDSM::params$delta_proportion_diverted,
                                               .surv_juv_outmigration_sac_delta_intercept_one = winterRunDSM::params$.surv_juv_outmigration_sac_delta_intercept_one,
                                               .surv_juv_outmigration_sac_delta_intercept_two = winterRunDSM::params$.surv_juv_outmigration_sac_delta_intercept_two,
                                               .surv_juv_outmigration_sac_delta_intercept_three = winterRunDSM::params$.surv_juv_outmigration_sac_delta_intercept_three,
                                               .surv_juv_outmigration_sac_delta_delta_flow = winterRunDSM::params$.surv_juv_outmigration_sac_delta_delta_flow,
                                               .surv_juv_outmigration_sac_delta_avg_temp = winterRunDSM::params$.surv_juv_outmigration_sac_delta_avg_temp,
                                               .surv_juv_outmigration_sac_delta_perc_diversions = winterRunDSM::params$.surv_juv_outmigration_sac_delta_perc_diversions,
                                               .surv_juv_outmigration_sac_delta_medium = winterRunDSM::params$.surv_juv_outmigration_sac_delta_medium,
                                               .surv_juv_outmigration_sac_delta_large = winterRunDSM::params$.surv_juv_outmigration_sac_delta_large,
                                               ..surv_juv_outmigration_sj_int = winterRunDSM::params$..surv_juv_outmigration_sj_int,
                                               ..surv_juv_outmigration_sac_int_one = winterRunDSM::params$..surv_juv_outmigration_sac_int_one,
                                               ..surv_juv_outmigration_sac_prop_diversions = winterRunDSM::params$..surv_juv_outmigration_sac_prop_diversions,
                                               ..surv_juv_outmigration_sac_total_diversions = winterRunDSM::params$..surv_juv_outmigration_sac_total_diversions,
                                               ..surv_juv_outmigration_sac_int_two = winterRunDSM::params$..surv_juv_outmigration_sac_int_two,
                                               .surv_juv_outmigration_san_joaquin_medium = winterRunDSM::params$.surv_juv_outmigration_san_joaquin_medium,
                                               .surv_juv_outmigration_san_joaquin_large = winterRunDSM::params$.surv_juv_outmigration_san_joaquin_large,
                                               surv_juv_outmigration_sac_delta_model_weights = winterRunDSM::params$surv_juv_outmigration_sac_delta_model_weights,
                                               min_survival_rate = winterRunDSM::params$min_survival_rate) #migratory_survival$uppermid_sac)
  expect_equal(migratory_survival, expected_migratory_survival)
})
