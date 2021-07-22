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
expected_lms_mig_surv <- c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189)

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = fallRunDSM::params$upper_sacramento_flows[month, year]),
               expected_lms_mig_surv)
})


# Tests migratory survival for san joaquin fish survival function
expected_san_joaquin_surv <- structure(c(0.0293122307513563, 0.117118990875781, 0.218061322644411,
                                     0.218061322644411), .Dim = c(1L, 4L),
                                   .Dimnames = list(NULL,
                                                    c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_san_joaquin_surv)
})

# Tests migratory survival for sac delta outmigration survival function
expected_sac_delta_mig_surv <- structure(c(0.361381001327841, 0.440065384516238, 0.521492554744754,
                                           0.521492554744754),
                                         .Dim = c(1L, 4L),
                                         .Dimnames = list(NULL,  c("s", "m", "l", "vl")))
test_that('The migratory_juv_surv function for sac delta returns the expected values for row one of year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = fallRunDSM::params$delta_inflow[month, year, "North Delta"],
                                               avg_temp = fallRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                                               perc_diversions = fallRunDSM::params$delta_proportion_diverted[month, year, "North Delta"] * 100),
               expected_sac_delta_mig_surv)
})

# tests the surv_juv_outmigration_delta function'
expected_surv_juv_outmigration <- structure(c(0.266668614822945, 0.000123932662831837, 0.000819655793037249,
                                              0.00566155265467863, 0.266668614822945, 0.000123932662831837,
                                              0.000819655793037249, 0.00566155265467863, 0.266668614822945,
                                              0.000123932662831837, 0.000819655793037249, 0.00566155265467863,
                                              0.373914118050784, 0.000245928323835351, 0.00124096914866476,
                                              0.0110614050155086),
                                            .Dim = c(4L, 4L),
                                            .Dimnames = list(c("northern_fish",  "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"),
                                                             c("s", "m", "l", "vl")))
test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = fallRunDSM::params$cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = fallRunDSM::params$freeport_flows[month, year],
                                           vernalis_flow = fallRunDSM::params$vernalis_flows[month, year],
                                           stockton_flow = fallRunDSM::params$stockton_flows[month, year],
                                           vernalis_temperature = fallRunDSM::params$vernalis_temps[month, year],
                                           prisoners_point_temperature = fallRunDSM::params$prisoners_point_temps[month, year],
                                           CVP_exp = fallRunDSM::params$CVP_exports[month, year],
                                           SWP_exp = fallRunDSM::params$SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})

# Tests the rearing survival rates function
expected_survival <- list(inchannel = structure(c(0.757132564901097, 0.966912366105333,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.964781767131652, 1e-04, 1e-04,
                                                  1e-04, 0.965197693086671, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.785710291402525, 0.838479069333334, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.0616888352059292, 1e-04, 1e-04, 1e-04, 0.0503860203840971,
                                                  1e-04, 1e-04, 0.931949122328572, 0.992270474524888, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.991758832953198, 1e-04, 1e-04, 1e-04, 0.991858849251346,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.941544688755609,
                                                  0.957991015320459, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.224091758328596,
                                                  1e-04, 1e-04, 1e-04, 0.189027474884767, 1e-04, 1e-04, 0.966431509808695,
                                                  0.996308221146246, 1e-04, 1e-04, 1e-04, 1e-04, 0.996062790207378,
                                                  1e-04, 1e-04, 1e-04, 0.996110777735786, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 0.971314520071448, 0.979566909662601, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.377780716422682, 1e-04, 1e-04,
                                                  1e-04, 0.328860845155822, 1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1), .Dim = c(31L, 4L)),
                          floodplain = structure(c(0.808752649644073,
                                                   0.974167054811826, 1e-04, 1e-04, 1e-04, 1e-04, 0.971925839131899,
                                                   1e-04, 1e-04, 1e-04, 0.965197693086671, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 0.785710291402525, 0.979122811970951, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.0616888352059292, 1e-04, 1e-04,
                                                   1e-04, 0.0503860203840971, 1e-04, 1e-04, 0.948515805017899, 0.993994776363888,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.993462791384788, 1e-04, 1e-04,
                                                   1e-04, 0.991858849251346, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 0.941544688755609, 0.995169677417565, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.224091758328596, 1e-04, 1e-04, 1e-04, 0.189027474884767,
                                                   1e-04, 1e-04, 0.974792513958658, 0.997134009170807, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.996879288140057, 1e-04, 1e-04, 1e-04, 0.996110777735786,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.971314520071448,
                                                   0.9976964553768, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.377780716422682,
                                                   1e-04, 1e-04, 1e-04, 0.328860845155822, 1e-04, 1e-04, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L)),
                          sutter = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          yolo = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          delta = structure(c(0.0932457862245425, 2.73832955765325e-06, 0.0932457862245425, 1.20292212396891e-05, 0.0932457862245425, 2.5287887701516e-05, 0.0932457862245425, 1),
                                            .Dim = c(2L, 4L), .Dimnames = list(c("North Delta", "South Delta"), c("s", "m", "l", "vl"))))
habitats <- list(
  spawning_habitat = fallRunDSM::params$spawning_habitat,
  inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat = fallRunDSM::params$floodplain_habitat,
  weeks_flooded = fallRunDSM::params$weeks_flooded
)

scenario_data <- DSMscenario::load_scenario(scenario = DSMscenario::scenarios$ONE,
                                            habitat_inputs = habitats,
                                            species = DSMscenario::species$FALL_RUN)
test_that("get_rearing_survival returns the expected result", {
  set.seed(2021)
  survival <- get_rearing_survival(year = year, month = month,
                                   survival_adjustment = scenario_data$survival_adjustment,
                                   mode = "simulate",
                                   avg_temp = fallRunDSM::params$avg_temp,
                                   avg_temp_delta = fallRunDSM::params$avg_temp_delta,
                                   prob_strand_early = fallRunDSM::params$prob_strand_early,
                                   prob_strand_late = fallRunDSM::params$prob_strand_late,
                                   proportion_diverted = fallRunDSM::params$proportion_diverted,
                                   total_diverted = fallRunDSM::params$total_diverted,
                                   delta_proportion_diverted = fallRunDSM::params$delta_proportion_diverted,
                                   delta_total_diverted = fallRunDSM::params$delta_total_diverted,
                                   weeks_flooded = fallRunDSM::params$weeks_flooded,
                                   prop_high_predation = fallRunDSM::params$prop_high_predation,
                                   contact_points = fallRunDSM::params$contact_points,
                                   delta_contact_points = fallRunDSM::params$delta_contact_points,
                                   delta_prop_high_predation = fallRunDSM::params$delta_prop_high_predation,
                                   ..surv_juv_rear_int = fallRunDSM::params$..surv_juv_rear_int,
                                   ..surv_juv_rear_contact_points = fallRunDSM::params$..surv_juv_rear_contact_points,
                                   ..surv_juv_rear_prop_diversions = fallRunDSM::params$..surv_juv_rear_prop_diversions,
                                   ..surv_juv_rear_total_diversions = fallRunDSM::params$..surv_juv_rear_total_diversions,
                                   ..surv_juv_bypass_int = fallRunDSM::params$..surv_juv_bypass_int,
                                   ..surv_juv_delta_int = fallRunDSM::params$..surv_juv_delta_int,
                                   ..surv_juv_delta_contact_points = fallRunDSM::params$..surv_juv_delta_contact_points,
                                   .surv_juv_rear_contact_points = fallRunDSM::params$.surv_juv_rear_contact_points,
                                   ..surv_juv_delta_total_diverted = fallRunDSM::params$..surv_juv_delta_total_diverted,
                                   .surv_juv_delta_contact_points = fallRunDSM::params$.surv_juv_delta_contact_points,
                                   .surv_juv_delta_total_diverted = fallRunDSM::params$.surv_juv_delta_total_diverted,
                                   .surv_juv_rear_prop_diversions = fallRunDSM::params$.surv_juv_rear_prop_diversions,
                                   .surv_juv_rear_total_diversions = fallRunDSM::params$.surv_juv_rear_total_diversions,
                                   .surv_juv_rear_avg_temp_thresh = fallRunDSM::params$.surv_juv_rear_avg_temp_thresh,
                                   .surv_juv_rear_high_predation = fallRunDSM::params$.surv_juv_rear_high_predation,
                                   .surv_juv_rear_stranded = fallRunDSM::params$.surv_juv_rear_stranded,
                                   .surv_juv_rear_medium = fallRunDSM::params$.surv_juv_rear_medium,
                                   .surv_juv_rear_large = fallRunDSM::params$.surv_juv_rear_large,
                                   .surv_juv_rear_floodplain = fallRunDSM::params$.surv_juv_rear_floodplain,
                                   .surv_juv_bypass_avg_temp_thresh = fallRunDSM::params$.surv_juv_bypass_avg_temp_thresh,
                                   .surv_juv_bypass_high_predation = fallRunDSM::params$.surv_juv_bypass_high_predation,
                                   .surv_juv_bypass_medium = fallRunDSM::params$.surv_juv_bypass_medium,
                                   .surv_juv_bypass_large = fallRunDSM::params$.surv_juv_bypass_large,
                                   .surv_juv_bypass_floodplain = fallRunDSM::params$.surv_juv_bypass_floodplain,
                                   .surv_juv_delta_avg_temp_thresh = fallRunDSM::params$.surv_juv_delta_avg_temp_thresh,
                                   .surv_juv_delta_high_predation = fallRunDSM::params$.surv_juv_delta_high_predation,
                                   .surv_juv_delta_prop_diverted = fallRunDSM::params$.surv_juv_delta_prop_diverted,
                                   .surv_juv_delta_medium = fallRunDSM::params$.surv_juv_delta_medium,
                                   .surv_juv_delta_large = fallRunDSM::params$.surv_juv_delta_large,
                                   min_survival_rate = fallRunDSM::params$min_survival_rate)
  expect_equal(survival, expected_survival)
})

expected_migratory_survival <- list(delta = structure(c(0.266668614822945, 0.000123932662831837,
                                                        0.000819655793037249, 0.00566155265467863, 0.266668614822945,
                                                        0.000123932662831837, 0.000819655793037249, 0.00566155265467863,
                                                        0.266668614822945, 0.000123932662831837, 0.000819655793037249,
                                                        0.00566155265467863, 0.373914118050784, 0.000245928323835351,
                                                        0.00124096914866476, 0.0110614050155086),
                                                      .Dim = c(4L, 4L),
                                                      .Dimnames = list(
                                                        c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish",
                                                          "southern_fish"),
                                                        c("s", "m", "l", "vl"))),
                                    san_joaquin = structure(c(0.0293122307513563, 0.117118990875781, 0.218061322644411, 0.218061322644411),
                                                            .Dim = c(1L,  4L),
                                                            .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    uppermid_sac = c(s = 0.189,  m = 0.189, l = 0.189, vl = 0.189),
                                    lowermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    lower_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    sutter = structure(c(0.01, 0.01, 0.01, 1),
                                                       .Dim = c(1L, 4L),
                                                       .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    yolo = structure(c(0.01, 0.01, 0.01, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    sac_delta = structure(c(0.709190977181413, 0.82945620000403, 0.955358125560114, 0.955358125560114),
                                                          .Dim = c(1L, 4L),
                                                          .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    bay_delta = 0.358)

test_that("get_migratory_survival returns the expected result", {
  set.seed(2021)
  migratory_survival <- get_migratory_survival(year = year, month = month,
                                               cc_gates_prop_days_closed = fallRunDSM::params$cc_gates_prop_days_closed,
                                               freeport_flows = fallRunDSM::params$freeport_flows,
                                               vernalis_flows = fallRunDSM::params$vernalis_flows,
                                               stockton_flows = fallRunDSM::params$stockton_flows,
                                               vernalis_temps = fallRunDSM::params$vernalis_temps,
                                               prisoners_point_temps = fallRunDSM::params$prisoners_point_temps,
                                               CVP_exports = fallRunDSM::params$CVP_exports,
                                               SWP_exports = fallRunDSM::params$SWP_exports,
                                               upper_sacramento_flows = fallRunDSM::params$upper_sacramento_flows,
                                               delta_inflow = fallRunDSM::params$delta_inflow,
                                               avg_temp_delta = fallRunDSM::params$avg_temp_delta,
                                               avg_temp = fallRunDSM::params$avg_temp,
                                               delta_proportion_diverted = fallRunDSM::params$delta_proportion_diverted,
                                               .surv_juv_outmigration_sac_delta_intercept_one = fallRunDSM::params$.surv_juv_outmigration_sac_delta_intercept_one,
                                               .surv_juv_outmigration_sac_delta_intercept_two = fallRunDSM::params$.surv_juv_outmigration_sac_delta_intercept_two,
                                               .surv_juv_outmigration_sac_delta_intercept_three = fallRunDSM::params$.surv_juv_outmigration_sac_delta_intercept_three,
                                               .surv_juv_outmigration_sac_delta_delta_flow = fallRunDSM::params$.surv_juv_outmigration_sac_delta_delta_flow,
                                               .surv_juv_outmigration_sac_delta_avg_temp = fallRunDSM::params$.surv_juv_outmigration_sac_delta_avg_temp,
                                               .surv_juv_outmigration_sac_delta_perc_diversions = fallRunDSM::params$.surv_juv_outmigration_sac_delta_perc_diversions,
                                               .surv_juv_outmigration_sac_delta_medium = fallRunDSM::params$.surv_juv_outmigration_sac_delta_medium,
                                               .surv_juv_outmigration_sac_delta_large = fallRunDSM::params$.surv_juv_outmigration_sac_delta_large,
                                               ..surv_juv_outmigration_sj_int = fallRunDSM::params$..surv_juv_outmigration_sj_int,
                                               ..surv_juv_outmigration_sac_int_one = fallRunDSM::params$..surv_juv_outmigration_sac_int_one,
                                               ..surv_juv_outmigration_sac_prop_diversions = fallRunDSM::params$..surv_juv_outmigration_sac_prop_diversions,
                                               ..surv_juv_outmigration_sac_total_diversions = fallRunDSM::params$..surv_juv_outmigration_sac_total_diversions,
                                               ..surv_juv_outmigration_sac_int_two = fallRunDSM::params$..surv_juv_outmigration_sac_int_two,
                                               .surv_juv_outmigration_san_joaquin_medium = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_medium,
                                               .surv_juv_outmigration_san_joaquin_large = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_large,
                                               min_survival_rate = fallRunDSM::params$min_survival_rate,
                                               surv_juv_outmigration_sac_delta_model_weights = fallRunDSM::params$surv_juv_outmigration_sac_delta_model_weights) #migratory_survival$uppermid_sac)
  expect_equal(migratory_survival, expected_migratory_survival)
})


