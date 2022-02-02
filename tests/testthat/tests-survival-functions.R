library(testthat)
library(fallRunDSM)
# tests for DSM survival functions
# Defines inputs to use in testing
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

# Tests surv_juv_rear survival function ----------------------------------------
# Stochastic
expected_surv_juv_rear <- list(inchannel = structure(c(0.72111184400087, 0.919085299913685,
                                                       0.959804841544676, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL,  c("s", "m", "l", "vl"))),
                               floodplain = structure(c(0.834189256892422,   0.954755901624986, 0.977778028604605, 1),
                                                      .Dim = c(1L, 4L),
                                                      .Dimnames = list("Upper Sacramento River", c("s", "m", "l", "vl"))))

test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1, stochastic = TRUE', {
  set.seed(2021)
  surv <- surv_juv_rear(max_temp_thresh = maxT25[1],
                        avg_temp_thresh = aveT20[1],
                        high_predation = high_predation[1],
                        contact_points = fallRunDSM::params$contact_points[1],
                        prop_diversions = fallRunDSM::params$proportion_diverted[1],
                        total_diversions = fallRunDSM::params$total_diverted[1],
                        stranded = ws_strand[1],
                        weeks_flooded = fallRunDSM::params$weeks_flooded[, month, year][1],
                        ..surv_juv_rear_int = fallRunDSM::params$..surv_juv_rear_int[1],
                        ..surv_juv_rear_contact_points = -0.0067662,
                        ..surv_juv_rear_prop_diversions = -0.1755,
                        ..surv_juv_rear_total_diversions = -0.0004515,
                        stochastic = TRUE)
  expect_equal(surv, expected_surv_juv_rear)
})

# Deterministic
expected_surv_juv_rear_det <- list(inchannel = structure(c(0.72111184400087, 0.919085299913685,
                                                           0.959804841544676, 1),
                                                         .Dim = c(1L, 4L),
                                                         .Dimnames = list("Upper Sacramento River",
                                                                          c("s", "m", "l", "vl"))),
                                   floodplain = structure(c(0.834189256892422,
                                                            0.954755901624986, 0.977778028604605, 1),
                                                          .Dim = c(1L, 4L),
                                                          .Dimnames = list(
                                                            "Upper Sacramento River", c("s", "m", "l", "vl"))))

test_that('The surv_juv_rear function returns the expected values for year 1 month 9, stochastic = FALSE', {
  surv <- surv_juv_rear(max_temp_thresh = maxT25[1],
                        avg_temp_thresh = aveT20[1],
                        high_predation = high_predation[1],
                        contact_points = fallRunDSM::params$contact_points[1],
                        prop_diversions = fallRunDSM::params$proportion_diverted[1],
                        total_diversions = fallRunDSM::params$total_diverted[1],
                        stranded = ws_strand[1],
                        weeks_flooded = fallRunDSM::params$weeks_flooded[, month, year][1],
                        ..surv_juv_rear_int = fallRunDSM::params$..surv_juv_rear_int[1],
                        ..surv_juv_rear_contact_points = -0.0067662,
                        ..surv_juv_rear_prop_diversions = -0.1755,
                        ..surv_juv_rear_total_diversions = -0.0004515,
                        stochastic = FALSE)
  expect_equal(surv, expected_surv_juv_rear_det)
})

# Tests surv_juv_delta survival function ---------------------------------------
# Stochastic
expected_delta_juv_surv <- structure(c(0.0932457862245425, 1e-04, 0.0932457862245425, 1e-04,
                                       0.0932457862245425, 1e-04, 1, 1),
                                     .Dim = c(2L, 4L),
                                     .Dimnames = list(c("North Delta", "South Delta"), c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9, stochastic = TRUE', {
  set.seed(2012)
  surv <- surv_juv_delta(avg_temp = fallRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                         max_temp_thresh = maxT25D,
                         avg_temp_thresh = aveT20D,
                         high_predation = fallRunDSM::params$delta_prop_high_predation,
                         contact_points = fallRunDSM::params$delta_contact_points,
                         prop_diverted = fallRunDSM::params$delta_proportion_diverted,
                         total_diverted = fallRunDSM::params$delta_total_diverted,
                         stochastic = TRUE)
  expect_equal(surv,
               expected_delta_juv_surv)
})
# Deterministic
expected_delta_juv_surv_det <- structure(c(0.0932457862245425, 1e-04, 0.0932457862245425, 1e-04,
                                       0.0932457862245425, 1e-04, 1, 1),
                                     .Dim = c(2L, 4L),
                                     .Dimnames = list(c("North Delta", "South Delta"), c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9, stochastic = FALSE', {
  surv <- surv_juv_delta(avg_temp = fallRunDSM::params$avg_temp_delta[month, year, "North Delta"],
                         max_temp_thresh = maxT25D,
                         avg_temp_thresh = aveT20D,
                         high_predation = fallRunDSM::params$delta_prop_high_predation,
                         contact_points = fallRunDSM::params$delta_contact_points,
                         prop_diverted = fallRunDSM::params$delta_proportion_diverted,
                         total_diverted = fallRunDSM::params$delta_total_diverted,
                         stochastic = FALSE)
  expect_equal(surv,
               expected_delta_juv_surv_det)
})

# Tests surv_juv_bypass survival function --------------------------------------
# Stochastic
expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1), .Dim = c(1L, 4L),
                                      .Dimnames = list( NULL, c("s", "m", "l", "vl")))

test_that('The bypass_juv_surv function returns the expected values for year 1 month 9, stochastic = TRUE', {
  set.seed(2021)
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0,
                               ..surv_juv_bypass_int = fallRunDSM::params$..surv_juv_bypass_int,
                               stochastic = TRUE),
               expected_bypass_juv_surv)
})
# Deterministic
expected_bypass_juv_surv_det <- structure(c(1e-04, 1e-04, 1e-04, 1), .Dim = c(1L, 4L),
                                      .Dimnames = list( NULL, c("s", "m", "l", "vl")))
test_that('The bypass_juv_surv function returns the expected values for year 1 month 9, stochastic = FALSE', {
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0,
                               ..surv_juv_bypass_int = fallRunDSM::params$..surv_juv_bypass_int,
                               stochastic = FALSE),
               expected_bypass_juv_surv_det)
})

# Tests migratory survival for lower mid sac fish ------------------------------
expected_lms_mig_surv <- c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189)

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = fallRunDSM::params$upper_sacramento_flows[month, year]),
               expected_lms_mig_surv)
})

# Tests migratory survival for san joaquin fish --------------------------------
expected_san_joaquin_surv <- structure(c(0.393082390467511, 0.739934122927998, 0.856758760416419,
                                         0.856758760416419),
                                       .Dim = c(1L, 4L),
                                       .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_san_joaquin_surv)
})

# tests the surv_juv_outmigration_delta function -------------------------------
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

# Tests the rearing survival rates function ------------------------------------
# Stochastic
expected_survival <- list(inchannel = structure(c(0.856624499023856, 0.996343713622718,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.888430213662521, 1e-04, 1e-04,
                                                  1e-04, 0.999852145081355, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.0672236211774865, 0.00182146636085243, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 0.225323686458255, 1e-04, 1e-04, 1e-04,
                                                  0.000147331263505929, 1e-04, 1e-04, 0.963298036464869, 0.999165334343724,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.972207574576654, 1e-04, 1e-04,
                                                  1e-04, 0.999966338804077, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.240463366109628, 0.00795245552282462, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 0.560968279082631, 1e-04, 1e-04, 1e-04,
                                                  0.000646894863366816, 1e-04, 1e-04, 0.98219888277702, 0.999602788471264,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.986584071105416, 1e-04, 1e-04,
                                                  1e-04, 0.999983987600884, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.39959790117512, 0.0165726457261406, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 0.728711028883352, 1e-04, 1e-04, 1e-04, 0.00135895460181033,
                                                  1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L
                                                  )),
                          floodplain = structure(c(0.909198108464421, 0.996343713622718,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.923406187750263, 1e-04, 1e-04,
                                                   1e-04, 0.999852145081355, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 0.0672236211774865, 0.412416876764704, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.225323686458255, 1e-04, 1e-04, 1e-04, 0.00757602514176905,
                                                   1e-04, 1e-04, 0.977450513506025, 0.999165334343724, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 0.981333859571199, 1e-04, 1e-04, 1e-04, 0.999966338804077,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.240463366109628,
                                                   0.755103092910063, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.560968279082631,
                                                   1e-04, 1e-04, 1e-04, 0.0218361758208064, 1e-04, 1e-04, 0.989118953424544,
                                                   0.999602788471264, 1e-04, 1e-04, 1e-04, 1e-04, 0.991022245368429,
                                                   1e-04, 1e-04, 1e-04, 0.999983987600884, 1e-04, 1e-04, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 0.39959790117512, 0.866344489667394, 1e-04,
                                                   1e-04, 1e-04, 1e-04, 1e-04, 0.728711028883352, 1e-04, 1e-04,
                                                   1e-04, 0.0340855616477089, 1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1,
                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                   1, 1, 1), .Dim = c(31L, 4L)),
                          sutter = structure(c(1e-04, 1e-04, 1e-04, 1),
                                             .Dim = c(1L, 4L),
                                             .Dimnames = list(NULL, c("s", "m",  "l", "vl"))),
                          yolo = structure(c(1e-04, 1e-04, 1e-04, 1),
                                           .Dim = c(1L, 4L),
                                           .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          delta = structure(c(0.0932457862245425, 1, 0.0932457862245425, 1, 0.0932457862245425, 1, 1, 1),
                                            .Dim = c(2L, 4L),
                                            .Dimnames = list(c("North Delta", "South Delta"), c("s","m", "l", "vl"))))
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
test_that("get_rearing_survival returns the expected result, stochastic = TRUE", {
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
                                   min_survival_rate = fallRunDSM::params$min_survival_rate,
                                   stochastic = TRUE)
  expect_equal(survival, expected_survival)
})
# Deterministic
rearing_survival_det <- list(inchannel = structure(c(0.915837246366906, 0.974085279282976,
                                                     0.0172412210688993, 0.00875260768814385, 0.000885917697965271,
                                                     0.00192871158220226, 0.881732834530317, 0.000199007637167694,
                                                     0.000168797047454773, 0.0311905821355649, 0.916207234498557,
                                                     0.0311849382467165, 0.363679081975831, 0.000170962008595541,
                                                     0.00025226572170483, 0.0318448209653818, 0.00972966133072185,
                                                     0.351157571406356, 0.0109797294268581, 0.260681791753687, 9.99560557602883e-05,
                                                     0.00236772960410237, 0.110188922251298, 0.00012287405910406,
                                                     0.976566499981433, 0.000172134577851134, 0.0129382965524694,
                                                     0.0456222973689647, 0.00125464824725702, 0.000307258100697566,
                                                     9.99713876722231e-05, 0.966611585026083, 0.977571528920252, 0.033529100550375,
                                                     0.017162184186654, 0.0017287962191172, 0.00192936086488351, 0.969813787760817,
                                                     0.000323898192916928, 0.000282634280177207, 0.0313069314404304,
                                                     0.916336016107895, 0.0313725508311694, 0.688768862237255, 0.000186927958702088,
                                                     0.000252303515806104, 0.0367218244168067, 0.0213981261596561,
                                                     0.703472902989487, 0.0465010145690213, 0.279806954796677, 9.99560575618316e-05,
                                                     0.0053021173217221, 0.244080314961836, 0.000199780922748418,
                                                     0.985675523264171, 0.000204558462234152, 0.0492123637470134,
                                                     0.0686824422645692, 0.00548602903819954, 0.000979351384715302,
                                                     9.99740993970717e-05, 0.974964352059291, 0.978112489873993, 0.0392815660722533,
                                                     0.020177101885405, 0.00205229295579621, 0.00192946124031078,
                                                     0.985019452551268, 0.000378094278334877, 0.000345345723534104,
                                                     0.0313249887336499, 0.916355920088076, 0.0314017449795847, 0.799161283922044,
                                                     0.000190059080061623, 0.000252309357839838, 0.0376123827506418,
                                                     0.0263036532814517, 0.832552228023565, 0.0929897507038318, 0.283015660719655,
                                                     9.99560601328013e-05, 0.00660192444160216, 0.300531824167725,
                                                     0.000304396038648286, 0.987098330356641, 0.000212362755083272,
                                                     0.0871447607310227, 0.0745066283008312, 0.0114625014998623, 0.00185005388870498,
                                                     9.99779691937205e-05, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                                                   .Dim = c(31L,   4L)),
                             floodplain = structure(c(0.933167942279199, 0.974085279282976,
                                                      0.0172412210688993, 0.00875260768814385, 0.000885917697965271,
                                                      0.00192871158220226, 0.91954434139137, 0.000222146720225951,
                                                      0.000168797047454773, 0.0311905821355649, 0.916207234498557,
                                                      0.0311849382467165, 0.363679081975831, 0.000170962008595541,
                                                      0.00025226572170483, 0.0332135121421088, 0.00972966133072185,
                                                      0.351157571406356, 0.420694503221018, 0.260681791753687, 0.000217816031198433,
                                                      0.00236772960410237, 0.103038413587679, 0.00012287405910406,
                                                      0.976566499981433, 0.000172134577851134, 0.0420399613315284,
                                                      0.0456222973689647, 0.0221571686698767, 0.000307258100697566,
                                                      0.000154770286438671, 0.970913144594051, 0.977571528920252, 0.033529100550375,
                                                      0.017162184186654, 0.0017287962191172, 0.00192936086488351, 0.979743658058925,
                                                      0.000346114552036933, 0.000282634280177207, 0.0313069314404304,
                                                      0.916336016107895, 0.0313725508311694, 0.688768862237255, 0.000186927958702088,
                                                      0.000252303515806104, 0.0371174131739443, 0.0213981261596561,
                                                      0.703472902989487, 0.761344732576533, 0.279806954796677, 0.000228591268211704,
                                                      0.0053021173217221, 0.235690250468192, 0.000199780922748418,
                                                      0.985675523264171, 0.000204558462234152, 0.106678679727775, 0.0686824422645692,
                                                      0.0635383280357564, 0.000979351384715302, 0.000247462228462981,
                                                      0.977038842892032, 0.978112489873993, 0.0392815660722533, 0.020177101885405,
                                                      0.00205229295579621, 0.00192946124031078, 0.989853748031195,
                                                      0.000393341936627411, 0.000345345723534104, 0.0313249887336499,
                                                      0.916355920088076, 0.0314017449795847, 0.799161283922044, 0.000190059080061623,
                                                      0.000252309357839838, 0.0378081882006202, 0.0263036532814517,
                                                      0.832552228023565, 0.870236415423323, 0.283015660719655, 0.000230455496835441,
                                                      0.00660192444160216, 0.294349738765384, 0.000304396038648286,
                                                      0.987098330356641, 0.000212362755083272, 0.150231247644022, 0.0745066283008312,
                                                      0.0988124033333366, 0.00185005388870498, 0.000299657152388178,
                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                                                    .Dim = c(31L, 4L)),
                             sutter = structure(c(0.00108970435746841,
                                                  0.00320798605540643, 0.00474241949418654, 1),
                                                .Dim = c(1L, 4L),
                                                .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl"))),
                             yolo = structure(c(0.00108970435746841, 0.00320798605540643,
                                                0.00474241949418654, 1),
                                              .Dim = c(1L, 4L),
                                              .Dimnames = list(
                                                  "Yolo Bypass", c("s", "m", "l", "vl"))),
                             delta = structure(c(0.0932457862245425,
                                                 0.998900333916521, 0.0932457862245425, 0.998900333916521,
                                                 0.0932457862245425, 0.998900333916521, 1, 1),
                                               .Dim = c(2L, 4L),
                                               .Dimnames = list(c("North Delta", "South Delta"), c("s",  "m", "l", "vl"))))
test_that("get_rearing_survival returns the expected result, stochastic = FALSE", {
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
                                   min_survival_rate = fallRunDSM::params$min_survival_rate,
                                   stochastic = FALSE)
  expect_equal(survival, rearing_survival_det)
})

# Tests migratory survival rates -----------------------------------------------
# Stochastic
expected_migratory_survival <- list(uppermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    lowermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    lower_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    sutter = structure(c(0.01, 0.01, 0.01, 1),
                                                       .Dim = c(1L, 4L),
                                                       .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    yolo = structure(c(0.01, 0.01, 0.01, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    san_joaquin = structure(c(0.393082390467511,  0.739934122927998, 0.856758760416419, 0.856758760416419),
                                                            .Dim = c(1L,  4L),
                                                            .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    delta = structure(c(0.266668614822945,
                                                        0.000123932662831837, 0.000819655793037249, 0.00566155265467863,
                                                        0.266668614822945, 0.000123932662831837, 0.000819655793037249,
                                                        0.00566155265467863, 0.266668614822945, 0.000123932662831837,
                                                        0.000819655793037249, 0.00566155265467863, 0.373914118050784,
                                                        0.000245928323835351, 0.00124096914866476, 0.0110614050155086 ),
                                                      .Dim = c(4L, 4L),
                                                      .Dimnames = list(c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"),
                                                                       c("s", "m", "l", "vl"))),
                                    bay_delta = 0.358)

test_that("get_migratory_survival returns the expected result, stochastic = TRUE", {
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
                                               ..surv_juv_outmigration_sj_int = fallRunDSM::params$..surv_juv_outmigration_sj_int,
                                               .surv_juv_outmigration_san_joaquin_medium = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_medium,
                                               .surv_juv_outmigration_san_joaquin_large = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_large,
                                               min_survival_rate = fallRunDSM::params$min_survival_rate,
                                               stochastic = TRUE) #migratory_survival$uppermid_sac)
  expect_equal(migratory_survival, expected_migratory_survival)
})
# Deterministic
expected_migratory_survival_det <- list(uppermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                        lowermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                        lower_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                        sutter = structure(c(0.0330106703577557, 0.0566390859337121, 0.0688652270321281, 1),
                                                           .Dim = c(1L, 4L), .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl"))),
                                        yolo = structure(c(0.0330106703577557, 0.0566390859337121, 0.0688652270321281, 1),
                                                         .Dim = c(1L,4L),
                                                         .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl" ))),
                                        san_joaquin = structure(c(0.393082390467511, 0.739934122927998,  0.856758760416419, 0.856758760416419),
                                                                .Dim = c(1L, 4L), .Dimnames = list( NULL, c("s", "m", "l", "vl"))),
                                        delta = structure(c(0.266668614822945,
                                                            0.000123932662831837, 0.000819655793037249, 0.00566155265467863,
                                                            0.266668614822945, 0.000123932662831837, 0.000819655793037249,
                                                            0.00566155265467863, 0.266668614822945, 0.000123932662831837,
                                                            0.000819655793037249, 0.00566155265467863, 0.373914118050784,
                                                            0.000245928323835351, 0.00124096914866476, 0.0110614050155086),
                                                          .Dim = c(4L, 4L),
                                                          .Dimnames = list(c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish" ),
                                                                           c("s", "m", "l", "vl"))),
                                        bay_delta = 0.358)

test_that("get_migratory_survival returns the expected result, stochastic = FALSE", {
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
                                               ..surv_juv_outmigration_sj_int = fallRunDSM::params$..surv_juv_outmigration_sj_int,
                                               .surv_juv_outmigration_san_joaquin_medium = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_medium,
                                               .surv_juv_outmigration_san_joaquin_large = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_large,
                                               min_survival_rate = fallRunDSM::params$min_survival_rate,
                                               stochastic = FALSE)
  expect_equal(migratory_survival, expected_migratory_survival_det)
})

