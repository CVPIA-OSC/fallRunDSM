library(testthat)
library(fallRunDSM)
# tests for survival functions
# Lists inputs to use in testing
list2env(load_baseline_data(), envir = .GlobalEnv)

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
                             contact_points = contact_points[1],
                             prop_diversions = proportion_diverted[1],
                             total_diversions = total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = weeks_flooded[, month, year][1],
                             ..surv_juv_rear_int = betas[1, 1],
                             ..surv_juv_rear_contact_points = -0.0067662,
                             ..surv_juv_rear_prop_diversions = -0.1755,
                             ..surv_juv_rear_total_diversions = -0.0004515),
               expected_surv_juv_rear)
})

# Tests surv_juv_delta survival function
expected_delta_juv_surv <- structure(c(0.035, 1e-04, 0.035, 1e-04, 0.035, 1e-04, 0.035, 1),
                                     .Dim = c(2L, 4L),
                                     .Dimnames = list(c("North Delta", "South Delta"),
                                                      c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(avg_temp = avg_temp_delta[month, year, "North Delta"],
                              max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = delta_prop_high_predation,
                              contact_points = delta_contact_points,
                              prop_diverted = delta_proportion_diverted,
                              total_diverted = delta_total_diverted),
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
  expect_equal(surv_juv_outmigration_sac(flow_cms = upper_sacramento_flows[month, year]),
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
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = delta_inflow[month, year, ],
                                               avg_temp = avg_temp_delta[month, year, ],
                                               perc_diversions = delta_proportion_diverted * 100)[1,],
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
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = freeport_flows[month, year],
                                           vernalis_flow = vernalis_flows[month, year],
                                           stockton_flow = stockton_flows[month, year],
                                           vernalis_temperature = vernalis_temps[month, year],
                                           prisoners_point_temperature = prisoners_point_temps[month, year],
                                           CVP_exp = CVP_exports[month, year],
                                           SWP_exp = SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})

## Tests survival functions with randomness (set.seed() for testing these)
# Tests the rearing survival rates function
expected_survival <- list(inchannel = structure(c(0.756926505860367, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 0.964770280794078, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.785446246229637,
                                                  0.824933710087479, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0611581437755223,
                                                  1e-04, 1e-04, 1e-04, 0.0501438996438266, 1e-04, 1e-04, 0.931878040026851,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.991756069935398, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  0.941458354024566, 0.9539172542105, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.222495247023805, 1e-04, 1e-04, 1e-04, 0.188251208638889,
                                                  1e-04, 1e-04, 0.966395147184063, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 0.99606146444774, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 0.971270811872851, 0.977536378532867,
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.3756193306119, 1e-04, 1e-04,
                                                  1e-04, 0.327742406925741, 1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                  1, 1, 1), .Dim = c(31L, 4L)),
                          floodplain = structure(c(0.808642588247137, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.971920053418542, 1e-04,
                                                 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                 0.785446246229637, 0.979122811970951, 1e-04, 1e-04, 1e-04, 1e-04,
                                                 1e-04, 0.0611581437755223, 1e-04, 1e-04, 1e-04, 0.0501438996438266,
                                                 1e-04, 1e-04, 0.948479631384221, 1e-04, 1e-04, 1e-04, 1e-04,
                                                 1e-04, 0.993461407501329, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.941458354024566, 0.995169677417565,
                                                 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.222495247023805, 1e-04,
                                                 1e-04, 1e-04, 0.188251208638889, 1e-04, 1e-04, 0.974774175179683,
                                                 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.996878624716637, 1e-04,
                                                 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04,
                                                 0.971270811872851, 0.9976964553768, 1e-04, 1e-04, 1e-04, 1e-04,
                                                 1e-04, 0.3756193306119, 1e-04, 1e-04, 1e-04, 0.327742406925741,
                                                 1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L
                                                 )),
                          sutter = structure(c(0.01, 0.01, 0.01, 1),
                                             .Dim = c(1L, 4L),
                                             .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                          yolo = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s",  "m", "l", "vl"))),
                          delta = structure(c(0.035, 1e-04, 0.035, 1e-04,  0.035, 1e-04, 0.035, 1), .Dim = c(2L, 4L),
                                            .Dimnames = list(c("North Delta", "South Delta"), c("s", "m", "l", "vl"))))

test_that("get_rearing_survival returns the expected result", {
  set.seed(2021)
  survival <- get_rearing_survival_rates(year = year, month = month, scenario = 0)
  expect_equal(survival, expected_survival)
})

expected_migratory_survival <- list(delta = structure(c(0.266668614822945, 2.26283033759458e-26,
                                                        1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945,
                                                        2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14,
                                                        0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25,
                                                        3.67469661043849e-14, 0.373914118050784, 4.49218800782043e-26,
                                                        2.2667851513676e-25, 8.17576203365024e-14), .Dim = c(4L, 4L), .Dimnames = list(
                                                          c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish",
                                                            "southern_fish"), c("s", "m", "l", "vl"))),
                                    san_joaquin = structure(c(0.0293122307513563, 0.117118990875781, 0.218061322644411, 0.218061322644411),
                                                            .Dim = c(1L,  4L),
                                                            .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    uppermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189),
                                    lowermid_sac = c(s = 0.189,  m = 0.189, l = 0.189, vl = 0.189),
                                    lower_sac = c(s = 0.189, m = 0.189,  l = 0.189, vl = 0.189),
                                    sutter = structure(c(0.01, 0.01, 0.01, 1),
                                                       .Dim = c(1L, 4L),
                                                       .Dimnames = list(NULL, c("s", "m", "l",  "vl"))),
                                    yolo = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L,  4L),
                                                     .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                                    sac_delta = structure(c(0.361475693542451,  0.347852745753957, 0.440086951822039, 0.38940055329485, 0.521502814849562,  0.433870203071194, 0.521502814849562, 0.433870203071194),
                                                          .Dim = c(2L,  4L), .Dimnames = list(c("North Delta", "South Delta"), c("s",  "m", "l", "vl"))),
                                    bay_delta = 0.358)

test_that("get_migratory_survival returns the expected result", {
  set.seed(2021)
  migratory_survival <- get_migratory_survival_rates(year = year, month = month)
  expect_equal(migratory_survival, expected_migratory_survival)
})




