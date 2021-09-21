library(testthat)
library(fallRunDSM)
# tests for adult functions (Tests in both Stochastic (by setting seed) and deterministic where appropriate)

# Define inputs to use in testing
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(fallRunDSM::params$tisdale_bypass_watershed + fallRunDSM::params$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(fallRunDSM::params$migratory_temperature_proportion_over_20[ , 10:12])
accumulated_degree_days <- cbind(oct = rowSums(fallRunDSM::params$degree_days[ , 10:12, year]),
                                 nov = rowSums(fallRunDSM::params$degree_days[ , 11:12, year]),
                                 dec = fallRunDSM::params$degree_days[ , 12, year])
average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, month_return_proportions)

# Tests adult straying function ------------------------------------------------
expected_straying_output <- c(`Upper Sacramento River` = 0.0179218144440285, `Antelope Creek` = 0.0740104504838898,
                              `Battle Creek` = 0.0678754718644023, `Bear Creek` = 0.0755029971698723,
                              `Big Chico Creek` = 0.0748474212008828, `Butte Creek` = 0.0730690238859734,
                              `Clear Creek` = 0.0709270693771585, `Cottonwood Creek` = 0.0709270693771585,
                              `Cow Creek` = 0.0704102088441598, `Deer Creek` = 0.0722674057922913,
                              `Elder Creek` = 0.0755511108365283, `Mill Creek` = 0.0717077263560781,
                              `Paynes Creek` = 0.0755029971698723, `Stony Creek` = 0.0756043218542417,
                              `Thomes Creek` = 0.0739657235650295, `Upper-mid Sacramento River` = 0.0110961379974576,
                              `Sutter Bypass` = 0.0758581800212435, `Bear River` = 0.0736293343892286,
                              `Feather River` = 0.0409341987251872, `Yuba River` = 0.0526037516322295,
                              `Lower-mid Sacramento River` = 0.0110961379974576, `Yolo Bypass` = 0.0758581800212435,
                              `American River` = 0.0579554510917732, `Lower Sacramento River` = 0.0110961379974576,
                              `Calaveras River` = 0.0110961379974576, `Cosumnes River` = 0.0110961379974576,
                              `Mokelumne River` = 0.0110961379974576, `Merced River` = 0.0429069359657317,
                              `Stanislaus River` = 0.0293554482561996, `Tuolumne River` = 0.0468862825202343,
                              `San Joaquin River` = 0.0110961379974576)

test_that('The straying function returns the expected values for year 1', {
  expect_equal(adult_stray(wild = 1,
                           natal_flow = fallRunDSM::params$prop_flow_natal[ , year],
                           south_delta_watershed = fallRunDSM::params$south_delta_routed_watersheds,
                           cross_channel_gates_closed = fallRunDSM::params$cc_gates_days_closed[10]),
               expected_straying_output)
})

# tests adult surv_en_route ----------------------------------------------------
expected_surv_en_route <- c(`Upper Sacramento River` = 0.826006723863018, `Antelope Creek` = 0.826006723863018,
                            `Battle Creek` = 0.826006723863018, `Bear Creek` = 0.826006723863018,
                            `Big Chico Creek` = 0.826006723863018, `Butte Creek` = 0.826006723863018,
                            `Clear Creek` = 0.826006723863018, `Cottonwood Creek` = 0.826006723863018,
                            `Cow Creek` = 0.826006723863018, `Deer Creek` = 0.826006723863018,
                            `Elder Creek` = 0.826006723863018, `Mill Creek` = 0.826006723863018,
                            `Paynes Creek` = 0.826006723863018, `Stony Creek` = 0.826006723863018,
                            `Thomes Creek` = 0.826006723863018, `Upper-mid Sacramento River` = 0.966006723863018,
                            `Sutter Bypass` = 0.966625146597986, `Bear River` = 0.826006723863018,
                            `Feather River` = 0.866006723863018, `Yuba River` = 0.866006723863018,
                            `Lower-mid Sacramento River` = 0.966625146597986, `Yolo Bypass` = 0.966625146597986,
                            `American River` = 0.636625146597986, `Lower Sacramento River` = 0.966625146597986,
                            `Calaveras River` = 0.866444291269305, `Cosumnes River` = 0.866444291269305,
                            `Mokelumne River` = 0.866444291269305, `Merced River` = 0.866308029319718,
                            `Stanislaus River` = 0.866308029319718, `Tuolumne River` = 0.866308029319718,
                            `San Joaquin River` = 0.966625146597986)

test_that('The adult enroute survival function returns the expected values for year 1', {
  expect_equal(surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                  bypass_overtopped = bypass_is_overtopped,
                                  adult_harvest = adult_harvest_rate),
               expected_surv_en_route)
})

# Tests prespawn survival ------------------------------------------------------
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.186695102303434, `Antelope Creek` = 0.207728478251407,
                            `Battle Creek` = 0.192399638098573, `Bear Creek` = 0.200487183703687,
                            `Big Chico Creek` = 0.19179734270666, `Butte Creek` = 0.19350167062593,
                            `Clear Creek` = 0.193727103262269, `Cottonwood Creek` = 0.195930939524092,
                            `Cow Creek` = 0.195521417995609, `Deer Creek` = 0.204045223640898,
                            `Elder Creek` = 0.206664064101252, `Mill Creek` = 0.193784921468211,
                            `Paynes Creek` = 0.206868158128987, `Stony Creek` = 0.189234555095818,
                            `Thomes Creek` = 0.189808459854109, `Upper-mid Sacramento River` = 0.257033209964195,
                            `Sutter Bypass` = 0.257033209964195, `Bear River` = 0.212143510649751,
                            `Feather River` = 0.226132808106689, `Yuba River` = 0.18242790055379,
                            `Lower-mid Sacramento River` = 0.257033209964195, `Yolo Bypass` = 0.257033209964195,
                            `American River` = 0.185927498942418, `Lower Sacramento River` = 0.257033209964195,
                            `Calaveras River` = 0.206567391969973, `Cosumnes River` = 0.187423805906952,
                            `Mokelumne River` = 0.169246852423638, `Merced River` = 0.177494211948965,
                            `Stanislaus River` = 0.184201295731171, `Tuolumne River` = 0.177270306163727,
                            `San Joaquin River` = 0.257033209964195)

test_that('The prespawn survival function returns the expected values for year 1', {
  expect_equal(surv_adult_prespawn(average_degree_days),
               expected_prespawn_surv)
})

# Tests egg to fry surv --------------------------------------------------------
expected_egg_surv <- c(`Upper Sacramento River` = 0.877201576428728, `Antelope Creek` = 0.860646516060177,
                       `Battle Creek` = 0.848866663369314, `Bear Creek` = 0.823190565958985,
                       `Big Chico Creek` = 0.827751089442284, `Butte Creek` = 0.87793701713448,
                       `Clear Creek` = 0.880662494488553, `Cottonwood Creek` = 0.775774207591575,
                       `Cow Creek` = 0.881073458332733, `Deer Creek` = 0.881354822847874,
                       `Elder Creek` = 0.823699871202999, `Mill Creek` = 0.792749575667186,
                       `Paynes Creek` = 0.823344026309075, `Stony Creek` = 0.815804967995425,
                       `Thomes Creek` = 0.823319086528136, `Upper-mid Sacramento River` = 0,
                       `Sutter Bypass` = 0, `Bear River` = 0.793732723775642, `Feather River` = 0.849869084494623,
                       `Yuba River` = 0.862064072688289, `Lower-mid Sacramento River` = 0,
                       `Yolo Bypass` = 0, `American River` = 0.823878626970645, `Lower Sacramento River` = 0,
                       `Calaveras River` = 0.79247536158286, `Cosumnes River` = 0.801849202166723,
                       `Mokelumne River` = 0.836082064528413, `Merced River` = 0.595654851966305,
                       `Stanislaus River` = 0.84529097804511, `Tuolumne River` = 0.871243026400488,
                       `San Joaquin River` = 0)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(egg_to_fry_surv <- surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                                                  scour = fallRunDSM::params$prob_nest_scoured,
                                                  temperature_effect = fallRunDSM::params$mean_egg_temp_effect),
               expected_egg_surv)
})

# Test get_spawning_adults -----------------------------------------------------
adults <- structure(c(22012, 72, 12626, 12, 12, 885, 8555, 1251, 1649,
                      569, 12, 1332, 51, 12, 12, 0, 0, 12, 52408, 7184, 0, 0, 24959,
                      0, 12, 499, 4514, 2145, 5405, 984, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0),
                    .Dim = 31:30,
                    .Dimnames = list(c("Upper Sacramento River",
                                       "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                       "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                       "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                       "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass",
                                       "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                       "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                       "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                       "Tuolumne River", "San Joaquin River"), NULL))
hatch_adults <- c(`Upper Sacramento River` = 6926L, `Antelope Creek` = 13L, `Battle Creek` = 16007L,
                  `Bear Creek` = 15L, `Big Chico Creek` = 18L, `Butte Creek` = 80L,
                  `Clear Creek` = 1082L, `Cottonwood Creek` = 914L, `Cow Creek` = 274L,
                  `Deer Creek` = 132L, `Elder Creek` = 15L, `Mill Creek` = 66L,
                  `Paynes Creek` = 25L, `Stony Creek` = 16L, `Thomes Creek` = 13L,
                  `Upper-mid Sacramento River` = 0L, `Sutter Bypass` = 0L, `Bear River` = 21L,
                  `Feather River` = 50417L, `Yuba River` = 8136L, `Lower-mid Sacramento River` = 0L,
                  `Yolo Bypass` = 0L, `American River` = 14083L, `Lower Sacramento River` = 0L,
                  `Calaveras River` = 28L, `Cosumnes River` = 14L, `Mokelumne River` = 2926L,
                  `Merced River` = 1402L, `Stanislaus River` = 1506L, `Tuolumne River` = 475L,
                  `San Joaquin River` = 0L)
seeds <- NULL

# stochastic
expected_spawners <- list(init_adults = c(22012, 72, 3171, 12, 12, 885, 8555, 1251,
                                          1649, 569, 12, 1332, 51, 12, 12, 0, 0, 12, 39562, 7184, 0, 0,
                                          19284, 0, 12, 499, 2747, 1526, 5405, 984, 0),
                          proportion_natural = c(0.63,
                                                 0.8, 0.1, 0.62031746031746, 0.8, 0.885, 0.7775, 0.6475, 0.8,
                                                 0.84, 0.62031746031746, 0.8475, 0.635, 0.62031746031746, 0.62031746031746,
                                                 0.62031746031746, 0.62031746031746, 1, 0.145, 0.46, 0.62031746031746,
                                                 0.62031746031746, 0.428333333333333, 0.62031746031746, 0.923333333333333,
                                                 0.98, 0.2425, 0.255, 0.295, 0.535, 0.62031746031746),
                          init_adults_by_month = structure(c(4867L,
                                                             17L, 741L, 3L, 4L, 195L, 1839L, 277L, 331L, 139L, 1L, 287L, 15L,
                                                             1L, 3L, 0L, 0L, 1L, 8886L, 1611L, 0L, 0L, 4280L, 0L, 1L, 118L,
                                                             574L, 353L, 1223L, 227L, 0L, 12175L, 38L, 1727L, 6L, 7L, 467L,
                                                             4777L, 724L, 971L, 328L, 6L, 764L, 31L, 6L, 6L, 0L, 0L, 8L, 21864L,
                                                             4016L, 0L, 0L, 10714L, 0L, 7L, 281L, 1572L, 841L, 3053L, 516L,
                                                             0L, 4970L, 17L, 703L, 3L, 1L, 223L, 1939L, 250L, 347L, 102L,
                                                             5L, 281L, 5L, 5L, 3L, 0L, 0L, 3L, 8812L, 1557L, 0L, 0L, 4290L,
                                                             0L, 4L, 100L, 601L, 332L, 1129L, 241L, 0L),
                                                           .Dim = c(31L, 3L)))
test_that("stochastic get spawning adults returns the expected values, stochastic = TRUE", {
  set.seed(2021)
  spawning_adults <- get_spawning_adults(year = year, adults = adults,
                                         hatch_adults = hatch_adults,
                                         month_return_proportions = params$month_return_proportions,
                                         natural_adult_removal_rate = fallRunDSM::params$natural_adult_removal_rate,
                                         mode = "seed",
                                         prop_flow_natal = fallRunDSM::params$prop_flow_natal,
                                         south_delta_routed_watersheds = fallRunDSM::params$south_delta_routed_watersheds,
                                         cc_gates_days_closed = fallRunDSM::params$cc_gates_days_closed,
                                         gates_overtopped = fallRunDSM::params$gates_overtopped,
                                         tisdale_bypass_watershed = fallRunDSM::params$tisdale_bypass_watershed,
                                         yolo_bypass_watershed = fallRunDSM::params$yolo_bypass_watershed,
                                         migratory_temperature_proportion_over_20 = fallRunDSM::params$migratory_temperature_proportion_over_20,
                                         ..surv_adult_enroute_int = fallRunDSM::params$..surv_adult_enroute_int,
                                         .adult_stray_intercept = fallRunDSM::params$.adult_stray_intercept,
                                         .adult_stray_wild = fallRunDSM::params$.adult_stray_wild,
                                         .adult_stray_natal_flow = fallRunDSM::params$.adult_stray_natal_flow,
                                         .adult_stray_cross_channel_gates_closed = fallRunDSM::params$.adult_stray_cross_channel_gates_closed,
                                         .adult_stray_prop_bay_trans = fallRunDSM::params$.adult_stray_prop_bay_trans,
                                         .adult_stray_prop_delta_trans = fallRunDSM::params$.adult_stray_prop_delta_trans,
                                         .adult_en_route_migratory_temp = fallRunDSM::params$.adult_en_route_migratory_temp,
                                         .adult_en_route_bypass_overtopped = fallRunDSM::params$.adult_en_route_bypass_overtopped,
                                         .adult_en_route_adult_harvest_rate = fallRunDSM::params$.adult_en_route_adult_harvest_rate,
                                         stochastic = TRUE)
  expect_equal(spawning_adults, expected_spawners)
})

# deterministic
expected_spawners_det <- list(init_adults = c(22013, 72, 3212, 13, 13, 886, 8555, 1251,
                                              1648, 568, 13, 1332, 50, 13, 13, 0, 0, 13, 39605, 7183, 0, 0,
                                              19239, 0, 13, 499, 2733, 1558, 5405, 985, 0),
                              proportion_natural = c(0.63,
                                                     0.8, 0.1, 0.62031746031746, 0.8, 0.885, 0.7775, 0.6475, 0.8,
                                                     0.84, 0.62031746031746, 0.8475, 0.635, 0.62031746031746, 0.62031746031746,
                                                     0.62031746031746, 0.62031746031746, 1, 0.145, 0.46, 0.62031746031746,
                                                     0.62031746031746, 0.428333333333333, 0.62031746031746, 0.923333333333333,
                                                     0.98, 0.2425, 0.255, 0.295, 0.535, 0.62031746031746),
                              init_adults_by_month = structure(c(4892,
                                                                 16, 714, 3, 3, 197, 1901, 278, 366, 126, 3, 296, 11, 3, 3, 0,
                                                                 0, 3, 8801, 1596, 0, 0, 4275, 0, 3, 111, 607, 346, 1201, 219,
                                                                 0, 12229, 40, 1784, 7, 7, 492, 4753, 695, 916, 316, 7, 740, 28,
                                                                 7, 7, 0, 0, 7, 22003, 3991, 0, 0, 10689, 0, 7, 277, 1519, 866,
                                                                 3003, 547, 0, 4892, 16, 714, 3, 3, 197, 1901, 278, 366, 126,
                                                                 3, 296, 11, 3, 3, 0, 0, 3, 8801, 1596, 0, 0, 4275, 0, 3, 111,
                                                                 607, 346, 1201, 219, 0), .Dim = c(31L, 3L)))

test_that("deterministic get spawning adults returns the expected values, stochastic = FALSE", {
  spawning_adults <- get_spawning_adults(year = year, adults = adults,
                                         hatch_adults = hatch_adults,
                                         month_return_proportions = params$month_return_proportions,
                                         natural_adult_removal_rate = fallRunDSM::params$natural_adult_removal_rate,
                                         mode = "seed",
                                         prop_flow_natal = fallRunDSM::params$prop_flow_natal,
                                         south_delta_routed_watersheds = fallRunDSM::params$south_delta_routed_watersheds,
                                         cc_gates_days_closed = fallRunDSM::params$cc_gates_days_closed,
                                         gates_overtopped = fallRunDSM::params$gates_overtopped,
                                         tisdale_bypass_watershed = fallRunDSM::params$tisdale_bypass_watershed,
                                         yolo_bypass_watershed = fallRunDSM::params$yolo_bypass_watershed,
                                         migratory_temperature_proportion_over_20 = fallRunDSM::params$migratory_temperature_proportion_over_20,
                                         ..surv_adult_enroute_int = fallRunDSM::params$..surv_adult_enroute_int,
                                         .adult_stray_intercept = fallRunDSM::params$.adult_stray_intercept,
                                         .adult_stray_wild = fallRunDSM::params$.adult_stray_wild,
                                         .adult_stray_natal_flow = fallRunDSM::params$.adult_stray_natal_flow,
                                         .adult_stray_cross_channel_gates_closed = fallRunDSM::params$.adult_stray_cross_channel_gates_closed,
                                         .adult_stray_prop_bay_trans = fallRunDSM::params$.adult_stray_prop_bay_trans,
                                         .adult_stray_prop_delta_trans = fallRunDSM::params$.adult_stray_prop_delta_trans,
                                         .adult_en_route_migratory_temp = fallRunDSM::params$.adult_en_route_migratory_temp,
                                         .adult_en_route_bypass_overtopped = fallRunDSM::params$.adult_en_route_bypass_overtopped,
                                         .adult_en_route_adult_harvest_rate = fallRunDSM::params$.adult_en_route_adult_harvest_rate,
                                         stochastic = FALSE)
  expect_equal(spawning_adults, expected_spawners_det)
})
# Tests spawn success function -------------------------------------------------
# Stochastic
init_adults <- expected_spawners$init_adults
min_spawn_habitat <- apply(params$spawning_habitat[ , 10:12, year], 1, min)

expected_juveniles <- structure(c(8095521, 41069, 749968, 8714, 4458, 376221, 3815753,
                                  512732, 831592, 341652, 0, 552172, 39434, 8554, 13198, 0, 0,
                                  8534, 16338602, 2818079, 0, 0, 8268153, 0, 12523, 182193, 1090560,
                                  467351, 2223517, 443537, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                .Dim = c(31L, 4L),
                                .Dimnames = list(NULL, c("fry", "", "", "")))

test_that("stochastic spawn success function returns the expected value, stochastic = TRUE", {
  set.seed(2021) # need to set seed since stochastic = TRUE
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = fallRunDSM::params$prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat,
                             stochastic = TRUE)
  expect_equal(round(juveniles, 2), round(expected_juveniles, 2))
})

# Deterministic
expected_juveniles <- structure(c(8162468, 31937, 750352, 4364, 4434, 404418, 3829870,
                                  521641, 751979, 268163, 4548, 547764, 21823, 4280, 4410, 0, 0,
                                  4208, 16793349, 3055648, 0, 0, 8075606, 0, 4157, 203945, 1028264,
                                  426281, 2231532, 397629, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                .Dim = c(31L,  4L),
                                .Dimnames = list(c("Upper Sacramento River", "Antelope Creek",
                                                   "Battle Creek", "Bear Creek", "Big Chico Creek", "Butte Creek",
                                                   "Clear Creek", "Cottonwood Creek", "Cow Creek", "Deer Creek",
                                                   "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                                                   "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                                                   "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                                   "Tuolumne River", "San Joaquin River"), c("fry", "", "", "")))

test_that("deterministic - spawn success function returns the expected value, stochastic = FALSE", {
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = fallRunDSM::params$prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat,
                             stochastic = FALSE)
  expect_equal(round(juveniles, 2), round(expected_juveniles, 2))
})


