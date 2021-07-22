library(testthat)
library(fallRunDSM)
# tests for adult functions
# Lists inputs to use in testing
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(fallRunDSM::params$tisdale_bypass_watershed + fallRunDSM::params$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(fallRunDSM::params$migratory_temperature_proportion_over_20[ , 10:12])
accumulated_degree_days <- cbind(oct = rowSums(fallRunDSM::params$degree_days[ , 10:12, year]),
                                 nov = rowSums(fallRunDSM::params$degree_days[ , 11:12, year]),
                                 dec = fallRunDSM::params$degree_days[ , 12, year])
average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, month_return_proportions)

# Tests adult straying function
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

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.811708351611047, `Antelope Creek` = 0.811708351611047,
                            `Battle Creek` = 0.811708351611047, `Bear Creek` = 0.811708351611047,
                            `Big Chico Creek` = 0.811708351611047, `Butte Creek` = 0.811708351611047,
                            `Clear Creek` = 0.811708351611047, `Cottonwood Creek` = 0.811708351611047,
                            `Cow Creek` = 0.811708351611047, `Deer Creek` = 0.811708351611047,
                            `Elder Creek` = 0.811708351611047, `Mill Creek` = 0.811708351611047,
                            `Paynes Creek` = 0.811708351611047, `Stony Creek` = 0.811708351611047,
                            `Thomes Creek` = 0.811708351611047, `Upper-mid Sacramento River` = 0.951708351611047,
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.811708351611047,
                            `Feather River` = 0.851708351611047, `Yuba River` = 0.851708351611047,
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433,
                            `American River` = 0.622574126822433, `Lower Sacramento River` = 0.952574126822433,
                            `Calaveras River` = 0.852320885972597, `Cosumnes River` = 0.852320885972597,
                            `Mokelumne River` = 0.852320885972597, `Merced River` = 0.852130112901254,
                            `Stanislaus River` = 0.852130112901254, `Tuolumne River` = 0.852130112901254,
                            `San Joaquin River` = 0.952574126822433)

test_that('The adult enroute survival function returns the expected values for year 1', {
  expect_equal(surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                  bypass_overtopped = bypass_is_overtopped,
                                  adult_harvest = adult_harvest_rate),
               expected_surv_en_route)
})

# Tests prespawn survival
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.930203323682749, `Antelope Creek` = 0.93835716913923,
                            `Battle Creek` = 0.932576236368561, `Bear Creek` = 0.935727582051041,
                            `Big Chico Creek` = 0.932331806687191, `Butte Creek` = 0.933019862449048,
                            `Clear Creek` = 0.933110040704823, `Cottonwood Creek` = 0.933981593081506,
                            `Cow Creek` = 0.933821003367044, `Deer Creek` = 0.937041118846014,
                            `Elder Creek` = 0.937981289865876, `Mill Creek` = 0.933133138318533,
                            `Paynes Creek` = 0.938053638431211, `Stony Creek` = 0.931275828832128,
                            `Thomes Creek` = 0.931514570483139, `Upper-mid Sacramento River` = 0.952574126822433,
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.939879063993009,
                            `Feather River` = 0.944337042511458, `Yuba River` = 0.928339777198531,
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433,
                            `American River` = 0.929873866760164, `Lower Sacramento River` = 0.952574126822433,
                            `Calaveras River` = 0.937946974912232, `Cosumnes River` = 0.930513801329565,
                            `Mokelumne River` = 0.922045673822907, `Merced River` = 0.926083513871507,
                            `Stanislaus River` = 0.929123817753324, `Tuolumne River` = 0.925978406656304,
                            `San Joaquin River` = 0.952574126822433)

test_that('The prespawn survival function returns the expected values for year 1', {
  expect_equal(surv_adult_prespawn(average_degree_days),
               expected_prespawn_surv)
})

# Tests egg to fry surv
expected_egg_surv <- c(`Upper Sacramento River` = 0.508283676958069, `Antelope Creek` = 0.535399956490784,
                       `Battle Creek` = 0.453066456696592, `Bear Creek` = 0.493791973234685,
                       `Big Chico Creek` = 0.516182007507822, `Butte Creek` = 0.556564999873063,
                       `Clear Creek` = 0.544088820855689, `Cottonwood Creek` = 0.471535406023962,
                       `Cow Creek` = 0.548107361679654, `Deer Creek` = 0.551271274174912,
                       `Elder Creek` = 0.49914209957619, `Mill Creek` = 0.498951249626129,
                       `Paynes Creek` = 0.49539261838608, `Stony Creek` = 0.488109078491193,
                       `Thomes Creek` = 0.495131822625042, `Upper-mid Sacramento River` = 0,
                       `Sutter Bypass` = 0, `Bear River` = 0.512991581615581, `Feather River` = 0.437030519553356,
                       `Yuba River` = 0.502413166776223, `Lower-mid Sacramento River` = 0,
                       `Yolo Bypass` = 0, `American River` = 0.478141299898216, `Lower Sacramento River` = 0,
                       `Calaveras River` = 0.503706109834966, `Cosumnes River` = 0.518675616972662,
                       `Mokelumne River` = 0.461524353396851, `Merced River` = 0.329759126931278,
                       `Stanislaus River` = 0.472280880371357, `Tuolumne River` = 0.511951882200814,
                       `San Joaquin River` = 0)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(egg_to_fry_surv <- surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                                                  scour = fallRunDSM::params$prob_nest_scoured,
                                                  temperature_effect = fallRunDSM::params$mean_egg_temp_effect),
               expected_egg_surv)
})

# Test get_spawning_adults
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
                      0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 31:30, .Dimnames = list(c("Upper Sacramento River",
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

expected_spawners <- list(init_adults = c(22012, 72, 12626, 12, 12, 885, 8555, 1251,
                                          1649, 569, 12, 1332, 51, 12, 12, 0, 0, 12, 52408, 7184, 0, 0,
                                          24959, 0, 12, 499, 4514, 2145, 5405, 984, 0),
                          proportion_natural = c(`Upper Sacramento River` = 0.63,
                                                 `Antelope Creek` = 0.8, `Battle Creek` = 0.1, `Bear Creek` = 0.62031746031746,
                                                 `Big Chico Creek` = 0.8, `Butte Creek` = 0.885, `Clear Creek` = 0.7775,
                                                 `Cottonwood Creek` = 0.6475, `Cow Creek` = 0.8, `Deer Creek` = 0.84,
                                                 `Elder Creek` = 0.62031746031746, `Mill Creek` = 0.8475, `Paynes Creek` = 0.635,
                                                 `Stony Creek` = 0.62031746031746, `Thomes Creek` = 0.62031746031746,
                                                 `Upper-mid Sacramento River` = 0.62031746031746, `Sutter Bypass` = 0.62031746031746,
                                                 `Bear River` = 1, `Feather River` = 0.145, `Yuba River` = 0.46,
                                                 `Lower-mid Sacramento River` = 0.62031746031746, `Yolo Bypass` = 0.62031746031746,
                                                 `American River` = 0.428333333333333, `Lower Sacramento River` = 0.62031746031746,
                                                 `Calaveras River` = 0.923333333333333, `Cosumnes River` = 0.98,
                                                 `Mokelumne River` = 0.2425, `Merced River` = 0.255, `Stanislaus River` = 0.295,
                                                 `Tuolumne River` = 0.535, `San Joaquin River` = 0.62031746031746
                          ),
                          natural_adults = c(22012, 72, 3171, 12, 12, 885, 8555, 1251,
                                             1649, 569, 12, 1332, 51, 12, 12, 0, 0, 12, 39562, 7184, 0, 0,
                                             19284, 0, 12, 499, 2747, 1526, 5405, 984, 0),
                          init_adults_by_month = structure(c(4867L,
                                                             17L, 741L, 3L, 4L, 195L, 1839L, 277L, 331L, 139L, 1L, 287L, 15L,
                                                             1L, 3L, 0L, 0L, 1L, 8886L, 1611L, 0L, 0L, 4280L, 0L, 1L, 118L,
                                                             574L, 353L, 1223L, 227L, 0L, 12175L, 38L, 1727L, 6L, 7L, 467L,
                                                             4777L, 724L, 971L, 328L, 6L, 764L, 31L, 6L, 6L, 0L, 0L, 8L, 21864L,
                                                             4016L, 0L, 0L, 10714L, 0L, 7L, 281L, 1572L, 841L, 3053L, 516L,
                                                             0L, 4970L, 17L, 703L, 3L, 1L, 223L, 1939L, 250L, 347L, 102L,
                                                             5L, 281L, 5L, 5L, 3L, 0L, 0L, 3L, 8812L, 1557L, 0L, 0L, 4290L,
                                                             0L, 4L, 100L, 601L, 332L, 1129L, 241L, 0L), .Dim = c(31L, 3L)))
test_that("Get spawning adults returns the expected values", {

  set.seed(2021)
  spawning_adults <- get_spawning_adults(year = year, adults = adults,
                                         hatch_adults = hatch_adults,
                                         month_return_proportions = params$month_return_proportions,
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
                                         .adult_en_route_adult_harvest_rate = fallRunDSM::params$.adult_en_route_adult_harvest_rate)
  expect_equal(spawning_adults, expected_spawners)
})

# Tests spawn success function
init_adults <- expected_spawners$init_adults
min_spawn_habitat <- apply(params$spawning_habitat[ , 10:12, year], 1, min)

expected_juveniles <- structure(c(63013983, 17382827, 14181724, 12267298, 11294498,
                                  171886959, 15665034, 1490579, 58531016, 22030799, 5360762, 13988979,
                                  8495281, 17170683, 26113261, 0, 0, 1808535, 47588280, 32767726,
                                  0, 0, 285959988, 0, 6277630, 14431357, 12732900, 22170279, 14001095,
                                  49874514, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L, 4L), .Dimnames = list(
                                    NULL, c("fry", "", "", "")))

test_that("spawn success function returns the expected value", {
  set.seed(2021)
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = fallRunDSM::params$prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat)
  expect_equal(round(juveniles, 2), round(expected_juveniles, 2))
})





