library(testthat)
library(winterRunDSM)
# tests for adult functions
# Lists inputs to use in testing
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(winterRunDSM::params$tisdale_bypass_watershed + winterRunDSM::params$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(winterRunDSM::params$migratory_temperature_proportion_over_20[ , 10:12])
accumulated_degree_days <- cbind(oct = rowSums(winterRunDSM::params$degree_days[ , 10:12, year]),
                                 nov = rowSums(winterRunDSM::params$degree_days[ , 11:12, year]),
                                 dec = winterRunDSM::params$degree_days[ , 12, year])
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
                           natal_flow = winterRunDSM::params$prop_flow_natal[ , year],
                           south_delta_watershed = winterRunDSM::params$south_delta_routed_watersheds,
                           cross_channel_gates_closed = winterRunDSM::params$cc_gates_days_closed[10]),
               expected_straying_output)
})

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.770142230517313, `Antelope Creek` = 0.970142230517313,
                            `Battle Creek` = 0.970142230517313, `Bear Creek` = 0.970142230517313,
                            `Big Chico Creek` = 0.970142230517313, `Butte Creek` = 0.970142230517313,
                            `Clear Creek` = 0.970142230517313, `Cottonwood Creek` = 0.970142230517313,
                            `Cow Creek` = 0.970142230517313, `Deer Creek` = 0.970142230517313,
                            `Elder Creek` = 0.970142230517313, `Mill Creek` = 0.970142230517313,
                            `Paynes Creek` = 0.970142230517313, `Stony Creek` = 0.970142230517313,
                            `Thomes Creek` = 0.970142230517313, `Upper-mid Sacramento River` = 0.970142230517313,
                            `Sutter Bypass` = 0.970687700961309, `Bear River` = 0.970142230517313,
                            `Feather River` = 0.970142230517313, `Yuba River` = 0.970142230517313,
                            `Lower-mid Sacramento River` = 0.970687700961309, `Yolo Bypass` = 0.970687700961309,
                            `American River` = 0.970687700961309, `Lower Sacramento River` = 0.970687700961309,
                            `Calaveras River` = 0.970528189037085, `Cosumnes River` = 0.970528189037085,
                            `Mokelumne River` = 0.970528189037085, `Merced River` = 0.970408003063303,
                            `Stanislaus River` = 0.970408003063303, `Tuolumne River` = 0.970408003063303,
                            `San Joaquin River` = 0.970687700961309)

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
                                                  scour = test_data$prob_nest_scoured,
                                                  temperature_effect = test_data$mean_egg_temp_effect),
               expected_egg_surv)
})







