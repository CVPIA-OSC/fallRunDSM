library(testthat)
library(winterRunDSM)
# tests for adult functions
# Lists inputs to use in testing
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(winterRunDSM::params$tisdale_bypass_watershed + winterRunDSM::params$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(winterRunDSM::params$migratory_temperature_proportion_over_20[ , 10:12])
accumulated_degree_days <- cbind(jan = rowSums(winterRunDSM::params$degree_days[ , 1:4, year]),
                                 feb = rowSums(winterRunDSM::params$degree_days[ , 2:4, year]),
                                 march = rowSums(winterRunDSM::params$degree_days[ , 3:4, year]),
                                 april = winterRunDSM::params$degree_days[ , 4, year])
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
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.921833381565262, `Antelope Creek` = 0.923683697482191,
                            `Battle Creek` = 0.918989018551566, `Bear Creek` = 0.911686427429973,
                            `Big Chico Creek` = 0.92746673029119, `Butte Creek` = 0.9283917315897,
                            `Clear Creek` = 0.931673539698316, `Cottonwood Creek` = 0.922669421058016,
                            `Cow Creek` = 0.922034266973123, `Deer Creek` = 0.925338898971336,
                            `Elder Creek` = 0.921614841328478, `Mill Creek` = 0.923219176714856,
                            `Paynes Creek` = 0.922016080237681, `Stony Creek` = 0.92455337220333,
                            `Thomes Creek` = 0.924364638581071, `Upper-mid Sacramento River` = 0.952574126822433,
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.928298230007684,
                            `Feather River` = 0.937505481324392, `Yuba River` = 0.918420271387002,
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433,
                            `American River` = 0.925479299116992, `Lower Sacramento River` = 0.952574126822433,
                            `Calaveras River` = 0.924152467804049, `Cosumnes River` = 0.919014268861845,
                            `Mokelumne River` = 0.918379447564648, `Merced River` = 0.916469920035829,
                            `Stanislaus River` = 0.922811061016294, `Tuolumne River` = 0.920992871793231,
                            `San Joaquin River` = 0.952574126822433)

test_that('The prespawn survival function returns the expected values for year 1', {
  expect_equal(surv_adult_prespawn(average_degree_days),
               expected_prespawn_surv)
})

# Tests egg to fry surv
expected_egg_surv <- c(`Upper Sacramento River` = 0.381246067692528, `Antelope Creek` = 0.409716513722015,
                       `Battle Creek` = 0.409716513722015, `Bear Creek` = 0.409716513722015,
                       `Big Chico Creek` = 0.410698870109287, `Butte Creek` = 0.410698870109287,
                       `Clear Creek` = 0.40873243623163, `Cottonwood Creek` = 0.412658353683433,
                       `Cow Creek` = 0.409716513722015, `Deer Creek` = 0.40873243623163,
                       `Elder Creek` = 0.413635448257544, `Mill Creek` = 0.410698870109287,
                       `Paynes Creek` = 0.409716513722015, `Stony Creek` = 0.40873243623163,
                       `Thomes Creek` = 0.410698870109287, `Upper-mid Sacramento River` = 0.395790554477812,
                       `Sutter Bypass` = 0.413635448257544, `Bear River` = 0.409716513722015,
                       `Feather River` = 0.393776360769257, `Yuba River` = 0.411679488887492,
                       `Lower-mid Sacramento River` = 0.412658353683433, `Yolo Bypass` = 0.413635448257544,
                       `American River` = 0.412658353683433, `Lower Sacramento River` = 0.412658353683433,
                       `Calaveras River` = 0.40873243623163, `Cosumnes River` = 0.411679488887492,
                       `Mokelumne River` = 0.409716513722015, `Merced River` = 0.409716513722015,
                       `Stanislaus River` = 0.409716513722015, `Tuolumne River` = 0.40873243623163,
                       `San Joaquin River` = 0.412658353683433)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(egg_to_fry_surv <- surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                                                  scour = winterRunDSM::params$prob_nest_scoured,
                                                  temperature_effect = winterRunDSM::params$..surv_egg_to_fry_mean_egg_temp_effect),
               expected_egg_surv)
})







