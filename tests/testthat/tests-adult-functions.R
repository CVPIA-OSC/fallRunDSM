library(testthat)
library(springRunDSM)
# tests for adult functions
# Lists inputs to use in testing
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(springRunDSM::params$tisdale_bypass_watershed + springRunDSM::params$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(springRunDSM::params$migratory_temperature_proportion_over_20[ , 3:6])
accumulated_degree_days <- cbind(march = rowSums(springRunDSM::params$degree_days[ , 3:6, year]),
                                 april = rowSums(springRunDSM::params$degree_days[ , 4:6, year]),
                                 may = rowSums(springRunDSM::params$degree_days[ , 5:6, year]),
                                 june = springRunDSM::params$degree_days[ , 6, year])

average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, springRunDSM::params$month_return_proportions)

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
                           natal_flow = springRunDSM::params$prop_flow_natal[ , year],
                           south_delta_watershed = springRunDSM::params$south_delta_routed_watersheds,
                           cross_channel_gates_closed = springRunDSM::params$cc_gates_days_closed[10]),
               expected_straying_output)
})

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.947091312293081, `Antelope Creek` = 0.947091312293081,
                            `Battle Creek` = 0.947091312293081, `Bear Creek` = 0.947091312293081,
                            `Big Chico Creek` = 0.947091312293081, `Butte Creek` = 0.947091312293081,
                            `Clear Creek` = 0.947091312293081, `Cottonwood Creek` = 0.947091312293081,
                            `Cow Creek` = 0.947091312293081, `Deer Creek` = 0.947091312293081,
                            `Elder Creek` = 0.947091312293081, `Mill Creek` = 0.947091312293081,
                            `Paynes Creek` = 0.947091312293081, `Stony Creek` = 0.947091312293081,
                            `Thomes Creek` = 0.947091312293081, `Upper-mid Sacramento River` = 0.951708351611047,
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.947091312293081,
                            `Feather River` = 0.947091312293081, `Yuba River` = 0.947091312293081,
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433,
                            `American River` = 0.948035342418959, `Lower Sacramento River` = 0.952574126822433,
                            `Calaveras River` = 0.948535758306968, `Cosumnes River` = 0.948535758306968,
                            `Mokelumne River` = 0.948535758306968, `Merced River` = 0.948781911699786,
                            `Stanislaus River` = 0.948781911699786, `Tuolumne River` = 0.948781911699786,
                            `San Joaquin River` = 0.952574126822433)

test_that('The adult enroute survival function returns the expected values for year 1', {
  expect_equal(surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                  bypass_overtopped = bypass_is_overtopped,
                                  adult_harvest = springRunDSM::params$adult_harvest_rate),
               expected_surv_en_route)
})

# Tests prespawn survival
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.916233382017154, `Antelope Creek` = 0.908449238634334,
                            `Battle Creek` = 0.895078401560995, `Bear Creek` = 0.882211192329061,
                            `Big Chico Creek` = 0.898666386649818, `Butte Creek` = 0.898627971647671,
                            `Clear Creek` = 0.925694019348151, `Cottonwood Creek` = 0.888483035970169,
                            `Cow Creek` = 0.887779998622643, `Deer Creek` = 0.899075216480382,
                            `Elder Creek` = 0.903894422010748, `Mill Creek` = 0.897166693462949,
                            `Paynes Creek` = 0.904784548590046, `Stony Creek` = 0.904261452864779,
                            `Thomes Creek` = 0.891546089238317, `Upper-mid Sacramento River` = 0.952574126822433,
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.917059065190712,
                            `Feather River` = 0.931278440752412, `Yuba River` = 0.901847586287811,
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433,
                            `American River` = 0.911218876020218, `Lower Sacramento River` = 0.952574126822433,
                            `Calaveras River` = 0.910456937173659, `Cosumnes River` = 0.881753431703019,
                            `Mokelumne River` = 0.908795054020212, `Merced River` = 0.895936836571233,
                            `Stanislaus River` = 0.915735676931185, `Tuolumne River` = 0.899259738897798,
                            `San Joaquin River` = 0.952574126822433)

test_that('The prespawn survival function returns the expected values for year 1', {
  expect_equal(surv_adult_prespawn(average_degree_days),
               expected_prespawn_surv)
})

# Tests egg to fry surv
expected_egg_surv <- c(`Upper Sacramento River` = 0.55126648754873, `Antelope Creek` = 0.557465511034345,
                       `Battle Creek` = 0.555249545010465, `Bear Creek` = 0.534371105617283,
                       `Big Chico Creek` = 0.537367315773288, `Butte Creek` = 0.569124433535414,
                       `Clear Creek` = 0.569292247931292, `Cottonwood Creek` = 0.506799804331753,
                       `Cow Creek` = 0.570696629269701, `Deer Creek` = 0.569325901735189,
                       `Elder Creek` = 0.539482360132187, `Mill Creek` = 0.514366344433048,
                       `Paynes Creek` = 0.534371105617283, `Stony Creek` = 0.528387818867039,
                       `Thomes Creek` = 0.535652339961507, `Upper-mid Sacramento River` = 0,
                       `Sutter Bypass` = 0, `Bear River` = 0.512991581615581, `Feather River` = 0.437030519553356,
                       `Yuba River` = 0.508044472640408, `Lower-mid Sacramento River` = 0,
                       `Yolo Bypass` = 0, `American River` = 0.539767947824993, `Lower Sacramento River` = 0,
                       `Calaveras River` = 0.511435418526615, `Cosumnes River` = 0.520695295226667,
                       `Mokelumne River` = 0.545634566287998, `Merced River` = 0.388654548325646,
                       `Stanislaus River` = 0.551201577688213, `Tuolumne River` = 0.564926181881676,
                       `San Joaquin River` = 0)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(egg_to_fry_surv <- surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                                                  scour = springRunDSM::params$prob_nest_scoured,
                                                  temperature_effect = springRunDSM::params$mean_egg_temp_effect),
               expected_egg_surv)
})







