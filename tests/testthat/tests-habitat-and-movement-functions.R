library(testthat)
library(springRunDSM)
# tests for habitat and movement functions

# set timing
year <- 1
month <- 3
avg_ocean_transition_month <- 1

# tests habitat functions
# tests get_habitat
expected_habitat <- list(inchannel = c(`Upper Sacramento River` = 284862.518600717,
                                       `Antelope Creek` = 150892.043988146, `Battle Creek` = 82045.9239658986,
                                       `Bear Creek` = 104731.101330098, `Big Chico Creek` = 67785.7236296131,
                                       `Butte Creek` = 44724.0799825342, `Clear Creek` = 38107.1430306237,
                                       `Cottonwood Creek` = 645383.29795896, `Cow Creek` = 321962.223658825,
                                       `Deer Creek` = 693573.699406254, `Elder Creek` = 37322.1846943144,
                                       `Mill Creek` = 78253.2693226914, `Paynes Creek` = 71506.5145287255,
                                       `Stony Creek` = 159332.669240259, `Thomes Creek` = 123579.72814241,
                                       `Upper-mid Sacramento River` = 182988.017385245, `Sutter Bypass` = 0,
                                       `Bear River` = 127250.864421874, `Feather River` = 2183493.49627923,
                                       `Yuba River` = 15291.0798779482, `Lower-mid Sacramento River` = 80276.739386738,
                                       `Yolo Bypass` = 0, `American River` = 489755.382540208, `Lower Sacramento River` = 5659.50923988249,
                                       `Calaveras River` = 57130.9315219469, `Cosumnes River` = 194952.984087356,
                                       `Mokelumne River` = 484714.241442331, `Merced River` = 493340.623200333,
                                       `Stanislaus River` = 36376.26961304, `Tuolumne River` = 845729.47341036,
                                       `San Joaquin River` = 688450.253724245),
                         floodplain = c(`Upper Sacramento River` = 0,
                                        `Antelope Creek` = 0, `Battle Creek` = 0, `Bear Creek` = 0, `Big Chico Creek` = 0,
                                        `Butte Creek` = 122329.695068898, `Clear Creek` = 2355.46832318245,
                                        `Cottonwood Creek` = 63413.1055808076, `Cow Creek` = 0, `Deer Creek` = 0,
                                        `Elder Creek` = 21367.9751522632, `Mill Creek` = 0, `Paynes Creek` = 0,
                                        `Stony Creek` = 2900.68426339273, `Thomes Creek` = 19832.2255934245,
                                        `Upper-mid Sacramento River` = 132712.481344752, `Sutter Bypass` = 0,
                                        `Bear River` = 244256.485299771, `Feather River` = 1291949.94566363,
                                        `Yuba River` = 303137.575686449, `Lower-mid Sacramento River` = 207618.75582803,
                                        `Yolo Bypass` = 0, `American River` = 300033.514585385, `Lower Sacramento River` = 10789.5889932842,
                                        `Calaveras River` = 11047.0706784565, `Cosumnes River` = 790248.402703304,
                                        `Mokelumne River` = 80319.5503935574, `Merced River` = 0, `Stanislaus River` = 117776.359037656,
                                        `Tuolumne River` = 621732.143016127, `San Joaquin River` = 4846596.81997531
                                       ),
                         sutter = 25969285.5112023,
                         yolo = 27767654.9254387,
                         north_delta = 20327680.998477,
                         south_delta = 20685020.6986651)
test_that('The get_habitat function returns the expected values for year 1 and month 3', {
  expect_equal(get_habitat(year = year, month = month,
                           inchannel_habitat_fry = springRunDSM::params$inchannel_habitat_fry,
                           inchannel_habitat_juvenile = springRunDSM::params$inchannel_habitat_juvenile,
                           floodplain_habitat = springRunDSM::params$floodplain_habitat,
                           sutter_habitat = springRunDSM::params$sutter_habitat,
                           yolo_habitat = springRunDSM::params$yolo_habitat,
                           delta_habitat = springRunDSM::params$delta_habitat), expected_habitat)
})

# Test movement functions
# Test that we have the expected amount of ocean entry success for
migrants_at_golden_gate <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3281, 3370, 281, 866,
                                       0, 4173, 141, 3, 13810, 2580, 1228, 676, 751, 0, 1, 0, 0, 2264,
                                       37421, 14922, 0, 0, 103480, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L,4L))

expected_ocean_entry_success <- c(199, 195, 30, 44, 0, 1341, 23, 0, 839, 785, 65, 60, 57, 0,
                                 1, 0, 0, 2172, 36102, 1457, 0, 0, 6427, 0, 0, 0, 0, 0, 0, 0, 0)

test_that('The ocean entry success function returns the expected values for year 1 month 3', {
  set.seed(2021)
  expect_equal(ocean_entry_success(migrants = migrants_at_golden_gate,
                                   month = month,
                                   avg_ocean_transition_month = avg_ocean_transition_month,
                                   .ocean_entry_success_length = springRunDSM::params$.ocean_entry_success_length,
                                   ..ocean_entry_success_int = springRunDSM::params$..ocean_entry_success_int,
                                   .ocean_entry_success_months = springRunDSM::params$.ocean_entry_success_months),
               expected_ocean_entry_success)
})
