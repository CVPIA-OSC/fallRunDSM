library(testthat)
library(winterRunDSM)
# tests for habitat and movement functions

# set timing
year <- 1
month <- 3
avg_ocean_transition_month <- 1

# tests habitat functions
# tests get_habitat
expected_habitat <- list(inchannel = c(`Upper Sacramento River` = 284862.518600717,
                                       `Antelope Creek` = 130539.514430651, `Battle Creek` = 35411.3612203755,
                                       `Bear Creek` = 99332.1925984989, `Big Chico Creek` = 60877.1108048435,
                                       `Butte Creek` = 25419.8800567722, `Clear Creek` = 39673.4301698992,
                                       `Cottonwood Creek` = 214588.229687703, `Cow Creek` = 530908.32572545,
                                       `Deer Creek` = 167761.246120804, `Elder Creek` = 30523.0895996864,
                                       `Mill Creek` = 80418.9361700345, `Paynes Creek` = 67820.3397367831,
                                       `Stony Creek` = 135409.432938176, `Thomes Creek` = 137034.606221835,
                                       `Upper-mid Sacramento River` = 182988.017385245, `Sutter Bypass` = 0,
                                       `Bear River` = 154815.346333696, `Feather River` = 510013.079422886,
                                       `Yuba River` = 69083.0399180935, `Lower-mid Sacramento River` = 80276.739386738,
                                       `Yolo Bypass` = 0, `American River` = 230167.377977434, `Lower Sacramento River` = 5659.50923988249,
                                       `Calaveras River` = 18714.9650757743, `Cosumnes River` = 126615.258654402,
                                       `Mokelumne River` = 370503.431387004, `Merced River` = 461669.519070154,
                                       `Stanislaus River` = 43919.4455190144, `Tuolumne River` = 476899.452625285,
                                       `San Joaquin River` = 548255.447803655),
                         floodplain = c(`Upper Sacramento River` = 0,
                                        `Antelope Creek` = 0, `Battle Creek` = 0, `Bear Creek` = 0, `Big Chico Creek` = 0,
                                        `Butte Creek` = 112771.801056231, `Clear Creek` = 1461.96960806135,
                                        `Cottonwood Creek` = 44204.7638931555, `Cow Creek` = 0, `Deer Creek` = 0,
                                        `Elder Creek` = 21367.9751522632, `Mill Creek` = 0, `Paynes Creek` = 0,
                                        `Stony Creek` = 2900.68426339273, `Thomes Creek` = 19832.2255934245,
                                        `Upper-mid Sacramento River` = 132712.481344752, `Sutter Bypass` = 0,
                                        `Bear River` = 244256.485299771, `Feather River` = 1291949.94566363,
                                        `Yuba River` = 303137.575686449, `Lower-mid Sacramento River` = 207618.75582803,
                                        `Yolo Bypass` = 0, `American River` = 300033.514585385, `Lower Sacramento River` = 10789.5889932842,
                                        `Calaveras River` = 11047.0706784565, `Cosumnes River` = 790248.402703304,
                                        `Mokelumne River` = 80319.5503935574, `Merced River` = 0, `Stanislaus River` = 117776.359037656,
                                        `Tuolumne River` = 621732.143016127, `San Joaquin River` = 4846596.81997531
                         ), sutter = 25969285.5112023, yolo = 27767654.9254387, north_delta = 20327680.998477,
                         south_delta = 20685020.6986651)
test_that('The get_habitat function returns the expected values for year 1 and month 3', {
  expect_equal(get_habitat(year = year, month = month,
                           inchannel_habitat_fry = winterRunDSM::params$inchannel_habitat_fry,
                           inchannel_habitat_juvenile = winterRunDSM::params$inchannel_habitat_juvenile,
                           floodplain_habitat = winterRunDSM::params$floodplain_habitat,
                           sutter_habitat = winterRunDSM::params$sutter_habitat,
                           yolo_habitat = winterRunDSM::params$yolo_habitat,
                           delta_habitat = winterRunDSM::params$delta_habitat), expected_habitat)
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

expected_ocean_entry_success <- c(320, 309, 29, 80, 0, 420, 14, 1, 1352, 275, 145, 66, 60, 0,
                                  0, 0, 0, 198, 3760, 1536, 0, 0, 10096, 0, 0, 0, 0, 0, 0, 0, 0
)

test_that('The ocean entry success function returns the expected values for year 1 month 3', {
  set.seed(2021)
  expect_equal(ocean_entry_success(migrants = migrants_at_golden_gate,
                                   month = month,
                                   avg_ocean_transition_month = avg_ocean_transition_month,
                                   .ocean_entry_success_length = winterRunDSM::params$.ocean_entry_success_length,
                                   ..ocean_entry_success_int = winterRunDSM::params$..ocean_entry_success_int,
                                   .ocean_entry_success_months = winterRunDSM::params$.ocean_entry_success_months),
               expected_ocean_entry_success)
})

