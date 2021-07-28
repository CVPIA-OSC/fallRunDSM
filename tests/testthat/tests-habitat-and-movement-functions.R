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

# Test that we have the expected amount of pulse movement for fish
expected_pulse_movement <- structure(c(0.000453375580564798, 0.00045004799330693, 0.000449971142814747,
                                       0.000450640926108725, 0.000450641147814439, 0.000450303029409245,
                                       0.000449542137770137, 0.000451037873144696, 0.000450451197899443,
                                       0.000450276810770172, 0.000450771364711169, 0.000450107816937,
                                       0.000450640926108725, 0.000537630386946598, 0.000450492791545382,
                                       0.000451214545956272, 0.000449268717327263, 0.000450465785186011,
                                       0.000451943949547694, 0.000450499258359213, 0.000449883011440903,
                                       0.000449268717327263, 0.000450794829621117, 0.000450100253818904,
                                       0.000504705084320591, 0.000450860352985386, 0.000452303640437641,
                                       0.00045481512925781, 0.000456321224039012, 0.000451700188842442,
                                       0.000452110251391907, 0.00237919146197861, 0.00237846629498965,
                                       0.00237844948665718, 0.00237859588571412, 0.00237859593413931,
                                       0.00237852205543228, 0.00237835560637642, 0.00237868255073418,
                                       0.00237855443671634, 0.00237851632445177, 0.00237862437230098,
                                       0.0023784793773933, 0.00237859588571412, 0.00239603291732538,
                                       0.00237856352490916, 0.00237872110001745, 0.0023782957282701,
                                       0.00237855762412341, 0.002378880099915, 0.00237856493783283,
                                       0.00237843020764347, 0.0023782957282701, 0.00237862949598242,
                                       0.00237847772356088, 0.00238977470104494, 0.0023786438019864,
                                       0.0023789584170467, 0.00237950359749497, 0.00237982915037264,
                                       0.00237882699082681, 0.00237891631699213, 0.000789695206963508,
                                       0.000793687165557612, 0.000793779946582285, 0.000792972217923774,
                                       0.000792971950891244, 0.00079337945341075, 0.000794298371691739,
                                       0.000792494470719371, 0.0007932008163897, 0.000793411073807132,
                                       0.000792815149891709, 0.000793614959396798, 0.000792972217923774,
                                       0.000702725721117185, 0.000793150687442467, 0.000792282063004114,
                                       0.000794629217566804, 0.000793183234804731, 0.000791406606031178,
                                       0.00079314289429399, 0.000793886379774277, 0.000794629217566804,
                                       0.000792786902670474, 0.000793624087066717, 0.000733795217043253,
                                       0.000792708038295229, 0.000790975766972648, 0.000787983491872096,
                                       0.000786202385039464, 0.000791698911797646, 0.000791207337354878,
                                       6.19310775476843e-06, 6.09180510463439e-06, 6.089476446456e-06,
                                       6.10978822076262e-06, 6.10979495038459e-06, 6.09953652312278e-06,
                                       6.07648615442911e-06, 6.1218436645128e-06, 6.10403074326343e-06,
                                       6.0987414548479e-06, 6.11374824343091e-06, 6.09361817047504e-06,
                                       6.10978822076262e-06, 9.07079632147475e-06, 6.10529268367162e-06,
                                       6.12721352042166e-06, 6.0682149823964e-06, 6.10447330130641e-06,
                                       6.14941086548879e-06, 6.10548889809303e-06, 6.0868065712358e-06,
                                       6.0682149823964e-06, 6.1144607720434e-06, 6.09338893969063e-06,
                                       7.87398249215928e-06, 6.11645067879196e-06, 6.16037340510536e-06,
                                       6.23721906626082e-06, 6.28355508560747e-06, 6.14198775126805e-06,
                                       6.15447801459402e-06), .Dim = c(31L, 4L), .Dimnames = list(c("Upper Sacramento River",
                                                                                                    "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                                                                    "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                                                                    "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                                                                    "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass",
                                                                                                    "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                                                                                    "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                                                                                    "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                                                                                    "Tuolumne River", "San Joaquin River"), c("s", "m", "l", "vl"
                                                                                                    )))

test_that('The pulse_movement function returns the expected values for year 1 month 3', {
  expect_equal(pulse_movement(springRunDSM::params$prop_pulse_flows[ , month]),
               expected_pulse_movement)
})
