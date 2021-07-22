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
  expect_equal(pulse_movement(winterRunDSM::params$prop_pulse_flows[ , month]),
               expected_pulse_movement)
})
# Test routing functions
juveniles <- structure(c(23562632.6818975, 95877.4873630614, 400486.276147004,
                         14696.4466670538, 15466.5250672724, 1230801.99830721, 11392333.5307564,
                         1505953.40992132, 2237111.18034545, 770950.136852591, 15511.9369082167,
                         1660902.32553158, 62818.1321951005, 14307.5895999285, 14822.7873816012,
                         0, 0, 15335.6115608913, 26878201.5840594, 9066230.94759099, 0,
                         0, 23405376.6695231, 0, 14870.5408311819, 651645.917994898, 3060092.04898684,
                         1231967.55285853, 6286476.48377481, 1223531.384084, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0), .Dim = c(31L, 4L),
                       .Dimnames = list(c("Upper Sacramento River", "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                          "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                          "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                          "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass",
                                          "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                          "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                          "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                          "Tuolumne River", "San Joaquin River"), c("fry", "", "", "")))

expected_route <- list(inchannel = structure(c(5706730, 95829.4873630614, 400327.276147004,
                                               14687.4466670538, 15453.5250672724, 0, 794819, 619698.40992132,
                                               2236113.18034545, 770607.136852591, 0, 1611117, 62795.1321951005,
                                               0, 0, 0, 0, 0, 984163.584059399, 1383970, 0, 0, 4611031, 0, 0,
                                               0, 1449701.04898684, 1231436.55285853, 879869, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0), .Dim = c(31L, 4L)),
                       floodplain = structure(c(0,
                                                0, 0, 0, 0, 1230801.99830721, 29301, 885965, 0, 0, 15511.9369082167,
                                                0, 0, 14307.5895999285, 14822.7873816012, 0, 0, 15335.6115608913,
                                                25893645, 6075573, 0, 0, 6013361, 0, 14870.5408311819, 651645.917994898,
                                                1609788, 0, 2360509, 1223531.384084, 0, 0, 0, 0, 0, 0, 0, 0,
                                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                0, 0), .Dim = c(31L, 4L)),
                       migrants = structure(c(17855902.6818975,
                                              48, 159, 9, 13, 0, 10568213.5307564, 290, 998, 343, 0, 49785.32553158,
                                              23, 0, 0, 0, 0, 0, 393, 1606687.94759099, 0, 0, 12780984.6695231,
                                              0, 0, 0, 603, 531, 3046098.48377481, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0), .Dim = c(31L, 4L), .Dimnames = list(c("Upper Sacramento River",
                                                                                              "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                                                              "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                                                              "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                                                              "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass",
                                                                                              "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                                                                              "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                                                                              "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                                                                              "Tuolumne River", "San Joaquin River"), c("fry", "", "", ""))))

test_that('The route() function returns the expected values for year 1 month 3', {
  set.seed(2021)
  route <- route(year = year, month = month,
                 juveniles = juveniles,
                 inchannel_habitat = expected_habitat$inchannel,
                 floodplain_habitat = expected_habitat$floodplain,
                 prop_pulse_flows = winterRunDSM::params$prop_pulse_flows,
                 .pulse_movement_intercept = winterRunDSM::params$.pulse_movement_intercept,
                 .pulse_movement_proportion_pulse = winterRunDSM::params$.pulse_movement_proportion_pulse,
                 .pulse_movement_medium = winterRunDSM::params$.pulse_movement_medium,
                 .pulse_movement_large = winterRunDSM::params$.pulse_movement_large,
                 .pulse_movement_vlarge = winterRunDSM::params$.pulse_movement_vlarge,
                 .pulse_movement_medium_pulse = winterRunDSM::params$.pulse_movement_medium_pulse,
                 .pulse_movement_large_pulse = winterRunDSM::params$.pulse_movement_large_pulse,
                 .pulse_movement_very_large_pulse = winterRunDSM::params$.pulse_movement_very_large_pulse,
                 territory_size = winterRunDSM::params$territory_size)
  expect_equal(route, expected_route)
})

# Test the route bypass function
sutter_fish <- structure(c(12298980, 0, 50880, 0, 10, 0, 6144095, 378, 0, 0,
                           0, 276, 0, 3, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(15L, 4L), .Dimnames = list(
                             c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                               "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                               "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                               "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek"
                             ), c("s", "m", "l", "vl")))
migratory_survival <- list(delta = structure(c(0.266668614822945, 2.26283033759458e-26,
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
expected_route_sutter <- list(inchannel = structure(c(12298980, 0, 50880, 0, 10, 0, 6144095,
                                                      378, 0, 0, 0, 276, 0, 3, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(15L, 4L), .Dimnames = list(
                                                        c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                                                          "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                                                          "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                                                          "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek"
                                                        ), c("s", "m", "l", "vl"))),
                              migrants = structure(c(0L, 0L,
                                                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), .Dim = c(15L, 4L), .Dimnames = list(
                                                       NULL, c("s", "m", "l", "vl"))))

test_that('The route_bypass() function returns the expected values for year 1 month 3', {
  set.seed(2021)
  route_sutter <- route_bypass(bypass_fish = sutter_fish,
                               bypass_habitat = expected_habitat$sutter,
                               migration_survival_rate = migratory_survival$sutter,
                               territory_size = winterRunDSM::params$territory_size)
  expect_equal(route_sutter, expected_route_sutter)
})

# Test the route regional function
expected_route_regional <- list(inchannel = structure(c(2437809, 0, 10084, 0, 2, 0, 1217820,
                                                        75, 0, 0, 0, 55, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(15L, 4L), .Dimnames = list(
                                                          c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                                                            "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                                                            "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                                                            "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek"
                                                          ), c("s", "m", "l", "vl"))),
                                floodplain = structure(c(1768816,
                                                         0, 7317, 0, 1, 0, 883632, 54, 0, 0, 0, 40, 0, 0, 1, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                ),
                                .Dim = c(15L, 4L),
                                .Dimnames = list(c("Upper Sacramento River",
                                                   "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                   "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                   "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                   "Thomes Creek"), c("s", "m", "l", "vl"))),
                                migrants = structure(c(1527499L,
                                                       0L, 6363L, 0L, 2L, 0L, 763762L, 37L, 0L, 0L, 0L, 33L, 0L, 1L,
                                                       0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                       0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                       0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), .Dim = c(15L, 4L)))
test_that('The route_regional() function returns the expected values for year 1 month 3', {
  set.seed(2021)
  route <- route_regional(month = month,
                          migrants = sutter_fish,
                          inchannel_habitat = expected_habitat$inchannel[16],
                          floodplain_habitat = expected_habitat$floodplain[16],
                          prop_pulse_flows = winterRunDSM::params$prop_pulse_flows[16, , drop = FALSE],
                          migration_survival_rate = migratory_survival$uppermid_sac,
                          territory_size = winterRunDSM::params$territory_size)
  expect_equal(route, expected_route_regional)
})
