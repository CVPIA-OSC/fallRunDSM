library(testthat)
library(fallRunDSM)
# tests for routing functions
# Lists inputs to use in testing
test_data <- fallRunDSM::load_baseline_data()
migrants_at_golden_gate <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3281, 3370, 281, 866,
                                       0, 4173, 141, 3, 13810, 2580, 1228, 676, 751, 0, 1, 0, 0, 2264,
                                       37421, 14922, 0, 0, 103480, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L,4L))
year <- 1
month <- 3
avg_ocean_transition_month <- 1

# Test that we have the expected amount of ocean entry success for
expected_ocean_entry_success <- c(1854, 2959, 247, 760, 0, 302, 124, 3, 12125, 204, 1078, 45,
                                  659, 0, 1, 0, 0, 139, 2298, 916, 0, 0, 38219, 0, 0, 0, 0, 0,
                                  0, 0, 0)

test_that('The ocen entry success function returns the expected values for year 1 month 3', {
  expect_equal(ocean_entry_success(migrants = migrants_at_golden_gate,
                                   month = month,
                                   avg_ocean_transition_month = avg_ocean_transition_month),
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
  expect_equal(pulse_movement(test_data$prop_pulse_flows[ , month]),
               expected_pulse_movement)
})

# Test that route south delta works as expected
expected_south_delta_routing <- 0.0106985684999263

test_that('The south delta routing function returns the expected values for year 1 month 3', {
  expect_equal(route_south_delta(freeport_flow = test_data$freeport_flows[[month, year]] * 35.3147,
                                 dcc_closed = test_data$cc_gates_days_closed[month],
                                 month = month),
               expected_south_delta_routing)
})
