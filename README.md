# Fall Chinook Salmon Science Integration Team Model

## Primary Authors:

James T. Peterson  
U.S. Geological Survey, Oregon Cooperative Fish and Wildlife  
Research Unit, Oregon State University  
Corvallis, Oregon 97331-3803, [jt.peterson\@oregonstate.edu](mailto:jt.peterson@oregonstate.edu)

Adam Duarte  
USDA Forest Service, Pacific Northwest Research Station  
Olympia, Washington 98512  
[adam.duarte\@usda.gov](mailto:adam.duarte@usda.gov)

## Disclaimer:

Although this code has been processed successfully on a computer system at the U.S. Geological Survey (USGS), no warranty expressed or implied is made regarding the display or utility of the code for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The USGS or the U.S. Government shall not be held liable for improper or incorrect use of the code described and/or contained herein.

## License

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

IP-117068

## Usage

### Package Installation

The `fallRunDSM` package depends on a number of packages developed by the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). To install `fallRunDSM` and additional `CVPIA-OSC` packages use the `remotes::install_github()` function.

``` r
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/fallRunDSM")
remotes::install_github("CVPIA-OSC/DSMscenario")

# optional - need if calibrating model
remotes::install_github("CVPIA-OSC/DSMCalibrationData")

# optional - need if wanting to explore or modify flow, habitat, and temperature inputs
remotes::install_github("CVPIA-OSC/DSMflow")
remotes::install_github("CVPIA-OSC/DSMhabitat")
remotes::install_github("CVPIA-OSC/DSMtemperature")
```

### Run Model

The `fall_run_model()` is a Fall Run Chinook life cycle model used for [CVPIA's Structured Decision Making Process](http://cvpia.scienceintegrationteam.com/). Running the model simulates Fall Run Chinook population dynamics across 31 watersheds in California over a 20 year period.

The following code runs the fall run model with SIT defined scenario 1:

``` r
# seed the model
fall_run_seeds <- fall_run_model(mode = "seed")

# run the 20 year simulation
results <- fall_run_model(scenario = DSMscenario::scenarios$ONE,
                          mode = "simulate",
                          seeds = fall_run_seeds)
```

The following code runs the fall run model with a custom scenario defined in `scenario_df`:

``` r
# define scenario
scenario_df <- data.frame(watershed = c("Upper Sacramento River", "Battle Creek"),
                          action = c(3, 2),
                          start_year = c(1980, 1979),
                          end_year = c(1989, 1988),
                          units_of_effort = c(1, 2))

# create scenario input
scenario <- DSMscenario::get_action_matrices(scenario_df)

# seed model
fall_run_seeds <- fall_run_model(mode = "seed")

# evaluate the impact of your scenario over the 20 year simulation
results <- fall_run_model(scenario = scenario,
                          mode = "simulate",
                          seeds = fall_run_seeds)
```

## Details on Supporting Data

### Dependencies

The `fallRunDSM` package uses data from several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below.

<img src="man/figures/dependencyChain.svg" width="100%"/>

### Flow, Habitat, and Temperature Data

All data used in the `fallRunDSM` is passed in as a argument to `fall_run_model()` from a `fallRunDSM::params` data list that is composed of data objects from the following packages:

-   **Flow Data**: View detailed documentation of flow data inputs at [DSMflow](https://cvpia-osc.github.io/DSMflow/). Flow inputs to the `fallRunDSM` are generated using CalSim 2 data.
-   **Habitat Data**: View detailed documentation of habitat data inputs at [DSMhabitat](https://cvpia-osc.github.io/DSMhabitat/). Modeling details for each stream can be viewed [here](https://cvpia-osc.github.io/DSMhabitat/reference/habitat_data.html#modeling-details-for-streams).
-   **Temperature Data**: View detailed documentation of temperature data inputs at [DSMtemperature](https://cvpia-osc.github.io/DSMtemperature/). Modeling details for each stream can be viewed [here](https://cvpia-osc.github.io/DSMtemperature/reference/stream_temperature.html#watershed-modeling-details).

### Scenario Functionality

Running scenarios through the `fall_run_model()` model the impact of restoration actions on Fall Run Chinook populations. The [CVPIA SIT (Science Integration Team)](http://cvpia.scienceintegrationteam.com/) has developed restoration action portfolios composed of actions preformed on watersheds over a set time period.

There are seven predefined scenarios that were developed by the CVPIA SIT. Additional scenarios can be defined by creating a `scenario_df` describing watershed, action, start year, end year, and units of effort. The function `get_action_matrices()` takes a user defined `scenario_df` and returns a scenario in the correct format to be used as the `scenario` input for `fall_run_model()`. For additional description on how to build a scenario view `load_scenario()` documentation by searching `?DSMscenario::load_scenario()`

### Calibration Data

We prepared additional datasets in the `DSMCalibration` package for model calibration:

1.  [GrandTab](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment) estimated escapement data for the years 1998-2017. The GrandTab data is prepared as `DSMCalibrationData::grandtab_observed` and is used to measure the difference between model predictions and observed escapements. Grandtab data is additionally prepared as `DSMCalibrationData::grandtab_imputed` and is used to calculate the number of juveniles during the 20 year simulation.

2.  Proxy years are used to select Habitat, Flow, and Temperature data for 1998-2017 to correspond with the years of GrandTab escapement data. The data inputs to the DSM are for years 1980-1999. We selected proxy years for 1998-2017 from the 1980-1999 model inputs by [comparing the DWR water year indices](https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST).

For a detailed overview of the calibration process see the [calibration markdown.](https://cvpia-osc.github.io/fallRunDSM/articles/calibration-2021.html)
