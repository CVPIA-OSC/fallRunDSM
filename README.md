# Fall Chinook Salmon Science Integration Team Model

## Primary Authors:                                                     
                                                                     
James T. Peterson                                                    
U.S. Geological Survey, Oregon Cooperative Fish and Wildlife         
Research Unit, Oregon State University                               
Corvallis, Oregon 97331-3803, <jt.peterson@oregonstate.edu>            
                                                                     
Adam Duarte                                                          
Oregon Cooperative Fish and Wildlife Research Unit,                  
Oregon State University,                                             
Corvallis, Oregon 97331-3803, <adam.duarte@oregonstate.edu>

## Disclaimer:
Although this code has been processed successfully on a computer system at the U.S. Geological Survey (USGS), no warranty expressed or implied is made regarding the display or utility of the code for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The USGS or the U.S. Government shall not be held liable for improper or incorrect use of the  code  described and/or contained herein.

## License
[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

IP-117068

## Usage

### Package Installation
`fallRunDSM` works as part of a larger set of packages developed by the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). 
To install `fallRunDSM` and additional `CVPIA-OSC` packages use the `remotes::install_github()` function. 

```r
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/DSMflow")
remotes::install_github("CVPIA-OSC/DSMhabitat")
remotes::install_github("CVPIA-OSC/DSMtemperature")
remotes::install_github("CVPIA-OSC/DSMscenario")
remotes::install_github("CVPIA-OSC/DSMCalibrationData")
remotes::install_github("CVPIA-OSC/fallRunDSM")
```

### Run Model
The `fallrunDSM` is a Fall Run Chinook life cycle model used for CVPIA's Structured Decision Making Process.
Running the model simulates Fall Run Chinook population dynamics across 31 watersheds in California over a 20 year period. 
View the [fall_run_model()](docs/reference/fall_run_model.html) documentation for additional information on running the `fall_run_model`.

The following code runs the fall run model with SIT defined scenario 1 and `fall_run_seeds`:
```r
fall_run_seeds <- fallRunDSM::fall_run_model(mode = "seed")
fallRunDSM::fall_run_model(scenario = DSMscenario::scenarios$ONE,
                           mode = "simulate",
                           seeds = fall_run_seeds)
```

The following code runs the fall run model with a custom scenario defined in `scenario_df` and `fall_run_seeds`:
```r
scenario_df <- data.frame(watershed = c("Upper Sacramento River", "Battle Creek"),
                          action = c(3, 2),
                          start_year = c(1980, 1979),
                          end_year = c(1989, 1988),
                          units_of_effort = c(1, 2))

fall_run_seeds <- fallRunDSM::fall_run_model(mode = "seed")

fallRunDSM::fall_run_model(scenario = scenario_df,
                           mode = "simulate",
                           seeds = fall_run_seeds)
```

## Details on Supporting Data

### Dependencies
The `fallRunDSM` package uses data from several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below. 

<img src="man/figures/dependencyChain.svg" width="100%"/>

### Flow, Habitat, and Temperature Data

All data used in the `fallRunDSM` is passed in as a argument to `fall_run_model()` from a `fallRunDSM::params` data list. [`fallRunDSM::params`](docs/reference/params.html) is composed using data objects from the following packages:

* **Flow Data**: View detailed documentation of flow data inputs at [DSMflow](https://cvpia-osc.github.io/DSMflow/). Flow inputs to the `fallRunDSM` are matrices generated using CALSIM data.
* **Habitat Data**: View detailed documentation of habitat data inputs at [DSMhabitat](https://cvpia-osc.github.io/DSMhabitat/). Habitat inputs to `fallRunDSM` are matrices based on expert opinion and modeled results for tributaries in the central valley.
* **Temperature Data**: View detailed documentation of temperature data inputs at [DSMtemperature](https://cvpia-osc.github.io/DSMtemperature/). Temperature inputs to `fallRunDSM` are matrices and are modeled temperature inputs based on NOAA air temperature data.

```r
library(DSMflow)
library(DSMhabitat)
library(DSMtemperature)
```

### Scenario Functionality

Scenario functionality within the `fallRunDSM` models the effect of restoration actions on Fall run Chinook. 
The [CVPIA SIT (Science Integration Team)](http://cvpia.scienceintegrationteam.com/) has developed restoration action portfolioscomposed of actions preformed on watersheds over a set time period. 

There are seven predefined scenarios that were developed by the CVPIA SIT. Additional scenarios can be defined by creating a `scenario_df` describing watershed, action, start year, end year, and units of effort. For additional description on how to build a scenario view `load_scenario` documentation by searching `?DSMscenario::load_scenario()`  

```r
library(DSMscenario)
```

### Calibration Data

For `fallRunDSM` model calibration we prepared addtional datasets in a `DSMCalibration` package:

1. [GrandTab](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment) estimated escapement data for the years 1998-2017. The GrandTab data is prepared as `DSMCalibrationData::grandtab_observed` and is used to measure the difference between model predictions and observed escapements. Grandtab data is additionally prepared as `DSMCalibrationData::grandtab_imputed` and is used to calculate the number of juveniles during the 20 year simulation.

2. Proxy years are used to select Habitat, Flow, and Temperature data for 1998-2017 to correspond with the years of GrandTab escapement data. The data inputs to the DSM are for years 1980-1999. We selected proxy years for 1998-2017 from the 1980-1999 model inputs by [comparing the DWR water year indices](https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST).

For a detailed overview of the calibration process see the [calibration markdown.](calibration.Rmd)
```r 
library(DSMCalibrationData)
```
