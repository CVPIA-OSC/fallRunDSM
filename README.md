# Fall Chinook Salmon Science Iintegration Team Model

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

Describe install from github and remotes package

```r
remotes::install_github("CVPIA-OSC/DSMflow")
remotes::install_github("CVPIA-OSC/DSMhabitat")
remotes::install_github("CVPIA-OSC/DSMtemperature")
remotes::install_github("CVPIA-OSC/DSMscenario")
remotes::install_github("CVPIA-OSC/DSMCalibrationData")
remotes::install_github("CVPIA-OSC/fallRunDSM")
```

### Run Model

Run model with SIT defined scenaro 1:
```r
fall_run_seeds <- fallRunDSM::fall_run_model(mode = "seed")
fallRunDSM::fall_run_model(scenario = DSMscenario::scenarios$ONE,
                           mode = "simulate",
                           seeds = fall_run_seeds)
```

Run model with custom scenaro:
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

### Flow, Habitat, and Temperature Data

`fallRunDSM::params` is composed using data objects from the following packages
Describe top level description of purpose and sources of the data
Provide links to package websites

```r
library(DSMflow)
library(DSMhabitat)
library(DSMtemperature)
```

### Scenario Functionality

Describe purpose of scenarios and the SIT has developed some and link to docs

```r
library(DSMscenario)
```

### Calibration Data

Describe calibration use (contains proxy years, grandtab data etc) readme for package to link and point to calibration notebook

```r 
library(DSMCalibrationData)
```
