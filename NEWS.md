# fallRunDSM v3.0.0

fallRunDSM v3.0.0 is the Fall-run life cycle model used by the Science Integration Team as part of [CVPIA Structured Decision Making](http://cvpia.scienceintegrationteam.com/) process to evaluate priorities and information needs for management of anadromous fish in the Central Valley. A complete description of the model, model parameters and their source can be found in [Peterson and Duarte (2020)](https://onlinelibrary.wiley.com/doi/10.1111/rec.13244).


### Model Updates

The 3.0.0 release of the fallRunDSM introduces several notable changes:

* Growth rates are now calculated using transition matrices produced by Bioenergetics. These changes have been incorporated into the new `get_growth_rates` function in the file `R/growth.R`. The new matrices enable the simulation to take into account both temperature and prey density in order to determine the appropriate transition to use for either perennial or floodplain growth.

* CALSIM data is now updated to incorporate 2019 BiOp and 2020 ITP rulesets. The changes are introduced in the latest version of the [DSMflow](https://github.com/CVPIA-OSC/DSMflow) package and cached in this model package. Users are still able to run the model using previous rulesets by using the 2021 model `params`.

* Spawning habitat decay has been updated and transitioned away from a probabilistic decay to a flow-driven approach. Details on the analysis for these changes can be found [here](https://cvpia-osc.github.io/DSMhabitat/articles/flow-driven-spawning-decay.html).

* Yuba Fall Run adult estimates have been updated based on analysis from the [Lower Yuba river Vaki Riverwatcher™ Chinook Salmon passage and run differentiation analyses](https://cvpia-meeting-slides.s3.us-west-2.amazonaws.com/2020-Update_LYR-Chinook-Salmon-Run-Differentiation_December-2020.pdf) 


fallRunDSM v3.0.0 was run with the following data package releases to inform the [Near-term Restoration Strategy FY2021-2025](https://cvpia-documents.s3-us-west-1.amazonaws.com/CVPIA_Near-term-Restoration-Strategy_FY21-FY25_FINAL.pdf):

* [DSMtemperatue v3.0](https://github.com/CVPIA-OSC/DSMtemperature/tree/v3.0) 
* [DSMhabitat v3.0](https://github.com/CVPIA-OSC/DSMhabitat/tree/v3.0) 
* [DSMflow v3.0](https://github.com/CVPIA-OSC/DSMflow/tree/v3.0) 
* [DSMCalibration](https://github.com/CVPIA-OSC/DSMCalibrationData) 


# fallRunDSM v1.0.0 
fallRunDSM v1.0.0 is the Fall-run life cycle model used by the Science Integration Team as part of [CVPIA Structured Decision Making](http://cvpia.scienceintegrationteam.com/) process to evaluate priorities and information needs for management of anadromous fish in the Central Valley. A complete description of the model, model parameters and their source can be found in [Peterson and Duarte (2020)](https://onlinelibrary.wiley.com/doi/10.1111/rec.13244).

fallRunDSM v1.0.0 was run with the following data package releases to inform the [Near-term Restoration Strategy FY2021-2025](https://cvpia-documents.s3-us-west-1.amazonaws.com/CVPIA_Near-term-Restoration-Strategy_FY21-FY25_FINAL.pdf):

- [cvpiaTemperatue v2.0.1](https://github.com/FlowWest/cvpiaTemperature/releases/tag/v2.0.1)
- [cvpiaHabitat v1.1.1](https://github.com/FlowWest/cvpiaHabitat/releases/tag/v1.1.1)
- [cvpiaFlow v1.0.1](https://github.com/FlowWest/cvpiaFlow/releases/tag/v1.0.1)
- [cvpiaCalibration v1.0.5](https://github.com/FlowWest/cvpiaCalibration/releases/tag/v1.0.5)
