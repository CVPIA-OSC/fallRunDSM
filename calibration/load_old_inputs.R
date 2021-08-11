source("calibration/scale_habitat_params.R")
source("calibration/old_inputs.R")

# 2019 calibrated coefficients
params <- fallRunDSM::params

# overwrite to 2019 habitat values
params$inchannel_habitat_fry <- cvpiaData::fr_fry
dimnames(params$inchannel_habitat_fry) <- dimnames(fallRunDSM::params$inchannel_habitat_fry)
params$inchannel_habitat_juvenile <-cvpiaData::fr_juv
dimnames(params$inchannel_habitat_juvenile) <- dimnames(fallRunDSM::params$inchannel_habitat_juvenile)
params$floodplain_habitat <- cvpiaData::fr_fp
dimnames(params$floodplain_habitat) <- dimnames(fallRunDSM::params$floodplain_habitat)
params$spawning_habitat <- cvpiaData::fr_spawn
dimnames(params$spawning_habitat) <- dimnames(fallRunDSM::params$spawning_habitat)

params <- scale_habitat_params(params)
params <- DSMCalibrationData::set_synth_years(params,
                                              spawn_years = DSMCalibrationData::calibration_year_spawn_index_2019,
                                              years = DSMCalibrationData::calibration_year_index_2019)

params$prisoners_point_temps <- old_inputs$prisoners_point
dimnames(params$prisoners_point_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                 "Sep", "Oct", "Nov", "Dec"), c("1998", "1997", "1993", "1981",
                                                                                "1989", "1993", "1993", "1993", "1998", "1994", "1988", "1994",
                                                                                "1985", "1997", "1985", "1994", "1992", "1992", "1989", "1998"
                                                 ))

params$vernalis_temps <- old_inputs$vernalis
dimnames(params$vernalis_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                          "Sep", "Oct", "Nov", "Dec"), c("1998", "1997", "1993", "1981",
                                                                         "1989", "1993", "1993", "1993", "1998", "1994", "1988", "1994",
                                                                         "1985", "1997", "1985", "1994", "1992", "1992", "1989", "1998"
                                          ))

params$proportion_flow_bypass[ , , 1] <- old_inputs$prop.q.sutter
params$proportion_flow_bypass[ , , 2] <- params$proportion_flow_bypass[,,2]
dimnames(params$proportion_flow_bypass) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                  "Sep", "Oct", "Nov", "Dec"),
                                                c("1998", "1997", "1993", "1981",
                                                  "1989", "1993", "1993", "1993", "1998", "1994", "1988", "1994",
                                                  "1985", "1997", "1985", "1994", "1992", "1992", "1989", "1998"
                                                ),
                                                c("Sutter Bypass", "Yolo Bypass"))



# use old delta temp survival values
# proxy_2000_pp <- which.min(sapply(1:20, function(i) sum(abs(params$prisoners_point_temps[i] - params$prisoners_point_temps[,21]))))
# params$prisoners_point_temps <- cbind(old_inputs$prisoners_point, old_inputs$prisoners_point[,proxy_2000_pp])
# dimnames(params$prisoners_point_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
#                                                  "Sep", "Oct", "Nov", "Dec"),
#                                                c("1980", "1981", "1982", "1983",
#                                                  "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
#                                                  "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
#                                                  "2000"))
#
# proxy_2000_vern <- which.min(sapply(1:20, function(i) sum(abs(params$vernalis_temps[i] - params$vernalis_temps[,21]))))
# params$vernalis_temps <- cbind(old_inputs$vernalis, old_inputs$vernalis[,proxy_2000_vern])
# dimnames(params$vernalis_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
#                                           "Sep", "Oct", "Nov", "Dec"),
#                                         c("1980", "1981", "1982", "1983",
#                                           "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
#                                           "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
#                                           "2000"))
#
# params$proportion_flow_bypass[ , , 1] <- cbind(old_inputs$prop.q.sutter, old_inputs$prop.q.sutter[ , 20])
# params$proportion_flow_bypass[ , , 2] <- params$proportion_flow_bypass[,,2]
# dimnames(params$proportion_flow_bypass) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
#                                                   "Sep", "Oct", "Nov", "Dec"),
#                                                 c("1980", "1981", "1982", "1983",
#                                                   "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
#                                                   "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
#                                                   "2000"),
#                                                 c("Sutter Bypass", "Yolo Bypass"))



flooded <- params$floodplain_habitat > 0
params$weeks_flooded <- pmax(flooded*2, params$weeks_flooded)

adam_grand_tab <- structure(c(56214.27, 3090.42, 7366.8125, NA, 2056.035, 9149.258767908,
            12091.56, 54067.065431528, 119.56, 311.5855914725, 468.509155785,
            1741.09, 1157.205, 27470.52, 2226.66, 4948.7875, 252, 1010.22,
            5510.3091844135, 6520.04, 32765.41695332, 1183.84, 233.175268265,
            605.543454315, 1184.425, 1061.44, 35917.56, 3765.675, 11525.66,
            808.92, 2056.035, 5000.5560011675, 7528.36, 20696.397956436,
            362.6, 1527.9734518675, 359.622442899, 420.965, 357.38, 34944.84,
            1699.2, 6548.105, 1600.2, 1189.0425, 7772.749849607, 3467.94,
            8103.02662535999, 519.4, 254.319625085, 264.6243413505, 567.285,
            300.67, 10748.43, 1084.125, 3232.0675, 472.92, 721.2225, 2228.581307149,
            1112.28, 3341.59044208, 75.46, 69.0128312875, 89.8130199825,
            130.685, 119.84, 15588.09, 243.375, 5968.8675, 162.96, 184.755,
            604.208738697, 1498.22, 830.114463575999, 14.7, 25.40259534625,
            72.0355974705, 255.175, 207.58, 3671.01, 270.81, 2509.77, 48.72,
            86.445, 493.09744725, 1979.38, 1749.051835148, 0, 99.84835165,
            66.294971451, 175.525, 66.34, 10314.36, 327.45, 5591.78, 139.44,
            122.04, 4568.6026373765, 6138.7, 4849.928894592, 725.2, 281.9247576,
            120.5531464095, 320.37, 288.9, 7532.91, 368.16, 3763.8775, 556.08,
            1043.2725, 4810.220386529, 3812.48, 8461.62022418399, 51.94,
            392.6389592825, 290.9201121495, 386.155, 477.755, 18081.63, 719.505,
            5933.1025, 733.32, 754.275, 6474.3694823925, 3274.74, 12655.778504352,
            1049.58, 803.33872334875, 372.3999653295, 1181.77, 418.905, 25252.92,
            1947, 10369.5175, 861.84, 1861.9575, 15380.8051518325, 6353.98,
            19226.692515952, 0, 1038.27602134875, 523.322875197, 839.275,
            1030.41, 21971.88, 1249.62, 12279.835, 713.16, 2108.58, 6225.1909057155,
            4960.18, 8741.9572089, 365.54, 484.11766969125, 170.737328709,
            902.7, 234.33, 18055.8, 72.57, 6848.9975, 514.08, 875.4675, 2092.048102866,
            2778.86, 5194.654232688, 199.92, 672.65485133625, 230.9213111715,
            1810.12, 60.455, 2703.33, 554.01, 1928.9775, 212.52, 510.195,
            3944.2316919475, 1732.82, 4778.936607532, 1223.04, 291.32224952,
            470.5461521145, 2752.35, 720.645), .Dim = 13:14, .Dimnames = list(
              c("1", "6", "7", "10", "12", "19", "20", "23", "26", "27",
                "28", "29", "30"), NULL))
