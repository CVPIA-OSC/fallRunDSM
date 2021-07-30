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

# use old delta temp survival values
proxy_2000_pp <- which.min(sapply(1:20, function(i) sum(abs(params$prisoners_point_temps[i] - params$prisoners_point_temps[,21]))))
params$prisoners_point_temps <- cbind(old_inputs$prisoners_point, old_inputs$prisoners_point[,proxy_2000_pp])
dimnames(params$prisoners_point_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                 "Sep", "Oct", "Nov", "Dec"),
                                               c("1980", "1981", "1982", "1983",
                                                 "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
                                                 "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
                                                 "2000"))

proxy_2000_vern <- which.min(sapply(1:20, function(i) sum(abs(params$vernalis_temps[i] - params$vernalis_temps[,21]))))
params$vernalis_temps <- cbind(old_inputs$vernalis, old_inputs$vernalis[,proxy_2000_vern])
dimnames(params$vernalis_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                          "Sep", "Oct", "Nov", "Dec"),
                                        c("1980", "1981", "1982", "1983",
                                          "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
                                          "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
                                          "2000"))

params$proportion_flow_bypass[ , , 1] <- cbind(old_inputs$prop.q.sutter, old_inputs$prop.q.sutter[ , 20])
params$proportion_flow_bypass[ , , 2] <- params$proportion_flow_bypass[,,2]
dimnames(params$proportion_flow_bypass) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                  "Sep", "Oct", "Nov", "Dec"),
                                                c("1980", "1981", "1982", "1983",
                                                  "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
                                                  "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
                                                  "2000"),
                                                c("Sutter Bypass", "Yolo Bypass"))


flooded <- params$floodplain_habitat > 0
params$weeks_flooded <- pmax(flooded*2, params$weeks_flooded)

