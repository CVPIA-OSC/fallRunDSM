library(tidyverse)
library(DSMscenario)
library(parallel)

source("calibration/update_params.R")
res <- read_rds('calibration/solution-res4-08-03.rds')
solution <- res@solution
params_2021 <- update_params(x = solution, fallRunDSM::params)

s <- fall_run_model(mode = 'seed', ..params = params_2021)

run_scenario <- function(scenario, seed, params) {
  output <- fall_run_model(scenario = scenarios$ONE, mode = 'simulate',
                           stochastic = TRUE, ..params = params, seed = seed)

  prop_nat <- colSums(output$spawners * output$proportion_natural, na.rm = TRUE)[20]
  juv_biomass <- colSums(output$juvenile_biomass)[20]

  return(list(
    prop_nat = prop_nat,
    juv_biomass = juv_biomass
  ))
}

baseline <- run_scenario(scenario = scenarios$NO_ACTION, params = params_2021, seed = s)
s1 <- run_scenario(scenario = scenarios$ONE, params = params_2021, seed = s)
s2 <- run_scenario(scenario = scenarios$TWO, params = params_2021, seed = s)
s3 <- run_scenario(scenario = scenarios$THREE, params = params_2021, seed = s)
s4 <- run_scenario(scenario = scenarios$FOUR, params = params_2021, seed = s)
s5 <- run_scenario(scenario = scenarios$FIVE, params = params_2021, seed = s)
s6 <- run_scenario(scenario = scenarios$SIX, params = params_2021, seed = s)
s7 <- run_scenario(scenario = scenarios$SEVEN, params = params_2021, seed = s)

(s1$prop_nat - baseline$prop_nat) / baseline$prop_nat

prop_nats <- c(s1$prop_nat, s2$prop_nat, s3$prop_nat, s4$prop_nat, s5$prop_nat,
               s6$prop_nat, s7$prop_nat)
(prop_nats - baseline$prop_nat) / baseline$prop_nat

juv_biomasses <- c(s1$juv_biomass, s2$juv_biomass, s3$juv_biomass, s4$juv_biomass,
                   s5$juv_biomass, s6$juv_biomass, s7$juv_biomass)

(juv_biomasses - baseline$juv_biomass) / baseline$juv_biomass

library(tictoc)
cores <- detectCores() - 1

tic('baseline 250 times')
baseline_results <- mclapply(1:250, function(i) {run_scenario(scenario = scenarios$NO_ACTION,
                                                              params = params_2021, seed = s)},
                             mc.cores = cores)
toc()

s1_results <- mclapply(1:250, function(i) {run_scenario(scenario = scenarios$ONE,
                                                        params = params_2021, seed = s)},
                       mc.cores = cores)

plot(1:250, cummean(baseline_results$prop_nat))
plot(1:250, cummean(s1_results$prop_nat))

(mean(s1_results$prop_nat) - mean(baseline_results$prop_nat)) / mean(baseline_results$prop_nat)
