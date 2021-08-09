library(tidyverse)
library(DSMscenario)
library(parallel)


s <- fall_run_model(mode = 'seed', ..params = fallRunDSM::params)

run_scenario <- function(scenario, seed, params) {
  output <- fall_run_model(scenario = scenario, mode = 'simulate',
                           stochastic = FALSE, ..params = params, seed = seed)

  prop_nat <- colSums(output$spawners * output$proportion_natural, na.rm = TRUE)[20]
  juv_biomass <- colSums(output$juvenile_biomass)[20]

  return(list(
    prop_nat = prop_nat,
    juv_biomass = juv_biomass
  ))
}

baseline <- run_scenario(scenario = scenarios$NO_ACTION, params = fallRunDSM::params, seed = s)
s1 <- run_scenario(scenario = scenarios$ONE, params = fallRunDSM::params, seed = s)
s2 <- run_scenario(scenario = scenarios$TWO, params = fallRunDSM::params, seed = s)
s3 <- run_scenario(scenario = scenarios$THREE, params = fallRunDSM::params, seed = s)
s4 <- run_scenario(scenario = scenarios$FOUR, params = fallRunDSM::params, seed = s)
s5 <- run_scenario(scenario = scenarios$FIVE, params = fallRunDSM::params, seed = s)
s6 <- run_scenario(scenario = scenarios$SIX, params = fallRunDSM::params, seed = s)
s7 <- run_scenario(scenario = scenarios$SEVEN, params = fallRunDSM::params, seed = s)

prop_nats <- c(s1$prop_nat, s2$prop_nat, s3$prop_nat, s4$prop_nat, s5$prop_nat,
               s6$prop_nat, s7$prop_nat)

(prop_nats - baseline$prop_nat) / baseline$prop_nat

juv_biomasses <- c(s1$juv_biomass, s2$juv_biomass, s3$juv_biomass, s4$juv_biomass,
                   s5$juv_biomass, s6$juv_biomass, s7$juv_biomass)

(juv_biomasses - baseline$juv_biomass) / baseline$juv_biomass

library(tictoc)
cores <- detectCores() - 1

tic('baseline 250 times')
baseline_results <- mclapply(1:1000, function(i) {run_scenario(scenario = scenarios$NO_ACTION,
                                                               params = fallRunDSM::params, seed = s)},
                             mc.cores = cores)
toc()

s1_results <- mclapply(1:500, function(i) {run_scenario(scenario = scenarios$ONE,
                                                        params = fallRunDSM::params, seed = s)},
                       mc.cores = cores)

s7_results <- mclapply(1:500, function(i) {run_scenario(scenario = scenarios$SEVEN,
                                                        params = fallRunDSM::params, seed = s)},
                       mc.cores = cores)



baseline_prop_nat <- flatten_dbl(transpose(baseline_results)$prop_nat)
s1_prop_nat <- flatten_dbl(transpose(s1_results)$prop_nat)
s7_prop_nat <- flatten_dbl(transpose(s7_results)$prop_nat)

plot(1:1000, cummean(baseline_prop_nat))
plot(1:500, cummean(s1_prop_nat))
plot(1:500, cummean(s7_prop_nat))

range <- sample(1:500, 400)
(mean(s1_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(s7_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
