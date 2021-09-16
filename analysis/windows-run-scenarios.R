library(DSMscenario)
library(parallel)
library(doParallel)
library(purrr)
library(fallRunDSM)
library(dplyr)

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)


run_scenario <- function(scenario) {
  s <- fall_run_model(mode = 'seed', stochastic = TRUE)
  output <- fall_run_model(scenario = scenario, mode = 'simulate',
                           stochastic = TRUE, seed = s)

  prop_nat <- output$spawners * output$proportion_natural
  juv_biomass <- output$juvenile_biomass
  viability <- output$viability_metrics

  return(list(
    prop_nat = prop_nat,
    juv_biomass = juv_biomass,
    viability = viability
  ))
}

clusterExport(cl, list('run_scenario', 'fall_run_model'))

run_scenarios_parallel <- function(scenario, number_of_runs = 5000) {
  parLapply(cl, 1:number_of_runs,
           fun = function(i) {
             run_scenario(scenario = scenario)
           })
}

number_of_runs <- 5000

baseline_results <- run_scenarios_parallel(DSMscenario::scenarios$NO_ACTION, number_of_runs)
scenario_1_results <- run_scenarios_parallel(DSMscenario::scenarios$ONE, number_of_runs)
scenario_2_results <- run_scenarios_parallel(DSMscenario::scenarios$TWO, number_of_runs)
scenario_3_results <- run_scenarios_parallel(DSMscenario::scenarios$THREE, number_of_runs)
scenario_4_results <- run_scenarios_parallel(DSMscenario::scenarios$FOUR, number_of_runs)
scenario_5_results <- run_scenarios_parallel(DSMscenario::scenarios$FIVE, number_of_runs)
scenario_6_results <- run_scenarios_parallel(DSMscenario::scenarios$SIX, number_of_runs)
scenario_7_results <- run_scenarios_parallel(DSMscenario::scenarios$SEVEN, number_of_runs)
scenario_8_results <- run_scenarios_parallel(DSMscenario::scenarios$EIGHT, number_of_runs)
scenario_9_results <- run_scenarios_parallel(DSMscenario::scenarios$NINE, number_of_runs)
scenario_10_results <- run_scenarios_parallel(DSMscenario::scenarios$TEN, number_of_runs)
scenario_11_results <- run_scenarios_parallel(DSMscenario::scenarios$ELEVEN, number_of_runs)
scenario_12_results <- run_scenarios_parallel(DSMscenario::scenarios$TWELVE, number_of_runs)
scenario_13_results <- run_scenarios_parallel(DSMscenario::scenarios$THIRTEEN, number_of_runs)


baseline_prop_nat <- flatten_dbl(transpose(baseline_results)$prop_nat)
scenario_1_prop_nat <- flatten_dbl(transpose(scenario_1_results)$prop_nat)

plot(1:number_of_runs, cummean(baseline_prop_nat))
plot(1:number_of_runs, cummean(scenario_1_prop_nat))

(mean(scenario_1_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)

# close all cluster connections
closeAllConnections()
