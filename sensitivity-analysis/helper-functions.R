library(fallRunDSM)
library(DSMscenario)
library(purrr)
library(parallel)

run_scenario <- function(scenario, sensi_params) {
  seeds <- fall_run_model(mode = "seed", ..params = sensi_params, stochastic = FALSE)

  run <- fall_run_model(scenario = scenario,
                        mode = "simulate", seeds = seeds,
                        ..params = sensi_params, stochastic = FALSE)

  return(mean(colSums(run$spawners * run$proportion_natural, na.rm = TRUE)))
}

scenarios <- list(DSMscenario::scenarios$NO_ACTION, DSMscenario::scenarios$ONE,
                  DSMscenario::scenarios$TWO, DSMscenario::scenarios$THREE,
                  DSMscenario::scenarios$FOUR, DSMscenario::scenarios$FIVE,
                  DSMscenario::scenarios$SIX, DSMscenario::scenarios$SEVEN,
                  DSMscenario::scenarios$EIGHT, DSMscenario::scenarios$NINE,
                  DSMscenario::scenarios$TEN, DSMscenario::scenarios$ELEVEN,
                  DSMscenario::scenarios$TWELVE, DSMscenario::scenarios$THIRTEEN)

run_scenarios_scaled_param <- function(param, scalar) {

  sensi_params <- fallRunDSM::params
  sensi_params[param][[1]] <-
    if (param %in% c("mean_egg_temp_effect", "spawn_decay_rate", "rear_decay_rate")) {
      scaled_param <- sensi_params[param][[1]] * scalar
      boot::inv.logit(log((scaled_param + 1e-7) / ((1 - scaled_param) + 1e-7)))
    } else {
      sensi_params[param][[1]] * scalar
    }

  scenario_results_list <- mclapply(scenarios,
                                    function(scenario) {run_scenario(scenario, sensi_params)},
                                    mc.cores = 7)

  scenario_results <- unlist(scenario_results_list)

  return(data.frame(param, scalar, base = scenario_results[1],
                    scenario_1 = scenario_results[2], scenario_2 = scenario_results[3],
                    scenario_3 = scenario_results[4], scenario_4 = scenario_results[5],
                    scenario_5 = scenario_results[6], scenario_6 = scenario_results[7],
                    scenario_7 = scenario_results[8], scenario_8 = scenario_results[9],
                    scenario_9 = scenario_results[10], scenario_10 = scenario_results[11],
                    scenario_11 = scenario_results[12], scenario_12 = scenario_results[13],
                    scenario_13 = scenario_results[14]))
}

# param_sensitivity <- function(param) {
#   scalars <- seq(.5, 1.5, by = .1)
#   purrr::map_df(scalars, ~run_scenarios_scaled_param(param, .))
# }
#
library(tictoc)
tic('run once')
xx <- run_scenarios_scaled_param("hatchery_allocation", .5)
yy <- run_scenarios_scaled_param("hatchery_allocation", .5)
toc()


# run_scenario(scenario = DSMscenario::scenarios$NO_ACTION, sensi_params = sensi_params)
