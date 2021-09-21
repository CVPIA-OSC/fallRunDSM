library(fallRunDSM)
library(DSMscenario)
library(purrr)
library(parallel)
library(doParallel)

# don't need to do anything special for habitat inputs, max theoretical is 2x max value
# which is less than 1.5

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)

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

# register the functions for use in parallel mode
clusterExport(cl, list('run_scenario', 'fall_run_model', 'scenarios'))

# days in each month

run_scenarios_scaled_param <- function(param, scalar) {

  sensi_params <- fallRunDSM::params
  sensi_params[param][[1]] <-
    if (param %in% c("cc_gates_prop_days_closed", "cross_channel_stray_rate",
                     "delta_prop_high_predation", "delta_proportion_diverted",
                     "growth_rates", "growth_rates_floodplain",
                     "hatchery_allocation", "mean_egg_temp_effect",
                     "migratory_temperature_proportion_over_20", "min_survival_rate",
                     "month_return_proportions", "natural_adult_removal_rate",
                     "prob_nest_scoured", "prob_strand_early", "prob_strand_late",
                     "prop_flow_natal",
                     "prop_high_predation", "prop_pulse_flows", "proportion_diverted",
                     "proportion_flow_bypass", "proportion_hatchery", "rear_decay_rate",
                     "spawn_decay_rate",
                     "spawn_success_sex_ratio", "stray_rate")) {
      boot::inv.logit(log((sensi_params[param][[1]] + 1e-7) / ((1 - sensi_params[param][[1]]) + 1e-7)) * scalar)
    } else if (param %in% c("cc_gates_days_closed")) {
      # scale prop days closed using the 0-1 restriction
      prop_days_scaled <- boot::inv.logit(log((sensi_params["cc_gates_prop_days_closed"][[1]] + 1e-7) /
                                                ((1 - sensi_params["cc_gates_prop_days_closed"][[1]]) + 1e-7)) * scalar)
      floor(lubridate::days_in_month(1:12) * prop_days_scaled)
    } else if (param %in% c("weeks_flooded")) {
      scalar
    } else {
      sensi_params[param][[1]] * scalar
    }

  scenario_results_list <- parLapply(cl, scenarios,
                                     fun = function(scenario) {
                                       run_scenario(scenario, sensi_params)
                                     })

  scenario_results <- unlist(scenario_results_list)

  if (is.vector(scalar)) {
    scalar <- paste(round(scalar, 2), collapse = ",")
  }

  return(data.frame(param, scalar, base = scenario_results[1],
                    scenario_1 = scenario_results[2], scenario_2 = scenario_results[3],
                    scenario_3 = scenario_results[4], scenario_4 = scenario_results[5],
                    scenario_5 = scenario_results[6], scenario_6 = scenario_results[7],
                    scenario_7 = scenario_results[8], scenario_8 = scenario_results[9],
                    scenario_9 = scenario_results[10], scenario_10 = scenario_results[11],
                    scenario_11 = scenario_results[12], scenario_12 = scenario_results[13],
                    scenario_13 = scenario_results[14]))
}

param_sensitivity <- function(param) {
  scalars <- if (param == "weeks_flooded") {

    (function() {
      original <- fallRunDSM::params$weeks_flooded
      fp_filter <- fallRunDSM::params$floodplain_habitat > 0
      weeks_flooded_scaled <- array(dim = c(4, 31, 12, 21))
      weeks_flooded_scaled[1,,,] <- pmin(original + 2, 4) * fp_filter
      weeks_flooded_scaled[2,,,] <- pmax(original - 2, 1) * fp_filter
      weeks_flooded_scaled[3,,,] <- pmin(original + 1, 4) * fp_filter
      weeks_flooded_scaled[4,,,] <- pmax(original - 1, 1) * fp_filter
      return(weeks_flooded_scaled)
    })()

  } else {
    # default steps
    seq(.5, 1.5, by = .1)
  }

  purrr::map_df(scalars, ~run_scenarios_scaled_param(param, .))
}


library(tictoc)
tic("one param")
y <- param_sensitivity("hatchery_allocation")
toc()
View(x)

# how to separate coefficients from other model inputs within params
coefficients <- names(params)[grep('\\.', names(params))]
model_inputs <- sort(names(params)[grep('\\.', names(params), invert = TRUE)])

# close all cluster connections
closeAllConnections()
