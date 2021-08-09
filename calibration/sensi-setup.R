remotes::install_github("CVPIA-OSC/DSMflow@main")
remotes::install_github("CVPIA-OSC/DSMtemperature@main")
remotes::install_github("CVPIA-OSC/DSMhabitat@main")
remotes::install_github("CVPIA-OSC/DSMscenario@main")
# install.packages("doParallel") # needed for parallel processing

library(tidyverse)
library(parallel)
library(doParallel)
library(remotes)
library(fallRunDSM)
library(boot)

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)

sensi_params <- fallRunDSM::params

#  range (.5, 1.5) by .1
original <- 3
scalars <- seq(.5, 1.5, by = .1)
original * scalars # interate over these options and run model

temp_effect <- runif(31)
sensi_params$mean_egg_temp_effect <- boot::inv.logit(
  log(
    (temp_effect + 0.000001) / ((1 - temp_effect) + 0.0000001)
  )
)


# using the same scalar to modify the parameter value
# run the model (deterministic mode) once for each scenario and parameter option for each parameter
# report out the mean valley wide natural spawners for each run of the model
# these need to be constrained between 0 and 1 habitat decay for spawning and rearing, temp effect

# for inputs, scale with scalar, check that proportions remain between 0 and 1 and
# and that habitat inputs do not exceed theoretical maximums
# model weights must sum to 1
13*150*11*.16/60
coefficients <- names(params)[grep('\\.', names(params))]
model_inputs <- names(params)[grep('\\.', names(params), invert = TRUE)]

scalars <- seq(.5, 1.5, by = .1)
current_param <- unlist(sensi_params[coefficients[66]])
current_param <- unlist(sensi_params[coefficients[1]])
length(current_param)
current_param * scalars
sapply(1:11, function(i) {current_param * scalars[i]})



# don't need to do anything special for habitat inputs, max theoretical is 2x max value
# which is less than 1.5

# model weights
params$surv_juv_outmigration_sac_delta_model_weights

# constraing to between 0 and 1
temp_effect <- params$mean_egg_temp_effect * scalar
boot::inv.logit(
  log(
    (temp_effect + 0.000001) / ((1 - temp_effect) + 0.0000001)
  )
)



# scenarios will be done separate

all_results <- tibble(param_name, value, metric, scenario)

for (param in  params) {

  param_options <- params$param * scalars

  all_valley_wide_means = numeric(length(param_options))
  for (i in param_options) {
    for (scenario in scenarios) {
      update_param <- param_options[i]
      result <- fall_run_model(update_params, scenario)
      valley_wide_mean_nat_spawners <- mean(colSums(result$spawners * result$proportion_natural))
      bind_row(all_results, tibble(param_name = param, value = param_option[i], metric = valley_wide_mean_nat_spawners))
    }
  }

}

# model run --------------
fall_run_seeds <- fall_run_model(mode = "seed")

# base run, with NO ACTION
run_base <- function(...) {
  fall_run_model(scenario = DSMscenario::scenarios$NO_ACTION, mode = "simulate",
                 seeds = fall_run_seeds,..params = sensi_params)
}


# register the functions for use in parallel mode
clusterExport(cl, list('run_base', 'run_scenario5', 'fall_run_model', 'fall_run_seeds'))

# total number of times to run the model
model_iters <- 250

# run and time each
system.time(
  base_runs <- parLapply(cl, 1:model_iters, fun = run_base)
)

system.time(
  scenario_5 <- parLapply(cl, 1:model_iters, fun = run_scenario5)
)
