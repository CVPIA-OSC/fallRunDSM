# install required libraries
remotes::install_github("CVPIA-OSC/DSMflow@main")
remotes::install_github("CVPIA-OSC/DSMtemperature@main")
remotes::install_github("CVPIA-OSC/DSMhabitat@main")
remotes::install_github("CVPIA-OSC/DSMscenario@main")
install.packages("doParallel") # needed for parallel processing

library(tidyverse)
library(parallel)
library(doParallel)
library(remotes)
library(winterRunDSM)
library(boot)

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)

sensi_params <- winterRunDSM::params
temp_effect <- runif(31)
sensi_params$mean_egg_temp_effect <- boot::inv.logit(
  log(
    (temp_effect + 0.000001) / ((1 - temp_effect) + 0.0000001)
    )
  )

# model run --------------
winter_run_seeds <- winter_run_model(mode = "seed")

# base run, with NO ACTION
run_base <- function(...) {
  winter_run_model(scenario = DSMscenario::scenarios$NO_ACTION, mode = "simulate",
                 seeds = winter_run_seeds,..params = sensi_params)
}


# register the functions for use in parallel mode
clusterExport(cl, list('run_base', 'run_scenario5', 'winter_run_model', 'winter_run_seeds'))

# total number of times to run the model
model_iters <- 250

# run and time each
system.time(
  base_runs <- parLapply(cl, 1:model_iters, fun = run_base)
)

system.time(
  scenario_5 <- parLapply(cl, 1:model_iters, fun = run_scenario5)
)













