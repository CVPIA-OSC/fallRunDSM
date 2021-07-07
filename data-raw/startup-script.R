
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
library(fallRunDSM)

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)

# model run --------------
# seeds,I want both the models to be using the same seed therefore I leave the seeding
# out of the iterations, if we want seeding to happen every iteration we simply move
# this like into each of the model run functions below
fall_run_seeds <- fall_run_model(mode = "seed")


# base run, with NO ACTION
run_base <- function(...) {
  fall_run_model(scenario = DSMscenario::scenarios$NO_ACTION, mode = "simulate", seeds = fall_run_seeds)
}

# scenario run with scenario = 5
run_scenario5 <- function(...) {
  fall_run_model(scenario = DSMscenario::scenarios$FIVE, mode = "simulate", seeds = fall_run_seeds)
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



# Data wrangling and visualization
base_run_nat_spawn <- map_df(1:model_iters, function(i) {
  as_tibble(base_runs[[i]]$natural_spawners) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, nat_spawners, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "base")

scenario_run_nat_spawn <- map_df(1:model_iters, function(i) {
  as_tibble(scenario_5[[i]]$natural_spawners) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, nat_spawners, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "scenario5")

base_run_nat_spawn %>%
  filter(watershed == "Upper Sacramento River") %>%
  ggplot(aes(year, nat_spawners, group = run)) + geom_line(alpha=.1)

scenario_run_nat_spawn %>%
  filter(watershed == "Upper Sacramento River") %>%
  ggplot(aes(year, nat_spawners, group = run)) + geom_line(alpha=.1)

nat_spawn_results <- bind_rows(
  base_run_nat_spawn,
  scenario_run_nat_spawn
) %>%
  group_by(year, watershed, scenario) %>%
  summarise(
    avg_nat_spawners = mean(nat_spawners)
  ) %>% ungroup()

nat_spawn_results %>%
  filter(watershed == "Upper Sacramento River") %>%
  ggplot(aes(year, avg_nat_spawners, color = scenario)) + geom_line()












