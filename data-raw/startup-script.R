
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
# seed and run a model
run_model <- function(scenario=NULL) {
  fall_run_seeds <- fall_run_model(mode = "seed")
  fall_run_model(scenario = scenario, mode = "simulate", seeds = fall_run_seeds)
}

# register the functions for use in parallel mode
clusterExport(cl, list('run_model', 'fall_run_model'))

system.time(
  base_runs <- parLapply(cl, 1:2, fun = run_model, scenario = NULL)
)

system.time(
  scenario_runs <- parLapply(cl, 1:250, fun = run_scenario7_model)
)




base_run_nat_spawn <- map_df(1:250, function(i) {
  as_tibble(base_runs[[i]]$natural_spawners) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, nat_spawners, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "base")

scenario_run_nat_spawn <- map_df(1:250, function(i) {
  as_tibble(scenario_runs[[i]]$natural_spawners) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, nat_spawners, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "scenario7")

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












