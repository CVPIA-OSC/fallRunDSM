
# install required libraries
remotes::install_github("CVPIA-OSC/DSMflow") # main branch
remotes::install_github("CVPIA-OSC/DSMtemperature") # main
remotes::install_github("CVPIA-OSC/DSMhabitat") #
remotes::install_github("CVPIA-OSC/DSMscenario@main")

library(tidyverse)
library(parallel)
library(remotes)
library(DSMscenario)
# set up for parallel processing
cores <- detectCores() - 1

# model run --------------
# seed and run a model
run_model <- function(scenario = NULL) {
  fall_run_seeds <- fall_run_model(mode = "seed")
  fall_run_model(scenario = scenario, mode = "simulate", seeds = fall_run_seeds)
}

test_run <- run_model()

# set the seeds
base_runs <- parallel::pvec(1:10, run_model, mc.cores = 1)

base_runs <- lapply(X = 1:2, run_model)

base_runs <- replicate(10, {
  fall_run_seeds <- fall_run_model(mode = "seed")
  fall_run_model(mode = "simulate", seeds = fall_run_seeds)
  })

tic("scenario run")
scenario_run <- replicate(10, {
  fall_run_seeds <- fall_run_model(mode = "seed")
  fall_run_model(scenario = DSMscenario::scenarios$ONE,
                               mode = "simulate",
                               seeds = fall_run_seeds)
  })
toc()

base_run_spawn <- map_df(1:10, function(i) {
  as_tibble(base_runs["spawners", i][[1]]) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, spawner, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "base")


scenario_run_spawn <- map_df(1:10, function(i) {
  as_tibble(scenario_run["spawners", i][[1]]) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, spawner, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "scenario one")

spawn_results <- bind_rows(
  base_run_spawn,
  scenario_run_spawn
)

spawn_results %>%
  filter(watershed == "Upper Sacramento River") %>%
  ggplot(aes(year, spawner, color = scenario, group = run)) + geom_line()



base_run_biomass <- map_df(1:10, function(i) {
  as_tibble(base_runs["juvenile_biomass", i][[1]]) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, biomass, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "base")


scenario_run_biomass <- map_df(1:10, function(i) {
  as_tibble(scenario_run["juvenile_biomass", i][[1]]) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, biomass, -run, -watershed) %>%
    mutate(year = as.numeric(year))
}) %>%
  mutate(scenario = "scenario one")

biomass_results <- bind_rows(
  base_run_biomass,
  scenario_run_biomass
)

biomass_results %>%
  filter(watershed == "Upper Sacramento River") %>%
  mutate(group_name = paste(run, scenario)) %>%
  ggplot(aes(year, biomass, color = scenario, group = group_name)) + geom_line()


biomass_results %>%
  group_by(watershed, scenario)



biomass_results %>%
  filter(year == 20) %>%
  group_by(watershed, scenario) %>%
  summarise(
    avg_biomass = mean(biomass)
  ) %>% ungroup() %>%
  spread(scenario, avg_biomass) %>%
  mutate(diff = `scenario one` - base) %>%
  filter(diff < 0) %>% View()













