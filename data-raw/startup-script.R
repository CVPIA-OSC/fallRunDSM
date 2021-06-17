library(tidyverse)
library(DSMscenario)
library(tictoc)
library(fallRunDSM)
library(furrr)

library(parallel)
cores <- detectCores()
plan(multisession, workers = 8)
# check what version of packagea are installed
# use packageVersion() to check version of package installed

# load data

run_model <- function(scenario = NULL) {
  list2env(load_baseline_data(), .GlobalEnv)
  fall_run_seeds <- fall_run_model(mode = "seed")
  fall_run_model(scenario = scenario, mode = "simulate", seeds = fall_run_seeds)
}

# set the seeds
fall_run_seeds <- fall_run_model(mode = "seed")

base_runs <- parallel::mclapply(X = 1:10, run_model, mc.cores = cores)

tic("start model")
base_runs <- future_map(1:10, ~run_model(), seed=NULL)
toc()

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







# get library support needed to run the code
clusterEvalQ(cl,library(MASS))
# put objects in place that might be needed for the code
myData <- data.frame(x=1:10, y=rnorm(10))
clusterExport(cl,c("myData"))
# Set a different seed on each member of the cluster (just in case)
clusterSetRNGStream(cl)
#... then parallel replicate...
parSapply(cl, 1:10000, function(i,...) { x <- rnorm(10); mean(x)/sd(x) } )
#stop the cluster
stopCluster(cl)












