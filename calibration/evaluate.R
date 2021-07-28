library(GA)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(parallel)
library(fallRunDSM)
library(DSMCalibrationData)
library(DSMscenario)

source("calibration/update_params.R")
load('calibration/best-fit-2021-07-22.rds')

cores <- parallel::detectCores()

solution <- res@solution

params <- update_params(x = solution, fallRunDSM::params)
params <- DSMCalibrationData::set_synth_years(params)
calib_seeds <- matrix(0, nrow = 31, ncol = 25)
calib_seeds[, 1:5] <- DSMCalibrationData::grandtab_imputed$fall[, 1:5]

run_model <- function(i) {
  sim <- fall_run_model(scenario = DSMscenario::scenarios$NO_ACTION_GRANDTAB,
                        seeds = calib_seeds, mode = "simulate", ..params = params)
  return(sim)
}

params_calibrate_mode <- update_params(x = solution, fallRunDSM::params)
run_model_in_calibrate <- function(i) {
  sim <- fall_run_model(seeds = calib_seeds, mode = "calibrate",
                        ..params = params_calibrate_mode)
  return(sim)
}


library(tictoc)
tic('250 runs')
results <- parallel::mclapply(1:250, function(i) {run_model(i)}, mc.cores = 7)
toc()

tic('250 calibrate runs')
results_calib_mode <- parallel::mclapply(1:250, function(i) {run_model_in_calibrate(i)}, mc.cores = 7)
toc()


nat_spawn <- map_df(1:250, function(i) {
  as_tibble(results_calib_mode[[i]]$natural_spawner) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = as.numeric(year))
})

nat_spawn_2 <- map_df(1:250, function(i) {
  as_tibble(results_og_not_mixed[[i]]$natural_spawner) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = as.numeric(year))
})
remove_these <- names(which(is.na(DSMCalibrationData::grandtab_observed$fall[, 1])))
grand_tab <- as_tibble(DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  filter(!is.na(spawners)) %>%
  mutate(year = as.numeric(year) - 1997,
         observed_nat_spawn = round(((1-fallRunDSM::params$proportion_hatchery[watershed]) * spawners)))
all <- nat_spawn %>%
  left_join(grand_tab)
  # filter(year > 5)

all %>%
  filter(!(watershed %in% remove_these)) %>%
  ggplot(aes(x = year, group = run)) +
  geom_line(aes(y = nat_spawn), alpha = .1) +
  geom_line(aes(y = observed_nat_spawn), alpha = .1, color = 'red') +
  facet_wrap(~watershed, scales = 'free_y')
all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  ungroup() %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_spawn)) %>%
  ggplot(aes(nat_spawn, observed_nat_spawn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_spawn)) %>%
  ungroup() %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn))
