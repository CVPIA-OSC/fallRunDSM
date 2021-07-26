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

library(tictoc)
tic('250 runs')
results <- parallel::mclapply(1:250, function(i) {run_model(i)}, mc.cores = 7)
toc()

nat_spawn <- map_df(1:250, function(i) {
  as_tibble(results[[i]]$natural_spawner) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = as.numeric(year))
})

prop_nat <- map_df(1:250, function(i) {
  as_tibble(results[[i]]$proportion_natural) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, prop_nat, -watershed, -run) %>%
    mutate(year = as.numeric(year))
})

remove_these <- names(which(is.na(DSMCalibrationData::grandtab_observed$fall[, 1])))
prop_nat_mean_rates <- prop_nat %>%
  group_by(watershed) %>%
  summarise(mean_prop_nat = mean(prop_nat)) %>%
  ungroup() %>%
  mutate(fixed_rate = 1-fallRunDSM::params$proportion_hatchery[watershed]) %>%
  filter(!(watershed %in% remove_these))

grand_tab <- as_tibble(DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  filter(!is.na(spawners)) %>%
  left_join(prop_nat_mean_rates) %>%
  mutate(year = as.numeric(year) - 1997,
         observed_nat_fixed = round((fixed_rate * spawners)),
         observed_nat_mean = round((mean_prop_nat * spawners)))

all <- nat_spawn %>%
  left_join(grand_tab)

all %>%
  filter(!(watershed %in% remove_these)) %>%
  ggplot(aes(x = year, group = run)) +
  geom_line(aes(y = nat_spawn), alpha = .1) +
  geom_line(aes(y = observed_nat_fixed), alpha = .1, color = 'red') +
  facet_wrap(~watershed, scales = 'free_y')

all %>%
  filter(!(watershed %in% remove_these)) %>%
  group_by(watershed, run) %>%
  summarise(r = cor(nat_spawn, observed_nat_mean),
            r_fixed = cor(nat_spawn, observed_nat_fixed)) %>%
  filter(!is.na(r)) %>%
  # group_by(watershed) %>%
  ungroup() %>%
  summarise(mean(r), mean(r_fixed))

all %>%
  filter(year > 5) %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_fixed = mean(observed_nat_fixed),
            observed_nat_mean = mean(observed_nat_mean)) %>%
  ungroup() %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_fixed)) %>%
  # summarise(r = cor(nat_spawn, observed_nat_mean))
  ggplot(aes(nat_spawn, observed_nat_mean)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

all %>%
  filter(year > 5) %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_fixed = mean(observed_nat_fixed),
            observed_nat_mean = mean(observed_nat_mean)) %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_mean)) %>%
  ungroup() %>%
  summarise(r = cor(nat_spawn, observed_nat_mean))
