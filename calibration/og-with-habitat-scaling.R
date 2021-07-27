# TODO
# this version of the model bring back the old sac outmigration survival and
# the juv delta survival. This causes nat spawn numbers to be REALLY big. We need
# update this so that we can get similar values to running the latest version of
# their model from 11/13/2019

source("calibration/scale_habitat_params.R")

# 2019 calibrated coefficients
params <- fallRunDSM::params

# overwrite to 2019 habitat values
params$inchannel_habitat_fry <- cvpiaData::fr_fry
dimnames(params$inchannel_habitat_fry) <- dimnames(fallRunDSM::params$inchannel_habitat_fry)
params$inchannel_habitat_juvenile <-cvpiaData::fr_juv
dimnames(params$inchannel_habitat_juvenile) <- dimnames(fallRunDSM::params$inchannel_habitat_juvenile)
params$floodplain_habitat <- cvpiaData::fr_fp
dimnames(params$floodplain_habitat) <- dimnames(fallRunDSM::params$floodplain_habitat)
params$spawning_habitat <- cvpiaData::fr_spawn
dimnames(params$spawning_habitat) <- dimnames(fallRunDSM::params$spawning_habitat)

#
params_synth_years <- fallRunDSM::params

# overwrite to 2019 habitat values
params_synth_years$inchannel_habitat_fry <- cvpiaData::fr_fry
dimnames(params_synth_years$inchannel_habitat_fry) <- dimnames(fallRunDSM::params$inchannel_habitat_fry)
params_synth_years$inchannel_habitat_juvenile <-cvpiaData::fr_juv
dimnames(params_synth_years$inchannel_habitat_juvenile) <- dimnames(fallRunDSM::params$inchannel_habitat_juvenile)
params_synth_years$floodplain_habitat <- cvpiaData::fr_fp
dimnames(params_synth_years$floodplain_habitat) <- dimnames(fallRunDSM::params$floodplain_habitat)
params_synth_years$spawning_habitat <- cvpiaData::fr_spawn
dimnames(params_synth_years$spawning_habitat) <- dimnames(fallRunDSM::params$spawning_habitat)


# Investigate temperature input

# scale habitat using vect2 2019 values
params <- scale_habitat_params(params)
params_synth_years <- scale_habitat_params(params_synth_years)

# reorder inputs to 1998-2017
params_synth_years <- DSMCalibrationData::set_synth_years(params_synth_years)

# set seed with first 5 years of grandtab
calib_seeds <- matrix(0, nrow = 31, ncol = 25)
calib_seeds[, 1:5] <- DSMCalibrationData::grandtab_imputed$fall[, 1:5]

run_model_synth_years <- function(i) {
  sim <- fall_run_model(scenario = DSMscenario::scenarios$NO_ACTION_GRANDTAB,
                        seeds = calib_seeds, mode = "simulate", ..params = params_synth_years)
  return(sim)
}

run_model <- function(i) {
  sim <- fall_run_model(scenario = DSMscenario::scenarios$NO_ACTION,
                        seeds = calib_seeds, mode = "simulate", ..params = params)
  return(sim)
}

library(tictoc)
tic('250 runs')
results_synth_years <- parallel::mclapply(1:250, function(i) {run_model_synth_years(i)}, mc.cores = 7)
toc()

tic('250 runs')
results <- parallel::mclapply(1:250, function(i) {run_model(i)}, mc.cores = 7)
toc()

nat_spawn <- map_df(1:250, function(i) {
  as_tibble(results_og[[i]]$natural_spawner) %>%
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
  left_join(grand_tab) %>%
  filter(year > 5)

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












library(tidyverse)
r2_sim <- sim

r2_nat_spawners <- as_tibble(r2_sim$natural_spawners) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated") %>%
  filter(!(watershed %in% remove_these)) %>%
  bind_rows(
    as_tibble(sim2$natural_spawners) %>%
      mutate(watershed = DSMscenario::watershed_labels) %>%
      gather(year, spawners, -watershed) %>%
      mutate(type = "simulated not mixed") %>%
      filter(!(watershed %in% remove_these))
  )

observed <- as_tibble(DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, obs_spawners, -watershed) %>%
  mutate(year = as.numeric(year) - 1997) %>%
  filter(!is.na(obs_spawners))

r2_nat_spawners %>%
  mutate(year = as.numeric(year),
         prop_nat = 1-fallRunDSM::params$proportion_hatchery[watershed]) %>%
  left_join(observed) %>%
  # filter(spawners < 100000) %>%
  # mutate(obs_spawners = prop_nat * obs_spawners) %>%
  ggplot(aes(year, spawners)) +
  geom_line(aes(color = type)) +
  geom_point(aes(y = obs_spawners)) +
  facet_wrap(~watershed, scales = 'free_y')


r2_observed <- as_tibble(r2_sim$proportion_natural * DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed") %>%
  filter(!is.na(spawners),
         !(watershed %in% remove_these))

r2_eval <- bind_rows(r2_nat_spawners, r2_observed) %>%
  mutate(year = as.numeric(year))

r2_eval %>%
  ggplot(aes(year, spawners, color = type)) +
  geom_point() +
  facet_wrap(~watershed, scales = "free_y")

r2_eval %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() #+ facet_wrap(~watershed, scales = "free")

r2_eval %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

r2_eval %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

# apply ha

og_eval <- read_rds("~/projects/og-eval.rds") %>%
  mutate(kind = "original")


comparison <- bind_rows(updated_eval_df, og_eval)


comparison %>%
  filter(type == "simulated") %>%
  ggplot(aes(year, spawners, color = kind)) + geom_point() + facet_wrap(~watershed, scales = "free") +
  geom_point(data = comparison %>% filter(type == "observed", kind == "original"),
             aes(year, spawners), color = "black")














