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

cores <- parallel::detectCores()

# add solution to evaluate
solution <- res_stoch_round3@solution
# solution <- res_stoch_corr_penalty@solution

params_calibrate_mode <- update_params(x = solution, fallRunDSM::params)
run_model <- function(i) {
  sim <- fall_run_model(seeds = DSMCalibrationData::grandtab_imputed$fall, mode = "calibrate",
                        ..params = params_calibrate_mode)
  return(sim)
}


library(tictoc)
tic('250 runs')
results <- parallel::mclapply(1:250, function(i) {run_model(i)}, mc.cores = 15)
toc()

nat_spawn <- map_df(1:250, function(i) {
  as_tibble(results[[i]]) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = readr::parse_number(year) + 5)
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

all %>%
  filter(!(watershed %in% remove_these)) %>%
  ggplot(aes(x = year, group = run)) +
  geom_line(aes(y = nat_spawn), alpha = .1) +
  # geom_line(aes(y = observed_nat_spawn), alpha = .1, color = 'red') +
  geom_point(aes(y = observed_nat_spawn), alpha = .1, color = 'red', size=.8) +
  facet_wrap(~watershed, scales = 'free_y')

all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  ungroup() %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_spawn)) %>%
  ggplot(aes(observed_nat_spawn, nat_spawn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

# valley wide correlation
all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_spawn)) %>%
  ungroup() %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn))

# watershed level correlations
all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_spawn)) %>%
  group_by(watershed) %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn))




