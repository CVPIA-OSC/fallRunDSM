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
keep <- c(1,6,7,10,12,19,20,23,26:30)
prop_hatch_to_keep <- fallRunDSM::params$proportion_hatchery[keep]
watersheds_keep <- DSMscenario::watershed_labels[keep]
names(prop_hatch_to_keep) <- watersheds_keep

current_best_solution <- read_rds("calibration/fits/result-1-2021-08-10.rds")

# add solution to evaluate
solution <- current_best_solution@solution
# solution <- res_stoch_corr_penalty@solution

params_calibrate_mode <- update_params(x = solution, fallRunDSM::params)
params_calibrate_mode <- set_synth_years(params_calibrate_mode)
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
  as_tibble(results[[i]][keep, ]) %>%
    mutate(watershed = DSMscenario::watershed_labels[keep], run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = readr::parse_number(year) + 5)
})

grand_tab <- as_tibble(DSMCalibrationData::grandtab_observed$fall[keep,]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  filter(!is.na(spawners)) %>%
  mutate(year = as.numeric(year) - 1997,
         observed_nat_spawn = round(((1-prop_hatch_to_keep[watershed]) * spawners)))

all <- nat_spawn %>%
  left_join(grand_tab) %>%
  filter(year > 5)

all %>%
  ggplot(aes(x = year, group = run)) +
  geom_line(aes(y = nat_spawn), alpha = .1) +
  geom_line(aes(y = observed_nat_spawn), alpha = .1, color = 'red') +
  # geom_point(aes(y = observed_nat_spawn), alpha = .1, color = 'red', size=.8) +
  facet_wrap(~watershed, scales = 'free_y')

all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  ungroup() %>%
  filter(!is.na(observed_nat_spawn)) %>%
  ggplot(aes(observed_nat_spawn, nat_spawn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

plot_watershed_scatter <- function(w) {
  d <- all %>%
    filter(watershed == w) %>%
    group_by(year) %>%
    summarise(nat_spawn = mean(nat_spawn),
              observed_nat_spawn = mean(observed_nat_spawn)) %>%
    ungroup() %>%
    filter(!is.na(observed_nat_spawn))

  upper_lim <- min(50000, max(d$observed_nat_spawn))


  d %>%
    ggplot(aes(observed_nat_spawn, nat_spawn)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    xlim(0, upper_lim) +
    ylim(0, upper_lim) +
    labs(title = paste(w, "r = ", round(cor(d$nat_spawn, d$observed_nat_spawn), 4)),
         x = "Observed Natural Spawners",
         y = "Predicted Natural Spawners")

}

plot_watershed_scatter("Deer Creek")

# valley wide correlation
all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  filter(!is.na(observed_nat_spawn)) %>%
  ungroup() %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn))

# watershed level correlations
all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  filter(!is.na(observed_nat_spawn)) %>%
  group_by(watershed) %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn)) %>%
  arrange(desc(r))




