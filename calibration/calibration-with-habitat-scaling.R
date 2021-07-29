source("calibration/scale_habitat_params.R")
source("calibration/update_params.R")
load('calibration/0_output.rds')

solution <- res@solution

params <- update_params(x = solution, modified_hab_params)
params <- scale_habitat_params(params)
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
results_scaled_habitat <- parallel::mclapply(1:250, function(i) {run_model(i)}, mc.cores = 7)
toc()

nat_spawn_sh <- map_df(1:250, function(i) {
  as_tibble(results_scaled_habitat[[i]]$natural_spawner) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = as.numeric(year))
})

prop_nat_sh <- map_df(1:250, function(i) {
  as_tibble(results_scaled_habitat[[i]]$proportion_natural) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, prop_nat, -watershed, -run) %>%
    mutate(year = as.numeric(year))
})

remove_these <- names(which(is.na(DSMCalibrationData::grandtab_observed$fall[, 1])))

prop_nat_mean_rates <- prop_nat_sh %>%
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

all <- nat_spawn_sh %>%
  left_join(grand_tab) %>%
  filter(year > 5)

all %>%
  filter(!(watershed %in% remove_these)) %>%
  ggplot(aes(x = year, group = run)) +
  geom_line(aes(y = nat_spawn), alpha = .1) +
  geom_line(aes(y = observed_nat_fixed), alpha = .1, color = 'red') +
  facet_wrap(~watershed, scales = 'free_y')


all %>%
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
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_fixed = mean(observed_nat_fixed),
            observed_nat_mean = mean(observed_nat_mean)) %>%
  filter(!(watershed %in% remove_these), !is.na(observed_nat_mean)) %>%
  ungroup() %>%
  summarise(r = cor(nat_spawn, observed_nat_mean))















r2_nat_spawners <- as_tibble(r2_sim$natural_spawners) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated") %>%
  filter(!(watershed %in% remove_these))


r2_observed <- as_tibble(r2_sim$proportion_natural * DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed") %>%
  filter(!is.na(spawners),
         !(watershed %in% remove_these))

r2_eval <- bind_rows(r2_nat_spawners, r2_observed) %>%
  mutate(year = as.numeric(year))

r2_eval %>%
  ggplot(aes(year, spawners, color = type)) + geom_point() + facet_wrap(~watershed, scales = "free_y")

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














