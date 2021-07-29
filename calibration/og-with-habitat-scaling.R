library(tidyverse)
source("calibration/scale_habitat_params.R")
source("calibration/old_temperature_delta_survival.R")

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
# Investigate temperature input
params <- scale_habitat_params(params)

# use old delta temp survival values
proxy_2000_pp <- which.min(sapply(1:20, function(i) sum(abs(params$prisoners_point_temps[i] - params$prisoners_point_temps[,21]))))
params$prisoners_point_temps <- cbind(old_temp$prisoners_point, old_temp$prisoners_point[,proxy_2000_pp])
dimnames(params$prisoners_point_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                 "Sep", "Oct", "Nov", "Dec"),
                                               c("1980", "1981", "1982", "1983",
                                                 "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
                                                 "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
                                                 "2000"))

proxy_2000_vern <- which.min(sapply(1:20, function(i) sum(abs(params$vernalis_temps[i] - params$vernalis_temps[,21]))))
params$vernalis_temps <- cbind(old_temp$vernalis, old_temp$vernalis[,proxy_2000_vern])
dimnames(params$vernalis_temps) <- list(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                          "Sep", "Oct", "Nov", "Dec"),
                                        c("1980", "1981", "1982", "1983",
                                          "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991",
                                          "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
                                          "2000"))


# scale habitat using vect2 2019 values

# dd <- set_synth_years(params)
# round(rowMeans(dd$avg_temp - cvpiaCalibration::fall_inputs$juv.tmp), 3)

old_seeds <- read_csv('calibration/filledknownAdults_1998_2016.csv') %>%
  select(-watershed, -order) %>%
  as.matrix()

# grandtab_imputed$fall[, 1:19] - old_seeds
# # 53957
# grandtab_imputed$fall[,1]
# old_seeds[,1]
sim <- fall_run_model(seeds = old_seeds,
                      mode = "calibrate", ..params = params)

keep_num <- c(1,6,7,10,12,19,20,23,26:30)

grand_tab <- as_tibble(old_seeds[keep_num, ]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep_num]) %>%
  gather(year, spawners, -watershed) %>%
  filter(!is.na(spawners)) %>%
  mutate(year = parse_number(year),
         observed_nat_spawn = spawners*(1-fallRunDSM::params$proportion_hatchery[watershed]))

nat_spawn <- as_tibble(sim[keep_num, ]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep_num]) %>%
  gather(year, nat_spawn, -watershed) %>%
  mutate(year = parse_number(year) + 5)

both <- nat_spawn %>%
  left_join(grand_tab)

both %>%
  select(-spawners) %>%
  # group_by(watershed) %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn, use = 'pairwise.complete.obs'))

run_model <- function(i) {
  sim <- fall_run_model(seeds = old_seeds,
                        mode = "calibrate", ..params = params)
  return(sim)
}

library(tictoc)
tic('250 runs')
results_og <- parallel::mclapply(1:250, function(i) {run_model(i)}, mc.cores = 7)
toc()

keep <- DSMscenario::watershed_labels[c(1,6,7,10,12,19,20,23,26:30)]

nat_spawn <- map_df(1:250, function(i) {
  as_tibble(results_og[[i]]) %>%
    mutate(watershed = DSMscenario::watershed_labels, run = i) %>%
    gather(year, nat_spawn, -watershed, -run) %>%
    mutate(year = parse_number(year) + 5)
})


all <- nat_spawn %>%
  filter(watershed %in% keep) %>%
  left_join(grand_tab)

all %>%
  filter(!(watershed %in% remove_these)) %>%
  ggplot(aes(x = year, group = run)) +
  geom_line(aes(y = nat_spawn), alpha = .1) +
  geom_line(aes(y = observed_nat_spawn), alpha = .1, color = 'red') +
  facet_wrap(~watershed, scales = 'free_y')

all %>%
  filter(!(watershed %in% remove_these)) %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn), observed_nat_spawn = mean(observed_nat_spawn)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = nat_spawn)) +
  geom_line(aes(y = observed_nat_spawn), color = 'red') +
  facet_wrap(~watershed, scales = 'free_y')

all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  ungroup() %>%
  filter(!is.na(observed_nat_spawn)) %>%
  ggplot(aes(nat_spawn, observed_nat_spawn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

all %>%
  group_by(watershed, year) %>%
  summarise(nat_spawn = mean(nat_spawn),
            observed_nat_spawn = mean(observed_nat_spawn)) %>%
  filter(!is.na(observed_nat_spawn)) %>%
  ungroup() %>%
  summarise(r = cor(nat_spawn, observed_nat_spawn))


# library(tidyverse)
# r2_sim <- sim
#
# r2_nat_spawners <- as_tibble(r2_sim$natural_spawners) %>%
#   mutate(watershed = DSMscenario::watershed_labels) %>%
#   gather(year, spawners, -watershed) %>%
#   mutate(type = "simulated") %>%
#   filter(!(watershed %in% remove_these)) %>%
#   bind_rows(
#     as_tibble(sim2$natural_spawners) %>%
#       mutate(watershed = DSMscenario::watershed_labels) %>%
#       gather(year, spawners, -watershed) %>%
#       mutate(type = "simulated not mixed") %>%
#       filter(!(watershed %in% remove_these))
#   )
#
# observed <- as_tibble(DSMCalibrationData::grandtab_observed$fall) %>%
#   mutate(watershed = DSMscenario::watershed_labels) %>%
#   gather(year, obs_spawners, -watershed) %>%
#   mutate(year = as.numeric(year) - 1997) %>%
#   filter(!is.na(obs_spawners))
#
# r2_nat_spawners %>%
#   mutate(year = as.numeric(year),
#          prop_nat = 1-fallRunDSM::params$proportion_hatchery[watershed]) %>%
#   left_join(observed) %>%
#   # filter(spawners < 100000) %>%
#   # mutate(obs_spawners = prop_nat * obs_spawners) %>%
#   ggplot(aes(year, spawners)) +
#   geom_line(aes(color = type)) +
#   geom_point(aes(y = obs_spawners)) +
#   facet_wrap(~watershed, scales = 'free_y')
#
#
# r2_observed <- as_tibble(r2_sim$proportion_natural * DSMCalibrationData::grandtab_observed$fall) %>%
#   mutate(watershed = DSMscenario::watershed_labels) %>%
#   gather(year, spawners, -watershed) %>%
#   mutate(type = "observed") %>%
#   filter(!is.na(spawners),
#          !(watershed %in% remove_these))
#
# r2_eval <- bind_rows(r2_nat_spawners, r2_observed) %>%
#   mutate(year = as.numeric(year))
#
# r2_eval %>%
#   ggplot(aes(year, spawners, color = type)) +
#   geom_point() +
#   facet_wrap(~watershed, scales = "free_y")
#
# r2_eval %>%
#   spread(type, spawners) %>%
#   ggplot(aes(observed, simulated)) + geom_point() #+ facet_wrap(~watershed, scales = "free")
#
# r2_eval %>%
#   spread(type, spawners) %>%
#   filter(!is.na(observed) | !is.na(simulated)) %>%
#   group_by(watershed) %>%
#   summarise(
#     r = cor(observed, simulated, use = "pairwise.complete.obs")
#   )
#
# r2_eval %>%
#   spread(type, spawners) %>%
#   filter(!is.na(observed) | !is.na(simulated)) %>%
#   summarise(
#     r = cor(observed, simulated, use = "pairwise.complete.obs")
#   )
#
# # apply ha
#
# og_eval <- read_rds("~/projects/og-eval.rds") %>%
#   mutate(kind = "original")
#
#
# comparison <- bind_rows(updated_eval_df, og_eval)
#
#
# comparison %>%
#   filter(type == "simulated") %>%
#   ggplot(aes(year, spawners, color = kind)) + geom_point() + facet_wrap(~watershed, scales = "free") +
#   geom_point(data = comparison %>% filter(type == "observed", kind == "original"),
#              aes(year, spawners), color = "black")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
