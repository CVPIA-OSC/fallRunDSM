library(tidyverse)
library(lubridate)

# old model ----
load('calibration/0_output.rds')
base <- to_write_out

load('calibration/1_output.rds')
s1 <- to_write_out

load('calibration/7_output.rds')
s7 <- to_write_out

load('calibration/1_og_output.rds')
s1_isnan <- to_write_out

load('calibration/7_og_output.rds')
s7_isnan <- to_write_out


base_valley_nat_spawners <- map_dbl(1:500, ~colSums(base$nat_spawners[,,.])[20])
s1_valley_nat_spawners <- map_dbl(1:500, ~colSums(s1$nat_spawners[,,.])[20])
s7_valley_nat_spawners <- map_dbl(1:500, ~colSums(s7$nat_spawners[,,.])[20])

plot(1:500, cummean(base_valley_nat_spawners))
plot(1:500, cummean(s1_valley_nat_spawners))
plot(1:500, cummean(s7_valley_nat_spawners))

(mean(s1_valley_nat_spawners) - mean(base_valley_nat_spawners))/ mean(base_valley_nat_spawners)
(mean(s7_valley_nat_spawners) - mean(base_valley_nat_spawners))/ mean(base_valley_nat_spawners)

base_biomass <- map_dbl(1:500, ~colSums(base$juv_biomass[,,.])[20])
s1_biomass <- map_dbl(1:500, ~colSums(s1$juv_biomass[,,.])[20])
s7_biomass <- map_dbl(1:500, ~colSums(s7$juv_biomass[,,.])[20])

(mean(s1_biomass) - mean(base_biomass))/ mean(base_biomass)
(mean(s7_biomass) - mean(base_biomass))/ mean(base_biomass)

# new model ---
d <- read_rds('calibration//base_s1_s2_s7_1000_runs.rds')
glimpse(d)
unique(d$scenario)
d %>%
  group_by(run, year, scenario) %>%
  summarise(valley_wide = sum(nat_spawners)) %>%
  group_by(scenario, year) %>%
  summarise(mean_valley_wide = mean(valley_wide)) %>%
  filter(year == 20) %>%
  spread(scenario, mean_valley_wide) %>%
  mutate(s1 = (`scenario 1` - base) / base,
         s2 = (`scenario 2` - base) / base,
         s7 = (`scenario 7` - base) / base) %>%
  select(s1, s7)

habitats <- list(
  spawning_habitat = fallRunDSM::params$spawning_habitat,
  inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat = fallRunDSM::params$floodplain_habitat,
  weeks_flooded = fallRunDSM::params$weeks_flooded
)
library(DSMscenario)
base <- load_scenario(scenario = DSMscenario::scenarios$NO_ACTION,
                      species = DSMscenario::species$FALL_RUN,
                      habitat_inputs = habitats)

scenario1 <- load_scenario(scenario = DSMscenario::scenarios$ONE,
                           species = DSMscenario::species$FALL_RUN,
                           habitat_inputs = habitats)

scenario2 <- load_scenario(scenario = DSMscenario::scenarios$TWO,
                           species = DSMscenario::species$FALL_RUN,
                           habitat_inputs = habitats)

scenario7 <- load_scenario(scenario = DSMscenario::scenarios$SEVEN,
                           species = DSMscenario::species$FALL_RUN,
                           habitat_inputs = habitats)

fry_base <- base$inchannel_habitat_fry %>% DSMhabitat::square_meters_to_acres()
fry_s1 <- scenario1$inchannel_habitat_fry %>% DSMhabitat::square_meters_to_acres()
fry_s2 <- scenario2$inchannel_habitat_fry %>% DSMhabitat::square_meters_to_acres()

fp_base <- base$floodplain_habitat %>% DSMhabitat::square_meters_to_acres()
fp_s7 <- scenario7$floodplain_habitat %>% DSMhabitat::square_meters_to_acres()

fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels,
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>%
  arrange(year, month, watershed) %>%
  mutate(
    fry_base = as.vector(fry_base),
    fry_s1 = as.vector(fry_s1),
    fry_s2 = as.vector(fry_s2))

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels,
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>%
  arrange(year, month, watershed) %>%
  mutate(
    fp_base = as.vector(fp_base),
    fp_s7 = as.vector(fp_s7))

s1_watersheds_index <- sapply(1:31, function(i) {any(scenarios$ONE$inchannel[i, ] > 0)})
s2_watersheds_index <- sapply(1:31, function(i) {any(scenarios$TWO$inchannel[i, ] > 0)})
s7_watersheds_index <- sapply(1:31, function(i) {any(scenarios$SEVEN$floodplain[i, ] > 0)})


fry %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), fry_base, fry_s1) %>%
  filter(watershed %in% DSMscenario::watershed_labels[s1_watersheds_index]) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')

fry %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), fry_base, fry_s1) %>%
  filter(!(watershed %in% DSMscenario::watershed_labels[s1_watersheds_index])) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')


fry %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), fry_base, fry_s2) %>%
  filter(watershed %in% DSMscenario::watershed_labels[s2_watersheds_index]) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')

fry %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), fry_base, fry_s2) %>%
  filter(!(watershed %in% DSMscenario::watershed_labels[s2_watersheds_index])) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')


fp %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), fp_base, fp_s7) %>%
  filter(watershed %in% DSMscenario::watershed_labels[s7_watersheds_index]) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, fill = version)) +
  geom_col(position = 'dodge')
  facet_wrap(~watershed, scales = 'free_y')


