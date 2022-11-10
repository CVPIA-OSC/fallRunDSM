library(dplyr)
library(tidyr)
library(ggplot2)

fallRunDSM::params$spawning_habitat

watershed_decays <- readr::read_rds("data-raw/watershed-decay-acres.rds")


# spawning_habitat <-
avg_spawn_hab <- mean(upper_sac_spawn_hab$habitat_acres)

upper_sac_spawn_hab <- fallRunDSM::params$spawning_habitat["Upper Sacramento River",,] |>
  as_tibble() |>
  mutate(month = 1:12) |>
  pivot_longer(-month, names_to = "year", values_to = "habitat_sqm") |>
  mutate(habitat_acres = DSMhabitat::square_meters_to_acres(habitat_sqm),
         date = lubridate::ymd(paste0(year, "-", month, "-", lubridate::days_in_month(month)))) |>
  select(date, habitat_acres)

upper_sac_decay <- watershed_decays$`Upper Sacramento River`
upper_sac_decay |>
  filter(decay_type == "decay_min") |>
  left_join(upper_sac_spawn_hab) |>
  mutate(
    accum_decay = cumsum(decay_acres_month),
    diff_from_mean = avg_spawn_hab - accum_decay,
    prop_hab_of_mean = habitat_acres / avg_spawn_hab,
    hab_after_decay = diff_from_mean * prop_hab_of_mean
  ) |>
  ggplot(aes(date, hab_after_decay)) + geom_col()


apply_decay <- function(watershed, spawning_habitat, decay_curve, decay_level = c("min", "avg", "max")) {
  decay
}
