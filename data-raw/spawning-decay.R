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

first_habitat <- arrange(upper_sac_spawn_hab, date)$habitat_acres[1]

upper_sac_decay <- watershed_decays$`Upper Sacramento River`
upper_sac_spawning_decayed <- upper_sac_decay |>
  filter(decay_type == "decay_min") |>
  left_join(upper_sac_spawn_hab) |>
  mutate(
    accum_decay = cumsum(decay_acres_month),
    diff_from_mean = avg_spawn_hab - accum_decay,
    diff_from_first = first_habitat - accum_decay,
    prop_hab_of_mean = habitat_acres / avg_spawn_hab,
    prop_hab_of_first = habitat_acres / first_habitat,
    hab_after_decay_mean = diff_from_mean * prop_hab_of_mean,
    hab_after_decay_first = diff_from_first * prop_hab_of_first
  ) |>
  select(date, hab_after_decay_mean )

upper_sac_spawning_decayed |>
  left_join(upper_sac_spawn_hab) |>
  mutate(prop_decayed = hab_after_decay_mean / habitat_acres) |>
  pivot_longer(cols=hab_after_decay_mean:habitat_acres, names_to = "habitat_type", values_to = "habitat",
               names_transform = \(x) ifelse(x == "hab_after_decay_mean", "decayed", "no decay")) |>
  filter(lubridate::month(date) %in% 10:12) |>
  ggplot(aes(date, habitat, fill = habitat_type)) + geom_col(position = "dodge")



apply_decay <- function(watershed, spawning_habitat, decay_curve, decay_level = c("min", "avg", "max")) {
  decay
}
