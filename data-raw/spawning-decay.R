library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

fallRunDSM::params$spawning_habitat

watershed_decays <- readr::read_rds("data-raw/watershed-decay-acres.rds")
watershed_with_decay <- names(watershed_decays)

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
    hab_after_decay_from_mean = diff_from_mean * prop_hab_of_mean,
    hab_after_decay_from_first = diff_from_first * prop_hab_of_first
  ) |>
  select(date, hab_after_decay_from_mean )

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

watershed_spawning_hab <- purrr::map_df(watershed_with_decay, function(w) {
  fallRunDSM::params$spawning_habitat[w, ,] |>
    as_tibble() |> mutate(month = 1:12) |>
    pivot_longer(`1979`:`2000`, names_to = "year", values_to = "habitat") |>
    mutate(watershed = w,
           habitat = DSMhabitat::square_meters_to_acres(habitat))
}) |>
  mutate(
    date = lubridate::ymd(paste0(year, "-", month, "-", lubridate::days_in_month(month)))
  ) |>
  select(date, watershed, habitat)

watershed_spawning_hab_average <- watershed_spawning_hab |>
  group_by(watershed) |>
  summarise(avg = mean(habitat)) |>
  pull(avg) |>
  setNames(watershed_with_decay)


# For each watershed I need:
# 1. the average spawning habitat
# 2. time series of habitat
# 3. time series of decay curves (all three)

# get all watershed averages
watershed_spawnig_hab_avgs_acres <-
  DSMhabitat::square_meters_to_acres(apply(fallRunDSM::params$spawning_habitat[,10:12,], MARGIN = 1, mean))


# time series of watershed spawning habitat
spawning_array_to_data_frame <- function(d) {
  d |>
    as_tibble() |>
    mutate(month = 1:12) |>
    pivot_longer(-month, names_to = "year", values_to = "habitat_sqm") |>
    mutate(habitat_acres = DSMhabitat::square_meters_to_acres(habitat_sqm),
           date = lubridate::ymd(paste0(year, "-", month, "-", lubridate::days_in_month(month)))) |>
    select(date, habitat_acres)
}

watershed_spawning_habitats <-
  map(fallRunDSM::watershed_labels, \(x) spawning_array_to_data_frame(fallRunDSM::params$spawning_habitat[x,,])) |>
  set_names(fallRunDSM::watershed_labels)


# decays per watershed
watershed_decays <- readr::read_rds("data-raw/watershed-decay-acres.rds")

# apply decay to the habitat
compute_accum_decay <- function(watershed) {

  if (!(watershed %in% watershed_with_decay)) {
    watershed_spawning_habitats[[watershed]] |>
      transmute(
        date, decay_type = "none", decayed_habitat_acres = habitat_acres, prop_habitat_decayed = 1
      )
  } else {
    watershed_decays[[watershed]] |>
      left_join(watershed_spawning_habitats[[watershed]]) |>
      group_by(decay_type) |>
      mutate(
        accum_decay = cumsum(decay_acres_month),
        diff_from_mean = avg_spawn_hab - accum_decay,
        diff_from_first = first_habitat - accum_decay,
        prop_hab_of_mean = habitat_acres / avg_spawn_hab,
        prop_hab_of_first = habitat_acres / first_habitat,
        hab_after_decay_from_mean = diff_from_mean * prop_hab_of_mean,
        hab_after_decay_from_first = diff_from_first * prop_hab_of_first,
        prop_habitat_decayed = hab_after_decay_from_mean / habitat_acres
      ) |>
      ungroup() |>
      select(date, decay_type, decayed_habitat_acres = hab_after_decay_from_mean, prop_habitat_decayed)
  }
}

decayed_habitats <- map(fallRunDSM::watershed_labels, \(w) compute_accum_decay(w)) |>
  set_names(fallRunDSM::watershed_labels)
decayed_habitats$`Upper Sacramento River`






