library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(readr)
library(lubridate)

# TO CREATE THE WATERSHED DECAY DATASETS ------------------------
# gravel_size_scaledown <- read_rds("data-raw/gravel-size-scaledowns.rds")
# rating_curve <- read_rds("data-raw/rating-curves-with-threshold-of-movement.rds")
# watersheds_with_decay <- read_csv("data-raw/watersheds-with-decays.csv")
#
# scaledown_final <- 0.6056206 * .13
#
# MIN_flow_cfs_to_sed_cfd_final <- approxfun(
#   x = rating_curve$flow_cfs,
#   y = rating_curve$sed_ft3_per_day_min *
#     gravel_size_scaledown$avg_fraction *
#     scaledown_final
# )
#
# AVG_flow_cfs_to_sed_cfd_final <- approxfun(
#   x = rating_curve$flow_cfs,
#   y = rating_curve$sed_ft3_per_day_avg *
#     gravel_size_scaledown$avg_fraction *
#     scaledown_final
# )
#
# MAX_flow_cfs_to_sed_cfd_final <- approxfun(
#   x = rating_curve$flow_cfs,
#   y = rating_curve$sed_ft3_per_day_max *
#     gravel_size_scaledown$avg_fraction *
#     scaledown_final
# )
#
# dsm_flows <- DSMflow::flows_cfs$biop_2008_2009 |>
#   # filter(year(date) %in% 1979:2000) |>
#   pivot_longer(names_to = "watershed", values_to = "flow_cfs", -date) |>
#   filter(watershed %in% watersheds_with_decay$watershed)
#
# # Exceedance probs --------------------------------------
#
# exceedance_curves <- map(watersheds_with_decay$watershed, function(w) {
#   d <- dsm_flows |> filter(watershed == w) |>
#     mutate(cume_dist = dplyr::cume_dist(-flow_cfs)) |>
#     arrange(desc(cume_dist))
#
#   approxfun(x = d$cume_dist, y = d$flow_cfs)
# }) |>
#   set_names(watersheds_with_decay$watershed)
#
#
# upper_sac_exceedance_at_1800 <- 0.0569578
#
# watershed_offsets <- map_dbl(watersheds_with_decay$watershed, function(w) {
#   exceedance_curves[[w]](upper_sac_exceedance_at_1800)
# }) |> set_names(watersheds_with_decay$watershed)
#
# watershed_decays <- map2(watersheds_with_decay$watershed, watershed_offsets, function(w, x) {
#   dsm_flows |>
#     filter(watershed == w, year(date) %in% 1979:2000) |>
#     mutate(flow_adjusted = flow_cfs - x,
#            decay_min = MIN_flow_cfs_to_sed_cfd_final(flow_adjusted),
#            decay_avg = AVG_flow_cfs_to_sed_cfd_final(flow_adjusted),
#            decay_max = MAX_flow_cfs_to_sed_cfd_final(flow_adjusted)
#     ) |>
#     pivot_longer(names_to = "decay_type", values_to = "decay_amount", -c(date, watershed,
#                                                                          flow_cfs, flow_adjusted)) |>
#     mutate(decay_cfd = ifelse(is.na(decay_amount), 0, decay_amount),
#            decay_cfm = decay_cfd * days_in_month(month(date)),
#            decay_sqm = decay_cfm / 2,
#            decay_acres_month = decay_sqm / 43560) |>
#     select(date, watershed, flow_cfs, decay_type, decay_acres_month)
# }) |>
#   set_names(watersheds_with_decay$watershed)
#
# write_rds(watershed_decays, "data-raw/watershed-decay-acres.rds")

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

watershed_spawning_hab <- purrr::map_df(watersheds_with_decay$watershed, function(w) {
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
  setNames(watersheds_with_decay$watershed)


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

  if (!(watershed %in% watersheds_with_decay$watershed)) {
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
      transmute(date,
                decay_type = stringr::str_extract(decay_type, "min|avg|max"),
                decayed_habitat_acres = hab_after_decay_from_mean,
                prop_habitat_decayed)
  }
}

decayed_habitats <- map(fallRunDSM::watershed_labels, \(w) compute_accum_decay(w)) |>
  set_names(fallRunDSM::watershed_labels)
decayed_habitats$`Upper Sacramento River`






