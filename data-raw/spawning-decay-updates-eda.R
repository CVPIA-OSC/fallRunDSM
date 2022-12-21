library(fallRunDSM)

params_0 <- fallRunDSM::params
params_1 <- params_0
params_1$spawn_decay_multiplier <- DSMscenario::spawning_decay_multiplier$fr


s0 <- fall_run_model(..params = params_0)
s1 <- fall_run_model(..params = params_1)


sim_0 <- fall_run_model(seeds = s0, mode = "sim", ..params = params_0)
sim_1 <- fall_run_model(seeds = s1, mode = "sim", ..params = params_1)


d0 <- (sim_0$spawners * sim_0$proportion_natural) |>
  as_tibble() |>
  mutate(watershed = fallRunDSM::watershed_labels) |>
  pivot_longer(-watershed, names_to = "year", values_to = "spawners") |>
  mutate(type = "original spawn decay")

d1 <- (sim_1$spawners * sim_1$proportion_natural) |>
  as_tibble() |>
  mutate(watershed = fallRunDSM::watershed_labels) |>
  pivot_longer(-watershed, names_to = "year", values_to = "spawners") |>
  mutate(type = "new spawn decay")


d <- bind_rows(d0, d1) |> mutate(year = as.numeric(year))

d |>
  filter(watershed == "Upper Sacramento River") |>
  ggplot(aes(year, spawners, color = type)) + geom_line()


x <- purrr::map_df(1:31, function(i) {
  DSMhabitat::spawning_decay_multiplier$fr[i,,] |>
    as_tibble() |>
    mutate(month = month.abb,
           watershed = watershed_labels[i]) |>
    pivot_longer(`1979`:`2000`, names_to = "year", values_to = "prop")
}) |>
  mutate(year = as.numeric(year),
         month = factor(month, levels = month.abb),
         date = lubridate::as_date(paste0(year, "-", month, "-01")))


x |>
  filter(watershed %in% names(which(DSMhabitat::watershed_decay_status))) |>
  ggplot(aes(date, prop, color = watershed)) + geom_line() +
  facet_wrap(vars(watershed))

