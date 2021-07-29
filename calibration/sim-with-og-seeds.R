# run the model with the seeds derived from the seeds obtained by running the
# original model

source("calibration/scale_habitat_params.R")
# seeds derived from running the original model up to the 5 years
og_seeds <-
  structure(c(19232, 0, 1394, 1, 0, 91, 7762, 3526, 1489, 7, 2,
              93, 5, 0, 0, 0, 0, 0, 20113, 111, 0, 0, 129791, 0, 0, 3, 581,
              65, 448, 10, 0, 9868, 0, 723, 0, 0, 23, 5008, 1817, 652, 3, 1,
              25, 0, 0, 0, 0, 0, 0, 11594, 66, 0, 0, 74607, 0, 0, 0, 367, 32,
              275, 5, 0, 2570, 0, 150, 0, 0, 5, 2772, 562, 159, 1, 0, 12, 0,
              0, 0, 0, 0, 0, 7228, 31, 0, 0, 25865, 0, 0, 0, 464, 7, 67, 5,
              0, 749, 0, 39, 0, 0, 2, 861, 182, 44, 0, 0, 7, 0, 0, 0, 0, 0,
              0, 1674, 9, 0, 0, 3043, 0, 0, 0, 235, 0, 2, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L,
                                                                           25L))

scaled_habitat_params <- scale_habitat_params(fallRunDSM::params)

s <- fall_run_model()
model_with_original_seeds <- fall_run_model(seeds = s,
                                            ..params = fallRunDSM::params,
                                            mode = "simulate")



old_model_many_runs <- map_df(1:500, function(i) {
  as_tibble(base$nat_spawners[,,i]) %>%
    mutate(run = i,
           watershed = DSMscenario::watershed_labels) %>%
    gather(year, nat_spawners, -run, -watershed) %>%
    mutate(year = readr::parse_number(year))
}) %>%
  mutate(scenario = "version2019")


new_model <- as_tibble(model_with_original_seeds$natural_spawners) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, nat_spawners, -watershed) %>%
  mutate(year = readr::parse_number(year),
         type = "version2021")

models <- bind_rows(old_model, new_model)


w <- "Feather River"
old_model_many_runs %>%
  filter(watershed == w) %>%
  ggplot(aes(year, nat_spawners, group = run)) + geom_line(alpha=.01) +
  geom_line(data = new_model %>% filter(watershed == w),
            aes(year, nat_spawners), color = "red", inherit.aes = FALSE)


models %>%
  ggplot(aes(year, nat_spawners, color = type)) + geom_line() +
  facet_wrap(~watershed, scale = "free")

