library(dplyr)

set.seed(123119)
sensi_seeds <- fallRunDSM::fall_run_model(mode = "seed")


sensitivity_fall_run_model <- function(scenario, scenarios, sensi_seeds) {

  which_surv = scenarios[scenario, ]$which_surv
  location_surv = scenarios[scenario, ]$location_surv
  month_surv = scenarios[scenario, ]$month_surv

  model_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                  seeds = sensi_seeds,
                                                  which_surv = which_surv,
                                                  location_surv = location_surv,
                                                  month_surv = month_surv)

  output <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels,
                  survival_target = which_surv,
                  location_target = location_surv,
                  month_target = month_surv,
                  id = scenario) |>
    dplyr::select(id, location, survival_target, location_target, month_target, `1`:`20`)

  return(output)

}

# run scenarios in parallel -------
library(tictoc) # measure time to run
library(parallel)
library(doParallel)

# All 31 watersheds in model
# Delta
# Bay Delta

# scenarios to evaluate
scenarios1 <- expand.grid(location_surv = fallRunDSM::watershed_labels,
                          month_surv = c(1:8),
                          which_surv = c("juv_rear"))

scenarios4 <- expand.grid(location_surv = c("North Delta", "South Delta"),
                          month_surv = c(1:8),
                          which_surv = c("juv_rear"))

scenarios2  <- expand.grid(location_surv = c("Lower-mid Sacramento River",
                                             "Lower Sacramento River", "Sutter Bypass", "Yolo Bypass",
                                             "Delta", "Bay Delta", "San Joaquin River",
                                             "Upper-mid Sacramento River"
                                             ),
                           month_surv = c(1:8),
                           which_surv = "juv_migratory")

# pull spawning names from watersheds that have spawning for Fall Run
spawning_watersheds <- DSMhabitat::watershed_lengths %>%
  filter(lifestage == "spawning") %>%
  filter(species == "fr") %>%
  pull(watershed)

scenarios3 <- expand.grid(location_surv = spawning_watersheds,
                           month_surv = NA,
                           which_surv = "egg_to_fry")


# set up for running function in parallel
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)

clusterExport(cl, list("sensitivity_fall_run_model", "sensi_seeds",
                       "scenarios1", "scenarios2", "scenarios3", "scenarios4"))


tic("parallel 1")
scenario_results_list1 <- parLapply(cl, 1:nrow(scenarios1),
                                    fun = function(scenario) {
                                      sensitivity_fall_run_model(scenario, scenarios1, sensi_seeds)
                                    })
toc()

tic("parallel 2")
scenario_results_list2 <- parLapply(cl, 1:nrow(scenarios2),
                                    fun = function(scenario) {
                                      sensitivity_fall_run_model(scenario, scenarios2, sensi_seeds)
                                    })
toc()

tic("parallel 3")
scenario_results_list3 <- parLapply(cl, 1:nrow(scenarios3),
                                    fun = function(scenario) {
                                      sensitivity_fall_run_model(scenario, scenarios3, sensi_seeds)
                                    })
toc()

tic("parallel 4")
scenario_results_list4 <- parLapply(cl, 1:nrow(scenarios4),
                                    fun = function(scenario) {
                                      sensitivity_fall_run_model(scenario, scenarios4, sensi_seeds)
                                    })
toc()

# combine into one
r1 <- scenario_results_list1 |> dplyr::bind_rows()
r2 <- scenario_results_list2 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r1$id))
r3 <- scenario_results_list3 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r2$id))
r4 <- scenario_results_list4 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r3$id))
#r5 <- scenario_results_list5 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r4$id))

# do nothing
model_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                seeds = sensi_seeds,
                                                which_surv = NA,
                                                location_surv = NA,
                                                month_surv = NA)

do_nothing <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels,
                survival_target = NA,
                location_target = NA,
                month_target = NA,
                id = max(r4$id) + 1) |>
  dplyr::select(id, location, survival_target, location_target, month_target, `1`:`20`)

results <- dplyr::bind_rows(r1, r2, r3, r4, do_nothing)

# remove zero rows:
results <- results %>%
  mutate(row_sum = rowSums(results[6:20])) %>%
  filter(row_sum > 0) %>%
  glimpse

write_csv(results, "analysis/fall_run_survival_sensi_model_ouput.csv")

#results <- read_csv('analysis/fall_run_survival_sensi_model_ouput.csv')

# exploratory plots
# juv_rear:
results %>%
  filter(is.na(survival_target)) %>%
  bind_rows(results %>%
              filter(survival_target == "juv_rear",
                     location_target == "Upper Sacramento River",
                     month_target == 1)) %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_point(aes(x = as.factor(year), y = natural_spawners, color = as.factor(id), alpha = 0.5)) +
  coord_flip()

# juv migratory:
results %>%
  filter(is.na(survival_target)) %>%
  bind_rows(results %>%
              filter(survival_target == "juv_migratory",
                     location_target == "Bay Delta",
                     month_target == 5)) %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_point(aes(x = as.factor(year), y = natural_spawners, color = as.factor(id), alpha = 0.5)) +
  coord_flip()

# egg to fry:
results %>%
  filter(is.na(survival_target)) %>%
  bind_rows(results %>%
              filter(survival_target == "egg_to_fry",
                     location_target == "Upper Sacramento River")) %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_point(aes(x = as.factor(year), y = natural_spawners, color = as.factor(id), alpha = 0.5)) +
  coord_flip()
