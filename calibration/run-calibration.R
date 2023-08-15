library(fallRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

source("calibration/fitness.R")
source("calibration/update-params.R")


current_best_solution <- read_rds("calibration/calibration-result.rds")

params <- DSMCalibrationData::set_synth_years(fallRunDSM::params_2022)
# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -fall_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$fall,
              seeds = DSMCalibrationData::grandtab_imputed$fall,
              params = params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
              x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
              x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
              x[38], x[39], x[40]
            ),
          lower = c(2.5, rep(-3.5, 39)),
          upper = rep(3.5, 40),
          popSize = 150,
          maxiter = 10000,
          run = 50,
          parallel = TRUE,
          pmutation = .4)

readr::write_rds(res, paste0("calibration/fits/result-", Sys.Date(), ".rds"))

# Evaluate Results ------------------------------------

keep <- c(1,6,7,10,12,19,20,23,26:30)
result_solution <- res@solution[1, ]
result_params <- update_params(x = result_solution, fallRunDSM::params)
result_params <- DSMCalibrationData::set_synth_years(result_params)
result_sim <- fall_run_model(seeds = DSMCalibrationData::grandtab_imputed$fall, mode = "calibrate",
                         ..params = result_params,
                         stochastic = FALSE)

result_nat_spawners <- as_tibble(result_sim[keep, ,drop = F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)

result_observed <- as_tibble((1 - fallRunDSM::params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$fall[keep,, drop=F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed", year = as.numeric(year) - 1997) %>%
  filter(!is.na(spawners),
         year > 5)

result_eval_df <- bind_rows(result_nat_spawners, result_observed)

# Time series observed vs simulated by watershed
result_eval_df %>%
  ggplot(aes(year, spawners, color = type)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y")

# Scatter: observed vs simulated
result_eval_df %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Observed vs Predicted",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners") +
  xlim(0, 60000) +
  ylim(0, 30000)

# Correlation by watershed
result_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

# Valley wide correlation
result_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )


