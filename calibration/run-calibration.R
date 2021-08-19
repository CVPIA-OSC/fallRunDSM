library(fallRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

source("calibration/fitness.R")
source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(fallRunDSM::params)

current_best_solution <- read_rds("calibration/calibration-result.rds")

# proportion of fall run in feather/yuba for year 2010-2012
fall_prop_feather_yuba <- 1 - mean(c(0.076777295, 0.056932196, 0.081441457))

known_adults <- DSMCalibrationData::grandtab_observed$fall
known_adults["Feather River", ] <- DSMCalibrationData::grandtab_observed$fall["Feather River", ] * fall_prop_feather_yuba
known_adults["Yuba River", ] <- DSMCalibrationData::grandtab_observed$fall["Yuba River", ] * fall_prop_feather_yuba

calibration_seeds <- DSMCalibrationData::grandtab_imputed$fall
calibration_seeds["Feather River", ] <- DSMCalibrationData::grandtab_imputed$fall["Feather River", ] * fall_prop_feather_yuba
calibration_seeds["Yuba River", ] <- DSMCalibrationData::grandtab_imputed$fall["Yuba River", ] * fall_prop_feather_yuba


# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -fall_run_fitness(
              known_adults = known_adults,
              seeds = calibration_seeds,
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
          pmutation = .3,
          suggestions = current_best_solution@solution)

readr::write_rds(res, paste0("calibration/fits/result-", Sys.Date(), ".rds"))

# Evaluate Results ------------------------------------

keep <- c(1,6,7,10,12,19,20,23,26:30)
r1_solution <- res@solution

r1_params <- update_params(x = r1_solution, fallRunDSM::params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- fall_run_model(seeds = DSMCalibrationData::grandtab_imputed$fall, mode = "calibrate",
                         ..params = r1_params,
                         stochastic = FALSE)


r1_nat_spawners <- as_tibble(r1_sim[keep, ,drop = F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)


r1_observed <- as_tibble((1 - fallRunDSM::params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$fall[keep,, drop=F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed", year = as.numeric(year) - 1997) %>%
  filter(!is.na(spawners),
         year > 5)



r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)


r1_eval_df %>%
  ggplot(aes(year, spawners, color = type)) + geom_line() + facet_wrap(~watershed, scales = "free_y")

r1_eval_df %>%
  spread(type, spawners) %>%
  # filter(watershed == "Yuba River") %>%
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Observed vs Predicted",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners") +
  xlim(0, 60000) +
  ylim(0, 30000)

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

current_cor_watersheds %>% arrange(watershed)


r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )


