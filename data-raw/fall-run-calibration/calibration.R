library(GA)
library(fallRunDSMCalibration)
library(tidyverse)

# ..surv_adult_enroute_int = 3,
# ..surv_adult_prespawn_int = 3,
# ..surv_egg_to_fry_int = 0.041,
# ..surv_juv_rear_int = 3.5,
# ..surv_juv_rear_contact_points = -0.0068,
# ..surv_juv_rear_prop_diversions = -0.1755,
# ..surv_juv_rear_total_diversions = -0.0005,
# ..surv_juv_bypass_int = -3.5,
# ..surv_juv_delta_int = 1.4,
# ..surv_juv_delta_contact_points = (.0358 * -0.189),
# ..surv_juv_delta_total_diverted = (.5 * -0.0021),
# ..surv_juv_outmigration_sj_int = -3.5,
# ..surv_juv_outmigration_sac_int_one = 2.5,
# ..surv_juv_outmigration_sac_prop_diversions = (-3.51 * .05),
# ..surv_juv_outmigration_sac_total_diversions = (-.0021 * .215),
# ..surv_juv_outmigration_sac_int_two = 0.3,
# ..ocean_entry_success_int

seeds_frame <- read_csv("data-raw/fall-run-calibration/known-adults-2019.csv")
seeds <- seeds_frame$seed
known_adults <- seeds_frame[,]
list2env(load_baseline_data(), .GlobalEnv)
ocean_betas <- c(
  `Upper Sacramento River` = -0.5108849,
  `Antelope Creek` = 1.2,
  `Battle Creek` = 1.2,
  `Bear Creek` = 1.2,
  `Big Chico Creek` = 1.2,
  `Butte Creek` = -3.3233638,
  `Clear Creek` = 1.2,
  `Cottonwood Creek` = 1.2,
  `Cow Creek` = 1.2,
  `Deer Creek` = -3.2304288,
  `Elder Creek` = 1.2,
  `Mill Creek` = -3.4148335,
  `Paynes Creek` = 1.2,
  `Stony Creek` = 1.2,
  `Thomes Creek` = 1.2,
  `Upper-mid Sacramento River` = 1.2,
  `Sutter Bypass` = 1.2,
  `Bear River` = -3.5,
  `Feather River` = -3.5,
  `Yuba River` = -3.5,
  `Lower-mid Sacramento River` = 1.2,
  `Yolo Bypass` = 1.2,
  `American River` = -1.308341,
  `Lower Sacramento River` = 1.2,
  `Calaveras River` = -1.9841364,
  `Cosumnes River` = -1.9841364,
  `Mokelumne River` = 2.5000007,
  `Merced River` = -3.5,
  `Stanislaus River` = -3,
  `Tuolumne River` = -0.9,
  `San Joaquin River` = 1.2)

fr_seeds <- fall_run_model(..ocean_entry_success_int = ocean_betas)

base_surv_rear_int <- survival_betas$`2nd calibration adjustment`
base_run <- fall_run_model(seeds = fr_seeds,..surv_juv_rear_int = base_surv_rear_int,  ..ocean_entry_success_int = ocean_betas)
survival_betas
base_run$spawners[1, ]

mod1 <- fall_run_model(seeds = fr_seeds, ..surv_adult_enroute_int = 1.1, ..ocean_entry_success_int = ocean_betas)
mod2 <- fall_run_model(seeds = fr_seeds, ..surv_adult_enroute_int = -1.1, ..ocean_entry_success_int = ocean_betas)
mod1$spawners[1, ]


row_normalize <- function(x) {
 t(apply(x, 1, function(d) {
   ifelse((v = sd(d)) == 0, 0, (d-mean(d))/v)

  }))
}

fit <- function(adult_enroute, juv_rear, ws = 1:31) {
  preds <- fall_run_model(seeds = fr_seeds,
                          ..surv_adult_enroute_int = adult_enroute,
                          ..surv_juv_rear_int = juv_rear,
                          ..ocean_entry_success_int = ocean_betas)

  norm_base <- row_normalize(base_run$spawners[ws, , drop = FALSE])
  norm_preds <- row_normalize(preds$spawners[ws, , drop = FALSE])
  sum((norm_base - norm_preds)^2)
}

fit(1, 1)


a <- ga(type = "real-valued",
        fitness = function(x) -fit(x[1], x[2]),
        lower = c(0, 0), upper = c(4, 4),
        popSize = 10,
        monitor = TRUE,
        maxiter = 1000,
        run = 20,
        parallel = TRUE,
        mutation = gareal_nraMutation)


calib_result <- fall_run_model(seeds = fr_seeds,
                               ..surv_adult_enroute_int = a@solution[1,1],
                               ..surv_juv_rear_int =  a@solution[1, 2],
                               ..ocean_entry_success_int = ocean_betas)

results <- tibble(
  year = 1:20,
  base = base_run$spawners[1, ],
  # mod1 = mod1$spawners[1,],
  # mod2 = mod2$spawners[1,],
  calib = calib_result$spawners[1,]
) %>%
  gather("type", "value", -year)

results %>%
  ggplot(aes(year, value, color = type)) + geom_line()









