library(tidyverse)
library(fallRunDSM)
library(DSMscenario)

?DSMscenario::get_action_matrices

scenario_df_lms <- tibble(
  watershed = "Lower-mid Sacramento River",
  action = 3,
  start_year = 1980,
  end_year = 1999,
  units_of_effort = 1
)

scenario_df_ls <- tibble(
  watershed = "Lower Sacramento River",
  action = 3,
  start_year = 1980,
  end_year = 1999,
  units_of_effort = 1
)

scenario_df_both <- tibble(
  watershed = c("Lower-mid Sacramento River", "Lower Sacramento River"),
  action = 3,
  start_year = 1980,
  end_year = 1999,
  units_of_effort = 1
)

low_mid_sac_scenario <- DSMscenario::get_action_matrices(scenario_df_lms)
low_sac_scenario <- DSMscenario::get_action_matrices(scenario_df_ls)
both_low_sac_scenario <- DSMscenario::get_action_matrices(scenario_df_both)

s <- fall_run_model()

low_mid_sac_results <- fall_run_model(scenario = low_mid_sac_scenario,
                                      mode = "simulate", seeds = s, stochastic =  FALSE)
low_sac_results <- fall_run_model(scenario = low_sac_scenario,
                                  mode = "simulate", seeds = s, stochastic =  FALSE)
both_low_sac_results <- fall_run_model(scenario = both_low_sac_scenario,
                                       mode = "simulate", seeds = s, stochastic = FALSE)

summary(colSums(low_mid_sac_results$spawners * low_mid_sac_results$proportion_natural))
summary(colSums(low_sac_results$spawners * low_sac_results$proportion_natural))
summary(colSums(both_low_sac_results$spawners * both_low_sac_results$proportion_natural))
