# There is no easy way of doing this. The best approach is to swtich branches
# and rebuild in between runs of the model.


# Updated Transitions ----------------------

run_scenario <- function(scenario, seeds, prey_dens) {
  run <- fallRunDSM::fall_run_model(scenario = scenario,
                                    mode = "simulate", seeds = seeds, stochastic = FALSE,
                                    prey_density = prey_dens)
  return(mean(colSums(run$spawners * run$proportion_natural, na.rm = TRUE)))
}

seeds <- fallRunDSM::fall_run_model(mode = "seed", stochastic = FALSE)
scenarios <- list(DSMscenario::scenarios$NO_ACTION, DSMscenario::scenarios$ONE,
                  DSMscenario::scenarios$TWO, DSMscenario::scenarios$THREE,
                  DSMscenario::scenarios$FOUR, DSMscenario::scenarios$FIVE,
                  DSMscenario::scenarios$SIX, DSMscenario::scenarios$SEVEN,
                  DSMscenario::scenarios$EIGHT, DSMscenario::scenarios$NINE,
                  DSMscenario::scenarios$TEN, DSMscenario::scenarios$ELEVEN,
                  DSMscenario::scenarios$TWELVE, DSMscenario::scenarios$THIRTEEN)

# low
low_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "low"))
names(low_dens_growth_model) <- c("no action", as.character(1:13))
rev(sort(low_dens_growth_model))


# med
med_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "med"))
names(med_dens_growth_model) <- c("no action", as.character(1:13))
rev(sort(med_dens_growth_model))

# hi
hi_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "hi"))
names(hi_dens_growth_model) <- c("no action", as.character(1:13))
rev(sort(hi_dens_growth_model))

# max
max_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "max"))
names(max_dens_growth_model) <- c("no action", as.character(1:13))
rev(sort(max_dens_growth_model))
