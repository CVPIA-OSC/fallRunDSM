library(tidyverse)
library(DSMscenario)

source("calibration/update_params.R")
res <- read_rds('calibration/solution-res4-08-03.rds')
solution <- res@solution
params_2021 <- update_params(x = solution, fallRunDSM::params)

s <- fall_run_model(mode = 'seed')

baseline <- fall_run_model(scenario = scenarios$NO_ACTION, mode = 'simulate',
                           stochastic = FALSE, ..params = params_2021, seed = s)

s1 <- fall_run_model(scenario = scenarios$ONE, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)
s2 <- fall_run_model(scenario = scenarios$TWO, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)
s3 <- fall_run_model(scenario = scenarios$THREE, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)
s4 <- fall_run_model(scenario = scenarios$FOUR, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)
s5 <- fall_run_model(scenario = scenarios$FIVE, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)
s6 <- fall_run_model(scenario = scenarios$SIX, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)
s7 <- fall_run_model(scenario = scenarios$SEVEN, mode = 'simulate',
                     stochastic = FALSE, ..params = params_2021, seed = s)

baseline_prop_nat <- colSums(baseline$spawners * baseline$proportion_natural, na.rm = TRUE)[20]

s1_prop_nat <- colSums(s1$spawners * s1$proportion_natural, na.rm = TRUE)[20]
s2_prop_nat <- colSums(s2$spawners * s2$proportion_natural, na.rm = TRUE)[20]
s3_prop_nat <- colSums(s3$spawners * s3$proportion_natural, na.rm = TRUE)[20]
s4_prop_nat <- colSums(s4$spawners * s4$proportion_natural, na.rm = TRUE)[20]
s5_prop_nat <- colSums(s5$spawners * s5$proportion_natural, na.rm = TRUE)[20]
s6_prop_nat <- colSums(s6$spawners * s6$proportion_natural, na.rm = TRUE)[20]
s7_prop_nat <- colSums(s7$spawners * s7$proportion_natural, na.rm = TRUE)[20]

x <- c(s1_prop_nat, s2_prop_nat, s3_prop_nat,
       s4_prop_nat, s5_prop_nat, s6_prop_nat,
       s7_prop_nat)

(x - baseline_prop_nat) / baseline_prop_nat

round((x  - max(x)) / max(x), 2)
