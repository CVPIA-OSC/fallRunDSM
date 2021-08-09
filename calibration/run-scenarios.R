library(tidyverse)
library(DSMscenario)
library(parallel)
source("calibration/update_params.R")

res <- read_rds('calibration/best-fit-2021-07-28.rds')
solution <- res@solution
params <- update_params(x = solution, fallRunDSM::params)

run_model_base <- function(i) {
  seeds <- fall_run_model(mode = 'seed')
  base <- fall_run_model(scenario = DSMscenario::scenarios$NO_ACTION,
                         mode = "simulate", seeds = seeds, ..params = params)
  return(base)
}

run_model_s1 <- function(i) {
  seeds <- fall_run_model(mode = 'seed')
  s1 <- fall_run_model(scenario = DSMscenario::scenarios$ONE,
                       mode = "simulate", seeds = seeds, ..params = params)
  return(s1)
}

run_model_s7 <- function(i) {
  seeds <- fall_run_model(mode = 'seed')
  s7 <- fall_run_model(scenario = DSMscenario::scenarios$SEVEN,
                       mode = "simulate", seeds = seeds, ..params = params)
  return(s7)
}

library(tictoc)
tic('250 runs')
results_base <- parallel::mclapply(1:250, function(i) {run_model_base(i)}, mc.cores = 7)
toc()


tic('250 runs s1')
results_s1 <- parallel::mclapply(1:250, function(i) {run_model_s1(i)}, mc.cores = 7)
toc()

tic('250 runs s7')
results_s7 <- parallel::mclapply(1:250, function(i) {run_model_s7(i)}, mc.cores = 7)
toc()

library(purrr)
valleywide_nat_spawn_base <- map_dbl(1:250, ~colSums(results_base[[.]]$natural_spawners)[20])
valleywide_nat_spawn_s1 <- map_dbl(1:250, ~colSums(results_s1[[.]]$natural_spawners)[20])
valleywide_nat_spawn_s7 <- map_dbl(1:250, ~colSums(results_s7[[.]]$natural_spawners)[20])

plot(1:250, cummean(valleywide_nat_spawn_base))
plot(1:250, cummean(valleywide_nat_spawn_s1))
plot(1:250, cummean(valleywide_nat_spawn_s7))

(mean(valleywide_nat_spawn_s1) - mean(valleywide_nat_spawn_base)) / mean(valleywide_nat_spawn_base)
(mean(valleywide_nat_spawn_s7) - mean(valleywide_nat_spawn_base)) / mean(valleywide_nat_spawn_base)
