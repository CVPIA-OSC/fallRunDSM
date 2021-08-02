library(fallRunDSM)
library(profvis)
library(tictoc)
source("data-raw/variable_name_changes.R")

s <- fall_run_model()

profvis(new_model(seeds = adult_seed_10))

tic("testing model speed")
out <- new_model(seeds = adult_seed_10)
toc()
