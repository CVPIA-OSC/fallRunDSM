library(tictoc)
library(winterRunDSM)
library(parallel)
list2env(load_baseline_data(), envir = .GlobalEnv)

number_of_cores <- detectCores()

run_model <- function() {
  seeded_adults <- winter_run_model()
  output <- winter_run_model(seeds = seeded_adults)
  return(output)
}

tic('one model run')
r <- run_model()
toc()

tic('testing parallel run model')
results <- parallel::mclapply(1:20, run_model, mc.cores = number_of_cores)
toc()
