# 3 modes - seeding, calibration, simulation
# Things that change - length of simulation, the adults, prespawn adult mortality
# seeding, sim len: 5, adults: one yea of data reused, no prespawn mortality, return adults
# calibration: sim len: 20, adults: imputed grandtab, ?prespawn survival, return 20 years of simulated adults
# simulation: len: 20, adults: from seeding, prespawn surv true, return spawners

fr <- function(mode = c("seed", "simulate", "calibrate"), seeds = NULL) {
  mode <- match.arg(mode)
  sim_length <- switch(mode,
                       "seed" = 5,
                       "simulate" = 20,
                       "calibrate" = 20)

  adults <- switch (model,
                    "seed" = fallRunDSM::adult_seeds,
                    "simulate" = seeds,
                    "calibrate" = fallRunDSM::imputed_grandtab
  )


}




foo <- function(x, members = c("CORE", "ALL")) {
  ## evaluate choices
  members <- match.arg(members)
  ## do something
  print(members)
}
