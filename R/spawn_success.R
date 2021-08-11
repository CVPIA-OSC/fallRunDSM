#' @title Spawning Success
#' @description Calculates the annual reproductive success.
#' @param escapement The number of returning adults
#' @param adult_prespawn_survival The adult prespawn surival rate
#' @param egg_to_fry_survival The egg to fry survival rate
#' @param prob_scour The probability of nest scouring
#' @param spawn_habitat The available spawning habitat in square meters
#' @param sex_ratio The female to male spawning ratio
#' @param redd_size The size of redds including defensible space
#' @param fecundity The number of eggs per female
#' @source IP-117068
#' @export

spawn_success <- function(escapement, adult_prespawn_survival, egg_to_fry_survival,
                          prob_scour, spawn_habitat,
                          sex_ratio = fallRunDSM::params$spawn_success_sex_ratio,
                          redd_size = fallRunDSM::params$spawn_success_redd_size,
                          fecundity = fallRunDSM::params$spawn_success_fecundity,
                          stochastic){

  capacity <- spawn_habitat / redd_size

  spawner_potential <- if(stochastic) {
    rbinom(31, round(escapement), (adult_prespawn_survival * sex_ratio))
  } else {
    escapement * adult_prespawn_survival * sex_ratio
  }

  spawners <- pmin(spawner_potential, capacity)

  fry <- spawners * (1 - prob_scour) * fecundity * egg_to_fry_survival

  fry <-
    if (stochastic) {
      pmax(round(rnorm(31, fry, (sqrt(fry) / 2))), 0)
    } else {
      fry
    }

  zeros <- matrix(0, nrow = length(escapement), ncol = 3)
  cbind(fry, zeros)

}
