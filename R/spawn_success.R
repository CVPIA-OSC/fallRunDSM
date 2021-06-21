#' @title Spawning Success
#' @description Calculates the annual reproductive success.
#' @param escapement The number of returning adults
#' @param adult_prespawn_survival The adult prespawn surival rate
#' @param egg_to_fry_survival The egg to fry survival rate
#' @param prob_scour The probability of nest scouring
#' @param spawn_habitat The available spawning habitat in square meters
#' @param sex_ratio The female to male spawning ratio, default 0.5
#' @param redd_size The size of redds including defensible space, default value 9.29 square meters
#' @param fecundity The number of eggs per female, default value 5522
#' @source IP-117068
#' @export

spawn_success <- function(escapement, adult_prespawn_survival, egg_to_fry_survival,
                          prob_scour, spawn_habitat,
                          sex_ratio = 0.5,
                          redd_size = 9.29,
                          fecundity = 5522){

  capacity <- spawn_habitat / redd_size
  spawner_potential <- escapement * adult_prespawn_survival * sex_ratio

  spawners <- ifelse(spawner_potential > capacity, capacity, spawner_potential)
  fry <- spawners * (1 - prob_scour) * fecundity * egg_to_fry_survival

  zeros <- matrix(0, nrow = length(escapement), ncol = 3)
  cbind(fry, zeros)

}
