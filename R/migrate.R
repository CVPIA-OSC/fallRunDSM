#' @title Juvenile Migration
#' @description Application of survival rate to migrating juvenile fish
#' @param migrants An n by 4 matrix of juveniles (watersheds by size class)
#' @param migration_survival_rate The survival rates for s, m, l, and vl fish
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
migrate <- function(migrants, migration_survival_rate,
                    stochastic) {
  t(sapply(1:nrow(migrants), function(i) {
    if (stochastic) {
      rbinom(n = 4, size = round(migrants[i, ]), prob = migration_survival_rate)
    } else {
      round(migrants[i, ] * migration_survival_rate)
    }
  }))
}
