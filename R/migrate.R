#' @title Juvenile Migration
#' @description Stocastic application of survival rate to migrating juvenile fish
#' @param migrants An n by 4 matrix of juveniles (watersheds by size class)
#' @param migration_survival_rate The survival rates for s, m, l, and vl fish
#' @source IP-117068
#' @export
migrate <- function(migrants, migration_survival_rate) {
  t(sapply(1:nrow(migrants), function(i) {
      rbinom(n = 4, size = round(migrants[i, ]), prob = migration_survival_rate)
    }))
}
