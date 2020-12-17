#' @title Territory Size Requirement
#' @description Territory requirement as a function of fork length (Grant and Kramer 1990).
#' Default value returns the territory requirement in square meters for each size class.
#' @param fork_lengths The fork length in centimeters. The default value is the
#' mid-point for each size class.
#' @source IP-117068
#' @export
territory_by_size <- function(fork_lengths = c(mean(c(3.5, 4.2)), mean(c(4.2, 7.2)), mean(c(7.2, 11)), 0)) {
  10 ^ (2.61 * log10(fork_lengths) - 2.83)
}
