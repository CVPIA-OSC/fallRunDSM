#' @title Method of Moments Gamma Parameter Estimation
#' @description Estimate parameters for gamma distribution using the method of moments
#' @param mu mean
#' @param sigma standard deviation
gamma_MOM <- function(mu, sigma){
  alpha <-(mu / sigma)^2
  beta <- (sigma^2) / mu

  c(alpha = alpha, beta = beta)
}

#' @title Growth Transistion Probability
#' @description Generates transition probability matrices for growth inchannel
#' @param daily_rates The daily growth rate for inchannel (default value is 0.5 mm/day)
#' @param size_class_breaks The fork size class breaks
#' @source IP-117068
#' @export
growth <- function(daily_growth_rate = .5, size_class_breaks = c(35, 42, 72, 110)){

  transition_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(c('s', 'm', 'l', 'vl'), c('s', 'm', 'l', 'vl')))
  transition_matrix[4, 4] <- 1

  monthly_growth_rate <- daily_growth_rate * 30

  for(i in 2:4){
    mu <- (monthly_growth_rate + mean(c(size_class_breaks[i], size_class_breaks[i - 1])))
    sigma <- monthly_growth_rate * 0.3
    parameters <- gamma_MOM(mu, sigma)
    alpha <- parameters['alpha']
    beta <- parameters['beta']

    transition_matrix[(i-1), 1] <- pgamma(size_class_breaks[2], shape = alpha, scale = beta)
    transition_matrix[(i-1), 2] <- pgamma(size_class_breaks[3], shape = alpha, scale = beta) - transition_matrix[(i - 1), 1]
    transition_matrix[(i-1), 3] <- pgamma(size_class_breaks[4], shape = alpha, scale = beta) - sum(transition_matrix[(i - 1), 1:2])
    transition_matrix[(i-1), 4] <- 1- pgamma(size_class_breaks[4], shape = alpha, scale = beta)
  }

  # Eliminate nosense transitions and normalize just in case
  transition_matrix[2, 1] <- transition_matrix[3, 1] <- transition_matrix[3, 2] <- 0
  transition_matrix <- transition_matrix / rowSums(transition_matrix)

  transition_matrix
}

#' @title Floodplain Growth Transistion Probability
#' @description Generates transition probability matrices for growth on the
#' floodplain dependent on number of weeks inundated
#' @param daily_rates The daily growth rate for inchannel (0.5 mm/day) and floodplain (1.06 mm/day)
#' @param weeks_flooded The number of weeks inundated, 1-4 weeks
#' @source IP-117068
#' @export
growth_floodplain <- function(daily_rates = c(0.5, 1.06), weeks_flooded = 1:4){

  inchannel <- ifelse(weeks_flooded > 0, (4 - weeks_flooded) / 4, 1)
  floodplain <- 1 - inchannel

  growth_inchannel <- growth(daily_rates[1])
  growth_floodplain <- growth(daily_rates[2])

  transition_matrices <- array(0, dim = c(4, 4, 4),
                               dimnames = list(c('s', 'm', 'l', 'vl'),
                                               c('s', 'm', 'l', 'vl'),
                                               c('1 week flooded', paste(2:4, 'weeks flooded'))))
  transition_matrices[4, 4, ] <- 1

  for(i in 1:4){
    transition_matrices[ , , i] <- growth_inchannel * inchannel[i] + growth_floodplain * floodplain[i]

    # Eliminate nosense transitions and normalize floodplain values
    transition_matrices[2, 1, i] <- transition_matrices[3, 1, i] <- transition_matrices[3, 2, i] <- 0
    transition_matrices[ , , i] <- transition_matrices[ , , i] / rowSums(transition_matrices[ , , i])
  }

  transition_matrices
}
