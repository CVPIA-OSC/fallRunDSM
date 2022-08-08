#' @title Rearing Function
#' @description Calculates the number of juveniles that survive and grow inchannel and on the floodplain
#' @param juveniles Number of juveniles before growth or survival rates are applied
#'  (inchannel for tributaries or total for bypasses and delta)
#' @param survival_rate Survival rate by size class (inchannel for tributaries or total for bypasses and delta)
#' @param growth Growth transition matrix for juveniles (inchannel for tributaries or total for bypasses and delta)
#' @param floodplain_juveniles Number of juveniles on the floodplain before growth or survival rates are applied (NULL for bypasses and delta)
#' @param floodplain_survival_rate Floodplain survival rate by size class (NULL for bypasses and delta)
#' @param floodplain_growth Growth transition matrix for juveniles rearing on the floodplain (NULL for bypasses and delta)
#' @param weeks_flooded Number of weeks floodplain is inundated (0 through 4)
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
rear <- function(juveniles, survival_rate, growth, floodplain_juveniles = NULL,
                 floodplain_survival_rate = NULL, floodplain_growth = NULL,
                 weeks_flooded = NULL, stochastic){
  survived <-
    if (is.vector(survival_rate)) {
      t(sapply(1:nrow(juveniles), function(watershed) {
        if (stochastic) {
          rbinom(4, size = round(juveniles[watershed, ]), prob = survival_rate)
        } else {
          round(juveniles[watershed, ] * survival_rate)
        }
      }))
    } else {
      t(sapply(1:nrow(juveniles), function(watershed) {
        if (stochastic) {
          rbinom(4, size = round(juveniles[watershed, ]), prob = survival_rate[watershed, ])
        } else {
          round(juveniles[watershed, ] * survival_rate[watershed, ])
        }
      }))
    }

  # TODO fix the regional case when there are mnay watersheds for juvs
  # but not the same corresponding
  if (length(growth) == 16) {
    next_juveniles <- survived %*% growth
  } else {
    next_juveniles <- round(t(sapply(1:nrow(juveniles), function(i) {
      survived[i,, drop = FALSE] %*% growth[,,i]
    })))
  }

  if(!is.null(floodplain_juveniles)) {
    floodplain_juveniles_survived <- if (is.vector(floodplain_survival_rate)) {
      t(sapply(1:nrow(floodplain_juveniles), function(watershed) {
        if (stochastic) {
          rbinom(4, size = round(floodplain_juveniles[watershed, ]), prob = floodplain_survival_rate)
        } else {
          round(floodplain_juveniles[watershed, ] * floodplain_survival_rate)
        }
      }))
    } else {
      t(sapply(1:nrow(floodplain_juveniles), function(watershed) {
        if (stochastic) {
          rbinom(4, size = round(floodplain_juveniles[watershed, ]), prob = floodplain_survival_rate[watershed, ])
        } else {
          round(floodplain_juveniles[watershed, ] * floodplain_survival_rate[watershed, ])
        }
      }))
    }
    next_floodplain_juveniles <- c()
    for(i in 1:nrow(floodplain_juveniles)) {
      if (weeks_flooded[i] > 0) {
        if (length(floodplain_growth) == 16) { # case when we are doing a "regional" rear
          watershed_floodplain_juveniles <- floodplain_juveniles_survived[i, ] %*% floodplain_growth
        } else {
          watershed_floodplain_juveniles <- floodplain_juveniles_survived[i, ] %*% floodplain_growth[ , , i]
        }
        next_floodplain_juveniles <- rbind(next_floodplain_juveniles, watershed_floodplain_juveniles)
      } else {
        next_floodplain_juveniles <- rbind(next_floodplain_juveniles, rep(0, 4))
      }
    }
    return(list(inchannel = next_juveniles, floodplain = round(next_floodplain_juveniles)))
  }
  return(next_juveniles)
}
