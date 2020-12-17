#' @title Rearing Function
#' @description Calculates the number of juveniles that survive and grow inchannel and on the floodplain
#' @param juveniles number of juveniles before growth or survival rates are applied
#'  (inchannel for tributaries or total for bypasses and delta)
#' @param survival_rate survival rate by size class (inchannel for tributaries or total for bypasses and delta)
#' @param growth growth transition matrix for juveniles (inchannel for tributaries or total for bypasses and delta)
#' @param floodplain_juveniles number of juveniles on the floodplain before growth or survival rates are applied (NULL for bypasses and delta)
#' @param floodplain_survival_rate floodplain survival rate by size class (NULL for bypasses and delta)
#' @param floodplain_growth growth transition matrix for juveniles rearing on the floodplain (NULL for bypasses and delta)
#' @param weeks_flood number of weeks floodplain is inundated (0 through 4)
#' @source IP-117068
#' @export
rear <- function(juveniles, survival_rate, growth, floodplain_juveniles = NULL,
                 floodplain_survival_rate = NULL, floodplain_growth = NULL,
                 weeks_flooded = NULL){

  next_juveniles <- (juveniles * survival_rate) %*% growth

  if(!is.null(floodplain_juveniles)) {
    floodplain_juveniles_survived <- floodplain_juveniles * floodplain_survival_rate

    next_floodplain_juveniles <- c()

    for(i in 1:nrow(floodplain_juveniles)) {
      if (weeks_flooded[i] > 0) {
        watershed_floodplain_juveniles <- floodplain_juveniles_survived[i, ] %*% floodplain_growth[ , , weeks_flooded[i]]
        next_floodplain_juveniles <- rbind(next_floodplain_juveniles, watershed_floodplain_juveniles)
      } else {
        next_floodplain_juveniles <- rbind(next_floodplain_juveniles, rep(0, 4))
      }
    }
    return(list(inchannel = next_juveniles, floodplain = next_floodplain_juveniles))
  }

  return(next_juveniles)
}

