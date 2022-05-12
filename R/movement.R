#' @title Fry fish movement out of natal tributaries
#' @description Sends fry Chinook salmon out of natal tributaries when they are at the fry stage
#' The \code{snow_globe_movement} function sends fish out of natal tributaries
#'            when the sum of flows at freeport and vernalis exceed a threshold
#' The \code{genetic_movement} function sends fish out of natal tributaries
#'           based on a user specified proportion
#' @param juveniles An n by 4 matrix of juvenile fish size s, m, l, vl
#' @param freeport_flow The flow at freeport in cubic meters per second
#' @param vernalis_flow The flow at vernalis in cubic meters per second
#' @param threshold the threshold combined flow for initiating fish movement out of natal tributaries
#' @param p_leave the proportion of juvenile fish leaving in response to high flows
#' @param stochastic when true the fish leaving are assigned randomly
#'
#' @rdname movement
#' @export
snow_globe_movement<-function(juveniles, freeport_flow, vernalis_flow,
                              threshold = 1000, p_leave = 0.3, stochastic = FALSE){

  number_of_regions <- max(nrow(juveniles), 1)
  migrants <- matrix(0, ncol=4, nrow=number_of_regions)
  river_rear<-juveniles
  if((freeport_flow + vernalis_flow) >= threshold){
    if(stochastic){
      migrants[,1] <- rbinom(n = number_of_regions, juveniles[,1], p_leave)
    } else {
      migrants[,1] <- round(juveniles[,1]*p_leave)
    }
    river_rear[,1] <- juveniles[,1] - migrants[,1]
    river_rear <- pmax(river_rear, 0)
  }
  list(river_rear = river_rear, migrants = migrants)
}

#' @rdname movement
#' @export
genetic_movement <- function(juveniles, p_leave = 0.25, stochastic = FALSE){

  number_of_regions <- max(nrow(juveniles), 1)
  migrants <- matrix(0, ncol = 4, nrow = number_of_regions)
  river_rear <- juveniles
    if(stochastic){
      migrants[,1]<- rbinom(n = number_of_regions,juveniles[,1], p_leave)
    } else {
      migrants[,1]<-round(juveniles[,1]*p_leave)
    }
    river_rear[,1]<-juveniles[,1]- migrants[,1]
    river_rear <- pmax(river_rear, 0)

  list(river_rear = river_rear, migrants = migrants)
}
