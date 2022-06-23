#' @title Fry fish movement out of natal tributaries and mainstem segments
#' @description Sends fry Chinook salmon out of natal tributaries when they are at the fry stage
#' The \code{snow_globe_movement} function sends fish out of natal tributaries
#'            when the sum of flows at freeport and vernalis exceed a threshold
#' The \code{genetic_movement} function sends fish out of natal tributaries
#'           based on a user specified proportion
#' The \code{temperature_movement} function sends fish out of natal tributaries
#'           and mainstem Sacramento segments based on month and mean monthly temperature
#' The \code{time_movement} function sends fish out of natal tributaries
#'           and mainstem Sacramento segments based on month only
#' @param juveniles An n by 4 matrix of juvenile fish size s, m, l, vl
#' @param freeport_flow The flow at freeport in cubic meters per second
#' @param vernalis_flow The flow at vernalis in cubic meters per second
#' @param threshold the threshold combined flow for initiating fish movement out of natal tributaries
#' @param P.leave the proportion of juvenile fish leaving in response to high flows
#' @param MeanT mean monthly temperature in degrees C
#' @param Month the month of the year
#' @param stochastic when true the fish leaving are assigned randomly
#' 

snow_globe_movement<-function(juveniles,freeport_flow=DSMflow::freeport_flow[mnth,year],
                              vernalis_flow=DSMflow::vernalis_flow[mnth,year],
                              threshold = 1000, P.leave=0.3, stochastic){
  number_of_regions <- max(nrow(juveniles), 1)
  migrants <- matrix(0, ncol=4, nrow=number_of_regions)
  river_rear<-juveniles
  if((freeport_flow+vernalis_flow) >= threshold){
    if(stochastic){
      migrants[,1]<- rbinom(n=number_of_regions,juveniles[,1],P.leave)
    } else {
      migrants[,1]<-round(juveniles[,1]*P.leave)
    }
    river_rear[,1]<-juveniles[,1]- migrants[,1]
    river_rear <- pmax(river_rear, 0)
  } 
  list(river_rear = river_rear, migrants = migrants)
}

genetic_movement<-function(juveniles, P.leave=0.25, stochastic){
 
  number_of_regions <- max(nrow(juveniles), 1)
  migrants <- matrix(0, ncol = 4, nrow = number_of_regions)
  river_rear<-juveniles
    if(stochastic){
      migrants[,1]<- rbinom(n=number_of_regions,juveniles[,1],P.leave)
    } else {
      migrants[,1]<-round(juveniles[,1]*P.leave)
    }
    river_rear[,1]<-juveniles[,1]- migrants[,1]
    river_rear <- pmax(river_rear, 0)

  list(river_rear = river_rear, migrants = migrants)
}

temperature_movement<-function(juveniles, Month=3, MeanT=15, stochastic){
   P.leave= 1/(1+exp((-22.158 + 0.626687*MeanT + 3.73633*Month - 0.058834*MeanT*Month)))
  number_of_regions <- max(nrow(juveniles), 1)
  migrants <- matrix(0, ncol = 4, nrow = number_of_regions)
  river_rear<-juveniles
  if(stochastic){
    migrants[,1]<- rbinom(n=number_of_regions,juveniles[,1],P.leave)
  } else {
    migrants[,1]<-round(juveniles[,1]*P.leave)
  }
  river_rear[,1]<-juveniles[,1]- migrants[,1]
  river_rear <- pmax(river_rear, 0)
  
  list(river_rear = river_rear, migrants = migrants)
}

time_movement<-function(juveniles, Month=3, stochastic){
  P.leave= 1/(1+exp((-15.56021 + 3.55853*Month)))
  number_of_regions <- max(nrow(juveniles), 1)
  migrants <- matrix(0, ncol = 4, nrow = number_of_regions)
  river_rear<-juveniles
  if(stochastic){
    migrants[,1]<- rbinom(n=number_of_regions,juveniles[,1],P.leave)
  } else {
    migrants[,1]<-round(juveniles[,1]*P.leave)
  }
  river_rear[,1]<-juveniles[,1]- migrants[,1]
  river_rear <- pmax(river_rear, 0)
  
  list(river_rear = river_rear, migrants = migrants)
}

