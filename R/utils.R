#' @title Days in Month
#' @description Number of days withing a month
#' @param month Integer representation of month
days_in_month <- function(month, days_by_month = c(Jan = 31L, Feb = 28L, Mar = 31L,
                                       Apr = 30L, May = 31L, Jun = 30L,
                                       Jul = 31L, Aug = 31L, Sep = 30L,
                                       Oct = 31L, Nov = 30L, Dec = 31L)) {
  days <- days_by_month[month]
  names(days) <- NULL
  return(days)
}

#' @title Ocean Transition Month
#' @description Calculates the month juveniles transition to the ocean
#' @source IP-117068
ocean_transition_month <- function(stochastic) {
  if (stochastic) {
    v <- (runif(1) < 0.36) * 1
    T.day <- v * round(rgamma(1, shape = 3.212851, scale= 6.225000)) + (1 - v) * round(rgamma(1, shape = 19.349501, scale = 3.549833))
    round(T.day / 30)
  } else {
    return(2)
  }

}

#' @title Pretty Number
#' @description Formats numbers for ease of reading
pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}

append_new_chipps_juvs <- function(chipps_data_frame, chipps_matrix, year, month) {
  new_chipps_data_frame <- as.data.frame(chipps_matrix)
  new_chipps_data_frame$year <- year
  new_chipps_data_frame$month <- month
  new_chipps_data_frame$watershed <- watershed_labels
  rownames(new_chipps_data_frame) <- NULL

  rbind.data.frame(chipps_data_frame, new_chipps_data_frame)
}







