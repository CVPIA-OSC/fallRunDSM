#' @title Get habitat Values
#' @source IP-117068
#' @description Returns the appropriate habitat for spawning and rearing based on habitat inputs and a given simulation year and month
#' @details See \code{\link{params}} for details on parameter sources
#' @param year The current simulation year, 1-20
#' @param month The current simulation month, 1-8
#' @param inchannel_habitat_fry 3 dimensional array [watersheds, months, years] representing fry inchannel habitat in square meters
#' @param inchannel_habitat_juvenile 3 dimensional array [watersheds, months, years] representing juvenile inchannel habitat in square meters
#' @param floodplain_habitat 3 dimensional array [watersheds, months, years] representing floodplain habitat in square meters
#' @param sutter_habitat 2 dimensional array [months, years] representing sutter bypass habitat in square meters
#' @param yolo_habitat 2 dimensional array [months, years] representing yolo bypass habitat in square meters
#' @param delta_habitat 3 dimensional array [months, years, delta] representing north delta habitat [,, 1] and south delta habitat [,, 2] in square meters
#' @export
get_habitat <- function(year, month,
                        inchannel_habitat_fry,
                        inchannel_habitat_juvenile,
                        floodplain_habitat,
                        sutter_habitat,
                        yolo_habitat,
                        delta_habitat) {
  # set monthly habitat values
  ic_habitat <- if (month < 4) inchannel_habitat_fry[ , month, year] else inchannel_habitat_juvenile[ , month, year]
  floodplain_activation <- matrix(0, nrow = 31, ncol = 12)
  fp_habitat <- floodplain_habitat[ , month, year] + floodplain_activation[ , month]
  habitat_sutter <- sutter_habitat[month, year]
  habitat_yolo <- yolo_habitat[month, year]
  north_delta_habitat <- delta_habitat[month, year, 1]
  south_delta_habitat <- delta_habitat[month, year, 2]

  return(list(
    inchannel = ic_habitat,
    floodplain = fp_habitat,
    sutter = habitat_sutter,
    yolo = habitat_yolo,
    north_delta = north_delta_habitat,
    south_delta = south_delta_habitat
  ))
}

