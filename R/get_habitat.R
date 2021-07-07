#' @title Get habitat Values
#' @source IP-117068
#' @param year TODO
#' @param month TODO
#' @param inchannel_habitat_fry TODO
#' @param inchannel_habitat_juvenile TODO
#' @param floodplain_habitat TODO
#' @param sutter_habitat TODO
#' @param yolo_habitat TODO
#' @param north_delta_habitat TODO
#' @param south_delta_habitat TODO
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

