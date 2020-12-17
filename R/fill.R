#' @title Filling Natal Tributary Habitat
#' @description Allocates juvenile fish onto the floodplain, in-channel, and once
#' available habitat is full in the natal stream, assigns fish to out migrate
#' @param juveniles An n by 4 matrix of juvenile fish size s, m, l, vl
#' @param inchannel_habitat The available tributary habitat in square meters
#' @param floodplain_habitat The available floodplain habitat in square meters
#' @param territory_size A length 4 array of juvenile fish territory requirements s, m, l, vl
#' @param up_to_size_class Which size class and under is moved to floodplain and
#' inchannel habitat (small = 1, medium = 2, large = 3, very large = 4)
#' @source IP-117068
#' @export
fill_natal <- function(juveniles, inchannel_habitat, floodplain_habitat,
                       territory_size = c(0.0498944803729701, 0.138941944739835, 0.471083652829798, 0),
                       up_to_size_class = 2){

  number_of_regions <- max(nrow(juveniles), 1)

  migrants <- flood_rear <- river_rear <- matrix(0, ncol = 4, nrow = number_of_regions)

  # Assign individuals to flood habitat, largest first
  if(!is.null(floodplain_habitat)){
    for(r in 1:number_of_regions){
      for(i in up_to_size_class:1){
        flood_rear[r, i] <- min(round(floodplain_habitat[r] / territory_size[i]), juveniles[r, i])
        floodplain_habitat[r] <- max(floodplain_habitat[r] - flood_rear[r, i] * territory_size[i], 0)
      }}
  }

  flood_rear <- pmax(flood_rear, 0)
  juveniles <- pmax(juveniles - flood_rear, 0)
  for(r in 1:number_of_regions){
    for(i in 2:1){
      river_rear[r, i] <- min(round(inchannel_habitat[r] / territory_size[i]), juveniles[r ,i])
      inchannel_habitat[r] <- max(inchannel_habitat[r] - river_rear[r, i] * territory_size[i], 0)
    }}

  river_rear <- pmax(river_rear, 0)

  migrants <- pmax(juveniles - river_rear, 0)
  list(inchannel = river_rear, floodplain = flood_rear, migrants = migrants)
}

#' @title Filling Regional Habitat
#' @description Allocates juvenile fish onto the floodplain, in-channel, and once
#' available habitat is full, assigns fish to out migrate. Function is used to route fish
#' on the mainstem, bypasses, and deltas.
#' @param juveniles An n by 4 matrix of juvenile fish size s, m, l, vl
#' @param habitat The available habitat in square meters (in-channel for tributaries)
#' @param floodplain_habitat The available tributary floodplain habitat in square meters (NULL for bypasses and delta)
#' @param territory_size A length 4 array of juvenile fish territory requirements s, m, l, vl
#' @param up_to_size_class Which size class and under is moved to floodplain and
#' inchannel habitat (small = 1, medium = 2, large = 3, very large = 4)
#' @source IP-117068
#' @export
fill_regional <- function(juveniles, habitat, floodplain_habitat = NULL,
                          territory_size = c(0.0498944803729701, 0.138941944739835, 0.471083652829798, 0),
                          up_to_size_class = 3){

  all_sheds <- orig_tot <- colSums(juveniles)

  migrants <- flood_rear <- river_rear <- matrix(0, ncol = 4, nrow = 1)

  # Assign individuals to flood habitat largest first
  if(!is.null(floodplain_habitat)){
    for(i in up_to_size_class:1){
      flood_rear[i] <- min(round(floodplain_habitat / territory_size[i]), all_sheds[i])
      floodplain_habitat <- max(floodplain_habitat - flood_rear[i] * territory_size[i], 0)
    }
  }

  all_sheds <- all_sheds - flood_rear
  for(i in up_to_size_class:1){
    river_rear[i] <- min(round(habitat / territory_size[i]), all_sheds[i])
    habitat <- max(habitat - river_rear[i] * territory_size[i], 0)
  }

  migrants <- pmax(all_sheds - river_rear, 0)

  # remove negative and NaN values
  prop_flood <- mapply(flood_rear / orig_tot, FUN = max, 0, na.rm = TRUE)
  prop_river <- mapply(river_rear / orig_tot, FUN = max, 0, na.rm = TRUE)
  prop_migrant <- mapply(migrants / orig_tot, FUN = max, 0, na.rm = TRUE)

  # apportioning tributary fish to the region
  flood_rear <- round(t(t(juveniles) * prop_flood))
  river_rear <- round(t(t(juveniles) * prop_river))
  migrants <- round(t(t(juveniles) * prop_migrant))

  if (is.null(floodplain_habitat)) {
    list(inchannel = river_rear, migrants = migrants)
  } else {
    list(inchannel = river_rear, floodplain = flood_rear, migrants = migrants)
  }
}
