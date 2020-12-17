#' @title Get Spawning Adults
#' @param year the year of simulation
#' @param adults potential spawning adults for each watershed (length = 31) values must be integer
#' @source IP-117068
#' @export
get_spawning_adults <- function(year, adults, hatch_adults) {

  returning_hatchery_adults <- hatch_adults

  # well they all return the same surival
  stray_prop <- adult_stray(wild = 1,
                            natal_flow = prop_flow_natal[ , year],
                            south_delta_watershed = south_delta_routed_watersheds,
                            cross_channel_gates_closed = cc_gates_days_closed[10])

  straying_adults <- rbinom(n = 31, adults, stray_prop[1])

  #TODO random variable
  south_delta_routed_adults <- round(sum(straying_adults * south_delta_routed_watersheds))
  south_delta_stary_adults <- as.vector(rmultinom(1, south_delta_routed_adults, cross_channel_stray_rate))
  remaining_stray_adults <- round(sum(straying_adults * (1 - south_delta_routed_watersheds)))
  stray_adults <- as.vector(rmultinom(1, remaining_stray_adults, stray_rate))

  adults_after_stray <- adults - straying_adults + south_delta_stary_adults + stray_adults

  # are tisdale or yolo bypasses overtopped?
  # for all years and months 10-12 there is always at least one true
  bypass_is_overtopped <- as.logical(tisdale_bypass_watershed + yolo_bypass_watershed)

  # adult en route survival
  avg_migratory_temp <- rowMeans(migratory_temperature_proportion_over_20[ , 10:12])
  adult_en_route_surv <- surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                            bypass_overtopped = bypass_is_overtopped,
                                            adult_harvest = adult_harvest_rate)

  #TODO random variable
  adults_survived_en_route <- rbinom(n = 31, size = round(adults_after_stray), prob = adult_en_route_surv[1])
  # remove adults for hatcheries
  surviving_natural_adults <- rbinom(n = 31, size = round(adults_survived_en_route), prob = 1 - natural_adult_removal_rate)
  hatchery_adults <- rbinom(n = 31, size = round(returning_hatchery_adults), prob = adult_en_route_surv[[1]])


  init_adults <- surviving_natural_adults + hatchery_adults
  proportion_natural <- surviving_natural_adults / init_adults

  list(init_adults = init_adults,
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), NA_real_),
       natural_adults = surviving_natural_adults)

}












