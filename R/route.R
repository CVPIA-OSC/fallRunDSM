#' @title Route Natal Streams
#' @description Determines if juveniles stay in their natal tributary, are detoured
#' to a bypass, or out migrate during a simulated month
#' @details See \code{\link{params}} for details on parameter sources
#' @param year The current simulation year, 1-20
#' @param month The current simulation month, 1-8
#' @param juveniles An n by 4 matrix of juvenile fish by watershed and size class
#' @param inchannel_habitat A vector of available habitat in square meters
#' @param floodplain_habitat A vector of available floodplain habitat in square meters
#' @param prop_pulse_flows The proportion of pulse flows
#' @param .pulse_movement_intercept Intercept for \code{\link{pulse_movement}}
#' @param .pulse_movement_proportion_pulse Coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable
#' @param .pulse_movement_medium Size related intercept for \code{\link{pulse_movement}} medium sized fish
#' @param .pulse_movement_large Size related intercept for \code{\link{pulse_movement}} large sized fish
#' @param .pulse_movement_vlarge Size related intercept for \code{\link{pulse_movement}} very large sized fish
#' @param .pulse_movement_medium_pulse Additional coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable for medium size fish
#' @param .pulse_movement_large_pulse Additional coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable for large size fish
#' @param .pulse_movement_very_large_pulse Additional coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable for very large size fish
#' @param territory_size Array of juvenile fish territory requirements for \code{\link{fill_natal}}
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
route <- function(year, month, juveniles, inchannel_habitat, floodplain_habitat,
                  prop_pulse_flows,
                  .pulse_movement_intercept,
                  .pulse_movement_proportion_pulse,
                  .pulse_movement_medium,
                  .pulse_movement_large,
                  .pulse_movement_vlarge,
                  .pulse_movement_medium_pulse,
                  .pulse_movement_large_pulse,
                  .pulse_movement_very_large_pulse,
                  territory_size,
                  stochastic) {

  natal_watersheds <- fill_natal(juveniles = juveniles,
                                 inchannel_habitat = inchannel_habitat,
                                 floodplain_habitat = floodplain_habitat,
                                 territory_size = territory_size)

  # estimate probability leaving as function of pulse flow
  prob_pulse_leave <- pulse_movement(prop_pulse_flows[ , month],
                                     .intercept = .pulse_movement_intercept,
                                     .proportion_pulse = .pulse_movement_proportion_pulse,
                                     .medium = .pulse_movement_medium,
                                     .large = .pulse_movement_large,
                                     .vlarge = .pulse_movement_vlarge,
                                     .medium_pulse = .pulse_movement_medium_pulse,
                                     .large_pulse = .pulse_movement_large_pulse,
                                     .very_large_pulse = .pulse_movement_very_large_pulse)

  # total fish that will migrate because of pulse flows, this derived using total in river
  # and a binomial selection based on pr of movement due to pulse flows

  pulse_migrants <- if (stochastic) {
    t(sapply(1:nrow(juveniles), function(i) {
      rbinom(n = 4, size = round(natal_watersheds$inchannel[i, ]), prob = prob_pulse_leave[i, ])
    }))
  } else {
    round(natal_watersheds$inchannel * prob_pulse_leave)
  }


  # update in river fish based on the pulse flow results
  natal_watersheds$inchannel <- (natal_watersheds$inchannel - pulse_migrants)

  # update migratory fish based on the pulse flow results
  natal_watersheds$migrants <- natal_watersheds$migrants + pulse_migrants

  return(natal_watersheds)
}

#' @title Route Bypass
#' @description Determines if juveniles remain in the bypass or out migrate
#' @param bypass_fish An n by 4 matrix of juvenile fish by watershed and size class
#' @param bypass_habitat A vector of available habitat in square meters
#' @param migration_survival_rate The outmigration survival rate
#' @param territory_size Array of juvenile fish territory requirements for \code{\link{fill_regional}}
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
route_bypass <- function(bypass_fish, bypass_habitat, migration_survival_rate,
                         territory_size, stochastic) {

  bypass_fish <- fill_regional(juveniles = bypass_fish,
                               habitat = bypass_habitat,
                               territory_size = territory_size)

  bypass_fish$migrants <- t(
    sapply(1:nrow(bypass_fish$migrants), function(i) {
      if (stochastic) {
      rbinom(n = 4, size = bypass_fish$migrants[i, ], prob = migration_survival_rate)
      } else {
        round(bypass_fish$migrants[i, ] * migration_survival_rate)
      }
    }))

  colnames(bypass_fish$migrants) <- c('s', 'm', 'l', 'vl')

  return(bypass_fish)
}

#' @title Route Regions
#' @description Determines if juveniles stay in the region (Sections of Mainstem
#' Sacramento River or San Joaquin River) or out migrate during a simulated month
#' @param month The current simulation month, 1-8
#' @param year The current simulation year, 1-20
#' @param migrants An n by 4 matrix of juvenile fish by watershed and size class
#' @param inchannel_habitat A vector of available habitat in square meters
#' @param floodplain_habitat A vector of available floodplain habitat in square meters
#' @param prop_pulse_flows The proportion of pulse flows
#' @param migration_survival_rate The outmigration survival rate
#' @param proportion_flow_bypass Variable describing the proportion of flows routed through the bypasses, more details at \code{\link[DSMflow]{proportion_flow_bypasses}}
#' @param detour Values can be 'sutter' or 'yolo' if some juveniles are detoured on to that bypass, otherwise NULL
#' @param territory_size Array of juvenile fish territory requirements for \code{\link{fill_regional}}
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
route_regional <- function(month, year, migrants,
                           inchannel_habitat, floodplain_habitat,
                           prop_pulse_flows, migration_survival_rate,
                           proportion_flow_bypass, detour = NULL,
                           territory_size,
                           stochastic) {

  if (!is.null(detour)) {
    bypass <- ifelse(detour == 'sutter', "Sutter Bypass", "Yolo Bypass")

    detoured_fish <-
      if (stochastic) {
        t(sapply(1:nrow(migrants), function(i) {
          rbinom(n = 4,
                 size = round(migrants[i, ]),
                 prob = proportion_flow_bypass[month, year, bypass])
        }))
      } else {
        round(migrants * proportion_flow_bypass[month, year, bypass])
      }

    migrants <- pmax(migrants - detoured_fish, 0)
  }

  # fill up upper mainstem, but in river fish can leave due to pulses
  regional_fish <- fill_regional(juveniles = migrants,
                                 habitat = inchannel_habitat,
                                 floodplain_habitat = floodplain_habitat,
                                 territory_size = territory_size)
  # estimate probability leaving as function of pulse flow
  pulse_flows <- prop_pulse_flows[ , month]
  prob_pulse_leave <- matrix(pulse_movement(pulse_flows), ncol = 4, byrow = T)

  pulse_migrants <- t(sapply(1:nrow(regional_fish$inchannel), function(i) {
    if (stochastic) {
      rbinom(n = 4, size = regional_fish$inchannel[i, ], prob = prob_pulse_leave)
    } else {
      round(regional_fish$inchannel[i, ] * prob_pulse_leave)
    }
  }))

  # remove and add migrants
  regional_fish$inchannel <- regional_fish$inchannel - pulse_migrants
  regional_fish$migrants <- regional_fish$migrants + pulse_migrants

  # apply survival rate to migrants
  regional_fish$migrants <- t(
    sapply(1:nrow(regional_fish$migrants), function(i) {
      if (stochastic) {
        rbinom(n = 4, size = regional_fish$migrants[i, ], prob = migration_survival_rate)
      } else {
        round(regional_fish$migrants[i, ] * migration_survival_rate)
      }
    }))

  if (!is.null(detour)) {
    regional_fish$detoured <- detoured_fish
  }
  return(regional_fish)

}

#' @title Route To South Delta
#' @description Entrains northern juveniles into the South Delta
#' @param freeport_flow Monthly mean flow at freeport in cubic feet per second
#' @param dcc_closed Number of days the Delta Cross Channel gates are closed during the month
#' @param month Current simulation month as an integer for calculating number of days the Delta Cross Channel gates are open
#' @param mean_freeport_flow Mean of flow at freeport for standardizing discharge
#' @param sd_freeport_flow Standard Deviation of flow at freeport for standardizing discharge
#' @param .sss_int Intercept for Sutter and Steamboat Sloughs, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .sss_freeport_discharge Coefficient for \code{freeport_flow} for Sutter and Steamboat Sloughs, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .sss_upper_asymptote Parameter representing the upper asymptote for Sutter and Steamboat Sloughs, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .dcc_intercept Intercept for Delta Cross Channel, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .dcc_freeport_discharge Coefficient for \code{freeport_flow} for Delta Cross Channel Gates, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .gs_intercept Intercept for Georgiana Slough, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .gs_freeport_discharge Coefficient for \code{freeport_flow} for Georgiana Slough, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .gs_dcc_effect_on_routing Parameter representing the dcc effect on routing, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @param .gs_lower_asymptote Parameter representing the lower asymptote for Georgiana Slough, source: \href{https://doi.org/10.1139/cjfas-2017-0310}{This submodel is adapted from Perry et al. (2018)}
#' @export
#'
route_to_south_delta <- function(freeport_flow, dcc_closed, month,
                              mean_freeport_flow = 21546.19,
                              sd_freeport_flow = 14375.9,
                              .sss_int = 1.8922350,
                              .sss_freeport_discharge = 2.1703750,
                              .sss_upper_asymptote = 0.3512465,
                              .dcc_intercept = -1.4896200,
                              .dcc_freeport_discharge = -1.2488650,
                              .gs_intercept = -2.9481450,
                              .gs_freeport_discharge = -2.9118350,
                              .gs_dcc_effect_on_routing = -0.5548430,
                              .gs_lower_asymptote = 0.2729845){

  number_of_days <- days_in_month(month)
  daily_gate_status <- c(dcc_closed, number_of_days - dcc_closed)
  gate_status <- c(closed = 0, open = 1)

  standardized_flow <- (freeport_flow - mean_freeport_flow) / (2 * sd_freeport_flow)

  #----- First Junction, Sacramento and Sutter/Steamboat  ---------
  # Probability of entering Sutter/Steamboat
  psi_SS_x <- .sss_int + .sss_freeport_discharge * standardized_flow
  psi_SS <- .sss_upper_asymptote / (1 + exp(-psi_SS_x))
  # Probability of remaining in Sacramento at junction with Sutter/Steamboat
  psi_SAC1 <- 1 - psi_SS

  #----- Second junction, Sacramento, DCC, and Georgiana Slough  ---------
  # Probability of entering DCC conditional on arriving at the river junction
  # (i.e, conditional on remaining in the Sacramento River at the Sutter/Steamboat)
  psi_DCC_x <- .dcc_intercept + .dcc_freeport_discharge * standardized_flow
  psi_DCC <- (1 / (1 + exp(-psi_DCC_x))) * gate_status

  # Probability of entering Geo conditional on arriving at junction and
  # not entering DCC
  psi_GEO_notDCC_x <- .gs_intercept + .gs_freeport_discharge * standardized_flow + .gs_dcc_effect_on_routing * gate_status
  psi_GEO_notDCC <- .gs_lower_asymptote + (1 - .gs_lower_asymptote) / (1 + exp(-psi_GEO_notDCC_x))

  # Unconditional probability of entering Georgiana Slough, but conditional
  # on arriving at the junction of Sac, DCC, and Geo.
  psi_GEO <- (1 - psi_DCC) * psi_GEO_notDCC

  # Unconditional probability of remaining in Sacramento River
  DCC <- sum(psi_SAC1 * psi_DCC * daily_gate_status) / number_of_days
  Geo <- sum(psi_SAC1 * psi_GEO * daily_gate_status) / number_of_days

  return(DCC + Geo)
}

#' @title Route and Rear in the Deltas
#' @description Determines if juveniles stay in the delta or out migrate to golden gate
#' during a simulated month. Then the remaining juveniles in the delta rear
#' (growth and survival rates applied) and survival rates are applied to out migrating juveniles
#' @param year The current simulation year, 1-20
#' @param month The current simulation month, 1-8
#' @param migrants An n by 4 matrix of juvenile fish by watershed and size class
#' @param north_delta_fish An n by 4 matrix of juvenile fish by watershed and size class
#' @param south_delta_fish An n by 4 matrix of juvenile fish by watershed and size class
#' @param north_delta_habitat A vector of available habitat in square meters
#' @param south_delta_habitat A vector of available habitat in square meters
#' @param freeport_flows Monthly mean flow at freeport in cubic feet per second
#' @param cc_gates_days_closed Number of days the Cross Channel gates are closed during the month
#' @param rearing_survival_delta The rearing survival rate for North and South Delta
#' @param migratory_survival_delta The outmigration survival rate for North and South Delta
#' @param migratory_survival_bay_delta The outmigration survival rate in the Bay Delta
#' @param juveniles_at_chipps The accumulated juveniles at Chipps Island for the current year
#' @param growth_rates The delta growth rate
#' @param location_index Migratory survival probability location index for fish coming from 4 areas (1-4) representing
#' "northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", or "southern_fish" respectively
#' @param territory_size Array of juvenile fish territory requirements for \code{\link{fill_natal}}
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
route_and_rear_deltas <- function(year, month, migrants, north_delta_fish, south_delta_fish,
                         north_delta_habitat, south_delta_habitat,
                         freeport_flows,
                         cc_gates_days_closed,
                         rearing_survival_delta, migratory_survival_delta,
                         migratory_survival_bay_delta,
                         juveniles_at_chipps, growth_rates,
                         location_index = c(rep(1, 24), 3, rep(2, 2), rep(4, 4)),
                         territory_size,
                         stochastic) {

  prop_delta_fish_entrained <- route_to_south_delta(freeport_flow = freeport_flows[[month, year]] * 35.3147,
                                                 dcc_closed = cc_gates_days_closed[month],
                                                 month = month)

  sac_not_entrained <- t(sapply(1:nrow(migrants[1:23, ]), function(i) {
    if (stochastic) {
    rbinom(n = 4, migrants[1:23, ][i, ], prob = 1 - prop_delta_fish_entrained)
    } else {
      round(migrants[1:23, ][i, ] * (1 - prop_delta_fish_entrained))
    }
  }))

  # sac salvaged fish trucked to south delta
  migrants_and_salvaged <- migrants
  migrants_and_salvaged[1:23, ] <- migrants_and_salvaged[1:23, ] - sac_not_entrained

  north_delta_fish <- fill_regional(juveniles = sac_not_entrained + north_delta_fish,
                                    habitat = north_delta_habitat,
                                    territory_size = territory_size)

  south_delta_fish <- fill_regional(juveniles = migrants_and_salvaged + south_delta_fish,
                                    habitat = south_delta_habitat,
                                    territory_size = territory_size)

  if (month == 8) {
    north_delta_fish = list(migrants = north_delta_fish$inchannel + north_delta_fish$migrants)
    south_delta_fish = list(migrants = south_delta_fish$inchannel + south_delta_fish$migrants)
  }

  south_delta_migrants <- t(sapply(1:31, function(i) {
    if (stochastic) {
    rbinom(n = 4, size = round(south_delta_fish$migrants[i, ]), prob = migratory_survival_delta[location_index[i], ])
    } else {
      round(south_delta_fish$migrants[i, ] * migratory_survival_delta[location_index[i], ])
    }
  }))


  north_delta_out_survived <- t(sapply(1:nrow(north_delta_fish$migrants), function(i) {
    if (stochastic) {
      rbinom(n = 4, size = round(north_delta_fish$migrants[i, ]), prob = migratory_survival_bay_delta)
    } else {
      round(north_delta_fish$migrants[i, ] * migratory_survival_bay_delta)
    }
  }))

  south_delta_out_survived <- t(sapply(1:nrow(south_delta_migrants), function(i) {
    if (stochastic) {
      rbinom(n = 4, size = round(south_delta_migrants[i, ]), prob = migratory_survival_bay_delta)
    } else {
      round(south_delta_migrants[i, ] * migratory_survival_bay_delta)
    }
  }))

  migrants_at_golden_gate <- rbind(north_delta_out_survived, matrix(0, nrow = 8, ncol = 4)) + south_delta_out_survived

  juveniles_at_chipps <- juveniles_at_chipps + rbind(north_delta_fish$migrants, matrix(0, nrow = 8, ncol = 4)) + south_delta_migrants

  if (month != 8) {
    north_delta_fish <- rear(juveniles = north_delta_fish$inchannel,
                             survival_rate = rearing_survival_delta[1, ],
                             growth = growth_rates[,,1],
                             stochastic = stochastic)

    south_delta_fish <- rear(juveniles = south_delta_fish$inchannel,
                             survival_rate = rearing_survival_delta[2, ],
                             growth = growth_rates[,,1],
                             stochastic = stochastic)

  }

  return(list(migrants_at_golden_gate = migrants_at_golden_gate,
              north_delta_fish = north_delta_fish,
              south_delta_fish = south_delta_fish,
              juveniles_at_chipps = juveniles_at_chipps)
  )

}
