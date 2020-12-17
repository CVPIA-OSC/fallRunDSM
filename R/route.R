#' @title Route Natal Streams
#' @description Determines if juveniles stay in their natal tributary, are detoured
#' to a bypass, or out migrate during a simulated month
#' @param year The current simulation year, 1-20
#' @param month The current simulation month, 1-8
#' @param juvenile An n by 4 matrix of juvenile fish by watershed and size class
#' @param inchannel_habitat A vector of available habitat in square meters
#' @param floodplain_habitat A vector of available floodplain habitat in square meters
#' @param prop_pulse_flows The proportion of pulse flows
#' @param detour Values can be 'sutter' or 'yolo' if some juveniles are detoured on to that bypass, otherwise NULL
#' @source IP-117068
#' @export
route <- function(year, month, juveniles, inchannel_habitat, floodplain_habitat, prop_pulse_flows, detour = NULL) {

  natal_watersheds <- fill_natal(juveniles = juveniles,
                                 inchannel_habitat = inchannel_habitat,
                                 floodplain_habitat = floodplain_habitat)

  # estimate probability leaving as function of pulse flow
  prob_pulse_leave <- pulse_movement(prop_pulse_flows[ , month])

  # total fish that will migrate becuase of pulse flows, this derived using total in river
  # and a binomial selection based on pr of movement due to pulse flows
  pulse_migrants <- t(sapply(1:nrow(juveniles), function(i) {
    rbinom(n = 4, size = round(natal_watersheds$inchannel[i, ]), prob = prob_pulse_leave[i, ])
  }))


  # update in river fish based on the pulse flow results
  natal_watersheds$inchannel <- (natal_watersheds$inchannel - pulse_migrants)

  # update migratory fish based on the pulse flow results
  natal_watersheds$migrants <- natal_watersheds$migrants + pulse_migrants

  if (!is.null(detour)) {
    bypass <- ifelse(detour == 'sutter', "Sutter Bypass", "Yolo Bypass")

    detoured_fish <- t(sapply(1:nrow(natal_watersheds$migrants), function(i) {

      rbinom(n = 4,
             size = round(natal_watersheds$migrants[i, ]),
             prob = proportion_flow_bypass[month, year, bypass])
    }))

    natal_watersheds$migrants <- natal_watersheds$migrants - detoured_fish
    natal_watersheds$detoured <- detoured_fish
  }

  return(natal_watersheds)
}

#' @title Route Bypass
#' @description Determines if juveniles remain in the bypass or out migrate
#' @param bypass_fish An n by 4 matrix of juvenile fish by watershed and size class
#' @param bypass_habitat A vector of available habitat in square meters
#' @param migration_survival_rate The outmigration survival rate
#' @source IP-117068
#' @export
route_bypass <- function(bypass_fish, bypass_habitat, migration_survival_rate) {

  bypass_fish <- fill_regional(juveniles = bypass_fish,
                               habitat = bypass_habitat)

  bypass_fish$migrants <- t(
    sapply(1:nrow(bypass_fish$migrants), function(i) {

      rbinom(n = 4, size = bypass_fish$migrants[i, ], prob = migration_survival_rate)
    }))

  colnames(bypass_fish$migrants) <- c('s', 'm', 'l', 'vl')

  return(bypass_fish)
}

#' @title Route Regions
#' @description Determines if juveniles stay in the region (Sections of Mainstem
#' Sacramento River or San Joaquin River) or out migrate during a simulated month
#' @param month The simulation month, 1-8
#' @param migrants An n by 4 matrix of juvenile fish by watershed and size class
#' @param inchannel_habitat A vector of available habitat in square meters
#' @param floodplain_habitat A vector of available floodplain habitat in square meters
#' @param prop_pulse_flows The proportion of pulse flows
#' @param migration_survival_rate The outmigration survival rate
#' @source IP-117068
#' @export
route_regional <- function(month, migrants, inchannel_habitat, floodplain_habitat, prop_pulse_flows, migration_survival_rate) {
  # fill up upper mainstem, but in river fish can leave due to pulses

  regional_fish <- fill_regional(juveniles = migrants,
                                 habitat = inchannel_habitat,
                                 floodplain_habitat = floodplain_habitat)
  # estimate probability leaving as function of pulse flow
  pulse_flows <- prop_pulse_flows[ , month]
  prob_pulse_leave <- matrix(pulse_movement(pulse_flows), ncol = 4, byrow = T)

  pulse_migrants <- t(sapply(1:nrow(regional_fish$inchannel), function(i) {

    rbinom(n = 4, size = regional_fish$inchannel[i, ], prob = prob_pulse_leave)
  }))

  # remove and add migrants
  regional_fish$inchannel <- regional_fish$inchannel - pulse_migrants
  regional_fish$migrants <- regional_fish$migrants + pulse_migrants

  # apply survival rate to migrants
  regional_fish$migrants <- t(
    sapply(1:nrow(regional_fish$migrants), function(i) {

      rbinom(n = 4, size = regional_fish$migrants[i, ], prob = migration_survival_rate)
    }))


  return(regional_fish)

}

#' @title South Delta Routing
#' @description Routes juveniles through the South Delta
#' @param freeport_flow Monthly mean flow at freeport in cubic feet per second
#' @param dcc_closed Number of days the Delta Cross Channel gates are closed during the month
#' @param month Current simulation month as an integer for calculating number of days
#' the Delta Cross Channel gates are open
#' @param mean_freeport_flow Mean of flow at freeport for standardizing discharge
#' @param sd_freeport_flow Standard Deviation of flow at freeport for standardizing discharge
#' @param betas Parameters for Sutter and Steamboat Sloughs (sss),  Delta Cross
#' Channel (dcc), and Georgiana Slough (gs). All parameters are posterior medians
#' @details
#' This submodel is adapted from Perry et al. (2018) https://doi.org/10.1139/cjfas-2017-0310
#' @source IP-117068
#' @export
route_south_delta <- function(freeport_flow, dcc_closed, month,
                              mean_freeport_flow = 21546.19,
                              sd_freeport_flow = 14375.9,
                              betas = list(sss = c(intercept = 1.8922350,
                                                   `freeport discharge` = 2.1703750,
                                                   `upper asymptote` = 0.3512465),
                                           dcc = c(intercept = -1.4896200,
                                                   `freeport discharge` = -1.2488650),
                                           gs = c(intercept = -2.9481450,
                                                  `freeport discharge` = -2.9118350,
                                                  `DCC effect on routing` = -0.5548430,
                                                  `lower asymptote` = 0.2729845))){

  number_of_days <- days_in_month(month)
  daily_gate_status <- c(dcc_closed, number_of_days - dcc_closed)
  gate_status <- c(closed = 0, open = 1)

  standardized_flow <- (freeport_flow - mean_freeport_flow) / (2 * sd_freeport_flow)

  #----- First Junction, Sacramento and Sutter/Steamboat  ---------
  # Probability of entering Sutter/Steamboat
  psi_SS <- boot::inv.logit(betas$sss[1] + betas$sss[2] * standardized_flow)
  # psi_SS <- betas$sss[3] / (1 + exp(-lpsi_SS))
  # Probability of remaining in Sacramento at junction with Sutter/Steamboat
  psi_SAC1 <- 1 - psi_SS

  #----- Second junction, Sacramento, DCC, and Georgiana Slough  ---------
  # Probability of entering DCC conditional on arriving at the river junction
  # (i.e, conditional on remaining in the Sacramento River at the Sutter/Steamboat)
  psi_DCC <- boot::inv.logit(betas$dcc[1] + betas$dcc[2] * standardized_flow) * gate_status

  # Probability of entering Geo conditional on arriving at junction and
  # not entering DCC
  psi_GEO_notDCC <- betas$gs[4] + boot::inv.logit(betas$gs[1] + betas$gs[2] * standardized_flow + betas$gs[3] * gate_status)

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
#' @param year Simulation year, 1-20
#' @param month Simulation month, 1-8
#' @param migrants An n by 4 matrix of juvenile fish by watershed and size class
#' @param north_delta_fish An n by 4 matrix of juvenile fish by watershed and size class
#' @param south_delta_fish An n by 4 matrix of juvenile fish by watershed and size class
#' @param north_delta_habitat A vector of available habitat in square meters
#' @param south_delta_habitat A vector of available habitat in square meters
#' @param rearing_survival_delta The rearing survival rate for North and South Delta
#' @param migratory_survival_delta The outmigration survival rate for North and South Delta
#' @param migratory_survival_sac_delta The outmigration survival rate in the Sacramento Delta
#' @param migratory_survival_bay_delta The outmigration survival rate in the Bay Delta
#' @param juveniles_at_chipps The accumulated juveniles at Chipps Island for the current year
#' @param growth_rates The delta growth rate
#' @param location_index Migratory survival probability location index for fish coming from 4 areas (1-4) representing
#' "northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", or "southern_fish" respectively
#' @source IP-117068
#' @export
route_and_rear_deltas <- function(year, month, migrants, north_delta_fish, south_delta_fish,
                         north_delta_habitat, south_delta_habitat,
                         rearing_survival_delta, migratory_survival_delta,
                         migratory_survival_sac_delta, migratory_survival_bay_delta,
                         juveniles_at_chipps, growth_rates,
                         location_index = c(rep(1, 24), 3, rep(2, 2), rep(4, 4))) {

  prop_delta_fish_entrained <- route_south_delta(freeport_flow = freeport_flows[[month, year]] * 35.3147,
                                                 dcc_closed = cc_gates_days_closed[month],
                                                 month = month)

  sac_not_entrained <- t(sapply(1:nrow(migrants[1:23, ]), function(i) {

    rbinom(n = 4, migrants[1:23, ][i, ], prob = 1 - prop_delta_fish_entrained)
  }))

  # sac salvaged fish trucked to south delta
  migrants_and_salvaged <- migrants
  migrants_and_salvaged[1:23, ] <- migrants_and_salvaged[1:23, ] - sac_not_entrained

  north_delta_fish <- fill_regional(juveniles = sac_not_entrained + north_delta_fish,
                                    habitat = north_delta_habitat)

  south_delta_fish <- fill_regional(juveniles = migrants_and_salvaged + south_delta_fish,
                                    habitat = north_delta_habitat)

  if (month == 8) {
    north_delta_fish = list(migrants = north_delta_fish$inchannel + north_delta_fish$migrants)
    south_delta_fish = list(migrants = south_delta_fish$inchannel + south_delta_fish$migrants)
  }

  south_delta_migrants <- t(sapply(1:31, function(i) {

    rbinom(n = 4, size = round(south_delta_fish$migrants[i, ]), prob = migratory_survival_delta[location_index[i], ])
  }))

  migrants_out <- t(sapply(1:nrow(north_delta_fish$migrants), function(i) {

    rbinom(n = 4, size = round(north_delta_fish$migrants[i, ]), prob = migratory_survival_sac_delta[1, ])
  }))

  migrants_out_survived <- t(sapply(1:nrow(migrants_out), function(i) {

    rbinom(n = 4, size = round(migrants_out[i, ]), prob = migratory_survival_bay_delta)
  }))

  south_delta_survived <- t(sapply(1:nrow(south_delta_migrants), function(i) {

    rbinom(n = 4, size = round(south_delta_migrants[i, ]), prob = migratory_survival_bay_delta)
  }))

  migrants_at_golden_gate <- rbind(migrants_out_survived, matrix(0, nrow = 8, ncol = 4)) + south_delta_survived

  juveniles_at_chipps <- juveniles_at_chipps + rbind(migrants_out, matrix(0, nrow = 8, ncol = 4)) + south_delta_migrants

  if (month != 8) {
    north_delta_fish <- rear(juveniles = north_delta_fish$inchannel,
                             survival_rate = rearing_survival_delta[1, ],
                             growth = growth_rates)

    south_delta_fish <- rear(juveniles = south_delta_fish$inchannel,
                             survival_rate = rearing_survival_delta[2, ],
                             growth = growth_rates)

  }

  return(list(migrants_at_golden_gate = migrants_at_golden_gate,
              north_delta_fish = north_delta_fish,
              south_delta_fish = south_delta_fish,
              juveniles_at_chipps = juveniles_at_chipps)
  )

}
