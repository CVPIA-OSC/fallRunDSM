#' @title Juvenile Rearing Survival
#' @description Calculates the juvenile rearing survival inchannel and on the floodplain
#' @param max_temp_thresh variable representing probability of exceeding the max temperature threshold
#' @param avg_temp_thresh variable representing probability of exceeding the avg temperature threshold
#' @param high_predation variable representing indicator of high predation for a watershed
#' @param contact_points variable representing total number of contact points per watershed
#' @param prop_diversions variable representing proportion of water diverted
#' @param total_diversions variable representing total amount of water diverted
#' @param stranded variable representing stranding rate per watershed
#' @param weeks_flooded variable representing total weeks flooded per watershed
#' @param ..surv_juv_rear_int  intercept, source: calibration (varies by tributary)
#' @param .avg_temp_thresh coefficient for avg_temp_thresh variable, source: \href{https://www.noaa.gov/sites/default/files/atoms/files/07354626766.pdf}{Marine and Chech (2004)}
#' @param .high_predation coefficient for high_predation variable, source: \href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
#' @param ..surv_juv_rear_contact_points coefficient for contact_points variable, source: calibration
#' @param ..surv_juv_rear_prop_diversions coefficient for prop_diversions variable, source: calibration
#' @param ..surv_juv_rear_total_diversions coefficient for total_diversions variable, source: calibration
#' @param .stranded coefficient for stranded variable, source: \href{#}{USFWS (2006) and CDWR (2006)}
#' @param .medium parameter for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .large parameter for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .floodplain parameter for floodplain rearing benefit, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/SOMMER_T-SDWA+180+Floodplain+rearing+of+juvenile+chinook+salmon+evidence+of+enhanced+growth+and+survival+.pdf}{Sommer et al. (2001)}
#' @source IP-117068
#' @export
surv_juv_rear <- function(max_temp_thresh, avg_temp_thresh, high_predation,
                          contact_points, prop_diversions, total_diversions,
                          stranded, weeks_flooded,
                          ..surv_juv_rear_int = 3.5,
                          .avg_temp_thresh = -0.717,
                          .high_predation = -0.122,
                          ..surv_juv_rear_contact_points = -0.0068,
                          ..surv_juv_rear_prop_diversions = -0.1755,
                          ..surv_juv_rear_total_diversions = -0.0005,
                          .stranded = -1.939,
                          .medium = 1.48,
                          .large = 2.223,
                          .floodplain = 0.47){
  # determine the proportion of weeks when flooded vs not
  prop_ic <-ifelse(weeks_flooded > 0, (4 - weeks_flooded) / 4, 1)
  prop_fp <- 1 - prop_ic

  base_score_inchannel <- ..surv_juv_rear_int +
    (.avg_temp_thresh * avg_temp_thresh) +
    (.high_predation * high_predation) +
    (..surv_juv_rear_contact_points * contact_points * high_predation) +
    (..surv_juv_rear_prop_diversions * prop_diversions) +
    (..surv_juv_rear_total_diversions * total_diversions) +
    (.stranded * stranded)

  base_score_floodplain <- ..surv_juv_rear_int + .floodplain +
    (.avg_temp_thresh  * avg_temp_thresh) + (.high_predation * high_predation)

  # hi.tmp is emebeded here needs to be exposed so that it van be varied
  s1 <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score_inchannel))
  m1 <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score_inchannel + .medium))
  l1 <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score_inchannel  + .large))

  s2 <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score_floodplain)) ^ prop_fp
  m2 <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score_floodplain + .medium)) ^ prop_fp
  l2 <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score_floodplain + .large)) ^ prop_fp

  list(
    inchannel = cbind(s = s1,
                      m = m1,
                      l = l1,
                      vl = 1),
    floodplain = cbind(s = (s1^prop_ic * s2),
                       m = (m1^prop_ic * m2),
                       l = (l1^prop_ic * l2),
                       vl = 1)
  )
}

#' @title Juvenile Bypass Survival
#' @description Calculates the juvenile rearing survival in the bypasses
#' @param max_temp_thresh Variable representing the probability of exceeding the max temp threshold
#' @param avg_temp_thresh Variable representing the probability of exceeding the average temperature
#' @param high_predation Variable representing an indicator for high predation in watershed
#' @param ..surv_juv_bypass_int intercept, source: calibration
#' @param .avg_temp_thresh coefficient for avg_temp_thresh variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/marine_cech_water_temp_effects.pdf}{Marine and Chech (2004)}
#' @param .high_predation coefficient for high_predation variable, source:\href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
#' @param .medium parameter for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .large parameter for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .floodplain parameter for floodplain rearing benefit, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/SOMMER_T-SDWA+180+Floodplain+rearing+of+juvenile+chinook+salmon+evidence+of+enhanced+growth+and+survival+.pdf}{Sommer et al. (2001)}
#'
#' @source IP-117068
#' @export
surv_juv_bypass <- function(max_temp_thresh, avg_temp_thresh, high_predation,
                            ..surv_juv_bypass_int = -3.5,
                            .avg_temp_thresh = -0.717,
                            .high_predation = -0.122,
                            .medium = 1.48,
                            .large = 2.223,
                            .floodplain = 0.47){

  base_score <- ..surv_juv_bypass_int + .floodplain +
                .avg_temp_thresh * avg_temp_thresh +
                .high_predation * high_predation

  s <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score))
  m <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score + .medium))
  l <- ifelse(max_temp_thresh, .0001, boot::inv.logit(base_score + .large))

  cbind(s = s, m = m, l = l, vl = 1)
}

#' @title Juvenile Delta Survival
#' @description Calculates the juvenile rearing survival in the deltas
#' @param avg_temp Variable representing average temperature in the delta
#' @param max_temp_thresh Variable representing the probability of exceeding the max temperature
#' @param avg_temp_thresh Variable representing the probability of exceeding the average temperature
#' @param high_predation Variable representing an indicator for high predation in delta
#' @param contact_points Variable representing the number of contact points in watershed
#' @param prop_diverted Variable representing the proportion of water diverted
#' @param total_diverted Variable representing the total diversions
#' @param ..surv_juv_delta_int intercept, source: calibration
#' @param .avg_temp_thresh Coefficient for avg_temp_thresh variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/marine_cech_water_temp_effects.pdf}{Marine and Chech (2004)}
#' @param .high_predation Coefficient for high_predation variable, source: \href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
#' @param ..surv_juv_delta_contact_points Coefficient for contact_points variable, source: calibration
#' @param .prop_diverted Coefficient for prop_diversions variable, source: Newman and Brandes (2010)
#' @param ..surv_juv_delta_total_diverted Coefficient for total_diversions variable, source: calibration
#' @param .medium parameter for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .large parameter for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @source IP-117068
#' @export
surv_juv_delta <- function(avg_temp, max_temp_thresh, avg_temp_thresh, high_predation, contact_points,
                           prop_diverted, total_diverted,
                           ..surv_juv_delta_int = 1.4,
                           .avg_temp_thresh = -0.717,
                           .high_predation = -0.122,
                           ..surv_juv_delta_contact_points = 0.0358 * -0.189,
                           .prop_diverted = -3.51,
                           ..surv_juv_delta_total_diverted = 0.5 * -0.0021,
                           .medium = 1.48,
                           .large = 2.223){
  # north delta
  north_delta_surv <- rep((avg_temp <= 16.5)*.42 + (avg_temp > 16.5 & avg_temp < 19.5) * 0.42 / (1.55^(avg_temp-15.5)) + (avg_temp > 19.5 & avg_temp < 25)*0.035,4)

  # south delta
  base_score <- ..surv_juv_delta_int +
    .avg_temp_thresh * avg_temp_thresh[2] +
    .high_predation * high_predation[2] +
    ..surv_juv_delta_contact_points * contact_points[2] * high_predation[2] +
    .prop_diverted * prop_diverted[2] +
    ..surv_juv_delta_total_diverted * total_diverted[2]

  s <- ifelse(max_temp_thresh[2], .0001, boot::inv.logit(base_score))
  m <- ifelse(max_temp_thresh[2], .0001, boot::inv.logit(base_score + .medium))
  l <- ifelse(max_temp_thresh[2], .0001, boot::inv.logit(base_score + .large))

  south_delta_surv <- cbind(s = s, m = m, l = l, vl = 1)
  result <- rbind("north_delta" = north_delta_surv, "south_delta" = south_delta_surv)
  row.names(result) <- c("North Delta", "South Delta")

  result
}


#' @title Get Rearing Survival Rates
#' @description Calculates the juvenile inchannel, floodplain, bypasses, and
#' deltas rearing survival rates for a month and year of the simulation
#' @param year The simulation year, 1-20
#' @param month The simulation month, 1-8
#' @param scenario The current scenario
#' @param ..surv_juv_rear_int TODO
#' @param ..surv_juv_rear_contact_points TODO
#' @param ..surv_juv_rear_prop_diversions TODO
#' @param ..surv_juv_rear_total_diversions TODO
#' @param ..surv_juv_bypass_int TODO
#' @param ..surv_juv_delta_int TODO
#' @param ..surv_juv_delta_contact_points TODO
#' @param ..surv_juv_delta_total_diverted TODO
#' @param .surv_juv_rear_avg_temp_thresh TODO
#' @param .surv_juv_rear_high_predation TODO
#' @param .surv_juv_rear_stranded TODO
#' @param .surv_juv_rear_medium TODO
#' @param .surv_juv_rear_large TODO
#' @param .surv_juv_rear_floodplain TODO
#' @param .surv_juv_bypass_avg_temp_thresh TODO
#' @param .surv_juv_bypass_high_predation TODO
#' @param .surv_juv_bypass_medium TODO
#' @param .surv_juv_bypass_large TODO
#' @param .surv_juv_delta_avg_temp_thresh TODO
#' @param .surv_juv_delta_high_predation TODO
#' @param .surv_juv_delta_prop_diverted TODO
#' @param .surv_juv_delta_medium TODO
#' @param .surv_juv_delta_large TODO
#' @source IP-117068
#' @export
get_rearing_survival_rates <- function(year, month, scenario,
                                       avg_temp,
                                       avg_temp_delta,
                                       prob_strand_early,
                                       prob_strand_late,
                                       proportion_diverted,
                                       total_diverted,
                                       delta_proportion_diverted,
                                       delta_total_diverted,
                                       weeks_flooded,
                                       prop_high_predation,
                                       contact_points,
                                       delta_contact_points,
                                       delta_prop_high_predation,
                                       ..surv_juv_rear_int,
                                       ..surv_juv_rear_contact_points,
                                       ..surv_juv_rear_prop_diversions,
                                       ..surv_juv_rear_total_diversions,
                                       ..surv_juv_bypass_int,
                                       ..surv_juv_delta_int,
                                       ..surv_juv_delta_contact_points,
                                       ..surv_juv_delta_total_diverted,
                                       .surv_juv_rear_avg_temp_thresh,
                                       .surv_juv_rear_high_predation,
                                       .surv_juv_rear_stranded,
                                       .surv_juv_rear_medium,
                                       .surv_juv_rear_large,
                                       .surv_juv_rear_floodplain,
                                       .surv_juv_bypass_avg_temp_thresh,
                                       .surv_juv_bypass_high_predation,
                                       .surv_juv_bypass_medium,
                                       .surv_juv_bypass_large,
                                       .surv_juv_bypass_floodplain,
                                       .surv_juv_delta_avg_temp_thresh,
                                       .surv_juv_delta_high_predation,
                                       .surv_juv_delta_prop_diverted,
                                       .surv_juv_delta_medium,
                                       .surv_juv_delta_large) {
  watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                        "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                        "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                        "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                        "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                        "Feather River", "Yuba River", "Lower-mid Sacramento River",
                        "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                        "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                        "Tuolumne River", "San Joaquin River")

  size_class_labels <- c('s', 'm', 'l', 'vl')

  # weird temp stuff

  aveT20 <- rbinom(31, 1, boot::inv.logit(-14.32252 + 0.72102 * avg_temp[ , month , year]))
  maxT25 <- rbinom(31, 1, boot::inv.logit(-23.1766 + 1.4566 * avg_temp[ , month, year]))
  aveT20D <- rbinom(2, 1, boot::inv.logit(-18.30017 + 0.96991 * avg_temp_delta[month, year, ]))
  maxT25D <- rbinom(2, 1, boot::inv.logit(-157.537 + 6.998 * avg_temp_delta[month, year, ]))

  # set proportion fish stranding
  prob_ws_strand <- if(month < 4) prob_strand_early else prob_strand_late


  ws_strand <-rbinom(31, 1, prob_ws_strand)

  # proportion and total water diverted
  proportion_diverted <- proportion_diverted[ , month, year]
  total_diverted <- total_diverted[ , month, year]
  delta_proportion_diverted <- delta_proportion_diverted[month, year, ]
  delta_total_diverted <- delta_total_diverted[ month, year, ]

  # weeks flooded
  weeks_flood <- weeks_flooded[ , month, year]

  # predator information

  high_predation <- rbinom(31, 1, prop_high_predation)
  num_contact_points <- contact_points
  delta_num_contact_points <- delta_contact_points
  delta_high_predation <- delta_prop_high_predation

  # replicate values if needed
  if (length(..surv_juv_rear_int) == 1) ..surv_juv_rear_int <- rep(..surv_juv_rear_int, 31)

  rear_surv <- t(sapply(1:31, function(x) {
    surv_juv_rear(max_temp_thresh = maxT25[x],
                  avg_temp_thresh = aveT20[x],
                  high_predation = high_predation[x],
                  contact_points = num_contact_points[x],
                  prop_diversions = proportion_diverted[x],
                  total_diversions = total_diverted[x],
                  stranded = ws_strand[x],
                  weeks_flooded = weeks_flood[x],
                  ..surv_juv_rear_int = ..surv_juv_rear_int[x],
                  ..surv_juv_rear_contact_points = ..surv_juv_rear_contact_points,
                  ..surv_juv_rear_prop_diversions = ..surv_juv_rear_prop_diversions,
                  ..surv_juv_rear_total_diversions = ..surv_juv_rear_total_diversions,
                  .avg_temp_thresh = .surv_juv_rear_avg_temp_thresh,
                  .high_predation = .surv_juv_rear_high_predation,
                  .stranded = .surv_juv_rear_stranded,
                  .medium = .surv_juv_rear_medium,
                  .large = .surv_juv_rear_large,
                  .floodplain = .surv_juv_rear_floodplain)
  }))

  river_surv <- matrix(unlist(rear_surv[ , 1]), ncol = 4, byrow = TRUE)
  flood_surv <- matrix(unlist(rear_surv[ , 2]), ncol = 4, byrow = TRUE)

  if (!is.null(scenario)) {
    survival_increase <- matrix(0, nrow = 31, ncol = 4)
  }

  bp_surv <- surv_juv_bypass(max_temp_thresh = maxT25[22],
                             avg_temp_thresh = aveT20[22],
                             high_predation = 0,
                             ..surv_juv_bypass_int = ..surv_juv_bypass_int,
                             .avg_temp_thresh = .surv_juv_bypass_avg_temp_thresh,
                             .high_predation = .surv_juv_bypass_high_predation,
                             .medium = .surv_juv_bypass_medium,
                             .large = .surv_juv_bypass_large,
                             .floodplain = .surv_juv_bypass_floodplain)

  sutter_surv <- sqrt(bp_surv)
  yolo_surv <- sqrt(bp_surv)

  # TODO hi.tmp is an embeded value need to expose?
  delta_juv_surv <- surv_juv_delta(avg_temp = avg_temp_delta[month, year, "North Delta"],
                                   max_temp_thresh = maxT25D,
                                   avg_temp_thresh = aveT20D,
                                   high_predation = delta_high_predation,
                                   contact_points = delta_num_contact_points,
                                   prop_diverted = delta_proportion_diverted,
                                   total_diverted = delta_total_diverted,
                                   ..surv_juv_delta_int = ..surv_juv_delta_int,
                                   ..surv_juv_delta_contact_points = ..surv_juv_delta_contact_points,
                                   ..surv_juv_delta_total_diverted = ..surv_juv_delta_total_diverted,
                                   .avg_temp_thresh = .surv_juv_delta_avg_temp_thresh,
                                   .high_predation = .surv_juv_delta_high_predation,
                                   .prop_diverted = .surv_juv_delta_prop_diverted,
                                   .medium = .surv_juv_delta_medium,
                                   .large = .surv_juv_delta_large)

  return(
    list(
      inchannel = river_surv,
      floodplain = flood_surv,
      sutter = sutter_surv,
      yolo = yolo_surv,
      delta = delta_juv_surv)
  )
}

# JUVENILE MIGRATORY SURVIVAL -----
#' @title Juvenile Mainstem Sacramento Outmigration Survival
#' @description Calculates the Mainstem Sacramento juvenile out migration survival
#' @param flow_cms Variable representing upper Sacramento River flow in cubic meters per second
#' @source IP-117068
#' @export
surv_juv_outmigration_sac <- function(flow_cms){

  result <- rep((flow_cms <= 122)*0.03 + (flow_cms > 122 & flow_cms <= 303)*0.189 + (flow_cms > 303)*0.508,4)
  setNames(result, c("s", "m", "l", "vl"))
}


#' @title Juvenile San Joaquin Outmigration Survival
#' @description Calculates the San Joaquin River juvenile out migration survival
#' @param ..surv_juv_outmigration_sj_int intercept, source: calibration
#' @param .medium parameter for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .large parameter for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @source IP-117068
#' @export
surv_juv_outmigration_san_joaquin <- function(..surv_juv_outmigration_sj_int = -3.5,
                                              .medium = 1.48, .large = 2.223){

  s <- boot::inv.logit(..surv_juv_outmigration_sj_int)
  m <- boot::inv.logit(..surv_juv_outmigration_sj_int + .medium)
  l <- vl <- boot::inv.logit(..surv_juv_outmigration_sj_int + .large)

  cbind(s = s, m = m, l = l, vl = vl)
}

#' @title Juvenile Delta Outmigration Survival
#' @description Calculates the Sacramento Delta juvenile out migration survival
#' @param delta_flow Variable describing delta inflow in cubic meters per second
#' @param avg_temp Variable describing monthly mean temperature in celsius
#' @param perc_diversions Variable describing monthly mean percent diverted
#' @param .intercept_one Intercept for model one, source: TODO
#' @param .intercept_two Intercept for model two, source: TODO
#' @param .intercept_three Intercept for model three, source: TODO
#' @param .delta_flow Coefficient for delta_flow variable, source: TODO
#' @param .avg_temp Coefficient for avg_temp variable, source: TODO
#' @param .perc_diversions Coefficient for perc_diversions variable, source: TODO
#' @param .medium parameter for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @param .large parameter for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' @source IP-117068
#' @export
surv_juv_outmigration_sac_delta <- function(delta_flow, avg_temp, perc_diversions,
                                            .intercept_one = -3.5,
                                            .intercept_two =  0.3,
                                            .intercept_three = -3.5,
                                            .delta_flow = 0.0013,
                                            .avg_temp = 0.386,
                                            .perc_diversions = -0.033,
                                            .medium = 1.48,
                                            .large = 2.223){

  model_weight <- 1/3

  base_score1 <- .intercept_one + .delta_flow * delta_flow
  base_score2 <- .intercept_two + .avg_temp * avg_temp
  base_score3 <- .intercept_three + .perc_diversions * perc_diversions

  s <- model_weight * (boot::inv.logit(base_score1) +
                         boot::inv.logit(base_score2) +
                         boot::inv.logit(base_score3))

  m <- model_weight * (boot::inv.logit(base_score1 + .medium) +
                         boot::inv.logit(base_score2 + .medium) +
                         boot::inv.logit(base_score3 + .medium))

  vl <- l <- model_weight * (boot::inv.logit(base_score1 + .large) +
                               boot::inv.logit(base_score2 + .large) +
                               boot::inv.logit(base_score3 + .large))

  cbind(s = s, m = m, l = l, vl = vl)
}

#' @title Juvenile Delta Outmigration Survival
#' @description Calculates the North and South Delta juvenile out migration survival
#' @param prop_DCC_closed proportion of days the Delta Cross Channel Gates are closed
#' @param horr_barr indicator if head of old river physical barrier in place
#' @param freeport_flow average daily discharge at Freeport in cubic meters per second
#' @param vernalis_flow average daily discharge at Vernalis in cubic meters per second
#' @param stockton_flow average daily discharge at Stockton in cubic meters per second
#' @param vernalis_temperature average daily temperature at Vernalis in °C
#' @param prisoners_point_temperature average daily temperature of the San Joaquin River at Prisoners Point °C
#' @param CVP_exp average daily exports Central Valley Project in cubic meters per second
#' @param SWP_exp average daily exports State Water Project in cubic meters per second
#' @param Trap_trans proportion of smolts trapped at Vernalis and transported to Chips island
#' @section Parameters:
#' All parameters were derived from Perry et al. (2018)
#' @details function returns proportion of fish from the Sacramento at Feeeport (northern_fish)
#' Mokelumne and Cosumnes (cosumnes_mokelumne_fish), Calaveras (calaveras_fish) and
#' San Joaquin tributaries from Vernalis (southern_fish) arriving alive at Chipps
#' Island in four size groups (35-42mm, 42-72mm, 72-110mm, >110mm).
#' Note that the models were fit to data that were >80 mm. Therefore, this does not
#' predict outside of the data so sizes <= 80mm are assumed to me 80mm long as
#' requested by Russ Perry.
#' @source IP-117068
#' @export
surv_juv_outmigration_delta <- function(prop_DCC_closed, hor_barr, freeport_flow, vernalis_flow,
                                        stockton_flow, vernalis_temperature, prisoners_point_temperature, CVP_exp, SWP_exp, trap_trans){

  prop_DCC_open <- 1 - prop_DCC_closed

  # number of CVP pumps operating
  pump_operation_breaks <- c(60, 95.6, 499)
  possible_number_of_pumps <- c(1, 2, 3, 5)

  pump_index <- findInterval(CVP_exp, pump_operation_breaks) + 1
  number_of_pumps <- possible_number_of_pumps[pump_index]

  #### First estimate North Delta parameters
  freeport <- (freeport_flow - 610.1) / 814.2

  #Entrained into sutter/steamboat
  param_steamboat_intercept <- 2.014670488
  param_steamboat_flow <- 2.458233791 # Standardized Sacramento mean discharge at Freeport
  param_steamboat_upper_limit <- 0.36241455 # Upper asymptote for entrainment into Sutter/Steamboat
  psi_steam <- param_steamboat_upper_limit * boot::inv.logit(param_steamboat_intercept + param_steamboat_flow * freeport)

  # remain in Sacramento
  psi_sac1 <- 1- psi_steam

  # entrained DCC
  param_dcc_intercept <- -1.515076654
  param_dcc_discharge <- -1.282849232
  param_dcc_gates <- 0.030214424

  psi_dcc <- boot::inv.logit((-1.515076654 - 1.282849232 * freeport + 0.030214424 * prop_DCC_open)) * prop_DCC_open +
    (1 - prop_DCC_open) * boot::inv.logit(-10)

  # entrained georgiana slough
  param_georgiana_intercept <- -3.111
  param_georgiana_gates <- -0.9443
  param_georgiana_flow <- -3.1743
  param_georgiana_lower_limit <- 0.2669
  psi_geo <- (1 - psi_dcc) * (0.2669 + (1 - 0.2669) * boot::inv.logit(-3.111 - 0.9443 * prop_DCC_open - 3.1743 * freeport))

  # remain in Sacramento
  psi_sac2 <- 1- psi_dcc - psi_geo

  #size cutoffs 42,72,110, use min from study as smallest
  FL <- c(81, 81, 81, 140)
  size <-  0.152 * (FL - 155.1) / 21.6

  regions <-  c('Sac Freeport to Sutter/Steamboat junction', 'Sac Sutter/Steamboat junction to Georgiana',
                'Sutter/Steamboat Slough', 'Sac Georgiana Junction to Rio Vista',
                'Georgiana Slough', 'DCC to Moke', 'Sac Rio Vista to Chipps Island',
                'interior Delta')
  betas <- list(
    b0 = c(3.243, 3.243, 1.2095, 2.533, 1.1175, 0.03667, 1.0934, -0.46002),
    b_dcc_open = c(0.3225, 0.0673, 0.1508, -0.7343, -0.0769, -0.2541, -0.4816, -0.12312),
    b_freeport_flow = c(1.1049, 1.1049, 2.2758, 2.5756, 2.1591, 1.1510, 0.0379, 0.03898)
  )

  score <- function( b0, b_dcc_open, b_freeport_flow) {
    b0 + b_dcc_open * prop_DCC_open + b_freeport_flow * freeport
  }

  survival_rates <- purrr::map(purrr::pmap_dbl(betas, score), ~ boot::inv.logit(.x + size))
  names(survival_rates) <- regions

  #### Next estimate South Delta parameters
  # Probability of remaining in SJR at HOR
  prob_remain_at_head_old_river_intercept <- -0.75908
  prob_remain_at_head_old_river_barrier <- 1.72020
  prob_remain_at_head_old_river_flow <- 0.00361
  psi_sjr1 <- boot::inv.logit(-0.75908 + 1.72020 * hor_barr + 0.00361 * vernalis_flow + 0.02718 * hor_barr * vernalis_flow)

  # Probability of entering old river
  psi_OR <- 1 - psi_sjr1

  #Probability of remaining in SJR at Turner Cut
  prob_remain_at_turner_cut_intercept <-
  psi_sjr2 <- boot::inv.logit(5.83131 - 0.037708993 * stockton_flow)

  # probability of entering Turner cut
  psi_TC <- 1 - psi_sjr2

  #Probability of entrainment at CVP (Karp et al 2017) logit link
  psi_CVP <- boot::inv.logit(-3.9435 + 2.9025 * number_of_pumps -0.3771 * number_of_pumps^2)

  #Probability of entrainment at SWP
  psi_SWP <- (1 - psi_CVP) * boot::inv.logit(-1.48969 + 0.016459209 * SWP_exp)

  # Probability of remaining old river north
  psi_ORN <- 1 - psi_CVP - psi_SWP

  #Survival Tributaries to HOR logit link
  s_prea <- boot::inv.logit(5.77500 + 0.00706 * vernalis_flow - 0.32810 * vernalis_temperature + size)

  #Survival HOR to Turner Cut logit link
  s_a <- boot::inv.logit(-2.90330+ 0.01059 * vernalis_flow + size)

  #Survival SJR Turner Cut to Chipps
  s_bc <- boot::inv.logit(13.41840 - 0.90070 * prisoners_point_temperature + size)

  #Survival down OR HOR to CVP
  s_d <- boot::inv.logit(2.16030 -0.20500 * vernalis_temperature + size)

  #Survival ORN to Chipps Island (SJRGA)
  s_efc <- 0.01

  #Survival through CVP (Karp et al 2017) logit link
  s_CVP <- boot::inv.logit(-3.0771 + 1.8561 * number_of_pumps - 0.2284 * number_of_pumps^2)

  #Survival through SWP (Gingras 1997)
  s_SWP <- 0.1325

  # North origin fish movement and survival
  northern_fish <- survival_rates[[1]] * psi_steam * survival_rates[[3]] * survival_rates[[7]] +
    survival_rates[[1]] * psi_sac1 * survival_rates[[2]] * psi_sac2 * survival_rates[[4]] * survival_rates[[7]] +
    survival_rates[[1]] * psi_sac1 * survival_rates[[2]] * psi_dcc * survival_rates[[6]] * survival_rates[[8]] +
    survival_rates[[1]] * psi_sac1 * survival_rates[[2]] * psi_geo * survival_rates[[5]] * survival_rates[[8]]

  # Cosumnes and Mokelume fish
  cosumnes_mokelumne_fish <- survival_rates[[6]] * (s_bc ^ 1/2)

  #Calavaras River
  calaveras_fish <- s_bc

  #South origin fish
  southern_fish <-
    (1 - trap_trans) * s_prea * psi_sjr1 * s_a * psi_sjr2 * s_bc +
    (1 - trap_trans) * s_prea * psi_sjr1 * s_a * psi_TC * s_efc +
    (1 - trap_trans) * s_prea * psi_OR * s_d * psi_ORN * s_efc +
    (1 - trap_trans) * s_prea * psi_OR * s_d * psi_CVP * s_CVP +
    (1 - trap_trans) * s_prea * psi_OR * s_d * psi_SWP * s_SWP +
    trap_trans

  survival_rates <- rbind(northern_fish, cosumnes_mokelumne_fish, calaveras_fish, southern_fish)
  colnames(survival_rates) <- c('s', 'm', 'l', 'vl')
  return(survival_rates)

}

#' @title Get Migratory Survival Rates
#' @description Calculates the juvenile out migration survival rates in all
#' regions for a month and year of the simulation
#' @param year The simulation year, 1-20
#' @param month The simulation month, 1-8
#' @param cc_gates_prop_days_closed
#' @param freeport_flows
#' @param vernalis_flows
#' @param stockton_flows
#' @param vernalis_temps
#' @param prisoners_point_temps
#' @param CVP_exports
#' @param SWP_exports
#' @param ..surv_juv_outmigration_sj_int
#' @param ..surv_juv_outmigration_sac_int_one
#' @param ..surv_juv_outmigration_sac_prop_diversions
#' @param ..surv_juv_outmigration_sac_total_diversions
#' @param ..surv_juv_outmigration_sac_int_two
#' @param .surv_juv_outmigration_san_joquin_medium TODO
#' @param .surv_juv_outmigration_san_joaquin_large TODO
#' @source IP-117068
#' @export
get_migratory_survival_rates <- function(year, month,
                                         cc_gates_prop_days_closed = cc_gates_prop_days_closed,
                                         freeport_flows = freeport_flows,
                                         vernalis_flows = vernalis_flows,
                                         stockton_flows = stockton_flows,
                                         vernalis_temps = vernalis_temps,
                                         prisoners_point_temps = prisoners_point_temps,
                                         CVP_exports = CVP_exports,
                                         SWP_exports = SWP_exports,
                                         upper_sacramento_flows = upper_sacramento_flows,
                                         delta_inflow = delta_inflow,
                                         avg_temp_delta = avg_temp_delta,
                                         avg_temp = avg_temp,
                                         delta_proportion_diverted = delta_proportion_diverted,
                                         .surv_juv_outmigration_sac_delta_intercept_one = .surv_juv_outmigration_sac_delta_intercept_one,
                                         .surv_juv_outmigration_sac_delta_intercept_two = .surv_juv_outmigration_sac_delta_intercept_two,
                                         .surv_juv_outmigration_sac_delta_intercept_three = .surv_juv_outmigration_sac_delta_intercept_three,
                                         .surv_juv_outmigration_sac_delta_delta_flow = .surv_juv_outmigration_sac_delta_delta_flow,
                                         .surv_juv_outmigration_sac_delta_avg_temp = .surv_juv_outmigration_sac_delta_avg_temp,
                                         .surv_juv_outmigration_sac_delta_perc_diversions = .surv_juv_outmigration_sac_delta_perc_diversions,
                                         .surv_juv_outmigration_sac_delta_medium = .surv_juv_outmigration_sac_delta_medium,
                                         .surv_juv_outmigration_sac_delta_large = .surv_juv_outmigration_sac_delta_large,
                                         ..surv_juv_outmigration_sj_int = ..surv_juv_outmigration_sj_int,
                                         ..surv_juv_outmigration_sac_int_one = ..surv_juv_outmigration_sac_int_one,
                                         ..surv_juv_outmigration_sac_prop_diversions = ..surv_juv_outmigration_sac_prop_diversions,
                                         ..surv_juv_outmigration_sac_total_diversions = ..surv_juv_outmigration_sac_total_diversions,
                                         ..surv_juv_outmigration_sac_int_two = ..surv_juv_outmigration_sac_int_two,
                                         .surv_juv_outmigration_san_joquin_medium = .surv_juv_outmigration_san_joquin_medium,
                                         .surv_juv_outmigration_san_joaquin_large = .surv_juv_outmigration_san_joaquin_large) {


  aveT20 <- rbinom(31, 1, boot::inv.logit(-14.32252 + 0.72102 * avg_temp[ , month , year]))
  maxT25 <- rbinom(31, 1, boot::inv.logit(-23.1766 + 1.4566 * avg_temp[ , month, year]))

  delta_survival <- surv_juv_outmigration_delta(
    prop_DCC_closed = cc_gates_prop_days_closed[month],
    hor_barr = 0,
    freeport_flow = freeport_flows[month, year],
    vernalis_flow = vernalis_flows[month, year],
    stockton_flow = stockton_flows[month, year],
    vernalis_temperature = vernalis_temps[month, year],
    prisoners_point_temperature = prisoners_point_temps[month, year],
    CVP_exp = CVP_exports[month, year],
    SWP_exp = SWP_exports[month, year],
    trap_trans = 0) # newDsurv

  u_sac_flow <- upper_sacramento_flows[month, year]
  sj_migration_surv <- surv_juv_outmigration_san_joaquin(..surv_juv_outmigration_sj_int = ..surv_juv_outmigration_sj_int,
                                                         .medium = .surv_juv_outmigration_san_joquin_medium,
                                                         .large = .surv_juv_outmigration_san_joaquin_large)

  # set up the regional survivals
  # uppermid_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow,
  #                                                      avg_temp = avg_temp[16, month, year],
  #                                                      total_diversions = total_diverted[16],
  #                                                      prop_diversions = proportion_diverted[16])^.5 # UM.Sac.S
  uppermid_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow)


  # lowermid_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow,
  #                                                      avg_temp = avg_temp[21, month, year],
  #                                                      total_diversions = total_diverted[21],
  #                                                      prop_diversions = proportion_diverted[21])^.5 # LM.Sac.S
  lowermid_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow)


  # lower_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow,
  #                                                   avg_temp = avg_temp[24, month, year],
  #                                                   total_diversions = total_diverted[24],
  #                                                   prop_diversions = proportion_diverted[24])^.5 # LL.Sac.S
  lower_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow)

  sac_delta_migration_surv <- surv_juv_outmigration_sac_delta(delta_flow = delta_inflow[month, year, ],
                                                              avg_temp = avg_temp_delta[month, year, ],
                                                              perc_diversions = delta_proportion_diverted[month, year, ] * 100,
                                                              .intercept_one = .surv_juv_outmigration_sac_delta_intercept_one,
                                                              .intercept_two = .surv_juv_outmigration_sac_delta_intercept_two,
                                                              .intercept_three = .surv_juv_outmigration_sac_delta_intercept_three,
                                                              .delta_flow = .surv_juv_outmigration_sac_delta_delta_flow,
                                                              .avg_temp = .surv_juv_outmigration_sac_delta_avg_temp,
                                                              .perc_diversions = .surv_juv_outmigration_sac_delta_perc_diversions,
                                                              .medium = .surv_juv_outmigration_sac_delta_medium,
                                                              .large = .surv_juv_outmigration_sac_delta_large) #Sac.Delt.S

  bay_delta_migration_surv <- mean(c(0.43, 0.46, 0.26, 0.25, 0.39)) # Bay.S Chipps island to bay

  bp_surv <- surv_juv_bypass(max_temp_thresh = maxT25[22],
                             avg_temp_thresh = aveT20[22],
                             high_predation = 0)

  sutter <- sqrt(bp_surv)

  return(
    list(
      delta = delta_survival,
      san_joaquin = sj_migration_surv,
      uppermid_sac = uppermid_sac_migration_surv,
      lowermid_sac = lowermid_sac_migration_surv,
      lower_sac = lower_sac_migration_surv,
      sutter = sutter,
      yolo = sutter,
      sac_delta = sac_delta_migration_surv,
      bay_delta = bay_delta_migration_surv
    ))
}
