#' @title Juvenile Rearing Survival
#' @description Calculates the juvenile rearing survival inchannel and on the floodplain
#' @details See \code{\link{params}} for details on parameter sources
#' @param max_temp_thresh Variable representing probability of exceeding the max temperature threshold
#' @param avg_temp_thresh Variable representing probability of exceeding the avg temperature threshold
#' @param high_predation Variable representing indicator of high predation for a watershed
#' @param contact_points Variable representing total number of contact points per watershed
#' @param prop_diversions Variable representing proportion of water diverted
#' @param total_diversions Variable representing total amount of water diverted
#' @param stranded Variable representing stranding rate per watershed
#' @param weeks_flooded Variable representing total weeks flooded per watershed
#' @param ..surv_juv_rear_int  Intercept, source: calibration (varies by tributary)
#' @param .avg_temp_thresh Coefficient for \code{avg_temp_thresh} variable
#' @param .high_predation Coefficient for \code{high_predation} variable
#' @param .surv_juv_rear_contact_points Coefficient for \code{contact_points} variable
#' @param ..surv_juv_rear_contact_points Calibrated coefficient for \code{contact_points} variable
#' @param .surv_juv_rear_prop_diversions Coefficient for \code{prop_diversions} variable
#' @param ..surv_juv_rear_prop_diversions Calibrated coefficient for \code{prop_diversions} variable
#' @param .surv_juv_rear_total_diversions Coefficient for \code{total_diversions} variable
#' @param ..surv_juv_rear_total_diversions Calibrated coefficient for \code{total_diversions} variable
#' @param .stranded Coefficient for \code{stranded} variable
#' @param .medium Size related intercept for medium sized fish
#' @param .large Size related intercept for large sized fish
#' @param .floodplain Additional intercept for floodplain rearing benefit
#' @param min_survival_rate Estimated survival rate if temperature threshold is exceeded
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
surv_juv_rear <- function(max_temp_thresh, avg_temp_thresh, high_predation,
                          contact_points, prop_diversions, total_diversions,
                          stranded, weeks_flooded,
                          ..surv_juv_rear_int = fallRunDSM::params$..surv_juv_rear_int,
                          .avg_temp_thresh = fallRunDSM::params$.surv_juv_rear_avg_temp_thresh,
                          .high_predation = fallRunDSM::params$.surv_juv_rear_high_predation,
                          .surv_juv_rear_contact_points = fallRunDSM::params$.surv_juv_rear_contact_points,
                          ..surv_juv_rear_contact_points = fallRunDSM::params$..surv_juv_rear_contact_points,
                          .surv_juv_rear_prop_diversions = fallRunDSM::params$.surv_juv_rear_prop_diversions,
                          ..surv_juv_rear_prop_diversions = fallRunDSM::params$..surv_juv_rear_prop_diversions,
                          .surv_juv_rear_total_diversions = fallRunDSM::params$.surv_juv_rear_total_diversions,
                          ..surv_juv_rear_total_diversions = fallRunDSM::params$..surv_juv_rear_total_diversions,
                          .stranded = fallRunDSM::params$.surv_juv_rear_stranded,
                          .medium = fallRunDSM::params$.surv_juv_rear_medium,
                          .large = fallRunDSM::params$.surv_juv_rear_large,
                          .floodplain = fallRunDSM::params$.surv_juv_rear_floodplain,
                          min_survival_rate = fallRunDSM::params$min_survival_rate,
                          stochastic){
  # determine the proportion of weeks when flooded vs not
  prop_ic <-ifelse(weeks_flooded > 0, (4 - weeks_flooded) / 4, 1)
  prop_fp <- 1 - prop_ic

  base_score_inchannel <- ..surv_juv_rear_int +
    (.avg_temp_thresh * avg_temp_thresh) +
    (.high_predation * high_predation) +
    (.surv_juv_rear_contact_points * ..surv_juv_rear_contact_points * contact_points * high_predation) +
    (.surv_juv_rear_prop_diversions * ..surv_juv_rear_prop_diversions * prop_diversions) +
    (.surv_juv_rear_total_diversions * ..surv_juv_rear_total_diversions * total_diversions) +
    (.stranded * stranded)

  base_score_floodplain <- ..surv_juv_rear_int + .floodplain +
    (.avg_temp_thresh  * avg_temp_thresh) + (.high_predation * high_predation)

  if (stochastic) {
    s1 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_inchannel))
    m1 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_inchannel + .medium))
    l1 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_inchannel + .large))
    s2 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_floodplain)) ^ prop_fp
    m2 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_floodplain + .medium)) ^ prop_fp
    l2 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_floodplain + .large)) ^ prop_fp
  } else {
    s1 <- (boot::inv.logit(base_score_inchannel) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
    m1 <- (boot::inv.logit(base_score_inchannel + .medium) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
    l1 <- (boot::inv.logit(base_score_inchannel + .large) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
    s2 <- ((boot::inv.logit(base_score_floodplain) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)) ^ prop_fp
    m2 <- ((boot::inv.logit(base_score_floodplain + .medium) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)) ^ prop_fp
    l2 <- ((boot::inv.logit(base_score_floodplain + .large) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)) ^ prop_fp
  }

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
#' @details See \code{\link{params}} for details on parameter sources
#' @param max_temp_thresh Variable representing the probability of exceeding the max temp threshold
#' @param avg_temp_thresh Variable representing the probability of exceeding the average temperature
#' @param high_predation Variable representing an indicator for high predation in watershed
#' @param ..surv_juv_bypass_int Intercept, source: calibration
#' @param .avg_temp_thresh Coefficient for \code{avg_temp_thresh} variable
#' @param .high_predation Coefficient for \code{high_predation} variable
#' @param .medium Size related intercept for medium sized fish
#' @param .large Size related intercept for large sized fish
#' @param .floodplain Additional intercept for floodplain rearing benefit
#' @param min_survival_rate Estimated survival rate if temperature threshold is exceeded
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
surv_juv_bypass <- function(max_temp_thresh, avg_temp_thresh, high_predation,
                            ..surv_juv_bypass_int = fallRunDSM::params$..surv_juv_bypass_int,
                            .avg_temp_thresh = fallRunDSM::params$.surv_juv_bypass_avg_temp_thresh,
                            .high_predation = fallRunDSM::params$.surv_juv_bypass_high_predation,
                            .medium = fallRunDSM::params$.surv_juv_bypass_medium,
                            .large = fallRunDSM::params$.surv_juv_bypass_large,
                            .floodplain = fallRunDSM::params$.surv_juv_bypass_floodplain,
                            min_survival_rate = fallRunDSM::params$min_survival_rate,
                            stochastic){

  base_score <- ..surv_juv_bypass_int + .floodplain +
    .avg_temp_thresh * avg_temp_thresh +
    .high_predation * high_predation

  if (stochastic) {
    s <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score))
    m <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score + .medium))
    l <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score + .large))
  } else {
    s <- (boot::inv.logit(base_score) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
    m <- (boot::inv.logit(base_score + .medium) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
    l <- (boot::inv.logit(base_score + .large) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
  }

  cbind(s = s, m = m, l = l, vl = 1)
}

#' @title Juvenile Delta Survival
#' @description Calculates the juvenile rearing survival in the deltas
#' @details See \code{\link{params}} for details on parameter sources
#' @param avg_temp Variable representing average temperature in the delta
#' @param max_temp_thresh Variable representing the probability of exceeding the max temperature
#' @param avg_temp_thresh Variable representing the probability of exceeding the average temperature
#' @param high_predation Variable representing an indicator for high predation in delta
#' @param contact_points Variable representing the number of contact points in watershed
#' @param prop_diverted Variable representing the proportion of water diverted
#' @param total_diverted Variable representing the total diversions
#' @param ..surv_juv_delta_int intercept, source: calibration
#' @param .avg_temp_thresh Coefficient for \code{avg_temp_thresh} variable
#' @param .high_predation Coefficient for \code{high_predation} variable
#' @param .surv_juv_delta_contact_points Coefficient for \code{contact_points} variable
#' @param ..surv_juv_delta_contact_points Calibrated coefficient for \code{contact_points} variable
#' @param .prop_diverted Coefficient for \code{prop_diversions} variable
#' @param .surv_juv_delta_total_diverted Coefficient for \code{total_diversions} variable
#' @param ..surv_juv_delta_total_diverted Calibrated coefficient for \code{total_diversions} variable
#' @param .medium Size related intercept for medium sized fish
#' @param .large Size related intercept for large sized fish
#' @param min_survival_rate Estimated survival rate if temperature threshold is exceeded
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
surv_juv_delta <- function(avg_temp, max_temp_thresh, avg_temp_thresh, high_predation, contact_points,
                           prop_diverted, total_diverted,
                           ..surv_juv_delta_int = fallRunDSM::params$..surv_juv_delta_int,
                           .avg_temp_thresh = fallRunDSM::params$.surv_juv_delta_avg_temp_thresh,
                           .high_predation = fallRunDSM::params$.surv_juv_delta_high_predation,
                           .surv_juv_delta_contact_points = fallRunDSM::params$.surv_juv_delta_contact_points,
                           ..surv_juv_delta_contact_points = fallRunDSM::params$..surv_juv_delta_contact_points,
                           .prop_diverted = fallRunDSM::params$.surv_juv_delta_prop_diverted,
                           .surv_juv_delta_total_diverted = fallRunDSM::params$.surv_juv_delta_total_diverted,
                           ..surv_juv_delta_total_diverted = fallRunDSM::params$..surv_juv_delta_total_diverted,
                           .medium = fallRunDSM::params$.surv_juv_delta_medium,
                           .large =  fallRunDSM::params$.surv_juv_delta_large,
                           min_survival_rate = fallRunDSM::params$min_survival_rate,
                           stochastic){
  # north delta
  north_delta_surv <- c(rep((avg_temp <= 16.5)*.42 + (avg_temp > 16.5 & avg_temp < 19.5) * 0.42 /
                              (1.55^(avg_temp-15.5)) + (avg_temp > 19.5 & avg_temp < 25)*0.035,3), 1)

  # south delta
  base_score <- ..surv_juv_delta_int +
    .avg_temp_thresh * avg_temp_thresh[2] +
    .high_predation * high_predation[2] +
    .surv_juv_delta_contact_points * ..surv_juv_delta_contact_points * contact_points[2] * high_predation[2] +
    .prop_diverted * prop_diverted[2] +
    .surv_juv_delta_total_diverted * ..surv_juv_delta_total_diverted * total_diverted[2]

  if (stochastic) {
    s <- ifelse(max_temp_thresh[2], min_survival_rate, boot::inv.logit(base_score))
    m <- ifelse(max_temp_thresh[2], min_survival_rate, boot::inv.logit(base_score + .medium))
    l <- ifelse(max_temp_thresh[2], min_survival_rate, boot::inv.logit(base_score + .large))
  } else {
    s <- (boot::inv.logit(base_score) * (1 - max_temp_thresh[2])) + (min_survival_rate * max_temp_thresh[2])
    m <- (boot::inv.logit(base_score + .medium) * (1 - max_temp_thresh[2])) + (min_survival_rate * max_temp_thresh[2])
    l <- (boot::inv.logit(base_score + .large) * (1 - max_temp_thresh[2])) + (min_survival_rate * max_temp_thresh[2])
  }


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
#' @param survival_adjustment Survival adjustment applied to survival rates based on SIT strategies
#' @param mode The mode that the model is being run in
#' @param avg_temp The 1980-2000 the monthly mean water temperature in °C. More details at \code{\link[DSMtemperature]{stream_temperature}}
#' @param avg_temp_delta The 1980-2000 rearing temperature in the North and South Delta in °C. More details at \code{\link[DSMtemperature]{delta_temperature}}
#' @param prob_strand_early Experts estimated probabilities for predation, stranding, and nest scouring. More details at \code{\link[DSMhabitat]{prob_strand_early}}
#' @param prob_strand_late Experts estimated probabilities for predation, stranding, and nest scouring. More details at \code{\link[DSMhabitat]{prob_strand_late}}
#' @param proportion_diverted A dataset containing the proportion of flow diverted within watersheds for use with the CVPIA SIT Salmon Population Model. More details at \code{\link[DSMflow]{proportion_diverted}}
#' @param total_diverted A dataset containing the diverted flow in cms within watersheds for use with the CVPIA SIT Salmon Population Model. More details at \code{\link[DSMflow]{total_diverted}}
#' @param delta_proportion_diverted The proportion of delta inflow diverted from 1980-2000. More details at \code{\link[DSMflow]{delta_proportion_diverted}}
#' @param delta_total_diverted The total diverted of delta inflow in cubic meters per second from 1980-2000.. More details at \code{\link[DSMflow]{delta_total_diverted}}
#' @param weeks_flooded The 1980-2000 floodplain rearing habitat event duration in number of weeks. More details at \code{\link[DSMhabitat]{weeks_flooded}}
#' @param prop_high_predation Experts estimated probabilities for predation, stranding, and nest scouring. More details at \code{\link[DSMhabitat]{prop_high_predation}}
#' @param contact_points Number of contact points, estimated using PAD Contact points were derived from the Passage Assessment Database (PAD) maintained by California Department of Fish and Wildlife. Each location considered in the model (e.g., tributary, Sacramento reach, and delta subdivisions) was assessed for all structures identified in the PAD. More details at \code{\link[DSMhabitat]{contact_points}}
#' @param delta_contact_points Number of contact points, estimated using PAD Contact points were derived from the Passage Assessment Database (PAD) maintained by California Department of Fish and Wildlife. Each location considered in the model (e.g., tributary, Sacramento reach, and delta subdivisions) was assessed for all structures identified in the PAD. More details at \code{\link[DSMhabitat]{delta_contact_points}}
#' @param delta_prop_high_predation Expert estimated high predation probabilities. More details at \code{\link[DSMhabitat]{delta_prop_high_predation}}
#' @param ..surv_juv_rear_int Intercept for \code{\link{surv_juv_rear}}
#' @param .surv_juv_rear_contact_points Coefficient for \code{\link{surv_juv_rear}} \code{contact_points} variable
#' @param ..surv_juv_rear_contact_points Calibrated coefficient for \code{\link{surv_juv_rear}} \code{contact_points} variable
#' @param .surv_juv_rear_prop_diversions Coefficient for \code{\link{surv_juv_rear}} \code{prop_diversions} variable
#' @param ..surv_juv_rear_prop_diversions Calibrated coefficient for \code{\link{surv_juv_rear}} \code{prop_diversions} variable
#' @param .surv_juv_rear_total_diversions Coefficient for \code{\link{surv_juv_rear}} \code{total_diversions} variable
#' @param ..surv_juv_rear_total_diversions Calibrated coefficient for \code{\link{surv_juv_rear}} \code{total_diversions} variable
#' @param ..surv_juv_bypass_int Intercept for \code{\link{surv_juv_bypass}}
#' @param ..surv_juv_delta_int Intercept for \code{\link{surv_juv_delta}}
#' @param .surv_juv_delta_contact_points Coefficient for \code{\link{surv_juv_delta}} contact_points variable
#' @param ..surv_juv_delta_contact_points Calibrated coefficient for \code{\link{surv_juv_delta}} contact_points variable
#' @param .surv_juv_delta_total_diverted Coefficient for \code{\link{surv_juv_delta}} total_diversions variable
#' @param ..surv_juv_delta_total_diverted Calibrated coefficient for \code{\link{surv_juv_delta}} total_diversions variable
#' @param .surv_juv_rear_avg_temp_thresh Coefficient for \code{\link{surv_juv_rear}} \code{avg_temp_thresh} variable
#' @param .surv_juv_rear_high_predation Coefficient for \code{\link{surv_juv_rear}} \code{high_predation} variable
#' @param .surv_juv_rear_stranded Coefficient for \code{\link{surv_juv_rear}} \code{stranded} variable
#' @param .surv_juv_rear_medium Size related intercept for \code{\link{surv_juv_rear}} medium sized fish
#' @param .surv_juv_rear_large Size related intercept for \code{\link{surv_juv_rear}} large sized fish
#' @param .surv_juv_rear_floodplain Additional intercept for \code{\link{surv_juv_rear}} floodplain rearing benefit
#' @param .surv_juv_bypass_avg_temp_thresh Coefficient for \code{\link{surv_juv_bypass}} \code{avg_temp_thresh} variable
#' @param .surv_juv_bypass_high_predation Coefficient for \code{\link{surv_juv_bypass}} \code{high_predation} variable
#' @param .surv_juv_bypass_medium Size related intercept for \code{\link{surv_juv_bypass}} medium sized fish
#' @param .surv_juv_bypass_large Size related intercept for \code{\link{surv_juv_bypass}} large sized fish
#' @param .surv_juv_bypass_floodplain Additional intercept for \code{\link{surv_juv_bypass}} floodplain rearing benefit
#' @param .surv_juv_delta_avg_temp_thresh Coefficient for \code{\link{surv_juv_delta}} \code{avg_temp_thresh} variable
#' @param .surv_juv_delta_high_predation Coefficient for \code{\link{surv_juv_delta}} \code{high_predation} variable
#' @param .surv_juv_delta_prop_diverted Coefficient for \code{\link{surv_juv_delta}} \code{prop_diversions} variable
#' @param .surv_juv_delta_medium Size related intercept for \code{\link{surv_juv_delta}} medium sized fish
#' @param .surv_juv_delta_large Size related intercept for \code{\link{surv_juv_delta}} large sized fish
#' @param min_survival_rate Estimated survival rate if temperature threshold is exceeded
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
get_rearing_survival <- function(year, month,
                                 survival_adjustment,
                                 mode,
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
                                 .surv_juv_rear_contact_points,
                                 ..surv_juv_rear_contact_points,
                                 .surv_juv_rear_prop_diversions,
                                 ..surv_juv_rear_prop_diversions,
                                 .surv_juv_rear_total_diversions,
                                 ..surv_juv_rear_total_diversions,
                                 ..surv_juv_bypass_int,
                                 ..surv_juv_delta_int,
                                 .surv_juv_delta_contact_points,
                                 ..surv_juv_delta_contact_points,
                                 .surv_juv_delta_total_diverted,
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
                                 .surv_juv_delta_large,
                                 min_survival_rate,
                                 stochastic) {

  aveT20 <- boot::inv.logit(-14.32252 + 0.72102 * avg_temp[ , month , year])
  maxT25 <- boot::inv.logit(-23.1766 + 1.4566 * avg_temp[ , month, year])
  aveT20D <- boot::inv.logit(-18.30017 + 0.96991 * avg_temp_delta[month, year, ])
  maxT25D <- boot::inv.logit(-157.537 + 6.998 * avg_temp_delta[month, year, ])

  if (stochastic) {
    aveT20 <- rbinom(31, 1, aveT20)
    maxT25 <- rbinom(31, 1, maxT25)
    aveT20D <- rbinom(2, 1, aveT20D)
    maxT25D <- rbinom(2, 1, maxT25D)
  }
  # set proportion fish stranding
  prob_ws_strand <- if(month < 4) prob_strand_early else prob_strand_late

  ws_strand <- if (stochastic) {
    rbinom(31, 1, prob_ws_strand)
  } else {
    prob_ws_strand
  }

  # proportion and total water diverted
  proportion_diverted <- proportion_diverted[ , month, year]
  total_diverted <- total_diverted[ , month, year]
  delta_proportion_diverted <- delta_proportion_diverted[month, year, ]
  delta_total_diverted <- delta_total_diverted[ month, year, ]

  # weeks flooded
  weeks_flood <- weeks_flooded[ , month, year]

  # predator information

  high_predation <- if (stochastic) {
    rbinom(31, 1, prop_high_predation)
  } else {
    prop_high_predation
  }

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
                  .surv_juv_rear_contact_points = .surv_juv_rear_contact_points,
                  ..surv_juv_rear_contact_points = ..surv_juv_rear_contact_points,
                  .surv_juv_rear_prop_diversions = .surv_juv_rear_prop_diversions,
                  ..surv_juv_rear_prop_diversions = ..surv_juv_rear_prop_diversions,
                  .surv_juv_rear_total_diversions = .surv_juv_rear_total_diversions,
                  ..surv_juv_rear_total_diversions = ..surv_juv_rear_total_diversions,
                  .avg_temp_thresh = .surv_juv_rear_avg_temp_thresh,
                  .high_predation = .surv_juv_rear_high_predation,
                  .stranded = .surv_juv_rear_stranded,
                  .medium = .surv_juv_rear_medium,
                  .large = .surv_juv_rear_large,
                  .floodplain = .surv_juv_rear_floodplain,
                  min_survival_rate = min_survival_rate,
                  stochastic = stochastic)
  }))


  river_surv <- matrix(unlist(rear_surv[ , 1]), ncol = 4, byrow = TRUE)
  flood_surv <- matrix(unlist(rear_surv[ , 2]), ncol = 4, byrow = TRUE)

  if (mode != "seed") {
    river_surv <- pmin(river_surv * survival_adjustment[, year], 1)
    flood_surv <- pmin(flood_surv * survival_adjustment[, year], 1)
  }

  bp_surv <- surv_juv_bypass(max_temp_thresh = maxT25[22],
                             avg_temp_thresh = aveT20[22],
                             high_predation = 0,
                             ..surv_juv_bypass_int = ..surv_juv_bypass_int,
                             .avg_temp_thresh = .surv_juv_bypass_avg_temp_thresh,
                             .high_predation = .surv_juv_bypass_high_predation,
                             .medium = .surv_juv_bypass_medium,
                             .large = .surv_juv_bypass_large,
                             .floodplain = .surv_juv_bypass_floodplain,
                             min_survival_rate = min_survival_rate,
                             stochastic = stochastic)

  sutter_surv <- bp_surv
  yolo_surv <- bp_surv

  delta_juv_surv <- surv_juv_delta(avg_temp = avg_temp_delta[month, year, "North Delta"],
                                   max_temp_thresh = maxT25D,
                                   avg_temp_thresh = aveT20D,
                                   high_predation = delta_high_predation,
                                   contact_points = delta_num_contact_points,
                                   prop_diverted = delta_proportion_diverted,
                                   total_diverted = delta_total_diverted,
                                   ..surv_juv_delta_int = ..surv_juv_delta_int,
                                   .surv_juv_delta_contact_points = .surv_juv_delta_contact_points,
                                   ..surv_juv_delta_contact_points = ..surv_juv_delta_contact_points,
                                   .surv_juv_delta_total_diverted = .surv_juv_delta_total_diverted,
                                   ..surv_juv_delta_total_diverted = ..surv_juv_delta_total_diverted,
                                   .avg_temp_thresh = .surv_juv_delta_avg_temp_thresh,
                                   .high_predation = .surv_juv_delta_high_predation,
                                   .prop_diverted = .surv_juv_delta_prop_diverted,
                                   .medium = .surv_juv_delta_medium,
                                   .large = .surv_juv_delta_large,
                                   min_survival_rate = min_survival_rate,
                                   stochastic = stochastic)

  return(
    list(
      inchannel = pmin(river_surv, 1),
      floodplain = pmin(flood_surv, 1),
      sutter = pmin(sutter_surv, 1),
      yolo = pmin(yolo_surv, 1),
      delta = pmin(delta_juv_surv, 1))
  )
}

# JUVENILE MIGRATORY SURVIVAL -----
#' @title Juvenile Mainstem Sacramento Outmigration Survival
#' @description Calculates the Mainstem Sacramento juvenile out migration survival
#' @param flow_cms Variable representing upper Sacramento River flow in cubic meters per second
#' @source IP-117068
#' @export
surv_juv_outmigration_sac <- function(flow_cms){

  result <- rep((flow_cms <= 122) * 0.03 + (flow_cms > 122 & flow_cms <= 303) * 0.189 + (flow_cms > 303) * 0.508, 4)
  setNames(result, fallRunDSM::size_class_labels)
}


#' @title Juvenile San Joaquin Outmigration Survival
#' @description Calculates the San Joaquin River juvenile out migration survival
#' @details See \code{\link{params}} for details on parameter sources
#' @param ..surv_juv_outmigration_sj_int Intercept
#' @param .medium Size related intercept for medium sized fish
#' @param .large Size related intercept for large sized fish
#' @source IP-117068
#' @export
surv_juv_outmigration_san_joaquin <- function(..surv_juv_outmigration_sj_int = fallRunDSM::params$..surv_juv_outmigration_sj_int,
                                              .medium = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_medium,
                                              .large = fallRunDSM::params$.surv_juv_outmigration_san_joaquin_large){

  s <- boot::inv.logit(..surv_juv_outmigration_sj_int)
  m <- boot::inv.logit(..surv_juv_outmigration_sj_int + .medium)
  l <- vl <- boot::inv.logit(..surv_juv_outmigration_sj_int + .large)

  cbind(s = s, m = m, l = l, vl = vl)
}


#' @title Juvenile Delta Outmigration Survival
#' @description Calculates the North and South Delta juvenile out migration survival
#' @param prop_DCC_closed Proportion of days the Delta Cross Channel Gates are closed
#' @param hor_barr Indicator if head of old river physical barrier in place
#' @param freeport_flow Average daily discharge at Freeport in cubic meters per second
#' @param vernalis_flow Average daily discharge at Vernalis in cubic meters per second
#' @param stockton_flow Average daily discharge at Stockton in cubic meters per second
#' @param vernalis_temperature Average daily temperature at Vernalis in °C
#' @param prisoners_point_temperature Average daily temperature of the San Joaquin River at Prisoners Point °C
#' @param CVP_exp Average daily exports Central Valley Project in cubic meters per second
#' @param SWP_exp Average daily exports State Water Project in cubic meters per second
#' @param trap_trans Proportion of smolts trapped at Vernalis and transported to Chips island
#' @section Parameters:
#' All parameters were derived from Perry et al. (2018)
#' @details Function returns proportion of fish from the Sacramento at Feeeport (northern_fish)
#' Mokelumne and Cosumnes (cosumnes_mokelumne_fish), Calaveras (calaveras_fish) and
#' San Joaquin tributaries from Vernalis (southern_fish) arriving alive at Chipps
#' Island in four size groups (35-42mm, 42-72mm, 72-110mm, >110mm).
#' Note that the models were fit to data that were >80 mm. Therefore, this does not
#' predict outside of the data so sizes <= 80mm are assumed to me 80mm long as
#' requested by Russ Perry.
#' @source IP-117068
#' @export
surv_juv_outmigration_delta <- function(prop_DCC_closed, hor_barr, freeport_flow,
                                        vernalis_flow, stockton_flow,
                                        vernalis_temperature,
                                        prisoners_point_temperature, CVP_exp,
                                        SWP_exp, trap_trans){

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
  colnames(survival_rates) <- fallRunDSM::size_class_labels
  return(survival_rates)

}

#' @title Get Migratory Survival Rates
#' @description Calculates the juvenile out migration survival rates in all
#' regions for a month and year of the simulation
#' @param year The simulation year, 1-20
#' @param month The simulation month, 1-8
#' @param cc_gates_prop_days_closed The number of days and proportion of days the Delta Cross Channel gates are typically closed for each month. More details at \code{\link[DSMflow]{delta_cross_channel_closed}}
#' @param freeport_flows Flow in cms at Freeport (CALSIM node C400). More details at \code{\link[DSMflow]{freeport_flow}}
#' @param vernalis_flows Flow in cms at Vernalis (CALSIM node C639). More details at \code{\link[DSMflow]{vernalis_flow}}
#' @param stockton_flows Flow in cms at Stockton (CALSIM node C417A). More details at \code{\link[DSMflow]{stockton_flow}}
#' @param vernalis_temps Monthly water temperature (°C) at Vernalis from 1980-2000. More details at \code{\link[DSMtemperature]{vernalis_temperature}}
#' @param prisoners_point_temps Monthly water temperature (°C) at Prisoner's Point from 1980-2000. More details at \code{\link[DSMtemperature]{prisoners_point_temperature}}
#' @param CVP_exports Total exports for CVP in cms. Value is obtained using CALSIM variable DEL_CVP_EXP. More details at \code{\link[DSMflow]{cvp_exports}}
#' @param SWP_exports Total exports for SWP in cms. Value is obtained using CALSIM variable DEL_SWP_EXP. More details at \code{\link[DSMflow]{swp_exports}}
#' @param upper_sacramento_flows Average monthly flows on the Upper Sacramento River in cubic meters per second, more details at \code{\link[DSMflow]{upper_sacramento_flows}}
#' @param delta_inflow Variable describing delta inflow in cubic meters per second, more details at \code{\link[DSMflow]{delta_inflow}}
#' @param avg_temp_delta Variable describing monthly mean temperature in celsius, more details at  \code{\link[DSMtemperature]{delta_temperature}}
#' @param avg_temp Variable describing monthly mean temperature in celsius, more details at  \code{\link[DSMtemperature]{stream_temperature}}
#' @param delta_proportion_diverted Variable describing diversions from the delta in cubic meters per second, more details at \code{\link[DSMflow]{delta_proportion_diverted}}
#' @param ..surv_juv_outmigration_sj_int Intercept for \code{\link{surv_juv_outmigration_san_joaquin}}
#' @param .surv_juv_outmigration_san_joaquin_medium Size related intercept for \code{\link{surv_juv_outmigration_san_joaquin}} medium sized fish
#' @param .surv_juv_outmigration_san_joaquin_large Size related intercept for \code{\link{surv_juv_outmigration_san_joaquin}} large sized fish
#' @param min_survival_rate Estimated survival rate if temperature threshold is exceeded
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export
get_migratory_survival <- function(year, month,
                                   cc_gates_prop_days_closed,
                                   freeport_flows,
                                   vernalis_flows,
                                   stockton_flows,
                                   vernalis_temps,
                                   prisoners_point_temps,
                                   CVP_exports,
                                   SWP_exports,
                                   upper_sacramento_flows,
                                   delta_inflow,
                                   avg_temp_delta,
                                   avg_temp,
                                   delta_proportion_diverted,
                                   ..surv_juv_outmigration_sj_int,
                                   .surv_juv_outmigration_san_joaquin_medium,
                                   .surv_juv_outmigration_san_joaquin_large,
                                   min_survival_rate,
                                   stochastic) {

  aveT20 <- boot::inv.logit(-14.32252 + 0.72102 * avg_temp[ , month , year])
  maxT25 <- boot::inv.logit(-23.1766 + 1.4566 * avg_temp[ , month, year])

  if (stochastic) {
    aveT20 <- rbinom(31, 1, aveT20)
    maxT25 <- rbinom(31, 1, maxT25)
  }

  u_sac_flow <- upper_sacramento_flows[month, year]

  uppermid_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow)

  lowermid_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow)

  lower_sac_migration_surv <- surv_juv_outmigration_sac(flow_cms = u_sac_flow)

  bp_surv <- sqrt(surv_juv_bypass(max_temp_thresh = maxT25[22],
                                  avg_temp_thresh = aveT20[22],
                                  high_predation = 0,
                                  min_survival_rate = min_survival_rate,
                                  stochastic = stochastic))

  sj_migration_surv <- surv_juv_outmigration_san_joaquin(..surv_juv_outmigration_sj_int = ..surv_juv_outmigration_sj_int,
                                                         .medium = .surv_juv_outmigration_san_joaquin_medium,
                                                         .large = .surv_juv_outmigration_san_joaquin_large)

  delta_survival <- surv_juv_outmigration_delta(prop_DCC_closed = cc_gates_prop_days_closed[month],
                                                hor_barr = 0,
                                                freeport_flow = freeport_flows[month, year],
                                                vernalis_flow = vernalis_flows[month, year],
                                                stockton_flow = stockton_flows[month, year],
                                                vernalis_temperature = vernalis_temps[month, year],
                                                prisoners_point_temperature = prisoners_point_temps[month, year],
                                                CVP_exp = CVP_exports[month, year],
                                                SWP_exp = SWP_exports[month, year],
                                                trap_trans = 0) # newDsurv

  bay_delta_migration_surv <- mean(c(0.43, 0.46, 0.26, 0.25, 0.39)) # Bay.S Chipps island to bay


  return(
    list(
      uppermid_sac = pmin(uppermid_sac_migration_surv, 1),
      lowermid_sac = pmin(lowermid_sac_migration_surv, 1),
      lower_sac = pmin(lower_sac_migration_surv, 1),
      sutter = pmin(bp_surv, 1),
      yolo = pmin(bp_surv, 1),
      san_joaquin = pmin(sj_migration_surv, 1),
      delta = pmin(delta_survival, 1),
      bay_delta = pmin(bay_delta_migration_surv, 1)
    ))
}
