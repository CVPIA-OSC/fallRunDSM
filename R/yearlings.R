#'@title Spring-Run Yearling Growth
#' @description Generates transition probability matrices for yearling growth
#' @details See \code{\link{params}} for details on parameter sources
#' @param year Current simulation year
#' @param yearlings Yearlings in tributaries
#' @param yearling_territory_size Array of juvenile fish territory requirements
#' @param scenario The current scenario
#' @param inchannel_habitat_fry 3 dimensional array [watersheds, months, years] representing fry inchannel habitat in square meters
#' @param inchannel_habitat_juvenile 3 dimensional array [watersheds, months, years] representing juvenile inchannel habitat in square meters
#' @param floodplain_habitat 3 dimensional array [watersheds, months, years] representing floodplain habitat in square meters
#' @param sutter_habitat 2 dimensional array [months, years] representing sutter bypass habitat in square meters
#' @param yolo_habitat 2 dimensional array [months, years] representing yolo bypass habitat in square meters
#' @param north_delta_habitat 2 dimensional array [months, years] representing north delta habitat in square meters
#' @param south_delta_habitat 2 dimensional array [months, years] representing south delta habitat in square meters
#' @param avg_temp More details at \code{\link[DSMtemperature]{stream_tempetature}}
#' @param avg_temp_delta More details at \code{\link[DSMtempetature]{delta_temprature}}
#' @param prob_strand_early More details at \code{\link[DSMhabitat]{prop_strand_early}}
#' @param prob_strand_late More details at \code{\link[DSMhabitat]{prop_strand_late}}
#' @param proportion_diverted More details at \code{\link[DSMflow]{proportion_diverted}}
#' @param total_diverted More details at \code{\link[DSMflow]{total_diverted}}
#' @param delta_proportion_diverted More details at \code{\link[DSMflow]{delta_proportion_diverted}}
#' @param delta_total_diverted More details at \code{\link[DSMflow]{delta_total_diverted}}
#' @param weeks_flooded More details at \code{\link[DSMflow]{weeks_flooded}}
#' @param prop_high_predation More details at \code{\link[DSMhabitat]{prop_high_predation}}
#' @param contact_points More details at \code{\link[DSMhabitat]{contact_points}}
#' @param delta_contact_points More details at \code{\link[DSMhabitat]{delta_contact_points}}
#' @param delta_prop_high_predation More details at \code{\link[DSMhabitat]{delta_prop_high_predation}}
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
#' @param min_survival_rate estimated survival rate if temperature threshold is exceeded
#' @export
yearling_growth <- function(year,
                            month,
                            yearlings,
                            yearling_territory_size = springRunDSM::params$yearling_territory_size,
                            inchannel_habitat_fry,
                            inchannel_habitat_juvenile,
                            floodplain_habitat,
                            sutter_habitat,
                            yolo_habitat,
                            delta_habitat,
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
                            min_survival_rate) {

  growth_rates <- diag(1, nrow = 4, ncol = 4)
  growth_rates_floodplain <- replicate(4, diag(1, 4, 4))

  for (month in 5:10) {
    # only months 9, 10 experience growth
    if (month %in% 9:10) {
      growth_rates <- springRunDSM::params$growth_rates
      growth_rates_floodplain <- springRunDSM::params$growth_rates_floodplain
    }
    this_weeks_flooded <- weeks_flooded[, month, year]

    # we only care for floodplain and inchannel
    habitat <- get_habitat(year, month,
                           inchannel_habitat_fry,
                           inchannel_habitat_juvenile,
                           floodplain_habitat,
                           sutter_habitat,
                           yolo_habitat,
                           delta_habitat)

    # we only care for floodplain and inchannel
    survival_rates <- get_rearing_survival(year,
                                           month,
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
                                           min_survival_rate)

    ic_rearing <- matrix(0, nrow = nrow(yearlings), ncol = 4)
    fp_rearing <- matrix(0, nrow = nrow(yearlings), ncol = 4)
    floodplain_remaining <- habitat$floodplain

    for (w in 1:nrow(yearlings)) {
      for (s in 4:1) {
        # either the max the number of fish that fit or the total number of fish
        fp_rearing[w, s] <- min(floodplain_remaining[w]/yearling_territory_size[s], yearlings[w, s])
        # update floodplain remaining
        floodplain_remaining[w] <-
          max(floodplain_remaining[w] - fp_rearing[w, s]*yearling_territory_size[s], 0)
      }
    }

    fp_rearing <- round(pmax(fp_rearing, 0)) # everything must be greater than 0
    yearlings <- pmax(yearlings - fp_rearing, 0) # remove from yearlings and set min 0

    inchannel_remaining <- habitat$inchannel

    for (w in 1:nrow(yearlings)) {
      for (s in 4:1) {
        # either the max the number of fish that fit or the total number of fish
        ic_rearing[w, s] <- min(inchannel_remaining[w]/yearling_territory_size[s], yearlings[w, s])
        # update inchannel remaining
        inchannel_remaining[w] <-
          max(inchannel_remaining[w] - ic_rearing[w, s]*yearling_territory_size[s], 0)
      }
    }

    ic_rearing <- round(pmax(ic_rearing, 0))
    yearlings <- pmax(yearlings - ic_rearing, 0)

    # apply habitat specific mortality
    fp_rearing <- t(sapply(1:nrow(fp_rearing), function(i) {
      rbinom(4, size=fp_rearing[i, ], prob = survival_rates$floodplain[i, ])
    }))

    ic_rearing <- t(sapply(1:nrow(ic_rearing), function(i) {
      rbinom(4, size=ic_rearing[i, ], prob = survival_rates$inchannel[i, ])
    }))

    # apply growth
    ic_rearing <- round(ic_rearing %*% growth_rates)


    fp_rearing <- t(sapply(1:length(this_weeks_flooded), function(i) {
      if (this_weeks_flooded[i] == 0)
        fp_rearing[i, ]
      else
        fp_rearing[i, ] %*% growth_rates_floodplain[,,this_weeks_flooded[i]]
    }))

    yearlings <- round(fp_rearing + ic_rearing)

  }

  return(yearlings)
}
