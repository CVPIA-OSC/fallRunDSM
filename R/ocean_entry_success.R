#' @title Ocean Entry Success
#' @description Calculates the number of juveniles that survive entering the ocean
#' @param migrants Variable representing the number of juveniles at golden gate bridge
#' @param month Variable representing the current simulation month
#' @param avg_ocean_transition_month Variable representing the average month juveniles transition to the ocean
#' @param .ocean_entry_success_length Variable representing the fork lengths for each size classes
#' @param ..ocean_entry_success_int Intercept
#' @param .ocean_entry_success_months Coefficient for month variable
#' @source IP-117068
#' @export
ocean_entry_success <- function(migrants, month, avg_ocean_transition_month,
                                .ocean_entry_success_length = fallRunDSM::params$.ocean_entry_success_length,
                                ..ocean_entry_success_int = fallRunDSM::params$..ocean_entry_success_int,
                                .ocean_entry_success_months = fallRunDSM::params$.ocean_entry_success_months){

  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(..ocean_entry_success_int[[i]] +
                                           .ocean_entry_success_months * month_since +
                                           .ocean_entry_success_length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}

