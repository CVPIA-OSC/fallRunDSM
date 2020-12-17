#' @title Ocean Entry Success
#' @description Calculates the number of juveniles that survive entering the ocean
#' @param migrants The number of juveniles at golden gate bridge
#' @param month The current simulation month
#' @param avg_ocean_transition_month The average month juveniles transition to the ocean
#' @param length Fork lengths for each size classes. \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @param betas Parameters estimated through calibration
#' @section Parameters:
#' Parameters from the model are obtained from either literature, calibration, export elicitation,
#' and meta-analysis. The source for each parameter in this function are detailed below.
#' \itemize{
#' \item intercept 1-31: calibration estimate; varies by tributary
#' \item months: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' }
#' @source IP-117068
#' @export
ocean_entry_success <- function(migrants, month, avg_ocean_transition_month,
                                length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
                                betas = c(`intercept 1` = -0.5108849, `intercept 2` = 1.2, `intercept 3` = 1.2,
                                          `intercept 4` = 1.2, `intercept 5` = 1.2, `intercept 6` = -3.3233638,
                                          `intercept 7` = 1.2, `intercept 8` = 1.2, `intercept 9` = 1.2,
                                          `intercept 10` = -3.2304288, `intercept 11` = 1.2, `intercept 12` = -3.4148335,
                                          `intercept 13` = 1.2, `intercept 14` = 1.2, `intercept 15` = 1.2,
                                          `intercept 16` = 1.2, `intercept 17` = 1.2, `intercept 18` = -3.5,
                                          `intercept 19` = -3.5, `intercept 20` = -3.5, `intercept 21` = 1.2,
                                          `intercept 22` = 1.2, `intercept 23` = -1.308341, `intercept 24` = 1.2,
                                          `intercept 25` = -1.9841364, `intercept 26` = -1.9841364, `intercept 27` = 2.5000007,
                                          `intercept 28` = -3.5, `intercept 29` = -3, `intercept 30` = -0.9,
                                          `intercept 31` = 1.2, months = 0.35)){

  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(betas[i] + betas[32] * month_since + length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}

