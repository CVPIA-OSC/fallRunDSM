#' @title Ocean Entry Success
#' @description Calculates the number of juveniles that survive entering the ocean
#' @param migrants Variable representing the number of juveniles at golden gate bridge
#' @param month Variable representing the current simulation month
#' @param avg_ocean_transition_month Variable representing the average month juveniles transition to the ocean
#' @param length Variable representing the fork lengths for each size classes. \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @param ..ocean_entry_success_int Intercept, source: Calibration (Varies by tributary )
#' @param .month Coefficient for month variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @source IP-117068
#' @export
ocean_entry_success <- function(migrants, month, avg_ocean_transition_month,
                                length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
                                ..ocean_entry_success_int = c(
                                  `intercept Upper Sacramento River` = -0.5108849,
                                  `intercept Antelope Creek` = 1.2,
                                  `intercept Battle Creek` = 1.2,
                                  `intercept Bear Creek` = 1.2,
                                  `intercept Big Chico Creek` = 1.2,
                                  `intercept Butte Creek` = -3.3233638,
                                  `intercept Clear Creek` = 1.2,
                                  `intercept Cottonwood Creek` = 1.2,
                                  `intercept Cow Creek` = 1.2,
                                  `intercept Deer Creek` = -3.2304288,
                                  `intercept Elder Creek` = 1.2,
                                  `intercept Mill Creek` = -3.4148335,
                                  `intercept Paynes Creek` = 1.2,
                                  `intercept Stony Creek` = 1.2,
                                  `intercept Thomes Creek` = 1.2,
                                  `intercept Upper-mid Sacramento River` = 1.2,
                                  `intercept Sutter Bypass` = 1.2,
                                  `intercept Bear River` = -3.5,
                                  `intercept Feather River` = -3.5,
                                  `intercept Yuba River` = -3.5,
                                  `intercept Lower-mid Sacramento River` = 1.2,
                                  `intercept Yolo Bypass` = 1.2,
                                  `intercept American River` = -1.308341,
                                  `intercept Lower Sacramento River` = 1.2,
                                  `intercept Calaveras River` = -1.9841364,
                                  `intercept Cosumnes River` = -1.9841364,
                                  `intercept Mokelumne River` = 2.5000007,
                                  `intercept Merced River` = -3.5,
                                  `intercept Stanislaus River` = -3,
                                  `intercept Tuolumne River` = -0.9,
                                  `intercept San Joaquin River` = 1.2),
                                .months = 0.35){

  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(..ocean_entry_success_int[[i]] + .months * month_since + length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}

