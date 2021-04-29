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
                                  `Upper Sacramento River` = -0.5108849,
                                  `Antelope Creek` = 1.2,
                                  `Battle Creek` = 1.2,
                                  `Bear Creek` = 1.2,
                                  `Big Chico Creek` = 1.2,
                                  `Butte Creek` = -3.3233638,
                                  `Clear Creek` = 1.2,
                                  `Cottonwood Creek` = 1.2,
                                  `Cow Creek` = 1.2,
                                  `Deer Creek` = -3.2304288,
                                  `Elder Creek` = 1.2,
                                  `Mill Creek` = -3.4148335,
                                  `Paynes Creek` = 1.2,
                                  `Stony Creek` = 1.2,
                                  `Thomes Creek` = 1.2,
                                  `Upper-mid Sacramento River` = 1.2,
                                  `Sutter Bypass` = 1.2,
                                  `Bear River` = -3.5,
                                  `Feather River` = -3.5,
                                  `Yuba River` = -3.5,
                                  `Lower-mid Sacramento River` = 1.2,
                                  `Yolo Bypass` = 1.2,
                                  `American River` = -1.308341,
                                  `Lower Sacramento River` = 1.2,
                                  `Calaveras River` = -1.9841364,
                                  `Cosumnes River` = -1.9841364,
                                  `Mokelumne River` = 2.5000007,
                                  `Merced River` = -3.5,
                                  `Stanislaus River` = -3,
                                  `Tuolumne River` = -0.9,
                                  `San Joaquin River` = 1.2),
                                .months = 0.35){

  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(..ocean_entry_success_int[[i]] + .months * month_since + length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}

