#' @title Juvenile Pulse Movement
#' @description Calculates the proportion of juveniles in each size class that
#' migrate due to pulse flow
#' @param proportion_pulse The proportion of flow that is a pulse, estimated
#' by the standard deviation of flow divided by the median flow for each month
#' @param betas parameters estimated through calibration
#' @section Parameters:
#' All parameters were derived from Empirical model fit to CVPIA Chinook salmon screw
#' trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River,
#' Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @source IP-117068
#' @export
pulse_movement <- function(proportion_pulse,
                           betas = c(intercept = -7.70744, pulse = 0.26579,
                                     medium = 1.66845, large = 0.5706, vlarge = -4.305,
                                     `medium pulse` = -0.25477, `large pulse` = -0.44778,
                                     `very large pulse` = 0.329)){

  s <- boot::inv.logit(betas[1] + betas[2] * proportion_pulse)

  m <- boot::inv.logit(betas[1] + betas[3] + betas[2] * proportion_pulse + betas[6] * proportion_pulse)

  l <- boot::inv.logit(betas[1] + betas[4] + betas[2] * proportion_pulse + betas[7] * proportion_pulse)

  vl <- boot::inv.logit(betas[1] + betas[5] + betas[2] * proportion_pulse + betas[8] * proportion_pulse)


  cbind(s = s, m = m, l = l, vl = vl)
}
