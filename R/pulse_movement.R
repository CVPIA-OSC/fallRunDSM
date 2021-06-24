#' @title Juvenile Pulse Movement
#' @description Calculates the proportion of juveniles in each size class that
#' migrate due to pulse flow
#' @param proportion_pulse The proportion of flow that is a pulse, estimated
#' by the standard deviation of flow divided by the median flow for each month
#' @param .intercept Intercept, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .proportion_pulse Coefficient for the proportion_pulse variable, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .medium parameter for medium sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .large parameter for large sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .vlarge parameter for very large sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .medium_pulse Additional coefficient for proportion_pulse variable for medium size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .large_pulse Additional coefficient for proportion_pulse variable for large size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @param .very_large_pulse Additional coefficient for proportion_pulse variable for very large size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#' @source IP-117068
#' @export
pulse_movement <- function(proportion_pulse,
                           .intercept = fallRunDSM::params$.pulse_movement_intercept,
                           .proportion_pulse = fallRunDSM::params$.pulse_movement_proportion_pulse,
                           .medium = fallRunDSM::params$.pulse_movement_medium,
                           .large = fallRunDSM::params$.pulse_movement_large,
                           .vlarge = fallRunDSM::params$.pulse_movement_vlarge,
                           .medium_pulse = fallRunDSM::params$.pulse_movement_medium_pulse,
                           .large_pulse = fallRunDSM::params$.pulse_movement_large_pulse,
                           .very_large_pulse = fallRunDSM::params$.pulse_movement_very_large_pulse){

  s <- boot::inv.logit(.intercept + .proportion_pulse * proportion_pulse)

  m <- boot::inv.logit(.intercept + .medium + .proportion_pulse * proportion_pulse + .medium_pulse * proportion_pulse)

  l <- boot::inv.logit(.intercept + .large + .proportion_pulse * proportion_pulse + .large_pulse * proportion_pulse)

  vl <- boot::inv.logit(.intercept + .vlarge + .proportion_pulse * proportion_pulse + .very_large_pulse * proportion_pulse)


  cbind(s = s, m = m, l = l, vl = vl)
}
