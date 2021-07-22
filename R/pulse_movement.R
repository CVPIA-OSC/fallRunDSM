#' @title Juvenile Pulse Movement
#' @description Calculates the proportion of juveniles in each size class that
#' migrate due to pulse flow
#' @details See \code{\link{params}} for details on parameter sources
#' @param proportion_pulse The proportion of flow that is a pulse, estimated
#' by the standard deviation of flow divided by the median flow for each month
#' @param .intercept Intercept
#' @param .proportion_pulse Coefficient for the \code{proportion_pulse} variable
#' @param .medium Size related intercept for medium sized fish
#' @param .large Size related intercept for large sized fish
#' @param .vlarge Size related intercept for very large sized fish
#' @param .medium_pulse Additional coefficient for \code{proportion_pulse} variable for medium size fish
#' @param .large_pulse Additional coefficient for \code{proportion_pulse} variable for large size fish
#' @param .very_large_pulse Additional coefficient for \code{proportion_pulse} variable for very large size fish
#' @source IP-117068
#' @export
pulse_movement <- function(proportion_pulse,
                           .intercept = winterRunDSM::params$.pulse_movement_intercept,
                           .proportion_pulse = winterRunDSM::params$.pulse_movement_proportion_pulse,
                           .medium = winterRunDSM::params$.pulse_movement_medium,
                           .large = winterRunDSM::params$.pulse_movement_large,
                           .vlarge = winterRunDSM::params$.pulse_movement_vlarge,
                           .medium_pulse = winterRunDSM::params$.pulse_movement_medium_pulse,
                           .large_pulse = winterRunDSM::params$.pulse_movement_large_pulse,
                           .very_large_pulse = winterRunDSM::params$.pulse_movement_very_large_pulse){

  s <- boot::inv.logit(.intercept + .proportion_pulse * proportion_pulse)

  m <- boot::inv.logit(.intercept + .medium + .proportion_pulse * proportion_pulse + .medium_pulse * proportion_pulse)

  l <- boot::inv.logit(.intercept + .large + .proportion_pulse * proportion_pulse + .large_pulse * proportion_pulse)

  vl <- boot::inv.logit(.intercept + .vlarge + .proportion_pulse * proportion_pulse + .very_large_pulse * proportion_pulse)


  cbind(s = s, m = m, l = l, vl = vl)
}
