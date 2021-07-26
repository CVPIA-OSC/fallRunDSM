
#' @title Egg to Fry Survival
#' @description Calculates the survival of transitioning from egg to fry
#' @details See \code{\link{params}} for details on parameter sources
#' @param proportion_natural Variable describing the proportion of natural-origin spawners
#' @param scour Variable describing the probability of redd scouring event
#' @param temperature_effect Variable describing the effect of inchannel temperature on egg survival.
#' spring and spring estimated by C. Hammersmark (CBEC Ecoengineering Inc.). Winter-run value was calibrated.
#' @param ..surv_egg_to_fry_int Intercept
#' @param .proportion_natural Coefficient for \code{proportion_natural} variable
#' @param .scour Coefficient for \code{scour} variable
#' @source IP-117068
#' @export
surv_egg_to_fry <- function(proportion_natural,
                            scour,
                            temperature_effect = springRunDSM::params$mean_egg_temp_effect,
                            ..surv_egg_to_fry_int = springRunDSM::params$..surv_egg_to_fry_int,
                            .proportion_natural = springRunDSM::params$.surv_egg_to_fry_proportion_natural,
                            .scour = springRunDSM::params$.surv_egg_to_fry_scour){

  boot::inv.logit(..surv_egg_to_fry_int + .proportion_natural * proportion_natural +
                  .scour * scour) * temperature_effect
}

