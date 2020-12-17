
#' @title Egg to Fry Survival
#' @description Calculates the survival of transitioning from egg to fry
#' @param proportion_natural The proportion of natural-origin spawners
#' @param scour The probability of redd scouring event
#' @param temperature_effect The effect of inchannel temperature on egg survival.
#' Fall and spring estimated by C. Hammersmark (CBEC Ecoengineering Inc.). Winter-run value was calibrated.
#' @section Parameters:
#' Parameters from the model are obtained from either literature, calibration, export elicitation,
#' or meta-analysis. The source for each parameter in this function are detailed below.
#'
#'
#' \itemize{
#' \item intercept: Calibration
#' \item Natural Adults: \href{https://cdnsciencepub.com/doi/abs/10.1139/F10-168}{Chilcote et al. (2011)}
#' \item scour: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/montogemery_1996.pdf}{Montgomery et al. (1996)}
#' \item temperature_effect: Fall and spring estimated by C. Hammersmark (CBEC Ecoengineering Inc.)
#' }
#' @source IP-117068
#' @export
surv_egg_to_fry <- function(proportion_natural, scour, temperature_effect,
                            betas = c('intercept' = 0.041, `Natural Adults` = 0.533, 'scour' = -0.655)){

  boot::inv.logit(betas[1] + betas[2] * proportion_natural + betas[3] * scour) * temperature_effect
}

