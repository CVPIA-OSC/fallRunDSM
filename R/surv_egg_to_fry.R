
#' @title Egg to Fry Survival
#' @description Calculates the survival of transitioning from egg to fry
#' @param proportion_natural Variable describing the proportion of natural-origin spawners
#' @param scour Variable describing the probability of redd scouring event
#' @param temperature_effect Variable describing the effect of inchannel temperature on egg survival.
#' Fall and spring estimated by C. Hammersmark (CBEC Ecoengineering Inc.). Winter-run value was calibrated.
#' @param ..surv_egg_to_fry_int Intercept, source: Calibration
#' @param .proportion_natural Coefficient for proportion_natural variable, Source: \href{https://cdnsciencepub.com/doi/abs/10.1139/F10-168}{Chilcote et al. (2011)}
#' @param .scour Coefficient for scour variable, Source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/montogemery_1996.pdf}{Montgomery et al. (1996)}
#' @source IP-117068
#' @export
surv_egg_to_fry <- function(proportion_natural,
                            scour,
                            temperature_effect,
                            ..surv_egg_to_fry_int = 0.041,
                            .proportion_natural = 0.533,
                            .scour = -0.655){

  boot::inv.logit(..surv_egg_to_fry_int + .proportion_natural * proportion_natural +
                  .scour * scour) * temperature_effect
}

# TODO need to implement some api for sensitivity analysis
# tmp.eff = temperature_effect
# if(sum(vary == "egg.viab")){
#   viab<-log((tmp.eff+ 0.000001)/((1-tmp.eff)+0.0000001))
#   viab<- viab*pctil[vary == "egg.viab"]
#   tmp.eff<-inv.logit(viab)
# }
