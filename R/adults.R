#' @title Adult Straying
#' @description Calculate the proportion of adults straying to non-natal streams to spawn
#' @param wild Variable indicator of wild fish returning
#' @param natal_flow Variable describing proportion flows at tributary junctions coming from natal watershed in October
#' @param south_delta_watershed Variable indicator if watershed feeds into South Delta
#' @param cross_channel_gates_closed Variable describing number of days gates are closed for each month
#' @param prop_bay_trans Variable describing proportion transport to the bay
#' @param prop_delta_trans Variable describing proportion transport to the delta
#' @param .intercept Intercept, source: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#' @param .wild Coefficient for \code{wild} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#' @param .natal_flow Coefficient for \code{natal_flow} variable, source: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#' @param .cross_channel_gates_closed Coefficient for \code{cross_channel_gates_closed} variable, Source: Empirical model fit using  2008–2011 tagging data provided by East Bay Municipal Utility District.
#' @param .prop_bay_trans Coefficient for \code{prop_bay_trans} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#' @param .prop_delta_trans Coefficient for \code{prop_delta_trans} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#' @source IP-117068
#' @export
adult_stray <- function(wild, natal_flow, south_delta_watershed, cross_channel_gates_closed,
                        prop_bay_trans = 0, prop_delta_trans = 0,
                        .intercept = 3,
                        .wild = -5.5,
                        .natal_flow = -1.99,
                        .cross_channel_gates_closed = -0.174,
                        .prop_bay_trans = 2.09,
                        .prop_delta_trans = 2.89){

  boot::inv.logit(
    .intercept +
    .wild * wild +
    .natal_flow * natal_flow +
    .cross_channel_gates_closed * south_delta_watershed * cross_channel_gates_closed +
    .prop_bay_trans * prop_bay_trans * ( 1 - wild) +
    .prop_delta_trans * prop_delta_trans * (1 - wild)
  )

}

#' @title Adult En Route Survival
#' @description Calculate adult survival en route to spawning grounds
#' @param migratory_temp variable representing proportion of migratory corridor temperature above  20°C
#' @param bypass_overtopped Indicator for bypass overtopped
#' @param adult_harvest Adult harvest rate (Estimated with Coded Wire Tag data 2012–2013 (Palmer-Zwahlen & Kormos 2015; Palmer-Zwahlen et al. 2018))
#' @param ..surv_adult_enroute_int intercept, source: calibration
#' @param .migratory_temp coefficient for \code{migratory_temp} variable, source: \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{Schreck et al. (1994)}
#' @param .bypass_overtopped coefficient for \code{bypass_overtopped} variable, source: Expert opinion Ted Sommer, California Department of Water Resources (tributaries above bypasses only)
#' @source IP-117068
#' @export

surv_adult_enroute <- function(migratory_temp, bypass_overtopped, adult_harvest,
                               ..surv_adult_enroute_int = 3,
                               .migratory_temp = -0.26,
                               .bypass_overtopped = -0.019) {

  pmax(boot::inv.logit(..surv_adult_enroute_int +
                       .migratory_temp * migratory_temp +
                       .bypass_overtopped * bypass_overtopped) - adult_harvest, 0)
}

#' @title Adult Prespawn Survival
#' @description Calculate the adult prespawn survival
#' @param deg_day Variable describing average degree days
#' @param ..surv_adult_prespawn_int Intercept, Source: Calibration Estimate
#' @param .deg_day Coefficient for \code{deg_day} variable, source Colvin et al. (2018)
#' @source IP-117068
#' @export
surv_adult_prespawn <- function(deg_day, ..surv_adult_prespawn_int = 3, .deg_day = -0.000669526){

  boot::inv.logit(..surv_adult_prespawn_int + .deg_day * deg_day)
}
