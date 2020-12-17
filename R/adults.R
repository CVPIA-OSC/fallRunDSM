#' @title Adult Straying
#' @description Calculate the proportion of adults straying to non-natal streams to spawn
#' @param wild Indicator of wild fish returning
#' @param natal_flow Proportion flows at tributary junctions coming from natal watershed in October
#' @param south_delta_watershed Indicator if watershed feeds into South Delta
#' @param cross_channel_gates_closed Number of days gates are closed for each month
#' @param prop_delta_trans
#' @param prop_bay_trans
#' @param betas Parameters estimated from calibration
#' @section Parameters:
#' Parameters from the model are obtained from either literature, calibration, export elicitation,
#' or meta-analysis. The source for each parameter in this function are detailed below.
#'
#'
#' \itemize{
#' \item intercept- Intercept: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#' \item wild - Wild Salmon Parameter: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#' \item natal flow - Natal Flow Parameter: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#' \item gates open - Cross Channel Gates Parameter: Empirical model fit using  2008–2011 tagging data provided by East Bay Municipal Utility District.
#' \item bay - Bay transport proportion parameter. Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#' \item delta  - Delta transport proportion parameter. Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#' }
#' @source IP-117068
#' @export
adult_stray <- function(wild, natal_flow, south_delta_watershed, cross_channel_gates_closed,
                        prop_delta_trans = 0, prop_bay_trans = 0,
                        betas = c(intercept = 3, wild = -5.5, `natal flow` = -1.99,
                                  `gates open` = -0.174, bay = 2.09, delta = 2.89)){

  boot::inv.logit(
    betas[1] +
      betas[2] * wild +
      betas[3] * natal_flow +
      betas[4] * south_delta_watershed * cross_channel_gates_closed +
      betas[5] * prop_bay_trans * ( 1 - wild) +
      betas[6] * prop_delta_trans * (1 - wild)
  )

}

#' @title Adult En Route Survival
#' @description Calculate adult survial en route to spawning grounds
#' @param migratory_temp Proportion of migratory corridor temperature above  20°C
#' @param bypass_overtopped Indicator for bypass overtopped
#' @param adult_harvest Adult harvest rate (Estimated with Coded Wire Tag data 2012–2013 (Palmer-Zwahlen & Kormos 2015; Palmer-Zwahlen et al. 2018))
#' @param betas Parameters estimated from calibration
#' @section Parameters:
#' Parameters from the model are obtained from either literature, calibration, export elicitation,
#' or meta-analysis. The source for each parameter in this function are detailed below.
#'
#'
#' \itemize{
#' \item intercept: Calibration Estimate
#' \item temperature (Average daily temperature): \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{Schreck et al. (1994)}
#' \item overtop (Bypass overtopped parameter): Expert opinion Ted Sommer, California Department of Water Resources (tributaries above bypasses only)
#' \item gates open - Cross Channel Gates Parameter: Empirical model fit using  2008–2011 tagging data provided by East Bay Municipal Utility District.
#' }
#' @source IP-117068
#' @export
surv_adult_enroute <- function(migratory_temp, bypass_overtopped, adult_harvest,
                               betas = c(intercept = 3, temperature = -0.26, overtop = -0.019)) {

  pmax(boot::inv.logit(betas[1] + betas[2] * migratory_temp + betas[3] * bypass_overtopped) - adult_harvest, 0)

}

#' @title Adult Prespawn Survival
#' @description Calculate the adult prespawn survival
#' @param deg_day Average degree days
#' @param betas Parameters from calibration process
#' @section Parameters:
#' Parameters from the model are obtained from either literature, calibration, export elicitation,
#' or meta-analysis. The source for each parameter in this function are detailed below.
#'
#'
#' \itemize{
#' \item intercept: Calibration Estimate
#' }
#' @source IP-117068
#' @export
surv_adult_prespawn <- function(deg_day,
                                betas = c(intercept = 3, `degree days` = -0.000669526)){

  boot::inv.logit(betas[1] + betas[2] * deg_day)
}
