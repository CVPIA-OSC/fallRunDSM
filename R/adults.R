#' @title Adult Straying
#' @description Calculate the proportion of adults straying to non-natal streams to spawn
#' @details See \code{\link{params}} for details on parameter sources
#' @param wild Variable indicator of wild fish returning
#' @param natal_flow Variable describing proportion flows at tributary junctions coming from natal watershed in October
#' @param south_delta_watershed Variable indicator if watershed feeds into South Delta
#' @param cross_channel_gates_closed Variable describing number of days gates are closed for each month
#' @param prop_bay_trans Variable describing proportion transport to the bay
#' @param prop_delta_trans Variable describing proportion transport to the delta
#' @param .intercept Intercept
#' @param .wild Coefficient for \code{wild} variable
#' @param .natal_flow Coefficient for \code{natal_flow} variable
#' @param .cross_channel_gates_closed Coefficient for \code{cross_channel_gates_closed} variable
#' @param .prop_bay_trans Coefficient for \code{prop_bay_trans} variable
#' @param .prop_delta_trans Coefficient for \code{prop_delta_trans} variable
#' @source IP-117068
#' @export
adult_stray <- function(wild, natal_flow, south_delta_watershed, cross_channel_gates_closed,
                        prop_bay_trans = 0, prop_delta_trans = 0,
                        .intercept = winterRunDSM::params$.adult_stray_intercept,
                        .wild = winterRunDSM::params$.adult_stray_wild,
                        .natal_flow = winterRunDSM::params$.adult_stray_natal_flow,
                        .cross_channel_gates_closed = winterRunDSM::params$.adult_stray_cross_channel_gates_closed,
                        .prop_bay_trans = winterRunDSM::params$.adult_stray_prop_bay_trans,
                        .prop_delta_trans = winterRunDSM::params$.adult_stray_prop_delta_trans){

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
#' @details See \code{\link{params}} for details on parameter sources
#' @param migratory_temp variable representing proportion of migratory corridor temperature above  20°C
#' @param bypass_overtopped Indicator for bypass overtopped
#' @param adult_harvest Adult harvest rate
#' @param ..surv_adult_enroute_int Intercept
#' @param .migratory_temp Coefficient for \code{migratory_temp} variable
#' @param .bypass_overtopped Coefficient for \code{bypass_overtopped} variable
#' @source IP-117068
#' @export

surv_adult_enroute <- function(migratory_temp, bypass_overtopped, adult_harvest,
                               ..surv_adult_enroute_int = winterRunDSM::params$..surv_adult_enroute_int,
                               .migratory_temp = winterRunDSM::params$.adult_en_route_migratory_temp,
                               .bypass_overtopped = winterRunDSM::params$.adult_en_route_bypass_overtopped) {

  pmax(boot::inv.logit(..surv_adult_enroute_int +
                       .migratory_temp * migratory_temp +
                       .bypass_overtopped * bypass_overtopped) - adult_harvest, 0)
}

#' @title Adult Prespawn Survival
#' @description Calculate the adult prespawn survival
#' @details See \code{\link{params}} for details on parameter sources
#' @param deg_day Variable describing average degree days
#' @param ..surv_adult_prespawn_int Intercept
#' @param .deg_day Coefficient for \code{deg_day} variable
#' @source IP-117068
#' @export
surv_adult_prespawn <- function(deg_day,
                                ..surv_adult_prespawn_int = winterRunDSM::params$..surv_adult_prespawn_int,
                                .deg_day = winterRunDSM::params$.adult_prespawn_deg_day){

  boot::inv.logit(..surv_adult_prespawn_int + .deg_day * deg_day)
}
