#' @title Load Baseline Data
#' @description loads baseline data from DSMflow, DSMhabitat and DSMtemperature
#' @details
#' More details for each item in the list available below.
#'
#' \strong{Flow Inputs}
#' \itemize{
#'   \item freeport_flows - \link[DSMflow]{freeport_flow}
#'   \item vernalis_flows - \link[DSMflow]{vernalis_flow}
#'   \item stockton_flows - \link[DSMflow]{stockton_flow}
#'   \item CVP_exports - \link[DSMflow]{cvp_exports}
#'   \item SWP_exports - \link[DSMflow]{swp_exports}
#'   \item proportion_diverted - \link[DSMflow]{proportion_diverted}
#'   \item total_diverted - \link[DSMflow]{total_diverted}
#'   \item delta_proportion_diverted - \link[DSMflow]{delta_proportion_diverted}
#'   \item delta_total_diverted - \link[DSMflow]{delta_total_diverted}
#'   \item prop_pulse_flows - \link[DSMflow]{proportion_pulse_flows}
#'   \item prop_flow_natal - \link[DSMflow]{proportion_flow_natal}
#'   \item upper_sacramento_flows - \link[DSMflow]{upper_sacramento_flows}
#'   \item delta_inflow - \link[DSMflow]{delta_inflow}
#'   \item cc_gates_days_closed - \link[DSMflow]{delta_cross_channel_closed}
#'   \item cc_gates_prop_days_closed - \link[DSMflow]{delta_cross_channel_closed}
#'   \item proportion_flow_bypass - \link[DSMflow]{proportion_flow_bypasses}
#'   \item gates_overtopped - \link[DSMflow]{gates_overtopped}
#' }
#'
#' \strong{Temperature Inputs}
#' \itemize{
#'   \item vernalis_temps - \link[DSMTemperature]{vernalis_temperature}
#'   \item prisoners_point_temps - \link[DSMTemperature]{prisoners_point_temperature}
#'   \item degree_days - \link[DSMTemperature]{degree_days}
#'   \item mean_egg_temp_effect - \link[DSMTemperature]{egg_temperature_effect}
#'   \item avg_temp - \link[DSMTemperature]{stream_temperature}
#'   \item avg_temp_delta - \link[DSMTemperature]{delta_temperature}
#'   \item migratory_temperature_proportion_over_20 - \link[DSMTemperature]{migratory_temperature_proportion_over_20}
#' }
#'
#' \strong{Habitat Inputs}
#' \itemize{
#'   \item spawning_habitat - \link[DSMhabitat]{fr_spawn}
#'   \item inchannel_habitat_fry - \link[DSMhabitat]{fr_fry}
#'   \item inchannel_habitat_juvenile - \link[DSMhabitat]{fr_juv}
#'   \item floodplain_habitat - \link[DSMhabitat]{fr_fp}
#'   \item weeks_flooded - \link[DSMhabitat]{weeks_flooded}
#'   \item delta_habitat - \link[DSMhabitat]{delta_habitat}
#'   \item sutter_habitat - \link[DSMhabitat]{sutter_habitat}
#'   \item yolo_habitat - \link[DSMhabitat]{yolo_habitat}
#'   \item tisdale_bypass_watershed - \link[DSMhabitat]{tisdale_bypass_watershed}
#'   \item yolo_bypass_watershed - \link[DSMhabitat]{yolo_bypass_watershed}
#'   \item south_delta_routed_watersheds - \link[DSMhabitat]{south_delta_routed_watersheds}
#'   \item prop_high_predation - \link[DSMhabitat]{prop_high_predation}
#'   \item contact_points - \link[DSMhabitat]{contact_points}
#'   \item delta_contact_points - \link[DSMhabitat]{delta_contact_points}
#'   \item delta_prop_high_predation - \link[DSMhabitat]{delta_prop_high_predation}
#'   \item prob_strand_early - \link[DSMhabitat]{prob_strand_early}
#'   \item prob_strand_late - \link[DSMhabitat]{prob_strand_late}
#'   \item prob_nest_scoured - \link[DSMhabitat]{prob_nest_scoured}
#' }
#'
#' @export
load_baseline_data <- function() {
  # DSMflow variables -----
  freeport_flows <- DSMflow::freeport_flow
  vernalis_flows <- DSMflow::vernalis_flow
  stockton_flows <- DSMflow::stockton_flow
  CVP_exports <- DSMflow::cvp_exports
  SWP_exports <- DSMflow::swp_exports
  proportion_diverted <- DSMflow::proportion_diverted
  total_diverted <- DSMflow::total_diverted
  delta_proportion_diverted <- DSMflow::delta_proportion_diverted
  delta_total_diverted <- DSMflow::delta_total_diverted
  prop_pulse_flows <- DSMflow::proportion_pulse_flows
  prop_flow_natal <- DSMflow::proportion_flow_natal
  upper_sacramento_flows <- DSMflow::upper_sacramento_flows
  delta_inflow <- DSMflow::delta_inflow
  cc_gates_days_closed <- DSMflow::delta_cross_channel_closed["count", ]
  cc_gates_prop_days_closed <- DSMflow::delta_cross_channel_closed["proportion", ]
  proportion_flow_bypass <- DSMflow::proportion_flow_bypasses
  gates_overtopped <- DSMflow::gates_overtopped

  # DSMtemperature variables -----
  vernalis_temps <- DSMtemperature::vernalis_temperature
  prisoners_point_temps <- DSMtemperature::prisoners_point_temperature
  degree_days <- DSMtemperature::degree_days
  mean_egg_temp_effect <- DSMtemperature::egg_temperature_effect$fall_run
  avg_temp <- DSMtemperature::stream_temperature
  avg_temp_delta <- DSMtemperature::delta_temperature
  migratory_temperature_proportion_over_20 <- DSMtemperature::migratory_temperature_proportion_over_20

  # DSMhabitat variables -----
  spawning_habitat <- DSMhabitat::fr_spawn
  inchannel_habitat_fry <- DSMhabitat::fr_fry
  inchannel_habitat_juvenile <- DSMhabitat::fr_juv
  floodplain_habitat <- DSMhabitat::fr_fp
  weeks_flooded <- DSMhabitat::weeks_flooded
  delta_habitat <- DSMhabitat::delta_habitat
  sutter_habitat <- DSMhabitat::sutter_habitat
  yolo_habitat <- DSMhabitat::yolo_habitat
  tisdale_bypass_watershed <- DSMhabitat::tisdale_bypass_watershed
  yolo_bypass_watershed <- DSMhabitat::yolo_bypass_watershed
  south_delta_routed_watersheds <- DSMhabitat::south_delta_routed_watersheds
  prop_high_predation <- DSMhabitat::prop_high_predation
  contact_points <- DSMhabitat::contact_points
  delta_contact_points <- DSMhabitat::delta_contact_points
  delta_prop_high_predation <- DSMhabitat::delta_prop_high_predation
  prob_strand_early <- DSMhabitat::prob_strand_early
  prob_strand_late <- DSMhabitat::prob_strand_late
  prob_nest_scoured <- DSMhabitat::prob_nest_scoured

  list(
    freeport_flows = freeport_flows,
    vernalis_flows = vernalis_flows,
    stockton_flows = stockton_flows,
    CVP_exports = CVP_exports,
    SWP_exports = SWP_exports,
    proportion_diverted = proportion_diverted,
    total_diverted = total_diverted,
    delta_proportion_diverted = delta_proportion_diverted,
    delta_total_diverted = delta_total_diverted,
    prop_pulse_flows = prop_pulse_flows,
    prop_flow_natal = prop_flow_natal,
    upper_sacramento_flows = upper_sacramento_flows,
    delta_inflow = delta_inflow,
    cc_gates_days_closed = cc_gates_days_closed,
    cc_gates_prop_days_closed = cc_gates_prop_days_closed,
    proportion_flow_bypass = proportion_flow_bypass,
    gates_overtopped = gates_overtopped,
    vernalis_temps = vernalis_temps,
    prisoners_point_temps = prisoners_point_temps,
    degree_days = degree_days,
    mean_egg_temp_effect = mean_egg_temp_effect,
    avg_temp = avg_temp,
    avg_temp_delta = avg_temp_delta,
    migratory_temperature_proportion_over_20 = migratory_temperature_proportion_over_20,
    spawning_habitat = spawning_habitat,
    inchannel_habitat_fry = inchannel_habitat_fry,
    inchannel_habitat_juvenile = inchannel_habitat_juvenile,
    floodplain_habitat = floodplain_habitat,
    weeks_flooded = weeks_flooded,
    delta_habitat = delta_habitat,
    sutter_habitat = sutter_habitat,
    yolo_habitat = yolo_habitat,
    tisdale_bypass_watershed = tisdale_bypass_watershed,
    yolo_bypass_watershed = yolo_bypass_watershed,
    south_delta_routed_watersheds = south_delta_routed_watersheds,
    prop_high_predation = prop_high_predation,
    contact_points = contact_points,
    delta_contact_points = delta_contact_points,
    delta_prop_high_predation = delta_prop_high_predation,
    prob_strand_early = prob_strand_early,
    prob_strand_late = prob_strand_late,
    prob_nest_scoured = prob_nest_scoured
  )
}
