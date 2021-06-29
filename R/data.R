#' Adult Harvest Rate
#' @description Proportion of adults harvested from golden gate until they reach their natal shed
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"adult_harvest_rate"

#' Natural Spawners Removal Rate
#' @description Spawners removed for hatcheries
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"natural_adult_removal_rate"

#' Hatchery Allocation
#' @description The proportion of hatchery fish spawning
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"hatchery_allocation"

#' @title Adult Seeds
#' @description adult fish for the initial 5 years of the simulations derived
#' from average escapement estimates from 2013 to 2017 (Azat 2019).
#' @format A matrix with dimension 31 x 30 (watershed x year)
"adult_seeds"

#' @title Proportion Hatchery
#' @description TODO
"proportion_hatchery"

#' @title Month Return Proportions
#' @description the proportion of spawning fish in Oct-Dec
"month_return_proportions"

#' @title Mass by Size Class
#' @description mass of fish by the size class
"mass_by_size_class"

#' @title Cross Channel Stray Rate
#' @description TODO
"cross_channel_stray_rate"

#' @title Stray Rate
#' @description TODO
"stray_rate"

#' @title Diversity Groups
#' @description TODO
"diversity_group"

#' @rdname growth
"growth_rates"

#' @rdname growth_floodplain
"growth_rates_floodplain"

#' @title Model Parameters
#' @description A list containing all parameters needed for running the \code{\link{fall_run_model}}
#' @usage NULL
#' @format NULL
#' @section Habitat Inputs:
#' \itemize{
#'   \item \code{spawning_habitat}: More details at \code{\link[DSMhabitat]{fr_spawn}}
#'   \item \code{inchannel_habitat_fry}: More details at \code{\link[DSMhabitat]{}}
#'   \item \code{inchannel_habitat_juvenile}: More details at \code{\link[DSMhabitat]{}}
#'   \item \code{floodplain_habitat}: More details at \code{\link[DSMhabitat]{}}
#'   \item \code{sutter_habitat}: More details at \code{\link[DSMhabitat]{}}
#'   \item \code{yolo_habitat}: More details at \code{\link[DSMhabitat]{}}
#'   \item \code{delta_habitat}: More details at \code{\link[DSMhabitat]{}}
#' }
#' @section Spawning Adults:
#' \itemize{
#'   \item \code{prop_flow_natal}
#'   \item \code{south_delta_routed_watersheds}
#'   \item \code{cc_gates_days_closed}
#'   \item \code{gates_overtopped}
#'   \item \code{tisdale_bypass_watershed}
#'   \item \code{yolo_bypass_watershed}
#'   \item \code{migratory_temperature_proportion_over_20}
#'   \item \code{..surv_adult_enroute_int}
#'   \item \code{.adult_stray_intercept}
#'   \item \code{.adult_stray_wild}
#'   \item \code{.adult_stray_natal_flow}
#'   \item \code{.adult_stray_cross_channel_gates_closed}
#'   \item \code{.adult_stray_prop_bay_trans}
#'   \item \code{.adult_stray_prop_delta_trans}
#'   \item \code{.adult_en_route_migratory_temp}
#'   \item \code{.adult_en_route_bypass_overtopped}
#'   \item \code{.adult_en_route_adult_harvest_rate}
#'   \item \code{degree_days}
#'   \item \code{month_return_proportions}
#'   \item \code{..surv_adult_prespawn_int}
#'   \item \code{.adult_prespawn_deg_day}
#'   \item \code{prob_nest_scoured}
#'   \item \code{spawn_success_sex_ratio}
#'   \item \code{spawn_success_redd_size}
#'   \item \code{spawn_success_fecundity}
#' }
#' @section Egg to Fry Survival:
#' \itemize{
#'   \item \code{proportion_hatchery}
#'   \item \code{prob_nest_scoured}
#'   \item \code{mean_egg_temp_effect}
#'   \item \code{surv_egg_to_fry_proportion_natural}
#'   \item \code{surv_egg_to_fry_scour}
#'   \item \code{..surv_egg_to_fry_int}
#' }
#' @section Rearing Survival:
#' \itemize{
#'   \item \code{avg_temp}
#'   \item \code{avg_temp_delta}
#'   \item \code{prob_strand_early}
#'   \item \code{prob_strand_late}
#'   \item \code{proportion_diverted}
#'   \item \code{total_diverted}
#'   \item \code{delta_proportion_diverted}
#'   \item \code{delta_total_diverted}
#'   \item \code{weeks_flooded}
#'   \item \code{prop_high_predation}
#'   \item \code{contact_points}
#'   \item \code{delta_contact_points}
#'   \item \code{delta_prop_high_predation}
#'   \item \code{.surv_juv_rear_int}
#'   \item \code{.surv_juv_rear_contact_points}
#'   \item \code{.surv_juv_rear_prop_diversions}
#'   \item \code{.surv_juv_rear_total_diversions}
#'   \item \code{..surv_juv_bypass_int}
#'   \item \code{..surv_juv_delta_int}
#'   \item \code{..surv_juv_delta_contact_points}
#'   \item \code{..surv_juv_delta_total_diverted}
#'   \item \code{.surv_juv_rear_avg_temp_thresh}
#'   \item \code{.surv_juv_rear_high_predation}
#'   \item \code{.surv_juv_rear_stranded}
#'   \item \code{.surv_juv_rear_medium}
#'   \item \code{.surv_juv_rear_large}
#'   \item \code{.surv_juv_rear_floodplain}
#'   \item \code{.surv_juv_bypass_avg_temp_thresh}
#'   \item \code{.surv_juv_bypass_high_predation}
#'   \item \code{.surv_juv_bypass_medium}
#'   \item \code{.surv_juv_bypass_large}
#'   \item \code{.surv_juv_bypass_floodplain}
#'   \item \code{.surv_juv_delta_avg_temp_thresh}
#'   \item \code{.surv_juv_delta_high_predation}
#'   \item \code{.surv_juv_delta_prop_diverted}
#'   \item \code{.surv_juv_delta_medium}
#'   \item \code{.surv_juv_delta_large}
#' }
#' @section Migratory Survival:
#' \itemize{
#'   \item \code{cc_gates_prop_days_closed}
#'   \item \code{freeport_flows}
#'   \item \code{vernalis_flows}
#'   \item \code{stockton_flows}
#'   \item \code{vernalis_temps}
#'   \item \code{prisoners_point_temps}
#'   \item \code{CVP_exports}
#'   \item \code{SWP_exports}
#'   \item \code{upper_sacramento_flows}
#'   \item \code{delta_inflow}
#'   \item \code{avg_temp_delta}
#'   \item \code{avg_temp}
#'   \item \code{delta_proportion_diverted}
#'   \item \code{.surv_juv_outmigration_sac_delta_intercept_one}
#'   \item \code{.surv_juv_outmigration_sac_delta_intercept_two}
#'   \item \code{.surv_juv_outmigration_sac_delta_intercept_three}
#'   \item \code{.surv_juv_outmigration_sac_delta_delta_flow}
#'   \item \code{.surv_juv_outmigration_sac_delta_avg_temp}
#'   \item \code{.surv_juv_outmigration_sac_delta_perc_diversions}
#'   \item \code{.surv_juv_outmigration_sac_delta_medium}
#'   \item \code{.surv_juv_outmigration_sac_delta_large}
#'   \item \code{..surv_juv_outmigration_sj_int}
#'   \item \code{..surv_juv_outmigration_sac_int_one}
#'   \item \code{..surv_juv_outmigration_sac_prop_diversions}
#'   \item \code{..surv_juv_outmigration_sac_total_diversions}
#'   \item \code{..surv_juv_outmigration_sac_int_two}
#'   \item \code{.surv_juv_outmigration_san_joquin_medium}
#'   \item \code{.surv_juv_outmigration_san_joaquin_large}
#' }
#' @section Delta Routing and Rearing
#' \itemize{
#'   \item \code{freeport_flows}
#'   \item \code{cc_gates_days_closed}
#'   \item \code{growth_rates}
#' }
#' @section Routing
#' \itemize{
#'   \item \code{prop_pulse_flows}
#'   \item \code{proportion_flow_bypass}
#'   \item \code{.pulse_movement_intercept}
#'   \item \code{.pulse_movement_proportion_pulse}
#'   \item \code{.pulse_movement_medium}
#'   \item \code{.pulse_movement_large}
#'   \item \code{.pulse_movement_vlarge}
#'   \item \code{.pulse_movement_medium_pulse}
#'   \item \code{.pulse_movement_large_pulse}
#'   \item \code{.pulse_movement_very_large_pulse}
#' }
#' @section Rearing
#'
"params"
