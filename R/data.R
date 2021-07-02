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
#'   \item \code{inchannel_habitat_fry}: More details at \code{\link[DSMhabitat]{fr_fry}}
#'   \item \code{inchannel_habitat_juvenile}: More details at \code{\link[DSMhabitat]{fr_juv}}
#'   \item \code{floodplain_habitat}: More details at \code{\link[DSMhabitat]{fr_fp}}
#'   \item \code{sutter_habitat}: More details at \code{\link[DSMhabitat]{sutter_habitat}}
#'   \item \code{yolo_habitat}: More details at \code{\link[DSMhabitat]{yolo_habitat}}
#'   \item \code{delta_habitat}: More details at \code{\link[DSMhabitat]{delta_habitat}}
#' }
#' @section Spawning Adults:
#' \itemize{
#'   \item \code{prop_flow_natal}: More details at \code{\link[DSMflow]{proportion_flow_natal}}
#'   \item \code{south_delta_routed_watersheds}: More details at \code{\link[DSMhabitat]{south_delta_routed_watersheds}}
#'   \item \code{cc_gates_days_closed}: More details at \code{\link[DSMflow]{delta_cross_channel_closed}}
#'   \item \code{gates_overtopped}: More details at \code{\link[DSMflow]{gates_overtopped}}
#'   \item \code{tisdale_bypass_watershed}: More details at \code{\link[DSMhabitat]{tisdale_bypass_watershed}}
#'   \item \code{yolo_bypass_watershed}: More details at \code{\link[DSMhabitat]{yolo_bypass_watershed}}
#'   \item \code{migratory_temperature_proportion_over_20}: More details at \code{\link[DSMtemperature]{migratory_temperature_proportion_over_20}}
#'   \item \code{..surv_adult_enroute_int}:  Intercept, Source: calibration
#'   \item \code{.adult_stray_intercept}: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#'   \item \code{.adult_stray_wild}: Coefficient for \code{wild} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{.adult_stray_natal_flow}: Coefficient for \code{natal_flow} variable, source: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#'   \item \code{.adult_stray_cross_channel_gates_closed}: Coefficient for \code{cross_channel_gates_closed} variable, Source: Empirical model fit using  2008–2011 tagging data provided by East Bay Municipal Utility District.
#'   \item \code{.adult_stray_prop_bay_trans}: Coefficient for \code{prop_bay_trans} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{.adult_stray_prop_delta_trans}: Coefficient for \code{prop_delta_trans} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{.adult_en_route_migratory_temp}: Coefficient for \code{migratory_temp} variable, source: \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{Schreck et al. (1994)}
#'   \item \code{.adult_en_route_bypass_overtopped}: Coefficient for \code{bypass_overtopped} variable, source: Expert opinion Ted Sommer, California Department of Water Resources (tributaries above bypasses only)
#'   \item \code{.adult_en_route_adult_harvest_rate}:  Adult harvest rate, source:  \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{degree_days}: More details at \code{\link[DSMtemperature]{degree_days}}
#'   \item \code{month_return_proportions}: More details at \code{\link[fallRunDSM]{month_return_proportions}}
#'   \item \code{..surv_adult_prespawn_int}:  Intercept, Source: Calibration Estimate
#'   \item \code{.adult_prespawn_deg_day}: Coefficient for \code{deg_day} variable, source Colvin et al. (2018)
#'   \item \code{prob_nest_scoured}: More details at \code{\link[DSMhabitat]{prob_nest_scoured}}
#'   \item \code{spawn_success_sex_ratio}: Variable describing the female to male spawning ratio, default 0.5
#'   \item \code{spawn_success_redd_size}: Variable describing the size of redds including defensible space, default value 9.29 square meters
#'   \item \code{spawn_success_fecundity}: Variable describing the number of eggs per female, default value 5522
#' }
#' @section Egg to Fry Survival:
#' \itemize{git
#'   \item \code{proportion_hatchery}: More details at \code{\link[fallRunDSM]{proportion_hatchery}}
#'   \item \code{prob_nest_scoured}: More details at \code{\link[DSMhabitat]{prob_nest_scoured}}
#'   \item \code{mean_egg_temp_effect}: More details at \code{\link[DSMtemperature]{egg_temperature_effect}}
#'   \item \code{surv_egg_to_fry_proportion_natural}: TODO
#'   \item \code{surv_egg_to_fry_scour}: TODO
#'   \item \code{..surv_egg_to_fry_int}:  Intercept, Source: Calibration
#' }
#' @section Rearing Survival:
#' \itemize{
#'   \item \code{avg_temp}: More details at \code{\link[DSMtemperature]{stream_temperature}}
#'   \item \code{avg_temp_delta}: More details at \code{\link[DSMtemperature]{delta_temperature}}
#'   \item \code{prob_strand_early}: More details at \code{\link[DSMhabitat]{prob_strand_early}}
#'   \item \code{prob_strand_late}: More details at \code{\link[DSMhabitat]{prob_strand_late}}
#'   \item \code{proportion_diverted}: More details at \code{\link[DSMflow]{proportion_diverted}}
#'   \item \code{total_diverted}: More details at \code{\link[DSMflow]{total_diverted}}
#'   \item \code{delta_proportion_diverted}: More details at \code{\link[DSMflow]{delta_proportion_diverted}}
#'   \item \code{delta_total_diverted}: More details at \code{\link[DSMflow]{delta_total_diverted}}
#'   \item \code{weeks_flooded}: More details at \code{\link[DSMhabitat]{weeks_flooded}}
#'   \item \code{prop_high_predation}: More details at \code{\link[DSMhabitat]{prop_high_predation}}
#'   \item \code{contact_points}: More details at \code{\link[DSMhabitat]{contact_points}}
#'   \item \code{delta_contact_points}: More details at \code{\link[DSMhabitat]{delta_contact_points}}
#'   \item \code{delta_prop_high_predation}: More details at \code{\link[DSMhabitat]{delta_prop_high_predation}}
#'   \item \code{.surv_juv_rear_int}: Intercept, Source: calibration (varies by tributary)
#'   \item \code{.surv_juv_rear_contact_points}: Coefficient for contact_points variable, Source: calibration
#'   \item \code{.surv_juv_rear_prop_diversions}: Coefficient for prop_diversions variable, Source: calibration
#'   \item \code{.surv_juv_rear_total_diversions}: Coefficient for total_diversions variable, Source: calibration
#'   \item \code{..surv_juv_bypass_int}: Intercept, Source: calibration
#'   \item \code{..surv_juv_delta_int}: Intercept, Source: calibration
#'   \item \code{..surv_juv_delta_contact_points}: Coefficient for contact_points variable, Source: calibration
#'   \item \code{..surv_juv_delta_total_diverted}: Coefficient for total_diversions variable, Source: calibration
#'   \item \code{.surv_juv_rear_avg_temp_thresh} TODO
#'   \item \code{.surv_juv_rear_high_predation} TODO
#'   \item \code{.surv_juv_rear_stranded} TODO
#'   \item \code{.surv_juv_rear_medium} TODO
#'   \item \code{.surv_juv_rear_large} TODO
#'   \item \code{.surv_juv_rear_floodplain} TODO
#'   \item \code{.surv_juv_bypass_avg_temp_thresh} TODO
#'   \item \code{.surv_juv_bypass_high_predation} TODO
#'   \item \code{.surv_juv_bypass_medium} TODO
#'   \item \code{.surv_juv_bypass_large} TODO
#'   \item \code{.surv_juv_bypass_floodplain} TODO
#'   \item \code{.surv_juv_delta_avg_temp_thresh} Coefficient for avg_temp_thresh variable, Source: \href{https://www.noaa.gov/sites/default/files/atoms/files/07354626766.pdf}{Marine and Chech (2004)}
#'   \item \code{.surv_juv_delta_high_predation} TODO
#'   \item \code{.surv_juv_delta_prop_diverted} TODO
#'   \item \code{.surv_juv_delta_medium} TODO
#'   \item \code{.surv_juv_delta_large} TODO
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
#' @section Delta Routing and Rearing:
#' \itemize{
#'   \item \code{freeport_flows}
#'   \item \code{cc_gates_days_closed}
#'   \item \code{growth_rates}
#' }
#' @section Routing:
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
#' @section Rearing:
#' \itemize{
#'   \item \code{growth_rates}: More details at: \code{\link{growth_rates}}
#'   \item \code{growth_rates_floodplain}
#'   \item \code{weeks_flooded}
#'
#' }
#' @section Ocean Entry Success:
#' \itemize{
#'   \item \code{.ocean_entry_success_length}: Variable representing the fork lengths for each size classes: More details at: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#'   \item \code{..ocean_entry_success_int}
#'   \item \code{.ocean_entry_success_months}
#' }
#'
"params"
