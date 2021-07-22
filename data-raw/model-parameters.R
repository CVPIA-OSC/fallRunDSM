library(tidyverse)

# 2019 Calibration Parameters ------------
params <- list(

  # Data from winterRunDSM cache-data (values vary by run)
  hatchery_allocation = winterRunDSM::hatchery_allocation,
  natural_adult_removal_rate = winterRunDSM::natural_adult_removal_rate,
  proportion_hatchery = winterRunDSM::proportion_hatchery,
  month_return_proportions = winterRunDSM::month_return_proportions,
  survival_betas = winterRunDSM::survival_betas,
  growth_rates = winterRunDSM::growth_rates_inchannel,
  growth_rates_floodplain = winterRunDSM::growth_rates_floodplain,
  mass_by_size_class = winterRunDSM::mass_by_size_class,
  cross_channel_stray_rate = winterRunDSM::cross_channel_stray_rate,
  stray_rate = winterRunDSM::stray_rate,
  adult_harvest_rate = winterRunDSM::adult_harvest_rate,
  diversity_group = winterRunDSM::diversity_group,

  # Coefficients for adult submodules
  # Stray
  .adult_stray_intercept = 3,
  .adult_stray_wild = -5.5,
  .adult_stray_natal_flow = -1.99,
  .adult_stray_cross_channel_gates_closed = -0.174,
  .adult_stray_prop_bay_trans = 2.09,
  .adult_stray_prop_delta_trans = 2.89,
  # Enroute Survival
  ..surv_adult_enroute_int = 3.4999976,
  .adult_en_route_migratory_temp = -0.26,
  .adult_en_route_bypass_overtopped = -0.019,
  .adult_en_route_adult_harvest_rate = winterRunDSM::adult_harvest_rate, # varies by run
  # Prespawn Survival
  ..surv_adult_prespawn_int = 3,
  .adult_prespawn_deg_day = -0.000669526,

  # Routing coefficients and variables
  .pulse_movement_intercept = -7.70744,
  .pulse_movement_proportion_pulse = 0.26579,
  .pulse_movement_medium = 1.66845,
  .pulse_movement_large = 0.5706,
  .pulse_movement_vlarge = -4.305,
  .pulse_movement_medium_pulse = -0.25477,
  .pulse_movement_large_pulse = -0.44778,
  .pulse_movement_very_large_pulse = 0.329,
  territory_size = c(0.0498944803729701, 0.138941944739835, 0.471083652829798, 0),

  # Spawn success variables
  spawn_success_sex_ratio = 0.5,
  spawn_success_redd_size = 9.29,
  spawn_success_fecundity = 5522,

  # Egg to fry survival calibrated parameters and coefficients
  ..surv_egg_to_fry_int = 0.041,
  ..surv_egg_to_fry_mean_egg_temp_effect = 0.646623,
  .surv_egg_to_fry_proportion_natural = 0.533,
  .surv_egg_to_fry_scour = -0.655,

  # Juvenile rearing survival coefficients and variables
  ..surv_juv_rear_int = c(`Upper Sacramento River` = -0.656, `Antelope Creek` = -0.656, `Battle Creek` = -0.656,
                          `Bear Creek` = -0.656, `Big Chico Creek` = -0.656, `Butte Creek` = -0.656,
                          `Clear Creek` = -0.656, `Cottonwood Creek` = -0.656, `Cow Creek` = -0.656,
                          `Deer Creek` = -0.656, `Elder Creek` = -0.656, `Mill Creek` = -0.656,
                          `Paynes Creek` = -0.656, `Stony Creek` = -0.656, `Thomes Creek` = -0.656,
                          `Upper-mid Sacramento River` = -0.656, `Sutter Bypass` = -0.656,
                          `Bear River` = -0.656, `Feather River` = -0.656, `Yuba River` = -0.656,
                          `Lower-mid Sacramento River` = -0.656, `Yolo Bypass` = -0.656, `American River` = -0.656,
                          `Lower Sacramento River` = -0.656, `Calaveras River` = -0.656, `Cosumnes River` = -0.656,
                          `Mokelumne River` = -0.656, `Merced River` = -0.656, `Stanislaus River` = -0.656,
                          `Tuolumne River` = -0.656, `San Joaquin River` = -0.656),
  ..surv_juv_rear_contact_points = 0.0194795,
  .surv_juv_rear_contact_points = -0.189, # from literature
  ..surv_juv_rear_prop_diversions = 0.1,
  .surv_juv_rear_prop_diversions = -3.51, # from literature
  ..surv_juv_rear_total_diversions = 0.3,
  .surv_juv_rear_total_diversions = -0.0021, # from literature
  .surv_juv_rear_avg_temp_thresh = -0.717,
  .surv_juv_rear_high_predation = -0.122,
  .surv_juv_rear_stranded = -1.939,
  .surv_juv_rear_medium = 1.48,
  .surv_juv_rear_large = 2.223,
  .surv_juv_rear_floodplain = 0.47,
  min_survival_rate = 0.0001,

  # Juvenile bypass survival calibrated parameters and coefficients
  ..surv_juv_bypass_int = -3.5,
  .surv_juv_bypass_avg_temp_thresh = -0.717,
  .surv_juv_bypass_high_predation = -0.122,
  .surv_juv_bypass_medium = 1.48,
  .surv_juv_bypass_large = 2.223,
  .surv_juv_bypass_floodplain = 0.47,

  # Juvenile delta survival calibrated parameters and coefficients
  ..surv_juv_delta_int = 1.49,
  ..surv_juv_delta_contact_points = 0.0194795,
  .surv_juv_delta_contact_points = -0.189, # from literature
  ..surv_juv_delta_total_diverted = 0.4820249,
  .surv_juv_delta_total_diverted = -0.0021, # from literature
  .surv_juv_delta_avg_temp_thresh = -0.717,
  .surv_juv_delta_high_predation = -0.122,
  .surv_juv_delta_prop_diverted = -3.51,
  .surv_juv_delta_medium = 1.48,
  .surv_juv_delta_large = 2.223,

  # San joaquin outmigration calibrated intercept and coefficents
  ..surv_juv_outmigration_sj_int = -3.0188308,
  .surv_juv_outmigration_san_joaquin_medium = 1.48,
  .surv_juv_outmigration_san_joaquin_large = 2.223,

  # Sac delta outmigration coefficients and variables
  .surv_juv_outmigration_sac_delta_intercept_one = -3.5,
  .surv_juv_outmigration_sac_delta_intercept_two =  0.3,
  .surv_juv_outmigration_sac_delta_intercept_three = -3.5,
  .surv_juv_outmigration_sac_delta_delta_flow = 0.0013,
  .surv_juv_outmigration_sac_delta_avg_temp = 0.386,
  .surv_juv_outmigration_sac_delta_perc_diversions = -0.033,
  .surv_juv_outmigration_sac_delta_medium = 1.48,
  .surv_juv_outmigration_sac_delta_large = 2.223,
  surv_juv_outmigration_sac_delta_model_weights = rep(1/3, 3),

  # Ocean entry success coefficient and variable
  ..ocean_entry_success_int =  c(
    `Upper Sacramento River` = -2.9839253,
    `Antelope Creek` = -2.9839253,
    `Battle Creek` = -2.9839253,
    `Bear Creek` = -2.9839253,
    `Big Chico Creek` = -2.9839253,
    `Butte Creek` = -2.9839253,
    `Clear Creek` = -2.9839253,
    `Cottonwood Creek` = -2.9839253,
    `Cow Creek` = -2.9839253,
    `Deer Creek` = -2.9839253,
    `Elder Creek` = -2.9839253,
    `Mill Creek` = -2.9839253,
    `Paynes Creek` = -2.9839253,
    `Stony Creek` = -2.9839253,
    `Thomes Creek` = -2.9839253,
    `Upper-mid Sacramento River` = -2.9839253,
    `Sutter Bypass` = -2.9839253,
    `Bear River` = -2.9839253,
    `Feather River` = -2.9839253,
    `Yuba River` = -2.9839253,
    `Lower-mid Sacramento River` = -2.9839253,
    `Yolo Bypass` = -2.9839253,
    `American River` = -2.9839253,
    `Lower Sacramento River` = -2.9839253,
    `Calaveras River` = -2.9839253,
    `Cosumnes River` = -2.9839253,
    `Mokelumne River` = -2.9839253,
    `Merced River` = -2.9839253,
    `Stanislaus River` = -2.9839253,
    `Tuolumne River` = -2.9839253,
    `San Joaquin River` = -2.9839253),
  .ocean_entry_success_length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
  .ocean_entry_success_months = 0.35,

  ## Variable from load baseline data
  # DSMflow variables -----
  freeport_flows = DSMflow::freeport_flow,
  vernalis_flows = DSMflow::vernalis_flow,
  stockton_flows = DSMflow::stockton_flow,
  CVP_exports = DSMflow::cvp_exports,
  SWP_exports = DSMflow::swp_exports,
  proportion_diverted = DSMflow::proportion_diverted,
  total_diverted = DSMflow::total_diverted,
  delta_proportion_diverted = DSMflow::delta_proportion_diverted,
  delta_total_diverted = DSMflow::delta_total_diverted,
  prop_pulse_flows = DSMflow::proportion_pulse_flows,
  prop_flow_natal = DSMflow::proportion_flow_natal,
  upper_sacramento_flows = DSMflow::upper_sacramento_flows,
  delta_inflow = DSMflow::delta_inflow,
  cc_gates_days_closed = DSMflow::delta_cross_channel_closed["count", ],
  cc_gates_prop_days_closed = DSMflow::delta_cross_channel_closed["proportion", ],
  proportion_flow_bypass = DSMflow::proportion_flow_bypasses,
  gates_overtopped = DSMflow::gates_overtopped,

  # DSMtemperature variables -----
  vernalis_temps = DSMtemperature::vernalis_temperature,
  prisoners_point_temps = DSMtemperature::prisoners_point_temperature,
  degree_days = DSMtemperature::degree_days,
  # mean_egg_temp_effect = DSMtemperature::egg_temperature_effect$winter_run, # winter run values is calibrated
  avg_temp = DSMtemperature::stream_temperature,
  avg_temp_delta = DSMtemperature::delta_temperature,
  migratory_temperature_proportion_over_20 = DSMtemperature::migratory_temperature_proportion_over_20,

  # DSMhabitat variables -----
  spawning_habitat = DSMhabitat::wr_spawn,
  inchannel_habitat_fry = DSMhabitat::wr_fry, # vary by run
  inchannel_habitat_juvenile = DSMhabitat::wr_juv, # vary by run
  floodplain_habitat = DSMhabitat::wr_fp, # vary by run
  weeks_flooded = DSMhabitat::weeks_flooded,
  delta_habitat = DSMhabitat::delta_habitat,
  sutter_habitat = DSMhabitat::sutter_habitat,
  yolo_habitat = DSMhabitat::yolo_habitat,
  tisdale_bypass_watershed = DSMhabitat::tisdale_bypass_watershed,
  yolo_bypass_watershed = DSMhabitat::yolo_bypass_watershed,
  south_delta_routed_watersheds = DSMhabitat::south_delta_routed_watersheds,
  prop_high_predation = DSMhabitat::prop_high_predation,
  contact_points = DSMhabitat::contact_points,
  delta_contact_points = DSMhabitat::delta_contact_points,
  delta_prop_high_predation = DSMhabitat::delta_prop_high_predation,
  prob_strand_early = DSMhabitat::prob_strand_early,
  prob_strand_late = DSMhabitat::prob_strand_late,
  prob_nest_scoured = DSMhabitat::prob_nest_scoured
)

usethis::use_data(params, overwrite = TRUE)






