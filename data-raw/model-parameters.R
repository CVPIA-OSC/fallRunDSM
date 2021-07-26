library(tidyverse)

# 2019 Calibration Parameters ------------
params <- list(

  # Data from springRunDSM cache-data (values vary by run)
  hatchery_allocation = springRunDSM::hatchery_allocation,
  natural_adult_removal_rate = springRunDSM::natural_adult_removal_rate,
  proportion_hatchery = springRunDSM::proportion_hatchery,
  month_return_proportions = springRunDSM::month_return_proportions,
  survival_betas = springRunDSM::survival_betas,
  growth_rates = springRunDSM::growth_rates_inchannel,
  growth_rates_floodplain = springRunDSM::growth_rates_floodplain,
  mass_by_size_class = springRunDSM::mass_by_size_class,
  cross_channel_stray_rate = springRunDSM::cross_channel_stray_rate,
  stray_rate = springRunDSM::stray_rate,
  adult_harvest_rate = springRunDSM::adult_harvest_rate,
  diversity_group = springRunDSM::diversity_group,

  # Coefficients for adult submodules
  # stray
  .adult_stray_intercept = 3,
  .adult_stray_wild = -5.5,
  .adult_stray_natal_flow = -1.99,
  .adult_stray_cross_channel_gates_closed = -0.174,
  .adult_stray_prop_bay_trans = 2.09,
  .adult_stray_prop_delta_trans = 2.89,
  # Enroute survival
  ..surv_adult_enroute_int = 3,
  .adult_en_route_migratory_temp = -0.26,
  .adult_en_route_bypass_overtopped = -0.019,
  .adult_en_route_adult_harvest_rate = springRunDSM::adult_harvest_rate, # varies by run
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

  # Egg to fry survival calubrated parameters and coefficents
  ..surv_egg_to_fry_int = 0.041,
  .surv_egg_to_fry_proportion_natural = 0.533,
  .surv_egg_to_fry_scour = -0.655,

  # Juvenile rearing survival coefficients and variables
  ..surv_juv_rear_int = c(`Upper Sacramento River` = 1.5, `Antelope Creek` = -2.25097915, `Battle Creek` = 2.79189516,
                          `Bear Creek` = -2.25097915, `Big Chico Creek` = -2.25097915, `Butte Creek` = -0.70866158,
                          `Clear Creek` = 	2.79189516, `Cottonwood Creek` = -2.25097915, `Cow Creek` = -2.25097915,
                          `Deer Creek` = -2.31387713, `Elder Creek` = -2.25097915, `Mill Creek` = 1.8664113,
                          `Paynes Creek` = -2.25097915, `Stony Creek` = -2.25097915, `Thomes Creek` = -2.25097915,
                          `Upper-mid Sacramento River` = -2.56726844, `Sutter Bypass` = -2.25097915,
                          `Bear River` = -2.25097915, `Feather River` = 	-0.5542307, `Yuba River` = -3.49999933,
                          `Lower-mid Sacramento River` = 	-2.56726844, `Yolo Bypass` = -2.25097915, `American River` = -2.25097915,
                          `Lower Sacramento River` = -2.56726844, `Calaveras River` = -2.25097915, `Cosumnes River` = -2.25097915,
                          `Mokelumne River` = -2.25097915, `Merced River` = -2.25097915, `Stanislaus River` = -2.25097915,
                          `Tuolumne River` = -2.25097915, `San Joaquin River` = 2.10420292),
  .surv_juv_rear_contact_points = -0.189, # from literature
  ..surv_juv_rear_contact_points = 0.09999992,
  .surv_juv_rear_prop_diversions = -3.51, # from literature
  ..surv_juv_rear_prop_diversions = 0.01000010,
  .surv_juv_rear_total_diversions = -0.0021, # from literature
  ..surv_juv_rear_total_diversions = 0.19126503,
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

  # Juvenile delta survival coefficients and variables
  ..surv_juv_delta_int = 1.42642277,
  ..surv_juv_delta_contact_points = 0.09999992,
  .surv_juv_delta_contact_points = -0.189, # from literature
  ..surv_juv_delta_total_diverted = 0.61104442,
  .surv_juv_delta_total_diverted = -0.0021, # from literature
  .surv_juv_delta_avg_temp_thresh = -0.717,
  .surv_juv_delta_high_predation = -0.122,
  .surv_juv_delta_prop_diverted = -3.51,
  .surv_juv_delta_medium = 1.48,
  .surv_juv_delta_large = 2.223,

  # San joaquin outmigration calibrated intercept and coefficents
  ..surv_juv_outmigration_sj_int = -3.5,
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
  ..ocean_entry_success_int = c(
    `Upper Sacramento River` = -3.49954625,
    `Antelope Creek` = -3.49954625,
    `Battle Creek` = -2.59452699,
    `Bear Creek` = -3.49954625,
    `Big Chico Creek` = -3.49954625,
    `Butte Creek` = -1.5380522,
    `Clear Creek` = -2.59452699,
    `Cottonwood Creek` = -3.49954625,
    `Cow Creek` = -3.49954625,
    `Deer Creek` = -1.49855839,
    `Elder Creek` = -3.49954625,
    `Mill Creek` = -3.22990407,
    `Paynes Creek` = -3.49954625,
    `Stony Creek` = -3.49954625,
    `Thomes Creek` = -3.49954625,
    `Upper-mid Sacramento River` = -3.49954625,
    `Sutter Bypass` = -3.49954625,
    `Bear River` = 2.49974122,
    `Feather River` = 2.49974122,
    `Yuba River` = -2.96201071,
    `Lower-mid Sacramento River` = -3.49954625,
    `Yolo Bypass` = -3.49954625,
    `American River` = -3.49954625,
    `Lower Sacramento River` = -3.49954625,
    `Calaveras River` = -3.49954625,
    `Cosumnes River` = -3.49954625,
    `Mokelumne River` = -3.49954625,
    `Merced River` = -3.49954625,
    `Stanislaus River` = -3.49954625,
    `Tuolumne River` = -3.49954625,
    `San Joaquin River` = -3.49954625),
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
  mean_egg_temp_effect = DSMtemperature::egg_temperature_effect$spring_run,
  avg_temp = DSMtemperature::stream_temperature,
  avg_temp_delta = DSMtemperature::delta_temperature,
  migratory_temperature_proportion_over_20 = DSMtemperature::migratory_temperature_proportion_over_20,

  # DSMhabitat variables -----
  spawning_habitat = DSMhabitat::sr_spawn,
  inchannel_habitat_fry = DSMhabitat::sr_fry, # vary by run
  inchannel_habitat_juvenile = DSMhabitat::sr_juv, # vary by run
  floodplain_habitat = DSMhabitat::sr_fp, # vary by run
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
  prob_nest_scoured = DSMhabitat::prob_nest_scoured)

usethis::use_data(params, overwrite = TRUE)






