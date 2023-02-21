# Fitness Function ------------------
fall_run_fitness <- function(
  known_adults,
  seeds,
  params,
  surv_adult_enroute,
  surv_adult_prespawn,
  surv_egg_to_fry,
  bypass_surv_juv,
  upsac_surv_juv,
  butte_surv_juv,
  clear_surv_juv,
  deer_surv_juv,
  mill_surv_juv,
  sac_surv_juv,
  feather_and_bear_surv_juv,
  yuba_surv_juv,
  american_surv_juv,
  deltatribs_surv_juv,
  moke_surv_juv,
  merced_surv_juv,
  stan_surv_juv,
  tuol_surv_juv,
  sj_surv_juv,
  surv_juv_rear_contact_points,
  surv_juv_rear_prop_diversions,
  surv_juv_rear_total_diversions,
  surv_juv_bypass_int,
  surv_juv_delta_int,
  surv_juv_delta_contact_points,
  surv_juv_delta_total_diverted,
  surv_juv_outmigration_sj_int,
  default_ocean_entry_surv,
  upsac_ocean_entry_surv,
  butte_ocean_entry_surv,
  deer_ocean_entry_surv,
  mill_ocean_entry_surv,
  midsactribs_ocean_entry_surv,
  yuba_ocean_entry_surv,
  american_ocean_entry_surv,
  deltatribs_ocean_entry_surv,
  moke_ocean_entry_surv,
  merced_ocean_entry_surv,
  stan_ocean_entry_surv,
  tuol_ocean_entry_surv) {

  params_init <- params

  params_init$..surv_adult_enroute_int = surv_adult_enroute
  params_init$..surv_adult_prespawn_int = surv_adult_prespawn
  params_init$..surv_egg_to_fry_int = surv_egg_to_fry
  params_init$..surv_juv_rear_int = c(`Upper Sacramento River` = upsac_surv_juv,
                                      `Antelope Creek` = deer_surv_juv,
                                      `Battle Creek` = deer_surv_juv,
                                      `Bear Creek` = deer_surv_juv,
                                      `Big Chico Creek` = deer_surv_juv,
                                      `Butte Creek` = butte_surv_juv,
                                      `Clear Creek` = clear_surv_juv,
                                      `Cottonwood Creek` = deer_surv_juv,
                                      `Cow Creek` = deer_surv_juv,
                                      `Deer Creek` = deer_surv_juv,
                                      `Elder Creek` = deer_surv_juv,
                                      `Mill Creek` = mill_surv_juv,
                                      `Paynes Creek` = deer_surv_juv,
                                      `Stony Creek` = deer_surv_juv,
                                      `Thomes Creek` = deer_surv_juv,
                                      `Upper-mid Sacramento River` = sac_surv_juv,
                                      `Sutter Bypass` = bypass_surv_juv,
                                      `Bear River` = feather_and_bear_surv_juv,
                                      `Feather River` = feather_and_bear_surv_juv,
                                      `Yuba River` = yuba_surv_juv,
                                      `Lower-mid Sacramento River` = sac_surv_juv,
                                      `Yolo Bypass` = bypass_surv_juv,
                                      `American River` = american_surv_juv,
                                      `Lower Sacramento River` = sac_surv_juv,
                                      `Calaveras River` = deltatribs_surv_juv,
                                      `Cosumnes River` = deltatribs_surv_juv,
                                      `Mokelumne River` = moke_surv_juv,
                                      `Merced River` = merced_surv_juv,
                                      `Stanislaus River` = stan_surv_juv,
                                      `Tuolumne River` = tuol_surv_juv,
                                      `San Joaquin River` = sj_surv_juv)
  params_init$..surv_juv_rear_contact_points = surv_juv_rear_contact_points
  params_init$..surv_juv_rear_prop_diversions = surv_juv_rear_prop_diversions
  params_init$..surv_juv_rear_total_diversions = surv_juv_rear_total_diversions
  params_init$..surv_juv_bypass_int = surv_juv_bypass_int
  params_init$..surv_juv_delta_int = surv_juv_delta_int
  params_init$..surv_juv_delta_contact_points = surv_juv_delta_contact_points
  params_init$..surv_juv_delta_total_diverted = surv_juv_delta_total_diverted
  params_init$..surv_juv_outmigration_sj_int = surv_juv_outmigration_sj_int
  params_init$..ocean_entry_success_int = c(
    `Upper Sacramento River` = upsac_ocean_entry_surv,
    `Antelope Creek` = default_ocean_entry_surv,
    `Battle Creek` = default_ocean_entry_surv,
    `Bear Creek` = default_ocean_entry_surv,
    `Big Chico Creek` = default_ocean_entry_surv,
    `Butte Creek` = butte_ocean_entry_surv,
    `Clear Creek` = default_ocean_entry_surv,
    `Cottonwood Creek` = default_ocean_entry_surv,
    `Cow Creek` = default_ocean_entry_surv,
    `Deer Creek` = deer_ocean_entry_surv,
    `Elder Creek` = default_ocean_entry_surv,
    `Mill Creek` = mill_ocean_entry_surv,
    `Paynes Creek` = default_ocean_entry_surv,
    `Stony Creek` = default_ocean_entry_surv,
    `Thomes Creek` = default_ocean_entry_surv,
    `Upper-mid Sacramento River` = default_ocean_entry_surv,
    `Sutter Bypass` = default_ocean_entry_surv,
    `Bear River` = midsactribs_ocean_entry_surv,
    `Feather River` = midsactribs_ocean_entry_surv,
    `Yuba River` = yuba_ocean_entry_surv,
    `Lower-mid Sacramento River` = default_ocean_entry_surv,
    `Yolo Bypass` = default_ocean_entry_surv,
    `American River` = american_ocean_entry_surv,
    `Lower Sacramento River` = default_ocean_entry_surv,
    `Calaveras River` = deltatribs_ocean_entry_surv,
    `Cosumnes River` = deltatribs_ocean_entry_surv,
    `Mokelumne River` = moke_ocean_entry_surv,
    `Merced River` = merced_ocean_entry_surv,
    `Stanislaus River` = stan_ocean_entry_surv,
    `Tuolumne River` = tuol_ocean_entry_surv,
    `San Joaquin River` = default_ocean_entry_surv)


  keep <- c(1,6,7,10,12,19,20,23,26:30)
  num_obs <- rowSums(!is.na(known_adults[keep, 6:20]))
  total_obs <- sum(!is.na(known_adults[keep, 6:20]))
  weights <- num_obs / total_obs


  tryCatch({
    preds <- fall_run_model(mode = "calibrate",
                            seeds = seeds,
                            stochastic = FALSE,
                            ..params = params_init)

    known_nats <- known_adults[keep, 6:20] * (1 - params_init$proportion_hatchery[keep])
    mean_escapent <-rowMeans(known_nats, na.rm = TRUE)

    sse <- sum(((preds[keep,] - known_nats)^2 * weights)/mean_escapent, na.rm = TRUE)

    return(sse)
  },
  error = function(e) return(1e12),
  warning = function(w) return(1e12)
  )
}

#
# fall_run_fitness(
#   known_adults = DSMCalibrationData::grandtab_observed$fall,
#   seeds = DSMCalibrationData::grandtab_imputed$fall,
#   params = params,
#   x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
#   x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
#   x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
#   x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
#   x[38], x[39], x[40]
# )
#
#
# params_init <- DSMCalibrationData::set_synth_years(fallRunDSM::params_2022)
# seeds <- DSMCalibrationData::grandtab_imputed$fall
#
# preds <- fall_run_model(mode = "calibrate",
#                         seeds = seeds,
#                         stochastic = FALSE,
#                         ..params = params_init)
#



