library(fallRunDSM)

fall_run_fitness <- function(
  known_adults,
  seeds,
  surv_adult_enroute,
  surv_adult_prespawn,
  surv_egg_to_fry,
  uppermidsactribs_surv_juv,
  bypass_surv_juv,
  upsac_surv_juv,
  butte_surv_juv,
  deer_surv_juv,
  mill_surv_juv,
  sac_surv_juv,
  bear_surv_juv,
  feather_surv_juv,
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
  surv_juv_outmigration_sac_int_one,
  surv_juv_outmigration_sac_prop_diversions,
  surv_juv_outmigration_sac_total_diversions,
  surv_juv_outmigration_sac_int_two,
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

  params_init <- fallRunDSM::params

  params_init$..surv_adult_enroute_int = surv_adult_enroute
  params_init$..surv_adult_prespawn_int = surv_adult_prespawn
  params_init$..surv_egg_to_fry_int = surv_egg_to_fry
  params_init$..surv_juv_rear_int = c(`Upper Sacramento River` = upsac_surv_juv,
                                      `Antelope Creek` = uppermidsactribs_surv_juv,
                                      `Battle Creek` = uppermidsactribs_surv_juv,
                                      `Bear Creek` = uppermidsactribs_surv_juv,
                                      `Big Chico Creek` = uppermidsactribs_surv_juv,
                                      `Butte Creek` = butte_surv_juv,
                                      `Clear Creek` = uppermidsactribs_surv_juv,
                                      `Cottonwood Creek` = uppermidsactribs_surv_juv,
                                      `Cow Creek` = uppermidsactribs_surv_juv,
                                      `Deer Creek` = deer_surv_juv,
                                      `Elder Creek` = uppermidsactribs_surv_juv,
                                      `Mill Creek` = mill_surv_juv,
                                      `Paynes Creek` = uppermidsactribs_surv_juv,
                                      `Stony Creek` = uppermidsactribs_surv_juv,
                                      `Thomes Creek` = uppermidsactribs_surv_juv,
                                      `Upper-mid Sacramento River` = sac_surv_juv,
                                      `Sutter Bypass` = bypass_surv_juv,
                                      `Bear River` = uppermidsactribs_surv_juv,
                                      `Feather River` = uppermidsactribs_surv_juv,
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
  params_init$..surv_juv_outmigration_sac_int_one = surv_juv_outmigration_sac_int_one
  params_init$..surv_juv_outmigration_sac_prop_diversions = surv_juv_outmigration_sac_prop_diversions
  params_init$..surv_juv_outmigration_sac_total_diversions = surv_juv_outmigration_sac_total_diversions
  params_init$..surv_juv_outmigration_sac_int_two = surv_juv_outmigration_sac_int_two
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


  set.seed(1)
  preds <- fall_run_model(mode = "calibrate",
                          seeds = seeds,
                          ..params = params_init)

  sum(((preds$natural_spawners - (known_adults * preds$proportion_natural)) /
         rowMeans(known_adults, na.rm = TRUE)) ^ 2, na.rm = TRUE)
}


# for debugging

# res <- fall_run_fitness(known_adults = DSMCalibrationData::grandtab_observed$fall,
#                  seeds = DSMCalibrationData::grandtab_imputed$fall,
#                  0.682013715617359, 0.033827755600214, 0.541648151818663, 0.10156600503251,
#                  0.706047684885561, 0.706645149504766, 0.771451650885865, 0.21954283840023,
#                  0.734743040753528, 0.358398008625954, 0.568429754115641, 0.0235489930491894,
#                  0.692907620454207, 0.0584940805565566, 0.203661513281986, 0.890296793077141,
#                  0.768882901174948, 0.0739139886572957, 0.899652387015522, 0.685083156684414,
#                  0.614324184600264, 0.333454821724445, 0.0608288159128278, 0.67604007339105,
#                  0.448921357048675, 0.215740179643035, 0.476902798516676, 0.0553760717157274,
#                  0.489008299307898, 0.311485706595704, 0.267402980942279, 0.572966693434864,
#                  0.684153178008273, 0.247276975074783, 0.11046555894427, 0.513229911681265,
#                  0.349454613868147, 0.525649377843365, 0.600788121810183, 0.978078227257356,
#                  0.147390023805201, 0.585017720935866, 0.692717517493293, 0.365571736823767,
#                  0.83275669044815)
#
#




library(GA)

res <- ga(type = "real-valued", fitness = function(x) -fall_run_fitness(
  known_adults = DSMCalibrationData::grandtab_observed$fall,
  seeds = DSMCalibrationData::grandtab_imputed$fall,
  x[1],
  x[2],
  x[3],
  x[4],
  x[5],
  x[6],
  x[7],
  x[8],
  x[9],
  x[10],
  x[11],
  x[12],
  x[13],
  x[14],
  x[15],
  x[16],
  x[17],
  x[18],
  x[19],
  x[20],
  x[21],
  x[22],
  x[23],
  x[24],
  x[25],
  x[26],
  x[27],
  x[28],
  x[29],
  x[30],
  x[31],
  x[32],
  x[33],
  x[34],
  x[35],
  x[36],
  x[37],
  x[38],
  x[39],
  x[40],
  x[41],
  x[42],
  x[43],
  x[44],
  x[45]), lower = rep(-3.5, 45), upper = rep(3.5, 45), popSize = 100, maxiter = 10, run = 20, parallel = TRUE, mutation = gareal_nraMutation)
