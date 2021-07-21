library(fallRunDSM)
library(GA)
library(tidyverse)

init_to_previous_calibration <- function(object) {
  x <- c(3, 3, 0.041, 3.5, -3.5, 1.5, -2.5, -2.9, -1.1092908, -3.5,
         3.5, 3.5, -3.5, 2.5, -1.2, 1.9999999, -0.2, -0.1081707, -3.4999959,
         -0.4, 0.0358, 0.05, 0.215, -3.5, 1.4, 0.0358, 0.5, -3.5, 1.2,
         -0.5108849, -3.3233638, -3.2304288, -3.4148335, -3.5, -3.5, -1.308341,
         -1.9841364, 2.5000007, -3.5, -3, -0.9)
  sapply(1:length(x), function(p) rnorm(object@popSize, mean = x[p]))
}

default_calibration_values <-
  as.numeric(c(
    params$..surv_adult_enroute_int,
    params$..surv_adult_prespawn_int,
    params$..surv_egg_to_fry_int,
    params$..surv_juv_rear_int["Antelope Creek"], # all upper sac use same
    params$..surv_juv_rear_int["Sutter Bypass"],
    params$..surv_juv_rear_int["Upper Sacramento River"],
    params$..surv_juv_rear_int["Butte Creek"],
    params$..surv_juv_rear_int["Deer Creek"],
    params$..surv_juv_rear_int["Mill Creek"],
    params$..surv_juv_rear_int["Upper-mid Sacramento River"],
    params$..surv_juv_rear_int["Bear River"],
    params$..surv_juv_rear_int["Feather River"],
    params$..surv_juv_rear_int["Yuba River"],
    params$..surv_juv_rear_int["American River"],
    params$..surv_juv_rear_int["Calaveras River"],
    params$..surv_juv_rear_int["Mokelumne River"],
    params$..surv_juv_rear_int["Merced River"],
    params$..surv_juv_rear_int["Stanislaus River"],
    params$..surv_juv_rear_int["Tuolumne River"],
    params$..surv_juv_rear_int["San Joaquin River"],
    params$..surv_juv_rear_contact_points,
    params$..surv_juv_rear_prop_diversions,
    params$..surv_juv_rear_total_diversions,
    params$..surv_juv_bypass_int,
    params$..surv_juv_delta_int,
    params$..surv_juv_delta_contact_points,
    params$..surv_juv_delta_total_diverted,
    params$..surv_juv_outmigration_sj_int,
    params$..ocean_entry_success_int["Battle Creek"],
    params$..ocean_entry_success_int["Upper Sacramento River"],
    params$..ocean_entry_success_int["Butte Creek"],
    params$..ocean_entry_success_int["Deer Creek"],
    params$..ocean_entry_success_int["Mill Creek"],
    params$..ocean_entry_success_int["Bear River"],
    params$..ocean_entry_success_int["Yuba River"],
    params$..ocean_entry_success_int["American River"],
    params$..ocean_entry_success_int["Calaveras River"],
    params$..ocean_entry_success_int["Mokelumne River"],
    params$..ocean_entry_success_int["Merced River"],
    params$..ocean_entry_success_int["Stanislaus River"],
    params$..ocean_entry_success_int["Tuolumne River"]
  ))


# watersheds without calibration
remove_these <- names(which(is.na(DSMCalibrationData::grandtab_observed$fall[, 1])))

# Fitness Function ------------------
fall_run_fitness <- function(
  known_adults,
  seeds,
  params,
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


  set.seed(10)
  preds <- fall_run_model(mode = "calibrate",
                          seeds = seeds,
                          ..params = params_init)

  sum(((preds$natural_spawners - (known_adults * preds$proportion_natural)) /
         rowMeans(known_adults, na.rm = TRUE)) ^ 2, na.rm = TRUE)
}


# res <- fall_run_fitness(known_adults = DSMCalibrationData::grandtab_observed$fall,
#                  seeds = DSMCalibrationData::grandtab_imputed$fall,
#                  params = fallRunDSM::params,
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
#                  0.147390023805201)

# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -fall_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$fall,
              seeds = DSMCalibrationData::grandtab_imputed$fall,
              params = fallRunDSM::params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
              x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
              x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
              x[38], x[39], x[40], x[41]
            ),
          lower = rep(-5, 41),
          upper = rep(5, 41),
          popSize = 10,
          maxiter = 6,
          run = 20,
          parallel = TRUE)

res@population

update_params <- function(x, params) {

  surv_adult_enroute = x[1]
  surv_adult_prespawn = x[2]
  surv_egg_to_fry = x[3]
  uppermidsactribs_surv_juv = x[4]
  bypass_surv_juv = x[5]
  upsac_surv_juv = x[6]
  butte_surv_juv = x[7]
  deer_surv_juv = x[8]
  mill_surv_juv = x[9]
  sac_surv_juv = x[10]
  bear_surv_juv = x[11]
  feather_surv_juv = x[12]
  yuba_surv_juv = x[13]
  american_surv_juv = x[14]
  deltatribs_surv_juv = x[15]
  moke_surv_juv = x[16]
  merced_surv_juv = x[17]
  stan_surv_juv = x[18]
  tuol_surv_juv = x[19]
  sj_surv_juv = x[20]
  surv_juv_rear_contact_points = x[21]
  surv_juv_rear_prop_diversions = x[22]
  surv_juv_rear_total_diversions = x[23]
  surv_juv_bypass_int = x[24]
  surv_juv_delta_int = x[25]
  surv_juv_delta_contact_points = x[26]
  surv_juv_delta_total_diverted = x[27]
  surv_juv_outmigration_sj_int = x[28]
  default_ocean_entry_surv = x[29]
  upsac_ocean_entry_surv = x[30]
  butte_ocean_entry_surv = x[31]
  deer_ocean_entry_surv = x[32]
  mill_ocean_entry_surv = x[33]
  midsactribs_ocean_entry_surv = x[34]
  yuba_ocean_entry_surv = x[35]
  american_ocean_entry_surv = x[36]
  deltatribs_ocean_entry_surv = x[37]
  moke_ocean_entry_surv = x[38]
  merced_ocean_entry_surv = x[39]
  stan_ocean_entry_surv = x[40]
  tuol_ocean_entry_surv = x[41]

  params$..surv_adult_enroute_int = surv_adult_enroute
  params$..surv_adult_prespawn_int = surv_adult_prespawn
  params$..surv_egg_to_fry_int = surv_egg_to_fry
  params$..surv_juv_rear_int = c(`Upper Sacramento River` = upsac_surv_juv,
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
  params$..surv_juv_rear_contact_points = surv_juv_rear_contact_points
  params$..surv_juv_rear_prop_diversions = surv_juv_rear_prop_diversions
  params$..surv_juv_rear_total_diversions = surv_juv_rear_total_diversions
  params$..surv_juv_bypass_int = surv_juv_bypass_int
  params$..surv_juv_delta_int = surv_juv_delta_int
  params$..surv_juv_delta_contact_points = surv_juv_delta_contact_points
  params$..surv_juv_delta_total_diverted = surv_juv_delta_total_diverted
  params$..surv_juv_outmigration_sj_int = surv_juv_outmigration_sj_int
  params$..ocean_entry_success_int = c(
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

  return(params)

}


# Run 1: ------------------------------------
r1_solution <- res@solution

r1_params <- update_params(x = r1_solution, fallRunDSM::params)

r1_seeds <- fall_run_model(mode = "seed", ..params = r1_params)
r1_sim <- fall_run_model(seeds = r1_seeds, mode = "simulate", ..params = r1_params)

r1_nat_spawners <- as_tibble(r1_sim$natural_spawners) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated") %>%
  filter(!(watershed %in% remove_these))


r1_observed <- as_tibble(r1_sim$proportion_natural * DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed") %>%
  filter(!is.na(spawners),
         !(watershed %in% remove_these))

r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed) %>%
  mutate(year = as.numeric(year))


r1_eval_df %>%
  ggplot(aes(year, spawners, color = type)) + geom_point() + facet_wrap(~watershed, scales = "free_y")

r1_eval_df %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() #+ facet_wrap(~watershed, scales = "free")

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated), observed <= 50000) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated), observed <= 50000) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )


# Run2: with habitat scaling applied after calibration


vect2 <- c(2.0000000, 0.5059781, 1.6702959, 0.8441507, 1.6434544, 2.0000000, 0.5000000,
         1.0815585, 1.9624035, 0.6232790, 1.0783194, 1.9318056, 1.2704583, 0.9537940,
         0.9066874, 2.0000000, 1.0847540, 1.4589099, 2.0000000, 0.5769185, 1.0589013,
         0.5709694, 2.0000000, 0.6716419, 0.5237730, 1.8253104, 1.0990632, 2.0000000,
         1.4615010, 1.1809537, 0.9577044, 0.9697722, 1.1437721, 1.7819260)

modified_hab_params <- fallRunDSM::params


modified_hab_params$spawning_habitat[1,,]<-modified_hab_params$spawning_habitat[1,,]*vect2[1] # Upper Sac
modified_hab_params$spawning_habitat[6,,]<-modified_hab_params$spawning_habitat[6,,]*vect2[2] #Butte
modified_hab_params$spawning_habitat[7,,]<-modified_hab_params$spawning_habitat[7,,]*vect2[3] #Clear
modified_hab_params$spawning_habitat[10,,]<-modified_hab_params$spawning_habitat[10,,]*vect2[4] # Deer
modified_hab_params$spawning_habitat[12,,]<-modified_hab_params$spawning_habitat[12,,]*vect2[5] # Mill
modified_hab_params$spawning_habitat[19,,]<-modified_hab_params$spawning_habitat[19,,]*vect2[6] # Feather
modified_hab_params$spawning_habitat[20,,]<-modified_hab_params$spawning_habitat[20,,]*vect2[7]# Yuba
modified_hab_params$spawning_habitat[23,,]<-modified_hab_params$spawning_habitat[23,,]*vect2[8] # American
modified_hab_params$spawning_habitat[26,,]<-modified_hab_params$spawning_habitat[26,,]*vect2[9] # Cosumness
modified_hab_params$spawning_habitat[27,,]<-modified_hab_params$spawning_habitat[27,,]*vect2[10] # Mokelumne
modified_hab_params$spawning_habitat[28,,]<-modified_hab_params$spawning_habitat[28,,]*vect2[11] # Merced
modified_hab_params$spawning_habitat[29,,]<-modified_hab_params$spawning_habitat[29,,]*vect2[12] # Stanislaus
modified_hab_params$spawning_habitat[30,,]<-modified_hab_params$spawning_habitat[30,,]*vect2[13] # Tuolumne



modified_hab_params$inchannel_habitat_fry[1,,]<-modified_hab_params$inchannel_habitat_fry[1,,]*vect2[14] # Upper Sac
modified_hab_params$inchannel_habitat_fry[6,,]<-modified_hab_params$inchannel_habitat_fry[6,,]*vect2[15] # Butte
modified_hab_params$inchannel_habitat_fry[7,,]<-modified_hab_params$inchannel_habitat_fry[7,,]*vect2[16] # Clear
modified_hab_params$inchannel_habitat_fry[10,,]<-modified_hab_params$inchannel_habitat_fry[10,,]*vect2[17] # Deer
modified_hab_params$inchannel_habitat_fry[12,,]<-modified_hab_params$inchannel_habitat_fry[12,,]*vect2[18] # Mill
modified_hab_params$inchannel_habitat_fry[16,,]<-modified_hab_params$inchannel_habitat_fry[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
# Sutter (corridor for above) is changed below
modified_hab_params$inchannel_habitat_fry[19,,]<-modified_hab_params$inchannel_habitat_fry[19,,]*vect2[20] # Feather
modified_hab_params$inchannel_habitat_fry[20,,]<-modified_hab_params$inchannel_habitat_fry[20,,]*vect2[21] # Yuba
modified_hab_params$inchannel_habitat_fry[21,,]<-modified_hab_params$inchannel_habitat_fry[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
# Yolo (corridor for above) is changed below
modified_hab_params$inchannel_habitat_fry[23,,]<-modified_hab_params$inchannel_habitat_fry[23,,]*vect2[23] # American
modified_hab_params$inchannel_habitat_fry[24,,]<-modified_hab_params$inchannel_habitat_fry[24,,]*vect2[24] # Lower Sac (corridor for above)
modified_hab_params$inchannel_habitat_fry[26,,]<-modified_hab_params$inchannel_habitat_fry[26,,]*vect2[25] # Cosumness
modified_hab_params$inchannel_habitat_fry[27,,]<-modified_hab_params$inchannel_habitat_fry[27,,]*vect2[26] # Mokelumne
modified_hab_params$inchannel_habitat_fry[28,,]<-modified_hab_params$inchannel_habitat_fry[28,,]*vect2[27] # Merced
modified_hab_params$inchannel_habitat_fry[29,,]<-modified_hab_params$inchannel_habitat_fry[29,,]*vect2[28] # Stanislaus
modified_hab_params$inchannel_habitat_fry[30,,]<-modified_hab_params$inchannel_habitat_fry[30,,]*vect2[29] # Tuolumne
modified_hab_params$inchannel_habitat_fry[31,,]<-modified_hab_params$inchannel_habitat_fry[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)



modified_hab_params$inchannel_habitat_juvenile[1,,]<-modified_hab_params$inchannel_habitat_juvenile[1,,]*vect2[14] # Upper Sac
modified_hab_params$inchannel_habitat_juvenile[6,,]<-modified_hab_params$inchannel_habitat_juvenile[6,,]*vect2[15] # Butte
modified_hab_params$inchannel_habitat_juvenile[7,,]<-modified_hab_params$inchannel_habitat_juvenile[7,,]*vect2[16] # Clear
modified_hab_params$inchannel_habitat_juvenile[10,,]<-modified_hab_params$inchannel_habitat_juvenile[10,,]*vect2[17] # Deer
modified_hab_params$inchannel_habitat_juvenile[12,,]<-modified_hab_params$inchannel_habitat_juvenile[12,,]*vect2[18] # Mill
modified_hab_params$inchannel_habitat_juvenile[16,,]<-modified_hab_params$inchannel_habitat_juvenile[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
# Sutter (corridor for above) is changed below
modified_hab_params$inchannel_habitat_juvenile[19,,]<-modified_hab_params$inchannel_habitat_juvenile[19,,]*vect2[20] # Feather
modified_hab_params$inchannel_habitat_juvenile[20,,]<-modified_hab_params$inchannel_habitat_juvenile[20,,]*vect2[21] # Yuba
modified_hab_params$inchannel_habitat_juvenile[21,,]<-modified_hab_params$inchannel_habitat_juvenile[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
# Yolo (corridor for above) is changed below
modified_hab_params$inchannel_habitat_juvenile[23,,]<-modified_hab_params$inchannel_habitat_juvenile[23,,]*vect2[23] # American
modified_hab_params$inchannel_habitat_juvenile[24,,]<-modified_hab_params$inchannel_habitat_juvenile[24,,]*vect2[24] # Lower Sac (corridor for above)
modified_hab_params$inchannel_habitat_juvenile[26,,]<-modified_hab_params$inchannel_habitat_juvenile[26,,]*vect2[25] # Cosumness
modified_hab_params$inchannel_habitat_juvenile[27,,]<-modified_hab_params$inchannel_habitat_juvenile[27,,]*vect2[26] # Mokelumne
modified_hab_params$inchannel_habitat_juvenile[28,,]<-modified_hab_params$inchannel_habitat_juvenile[28,,]*vect2[27] # Merced
modified_hab_params$inchannel_habitat_juvenile[29,,]<-modified_hab_params$inchannel_habitat_juvenile[29,,]*vect2[28] # Stanislaus
modified_hab_params$inchannel_habitat_juvenile[30,,]<-modified_hab_params$inchannel_habitat_juvenile[30,,]*vect2[29] # Tuolumne
modified_hab_params$inchannel_habitat_juvenile[31,,]<-modified_hab_params$inchannel_habitat_juvenile[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)

modified_hab_params$sutter_habitat<-modified_hab_params$sutter_habitat*vect2[31]
modified_hab_params$yolo_habitat<-modified_hab_params$yolo_habitat*vect2[32]
modified_hab_params$delta_habitat[,,"North Delta"]<-modified_hab_params$delta_habitat[,,"North Delta"]*vect2[33]
modified_hab_params$delta_habitat[,,"South Delta"]<-modified_hab_params$delta_habitat[,,"South Delta"]*vect2[34]


r2_seeds <- fall_run_model(mode = "seed", ..params = modified_hab_params)
r2_sim <- fall_run_model(seeds = r2_seeds, mode = "simulate", ..params = modified_hab_params)


r2_nat_spawners <- as_tibble(r2_sim$natural_spawners) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated") %>%
  filter(!(watershed %in% remove_these))


r2_observed <- as_tibble(r2_sim$proportion_natural * DSMCalibrationData::grandtab_observed$fall) %>%
  mutate(watershed = DSMscenario::watershed_labels) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed") %>%
  filter(!is.na(spawners),
         !(watershed %in% remove_these))

r2_eval <- bind_rows(r2_nat_spawners, r2_observed) %>%
  mutate(year = as.numeric(year))

r2_eval %>%
  ggplot(aes(year, spawners, color = type)) + geom_point() + facet_wrap(~watershed, scales = "free_y")

r2_eval %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() #+ facet_wrap(~watershed, scales = "free")

r2_eval %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

r2_eval %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

# apply ha

og_eval <- read_rds("~/projects/og-eval.rds") %>%
  mutate(kind = "original")


comparison <- bind_rows(updated_eval_df, og_eval)


comparison %>%
  filter(type == "simulated") %>%
  ggplot(aes(year, spawners, color = kind)) + geom_point() + facet_wrap(~watershed, scales = "free") +
  geom_point(data = comparison %>% filter(type == "observed", kind == "original"),
            aes(year, spawners), color = "black")














