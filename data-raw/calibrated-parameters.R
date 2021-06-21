library(tidyverse)

vect <- c(3.5000000 , #inchannel default intercept
          1.5000000, # upper sacramento intercept
          -2.5000000, # butte creek intercept
          -2.9000000, # deer creek intercept
          -1.1092908, # mill creek intercept
          -3.5000000, # sacramento sections 1,2,3 intercepts
          3.5000000, # middle sac tribs
          -3.5000000, # ?
          2.5000000, # american river intercept
          -1.2000000, # delta tribs, calaveras and cosumnes
          1.9999999, # moke int
          -0.2000000, # merced int
          -0.1081707, # stan intercept
          -3.4999959, # tuol intercept
          -0.4000000, # sj main intercept
          -3.5000000, # bypass intercept
          1.4000000, # delta intercapt
          -3.5000000,
          2.5000000,
          0.3000000,
          -3.5000000,
          0.3000000,
          -3.5000000,
          1.2000000,
          -0.5108849,
          -3.3233638,
          -3.2304288,
          -3.4148335,
          -3.5000000,
          -3.5000000,
          -1.3083410,
          -1.9841364,
          2.5000007,
          -3.5000000,
          -3.0000000,
          -0.9000000
)

surv.adj <- rep(1, 31)
surv.adj[c(2, 4, 5, 8, 9, 11, 14, 13, 15, 18)] <- 0.025
surv.adj[c(8)] <- 0.50
surv.adj[c(9)] <- 0.25


# Create a dataframe that contains the watersheds and corresponding betas for each
index_ws_to_update <- c(1, 6, 10, 12, 16, 21, 24, 18, 19, 20, 23, 25, 26, 27, 28, 29, 30, 31, 22, 17)
`2nd calibration adjustment` <- c(vect[2:5], rep(vect[6], 3), vect[7], vect[7], vect[8],
                                  vect[9], vect[10], vect[10], vect[11], vect[12],
                                  vect[13], vect[14], vect[15], vect[16], vect[16])

beta_to_update <- tibble(
  order = index_ws_to_update,
  surv_juv_rear_int = `2nd calibration adjustment`
)

survival_betas <- cvpiaData::watershed_ordering %>%
  left_join(beta_to_update) %>%
  mutate(surv_juv_rear_int = ifelse(is.na(surv_juv_rear_int),
                                               3.5, surv_juv_rear_int)) %>%
  add_column(`average temperature` = -0.717,
             predation = -0.122,
             `contact points` = 0.0358,
             `contact points scaler` = -0.189,
             `proportion diverted` = 0.05,
             `proportion diverted scaler` = -3.51,
             `total diverted` = 0.215,
             `total diverted scaler` = -0.0021,
             stranded = -1.939,
             medium = 1.48,
             large = 2.223,
             `floodplain habitat` = 0.47,
             `survival adjustments` = surv.adj) %>%
  mutate(
    contact_points_coeff = .0358 * -.189,
    proportion_diverted_coeff = 0.05 * -3.51,
    total_diverted_coeff = 0.215 * -0.0021
  )

usethis::use_data(survival_betas, overwrite = TRUE)

# TODO make the model use these tibbles!

# delta_survival_betas <- tibble(
#   watershed = c("North Delta", "South Delta"),
#   intercept = 1.4, # vect[17]
#   `avg temp thresh` = -0.717,
#   predation = -0.122,
#   contact = 0.0358 * -0.189,
#   `prop diversions` = -3.51,
#   `total diversions` = 0.5 * -0.0021,
#   medium = 1.48,
#   large = 2.223
# )
#
# usethis::use_data(delta_survival_betas, overwrite = TRUE)
#
# outmigration_survival_betas <- tibble(
#   `intercept 1` = 2.5, flow = 0.0092,
#   `proportion diversion` = -3.51 * 0.05,
#   `total diversion` = -0.0021 * 0.215,
#   `intercept 2` = 0.3,
#   `average temperature` = 0.554,
#   `model weight` = .5,
#   medium = 1.48, large = 2.223
# )


# 2019 Calibration Parameters ------------
params <- list(

  # adults

  # growth

  # rear

  ..surv_adult_enroute_int = 3,
  ..surv_adult_prespawn_int = 3,
  ..surv_egg_to_fry_int = 0.041,
  ..surv_juv_rear_int = c(`Upper Sacramento River` = 1.5, `Antelope Creek` = 3.5, `Battle Creek` = 3.5,
                          `Bear Creek` = 3.5, `Big Chico Creek` = 3.5, `Butte Creek` = -2.5,
                          `Clear Creek` = 3.5, `Cottonwood Creek` = 3.5, `Cow Creek` = 3.5,
                          `Deer Creek` = -2.9, `Elder Creek` = 3.5, `Mill Creek` = -1.1092908,
                          `Paynes Creek` = 3.5, `Stony Creek` = 3.5, `Thomes Creek` = 3.5,
                          `Upper-mid Sacramento River` = -3.5, `Sutter Bypass` = -3.5,
                          `Bear River` = 3.5, `Feather River` = 3.5, `Yuba River` = -3.5,
                          `Lower-mid Sacramento River` = -3.5, `Yolo Bypass` = -3.5, `American River` = 2.5,
                          `Lower Sacramento River` = -3.5, `Calaveras River` = -1.2, `Cosumnes River` = -1.2,
                          `Mokelumne River` = 1.9999999, `Merced River` = -0.2, `Stanislaus River` = -0.1081707,
                          `Tuolumne River` = -3.4999959, `San Joaquin River` = -0.4),
  ..surv_juv_rear_contact_points = -0.0068,
  ..surv_juv_rear_prop_diversions = -0.1755,
  ..surv_juv_rear_total_diversions = -0.0005,
  ..surv_juv_bypass_int = -3.5,
  ..surv_juv_delta_int = 1.4,
  ..surv_juv_delta_contact_points = (.0358 * -0.189),
  ..surv_juv_delta_total_diverted = (.5 * -0.0021),
  ..surv_juv_outmigration_sj_int = -3.5,
  ..surv_juv_outmigration_sac_int_one = 2.5,
  ..surv_juv_outmigration_sac_prop_diversions = (-3.51 * .05),
  ..surv_juv_outmigration_sac_total_diversions = (-.0021 * .215),
  ..surv_juv_outmigration_sac_int_two = 0.3,
  ..ocean_entry_success_int = c(
    `Upper Sacramento River` = -0.5108849,
    `Antelope Creek` = 1.2,
    `Battle Creek` = 1.2,
    `Bear Creek` = 1.2,
    `Big Chico Creek` = 1.2,
    `Butte Creek` = -3.3233638,
    `Clear Creek` = 1.2,
    `Cottonwood Creek` = 1.2,
    `Cow Creek` = 1.2,
    `Deer Creek` = -3.2304288,
    `Elder Creek` = 1.2,
    `Mill Creek` = -3.4148335,
    `Paynes Creek` = 1.2,
    `Stony Creek` = 1.2,
    `Thomes Creek` = 1.2,
    `Upper-mid Sacramento River` = 1.2,
    `Sutter Bypass` = 1.2,
    `Bear River` = -3.5,
    `Feather River` = -3.5,
    `Yuba River` = -3.5,
    `Lower-mid Sacramento River` = 1.2,
    `Yolo Bypass` = 1.2,
    `American River` = -1.308341,
    `Lower Sacramento River` = 1.2,
    `Calaveras River` = -1.9841364,
    `Cosumnes River` = -1.9841364,
    `Mokelumne River` = 2.5000007,
    `Merced River` = -3.5,
    `Stanislaus River` = -3,
    `Tuolumne River` = -0.9,
    `San Joaquin River` = 1.2)
)

usethis::use_data(params, overwrite = TRUE)






