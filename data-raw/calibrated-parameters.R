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
  `2nd calibration adjustment`
)

survival_betas <- cvpiaData::watershed_ordering %>%
  left_join(beta_to_update) %>%
  mutate(`2nd calibration adjustment` = ifelse(is.na(`2nd calibration adjustment`),
                                               3.5, `2nd calibration adjustment`)) %>%
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
             `survival adjustments` = surv.adj)

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
