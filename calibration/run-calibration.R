library(fallRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)

source("calibration/fitness.R")
source("calibration/ga_population_init.R")
source("calibration/update_params.R")

calib_seeds <- matrix(0, nrow = 31, ncol = 25)
calib_seeds[, 1:5] <- DSMCalibrationData::grandtab_imputed$fall[, 1:5]


# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -fall_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$fall,
              seeds = calib_seeds,
              params = fallRunDSM::params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
              x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
              x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
              x[38], x[39], x[40], x[41]
            ),
          lower = c(2.5, rep(-3.5, 40)),
          upper = rep(3.5, 41),
          popSize = 75,
          maxiter = 10000,
          run = 20,
          parallel = TRUE,
          population = ga_population_init)
)


res2 <- ga(type = "real-valued",
          fitness =
            function(x) -fall_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$fall,
              seeds = calib_seeds,
              params = fallRunDSM::params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
              x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
              x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
              x[38], x[39], x[40], x[41]
            ),
          lower = rep(-3.5, 41),
          upper = rep(3.5, 41),
          popSize = 30,
          maxiter = 10000,
          run = 20,
          parallel = TRUE,
          population = ga_population_init)

res3 <- ga(type = "real-valued",
           fitness =
             function(x) -fall_run_fitness(
               known_adults = DSMCalibrationData::grandtab_observed$fall,
               seeds = calib_seeds,
               params = fallRunDSM::params,
               x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
               x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
               x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
               x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
               x[38], x[39], x[40], x[41]
             ),
           lower = c(2.5, rep(-3.5, 40)),
           upper = rep(3.5, 41),
           popSize = 300,
           maxiter = 10000,
           run = 50,
           parallel = TRUE,
           population = ga_population_init)

res4 <- ga(type = "real-valued",
           fitness =
             function(x) -fall_run_fitness(
               known_adults = DSMCalibrationData::grandtab_observed$fall,
               seeds = calib_seeds,
               params = fallRunDSM::params,
               x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
               x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
               x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
               x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
               x[38], x[39], x[40], x[41]
             ),
           lower = rep(-3.5, 41),
           upper = rep(3.5, 41),
           popSize = 150,
           maxiter = 10000,
           run = 20,
           parallel = TRUE,
           population = ga_population_init)


res5 <- ga(type = "real-valued",
           fitness =
             function(x) -fall_run_fitness(
               known_adults = DSMCalibrationData::grandtab_observed$fall,
               seeds = calib_seeds,
               params = fallRunDSM::params,
               x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
               x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
               x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
               x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37],
               x[38], x[39], x[40], x[41]
             ),
           lower = rep(-3.5, 41),
           upper = rep(3.5, 41),
           popSize = 1000,
           maxiter = 10000,
           run = 20,
           parallel = TRUE,
           population = ga_population_init)



# Evaluate Results ------------------------------------

# watersheds without calibration
remove_these <- names(which(is.na(DSMCalibrationData::grandtab_observed$fall[, 1])))

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
  ) %>% arrange(desc(abs(r)))

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed) | !is.na(simulated), observed <= 50000) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )


