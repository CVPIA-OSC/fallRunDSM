#' @title Fall Run Chinook Model
#' @description Fall Run Chinook life cycle model used for CVPIA's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"}, \code{"calibrate"}
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @param ..params Parameters for model and submodels. Defaults to \code{fallRunDSM::\code{\link{params}}}.
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model should be run stochastically. Defaults to \code{FALSE}.
#' @source IP-117068
#' @examples
#' fall_run_seeds <- fallRunDSM::fall_run_model(mode = "seed")
#' fallRunDSM::fall_run_model(scenario = DSMscenario::scenarios$ONE,
#'                            mode = "simulate",
#'                            seeds = fall_run_seeds)
#' @export
fall_run_model <- function(scenario = NULL, mode = c("seed", "simulate", "calibrate"),
                           seeds = NULL, ..params = fallRunDSM::params,
                           stochastic = FALSE){

  mode <- match.arg(mode)

  if (mode == "simulate") {
    if (is.null(scenario)) {
      # the do nothing scenario to force habitat degradation
      scenario <- DSMscenario::scenarios$NO_ACTION
    }

    habitats <- list(
      spawning_habitat = ..params$spawning_habitat,
      inchannel_habitat_fry = ..params$inchannel_habitat_fry,
      inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
      floodplain_habitat = ..params$floodplain_habitat,
      weeks_flooded = ..params$weeks_flooded
    )

    scenario_data <- DSMscenario::load_scenario(scenario,
                                                habitat_inputs = habitats,
                                                species = DSMscenario::species$FALL_RUN,
                                                spawn_decay_rate = ..params$spawn_decay_rate,
                                                rear_decay_rate = ..params$rear_decay_rate,
                                                stochastic = stochastic)

    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded

  }

  if (mode == "calibrate") {
    scenario_data <- list(
      survival_adjustment = matrix(1, nrow = 31, ncol = 21,
                                   dimnames = list(DSMscenario::watershed_labels,
                                                   1980:2000)))
  }

  output <- list(

    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    proportion_natural = matrix(NA_real_, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    north_delta_fish = data.frame()
  )


  if (mode == 'calibrate') {
    calculated_adults <- matrix(0, nrow = 31, ncol = 30)
  }

  adults <- switch (mode,
                    "seed" = fallRunDSM::adult_seeds,
                    "simulate" = seeds,
                    "calibrate" = seeds,
  )

  simulation_length <- switch(mode,
                              "seed" = 5,
                              "simulate" = 20,
                              "calibrate" = 20)

  for (year in 1:simulation_length) {
    adults_in_ocean <- numeric(31)
    # initialize 31 x 4 matrices for natal fish, migrants, and ocean fish
    lower_mid_sac_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:20], fallRunDSM::size_class_labels))
    lower_sac_fish <- matrix(0, nrow = 27, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:27], fallRunDSM::size_class_labels))
    upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:15], fallRunDSM::size_class_labels))
    sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:15], fallRunDSM::size_class_labels))
    yolo_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:20], fallRunDSM::size_class_labels))
    san_joaquin_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[28:30], fallRunDSM::size_class_labels))
    north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:23], fallRunDSM::size_class_labels))
    south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
    juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))

    avg_ocean_transition_month <- ocean_transition_month(stochastic = stochastic) # 2

    hatch_adults <- if (stochastic) {
      rmultinom(1, size = round(runif(1, 83097.01,532203.1)), prob = ..params$hatchery_allocation)[ , 1]
    } else {
      round(mean(c(83097.01,532203.1)) * ..params$hatchery_allocation)
    }

    spawners <- get_spawning_adults(year, round(adults), hatch_adults, mode = mode,
                                    month_return_proportions = ..params$month_return_proportions,
                                    prop_flow_natal = ..params$prop_flow_natal,
                                    south_delta_routed_watersheds = ..params$south_delta_routed_watersheds,
                                    cc_gates_days_closed = ..params$cc_gates_days_closed,
                                    gates_overtopped = ..params$gates_overtopped,
                                    tisdale_bypass_watershed = ..params$tisdale_bypass_watershed,
                                    yolo_bypass_watershed = ..params$yolo_bypass_watershed,
                                    migratory_temperature_proportion_over_20 = ..params$migratory_temperature_proportion_over_20,
                                    natural_adult_removal_rate = ..params$natural_adult_removal_rate,
                                    cross_channel_stray_rate = ..params$cross_channel_stray_rate,
                                    stray_rate = ..params$stray_rate,
                                    ..surv_adult_enroute_int = ..params$..surv_adult_enroute_int,
                                    .adult_stray_intercept = ..params$.adult_stray_intercept,
                                    .adult_stray_wild = ..params$.adult_stray_wild,
                                    .adult_stray_natal_flow = ..params$.adult_stray_natal_flow,
                                    .adult_stray_cross_channel_gates_closed = ..params$.adult_stray_cross_channel_gates_closed,
                                    .adult_stray_prop_bay_trans = ..params$.adult_stray_prop_bay_trans,
                                    .adult_stray_prop_delta_trans = ..params$.adult_stray_prop_delta_trans,
                                    .adult_en_route_migratory_temp = ..params$.adult_en_route_migratory_temp,
                                    .adult_en_route_bypass_overtopped = ..params$.adult_en_route_bypass_overtopped,
                                    .adult_en_route_adult_harvest_rate = ..params$.adult_en_route_adult_harvest_rate,
                                    stochastic = stochastic)

    init_adults <- spawners$init_adults

    output$spawners[ , year] <- init_adults
    output$proportion_natural[ , year] <- spawners$proportion_natural

    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = spawners$proportion_natural,
      scour = ..params$prob_nest_scoured,
      temperature_effect = ..params$mean_egg_temp_effect,
      .proportion_natural = ..params$.surv_egg_to_fry_proportion_natural,
      .scour = ..params$.surv_egg_to_fry_scour,
      ..surv_egg_to_fry_int = ..params$..surv_egg_to_fry_int
    )

    min_spawn_habitat <- apply(..params$spawning_habitat[ , 10:12, year], 1, min)

    accumulated_degree_days <- cbind(oct = rowSums(..params$degree_days[ , 10:12, year]),
                                     nov = rowSums(..params$degree_days[ , 11:12, year]),
                                     dec = ..params$degree_days[ , 12, year])

    average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, ..params$month_return_proportions)

    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)

    juveniles <- spawn_success(escapement = init_adults,
                               adult_prespawn_survival = prespawn_survival,
                               egg_to_fry_survival = egg_to_fry_surv,
                               prob_scour = ..params$prob_nest_scoured,
                               spawn_habitat = min_spawn_habitat,
                               sex_ratio = ..params$spawn_success_sex_ratio,
                               redd_size = ..params$spawn_success_redd_size,
                               fecundity = ..params$spawn_success_fecundity,
                               stochastic = stochastic)

    # TODO: udpate to reflect actual number of hypothesis. Are there 4 or 5?
    fish_0 <- fish_1 <- fish_2 <- fish_3 <- fish_4 <- fish_5 <- list(juveniles = juveniles,
                                       lower_mid_sac_fish = lower_mid_sac_fish,
                                       lower_sac_fish = lower_sac_fish,
                                       upper_mid_sac_fish = upper_mid_sac_fish,
                                       sutter_fish = sutter_fish,
                                       yolo_fish = yolo_fish,
                                       san_joaquin_fish = san_joaquin_fish,
                                       north_delta_fish = north_delta_fish,
                                       south_delta_fish = south_delta_fish,
                                       juveniles_at_chipps = juveniles_at_chipps,
                                       adults_in_ocean = adults_in_ocean)

    for (month in 1:8) {
      # add two movement hypothesis here
      habitat <- get_habitat(year, month,
                             inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                             inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                             floodplain_habitat = ..params$floodplain_habitat,
                             sutter_habitat = ..params$sutter_habitat,
                             yolo_habitat = ..params$yolo_habitat,
                             delta_habitat = ..params$delta_habitat)

      rearing_survival <- get_rearing_survival(year, month,
                                               survival_adjustment = scenario_data$survival_adjustment,
                                               mode = mode,
                                               avg_temp = ..params$avg_temp,
                                               avg_temp_delta = ..params$avg_temp_delta,
                                               prob_strand_early = ..params$prob_strand_early,
                                               prob_strand_late = ..params$prob_strand_late,
                                               proportion_diverted = ..params$proportion_diverted,
                                               total_diverted = ..params$total_diverted,
                                               delta_proportion_diverted = ..params$delta_proportion_diverted,
                                               delta_total_diverted = ..params$delta_total_diverted,
                                               weeks_flooded = ..params$weeks_flooded,
                                               prop_high_predation = ..params$prop_high_predation,
                                               contact_points = ..params$contact_points,
                                               delta_contact_points = ..params$delta_contact_points,
                                               delta_prop_high_predation = ..params$delta_prop_high_predation,
                                               ..surv_juv_rear_int= ..params$..surv_juv_rear_int,
                                               .surv_juv_rear_contact_points = ..params$.surv_juv_rear_contact_points,
                                               ..surv_juv_rear_contact_points = ..params$..surv_juv_rear_contact_points,
                                               .surv_juv_rear_prop_diversions = ..params$.surv_juv_rear_prop_diversions,
                                               ..surv_juv_rear_prop_diversions = ..params$..surv_juv_rear_prop_diversions,
                                               .surv_juv_rear_total_diversions = ..params$.surv_juv_rear_total_diversions,
                                               ..surv_juv_rear_total_diversions = ..params$..surv_juv_rear_total_diversions,
                                               ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                                               ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                                               .surv_juv_delta_contact_points = ..params$.surv_juv_delta_contact_points,
                                               ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
                                               .surv_juv_delta_total_diverted = ..params$.surv_juv_delta_total_diverted,
                                               ..surv_juv_delta_total_diverted = ..params$..surv_juv_delta_total_diverted,
                                               .surv_juv_rear_avg_temp_thresh = ..params$.surv_juv_rear_avg_temp_thresh,
                                               .surv_juv_rear_high_predation = ..params$.surv_juv_rear_high_predation,
                                               .surv_juv_rear_stranded = ..params$.surv_juv_rear_stranded,
                                               .surv_juv_rear_medium = ..params$.surv_juv_rear_medium,
                                               .surv_juv_rear_large = ..params$.surv_juv_rear_large,
                                               .surv_juv_rear_floodplain = ..params$.surv_juv_rear_floodplain,
                                               .surv_juv_bypass_avg_temp_thresh = ..params$.surv_juv_bypass_avg_temp_thresh,
                                               .surv_juv_bypass_high_predation = ..params$.surv_juv_bypass_high_predation,
                                               .surv_juv_bypass_medium = ..params$.surv_juv_bypass_medium,
                                               .surv_juv_bypass_large = ..params$.surv_juv_bypass_large,
                                               .surv_juv_bypass_floodplain = ..params$.surv_juv_bypass_floodplain,
                                               .surv_juv_delta_avg_temp_thresh = ..params$.surv_juv_delta_avg_temp_thresh,
                                               .surv_juv_delta_high_predation = ..params$.surv_juv_delta_high_predation,
                                               .surv_juv_delta_prop_diverted = ..params$.surv_juv_delta_prop_diverted,
                                               .surv_juv_delta_medium = ..params$.surv_juv_delta_medium,
                                               .surv_juv_delta_large = ..params$.surv_juv_delta_large,
                                               min_survival_rate = ..params$min_survival_rate,
                                               stochastic = stochastic)

      migratory_survival <- get_migratory_survival(year, month,
                                                   cc_gates_prop_days_closed = ..params$cc_gates_prop_days_closed,
                                                   freeport_flows = ..params$freeport_flows,
                                                   vernalis_flows = ..params$vernalis_flows,
                                                   stockton_flows = ..params$stockton_flows,
                                                   vernalis_temps = ..params$vernalis_temps,
                                                   prisoners_point_temps = ..params$prisoners_point_temps,
                                                   CVP_exports = ..params$CVP_exports,
                                                   SWP_exports = ..params$SWP_exports,
                                                   upper_sacramento_flows = ..params$upper_sacramento_flows,
                                                   delta_inflow = ..params$delta_inflow,
                                                   avg_temp_delta = ..params$avg_temp_delta,
                                                   avg_temp = ..params$avg_temp,
                                                   delta_proportion_diverted = ..params$delta_proportion_diverted,
                                                   ..surv_juv_outmigration_sj_int = ..params$..surv_juv_outmigration_sj_int,
                                                   .surv_juv_outmigration_san_joaquin_medium = ..params$.surv_juv_outmigration_san_joaquin_medium,
                                                   .surv_juv_outmigration_san_joaquin_large = ..params$.surv_juv_outmigration_san_joaquin_large,
                                                   min_survival_rate = ..params$min_survival_rate,
                                                   stochastic = stochastic)

      # hypothesis are layed out as follows:
      # 0. base filling + base move
      # 1. bsae filling + snow move
      # 2. base filling + genetics move
      # 3. base filling + temperature move
      # 4. base filling + time move
      # 5. base filling + base move
      # 6. bsae filling + snow move
      # 7. base filling + genetics move
      # 8. base filling + temperature move
      # 9. base filling + time move

      fish_0 <- juvenile_month_dynamic(hypothesis = 0,
                                       fish_0,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_1 <- juvenile_month_dynamic(hypothesis = 1,
                                       fish_1,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_2 <- juvenile_month_dynamic(hypothesis = 2,
                                       fish_2,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_3 <- juvenile_month_dynamic(hypothesis = 3,
                                       fish_3,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_4 <- juvenile_month_dynamic(hypothesis = 4,
                                       fish_4,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      # ---------- start density dep filling -------------------------------

      fish_5 <- juvenile_month_dynamic(hypothesis = 5,
                                       fish_5,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_6 <- juvenile_month_dynamic(hypothesis = 6,
                                       fish_5,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_7 <- juvenile_month_dynamic(hypothesis = 7,
                                       fish_5,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_8 <- juvenile_month_dynamic(hypothesis = 8,
                                       fish_5,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      fish_9 <- juvenile_month_dynamic(hypothesis = 9,
                                       fish_5,
                                       year = year, month = month,
                                       rearing_survival = rearing_survival,
                                       migratory_survival = migratory_survival,
                                       habitat = habitat, ..params = ..params,
                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                       stochastic = stochastic,
                                       juvenile = juvenile)

      tmp <- rbind(fish_0$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_0$south_delta_fish
      fish_0_df <- data.frame(tmp)
      fish_0_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_0_df$month = month
      fish_0_df$year = year
      fish_0_df$hypothesis = "zero"
      rownames(fish_0_df) <- NULL

      tmp <- rbind(fish_1$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_1$south_delta_fish
      fish_1_df <- data.frame(tmp)
      fish_1_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_1_df$month = month
      fish_1_df$year = year
      fish_1_df$hypothesis = "one"
      rownames(fish_1_df) <- NULL

      tmp <- rbind(fish_2$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_2$south_delta_fish
      fish_2_df <- data.frame(tmp)
      fish_2_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_2_df$month = month
      fish_2_df$year = year
      fish_2_df$hypothesis = "two"
      rownames(fish_2_df) <- NULL

      tmp <- rbind(fish_3$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_3$south_delta_fish
      fish_3_df <- data.frame(tmp)
      fish_3_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_3_df$month = month
      fish_3_df$year = year
      fish_3_df$hypothesis = "three"
      rownames(fish_3_df) <- NULL

      tmp <- rbind(fish_4$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_4$south_delta_fish
      fish_4_df <- data.frame(tmp)
      fish_4_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_4_df$month = month
      fish_4_df$year = year
      fish_4_df$hypothesis = "four"
      rownames(fish_4_df) <- NULL

      tmp <- rbind(fish_5$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_5$south_delta_fish
      fish_5_df <- data.frame(tmp)
      fish_5_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_5_df$month = month
      fish_5_df$year = year
      fish_5_df$hypothesis = "five"
      rownames(fish_5_df) <- NULL

      tmp <- rbind(fish_6$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_6$south_delta_fish
      fish_6_df <- data.frame(tmp)
      fish_6_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_6_df$month = month
      fish_6_df$year = year
      fish_6_df$hypothesis = "six"
      rownames(fish_6_df) <- NULL

      tmp <- rbind(fish_7$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_7$south_delta_fish
      fish_7_df <- data.frame(tmp)
      fish_7_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_7_df$month = month
      fish_7_df$year = year
      fish_7_df$hypothesis = "seven"
      rownames(fish_7_df) <- NULL

      tmp <- rbind(fish_8$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_8$south_delta_fish
      fish_8_df <- data.frame(tmp)
      fish_8_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_8_df$month = month
      fish_8_df$year = year
      fish_8_df$hypothesis = "eight"
      rownames(fish_8_df) <- NULL

      tmp <- rbind(fish_9$north_delta_fish, matrix(0, ncol = 4, nrow = 8)) + fish_9$south_delta_fish
      fish_9_df <- data.frame(tmp)
      fish_9_df$watershed = fallRunDSM::watershed_labels[1:31]
      fish_9_df$month = month
      fish_9_df$year = year
      fish_9_df$hypothesis = "nine"
      rownames(fish_9_df) <- NULL

      output$north_delta_fish <- dplyr::bind_rows(
        output$north_delta_fish,
        fish_0_df,
        fish_1_df,
        fish_2_df,
        fish_3_df,
        fish_4_df,
        fish_5_df,
        fish_6_df,
        fish_7_df,
        fish_8_df,
        fish_9_df
      )

    } # end month loop

    # juveniles_at_chipps <-
    #   # (1/6) * fish_0$juveniles_at_chipps +
    #   # (1/6) * fish_1$juveniles_at_chipps +
    #   # (1/6) * fish_2$juveniles_at_chipps +
    #   # (1/6) * fish_3$juveniles_at_chipps +
    #   (1/6) * fish_4$juveniles_at_chipps + 0
    #   # (1/6) * fish_5$juveniles_at_chipps

    adults_in_ocean <-
      (1/10) * fish_0$adults_in_ocean +
      (1/10) * fish_1$adults_in_ocean +
      (1/10) * fish_2$adults_in_ocean +
      (1/10) * fish_3$adults_in_ocean +
      (1/10) * fish_4$adults_in_ocean +
      (1/10) * fish_5$adults_in_ocean +
      (1/10) * fish_6$adults_in_ocean +
      (1/10) * fish_7$adults_in_ocean +
      (1/10) * fish_8$adults_in_ocean +
      (1/10) * fish_9$adults_in_ocean

    #still need adults in ocean and adult in ocean weights
    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% fallRunDSM::params$mass_by_size_class

    adults_returning <- t(sapply(1:31, function(i) {
      if (stochastic) {
        rmultinom(1, adults_in_ocean[i], prob = c(.25, .5, .25))
      } else {
        round(adults_in_ocean[i] * c(.25, .5, .25))
      }
    }))



    # distribute returning adults for future spawning
    if (mode == "calibrate") {
      calculated_adults[1:31, (year + 2):(year + 4)] <- calculated_adults[1:31, (year + 2):(year + 4)] + adults_returning
    } else {
      adults[1:31, (year + 2):(year + 4)] <- adults[1:31, (year + 2):(year + 4)] + adults_returning
    }


  } # end year for loop

  if (mode == "seed") {
    return(adults[ , 6:30])
  } else if (mode == "calibrate") {
    return(calculated_adults[, 6:20])
  }

  spawn_change <- sapply(1:19, function(year) {
    output$spawners[ , year] / (output$spawners[ , year + 1] + 1)
  })

  viable <- spawn_change >= 1 & output$proportion_natural[ , -1] >= 0.9 & output$spawners[ , -1] >= 833

  output$viability_metrics <- sapply(1:4, function(group) {
    colSums(viable[which(fallRunDSM::params$diversity_group == group), ])
  })

  return(output)

}
