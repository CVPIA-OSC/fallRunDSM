#' @title Fall Run Chinook Model
#' @description Fall Run Chinook life cycle model used for CVPIA's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"}, \code{"calibrate"}
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @param ..params Parameters for model and submodels
#' @source IP-117068
#' @export
fall_run_model <- function(scenario = NULL, mode = c("seed", "simulate", "calibrate"),
                           seeds = NULL, ..params = fallRunDSM::params){

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
                                   species = DSMscenario::species$FALL_RUN)

    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded
  }

  output <- list(

    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    natural_spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    proportion_natural = matrix(NA_real_, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20))
  )

  # initialize 31 x 4 matrices for natal fish, migrants, and ocean fish
  lower_mid_sac_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
  lower_sac_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
  upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:15], fallRunDSM::size_class_labels))
  sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:15], fallRunDSM::size_class_labels))
  yolo_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[18:20], fallRunDSM::size_class_labels))
  san_joaquin_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[28:30], fallRunDSM::size_class_labels))
  north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:23], fallRunDSM::size_class_labels))
  south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
  juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
  # proportion_natural <- matrix(NA_real_, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20))

  adults <- switch (mode,
                    "seed" = fallRunDSM::adult_seeds,
                    "simulate" = seeds,
                    "calibrate" = fallRunDSM::imputed_grandtab
  )

  simulation_length <- switch(mode,
                       "seed" = 5,
                       "simulate" = 20,
                       "calibrate" = 20)

  for (year in 1:simulation_length) {
    adults_in_ocean <- numeric(31)
    annual_migrants <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
    avg_ocean_transition_month <- ocean_transition_month() # 2

    hatch_adults <- rmultinom(1, size = round(runif(1, 83097.01,532203.1)), prob = ..params$hatchery_allocation)[ , 1]
    spawners <- get_spawning_adults(year, round(adults), hatch_adults, mode = mode,
                                    month_return_proportions = ..params$month_return_proportions,
                                    prop_flow_natal = ..params$prop_flow_natal,
                                    south_delta_routed_watersheds = ..params$south_delta_routed_watersheds,
                                    cc_gates_days_closed = ..params$cc_gates_days_closed,
                                    gates_overtopped = ..params$gates_overtopped,
                                    tisdale_bypass_watershed = ..params$tisdale_bypass_watershed,
                                    yolo_bypass_watershed = ..params$yolo_bypass_watershed,
                                    migratory_temperature_proportion_over_20 = ..params$migratory_temperature_proportion_over_20,
                                    ..surv_adult_enroute_int = ..params$..surv_adult_enroute_int,
                                    .adult_stray_intercept = ..params$.adult_stray_intercept,
                                    .adult_stray_wild = ..params$.adult_stray_wild,
                                    .adult_stray_natal_flow = ..params$.adult_stray_natal_flow,
                                    .adult_stray_cross_channel_gates_closed = ..params$.adult_stray_cross_channel_gates_closed,
                                    .adult_stray_prop_bay_trans = ..params$.adult_stray_prop_bay_trans,
                                    .adult_stray_prop_delta_trans = ..params$.adult_stray_prop_delta_trans,
                                    .adult_en_route_migratory_temp = ..params$.adult_en_route_migratory_temp,
                                    .adult_en_route_bypass_overtopped = ..params$.adult_en_route_bypass_overtopped,
                                    .adult_en_route_adult_harvest_rate = ..params$.adult_en_route_adult_harvest_rate)

    init_adults <- spawners$init_adults

    output$spawners[ , year] <- init_adults
    output$proportion_natural[ , year] <- spawners$proportion_natural
    output$natural_spawners[ , year] <- spawners$natural_adults

    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = 1 - ..params$proportion_hatchery,
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
                               fecundity = ..params$spawn_success_fecundity)

    for (month in 1:8) {
      habitat <- get_habitat(year, month,
                             inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                             inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                             floodplain_habitat = ..params$floodplain_habitat,
                             sutter_habitat = ..params$sutter_habitat,
                             yolo_habitat = ..params$yolo_habitat,
                             delta_habitat = ..params$delta_habitat)

      rearing_survival <- get_rearing_survival_rates(year, month,
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
                                                     ..surv_juv_rear_contact_points= ..params$..surv_juv_rear_contact_points,
                                                     ..surv_juv_rear_prop_diversions= ..params$..surv_juv_rear_prop_diversions,
                                                     ..surv_juv_rear_total_diversions= ..params$..surv_juv_rear_total_diversions,
                                                     ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                                                     ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                                                     ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
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
                                                     min_survival_rate = ..params$min_survival_rate)

      migratory_survival <- get_migratory_survival_rates(year, month,
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
                                                         .surv_juv_outmigration_sac_delta_intercept_one = ..params$.surv_juv_outmigration_sac_delta_intercept_one,
                                                         .surv_juv_outmigration_sac_delta_intercept_two = ..params$.surv_juv_outmigration_sac_delta_intercept_two,
                                                         .surv_juv_outmigration_sac_delta_intercept_three = ..params$.surv_juv_outmigration_sac_delta_intercept_three,
                                                         .surv_juv_outmigration_sac_delta_delta_flow = ..params$.surv_juv_outmigration_sac_delta_delta_flow,
                                                         .surv_juv_outmigration_sac_delta_avg_temp = ..params$.surv_juv_outmigration_sac_delta_avg_temp,
                                                         .surv_juv_outmigration_sac_delta_perc_diversions = ..params$.surv_juv_outmigration_sac_delta_perc_diversions,
                                                         .surv_juv_outmigration_sac_delta_medium = ..params$.surv_juv_outmigration_sac_delta_medium,
                                                         .surv_juv_outmigration_sac_delta_large = ..params$.surv_juv_outmigration_sac_delta_large,
                                                         ..surv_juv_outmigration_sj_int = ..params$..surv_juv_outmigration_sj_int,
                                                         ..surv_juv_outmigration_sac_int_one = ..params$..surv_juv_outmigration_sac_int_one,
                                                         ..surv_juv_outmigration_sac_prop_diversions = ..params$..surv_juv_outmigration_sac_prop_diversions,
                                                         ..surv_juv_outmigration_sac_total_diversions = ..params$..surv_juv_outmigration_sac_total_diversions,
                                                         ..surv_juv_outmigration_sac_int_two = ..params$..surv_juv_outmigration_sac_int_two,
                                                         .surv_juv_outmigration_san_joaquin_medium = ..params$.surv_juv_outmigration_san_joaquin_medium,
                                                         .surv_juv_outmigration_san_joaquin_large = ..params$.surv_juv_outmigration_san_joaquin_large,
                                                         min_survival_rate = ..params$min_survival_rate)

      migrants <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))

      if (month == 8) {
        # all remaining fish outmigrate
        sutter_fish <- migrate(sutter_fish, migratory_survival$sutter)
        upper_mid_sac_fish <- migrate(upper_mid_sac_fish + juveniles[1:15, ], migratory_survival$uppermid_sac)
        migrants[1:15, ] <- upper_mid_sac_fish + sutter_fish
        yolo_fish <- migrate(yolo_fish, migratory_survival$yolo)
        migrants[18:20, ] <- juveniles[18:20, ] + yolo_fish
        lower_mid_sac_fish <- migrate(lower_mid_sac_fish + migrants, migratory_survival$lowermid_sac)
        migrants <- lower_mid_sac_fish
        migrants[23, ] <- juveniles[23, ]
        lower_sac_fish <- migrate(lower_sac_fish + migrants, migratory_survival$lower_sac)
        migrants[25:27, ] <- juveniles[25:27, ]
        san_joaquin_fish <- migrate(juveniles[28:30, ] + san_joaquin_fish, migratory_survival$san_joaquin)
        migrants[18:20, ] <- migrants[18:20, ] + yolo_fish
        migrants[28:30, ] <- san_joaquin_fish

        delta_fish <- route_and_rear_deltas(year = year, month = month,
                                            migrants = round(migrants),
                                            north_delta_fish = north_delta_fish,
                                            south_delta_fish = south_delta_fish,
                                            north_delta_habitat = habitat$north_delta,
                                            south_delta_habitat = habitat$south_delta,
                                            freeport_flows = ..params$freeport_flows,
                                            cc_gates_days_closed = ..params$cc_gates_days_closed,
                                            rearing_survival_delta = rearing_survival$delta,
                                            migratory_survival_delta = migratory_survival$delta,
                                            migratory_survival_sac_delta = migratory_survival$sac_delta,
                                            migratory_survival_bay_delta = migratory_survival$bay_delta,
                                            juveniles_at_chipps = juveniles_at_chipps,
                                            growth_rates = ..params$growth_rates)


        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate

        annual_migrants <- annual_migrants + migrants_at_golden_gate
      } else {
        # if month < 8
        # route northern natal fish stay and rear or migrate downstream ------
        upper_sac_trib_fish <-  route(year = year,
                                      month = month,
                                      juveniles = juveniles[1:15, ],
                                      inchannel_habitat = habitat$inchannel[1:15],
                                      floodplain_habitat = habitat$floodplain[1:15],
                                      prop_pulse_flows = ..params$prop_pulse_flows[1:15, ],
                                      proportion_flow_bypass = ..params$proportion_flow_bypass,
                                      detour = 'sutter',
                                      .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                      .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                      .pulse_movement_medium = ..params$.pulse_movement_medium,
                                      .pulse_movement_large = ..params$.pulse_movement_large,
                                      .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                      .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                      .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                      .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse)

        upper_sac_trib_rear <- rear(juveniles = upper_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[1:15, ],
                                    growth = ..params$growth_rates,
                                    floodplain_juveniles = upper_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[1:15, ],
                                    floodplain_growth = ..params$growth_rates_floodplain,
                                    weeks_flooded = ..params$weeks_flooded[1:15, month, year])

        juveniles[1:15, ] <- upper_sac_trib_rear$inchannel + upper_sac_trib_rear$floodplain

        # route migrant fish into Upper-mid Sac Region (fish from watersheds 1:15)
        # regional fish stay and rear
        # or migrate further downstream or in sutter bypass
        sutter_fish <- route_bypass(bypass_fish = sutter_fish + upper_sac_trib_fish$detoured,
                                    bypass_habitat = habitat$sutter,
                                    migration_survival_rate = migratory_survival$sutter)

        upper_mid_sac_fish <- route_regional(month = month,
                                             migrants = upper_mid_sac_fish + upper_sac_trib_fish$migrants,
                                             inchannel_habitat = habitat$inchannel[16],
                                             floodplain_habitat = habitat$floodplain[16],
                                             prop_pulse_flows = ..params$prop_pulse_flows[16, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$uppermid_sac)


        migrants[1:15, ] <- upper_mid_sac_fish$migrants + sutter_fish$migrants

        sutter_fish <- rear(juveniles = sutter_fish$inchannel,
                            survival_rate = matrix(rep(rearing_survival$sutter, nrow(sutter_fish$inchannel)), ncol = 4, byrow = TRUE),
                            growth = fallRunDSM::params$growth_rates)

        upper_mid_sac_fish <- rear(juveniles = upper_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[16, ],
                                   growth = fallRunDSM::params$growth_rates,
                                   floodplain_juveniles = upper_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[16, ],
                                   floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                   weeks_flooded = rep(..params$weeks_flooded[16, month, year], nrow(upper_mid_sac_fish$inchannel)))

        upper_mid_sac_fish <- upper_mid_sac_fish$inchannel + upper_mid_sac_fish$floodplain

        # route migrant fish into Lower-mid Sac Region (fish from watersheds 18:20, and migrants from Upper-mid Sac Region)
        # regional fish stay and rear
        # or migrate further downstream  or in yolo bypass
        lower_mid_sac_trib_fish <- route(year = year,
                                         month = month,
                                         juveniles = juveniles[18:20, ],
                                         inchannel_habitat = habitat$inchannel[18:20],
                                         floodplain_habitat = habitat$floodplain[18:20],
                                         prop_pulse_flows =  ..params$prop_pulse_flows[18:20, ],
                                         proportion_flow_bypass = ..params$proportion_flow_bypass,
                                         detour = 'yolo',
                                         .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                         .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                         .pulse_movement_medium = ..params$.pulse_movement_medium,
                                         .pulse_movement_large = ..params$.pulse_movement_large,
                                         .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                         .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                         .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                         .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse)

        lower_mid_sac_trib_rear <- rear(juveniles = lower_mid_sac_trib_fish$inchannel,
                                        survival_rate = rearing_survival$inchannel[18:20, ],
                                        growth = fallRunDSM::params$growth_rates,
                                        floodplain_juveniles = lower_mid_sac_trib_fish$floodplain,
                                        floodplain_survival_rate = rearing_survival$floodplain[18:20, ],
                                        floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                        weeks_flooded = ..params$weeks_flooded[18:20, month, year])

        juveniles[18:20, ] <- lower_mid_sac_trib_rear$inchannel + lower_mid_sac_trib_rear$floodplain

        yolo_fish <- route_bypass(bypass_fish = yolo_fish + lower_mid_sac_trib_fish$detoured,
                                  bypass_habitat = habitat$yolo,
                                  migration_survival_rate = migratory_survival$yolo)

        migrants[18:20, ] <- lower_mid_sac_trib_fish$migrants + yolo_fish$migrants

        lower_mid_sac_fish <- route_regional(month = month,
                                             migrants = lower_mid_sac_fish + migrants,
                                             inchannel_habitat = habitat$inchannel[21],
                                             floodplain_habitat = habitat$floodplain[21],
                                             prop_pulse_flows = ..params$prop_pulse_flows[21, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$lowermid_sac)

        migrants <- lower_mid_sac_fish$migrants

        # rear
        yolo_fish <- rear(juveniles = yolo_fish$inchannel,
                          survival_rate = matrix(rep(rearing_survival$yolo, nrow(yolo_fish$inchannel)), ncol = 4, byrow = TRUE),
                          growth = fallRunDSM::params$growth_rates)

        lower_mid_sac_fish <- rear(juveniles = lower_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[21, ],
                                   growth = fallRunDSM::params$growth_rates,
                                   floodplain_juveniles = lower_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[21, ],
                                   floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                   weeks_flooded = rep(..params$weeks_flooded[21, month, year], nrow(lower_mid_sac_fish$inchannel)))

        lower_mid_sac_fish <- lower_mid_sac_fish$inchannel + lower_mid_sac_fish$floodplain

        # route migrant fish into Lower Sac Region (fish from watershed 23, and migrants from Lower-mid Sac Region)
        # regional fish stay and rear
        # or migrate north delta
        lower_sac_trib_fish <- route(year = year,
                                     month = month,
                                     juveniles = juveniles[23, , drop = FALSE],
                                     inchannel_habitat = habitat$inchannel[23],
                                     floodplain_habitat = habitat$floodplain[23],
                                     prop_pulse_flows =  ..params$prop_pulse_flows[23, , drop = FALSE],
                                     .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                     .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                     .pulse_movement_medium = ..params$.pulse_movement_medium,
                                     .pulse_movement_large = ..params$.pulse_movement_large,
                                     .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                     .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                     .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                     .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse)

        lower_sac_trib_rear <- rear(juveniles = lower_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[23, , drop = FALSE],
                                    growth = fallRunDSM::params$growth_rates,
                                    floodplain_juveniles = lower_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[23, , drop = FALSE],
                                    floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                    weeks_flooded = ..params$weeks_flooded[23, month, year])

        juveniles[23, ] <- lower_sac_trib_rear$inchannel + lower_sac_trib_rear$floodplain

        migrants[23, ] <- lower_sac_trib_fish$migrants

        lower_sac_fish <- route_regional(month = month,
                                         migrants = lower_sac_fish + migrants,
                                         inchannel_habitat = habitat$inchannel[24],
                                         floodplain_habitat = habitat$floodplain[24],
                                         prop_pulse_flows = ..params$prop_pulse_flows[24, , drop = FALSE],
                                         migration_survival_rate = migratory_survival$lower_sac)

        migrants <- lower_sac_fish$migrants

        lower_sac_fish <- rear(juveniles = lower_sac_fish$inchannel,
                               survival_rate = rearing_survival$inchannel[24, ],
                               growth = fallRunDSM::params$growth_rates,
                               floodplain_juveniles = lower_sac_fish$floodplain,
                               floodplain_survival_rate = rearing_survival$floodplain[24, ],
                               floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                               weeks_flooded = rep(..params$weeks_flooded[24, month, year], nrow(lower_sac_fish$inchannel)))

        lower_sac_fish <- lower_sac_fish$inchannel + lower_sac_fish$floodplain

        # route southern natal fish stay and rear or migrate downstream ------

        # route migrant fish into South Delta Region (fish from watersheds 25:27)
        # regional fish stay and rear
        # or migrate to south delta
        south_delta_trib_fish <- route(year = year,
                                       month = month,
                                       juveniles = juveniles[25:27, ],
                                       inchannel_habitat = habitat$inchannel[25:27],
                                       floodplain_habitat = habitat$floodplain[25:27],
                                       prop_pulse_flows =  ..params$prop_pulse_flows[25:27, ],
                                       .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                       .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                       .pulse_movement_medium = ..params$.pulse_movement_medium,
                                       .pulse_movement_large = ..params$.pulse_movement_large,
                                       .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                       .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                       .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                       .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse)

        south_delta_trib_rear <- rear(juveniles = south_delta_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[25:27, ],
                                      growth = fallRunDSM::params$growth_rates,
                                      floodplain_juveniles = south_delta_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[25:27, ],
                                      floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                      weeks_flooded = ..params$weeks_flooded[25:27, month, year])

        juveniles[25:27, ] <- south_delta_trib_rear$inchannel + south_delta_trib_rear$floodplain

        migrants[25:27, ] <- south_delta_trib_fish$migrants

        # route migrant fish into San Joquin River (fish from watersheds 28:30)
        # regional fish stay and rear
        # or migrate to south delta

        san_joaquin_trib_fish <- route(year = year,
                                       month = month,
                                       juveniles = juveniles[28:30, ],
                                       inchannel_habitat = habitat$inchannel[28:30],
                                       floodplain_habitat = habitat$floodplain[28:30],
                                       prop_pulse_flows =  ..params$prop_pulse_flows[28:30, ],
                                       .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                       .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                       .pulse_movement_medium = ..params$.pulse_movement_medium,
                                       .pulse_movement_large = ..params$.pulse_movement_large,
                                       .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                       .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                       .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                       .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse)

        san_joaquin_trib_rear <- rear(juveniles = san_joaquin_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[28:30, ],
                                      growth = fallRunDSM::params$growth_rates,
                                      floodplain_juveniles = san_joaquin_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[28:30, ],
                                      floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                      weeks_flooded = ..params$weeks_flooded[28:30, month, year])

        juveniles[28:30, ] <- san_joaquin_trib_rear$inchannel + san_joaquin_trib_rear$floodplain

        san_joaquin_fish <- route_regional(month = month,
                                           migrants = san_joaquin_fish + san_joaquin_trib_fish$migrants,
                                           inchannel_habitat = habitat$inchannel[31],
                                           floodplain_habitat = habitat$floodplain[31],
                                           prop_pulse_flows = ..params$prop_pulse_flows[31, , drop = FALSE],
                                           migration_survival_rate = migratory_survival$san_joaquin)

        migrants[28:30, ] <- san_joaquin_fish$migrants

        san_joaquin_fish <- rear(juveniles = san_joaquin_fish$inchannel,
                                 survival_rate = rearing_survival$inchannel[31, ],
                                 growth = fallRunDSM::params$growth_rates,
                                 floodplain_juveniles = san_joaquin_fish$floodplain,
                                 floodplain_survival_rate = rearing_survival$floodplain[31, ],
                                 floodplain_growth = fallRunDSM::params$growth_rates_floodplain,
                                 weeks_flooded = rep(..params$weeks_flooded[31, month, year], nrow(san_joaquin_fish$inchannel)))

        san_joaquin_fish <- san_joaquin_fish$inchannel + san_joaquin_fish$floodplain

        delta_fish <- route_and_rear_deltas(year = year, month = month,
                                            migrants = round(migrants),
                                            north_delta_fish = north_delta_fish,
                                            south_delta_fish = south_delta_fish,
                                            north_delta_habitat = habitat$north_delta,
                                            south_delta_habitat = habitat$south_delta,
                                            freeport_flows = ..params$freeport_flows,
                                            cc_gates_days_closed = ..params$cc_gates_days_closed,
                                            rearing_survival_delta = rearing_survival$delta,
                                            migratory_survival_delta = migratory_survival$delta,
                                            migratory_survival_sac_delta = migratory_survival$sac_delta,
                                            migratory_survival_bay_delta = migratory_survival$bay_delta,
                                            juveniles_at_chipps = juveniles_at_chipps,
                                            growth_rates = fallRunDSM::params$growth_rates)

        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate

        annual_migrants <- annual_migrants + migrants_at_golden_gate

        north_delta_fish <- delta_fish$north_delta_fish
        south_delta_fish <- delta_fish$south_delta_fish
        juveniles_at_chipps <- delta_fish$juveniles_at_chipps
      }

      adults_in_ocean <- adults_in_ocean + ocean_entry_success(migrants = migrants_at_golden_gate,
                                                               month = month,
                                                               avg_ocean_transition_month = avg_ocean_transition_month,
                                                               ocean_entry_success_length = ..params$ocean_entry_success_length,
                                                               ..ocean_entry_success_int = ..params$..ocean_entry_success_int,
                                                               .ocean_entry_success_months = ..params$.ocean_entry_success_months)

    } # end month loop

    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% fallRunDSM::params$mass_by_size_class

    adults_returning <- t(sapply(1:31, function(i) {
      rmultinom(1, adults_in_ocean[i], prob = c(.25, .5, .25))
    }))

    # distribute returning adults for future spawning
    if (mode != "calibrate") {
      adults[1:31, (year + 2):(year + 4)] <- adults[1:31, (year + 2):(year + 4)] + adults_returning
    }

  } # end year for loop

  if (mode == "seed") {
    return(adults[ , 6:30])
  } else if (mode == "calibrate") {
    return(output)
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





