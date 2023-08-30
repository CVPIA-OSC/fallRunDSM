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

    # check if the params has the new decay element, if yes use new function applying decay
    # to spawning, if not then use the old method. This is a temporary bit of code to allow
    # for quick comparison between two versions of the model.
    if ("spawn_decay_multiplier" %in% names(..params)) {
      scenario_data <- DSMscenario::load_scenario(scenario,
                                                  habitat_inputs = habitats,
                                                  species = DSMscenario::species$FALL_RUN,
                                                  spawn_decay_rate = ..params$spawn_decay_rate,
                                                  rear_decay_rate = ..params$rear_decay_rate,
                                                  spawn_decay_multiplier = ..params$spawn_decay_multiplier,
                                                  stochastic = stochastic)
    } else {
      scenario_data <- DSMscenario::load_scenario(scenario,
                                                  habitat_inputs = habitats,
                                                  species = DSMscenario::species$FALL_RUN,
                                                  spawn_decay_rate = ..params$spawn_decay_rate,
                                                  rear_decay_rate = ..params$rear_decay_rate,
                                                  stochastic = stochastic)
    }

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

    fish_list <- lapply(1:8, function(i) list(juveniles = juveniles,
                                               lower_mid_sac_fish = lower_mid_sac_fish,
                                               lower_sac_fish = lower_sac_fish,
                                               upper_mid_sac_fish = upper_mid_sac_fish,
                                               sutter_fish = sutter_fish,
                                               yolo_fish = yolo_fish,
                                               san_joaquin_fish = san_joaquin_fish,
                                               north_delta_fish = north_delta_fish,
                                               south_delta_fish = south_delta_fish,
                                               juveniles_at_chipps = juveniles_at_chipps,
                                               adults_in_ocean = adults_in_ocean))
    names(fish_list) <- c(paste0("route_", 1:8, "_fish"))

    # TODO Some temperatures are over the 28C limit, for now I am going to
    # just make these be 28. Both of these cases in the 20 years of data
    # are barely over 28 so I think for now this is justified. With that
    # said we need to make this change in the cached data itself and make
    # a note of it.

    growth_temps <- ..params$avg_temp
    growth_temps[which(growth_temps > 28)] <- 28

    for (month in 1:8) {

      growth_rates_ic <- get_growth_rates(growth_temps[,month, year],
                                          prey_density = ..params$prey_density)

      growth_rates_fp <- get_growth_rates(growth_temps[,month, year],
                                          prey_density = ..params$prey_density,
                                          floodplain = TRUE)

      growth_rates_delta <- get_growth_rates(..params$avg_temp_delta[month, year,],
                                             prey_density = ..params$prey_density_delta)

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
      # 1. base filling + base
      # 2. base filling + snow
      # 3. base filling + genetics
      # 4. base filling + temperature
      # 5. dens filling + base
      # 6. dens filling + snow
      # 7. dens filling + genetics
      # 8. dens filling + temperature


      fish_list$route_1_fish <- juvenile_month_dynamic(
        fish_list$route_1_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta
      )

      fish_list$route_2_fish <- juvenile_month_dynamic(
        fish_list$route_2_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        movement_fn = fallRunDSM::snow_globe_movement,
        movement_months = 1:2,
        movement_args = list(freeport_flow = ..params$freeport_flows[month, year],
                             vernalis_flow = ..params$vernalis_flows[month, year],
                             threshold = 1000, p_leave = 0.3, stochastic = stochastic)
      )

      fish_list$route_3_fish <- juvenile_month_dynamic(
        fish_list$route_3_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        movement_fn = fallRunDSM::genetic_movement,
        movement_months = 1:2,
        movement_args = list(p_leave = 0.25, stochastic = stochastic)
      )

      fish_list$route_4_fish <- juvenile_month_dynamic(
        fish_list$route_4_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        movement_fn = fallRunDSM::temperature_movement,
        movement_months = 1:2,
        movement_args = list(movement_month = 3, movement_temp = 15, stochastic = stochastic)
      )


      fish_list$route_5_fish <- juvenile_month_dynamic(
        fish_list$route_6_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
        filling_args = list(up_to_size_class = 2,
                            ..floodplain_capacity = ..params$..floodplain_capacity,
                            ..habitat_capacity = ..params$..habitat_capacity),
        filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
        filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                     ..habitat_capacity = ..params$..habitat_capacity)
      )


      fish_list$route_6_fish <- juvenile_month_dynamic(
        fish_list$route_6_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
        filling_args = list(up_to_size_class = 2, ..floodplain_capacity = ..params$..floodplain_capacity,
                            ..habitat_capacity = ..params$..habitat_capacity),
        filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
        filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                     ..habitat_capacity = ..params$..habitat_capacity),
        movement_fn = fallRunDSM::snow_globe_movement,
        movement_months = 1:2,
        movement_args = list(freeport_flow = ..params$freeport_flows[month, year],
                             vernalis_flow = ..params$vernalis_flows[month, year],
                             threshold = 1000, p_leave = 0.3, stochastic = stochastic)
      )

      fish_list$route_7_fish <- juvenile_month_dynamic(
        fish_list$route_7_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
        filling_args = list(up_to_size_class = 2, ..floodplain_capacity = ..params$..floodplain_capacity,
                            ..habitat_capacity = ..params$..habitat_capacity),
        filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
        filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                     ..habitat_capacity = ..params$..habitat_capacity),
        movement_fn = fallRunDSM::genetic_movement,
        movement_months = 1:2,
        movement_args = list(p_leave = 0.25, stochastic = stochastic)
      )

      fish_list$route_8_fish <- juvenile_month_dynamic(
        fish_list$route_8_fish,
        year = year, month = month,
        rearing_survival = rearing_survival,
        migratory_survival = migratory_survival,
        habitat = habitat, ..params = ..params,
        avg_ocean_transition_month = avg_ocean_transition_month,
        stochastic = stochastic,
        ic_growth = growth_rates_ic,
        fp_growth = growth_rates_fp,
        delta_growth = growth_rates_delta,
        filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
        filling_args = list(up_to_size_class = 2, ..floodplain_capacity = ..params$..floodplain_capacity,
                            ..habitat_capacity = ..params$..habitat_capacity),
        filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
        filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                     ..habitat_capacity = ..params$..habitat_capacity),
        movement_fn = fallRunDSM::temperature_movement,
        movement_months = 1:2,
        movement_args = list(movement_month = 3, movement_temp = 15, stochastic = stochastic)
      )

      if (FALSE) {
        fish_1_df <- create_fish_df(fish_df = fish_list$route_1_fish, month = month, year = year)
        fish_2_df <- create_fish_df(fish_df = fish_list$route_2_fish, month = month, year = year)
        fish_3_df <- create_fish_df(fish_df = fish_list$route_3_fish, month = month, year = year)
        fish_4_df <- create_fish_df(fish_df = fish_list$route_4_fish, month = month, year = year)
        fish_5_df <- create_fish_df(fish_df = fish_list$route_5_fish, month = month, year = year)
        fish_6_df <- create_fish_df(fish_df = fish_list$route_6_fish, month = month, year = year)
        fish_7_df <- create_fish_df(fish_df = fish_list$route_7_fish, month = month, year = year)
        fish_8_df <- create_fish_df(fish_df = fish_list$route_8_fish, month = month, year = year)

        output$north_delta_fish <- dplyr::bind_rows(
          output$north_delta_fish,
          fish_1_df,
          fish_2_df,
          fish_3_df,
          fish_4_df,
          fish_5_df,
          fish_6_df,
          fish_7_df,
          fish_8_df
        )
      }

    } # end month loop

    adults_in_ocean <-
      ..params$..adults_in_ocean_weights[1] * fish_list$route_1_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[2] * fish_list$route_2_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[3] * fish_list$route_3_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[4] * fish_list$route_4_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[5] * fish_list$route_5_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[6] * fish_list$route_6_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[7] * fish_list$route_7_fish$adults_in_ocean +
      ..params$..adults_in_ocean_weights[8] * fish_list$route_8_fish$adults_in_ocean

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
