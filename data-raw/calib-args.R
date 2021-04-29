x <- fall_run_model(seeds = fr,
               ..surv_adult_enroute_int =-1.2,
               ..surv_adult_prespawn_int = -100,
               ..surv_egg_to_fry_int = -100,
               ..surv_juv_rear_int = -100,
               ..surv_juv_rear_contact_points = -10,
               ..surv_juv_rear_prop_diversions = .5,
               ..surv_juv_rear_total_diversions = .5,
               ..surv_juv_bypass_int = .5,
               ..surv_juv_delta_int = 1.3,
               ..surv_juv_delta_contact_points = 4.5,
               ..surv_juv_delta_total_diverted = 4.1,
               ..surv_juv_outmigration_sj_int = 1.1,
               ..surv_juv_outmigration_sac_int_one = 12,
               ..surv_juv_outmigration_sac_prop_diversions = 1.1,
               ..surv_juv_outmigration_sac_total_diversions = 6.6,
               ..surv_juv_outmigration_sac_int_two = 1.1,
               ..ocean_entry_success_int = rep(1.1, 31))

print(x$spawners[1, 1])


x <- tibble(
  deg = d,
  surv = surv_adult_prespawn(deg_day = deg)

)

x %>%
  ggplot(aes(deg, surv)) + geom_point()
