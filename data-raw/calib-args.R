x <- fall_run_model(seeds = fr,
               ..surv_adult_enroute_int = .01,
               ..surv_adult_prespawn_int = 100,
               ..surv_egg_to_fry_int = 34,
               ..surv_juv_rear_int = rep(1, 31),
               ..surv_juv_rear_contact_points = rep(0, 31),
               ..surv_juv_rear_prop_diversions = rep(1, 31),
               ..surv_juv_rear_total_diversions = rep(1, 31))
print(x$spawners[1, 1])
