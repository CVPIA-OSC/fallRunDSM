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














