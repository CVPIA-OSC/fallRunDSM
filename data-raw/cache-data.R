library(tidyverse)
library(DSMCalibrationData)

# watershed labels
watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                      "Feather River", "Yuba River", "Lower-mid Sacramento River",
                      "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                      "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                      "Tuolumne River", "San Joaquin River")

usethis::use_data(watershed_labels)

# Adult seeds
adult_seeds <- matrix(0, nrow = 31, ncol = 30)
no_sr_spawn <- !as.logical(DSMhabitat::watershed_species_present[1:31, ]$sr *
              DSMhabitat::watershed_species_present[1:31,]$spawn)

adult_seed_values <- DSMCalibrationData::mean_escapement_2013_2017 %>%
  bind_cols(no_sr_spawn = no_sr_spawn) %>%
  select(watershed, Spring, no_sr_spawn) %>%
  mutate(corrected_spring = case_when(
    no_sr_spawn ~ 0,
    is.na(Spring) | Spring < 10 ~ 12,
    TRUE ~ Spring)
  ) %>% pull(corrected_spring)

adult_seeds[ , 1] <- adult_seed_values
rownames(adult_seeds) <- watershed_labels
usethis::use_data(adult_seeds, overwrite = TRUE)

# Prop hatchery come from 2010-2013 CWI reports
butte_creek_hatch = mean(c(0.01, 0, 0, 0))
feather_river_hatch = mean(c(0.78, 0.90, 0.90, 0.84))
yuba_river_hatch = mean(c(0.71, 0.495, 0.36, 0.40))
proportion_hatchery <- c(rep(0, 5), butte_creek_hatch, rep(0, 12), feather_river_hatch, yuba_river_hatch, rep(0, 11))
names(proportion_hatchery) <- watershed_labels
usethis::use_data(proportion_hatchery, overwrite = TRUE)

# Proportion adults spawners (including hatchery fish) across 4 months (March-June)
month_return_proportions <- c(0.125, 0.375, 0.375, 0.125)
names(month_return_proportions) <- month.abb[3:6]
usethis::use_data(month_return_proportions, overwrite = TRUE)

# Mass by size class
mass_by_size_class <- c(0.5, 1.8, 9.1, 31.4)
names(mass_by_size_class) <- c("s", "m", "l", "vl")
usethis::use_data(mass_by_size_class, overwrite = TRUE)


# stray rates differ based on run
cross_channel_stray_rate <- c(1, 2, 2, 0, 1, 2, 2, 1, 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)/20
names(cross_channel_stray_rate) <- watershed_labels
usethis::use_data(cross_channel_stray_rate, overwrite = TRUE)

stray_rate <- c(1, 2, 2, 0, 1, 2, 2, 1, 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0)/26
names(stray_rate) <- watershed_labels
usethis::use_data(stray_rate, overwrite = TRUE)

# differs based on run ------
# No adult harvest for spring run
adult_harvest_rate <- rep(0, 31)
names(adult_harvest_rate) <- watershed_labels
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

# Feather River(19) has the only non zero value for natural adult removal springrun
natural_adult_removal_rate <- c(rep(0, 18), 0.22, rep(0, 12)) #differs based on run
names(natural_adult_removal_rate) <- watershed_labels
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)

# Hatchery allocation
hatchery_allocation <- c(0, 0.00012, 0.00445, 0, 0.00076, 0.00285, 0.00038,
                         0.00009, 0, 0.00536, 0, 0, 0, 0, 0, 0, 0,0, 0.86531,
                         0.12068, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # differs based on run
names(hatchery_allocation) <- watershed_labels
usethis::use_data(hatchery_allocation, overwrite = TRUE)

# Sit defined diversity groups
original_groups <- read_csv("data-raw/misc/Grouping.csv")
diversity_group <- original_groups$DiversityGroup
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)

# Size class labels
size_class_labels <- c('s', 'm', 'l', 'vl')
usethis::use_data(size_class_labels)

# calculate growth rates
growth_rates_inchannel <- growth()
usethis::use_data(growth_rates_inchannel)
growth_rates_floodplain <- growth_floodplain()
usethis::use_data(growth_rates_floodplain)













