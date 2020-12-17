library(tidyverse)

adult_seeds <- matrix(0, nrow = 31, ncol = 30)
adult_seeds[ , 1] <- c(22012L, 72L, 12626L, 12L, 12L, 885L, 8555L, 1251L, 1649L, 569L,
                       12L, 1332L, 51L, 12L, 12L, 0L, 0L, 12L, 52408L, 7184L, 0L, 0L,
                       24959L, 0L, 12L, 499L, 4514L, 2145L, 5405L, 984L, 0L)

rownames(adult_seeds) <- DSMhabitat::watershed_metadata$watershed[-32]

usethis::use_data(adult_seeds, overwrite = TRUE)

proportion_hatchery <- c(0.37, 0.2, 0.9, 0.37968253968254, 0.2, 0.115, 0.2225, 0.3525,
                         0.2, 0.16, 0.37968253968254, 0.1525, 0.365, 0.37968253968254,
                         0.37968253968254, 0.37968253968254, 0.37968253968254, 0, 0.855,
                         0.54, 0.37968253968254, 0.37968253968254, 0.571666666666667,
                         0.37968253968254, 0.0766666666666667, 0.02, 0.7575, 0.745, 0.705,
                         0.465, 0.37968253968254) # differs based on run? TODO

names(proportion_hatchery) <- DSMhabitat::watershed_metadata$watershed[-32]

usethis::use_data(proportion_hatchery, overwrite = TRUE)


# @title Hatchery Returns
# @export
# emanuel: not needed
# total_hatchery_returning <- round(stats::runif(1,83097.01, 532203.1)) # what is this

# @title Proportion of Adults Spawning October to December
# @export
month_return_proportions <- c(0.2222222,0.5555556,0.2222222)
names(month_return_proportions) <- month.abb[10:12]
usethis::use_data(month_return_proportions, overwrite = TRUE)

# Mass by size class
mass_by_size_class <- c(0.5, 1.8, 9.1, 31.4)
names(mass_by_size_class) <- c("s", "m", "l", "vl")
usethis::use_data(mass_by_size_class, overwrite = TRUE)



# TODO come up with better names
cross_channel_stray_rate <- c(rep(1, 15), 0, 0, 2, 2, 2, 0, 0, 3, 0, rep(0, 7)) / 24
names(cross_channel_stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(cross_channel_stray_rate, overwrite = TRUE)

stray_rate <- c(rep(1, 15), 0, 0, 1, 1, 1, 0, 0, 1, 0, rep(1, 6), 0) / 25
names(stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(stray_rate, overwrite = TRUE)


# TODO confirm that these are not being used in the refactored model?
# TODO better names and refactor
cc.aloc <- c(rep(1,15),0,0,2,2,2,0,0,3,0,rep(0,7))/24
oth.aloc <- c(rep(1,15),0,0,1,1,1,0,0,1,0,rep(1,6),0)/25


# differs based on run ------

adult_harvest_rate <- c(0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14,
                        0.14, 0.14, 0.14, 0.14, 0.14, 0, 0, 0.14, 0.1, 0.1, 0, 0, 0.33,
                        0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0)
names(adult_harvest_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

natural_adult_removal_rate <- c(0, 0, 0.7456033, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0.2442951, 0, 0, 0, 0.2291128, 0, 0, 0, 0.3944915, 0.2737981,
                                0, 0, 0)  # differs based on run
names(natural_adult_removal_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)


hatchery_allocation <- c(0.067203316, 0.000177541, 0.150929409, 0.000154172, 0.000177541,
                         0.000792731, 0.010619948, 0.008426863, 0.002838529, 0.001160381,
                         0.000154172, 0.000662392, 0.000177541, 0.000154172, 0.000154172,
                         0, 0, 0.000154172, 0.482868351, 0.077602736, 0, 0, 0.133611589,
                         0, 0.000296477, 0.000154172, 0.02819385, 0.013632706, 0.014822544,
                         0.004880526, 0) # differs based on run
names(hatchery_allocation) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(hatchery_allocation, overwrite = TRUE)


original_groups <- read_csv("data-raw/misc/Grouping.csv")

diversity_group <- original_groups$DiversityGroup
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)

