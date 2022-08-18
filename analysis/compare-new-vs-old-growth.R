# There is no easy way of doing this. The best approach is to swtich branches
# and rebuild in between runs of the model.

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
# Updated Transitions ----------------------

run_scenario <- function(scenario, seeds, prey_dens) {
  run <- fallRunDSM::fall_run_model(scenario = scenario,
                                    mode = "simulate", seeds = seeds, stochastic = FALSE,
                                    prey_density = prey_dens)
  return(mean(colSums(run$spawners * run$proportion_natural, na.rm = TRUE)))
}

scenarios <- list(DSMscenario::scenarios$NO_ACTION, DSMscenario::scenarios$ONE,
                  DSMscenario::scenarios$TWO, DSMscenario::scenarios$THREE,
                  DSMscenario::scenarios$FOUR, DSMscenario::scenarios$FIVE,
                  DSMscenario::scenarios$SIX, DSMscenario::scenarios$SEVEN,
                  DSMscenario::scenarios$EIGHT, DSMscenario::scenarios$NINE,
                  DSMscenario::scenarios$TEN, DSMscenario::scenarios$ELEVEN,
                  DSMscenario::scenarios$TWELVE, DSMscenario::scenarios$THIRTEEN)

# low
seeds <- fallRunDSM::fall_run_model(mode = "seed", stochastic = FALSE, prey_density = "low")
low_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "low"))
names(low_dens_growth_model) <- c("no action", as.character(1:13))
low_rankings <- rev(sort(low_dens_growth_model))


# med
seeds <- fallRunDSM::fall_run_model(mode = "seed", stochastic = FALSE, prey_density = "med")
med_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "med"))
names(med_dens_growth_model) <- c("no action", as.character(1:13))
med_rankings <- rev(sort(med_dens_growth_model))

# hi
seeds <- fallRunDSM::fall_run_model(mode = "seed", stochastic = FALSE, prey_density = "hi")
hi_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "hi"))
names(hi_dens_growth_model) <- c("no action", as.character(1:13))
hi_rankings <- rev(sort(hi_dens_growth_model))

# max
seeds <- fallRunDSM::fall_run_model(mode = "seed", stochastic = FALSE, prey_density = "max")
max_dens_growth_model <-
  purrr::map_dbl(scenarios, ~run_scenario(., seeds, prey_dens = "max"))
names(max_dens_growth_model) <- c("no action", as.character(1:13))
max_rankings <- rev(sort(max_dens_growth_model))

orig_rank <- c(`5` = 89655.55, `6` = 87771.85, `11` = 86170.45, `3` = 85571.85,
               `2` = 85571.85, `1` = 85127, `10` = 84921.1, `9` = 82825.75,
               `12` = 82803.35, `13` = 82725.9, `8` = 82598.85, `no action` = 82516.95,
               `7` = 81975.15, `4` = 81616.55)

ranks <- tibble(
  low_dens = names(low_rankings),
  med_dens = names(med_rankings),
  hi_dens = names(hi_rankings),
  max_dens = names(max_rankings),
  original_rank = names(orig_rank)
)

ranks_values <- tibble(
  low_dens = low_rankings,
  med_dens = med_rankings,
  hi_dens = hi_rankings,
  max_dens = max_rankings,
  original_rank = orig_rank
)

readr::write_csv(ranks, "data-raw/scenario-ranks.csv")
readr::write_csv(ranks_values, "data-raw/scenario-ranks-vals.csv")


# plots -------------------------------


# LOW
low_seeds <- fall_run_model(mode = "seed", prey_density = "low")
low_dens_sim <- fall_run_model(mode = "simulate", seeds = low_seeds, prey_density = "low",
                               track_juveniles = TRUE)
low_dens_outmigration_dist <- low_dens_sim$delta_fish |>
  as_tibble() |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  group_by(year, month, size) |>
  summarise(valley_total = sum(count)) |>
  ungroup() |>
  group_by(year) |>
  mutate(annual_total = sum(valley_total)) |>
  ungroup() |>
  mutate(
    prop_of_year = valley_total / annual_total
  ) |>
  group_by(month, size) |>
  summarise(avg_prop = mean(prop_of_year)) |>
  ungroup() |>
  mutate(month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")))

low_plot <- low_dens_outmigration_dist |>
  ggplot(aes(month_label, avg_prop, fill=size_label)) +
  geom_col() +
  scale_fill_brewer(palette = "BrBG") +
  theme_gray() +
  ylim(c(0, 1)) +
  labs(x = "", y = "Proportion Outmigrating",
       title = "Prey Density: LOW",
       fill = "Size")



# MED
med_seeds <- fall_run_model(mode = "seed", prey_density = "med")
med_dens_sim <- fall_run_model(mode = "simulate", seeds = med_seeds, prey_density = "med",
                               track_juveniles = TRUE)

med_dens_outmigration_dist <- med_dens_sim$delta_fish |>
  as_tibble() |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  group_by(year, month, size) |>
  summarise(valley_total = sum(count)) |>
  ungroup() |>
  group_by(year) |>
  mutate(annual_total = sum(valley_total)) |>
  ungroup() |>
  mutate(
    prop_of_year = valley_total / annual_total
  ) |>
  group_by(month, size) |>
  summarise(avg_prop = mean(prop_of_year)) |>
  ungroup() |>
  mutate(month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")))

med_plot <- med_dens_outmigration_dist |>
  ggplot(aes(month_label, avg_prop, fill=size_label)) +
  geom_col() +
  scale_fill_brewer(palette = "BrBG") +
  theme_gray() +
  ylim(c(0, 1)) +
  labs(x = "", y = "Proportion Outmigrating",
       title = "Prey Density: MED",
       fill = "Size")



# HI
hi_seeds <- fall_run_model(mode = "seed", prey_density = "hi")
hi_dens_sim <- fall_run_model(mode = "simulate", seeds = hi_seeds, prey_density = "hi",
                               track_juveniles = TRUE)

hi_dens_outmigration_dist <- hi_dens_sim$delta_fish |>
  as_tibble() |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  group_by(year, month, size) |>
  summarise(valley_total = sum(count)) |>
  ungroup() |>
  group_by(year) |>
  mutate(annual_total = sum(valley_total)) |>
  ungroup() |>
  mutate(
    prop_of_year = valley_total / annual_total
  ) |>
  group_by(month, size) |>
  summarise(avg_prop = mean(prop_of_year)) |>
  ungroup() |>
  mutate(month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")))

hi_plot <- hi_dens_outmigration_dist |>
  ggplot(aes(month_label, avg_prop, fill=size_label)) +
  geom_col() +
  scale_fill_brewer(palette = "BrBG") +
  theme_gray() +
  ylim(c(0, 1)) +
  labs(x = "", y = "Proportion Outmigrating",
       title = "Prey Density: HI",
       fill = "Size")



# MAX
max_seeds <- fall_run_model(mode = "seed", prey_density = "max")
max_dens_sim <- fall_run_model(mode = "simulate", seeds = max_seeds, prey_density = "max",
                               track_juveniles = TRUE)

max_dens_outmigration_dist <- max_dens_sim$delta_fish |>
  as_tibble() |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  group_by(year, month, size) |>
  summarise(valley_total = sum(count)) |>
  ungroup() |>
  group_by(year) |>
  mutate(annual_total = sum(valley_total)) |>
  ungroup() |>
  mutate(
    prop_of_year = valley_total / annual_total
  ) |>
  group_by(month, size) |>
  summarise(avg_prop = mean(prop_of_year)) |>
  ungroup() |>
  mutate(month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")))

max_plot <- max_dens_outmigration_dist |>
  ggplot(aes(month_label, avg_prop, fill=size_label)) +
  geom_col() +
  scale_fill_brewer(palette = "BrBG") +
  theme_gray() +
  ylim(c(0, 1)) +
  labs(x = "", y = "Proportion Outmigrating",
       title = "Prey Density: MAX",
       fill = "Size")

load(file = "../../cvpia-fw/original-plot.rdata")

grid.arrange(low_plot, med_plot, hi_plot, max_plot, original_plot, nrow = 2)

low_plot + med_plot + hi_plot + max_plot + original_plot +
  guide_area() +
  plot_layout(nrow = 2, guides = "collect") +
  theme_minimal()



outmigration_dist_all_dens <- bind_rows(
  low_dens_outmigration_dist,
  med_dens_outmigration_dist,
  hi_dens_outmigration_dist,
  max_dens_outmigration_dist
)



