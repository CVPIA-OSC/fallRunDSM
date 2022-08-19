library(fallRunDSM)
library(tidyverse)

seeds <- fall_run_model()

sim <- fall_run_model(seeds = seeds, mode = "simulate")

floodplain_rearing_survival <- as_tibble(sim$fp_rearing_survival) |>
  mutate(
    size = factor(size, levels = c("s", "m", "l", "vl")),
    month_label = factor(month.abb[month], levels = month.abb)
  )

floodplain_rearing_survival |>
  filter(watershed == "San Joaquin River",
         year == 10) |>
  ggplot(aes(month, survival, color = size)) + geom_line() +
  scale_y_log10()

floodplain_rearing_survival |>
  group_by(watershed, month_label, size) |>
  summarise(
    avg_survival = mean(survival),
    min_survival = min(survival),
    max_survival = max(survival)
  ) |>
  filter(watershed == "American River") |>
  ggplot(aes(month_label, avg_survival, fill = size)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "BrBG")
