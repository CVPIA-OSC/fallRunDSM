library(fallRunDSM)
library(tidyverse)


hypothesis_levels <- c(
  "Base fill + no additional movement",
  "Base fill + Snow Glove Movement",
  "Base fill + Genetics Movement",
  "Base fill + Temperature Movement",
  "Base fill + Time Movemennt",
  "Density fill + no additional movement",
  "Density fill + Snow Glove Movement",
  "Density fill + Genetics Movement",
  "Density fill + Temperature Movement",
  "Density fill + Time Movemennt"
)

s <- fall_run_model()
run <- fall_run_model(seeds = s, mode = "simulate")

valley_wide <-
  run$north_delta_fish |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  group_by(year, hypothesis, month, size) |> # valley sum
  summarise(valley_total = sum(count)) |>
  ungroup() |>
  group_by(year, hypothesis) |> # year totals
  mutate(
    year_total = sum(valley_total)
  ) |>
  ungroup() |>
  group_by(year, month, hypothesis) |>
  mutate(
    prop_fish = valley_total / year_total,
    prop_fish = ifelse(is.na(prop_fish), 0, prop_fish)
  )

valley_wide |>
  filter(year == 1) |>
  mutate(month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")),
         hypothesis_label = factor(hypothesis, levels = c("zero", "one", "two",
                                                          "three", "four", "five"))) |>
  ggplot(aes(month_label, prop_fish, fill = size_label)) +
  geom_col(position = "dodge") +
  facet_wrap(. ~ hypothesis_label)



# -----

american_river <- run$north_delta_fish |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  # mutate(cal_year = year + 1979) |>
  filter(watershed == "American River")


american_river_props <- american_river %>%
  group_by(watershed, year, hypothesis) %>%
  mutate(total_fish = sum(count)) %>%
  ungroup() %>%
  group_by(watershed, year, month, hypothesis) %>%
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) |>
  ungroup()


american_river_props |>
  mutate(cal_year = year + 1979,
         month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")),
         hypothesis_label = factor(hypothesis, levels = c("zero", "one", "two",
                                                          "three", "four", "five",
                                                          "six", "seven", "eight",
                                                          "nine"),
                                   labels = hypothesis_levels)) |>
  filter(cal_year == 1990) |>
  ggplot(aes(month_label, prop_fish, fill = size_label)) +
  geom_col(position = "dodge") +
  facet_wrap(~hypothesis_label, nrow = 2) +
  labs(title = "American River - 1990")


american_river_props |>
  filter(year == 1, hypothesis == "zero") |>
  select(month, year, size, count) |>
  pivot_wider(names_from = size, values_from = count)



# ----

upsac_river <- run$north_delta_fish |>
  pivot_longer(names_to = "size", values_to = "count", s:vl) |>
  # mutate(cal_year = year + 1979) |>
  filter(watershed == "Upper Sacramento River")


upsac_river_props <- upsac_river %>%
  group_by(watershed, year, hypothesis) %>%
  mutate(total_fish = sum(count)) %>%
  ungroup() %>%
  group_by(watershed, year, month, hypothesis) %>%
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) |>
  ungroup()


upsac_river_props |>
  mutate(cal_year = year + 1979,
         month_label = factor(month.abb[month], levels = month.abb),
         size_label = factor(size, levels = c("s", "m", "l", "vl")),
         hypothesis_label = factor(hypothesis, levels = c("zero", "one", "two",
                                                          "three", "four", "five",
                                                          "six", "seven", "eight",
                                                          "nine"),
                                   labels = hypothesis_levels)) |>
  filter(cal_year == 1997) |>
  ggplot(aes(month_label, prop_fish, fill = size_label)) +
  geom_col(position = "dodge") +
  facet_wrap(~hypothesis_label, nrow = 2)


american_river_props |>
  filter(year == 1, hypothesis == "zero") |>
  select(month, year, size, count) |>
  pivot_wider(names_from = size, values_from = count)

