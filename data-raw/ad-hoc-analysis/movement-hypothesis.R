library(fallRunDSM)
library(tidyverse)


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
                                                          "three", "four", "five"))) |>
  filter(cal_year == 1990) |>
  ggplot(aes(month_label, prop_fish, fill = size_label)) +
  geom_col(position = "dodge") +
  facet_wrap(. ~ hypothesis_label)


american_river_props |>
  filter(year == 1, hypothesis == "zero") |>
  select(month, year, size, count) |>
  pivot_wider(names_from = size, values_from = count)


usac_h0 <- list(inchannel = structure(c(1887033, 0, 212268, 0, 17525, 0,
                                        784596, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = c(15L, 4L), dimnames = list(
                                          c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                                            "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                                            "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                                            "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek"
                                          ), c("s", "m", "l", "vl"))), floodplain = structure(c(441425,
                                                                                                22551, 0, 17293, 0, 92892, 29301, 882198, 323205, 137044, 18029,
                                                                                                67131, 21617, 16959, 17477, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = c(15L, 4L)), migrants = structure(c(3937863,
                                                                                                                                                                                  0, 536800, 0, 8, 0, 378315, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                                                                ), dim = c(15L, 4L), dimnames = list(c("Upper Sacramento River",
                                                                                                                                       "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                                                                                                       "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                                                                                                       "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                                                                                                       "Thomes Creek"), c("fry", "", "", ""))))


usac_h1 <- list(inchannel = structure(c(4384441, 15779, 524112, 12100, 12267,
                                        64995, 834173, 617260, 226141, 95888, 12614, 46971, 15125, 11866,
                                        12228, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0), dim = c(15L, 4L), dimnames = list(c("Upper Sacramento River",
                                                                                               "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                                                               "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                                                               "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                                                               "Thomes Creek"), c("fry", "", "", ""))), floodplain = structure(c(441425,
                                                                                                                                                                 22551, 0, 17293, 0, 92892, 29301, 882198, 323205, 137044, 18029,
                                                                                                                                                                 67131, 21617, 16959, 17477, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = c(15L, 4L)), migrants = structure(c(1881880,
                                                                                                                                                                                                                                                   6772, 224956, 5193, 5266, 27897, 358039, 264938, 97064, 41156,
                                                                                                                                                                                                                                                   5415, 20160, 6492, 5093, 5249, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                                                                                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                                                                                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = c(15L, 4L), dimnames = list(
                                                                                                                                                                                                                                                     c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                                                                                                                                                                                                                                                       "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                                                                                                                                                                                                                                                       "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                                                                                                                                                                                                                                                       "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek"
                                                                                                                                                                                                                                                     ), c("fry", "", "", ""))))

# ------------------------

usac_h0$migrants - usac_h1$migrants




