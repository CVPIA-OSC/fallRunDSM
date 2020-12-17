library(tidyverse)

hatchery_data <- read_csv("data-raw/dsm-parameter-docs/cwt-and-temp-analysis/hatchery_fish_fate2010_2012.csv",
                          skip = 1,
                          col_names = c(
                            "trib", "prop_hatchery", "prop_natural",
                            "total_escape", "year", "hatchery_returns",
                            "source", "is_hatchery", "run"
                          ),
                          col_types = cols(
                            trib = col_character(),
                            prop_hatchery = col_double(),
                            prop_natural = col_double(),
                            total_escape = col_double(),
                            year = col_double(),
                            hatchery_returns = col_double(),
                            source = col_character(),
                            is_hatchery = col_double(),
                            run = col_character()
                          )
)

proportion_of_spring <- tibble(
  prop = c(0.076777295, 0.056932196, 0.081441457),
  year = 2010:2012
)

feather_yuba <-
  hatchery_data %>%
  filter(trib %in% c("Feather River", "Yuba River")) %>%
  left_join(proportion_of_spring, by = c("year" = "year"))

feather_yuba_fall <-
  feather_yuba %>%
  mutate(
    run = "Fall",
    total_escape = total_escape * (1 - prop),
    hatchery_returns = hatchery_returns * (1 - prop)
  ) %>%
  select(-prop)

feather_yuba_spring <-
  feather_yuba %>%
  mutate(
    run = "Spring",
    total_escape = total_escape * prop,
    hatchery_returns = hatchery_returns * prop
  ) %>%
  select(-prop)

feather_yuba_updates <- bind_rows(feather_yuba_fall, feather_yuba_spring)


# replace existing yuba/feather with these updated values
hatchery_data2 <- hatchery_data %>%
  filter(!(trib %in% c("Feather River", "Yuba River"))) %>%
  add_row(feather_yuba_updates)



# Fall Run ----------------------------------------------------------------

hatchery_fall_run <- hatchery_data2 %>%
  filter(run == "Fall")

# hatchery returns by year
fall_run_hatchery_returns <- hatchery_fall_run %>%
  group_by(year) %>%
  summarise(hatchery_returns = sum(hatchery_returns))

wild_totals <- hatchery_fall_run %>%
  filter(is_hatchery == 0) %>%
  group_by(year) %>%
  summarise(total = sum(hatchery_returns))

wild_totals %>%
  left_join(fall_run_hatchery_returns) %>%
  mutate(
    x = total / hatchery_returns,

  )































