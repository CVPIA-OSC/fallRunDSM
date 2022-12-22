# in this script i show how to use the output when the model is set to
# keep_all_data = TRUE

library(purrr)
library(tidyverse)

s <- fall_run_model()
res <- fall_run_model(seeds = s, mode = "sim", keep_all_data = TRUE)

# upper mid sac fish 20 years, 8 months, 15 watersheds, 4 size classes
dim(res$upper_mid_sac_fish)


upper_mid_sac_fish <- res$upper_mid_sac_fish
s <- upper_mid_sac_fish[,,"Upper Sacramento River", 's']  |>
  as_tibble() |>
  mutate(size_class = "small",
         year = row_number())
m <- upper_mid_sac_fish[,,"Upper Sacramento River", 'm']  |>
  as_tibble() |>
  mutate(size_class = "medium",
         year = row_number())
l <- upper_mid_sac_fish[,,"Upper Sacramento River", 'l']  |>
  as_tibble() |>
  mutate(size_class = "large",
         year = row_number())
vl <- upper_mid_sac_fish[,,"Upper Sacramento River", 'vl']  |>
  as_tibble() |>
  mutate(size_class = "very large",
         year = row_number())





all_data <- map_df(fallRunDSM::watershed_labels[1:15], function(w) {
  s <- upper_mid_sac_fish[,,w, 's']  |>
    as_tibble() |>
    mutate(size_class = "small",
           year = row_number(),
           watershed = w)
  m <- upper_mid_sac_fish[,,w, 'm']  |>
    as_tibble() |>
    mutate(size_class = "medium",
           year = row_number(),
           watershed = w)
  l <- upper_mid_sac_fish[,,w, 'l']  |>
    as_tibble() |>
    mutate(size_class = "large",
           year = row_number(),
           watershed = w)
  vl <- upper_mid_sac_fish[,,w, 'vl']  |>
    as_tibble() |>
    mutate(size_class = "very large",
           year = row_number(),
           watershed = w)

  bind_rows(s, m, l, vl)

})


all_data2 <- all_data |>
  pivot_longer(Jan:Aug, names_to = "month", values_to = "juvs") |>
  mutate(year = year + 1979,
         date = lubridate::as_date(paste0(year, "-", month, "-01")))


all_data2 |>
  filter(watershed == "Upper Sacramento River") |>
  ggplot(aes(date, juvs, color = size_class)) + geom_line()


all_data2 |>
  filter(year == 1980, month == "Feb")










