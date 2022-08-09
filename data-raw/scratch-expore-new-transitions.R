library(purrr)
library(tidyverse)

growth_rates_current <- params$growth_rates


# month 1 year 1
growth_rates_new <- get_growth_rates(temperature = 1:28, prey_density = "max")


growth_rates_df <- map_df(1:28, function(x) {
  growth_rates_new[,,x] |> as_tibble() |>
    mutate(size_from = c("s", "m", "l", "vl")) |>
    pivot_longer(names_to = "size_to", values_to = "prob", s:vl) |>
    mutate(temperature = x)

}) |>
  mutate(
    size_from = factor(size_from, levels = c("s", "m", "l", "vl")),
    size_to = factor(size_to, levels = c("s", "m", "l", "vl"))
  )

growth_rates_df |>
  ggplot() +
  geom_raster(aes(size_from, size_to, fill = prob), interpolate = FALSE) +
  scale_fill_gradient(low = "blue", high = "red") +
  facet_wrap(vars(temperature), nrow = 4)

