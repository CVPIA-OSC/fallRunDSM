library(tidyverse)


transition_matrices <- read_rds("data-raw/growTPM.rds")

dim(transition_matrices)

transition_matrices[,,1,]


gr <- function(temperature, prey_density = c("low", "med", "hi", "max"),
               floodplain = FALSE) {

  temp_index <- ceiling(temperature)
  prey_density <- match.arg(prey_density)

  if (temp_index > 28 | temp_index < 1) {
    stop("temperature out of range of transition probability matrices, defined range is from 1C to 28C", call. = FALSE)
  }

  density_index <- if (floodplain) {
    paste0("floodplain_", prey_density)
  } else {
    paste0("perennial_", prey_density)
  }

  return(transition_matrices[,,temp_index, density_index])

}

