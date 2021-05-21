#' Adult Harvest Rate
#' @description Proportion of adults harvested from golden gate until they reach their natal shed
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"adult_harvest_rate"

#' Natural Spawners Removal Rate
#' @description Spawners removed for hatcheries
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"natural_adult_removal_rate"

#' Hatchery Allocation
#' @description The proportion of hatchery fish spawning
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"hatchery_allocation"

#' @title Adult Seeds
#' @description adult fish for the initial 5 years of the simulations derived
#' from average escapement estimates from 2013 to 2017 (Azat 2019).
#' @format A matrix with dimension 31 x 30 (watershed x year)
"adult_seeds"

#' @title Proportion Hatchery
#' @description TODO
"proportion_hatchery"

#' @title Month Return Proportions
#' @description the proportion of spawning fish in Oct-Dec
"month_return_proportions"

#' @title Mass by Size Class
#' @description mass of fish by the size class
"mass_by_size_class"

#' @title Cross Channel Stray Rate
#' @description TODO
"cross_channel_stray_rate"

#' @title Stray Rate
#' @description TODO
"stray_rate"

#' @title Model Parameters from Calibration
#' @description calibration derived parameters used in submodels.
"params"

#' @title Diversity Groups
#' @description TODO
"diversity_group"

#' @rdname growth
"growth_rates"

#' @rdname growth_floodplain
"growth_rates_floodplain"
