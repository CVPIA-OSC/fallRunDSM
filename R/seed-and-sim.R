#' @title Seed and Simulate
#' @description
#' A wrapper function to run both seeding and simulation with one function call. Using this function
#' also ensures that both seed and sim are run with same scenario and params.
#' @param scenario a scenarion matrix to use
#' @param ..params model params list to use for seeding and simulation
#' @param stochastic should the seedind and sim be run in stochastic mode?
#' @export
#' @md
fall_run_seed_and_sim <- function(scenario = NULL, ..params = fallRunDSM::params,
                              stochastic = FALSE, verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("running seeding")
  }

  seeds <- fall_run_model(scenario = scenario, mode = "seed", ..params = ..params, stochastic = stochastic)

  if (verbose) {
    cli::cli_alert_success("seeding complete")
    cli::cli_alert_info("running simulation")
  }

  sim <- fall_run_model(scenario = scenario, mode = "sim", seeds = seeds, ..params = ..params, stochastic = stochastic)

  if (verbose) {
    cli::cli_alert_success("simulation complete")
  }

  return(sim)
}
