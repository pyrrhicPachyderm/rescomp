#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode()`)
#'
#' @param pars S3 object of class `rescomp` returned by
#'     `rescomp::spec_rescomp()`.
#' @param stochastic A boolean. If TRUE, use stochastic simulation,
#'     via tau leaping instead of differential equations.
#' @param totaltime Numeric vector of length 1: the total simulation time.
#'     If provided, overrides the value in `pars`.
#' @param cinit Numeric vector of length 1 or length `spnum` specifying
#'     initial consumer state values (densities).
#'     If provided, overrides the value in `pars`.
#' @param rinit Numeric vector of length 1 or length `resnum` specifying
#'     initial resource state values (concentrations).
#'     If provided, overrides the value in `pars`.
#' @param ... Other arguments passed to `deSolve::ode()`
#'
#' @return A list of two comprising i) the model dynamics and ii) model
#'     specifications.
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' sim_rescomp(pars = pars)
sim_rescomp <- function(pars, stochastic = FALSE, totaltime, cinit, rinit, ...) {
  # TODO: For parameters that override the values in `pars`, error-check them the same as spec_rescomp().
  # Write helper functions for error-checking that can be called both here and in spec_rescomp().
  if (!missing(totaltime)) {
    pars$totaltime <- totaltime
    cli::cli_alert_info("Overwriting {.arg totaltime} in {.arg pars}.")
  }
  if (!missing(cinit)) {
    pars$cinit <- cinit
    cli::cli_alert_info("Overwriting {.arg cinit} in {.arg pars}.")
  }
  if (!missing(rinit)) {
    pars$rinit <- rinit
    cli::cli_alert_info("Overwriting {.arg rinit} in {.arg pars}.")
  }

  pars$event_schedule_df <- prepare_event_schedule_df(pars$events, pars$totaltime)
  y <- c(pars$cinit, pars$rinit)

  if (!stochastic) {
    times <- seq(0, pars$totaltime, by = 0.1) # TODO: Make step size customisable.

    if (nrow(pars$event_schedule_df) > 0) {
      events <- list(
        func = ode_event_func,
        time = pars$event_schedule_df$time
      )
    } else {
      events <- list()
    }

    mod <- deSolve::ode(
      func = def_cr_ode,
      y = y,
      parms = pars,
      times = times,
      events = events,
      ...
    )
  } else {
    # TODO: Handle events.
    mod <- run_ssa(y, pars, start_time = 0, run_time = pars$totaltime)
  }

  out <- list(mod, pars[])
  return(out)
}
