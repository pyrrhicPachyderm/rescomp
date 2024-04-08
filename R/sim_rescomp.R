sim_rescomp <- function(pars, ...) {
  times <- time_vals(pars$totaltime)
  y <- c(pars$cinit, pars$rinit)

  mod <- deSolve::ode(
    func = def_cr_ode,
    y = y,
    parms = pars,
    times = times$totaltime,
    ...
  )

  out <- list(mod, pars[])
  return(out)
}
