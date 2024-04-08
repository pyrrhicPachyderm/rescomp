spec_rescomp <- function(spnum = 1,
                         resnum = 1,
                         funcresp,
                         quota,
                         essential = FALSE,
                         mort = 0.03,
                         ressupply,
                         params = rescomp_param_list(),
                         totaltime = 1000,
                         cinit = 10,
                         rinit = 10,
                         verbose = TRUE) {
  pars <- list(
    spnum = spnum,
    resnum = resnum,
    funcresp = funcresp,
    quota = quota,
    essential = essential,
    mort = mort,
    ressupply = ressupply,
    params = params,
    totaltime = totaltime,
    cinit = cinit,
    rinit = rinit
  )

  class(pars) <- "rescomp"
  if (verbose == TRUE) {
    print(pars)
  }

  invisible(pars)
}

#TODO: print.rescomp
