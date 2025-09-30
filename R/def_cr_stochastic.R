# Order of transitions:
# - Consumer growth & consumption (consumer-major order)
# - Consumer death
# - Resource supply
# - Resource depletion

# If resources are essential, each consumer has a single growth transition, taking a dose of all resources.
# If resources are substitutable, each consumer has one growth transition per resource.

# Each growth transition changes the consumer density by 1, and adjusts the resources appropriately.
# This is definitely the desired behaviour if the quota is specified.
# There is an argument to be made that, if the efficiency is specified instead of the quota, each transition should represent the consumption of a single unit of resource, adjusting consumer density appropriately.
# While this works in the case of substitutable resources, it breaks down in the case of essential resources with different efficiencies on each resource.
# In this case, each transition would have to represent the consumption of one unit of the limiting resource, which may correspond to fractional consumption of other resources, which makes it useless.
# Ultimately, the size of a given transition matter primarily for allowing consumer/resource extinctions, which may prove impossible if fractional transitions occur (as then it cannot step all the way to zero).
# Thus, which is more important depends on whether consumer or resource extinction is more important, which varies by system and research question.
# However, consumer extinction seems more likely to be of interest in most cases, hence the current choice.
# Ideally, the quota should always be an integer, or better yet, always be 1, allowing extinction of both consumers and resources to occur properly.
# TODO: Add a warning when using a non-integer quota with stochastic simulation.

#' Get the list of transitions for a Gillespie algorithm simulation
#'
#' @param pars The `rescomp` object passed to `sim_rescomp()`.
#'
#' @return A list of transitions suitable to pass to `adaptivetau::ssa.adaptivetau()`.
#' @export
#'
#' @examples
#' # TODO
def_cr_transitions <- function(pars) {
  sp_indices <- 1:pars$spnum
  res_indices <- (pars$spnum + 1):(pars$spnum + pars$resnum)

  # TODO: There is a fundamental problem with using ssa.adaptivetau() here.
  # Namely, the transitions must all by defined up front, and only the rates of the transitions are allowed to change.
  # This does not work if the quota or efficiency are time-dependent.
  # For now, we shall assume that the quota and efficiency are non time-dependent, and use the values from time zero.
  # Fixing this properly will require ditching the dependency on the adaptivetau package.
  params <- get_params(pars$params, 0)
  if (is.null(pars$efficiency)) {
    quota <- get_coefs_matrix(pars$quota, params)
  } else {
    quota <- 1 / get_coefs_matrix(pars$efficiency, params)
  }

  if (pars$essential) {
    growth <- lapply(1:pars$spnum, function(i) {
      names <- c(sp_indices[i], res_indices)
      values <- c(1, quota)
      return(setNames(values, names))
    })
  } else {
    growth <- do.call(
      c,
      lapply(1:pars$spnum, function(i) {
        lapply(1:pars$resnum, function(j) {
          names <- c(sp_indices[i], res_indices[j])
          values <- c(1, quota[j])
          return(setNames(values, names))
        })
      })
    )
  }

  death <- lapply(1:pars$spnum, function(i) {
    names <- sp_indices[i]
    values <- -1
    return(setNames(values, names))
  })

  ressupply <- lapply(1:pars$resnum, function(i) {
    names <- res_indices[i]
    values <- 1
    return(setNames(values, names))
  })

  # TODO: It will be necessary to split ressupply into an addition and subtraction component, as the Gillespie algorithm does not allow for negative rates of positive transitions.
  # This can't be done here alone; this will require a fundamental change to the ressupply logic.

  return(c(growth, death, ressupply))
}
