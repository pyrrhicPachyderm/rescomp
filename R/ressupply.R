#' Create a resource supply rate using an arbitrary function
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param func A function that takes `resources` (a numeric vector of resource concentrations) and `params` (a list of parameters) and returns a vector of rates of change of each resource.
#' @param decrease_func A function as specified for `func`, save that the result is a rate of decrease for each resource (specified as a non-negative number).
#' @param resnum The number of resources; the expected length of the `resources` argument to `func` and the length of the vector returned by `func`.
#'
#' @details
#' If `resnum` is NULL, `spec_rescomp()` will attempt to infer it.
#' This is fine if the result is passed directly to `spec_rescomp()`, but may fail if the result must be combined with other `rescomp_ressupply` first.
#' If performing stochastic simulation, it is required that `func` and `decrease_func` always return non-negative numbers, which are taken as the rates of positive and negative resource transitions.
#' The total rate of change of resources is the difference between `func` and `decrease_func`.
#' For non-stochastic simulation (or as long as resource supply is always non-negative), it suffices to specify the entire resource supply function in `func`, and leave `decrease_func` as NULL.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' # Two resources, A and B, with constant supply of A, and A spontaneously converting to B
#' ressupply <- ressupply_custom(
#'   function(resources, params) {
#'     conversion <- params$conversion * resources[1]
#'     return(c(params$supply - conversion, conversion))
#'   },
#'   resnum = 2
#' )
#' get_ressupply(ressupply, c(10, 20), list(supply = 3, conversion = 0.2))
ressupply_custom <- function(func, decrease_func = NULL, resnum = NULL) {
  ressupply <- list(func = func, decrease_func = decrease_func, resnum = resnum)
  class(ressupply) <- c("rescomp_ressupply_custom", "rescomp_ressupply")
  return(ressupply)
}

#' Create a resource supply rate using a constant rate resource supply
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param rate A vector or `rescomp_coefs_vector`, with one number per resource. The supply rate of each resource.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' ressupply <- ressupply_constant(c(0.2, 0.3))
#' get_ressupply(ressupply, c(2, 10), list())
#' get_ressupply(ressupply, c(5, 20), list())
#' # The above two give the same result; constant supply doesn't depend on existing concentration.
#'
#' ressupply <- ressupply_constant(rescomp_coefs_lerp(c(0.2, 0.3), c(0.4, 0.6), "extra_supply"))
#' get_ressupply(ressupply, c(2, 10), list(extra_supply = 0.2))
#' get_ressupply(ressupply, c(2, 10), list(extra_supply = 0.8))
ressupply_constant <- function(rate) {
  check_coefs_vector(rate)
  ressupply <- list(rate = rate, resnum = get_coefs_length(rate))
  class(ressupply) <- c("rescomp_ressupply_constant", "rescomp_ressupply")
  return(ressupply)
}

#' Create a resource supply rate using logistic resource growth
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param r A vector or `rescomp_coefs_vector`, with one number per resource. The intrinsic growth rate of each resource.
#' @param k A vector or `rescomp_coefs_vector`, with one number per resource. The carrying capacity of each resource.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' ressupply <- ressupply_logistic(
#'   r = rescomp_coefs_lerp(c(0.2, 0.3), c(0, 0), "growth_inhibition"),
#'   k = c(10, 20)
#' )
#' get_ressupply(ressupply, c(2, 10), list(growth_inhibition = 0))
#' get_ressupply(ressupply, c(20, 0), list(growth_inhibition = 0))
#' get_ressupply(ressupply, c(2, 10), list(growth_inhibition = 0.8))
ressupply_logistic <- function(r, k) {
  check_coefs_vector(r)
  check_coefs_vector(k)
  check_coefs_coordinate(r, k)
  ressupply <- list(r = r, k = k, resnum = get_coefs_length(r))
  class(ressupply) <- c("rescomp_ressupply_logistic", "rescomp_ressupply")
  return(ressupply)
}

#' Create a resource supply rate using chemostat dynamics
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param dilution A numeric vector or `rescomp_coefs_vector`, of length one.
#' @param concentration A vector or `rescomp_coefs_vector`, with one number per resource. The concentration of each resource in the incoming medium.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' ressupply <- ressupply_chemostat(
#'   dilution = 0.01,
#'   concentration = rescomp_coefs_lerp(c(0, 0, 0), c(2, 3, 4), "ressupply_scaling")
#' )
#' get_ressupply(ressupply, c(2, 4, 10), list(ressupply_scaling = 0))
#' get_ressupply(ressupply, c(3, 3, 3), list(ressupply_scaling = 1))
ressupply_chemostat <- function(dilution, concentration) {
  check_coefs_vector(dilution, length = 1)
  check_coefs_vector(concentration)
  ressupply <- list(dilution = dilution, concentration = concentration, resnum = get_coefs_length(concentration))
  class(ressupply) <- c("rescomp_ressupply_chemostat", "rescomp_ressupply")
  return(ressupply)
}

#' Get resource supply rates from a `rescomp_ressupply` object
#'
#' `get_ressupply` gets the resource supply rates of each resource, given the current resource concentrations.
#' It is simply the difference between the results of `get_ressupply_increase` and ``get_ressupply_decrease`.
#' It is only important that increases and decreases are treated differently when performing stochastic simulations,
#' as these are the independent rates of positive and negative transitions.
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_ressupply` objects.
#'
#' @param ressupply_obj An object of class `rescomp_funcresp`.
#' @param resources A vector of resource concentrations.
#' @param params A list of time-dependent parameters.
#'
#' @returns A vector of rates of change of resource concentrations, of the same length as `resources`.
#' @export
#'
#' @examples
#' # Two resources, A and B, with constant supply of A, and A spontaneously converting to B
#' ressupply <- ressupply_custom(
#'   function(resources, params) {
#'     conversion <- params$conversion * resources[1]
#'     return(c(params$supply - conversion, conversion))
#'   },
#'   resnum = 2
#' )
#' get_ressupply(ressupply, c(10, 20), list(supply = 3, conversion = 0.2))
#' try(get_ressupply(ressupply, c(10, 20, 30), list(supply = 3, conversion = 0.2)))
get_ressupply <- function(ressupply_obj, resources, params) {
  UseMethod("get_ressupply")
}

#' @rdname get_ressupply
#' @export
get_ressupply_increase <- function(ressupply_obj, resources, params) {
  UseMethod("get_ressupply_increase")
}

#' @rdname get_ressupply
#' @export
get_ressupply_decrease <- function(ressupply_obj, resources, params) {
  UseMethod("get_ressupply_decrease")
}

#' @export
get_ressupply.default <- function(ressupply_obj, resources, params) {
  return(get_ressupply_increase(ressupply_obj, resources, params) - get_ressupply_decrease(ressupply_obj, resources, params))
}

#' @export
get_ressupply_increase.default <- function(ressupply_obj, resources, params) {
  vec <- get_ressupply(ressupply_obj, resources, params)
  vec[vec < 0] <- 0
  return(vec)
}

#' @export
get_ressupply_decrease.default <- function(ressupply_obj, resources, params) {
  vec <- get_ressupply(ressupply_obj, resources, params)
  vec[vec > 0] <- 0
  return(vec)
}

#' @export
get_ressupply_increase.rescomp_ressupply_custom <- function(ressupply_obj, resources, params) {
  vec <- ressupply_obj$func(resources, params)
  check_coefs(vec, length(resources), "`func` of `ressupply_custom`", "resnum")
  return(vec)
}

#' @export
get_ressupply_decrease.rescomp_ressupply_custom <- function(ressupply_obj, resources, params) {
  if (is.null(ressupply_obj$decrease_func)) {
    return(0)
  }
  vec <- ressupply_obj$decrease_func(resources, params)
  check_coefs(vec, length(resources), "`decrease_func` of `ressupply_custom`", "resnum")
  return(vec)
}

#' @export
get_ressupply.rescomp_ressupply_constant <- function(ressupply_obj, resources, params) {
  return(get_coefs_vector(ressupply_obj$rate, params))
}

#' @export
get_ressupply.rescomp_ressupply_logistic <- function(ressupply_obj, resources, params) {
  # TODO: I'm sure there's a way to describe logistic growth appropriately as separate birth and death rates, for get_ressupply_increase and get_ressupply_decrease.
  r <- get_coefs_vector(ressupply_obj$r, params)
  k <- get_coefs_vector(ressupply_obj$k, params)
  return(r * resources * (1 - resources / k))
}

#' @export
get_ressupply_increase.rescomp_ressupply_chemostat <- function(ressupply_obj, resources, params) {
  dilution <- get_coefs_vector(ressupply_obj$dilution, params)
  concentration <- get_coefs_vector(ressupply_obj$concentration, params)
  return(dilution * concentration)
}

#' @export
get_ressupply_decrease.rescomp_ressupply_chemostat <- function(ressupply_obj, resources, params) {
  dilution <- get_coefs_vector(ressupply_obj$dilution, params)
  return(dilution * resources)
}

#' @export
propagate_crnum.rescomp_ressupply_custom <- function(obj, spnum, resnum) {
  if (is.null(obj$resnum)) {
    obj$resnum <- resnum
  }
  return(obj)
}

#' @export
propagate_crnum.rescomp_ressupply_constant <- function(obj, spnum, resnum) {
  obj$rate <- propagate_rnum(obj$rate, resnum)
  return(obj)
}

#' @export
propagate_crnum.rescomp_ressupply_logistic <- function(obj, spnum, resnum) {
  obj$r <- propagate_rnum(obj$r, resnum)
  obj$k <- propagate_rnum(obj$k, resnum)
  return(obj)
}

#' @export
propagate_crnum.rescomp_ressupply_chemostat <- function(obj, spnum, resnum) {
  obj$concentration <- propagate_rnum(obj$concentration, resnum)
  return(obj)
}
