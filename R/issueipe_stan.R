#' Run Stan Model
#'
#' This function runs a Stan model using the `rstan::sampling` function to
#' estimate issue specific ideal points.
#' It allows for specifying various parameters for the Stan model execution.
#'
#' @param data A list of data for Stan.
#' @param init A list of initial values for Stan.
#' @param chains Number of chains (default: 2).
#' @param iter Number of iterations (default: 10).
#' @param warmup Number of warmup iterations (default: 5).
#' @param thin Thinning rate (default: 1).
#' @param seed Random seed (default: 1).
#' @param ... Other arguments to be passed to `rstan::sampling`.
#'
#' @return A Stan fit object.
#' @useDynLib issueipe, .registration=TRUE
#' @export
issueipe_stan <- function(
    data,
    init,
    chains = 2,
    iter = 10,
    warmup = 5,
    thin = 1,
    seed = 1,
    ...) {
  stan_fit <- rstan::sampling(
    object = stanmodels$issueipe,
    data = data,
    init = init,
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    seed = seed,
    ...
  )
  return(stan_fit)
}
