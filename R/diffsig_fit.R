#' Hierarchical Bayesian Modeling with Stan
#'
#' @export
#' @param x risk factor matrix (NxM)
#' @param y mutational count matrix (LxN)
#' @param C muational signature matrix (LxK)
#' @param beta_sd standard deviation for sampling hyperparameter beta
#' @param ... Arguments passed to `rstan::stan` (e.g. thin, init, ...).
#' @return An object of class `stanfit` returned by `rstan::stan`
#'
diffsig_fit <- function(x, y, C, beta_sd, pars=c('beta'), include = T, ...) {
  standata <- list(X = x, Y = y, C = C, beta_sd = beta_sd,
                   M = ncol(X), L = nrow(C), K = ncol(C), N = length(y))
  out <- rstan::sampling(stanmodels$stan_model, data = standata,
                         pars = pars, include = include, ...)
  return(out)
}
