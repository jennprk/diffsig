#' Hierarchical Bayesian Modeling with Stan
#'
#' @export
#' @param x risk factor matrix (MxN)
#' @param y mutational count matrix (LxN)
#' @param C muational signature matrix (LxK)
#' @param beta_sd standard deviation for sampling hyperparameter beta
#' @param ... Arguments passed to `rstan::stan` (e.g. thin, init, ...).
#' @return An object of class `stanfit` returned by `rstan::stan`
#'
diffsig_fit <- function(x, y, C, beta_sd, pars=c('beta'), include = T, ...) {
  start = Sys.time()
  standata <- list(X = x, Y = y, C = C, beta_sd = beta_sd,
                   M = nrow(X), L = nrow(C), K = ncol(C), N = ncol(y))
  out <- rstan::sampling(stanmodels$stan_model, data = standata,
                         pars = pars, include = include, ...)
  end = Sys.time()
  total_time = difftime(end,start,units="mins")
  print(total_time)

  return(out)
}
