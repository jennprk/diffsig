#' Hierarchical Bayesian Modeling with Stan
#'
#' @export
#' @param x risk factor matrix (MxN)
#' @param y mutational count matrix (LxN)
#' @param C muational signature matrix (LxK)
#' @param beta_sd standard deviation for sampling hyperparameter beta
#' @param pars desired parameters from `stanfit` to save. Default is "beta" (pars="beta"), but can also include parameters such as "tau".
#' @param ... Arguments passed to `rstan::stan` (e.g. thin, init, ...).
#' @return An object of class `stanfit` returned by `rstan::stan`
#'
diffsig_fit <- function(X, Y, C, beta_sd, pars=c('beta'), include = T, ...) {
  require(rstan)

  start = Sys.time()
  data <- list(X = X, Y = Y, C = C, beta_sd = beta_sd,
                   M = nrow(X), L = nrow(C), K = ncol(C), N = ncol(Y))
  out <- rstan::sampling(stanmodels$diffsig, data = data,
                         pars = pars, include = include, ...)
  end = Sys.time()
  total_time = difftime(end,start,units="mins")
  print(paste0("total computation time ",total_time, "mins"))

  # return(list(out,total_time))
  return(out)
}
