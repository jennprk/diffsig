#' Hierarchical Bayesian Modeling with Stan
#'
#' @export
#' @param X risk factor matrix (MxN) - M: no. of risk factors including intercept, N: no. of samples
#' @param Y (mutational) count matrix (LxN) - L: dimension of count matrix (by default, L=96)
#' @param C (muational) signature matrix (LxK) - K: no. of signatures
#' @param beta_sd standard deviation for sampling hyperparameter beta
#' @param pars desired parameters from `stanfit` to save. Default is "beta" (`pars="beta"`), but can also include parameters such as "tau".
#' @param include passes on to `include` in `rstan::sampling`. Default set as `TRUE`.
#' @param ... arguments passed to `rstan::stan` (e.g. thin, init, ...).
#' @importFrom rstan sampling summary
#' @return An object of class `stanfit` returned by `rstan::stan`
#' @examples
#' data(simdat)
#' data(C_k)
#' diffsig_fit(simdat$X, simdat$Y, C_k, pars="beta")
#'
diffsig_fit <- function(X, Y, C, beta_sd, pars=c('beta'), include = T, ...) {
  start = Sys.time()
  data <- list(X = X, Y = Y, C = C, beta_sd = beta_sd,
                   M = nrow(X), L = nrow(C), K = ncol(C), N = ncol(Y))
  out <- rstan::sampling(stanmodels$diffsig, data = data,
                         pars = pars, include = include, ...)
  end = Sys.time()
  total_time = difftime(end,start,units="mins")
  print(paste0("total computation time ",total_time, "mins"))

  return(out)
  # return(list(out,total_time))
}
