#' Diffsig coverage and root mean squared error (RMSE) calculation function
#'
#' @export
#' @param truebeta a vector of true betas
#' @param fit a `stanfit` object returned by `rstan::stan`
#' @param coverage a 2-dimension vector of lower/upper credible interval
#' @param beta_sd standard deviation for sampling hyperparameter beta
#' @param ... Arguments passed to `rstan::stan` (e.g. thin, init, ...).
#' @return An object of class `stanfit` returned by `rstan::stan`
#'
diffsig_fit <- function(truebeta, fit, credibleInterval, include = T, ...) {

  if (!is.numeric(truebeta)) {
    stop("beta values should be numeric")
  }
  for (i in 1:length(truebeta)) {
    c(summary(fit, pars="beta")$summary[j,1],
    summary(fit, pars="beta", probs=c(.1,.9))$summary[j,4],
    summary(fit, pars="beta", probs=c(.1,.9))$summary[j,5])))


  }


mutate(truebeta = as.numeric(truebeta),
       estimate = as.numeric(estimate),
       min = as.numeric(min),
       max = as.numeric(max))

res_liver1$contain[i] = res_liver1[i,"truebeta"] > res_liver1[i,"min"] && res_liver1[i,"truebeta"] < res_liver1[i,"max"]
res_liver1$rmse[i] = sqrt(mean((res_liver1$truebeta[i] - res_liver1$estimate[i])^2))

  return(out)
}
