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
diffsig_eval <- function(truebeta, fit, credibleInterval=80, include = T, ...) {

  if (class(fit)!="stanfit") {
    stop("fit has to be a stanfit object from rstan package")
  }

  truebeta = c(t(simdat$truebeta))
  CI_low = (100-credibleInterval)/2/100
  CI_up = 1-CI_low

  estimate= c()
  for (i in 1:length(truebeta)) {
    estimate= rbind(estimate,summary(fit, pars="beta", probs=c(CI_low,CI_up))$summary[i,c(1,4,5)])
  }
  estimate = as.data.frame(cbind(truebeta,estimate))
  colnames(estimate) = c("truebeta","estimate","min","max")

  for (i in 1:length(truebeta)) {
    estimate$contain[i] = estimate[i,"truebeta"] > estimate[i,"min"] && estimate[i,"truebeta"] < estimate[i,"max"]
    estimate$rmse[i] = sqrt(mean((estimate$truebeta[i] - estimate$estimate[i])^2))
  }

  colnames(estimate) = c("truebeta","estimate","10%","90%","contain","rmse")

  return(list(estimate, perc_coverage=mean(estimate$contain), mean_rmse=mean(estimate$rmse)))
}
