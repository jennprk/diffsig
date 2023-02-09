#' Diffsig coverage and root mean squared error (RMSE) calculation function
#'
#' @export
#' @param betatrue a vector of true betas
#' @param fit a `stanfit` object returned by `rstan::stan`
#' @param ci_level a 2-dimension vector of lower/upper credible interval
#' @param ... Arguments passed to `rstan::stan` (e.g. thin, init, ...).
#' @return An object of class `stanfit` returned by `rstan::stan`
#' @example diffsig_eval(simdat$beta, fit,)
#'
diffsig_eval <- function(betatrue, fit, ci_level=80, ...) {

  if (class(fit)!="stanfit") {
    stop("fit has to be a stanfit object from rstan package")
  }

  beta_vec = c(t(betatrue))
  ci_low = (100-ci_level)/2/100
  ci_up = 1-ci_low

  estimate= c()
  for (i in 1:length(beta_vec)) {
    estimate= rbind(estimate,
                    summary(fit,pars="beta", probs=c(ci_low,ci_up))$summary[i,c(1,4,5)])
  }
  estimate = as.data.frame(cbind(beta_vec,estimate))
  colnames(estimate) = c("truebeta","estimate","min","max")

  for (i in 1:length(beta_vec)) {
    estimate$contain[i] = estimate[i,"truebeta"] > estimate[i,"min"] && estimate[i,"truebeta"] < estimate[i,"max"]
    estimate$rmse[i] = sqrt(mean((estimate$truebeta[i] - estimate$estimate[i])^2))
  }

  colnames(estimate) = c("truebeta","estimate","10%","90%","contain","rmse")

  return(list(estimate,
              perc_coverage=mean(estimate$contain),
              mean_rmse=mean(estimate$rmse)))
}
