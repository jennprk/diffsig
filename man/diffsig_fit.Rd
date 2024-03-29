% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffsig_fit.R
\name{diffsig_fit}
\alias{diffsig_fit}
\title{Hierarchical Bayesian Modeling with Stan}
\usage{
diffsig_fit(X, Y, C, beta_sd, pars = c("beta"), include = T, ...)
}
\arguments{
\item{X}{risk factor matrix (MxN)}

\item{Y}{mutational count matrix (96xN)}

\item{C}{muational signature matrix (96xK)}

\item{beta_sd}{standard deviation for sampling hyperparameter beta}

\item{pars}{desired parameters from \code{stanfit} to save. Default is "beta" (\code{pars="beta"}), but can also include parameters such as "tau".}

\item{include}{passes on to \code{include} in \code{rstan::sampling}. Default set as \code{TRUE}.}

\item{...}{arguments passed to \code{rstan::stan} (e.g. thin, init, ...).}
}
\value{
An object of class \code{stanfit} returned by \code{rstan::stan}
}
\description{
Hierarchical Bayesian Modeling with Stan
}
\examples{
data(simdat)
data(C_k)
diffsig_fit(simdat$X, simdat$Y, C_k, pars="beta")

}
