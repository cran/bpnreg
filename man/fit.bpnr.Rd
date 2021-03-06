% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classfunctions.R
\name{fit.bpnr}
\alias{fit.bpnr}
\title{Model fit for a Bayesian circular regression model}
\usage{
\method{fit}{bpnr}(object)
}
\arguments{
\item{object}{a \code{bpnr object} obtained from the function \code{bpnr()}.}
}
\value{
a matrix containing the computed log pointwise predictive density
  (lppd), Deviance Information Criterion (DIC), an alternative version of the
  DIC (DIC_alt), and the Watanabe-Akaike Information Criterion computed in
  two different ways (WAIC1, WAIC2). The matrix also contains the number of
  parameters or 'effective number' of parameters that the several statistics
  are based on. Computation of the criteria is done according to Gelman et.al
  (2014) in *Bayesian Data Analysis*.
}
\description{
Outputs several model fit statistics for the Bayesian circular regression
model
}
\examples{
library(bpnreg)
fit.Motor <- bpnr(pred.I = Phaserad ~ 1 + Cond, data = Motor,
its = 100, burn = 10, n.lag = 3)
fit(fit.Motor)

}
