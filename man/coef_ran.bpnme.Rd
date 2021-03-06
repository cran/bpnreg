% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classfunctions.R
\name{coef_ran.bpnme}
\alias{coef_ran.bpnme}
\title{Obtain random effect variances of a Bayesian circular mixed-effects model}
\usage{
\method{coef_ran}{bpnme}(object, type = "linear")
}
\arguments{
\item{object}{a \code{bpnme object} obtained from the function
\code{\link{bpnme}}.}

\item{type}{one of \code{c("linear", "circular")} to get either the linear or
circular random effect variances.}
}
\value{
A matrix with posterior summaries of the random effect variances.
}
\description{
Gives posterior summaries of the circular or linear random effect variances.
The circular random intercept variance and circular random slope variance of
categorical predictors is computed as 1 - mean resultant length.
}
\examples{
library(bpnreg)
fit.Maps <- bpnme(pred.I = Error.rad ~ Maze + Trial.type + L.c + (1|Subject),
data = Maps,
its = 100, burn = 1, n.lag = 1)
coef_ran(fit.Maps)
coef_ran(fit.Maps, type = "circular")

}
