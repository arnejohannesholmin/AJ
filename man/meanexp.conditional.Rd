% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanexp.conditional.R
\name{meanexp.conditional}
\alias{meanexp.conditional}
\title{Locating the parameter of an exponentially distributed 'X' based on the mean of Y=X|X<C or Y=X|X>C. Limited accuracy if the mean is close to the limiting value of C/2 (for which the distribution of the lower values is almost uniform). 2012-05-23: The function returns biased estimates for the case type=="l", especially for small sample sizes. The reason for this is not yet established. Use the function meanexp.quantile() instead.}
\usage{
meanexp.conditional(mean, C, x = NULL, beta = C, pres = C * 1e-04,
  max.iter = 100, type = "l")
}
\arguments{
\item{mean}{is the mean of the values of the exponentially distributed lower or greater than 'C'.}

\item{C}{it the value defining 'Y'.}

\item{x}{it a vector of the data from which 'mean' is calculated (overrides 'mean' if given).}

\item{beta}{is the initial parameter value, defaulted to 'C'. Should not be chosen larger than 'C'.}

\item{pres}{is the required presicion of the iteration.}

\item{max.iter}{is the maximum number of iterations.}

\item{type}{is "l" if 'mean' is the mean of X|X<C and "g" if 'mean' is the mean of X|X>C.}
}
\value{

}
\description{
Locating the parameter of an exponentially distributed 'X' based on the mean of Y=X|X<C or Y=X|X>C. Limited accuracy if the mean is close to the limiting value of C/2 (for which the distribution of the lower values is almost uniform). 2012-05-23: The function returns biased estimates for the case type=="l", especially for small sample sizes. The reason for this is not yet established. Use the function meanexp.quantile() instead.
}
\examples{
\dontrun{}

}
