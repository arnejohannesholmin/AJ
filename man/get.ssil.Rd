% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.ssil.R
\name{get.ssil}
\alias{get.ssil}
\title{Calculates the spherical surface integral of a beam pattern from a acoustic fish models at radius 1 meter, for the given reference frequency and reference length of the scoustic model.}
\usage{
get.ssil(case = NULL, f0 = 38000, L0 = 0.32 * 0.26, kL = NULL,
  pres = 1e-06, max.cells = 1e+06, method = c("linear", "closest"))
}
\arguments{
\item{case}{is the identifier of the case, given as a string holding the path of the directory of the TSD files required (empirical beam pattern file and ctd file).}

\item{f0}{is the reference frequency at which 'chi' should be calculated, defaulted to 38000 Hz.}

\item{L0}{is the reference length of the acoustic model, at which 'chi' should be calculated, defaulted to 32 cm fish of which 0.26 is swim bladder length (Gorska and Ona 2003).}

\item{pres}{is the desired presition of the integration (used in integrateonsphere()).}

\item{max.cells}{is the maximum number of cells in the grid (used in integrateonsphere()).}

\item{method}{is "closest" if the chi value of the closest grid point is to be selected, and "linear" if linear interpolation should be used to extract the chi value.}
}
\value{

}
\description{
Calculates the spherical surface integral of a beam pattern from a acoustic fish models at radius 1 meter, for the given reference frequency and reference length of the scoustic model.
}
\examples{
\dontrun{}

}
