% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{log01}
\alias{log01}
\title{log01}
\usage{
log01(v)
}
\arguments{
\item{v}{numeric vector}
}
\description{
Internal soundgen function
}
\details{
Normalizes, log-transforms, and re-normalizes an input vector, so it ranges
from 0 to 1
}
\examples{
v = exp(1:10)
soundgen:::log01(v)
}
\keyword{internal}
