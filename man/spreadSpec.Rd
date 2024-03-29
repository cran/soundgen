% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loudness_utilities.R
\name{spreadSpec}
\alias{spreadSpec}
\title{Spread spectrum}
\usage{
spreadSpec(barkSpec)
}
\arguments{
\item{barkSpec}{a numeric vector of length equal to the number of critical
bands (eg 1 to 24 barks), giving the power in each band on a linear scale
(not dB)}
}
\description{
Internal soundgen function
}
\details{
Spreads spectrum to account for frequency masking across critical bands. See
Wonho (1999) "Enhanced modified bark spectral distortion (EMBSD)"
}
\examples{
barkSpec = c(rep(0, 10), 20000, 2e5, 2e4, 1e4, 2000, rep(0, 8))
plot(soundgen:::spreadSpec(barkSpec), type = 'b', col = 'red')
points(barkSpec, type = 'b', col = 'blue')
}
\keyword{internal}
