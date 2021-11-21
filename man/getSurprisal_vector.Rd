% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surprisal.R
\name{getSurprisal_vector}
\alias{getSurprisal_vector}
\title{Get surprisal per vector}
\usage{
getSurprisal_vector(x)
}
\arguments{
\item{x}{numeric vector representing the time sequence of interest, eg
amplitudes in a frequency bin over multiple STFT frames}
}
\description{
Internal soundgen function called by \code{\link{getSurprisal}}. Estimates
the unexpectedness or "surprisal" of the last element of input vector.
}
\examples{
x = c(rep(1, 3), rep(0, 4), rep(1, 3), rep(0, 4), rep(1, 3), 0, 0)
soundgen:::getSurprisal_vector(x)
soundgen:::getSurprisal_vector(c(x, 1))
soundgen:::getSurprisal_vector(c(x, 13))
}
\keyword{internal}