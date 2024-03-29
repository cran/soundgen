% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pitch_postprocessing.R
\name{costJumps}
\alias{costJumps}
\title{Cost of jumps}
\usage{
costJumps(cand1, cand2)
}
\arguments{
\item{cand1, cand2}{two candidate pitch values}
}
\description{
Internal soundgen function.
}
\details{
Internal helper function for calculating the cost of transitions between
pitch candidates. Needed for postprocessing of pitch contour - finding the
optimal pitch contour.
}
\examples{
a = seq(-3, 3, by = .01)
b = 1 / (1 + 10 * exp(3 - 7 * abs(a)))
plot(a, b, type = 'l')
}
\keyword{internal}
