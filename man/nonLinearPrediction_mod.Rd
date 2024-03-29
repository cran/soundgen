% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nonlinPred.R
\name{nonLinearPrediction_mod}
\alias{nonLinearPrediction_mod}
\title{Nonlinear prediction modified}
\usage{
nonLinearPrediction_mod(
  time.series,
  embedding.dim,
  time.lag,
  nPoints,
  radius,
  radius.increment
)
}
\arguments{
\item{time.series, embedding.dim, time.lag, radius, radius.increment}{see \code{\link[nonlinearTseries]{nonLinearPrediction}}}

\item{nPoints}{number of points to return}
}
\description{
A slightly modified version of
\code{\link[nonlinearTseries]{nonLinearPrediction}} that can return multiple
new points in one go, without having to call the function repeatedly.
}
\keyword{internal}
