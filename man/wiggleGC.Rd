% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soundgen_utilities.R
\name{wiggleGC}
\alias{wiggleGC}
\title{Wiggle glottal cycles}
\usage{
wiggleGC(dep, len, nGC, pitch_per_gc, rw, effect_on)
}
\arguments{
\item{dep}{a vector of any length specifying the strengh of applied effect as
2 ^ rnorm(..., 0, dep))}

\item{len}{a vector of any length specifying the period of applied effect in
ms}

\item{nGC}{number of glottal cycles}

\item{pitch_per_gc}{vector of length nGC specifying pitch per glottal cycle,
Hz}

\item{rw}{vector of length nGC specifying a random walk around 1 to multiply
the effect with}

\item{effect_on}{vector of length nGC specifying glottal cycles to which the
effect should be applied (0 = off, 1 = on)}
}
\description{
Internal soundgen function
}
\details{
Helper function for preparing a vector of multiplication factors for adding
jitter and shimmer per glottal cycle. Generates random anchors for each
jitter/shimmer period and draws a smooth contour between them by spline
interpolation.
}
\examples{
plot(soundgen:::wiggleGC(dep = 5 / 12, len = c(3, 50), nGC = 100,
              pitch_per_gc = rnorm(100, 150, 10),
              rw = rep(1, 100), effect_on = rep(1, 100)),
     type = 'b')
plot(soundgen:::wiggleGC(dep = 5 / 12, len = c(3, 50), nGC = 100,
              pitch_per_gc = rnorm(100, 150, 10),
              rw = rep(1, 100),
              effect_on = c(rep(1, 30), rep(0, 20), rep(1, 50))),
     type = 'b')
plot(soundgen:::wiggleGC(dep = c(1/12, 10/12), len = c(3, 50), nGC = 100,
              pitch_per_gc = rnorm(100, 150, 10),
              rw = rep(1, 100), effect_on = rep(1, 100)),
     type = 'b')
}
\keyword{internal}
