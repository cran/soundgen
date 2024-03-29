% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loudness_utilities.R
\name{scaleSPL}
\alias{scaleSPL}
\title{Scale SPL}
\usage{
scaleSPL(x, scale = NULL, SPL_measured = 70, Pref = 2e-05)
}
\arguments{
\item{x}{numeric vector ranging from -1 to +1}

\item{scale}{maximum possible amplitude of input used for normalization of
input vector (only needed if \code{x} is a numeric vector)}

\item{SPL_measured}{sound pressure level at which the sound is presented, dB}

\item{Pref}{reference pressure, Pa (currently has no effect on the estimate)}
}
\description{
Internal soundgen function
}
\details{
Converts a sound from SPL on any scale to a desired level of dB SPL.
See Timoney et al. (2004) "Implementing loudness models in MATLAB"
}
\examples{
sound = rnorm(100) * getSmoothContour(c(0, 1, 0), len = 100)
sound = sound / max(abs(sound))
# plot(sound, type = 'l')
sound_scaled = soundgen:::scaleSPL(sound, Pref = 2e-1)
plot(sound_scaled, type = 'l')

sound2 = sound / 3
range(soundgen:::scaleSPL(sound2, scale = NULL))
range(soundgen:::scaleSPL(sound2, scale = 1))
}
\keyword{internal}
