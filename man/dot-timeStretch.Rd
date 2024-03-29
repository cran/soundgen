% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeStretch.R
\name{.timeStretch}
\alias{.timeStretch}
\title{Time stretch per sound}
\usage{
.timeStretch(audio, stretch, precision, normalize = TRUE, play = FALSE)
}
\arguments{
\item{audio}{a list returned by \code{readAudio}}

\item{stretch}{1 = no change, >1 = longer, <1 = shorter. Single value, vector,
or anchor format (see \code{\link{soundgen}})}

\item{precision}{the number of points used for estimating the duration of
output (more = better, but slower)}

\item{play}{if TRUE, plays the synthesized sound using the default player on
your system. If character, passed to \code{\link[tuneR]{play}} as the name
of player to use, eg "aplay", "play", "vlc", etc. In case of errors, try
setting another default player for \code{\link[tuneR]{play}}}
}
\description{
Internal soundgen function called by \code{\link{timeStretch}}
}
\keyword{internal}
