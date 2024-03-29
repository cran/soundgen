% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soundgen_utilities.R
\name{upsampleGC}
\alias{upsampleGC}
\title{Upsample glottal cycles}
\usage{
upsampleGC(pitch_per_gc, samplingRate = 16000)
}
\arguments{
\item{pitch_per_gc}{a vector of fundamental frequencies per glottal cycle}

\item{samplingRate}{target sampling rate after upsampling, in Hz}
}
\value{
Returns a list of two vectors: pitch_upsampled (the upsampled version
  of the input) and gc_upsampled (new indices of glottal cycles on an
  upsampled scale)
}
\description{
Internal soundgen function.
}
\details{
Upsamples a pitch contour to samplingRate through linear interpolation
between successive glottal cycles.
}
\examples{
soundgen:::upsampleGC(pitch_per_gc = c(100, 150, 130), samplingRate = 16000)
}
\keyword{internal}
