% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/am.R
\name{getPeakFreq}
\alias{getPeakFreq}
\title{Get peak frequency}
\usage{
getPeakFreq(x, samplingRate, freqRange = NULL, parab = TRUE, plot = FALSE)
}
\description{
Internal soundgen function for finding frequency modulation in pitch
contours. Called by analyze().
}
\examples{
\dontrun{
s = soundgen(sylLen = 1000, pitch = 500,
  vibratoFreq = c(6, 12), vibratoDep = 2,
  temperature = .001, addSilence = 5)
an = analyze(s, 16000, step = 5, windowLength = 25,
  plot = TRUE, yScale = 'bark')
plot(an$detailed$fmFreq, type = 'l')
}
}
\keyword{internal}
