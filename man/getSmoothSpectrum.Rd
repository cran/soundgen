% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectrogram.R
\name{getSmoothSpectrum}
\alias{getSmoothSpectrum}
\title{Get smooth spectrum}
\usage{
getSmoothSpectrum(
  sound,
  samplingRate = NULL,
  spectrum = NULL,
  len,
  loessSpan,
  windowLength = 100,
  overlap = 0,
  plot = FALSE,
  xlab = "Frequency, kHz",
  ylab = "dB",
  type = "l",
  ...
)
}
\arguments{
\item{sound}{the audio (numeric, any scale)}

\item{samplingRate}{sampling rate of \code{x} (only needed if \code{x} is a
numeric vector)}

\item{spectrum}{pre-extracted spectrum in dB with columns "freq" and "ampl"}

\item{len}{the desired resolution of the output}

\item{loessSpan}{passed to loess to control the amount of smoothing (.01 =
minimal smoothing, 1 = strong smoothing)}

\item{windowLength}{length of FFT window, ms}

\item{overlap}{overlap between successive FFT frames, \%}

\item{plot}{should a spectrogram be plotted? TRUE / FALSE}

\item{...}{other graphical parameters}
}
\description{
Internal soundgen function.
}
\examples{
s = soundgen(sylLen = 100, pitch = 500, addSilence = FALSE)
soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = .01, plot = TRUE)
soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = .1, plot = TRUE)
soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = .5, plot = TRUE)
soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = 1, plot = TRUE)

sp = seewave::meanspec(s, f = 16000, dB = 'max0')
colnames(sp) = c('freq', 'ampl')
soundgen:::getSmoothSpectrum(spectrum = sp, len = 500, loessSpan = .1, plot = TRUE)
}
\keyword{internal}
