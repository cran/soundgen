% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modulationSpectrum.R
\name{.modulationSpectrum}
\alias{.modulationSpectrum}
\title{Modulation spectrum per sound}
\usage{
.modulationSpectrum(
  audio,
  amRes = 5,
  maxDur = 5,
  logSpec = FALSE,
  windowLength = 15,
  step = NULL,
  overlap = 80,
  wn = "hanning",
  zp = 0,
  power = 1,
  roughRange = c(30, 150),
  amRange = c(10, 200),
  returnMS = TRUE,
  returnComplex = FALSE,
  plot = TRUE,
  savePlots = NULL,
  logWarp = NA,
  quantiles = c(0.5, 0.8, 0.9),
  kernelSize = 5,
  kernelSD = 0.5,
  colorTheme = c("bw", "seewave", "heat.colors", "...")[1],
  main = NULL,
  xlab = "Hz",
  ylab = "1/KHz",
  xlim = NULL,
  ylim = NULL,
  width = 900,
  height = 500,
  units = "px",
  res = NA,
  ...
)
}
\arguments{
\item{audio}{a list returned by \code{readAudio}}

\item{amRes}{target resolution of amplitude modulation, Hz. If \code{NULL},
the entire sound is analyzed at once, resulting in a single roughness value
(unless it is longer than \code{maxDur}, in which case it is analyzed in
chunks \code{maxDur} s long). If \code{amRes} is set, roughness is
calculated for windows \code{~1000/amRes} ms long (but at least 3 STFT
frames). \code{amRes} also affects the amount of smoothing when calculating
\code{amFreq} and \code{amDep}}

\item{maxDur}{sounds longer than \code{maxDur} s are split into fragments,
and the modulation spectra of all fragments are averaged}

\item{logSpec}{if TRUE, the spectrogram is log-transformed prior to taking 2D
FFT}

\item{windowLength}{length of FFT window, ms}

\item{step}{you can override \code{overlap} by specifying FFT step, ms (NB:
because digital audio is sampled at discrete time intervals of
1/samplingRate, the actual step and thus the time stamps of STFT frames
may be slightly different, eg 24.98866 instead of 25.0 ms)}

\item{overlap}{overlap between successive FFT frames, \%}

\item{wn}{window type accepted by \code{\link[seewave]{ftwindow}}, currently
gaussian, hanning, hamming, bartlett, rectangular, blackman, flattop}

\item{zp}{window length after zero padding, points}

\item{power}{raise modulation spectrum to this power (eg power = 2 for ^2, or
"power spectrum")}

\item{roughRange}{the range of temporal modulation frequencies that
constitute the "roughness" zone, Hz}

\item{amRange}{the range of temporal modulation frequencies that we are
interested in as "amplitude modulation" (AM), Hz}

\item{returnMS}{if FALSE, only roughness is returned (much faster)}

\item{returnComplex}{if TRUE, returns a complex modulation spectrum (without
normalization and warping)}

\item{plot}{if TRUE, plots the modulation spectrum of each sound}

\item{savePlots}{if a valid path is specified, a plot is saved in this folder
(defaults to NA)}

\item{logWarp}{the base of log for warping the modulation spectrum (ie log2
if logWarp = 2); set to NULL or NA if you don't want to log-warp}

\item{quantiles}{labeled contour values, \% (e.g., "50" marks regions that
contain 50\% of the sum total of the entire modulation spectrum)}

\item{kernelSize}{the size of Gaussian kernel used for smoothing (1 = no
smoothing)}

\item{kernelSD}{the SD of Gaussian kernel used for smoothing, relative to its
size}

\item{colorTheme}{black and white ('bw'), as in seewave package ('seewave'),
or any palette from \code{\link[grDevices]{palette}} such as 'heat.colors',
'cm.colors', etc}

\item{main}{graphical parameters}

\item{xlab}{graphical parameters}

\item{ylab}{graphical parameters}

\item{xlim}{graphical parameters}

\item{ylim}{graphical parameters}

\item{width}{parameters passed to
\code{\link[grDevices]{png}} if the plot is saved}

\item{height}{parameters passed to
\code{\link[grDevices]{png}} if the plot is saved}

\item{units}{parameters passed to
\code{\link[grDevices]{png}} if the plot is saved}

\item{res}{parameters passed to
\code{\link[grDevices]{png}} if the plot is saved}

\item{...}{other graphical parameters passed on to \code{filled.contour.mod}
and \code{\link[graphics]{contour}} (see \code{\link{spectrogram}})}
}
\description{
Internal soundgen function.
}
\keyword{internal}
