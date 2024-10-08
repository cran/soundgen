% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modulationSpectrum.R
\name{modulationSpectrumFragment}
\alias{modulationSpectrumFragment}
\title{Modulation spectrum per fragment}
\usage{
modulationSpectrumFragment(
  sound,
  samplingRate,
  specSource = "STFT",
  audSpec_pars = NULL,
  msType = c("2D", "1D")[1],
  windowLength,
  windowLength_points,
  step,
  step_points,
  lowestFreq,
  wn = "hanning",
  zp = 0,
  specMethod = c("spec", "meanspec")[2],
  logSpec = FALSE,
  logMPS = FALSE,
  power = 1,
  normalize = TRUE
)
}
\arguments{
\item{sound}{numeric vector}

\item{samplingRate}{sampling rate of \code{x} (only needed if \code{x} is a
numeric vector)}

\item{specSource}{'STFT' = Short-Time Fourier Transform; 'audSpec' = a bank
of bandpass filters (see \code{\link{audSpectrogram}})}

\item{audSpec_pars}{parameters for extracting an auditory spectrogram if
\code{specType = 'audSpec'}. If \code{specType = 'STFT'}, these settings
have no effect}

\item{msType}{'2D' = two-dimensional Fourier transform of a spectrogram; '1D'
= separately calculated spectrum of each frequency band}

\item{windowLength, step, wn, zp}{parameters for extracting a spectrogram if
\code{specType = 'STFT'}. Window length and step are specified in ms (see
\code{\link{spectrogram}}). If \code{specType = 'audSpec'}, these settings
have no effect}

\item{specMethod}{the function to call when calculating the spectrum of each
frequency band (only used when \code{msType = '1D'}); 'meanspec' is faster
and less noisy, whereas 'spec' produces higher resolution}

\item{logSpec}{if TRUE, the spectrogram is log-transformed prior to taking 2D
FFT}

\item{logMPS}{if TRUE, the modulation spectrum is log-transformed prior to
calculating roughness}

\item{power}{raise modulation spectrum to this power (eg power = 2 for ^2, or
"power spectrum")}

\item{normalize}{if TRUE, the modulation spectrum of each analyzed fragment
\code{maxDur} in duration is separately normalized to have max = 1}
}
\description{
Internal soundgen function.
}
\examples{
s = soundgen(amFreq = 25, amDep = 100)
ms = soundgen:::modulationSpectrumFragment(s, 16000,
  windowLength = 50, windowLength_points = .05 * 16000,
  step = 5, step_points = .005 * 16000)
image(as.numeric(colnames(ms$ms_half)), as.numeric(rownames(ms$ms_half)),
      t(log(ms$ms_half)))
}
\keyword{internal}
