% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audSpec.R
\name{.audSpectrogram}
\alias{.audSpectrogram}
\title{Auditory spectrogram per sound}
\usage{
.audSpectrogram(
  audio,
  step = 1,
  dynamicRange = 80,
  filterType = c("butterworth", "chebyshev", "gammatone")[1],
  nFilters = 128,
  nFilters_oct = NULL,
  filterOrder = if (filterType == "gammatone") 4 else 3,
  bandwidth = NULL,
  bandwidthMult = 1,
  minFreq = 20,
  maxFreq = audio$samplingRate/2,
  minBandwidth = 10,
  output = c("audSpec", "audSpec_processed", "filterbank", "filterbank_env", "roughness"),
  plot = TRUE,
  plotFilters = FALSE,
  osc = c("none", "linear", "dB")[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = "bark",
  contrast = 0.2,
  brightness = 0,
  maxPoints = c(1e+05, 5e+05),
  padWithSilence = TRUE,
  colorTheme = c("bw", "seewave", "heat.colors", "...")[1],
  col = NULL,
  extraContour = NULL,
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = NULL,
  grid = NULL,
  width = 900,
  height = 500,
  units = "px",
  res = NA,
  ...
)
}
\arguments{
\item{audio}{a list returned by \code{readAudio}}

\item{step}{step, ms (determines time resolution of the plot, but not of the
returned envelopes per channel). step = NULL means no downsampling at all
(ncol of output = length of input audio)}

\item{dynamicRange}{dynamic range, dB. All values more than one dynamicRange
under maximum are treated as zero}

\item{filterType}{"butterworth" = Butterworth filter
\code{\link[signal]{butter}}, "chebyshev" = Chebyshev filter
\code{\link[signal]{butter}}, "gammatone" =
\code{\link[seewave]{gammatone}}}

\item{nFilters}{the number of filters between \code{minFreq} and
\code{maxFreq} (determines frequency resolution, while \code{yScale}
determines the location of center frequencies)}

\item{nFilters_oct}{an alternative way to specify frequency resolution: the
number of filters per octave}

\item{filterOrder}{filter order (defaults to 4 for gammatones, 3 otherwise)}

\item{bandwidth}{filter bandwidth, octaves. If NULL, defaults to ERB
bandwidths as in \code{\link[seewave]{gammatone}}}

\item{bandwidthMult}{a scaling factor for all bandwidths (1 = no effect)}

\item{minFreq, maxFreq}{the range of frequencies to analyze}

\item{minBandwidth}{minimum filter bandwidth, Hz (otherwise filters may
become too narrow when nFilters is high; has no effect if filterType =
'gammatone')}

\item{output}{a list of measures to return. Defaults to everything, but this
takes a lot of RAM, so shorten to what's needed if analyzing many files at
once}

\item{plot}{should a spectrogram be plotted? TRUE / FALSE}

\item{plotFilters}{if TRUE, plots the filters as central frequencies ±
bandwidth/2}

\item{osc}{"none" = no oscillogram; "linear" = on the original scale; "dB" =
in decibels}

\item{heights}{a vector of length two specifying the relative height of the
spectrogram and the oscillogram (including time axes labels)}

\item{ylim}{frequency range to plot, kHz (defaults to 0 to Nyquist
frequency). NB: still in kHz, even if yScale = bark, mel, or ERB}

\item{yScale}{determines the location of center frequencies of the filters}

\item{contrast}{spectrum is exponentiated by contrast (any real number,
recommended -1 to +1). Contrast >0 increases sharpness, <0 decreases
sharpness}

\item{brightness}{how much to "lighten" the image (>0 = lighter, <0 = darker)}

\item{maxPoints}{the maximum number of "pixels" in the oscillogram (if any)
and spectrogram; good for quickly plotting long audio files; defaults to
c(1e5, 5e5)}

\item{padWithSilence}{if TRUE, pads the sound with just enough silence to
resolve the edges properly (only the original region is plotted, so the
apparent duration doesn't change)}

\item{colorTheme}{black and white ('bw'), as in seewave package ('seewave'),
matlab-type palette ('matlab'), or any palette from
\code{\link[grDevices]{palette}} such as 'heat.colors', 'cm.colors', etc}

\item{col}{actual colors, eg rev(rainbow(100)) - see ?hcl.colors for colors
in base R (overrides colorTheme)}

\item{extraContour}{a vector of arbitrary length scaled in Hz (regardless of
yScale!) that will be plotted over the spectrogram (eg pitch contour); can
also be a list with extra graphical parameters such as lwd, col, etc. (see
examples)}

\item{xlab, ylab, main, mar, xaxp}{graphical parameters for plotting}

\item{grid}{if numeric, adds n = \code{grid} dotted lines per kHz}

\item{width, height, units, res}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{...}{other graphical parameters}
}
\description{
Internal soundgen function.
}
\keyword{internal}
