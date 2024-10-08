% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phasegram.R
\name{.phasegram}
\alias{.phasegram}
\title{Phasegram per sound}
\usage{
.phasegram(
  audio,
  windowLength = 10,
  step = windowLength/2,
  timeLag = NULL,
  theilerWindow = NULL,
  nonlinStats = c("ed", "d2", "ml", "sur"),
  pars_ed = list(max.embedding.dim = 15),
  pars_d2 = list(min.embedding.dim = 2, min.radius = 0.001, n.points.radius = 20),
  pars_ml = list(min.embedding.dim = 2, radius = 0.001),
  pars_sur = list(FUN = nonlinearTseries::timeAsymmetry, K = 1),
  bw = 0.01,
  bins = 5/bw,
  plot = TRUE,
  rasterize = FALSE,
  colorTheme = c("bw", "seewave", "heat.colors", "...")[1],
  col = NULL,
  xlab = "Time",
  ylab = "",
  main = NULL,
  width = 900,
  height = 500,
  units = "px",
  res = NA,
  ...
)
}
\arguments{
\item{audio}{a list returned by \code{readAudio}}

\item{windowLength}{the length of each frame analyzed separately (ms)}

\item{step}{time step between consecutive frames (ms)}

\item{timeLag}{time lag between the original and time-shifted version of each
frame that together represent the phase portrait (ms). Defaults to the
number of steps beyond which the mutual information function reaches its
minimum or, if that fails, the steps until mutual information experiences
the first exponential decay - see \code{\link[nonlinearTseries]{timeLag}}}

\item{theilerWindow}{time lag between two points that are considered locally
independent and can be treated as neighbors in the reconstructed phase
space. defaults to the first minimum or, if unavailable, the first zero of
the autocorrelation function (or, failing that, to \code{timeLag * 2})}

\item{nonlinStats}{nonlinear statistics to report: "ed" = the optimal number
of embedding dimensions, "d2" = correlation dimension D2, "ml" = maximum
Lyapunov exponent, "sur" = the results of surrogate data testing for
stochasticity. These are calculated using the functionality of the package
nonlinearTseries, which is seriously slow, so the default is just to get
the phasegram itself}

\item{pars_ed}{a list of control parameters passed to
\code{\link[nonlinearTseries]{estimateEmbeddingDim}}}

\item{pars_d2}{a list of control parameters passed to
\code{\link[nonlinearTseries]{corrDim}}}

\item{pars_ml}{a list of control parameters passed to
\code{\link[nonlinearTseries]{maxLyapunov}}}

\item{pars_sur}{a list of control parameters passed to
\code{\link[nonlinearTseries]{surrogateTest}}}

\item{bw}{standard deviation of the smoothing kernel, as in
\code{\link[stats]{density}}}

\item{bins}{the number of bins along the Y axis after rasterizing (has no
effect if \code{rasterize = FALSE})}

\item{plot}{should a spectrogram be plotted? TRUE / FALSE}

\item{rasterize}{if FALSE, only plots and returns Poincare sections on the
original scale (most graphical parameters will then have no effect); if
TRUE, rasterizes the phasegram matrix and plots it with more graphical
parameters}

\item{colorTheme}{black and white ('bw'), as in seewave package ('seewave'),
matlab-type palette ('matlab'), or any palette from
\code{\link[grDevices]{palette}} such as 'heat.colors', 'cm.colors', etc}

\item{col}{actual colors, eg rev(rainbow(100)) - see ?hcl.colors for colors
in base R (overrides colorTheme)}

\item{xlab, ylab, main}{graphical parameters passed to
soundgen:::filled.contour.mod (if \code{rasterize = TRUE}) or plot (if
\code{rasterize = FALSE})}

\item{width, height, units, res}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{...}{other graphical parameters passed to soundgen:::filled.contour.mod
(if \code{rasterize = TRUE})  or plot (if \code{rasterize = FALSE})}
}
\description{
Internal soundgen function called by \code{\link{phasegram}}.
}
\keyword{internal}
