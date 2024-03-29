% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detectNLP.R
\name{.detectNLP}
\alias{.detectNLP}
\title{Detect NLP per sound}
\usage{
.detectNLP(
  audio,
  predictors = c("nPeaks", "d2", "subDep", "amEnvDep", "entropy", "HNR", "CPP",
    "roughness"),
  thresProb = 0.4,
  unvoicedToNone = FALSE,
  train = soundgen::detectNLP_training_nonv,
  scale = NULL,
  from = NULL,
  to = NULL,
  pitchManual_list = NULL,
  pars_analyze = list(windowLength = 50, roughness = list(windowLength = 15, step = 3),
    plot = FALSE),
  pars_phasegram = list(nonlinStats = "d2"),
  pars_naiveBayes = list(prior = "static", wlClumper = 3),
  jumpThres = 14,
  jumpWindow = 100,
  plot = FALSE,
  savePlots = NULL,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  type = "b",
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

\item{scale}{maximum possible amplitude of input used for normalization of
input vector (only needed if \code{x} is a numeric vector)}

\item{from, to}{if NULL (default), analyzes the whole sound, otherwise
from...to (s)}

\item{plot}{if TRUE, plot a contour of RMS amplitude}

\item{savePlots}{full path to the folder in which to save the plots (NULL =
don't save, '' = same folder as audio)}

\item{xlab, ylab, main}{general graphical parameters}

\item{width, height, units, res}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{...}{other graphical parameters}
}
\description{
Internal soundgen function called by \code{\link{detectNLP}}.
}
\keyword{internal}
