% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{analyze}
\alias{analyze}
\title{Analyze sound}
\usage{
analyze(x, samplingRate = NULL, silence = 0.04, windowLength = 50,
  step = NULL, overlap = 50, wn = "gaussian", zp = 0, cutFreq = 6000,
  nFormants = 3, pitchMethods = c("autocor", "spec", "dom"),
  entropyThres = 0.6, pitchFloor = 75, pitchCeiling = 3500,
  priorMean = HzToSemitones(300), priorSD = 6, priorPlot = FALSE,
  nCands = 1, minVoicedCands = "autom", domThres = 0.1, domSmooth = 220,
  autocorThres = 0.7, autocorSmooth = NULL, cepThres = 0.3,
  cepSmooth = NULL, cepZp = 0, specThres = 0.3, specPeak = 0.35,
  specSinglePeakCert = 0.4, specHNRslope = 0.8, specSmooth = 150,
  specMerge = 1, shortestSyl = 20, shortestPause = 60, interpolWin = 3,
  interpolTol = 0.3, interpolCert = 0.3, pathfinding = c("none", "fast",
  "slow")[2], annealPars = list(maxit = 5000, temp = 1000),
  certWeight = 0.5, snakeStep = 0.05, snakePlot = FALSE, smooth = 1,
  smoothVars = c("pitch", "dom"), summary = FALSE, plot = TRUE,
  savePath = NA, plotSpec = TRUE, specPlot = NULL, pitchPlot = list(col
  = rgb(0, 0, 1, 0.75), lwd = 3), candPlot = list(levels = c("autocor",
  "spec", "dom", "cep"), col = c("green", "red", "orange", "violet"), pch =
  c(16, 2, 3, 7), cex = 2), ylim = NULL, xlab = "Time, ms", ylab = "kHz",
  main = NULL, width = 900, height = 500, units = "px", ...)
}
\arguments{
\item{x}{path to a .wav file or a vector of amplitudes with specified
samplingRate}

\item{samplingRate}{sampling rate of \code{x} (only needed if
\code{x} is a numeric vector, rather than a .wav file)}

\item{silence}{(0 to 1) frames with mean abs amplitude below silence
threshold are not analyzed at all. NB: this number is dynamically updated:
the actual silence threshold may be higher depending on the quietest frame,
but it will never be lower than this specified number.}

\item{windowLength}{length of FFT window, ms}

\item{step}{you can override \code{overlap} by specifying FFT step, ms}

\item{overlap}{overlap between successive FFT frames, \%}

\item{wn}{window type: gaussian, hanning, hamming, bartlett, rectangular,
blackman, flattop}

\item{zp}{window length after zero padding, points}

\item{cutFreq}{(>0 to Nyquist, Hz) repeat the calculation of spectral
descriptives after discarding all info above \code{cutFreq}.
Recommended if the original sampling rate varies across different analyzed
audio files}

\item{nFormants}{the number of formants to extract per FFT frame. Calls
\code{\link[phonTools]{findformants}} with default settings}

\item{pitchMethods}{methods of pitch estimation to consider for determining
pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band}

\item{entropyThres}{pitch tracking is not performed for frames with Weiner
entropy above \code{entropyThres}, but other spectral descriptives are
still calculated}

\item{pitchFloor, pitchCeiling}{absolute bounds for pitch candidates (Hz)}

\item{priorMean, priorSD}{specifies the mean and sd of gamma distribution
describing our prior knowledge about the most likely pitch values for this
file. Specified in semitones: \code{priorMean = HzToSemitones(300),
priorSD = 6} gives a prior with mean = 300 Hz and SD of 6 semitones (half
an octave)}

\item{priorPlot}{if TRUE, produces a separate plot of the prior}

\item{nCands}{maximum number of pitch candidates per method (except for
\code{dom}, which returns at most one candidate per frame), normally 1...4}

\item{minVoicedCands}{minimum number of pitch candidates that
have to be defined to consider a frame voiced (defaults to 2 if \code{dom}
is among other candidates and 1 otherwise)}

\item{domThres}{(0 to 1) to find the lowest dominant frequency band, we
do short-term FFT and take the lowest frequency with amplitude at least
domThres}

\item{domSmooth}{the width of smoothing interval (Hz) for finding
\code{dom}}

\item{autocorThres, cepThres, specThres}{(0 to 1) separate
voicing thresholds for detecting pitch candidates with three different
methods: autocorrelation, cepstrum, and BaNa algorithm (see Details). Note
that HNR is calculated even for unvoiced frames.}

\item{autocorSmooth}{the width of smoothing interval (in bins) for
finding peaks in the autocorrelation function. Defaults to 7 for sampling
rate 44100 and smaller odd numbers for lower values of sampling rate}

\item{cepSmooth}{the width of smoothing interval (in bins) for finding
peaks in the cepstrum. Defaults to 31 for sampling rate 44100 and smaller
odd numbers for lower values of sampling rate}

\item{cepZp}{zero-padding of the spectrum used for cepstral pitch detection
(final length of spectrum after zero-padding in points, e.g. 2 ^ 13)}

\item{specPeak, specHNRslope}{when looking for putative harmonics in
the spectrum, the threshold for peak detection is calculated as
\code{specPeak * (1 - HNR * specHNRslope)}}

\item{specSinglePeakCert}{(0 to 1) if F0 is calculated based on a single
harmonic ratio (as opposed to several ratios converging on the same
candidate), its certainty is taken to be \code{specSinglePeakCert}}

\item{specSmooth}{the width of window for detecting peaks in the spectrum, Hz}

\item{specMerge}{pitch candidates within \code{specMerge} semitones are
merged with boosted certainty}

\item{shortestSyl}{the smallest length of a voiced segment (ms) that
constitutes a voiced syllable (shorter segments will be replaced by NA, as
if unvoiced)}

\item{shortestPause}{the smallest gap between voiced syllables (ms) that
means they shouldn't be merged into one voiced syllable}

\item{interpolWin, interpolTol, interpolCert}{control the behavior of
interpolation algorithm when postprocessing pitch candidates. To turn off
interpolation, set \code{interpolWin} to NULL. See
\code{soundgen:::pathfinder} for details.}

\item{pathfinding}{method of finding the optimal path through pitch
candidates: 'none' = best candidate per frame, 'fast' = simple heuristic,
'slow' = annealing. See \code{soundgen:::pathfinder}}

\item{annealPars}{a list of control parameters for postprocessing of
pitch contour with SANN algorithm of \code{\link[stats]{optim}}. This is
only relevant if \code{pathfinding = 'slow'}}

\item{certWeight}{(0 to 1) in pitch postprocessing, specifies how much we
prioritize the certainty of pitch candidates vs. pitch jumps / the internal
tension of the resulting pitch curve}

\item{snakeStep}{optimized path through pitch candidates is further
processed to minimize the elastic force acting on pitch contour. To
disable, set \code{snakeStep} to NULL}

\item{snakePlot}{if TRUE, plots the snake}

\item{smooth, smoothVars}{if \code{smooth} is a positive number, outliers of
the variables in \code{smoothVars} are adjusted with median smoothing.
\code{smooth} of 1 corresponds to a window of ~100 ms and tolerated
deviation of ~4 semitones. To disable, set \code{smooth} to NULL}

\item{summary}{if TRUE, returns only a summary of the measured acoustic
variables (mean, median and SD). If FALSE, returns a list containing
frame-by-frame values}

\item{plot}{if TRUE, produces a spectrogram with pitch contour overlaid}

\item{savePath}{if a valid path is specified, a plot is saved in this
folder (defaults to NA)}

\item{plotSpec}{if \code{FALSE}, the spectrogram will not be plotted}

\item{specPlot}{deprecated since soundgen 1.1.2. Pass its arguments directly
to the main function or set \code{plotSpec = FALSE} to remove the
spectrogram}

\item{pitchPlot}{a list of graphical parameters for displaying the final
pitch contour. Set to \code{NULL} or \code{NA} to suppress}

\item{candPlot}{a list of graphical parameters for displaying
individual pitch candidates. Set to \code{NULL} or \code{NA} to suppress}

\item{ylim}{frequency range to plot, kHz (defaults to 0 to Nyquist frequency)}

\item{xlab, ylab, main}{plotting parameters}

\item{width, height, units}{parameters passed to \code{\link[grDevices]{jpeg}}
if the plot is saved}

\item{...}{other graphical parameters passed to \code{\link{spectrogram}}}
}
\value{
If \code{summary = TRUE}, returns a dataframe with one row and three
  column per acoustic variable (mean / median / SD). If \code{summary =
  FALSE}, returns a dataframe with one row per FFT frame and one column per
  acoustic variable. The best guess at the pitch contour considering all
  available information is stored in the variable called "pitch". In
  addition, the output contains a number of other acoustic descriptors and
  pitch estimates by separate algorithms included in \code{pitchMethods}. See
  the vignette on acoustic analysis for a full explanation of returned
  measures.
}
\description{
Acoustic analysis of a single sound file: pitch tracking and basic spectral
characteristics. The default values of arguments are optimized for human
non-linguistic vocalizations. See the vignette on acoustic analysis for
details.
}
\examples{
sound = soundgen(sylLen = 300, pitchAnchors = list(
  time = c(0, .5, 1), value = c(900, 400, 2300)),
  noiseAnchors = list(time = c(0, 300), value = c(-40, 00)),
  temperature = 0, addSilence = 0, overlap = 0)
# playme(sound, 16000)
a = analyze(sound, samplingRate = 16000, plot = TRUE)

\dontrun{
sound1 = soundgen(sylLen = 900, pitchAnchors = list(
  time = c(0, .3, .9, 1), value = c(300, 900, 400, 2300)),
  noiseAnchors = list(time = c(0, 300), value = c(-40, 00)),
  temperature = 0, addSilence = 0)
# improve the quality of postprocessing:
a1 = analyze(sound1, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
median(a1$pitch, na.rm = TRUE)  # 586 Hz
# (can vary, since postprocessing is stochastic)
# compare to the true value:
median(getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
  value = c(300, 900, 400, 2300)), len = 1000))  # 611 Hz

# the same pitch contour, but harder b/c of subharmonics and jitter
sound2 = soundgen(sylLen = 900, pitchAnchors = list(
  time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
  noiseAnchors = list(time = c(0, 900), value = c(-40, 20)),
  subDep = 100, jitterDep = 0.5, nonlinBalance = 100, temperature = 0)
# playme(sound2, 16000)
a2 = analyze(sound2, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
# many candidates are off, but the overall contour should be mostly accurate

# Fancy plotting options:
a = analyze(sound2, samplingRate = 16000, plot = TRUE,
  xlab = 'Time, ms', colorTheme = 'seewave',
  contrast = .5, ylim = c(0, 4),
  candPlot = list(cex = 3, col = c('gray70', 'yellow', 'purple', 'maroon')),
  pitchPlot = list(col = 'black', lty = 3, lwd = 3))

# Plot pitch candidates w/o a spectrogram
a = analyze(sound2, samplingRate = 16000, plot = TRUE, plotSpec = FALSE)
}
}