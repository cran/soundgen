% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiftFormants.R
\name{shiftFormants}
\alias{shiftFormants}
\title{Shift formants}
\usage{
shiftFormants(
  x,
  multFormants,
  samplingRate = NULL,
  freqWindow = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = "gaussian",
  interpol = c("approx", "spline")[1],
  normalize = c("max", "orig", "none")[2],
  play = FALSE,
  saveAudio = NULL,
  reportEvery = NULL,
  cores = 1,
  ...
)
}
\arguments{
\item{x}{path to a folder, one or more wav or mp3 files c('file1.wav',
'file2.mp3'), Wave object, numeric vector, or a list of Wave objects or
numeric vectors}

\item{multFormants}{1 = no change, >1 = raise formants (eg 1.1 = 10\% up, 2 =
one octave up), <1 = lower formants. Anchor format accepted (see
\code{\link{soundgen}})}

\item{samplingRate}{sampling rate of \code{x} (only needed if \code{x} is a
numeric vector)}

\item{freqWindow}{the width of spectral smoothing window, Hz. Defaults to
detected f0}

\item{dynamicRange}{dynamic range, dB. All values more than one dynamicRange
under maximum are treated as zero}

\item{windowLength}{length of FFT window, ms}

\item{step}{you can override \code{overlap} by specifying FFT step, ms (NB:
because digital audio is sampled at discrete time intervals of
1/samplingRate, the actual step and thus the time stamps of STFT frames
may be slightly different, eg 24.98866 instead of 25.0 ms)}

\item{overlap}{overlap between successive FFT frames, \%}

\item{wn}{window type accepted by \code{\link[seewave]{ftwindow}}, currently
gaussian, hanning, hamming, bartlett, blackman, flattop, rectangle}

\item{interpol}{the method for interpolating scaled spectra}

\item{normalize}{"orig" = same as input (default), "max" = maximum possible
peak amplitude, "none" = no normalization}

\item{play}{if TRUE, plays the synthesized sound using the default player on
your system. If character, passed to \code{\link[tuneR]{play}} as the name
of player to use, eg "aplay", "play", "vlc", etc. In case of errors, try
setting another default player for \code{\link[tuneR]{play}}}

\item{saveAudio}{full path to the folder in which to save audio files (one
per detected syllable)}

\item{reportEvery}{when processing multiple inputs, report estimated time
left every ... iterations (NULL = default, NA = don't report)}

\item{cores}{number of cores for parallel processing}

\item{...}{other graphical parameters}
}
\description{
Raises or lowers formants (resonance frequencies), changing the voice quality
or timbre of the sound without changing its pitch, statically or dynamically.
Note that this is only possible when the fundamental frequency f0 is lower
than the formant frequencies. For best results, \code{freqWindow} should be
no lower than f0 and no higher than formant bandwidths. Obviously, this is
impossible for many signals, so just try a few reasonable values, like ~200
Hz for speech. If \code{freqWindow} is not specified, soundgen sets it to the
average detected f0, which is slow.
}
\details{
Algorithm: phase vocoder. In the frequency domain, we separate the complex
spectrum of each STFT frame into two parts. The "receiver" is the flattened
or smoothed complex spectrum, where smoothing is achieved by obtaining a
smoothed magnitude envelope (the amount of smoothing is controlled by
\code{freqWindow}) and then dividing the complex spectrum by this envelope.
This basically removes the formants from the signal. The second component,
"donor", is a scaled and interpolated version of the same smoothed magnitude
envelope as above - these are the formants shifted up or down. Warping can be
easily implemented instead of simple scaling if nonlinear spectral
transformations are required. We then multiply the "receiver" and "donor"
spectrograms and reconstruct the audio with iSTFT.
}
\examples{
s = soundgen(sylLen = 200, ampl = c(0,-10),
             pitch = c(250, 350), rolloff = c(-9, -15),
             noise = -40,
             formants = 'aii', addSilence = 50)
# playme(s)
s1 = shiftFormants(s, samplingRate = 16000, multFormants = 1.25,
                   freqWindow = 200)
# playme(s1)

\dontrun{
data(sheep, package = 'seewave')  # import a recording from seewave
playme(sheep)
spectrogram(sheep)

# Lower formants by 4 semitones or ~20\% = 2 ^ (-4 / 12)
sheep1 = shiftFormants(sheep, multFormants = 2 ^ (-4 / 12), freqWindow = 150)
playme(sheep1, sheep@samp.rate)
spectrogram(sheep1, sheep@samp.rate)

orig = seewave::meanspec(sheep, wl = 128, plot = FALSE)
shifted = seewave::meanspec(sheep1, wl = 128, f = sheep@samp.rate, plot = FALSE)
plot(orig[, 1], log(orig[, 2]), type = 'l')
points(shifted[, 1], log(shifted[, 2]), type = 'l', col = 'blue')

# dynamic change: raise formants at the beginning, lower at the end
sheep2 = shiftFormants(sheep, multFormants = c(1.3, .7), freqWindow = 150)
playme(sheep2, sheep@samp.rate)
spectrogram(sheep2, sheep@samp.rate)
}
}
\seealso{
\code{\link{shiftPitch}} \code{\link{transplantFormants}}
}
