#' @import stats graphics utils grDevices
#' @encoding UTF-8
NULL

#' The soundgen package
#'
#'Parametric source-filter synthesis of harmonic-noise signals, such as animal
#'vocalizations and human voice, with control over pitch, formants, noise,
#'amplitude modulation, nonlinear phenomena, and morphing. General signal
#'processing tools for audio analysis and manipulation: pitch tracking, formant
#'and vocal tract length estimation, reassigned and auditory spectrograms,
#'modulation spectra and psychoacoustic roughness, self-similarity and
#'surprisal, audio segmentation, pitch and formant shifting, etc. Includes four
#'interactive web apps for audio synthesis, annotation, formant analysis, and
#'manually correcting pitch contours.
#'
#' Visualization \itemize{
#'   \item \link{spectrogram} Ordinary (STFT) spectrograms
#'   \item \link{audSpectrogram} Auditory spectrograms
#'   \item \link{modulationSpectrum} Modulation spectra and roughness (see Anikin 2025)
#'   \item \link{ssm} Self-similarity matrices, acoustic novelty (see Anikin 2026)
#'   \item \link{phasegram} Nonlinear analysis, phasegrams (see Anikin & Herbst 2025)
#' }
#' Acoustic analysis \itemize{
#'   \item \link{analyze} General-purpose acoustic analysis (extracts nearly
#'   everything: spectral descriptives, pitch, formants, loudness, roughness,
#'   ...)
#'   \item \link{segment} Audio segmentation, analysis of rhythm and temporal
#'   structure of sequences
#'   \item \link{getSurprisal} Shannon and autocorrelation-based surprisal,
#'   Bayesian surprise (see Anikin 2026)
#'   \item \link{estimateVTL}, \link{schwa} Formant analysis (see Anikin et al. 2024)
#' }
#' Interactive acoustic analysis \itemize{
#'   \item \link{pitch_app} Manually corrected formant measurement
#'   \item \link{formant_app} Manually corrected pitch tracking
#'   \item \link{annotation_app} Audio annotation
#' }
#' Sound synthesis \itemize{
#'   \item \link{soundgen} The main function for sound and voice synthesis (see Anikin 2019)
#'   \item \link{soundgen_app} Interactive version of the soundgen() function
#'   \item \link{morph} Morphing of two sounds based on their soundgen code
#' }
#' Sound modification \itemize{
#'   \item \link{normalizeFolder} Peak / RMS etc normalization of recordings
#'   \item \link{timeStretch}, \link{shiftPitch}, \link{prosody},
#'   \link{shiftFormants} Selective modification of fundamental frequency,
#'   duration, and/or resonance frequency
#'   \item \link{noiseRemoval}, \link{compressor}, \link{addAM}, \link{resample},
#'   etc: various audio effects
#' }
#'
#' Vignettes \itemize{
#'   \item https://cogsci.se/soundgen/spectrograms.html
#'   \item https://cogsci.se/soundgen/sound_generation.html
#'   \item https://cogsci.se/soundgen/acoustic_analysis.html
#'   \item https://cogsci.se/soundgen/matching/matching.html
#' }
#'
#' @references \itemize{
#'   \item Anikin, A. (2019). Soundgen: an open-source tool for synthesizing
#'   nonverbal vocalizations. Behavior Research Methods, 51(2), 778-792.
#'   \item Anikin, A., Barreda, S. & Reby, D. (2024) A practical guide to
#'   calculating vocal tract length and scale-invariant formant patterns.
#'   Behavior Research Methods 56, 5588–5604.
#'   \item Anikin, A. (2025) Acoustic estimation of voice roughness. Attention,
#'   Perception, & Psychophysics 87: 1771–1787.
#'   \item Anikin, A. & Herbst, C.T. (2025) How to analyze and manipulate
#'   nonlinear phenomena in voice recordings. Philosophical Transactions B 380:
#'   20240003.
#'   \item Anikin. A. (2026) Measuring surprisal in sound sequences. Behavior
#'   Research Methods.
#' }
#'
#' @docType package
#' @keywords internal
#' @aliases soundgen-package
"_PACKAGE"

# Create a single internal environment for all app states
# This avoids global namespace pollution and safely passes data to Shiny apps
.soundgen_env = new.env(parent = emptyenv())


# .onLoad = function(libname, pkgname) {
# }

.onAttach = function(libname, pkgname) {
  mes = paste0(
    "Soundgen ", packageVersion('soundgen'),
    "\nTips & demos on project's homepage: http://cogsci.se/soundgen.html",
    "\nPlease cite as: ",
    "Anikin, A. (2019). Soundgen: an open-source tool for synthesizing ",
    "nonverbal vocalizations. Behavior Research Methods, 51(2), 778-792."
  )
  packageStartupMessage(mes)
}


#' Canonical roxygen docs (inherited, not user-facing)
#' @param x path to a folder, one or more wav or mp3 files c('file1.wav',
#'   'file2.mp3'), Wave object, numeric vector, or a list of Wave objects or
#'   numeric vectors
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector)
#' @param scale maximum possible amplitude of input, used to normalize the input
#'   vector (only needed if \code{x} is a numeric vector)
#' @param from,to if NULL (default), analyzes the whole sound, otherwise
#'   from...to (s)
#'
#' @param windowLength length of the analysis window, ms
#' @param step step between successive windows, ms; if provided, overrides
#'   \code{overlap}; because digital audio is sampled at discrete time intervals
#'   of 1/samplingRate, the actual step and thus the time stamps of STFT frames
#'   may be slightly different - e.g., 24.98866 instead of 25.0 ms
#' @param overlap overlap between successive windows, \%
#' @param wn window type accepted by \code{\link{winFun}}: character string or
#'   function
#' @param zp window length after zero padding, samples. No padding is performed
#'   if \code{zp} < analysis window length in samples. If NULL, the signal is
#'   padded to a good number for speeding up the FFT
#' @param dynamicRange regions under \code{-dynamicRange} dB are treated as silent
#'
#' @param play if TRUE, plays the output audio using the default player on your
#'   system. If a character string, it is passed to \code{\link{playme}} as the
#'   name of the player to use (e.g. 'aplay', 'play', 'vlc'). In case of errors,
#'   try setting another default player for \code{\link{playme}}
#' @param saveAudio if TRUE, saves the processed audio in a subdirectory named
#'   after the function and created in the input directory (if input is a file
#'   or folder) or in the working directory
#' @param plot if TRUE, produces a plot of the results
#' @param savePlots if TRUE, creates a subdirectory in the input directory (if
#'   input is a file or folder) or in the working directory (if input is a
#'   vector etc), named after the function (eg "spectrogram/"). All plots and
#'   audio files (if any) are saved in this new directory. If there are multiple
#'   inputs, an html notebook is also created for easy viewing and listening
#' @param embed if TRUE and savePlots is set and there are multiple inputs, all
#'   saved images and audio (if any) are embedded in the exported html notebook
#'   for easy sharing; if FALSE, the html file links to separate images and audio
#'   files (but separate files are still saved). NB: for this to work, package
#'   "base64enc" must be installed
#' @param reportEvery when processing multiple inputs, report estimated time
#'   left every \code{reportEvery} iterations (NULL = default, NA = don't
#'   report); see \code{\link{reportTime}}
#' @param cores number of cores for parallel processing
#' @param width,height,units,res graphical parameters for saving plots passed to
#'   \code{png}
#' @param normalize "orig" = same as input (default), "max" = maximum possible
#'   peak amplitude for the given input scale, "none" = no normalization
#' @keywords internal
.roxygen_defaults = function(
    x, samplingRate, scale, from, to,
    windowLength, step, overlap, wn, zp, dynamicRange,
    play, saveAudio, plot, savePlots, embed, reportEvery,
    cores, width, height, units, res, normalize) invisible(NULL)
