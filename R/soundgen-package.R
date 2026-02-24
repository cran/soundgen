# TODO: (Windows) formant_app etc - opaque selection on some systems (Win10?); make color theme in apps compatible with dark mode; spectrogram etc - relative paths not working on Windows only, abs path only if saveAudio is the same as input folder (?); checkInput complains when running any folder function on some Win10 machines (?); check or remove identifyAndPlay - not working in RStudio (but OK in console)
# NB: turn off debug mode in pitch_app & formant_app & annotation_app before submitting to CRAN!

# TODO maybe: entropy etc - use power spectrum?; soundgen: increase sampling rate if creating one gc at a time for smooth f0 changes, then downsample again; segment - return the spectrum and spectral peaks of the signal contour (as an alternative to "interburst"); check math behind formants (eg use LPC to create all formants in one go); segment_ann() - segment audio based on annotations; per-channel normalization (https://librosa.org/doc/main/generated/librosa.pcen.html); superlets; repetitive CPU-intense tasks with Rcpp (see https://blog.ephorie.de/why-r-for-data-science-and-not-python); try to use ifft to create nice glottal cycles from the desired spectrum (or some sensible model of glottal pulses); option to plot legend in spectrogram, plotMS, etc; add AM either before or after adding formants; formant_app - drag annotation borders to change duration; check main in all plots - should be like analyze & spectrogram ('' if audio$filename_base = 'sound'); include the output of segment in analyze (just for convenience); compareSounds - input folder creates a distance matrix based on features and/or melSpec; inverse distance weighting interpolation instead of interpolMatrix; sharpness in getLoudness (see Fastl p. 242); refine cepstrum to look for freq windows with a strong cepstral peak, like opera singing over the orchestra; morph multiple sounds not just 2; maybe vectorize lipRad/noseRad; soundgen - pitch2 for dual source (desynchronized vocal folds); morph() - tempEffects; streamline saving all plots a la ggsave: filename, path, different supported devices instead of only png()

# Debugging tip: run smth like options('browser' = '/usr/bin/chromium-browser') or options('browser' = '/usr/bin/google-chrome') to check a Shiny app in a non-default browser

#' @importFrom foreach %dopar%
#' @import stats graphics utils grDevices
# need to import smth from bslib, otherwise a note
#' @importFrom bslib page_fluid
#' @encoding UTF-8
NULL

#' The soundgen package
#'
#' Performs parametric synthesis of sounds with harmonic and noise components
#' such as animal vocalizations or human voice. Also offers tools for audio
#' manipulation and acoustic analysis, including pitch tracking, spectral
#' analysis, audio segmentation, pitch and formant shifting, etc. Includes four
#' interactive web apps for synthesizing and annotating audio, manually
#' correcting pitch contours, and measuring formant frequencies.
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
#'   \item \link{pitch_app} Manually corrected pitch tracking
#'   \item \link{formant_app} Manually corrected pitch tracking
#'   \item \link{annotation_app} Audio annotation
#' }
#' Sound synthesis \itemize{
#'   \item \link{soundgen} The main function for sound and voice synthesis (see Anikin 2019)
#'   \item \link{soundgen_app} Interaction version of the soundgen() function
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
#'   \item Anikin. A. (2026) Measuring surprisal in sound sequences.
#'   https://cogsci.se/publications/pdf/anikin_2026_surprisal_preprint.pdf
#' }
#'
#' @docType package
#' @keywords internal
#' @aliases soundgen-package
"_PACKAGE"



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
