#' Acoustic analysis
#'
#' Acoustic analysis of one or more sounds: pitch tracking, basic spectral
#' characteristics, formants, estimated loudness (see
#' \code{\link{getLoudness}}), roughness (see \code{\link{modulationSpectrum}}),
#' novelty (see \code{\link{ssm}}), etc. The default values of arguments are
#' optimized for human non-linguistic vocalizations. For high-precision work,
#' first extract and manually correct pitch contours with
#' \code{\link{pitch_app}}, PRAAT, or whatever, and then run
#' \code{analyze(pitchManual = ...)} with these manual contours. For more
#' information, see \url{https://cogsci.se/soundgen/acoustic_analysis.html}
#'
#' Each pitch tracker is controlled by its own list of settings, as follows:
#' \describe{
#'   \item{\code{pitchDom} (lowest dominant frequency band)}{
#'     \itemize{
#'       \item \code{domThres} (0 to 1) to find the lowest dominant frequency band, we
#'         do short-term FFT and take the lowest frequency with amplitude at least
#'         domThres
#'       \item \code{domSmooth} the width of smoothing interval (Hz) for
#'         finding \code{dom}
#'     }
#'   }
#'   \item{\code{pitchAutocor} (autocorrelation)}{
#'     \itemize{
#'       \item \code{autocorThres} voicing threshold (unitless, ~0 to 1)
#'       \item \code{autocorSmooth} the width of smoothing interval (in bins) for
#'         finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'         rate 44100 and smaller odd numbers for lower values of sampling rate
#'       \item \code{autocorUpsample} upsamples acf to this resolution (Hz) to improve
#'         accuracy in high frequencies
#'       \item \code{autocorBestPeak} amplitude of the lowest best candidate relative
#'         to the absolute max of the acf
#'       \item \code{interpol} method of interpolating the ACF: "sinc" for
#'       maximum precision, "none" for speed
#'     }
#'   }
#'   \item{\code{pitchCep} (cepstrum)}{
#'     \itemize{
#'       \item \code{cepThres} voicing threshold (unitless, ~0 to 1)
#'       \item \code{cepZp} zero-padding of the spectrum used for cepstral pitch
#'         detection (final length of spectrum after zero-padding in points, e.g. 2 ^ 13)
#'     }
#'   }
#'   \item{\code{pitchSpec} (ratio of harmonics - BaNa algorithm)}{
#'     \itemize{
#'       \item \code{specThres} voicing threshold (unitless, ~0 to 1)
#'       \item \code{specPeak,specHNRslope} when looking for putative harmonics in the
#'         spectrum, the threshold for peak detection is calculated as
#'         \code{specPeak * (1 - HNR * specHNRslope)}
#'       \item \code{specSmooth} the width of window for detecting peaks in the
#'         spectrum, Hz
#'       \item \code{specMerge} pitch candidates within \code{specMerge} semitones are
#'         merged with boosted certainty
#'       \item \code{specSinglePeakCert} (0 to 1) if F0 is calculated based on a single
#'         harmonic ratio (as opposed to several ratios converging on the same
#'         candidate), its certainty is taken to be \code{specSinglePeakCert}
#'       \item \code{specMethod} "commonFactor" = highest common factor of
#'       putative harmonics, "BaNa" = ratio of putative harmonics
#'       \item \code{specRatios} for method = "commonFactor", the number of
#'       harmonics and integer fractions to consider
#'     }
#'   }
#'   \item{\code{pitchHps} (harmonic product spectrum)}{
#'     \itemize{
#'       \item \code{hpsNum} the number of times to downsample the spectrum
#'       \item \code{hpsThres} voicing threshold (unitless, ~0 to 1)
#'       \item \code{hpsNorm} the amount of inflation of hps pitch certainty (0 = none)
#'       \item \code{hpsPenalty} the amount of penalizing hps candidates in low
#'         frequencies (0 = none)
#'     }
#'   }
#'   \item{\code{pitchZc} (zero crossings)}{
#'     \itemize{
#'       \item \code{zcThres} pitch candidates with certainty below this value are
#'         treated as noise and set to NA (0 = nothing discarded, 1 = pitch must be
#'         perfectly stable over \code{zcWin})
#'       \item \code{zcWin} certainty in pitch candidates depends on how stable pitch
#'         is over \code{zcWin} glottal cycles (odd integer > 3)
#'     }
#'   }
#' }
#'
#' Each of these lists also accepts graphical parameters that affect how pitch
#' candidates are plotted, eg \code{pitchDom = list(domThres = .5, col = 'yellow')}.
#'
#' Other arguments that are lists of subroutine-specific settings include:
#' \describe{
#'   \item{\code{harmHeight} (finding how high harmonics reach in the spectrum)}{
#'     \itemize{
#'       \item \code{harmThres} minimum height of spectral peak, dB
#'       \item \code{harmPerSel} the number of harmonics per sliding selection
#'       \item \code{harmTol} maximum tolerated deviation of peak frequency from
#'         multiples of f0, proportion of f0
#'     }
#'   }
#' }
#'
#' @seealso \code{\link{pitch_app}} \code{\link{getLoudness}}
#'   \code{\link{segment}} \code{\link{getRMS}}
#'
#' @inheritParams .roxygen_defaults
#' @param silence (0 to 1 as proportion of max amplitude of the anayzed sound)
#'   frames with RMS amplitude below \code{silence * max_ampl adjusted by scale}
#'   are not analyzed at all
#' @param cutFreq if specified, spectral descriptives (peakFreq, specCentroid,
#'   specSlope, and quartiles) are calculated only between \code{cutFreq[1]} and
#'   \code{cutFreq[2]}, Hz. If a single number is given, analyzes frequencies
#'   from 0 to \code{cutFreq}. For ex., when analyzing recordings with varying
#'   sampling rates, set to half the lowest sampling rate to make the spectra
#'   more comparable.
#' @param formants a list of arguments passed to
#'   \code{\link[phonTools]{findformants}} for LPC analysis
#' @param nFormants the number of formants to extract per STFT frame (0 = no
#'   formant analysis, NULL = as many as possible)
#' @param loudness a list of parameters passed to \code{\link{getLoudness}} for
#'   measuring subjective loudness, namely \code{SPL_measured, spreadSpectrum,
#'   sharpnessMethod}. NULL = skip loudness analysis
#' @param roughness a list of parameters passed to
#'   \code{\link{modulationSpectrum}} for measuring roughness and fluctuation
#'   strength. NULL = skip roughness analysis
#' @param novelty a list of parameters passed to \code{\link{ssm}} for measuring
#'   spectral novelty. NULL = skip novelty analysis
#' @param pitchMethods methods of pitch estimation to consider for determining
#'   pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
#'   'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band, 'hps' =
#'   harmonic product spectrum, 'zc' = zero crossings, NULL = no pitch analysis
#' @param pitchManual manually corrected pitch contour. For a single sound,
#'   provide a numeric vector of any length. For multiple sounds, provide a
#'   dataframe with columns "file" and "pitch" (or path to a csv file) as
#'   returned by \code{\link{pitch_app}}, ideally with the same windowLength and
#'   step as in current call to analyze. A named list with pitch vectors per
#'   file is also accepted - e.g., as returned by \code{\link{pitch_app}}
#' @param pitchFloor,pitchCeiling absolute bounds for pitch candidates (Hz)
#' @param priorMean,priorSD specifies the mean (Hz) and standard deviation
#'   (semitones) of gamma distribution describing our prior knowledge about the
#'   most likely pitch values for this file. For ex., \code{priorMean = 300,
#'   priorSD = 6} gives a prior with mean = 300 Hz and SD = 6 semitones (half
#'   an octave). NULL = no priors used at all; NA = no priors in the first pass,
#'   adaptive priors in the second pass if \code{priorAdapt = TRUE}
#' @param priorAdapt adaptive second-pass prior: if TRUE, optimal pitch contours
#'   are estimated first with a prior determined by \code{priorMean,priorSD}, and
#'   then with a new prior adjusted according to this first-pass pitch contour
#' @param nCands maximum number of pitch candidates per method, normally 1 to 4
#'   (except for \code{dom} and \code{hps}, which return at most one candidate
#'   per frame)
#' @param minVoicedCands minimum number of pitch candidates that have to be
#'   defined to consider a frame voiced (if NULL, defaults to 2 if \code{dom} is
#'   among other candidates and 1 otherwise)
#' @param pitchDom a list of control parameters for pitch tracking using the
#' lowest dominant frequency band or "dom" method
#' @param pitchAutocor a list of control parameters for pitch tracking using the
#'   autocorrelation or "autocor" method
#' @param pitchCep a list of control parameters for pitch tracking using the
#'   cepstrum or "cep" method
#' @param pitchSpec a list of control parameters for pitch tracking using the
#'   BaNa or "spec" method
#' @param pitchHps a list of control parameters for pitch tracking using the
#'   harmonic product spectrum or "hps" method
#' @param pitchZc a list of control parameters for pitch tracking based on zero
#'   crossings in bandpass-filtered audio or "zc" method
#' @param harmHeight a list of control parameters for estimating how high
#'   harmonics reach in the spectrum
#' @param subh a list of control parameters for estimating the strength of
#'   subharmonics per frame - that is, spectral energy at integer fractions of
#'   f0: f0/2, f0/3, etc.
#' @param flux a list of control parameters for calculating feature-based flux
#'   (not spectral flux)
#' @param amRange target range of frequencies for amplitude modulation
#'   (\code{amFreq}, Hz): a vector of length 2, defaults to \code{c(10, 60)}.
#'   Affects both \code{amMsFreq} and \code{amEnvFreq}. NB: f0 should not fall
#'   into this range, or AM will treat glottal cycles as modulation
#' @param fmRange target range of frequencies for analyzing frequency
#'   modulation (\code{fmFreq}, Hz): a vector of length 2, defaults to
#'   \code{c(5, 1000 / step / 2)}
#' @param shortestSyl the smallest length of a voiced segment (ms) that
#'   constitutes a voiced syllable (shorter segments will be replaced by NA, as
#'   if voiceless)
#' @param shortestPause the smallest gap between voiced syllables (ms): large
#'   value = interpolate and merge, small value = treat as separate syllables
#'   separated by a voiceless gap; shortestPause < step disables any gap
#'   tolerance (a single voiceless frame terminates the syllable)
#' @param interpolPitch a list of parameters (currently \code{win, tol, cert}) for
#'   interpolating missing pitch candidates (NULL = no interpolation)
#' @param certWeight (0 to 1) in pitch postprocessing, specifies how much we
#'   prioritize the certainty of pitch candidates vs. pitch jumps / the internal
#'   tension of the resulting pitch curve
#' @param smooth,smoothVars if \code{smooth} is a positive number, outliers of
#'   the variables in \code{smoothVars} are adjusted with median smoothing.
#'   \code{smooth} of 1 corresponds to a window of ~100 ms and tolerated
#'   deviation of ~4 semitones. To disable, set \code{smooth = 0}
#' @param summaryFun functions used to summarize each acoustic characteristic,
#'   eg \code{c('mean', 'sd')}; user-defined functions are fine (see examples);
#'   NAs are omitted automatically for mean/median/sd/min/max/range/sum,
#'   otherwise take care of NAs yourself
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   permitted range: 'adjust' = reset to default value, 'abort' = stop
#'   execution, 'ignore' = throw a warning and continue (may crash)
#' @param plot if TRUE, produces a spectrogram with pitch contour overlaid
#' @param showLegend if TRUE, adds a legend with pitch tracking methods
#' @param pitchPlot a list of graphical parameters for displaying the final
#'   pitch contour. Set to \code{list(type = 'n')} to suppress
#' @param extraContour name of an output variable to overlap on the pitch
#'   contour plot, eg 'peakFreq' or 'loudness'; can also be a list with extra
#'   graphical parameters, eg \code{extraContour = list(x = 'harmHeight', col =
#'   'red')}
#' @param osc "linear" = on the original scale (default); "none" = no
#'   oscillogram; "dB" = in decibels
#' @param ylim frequency range to plot, kHz (defaults to 0 to Nyquist
#'   frequency). NB: still in kHz, even if yScale = bark, mel, or ERB
#' @param xlab,ylab,main plotting parameters
#' @param ... other graphical parameters passed to \code{\link{spectrogram}}
#'
#' @return A list with \code{$detailed} frame-by-frame descriptives and a
#'   \code{$summary} with one row per file, as determined by \code{summaryFun}
#'   (e.g., mean / median / SD of each acoustic variable across all STFT
#'   frames). Output measures include:
#' \describe{
#'   \item{duration}{total duration, s}
#'   \item{duration_noSilence}{duration from the beginning of the first
#'     non-silent STFT frame to the end of the last non-silent STFT frame, s
#'     (NB: depends strongly on \code{windowLength} and \code{silence}
#'     settings)}
#'   \item{time}{time of the middle of each frame (ms)}
#'   \item{amEnvFreq,amEnvDep,amEnvPurity}{frequency (Hz), purity (0 to 1), and
#'   depth (0 to 100) of amplitude modulation estimated from a smoothed
#'   amplitude envelope}
#'   \item{amMsFreq,amMsPurity}{frequency (Hz) and purity (dB) of amplitude
#'     modulation estimated via \code{\link{modulationSpectrum}}}
#'   \item{ampl}{root mean square of amplitude per frame, calculated as
#'     sqrt(mean(frame ^ 2))}
#'   \item{ampl_noSilence}{same as \code{ampl}, but ignoring silent frames}
#'   \item{CPP}{Cepstral Peak Prominence, dB (a measure of pitch quality, the
#'     ratio of the highest peak in the cepstrum to the regression line drawn
#'     through it)}
#'   \item{dom}{lowest dominant frequency band (Hz) (see "Pitch tracking
#'     methods / Dominant frequency" in the vignette)}
#'   \item{entropyW}{Wiener entropy of the spectrum of the current frame
#'     (=spectral flatness). Close to 0: pure tone or tonal sound with nearly
#'     all energy in harmonics; close to 1: white noise}
#'   \item{entropySh}{Normalized Shannon entropy of the spectrum of the current
#'     frame: 0 = pure tone, 1 = white noise}
#'   \item{f1_freq, f1_width, ...}{the frequency and bandwidth of the first
#'     nFormants formants per STFT frame, as calculated by
#'     phonTools::findformants}
#'   \item{fluctuation}{strength of low-frequency modulation at ~4 Hz (0.25-30
#'   Hz), calculated from a modulation spectrum as a complement to
#'   psychoacoustic roughness; see \code{\link{modulationSpectrum}}}
#'   \item{flux}{feature-based flux, the rate of change in acoustic features
#'     such as pitch, HNR, etc. (0 = none, 1 = max); "epoch" is an audio segment
#'     between two peaks of flux that exceed a threshold of
#'     \code{flux = list(thres = ...)} (listed in output$detailed only)}
#'   \item{fmFreq}{frequency of frequency modulation (FM) such as vibrato or
#'     jitter, Hz}
#'   \item{fmDep}{depth of FM, semitones}
#'   \item{fmPurity}{purity or dominance of the main FM frequency (fmFreq), 0 to
#'     1}
#'   \item{harmEnergy}{the amount of energy in upper harmonics, namely the ratio
#'     of total spectral mass above 1.25 x F0 to the total spectral mass below
#'     1.25 x F0 (dB)}
#'   \item{harmHeight}{how high harmonics reach in the spectrum, based on the
#'     best guess at pitch (or the manually provided pitch values)}
#'   \item{HNR}{harmonics-to-noise ratio (dB), a measure of harmonicity (see
#'   "Pitch tracking methods / Autocorrelation"). If HNR = 0 dB, there is as
#'   much energy in harmonics as in noise}
#'   \item{loudness}{subjective loudness, in sone, corresponding to the chosen
#'     SPL_measured - see \code{\link{getLoudness}}}
#'   \item{novelty}{spectral novelty - a measure of how variable the spectrum is
#'     on a particular time scale, as estimated by \code{\link{ssm}}}
#'   \item{peakFreq}{the frequency with maximum spectral power (Hz)}
#'   \item{pitch}{post-processed pitch contour based on all F0 estimates}
#'   \item{quartile25, quartile50, quartile75}{the 25th, 50th, and 75th
#'     quantiles of the spectrum of voiced frames (Hz)}
#'   \item{roughness}{the amount of amplitude modulation in the roughness range, see
#'     \code{\link{modulationSpectrum}} and Anikin 2025}
#'   \item{sharpness}{psychoacoustic sharpness: related to spectral centroid,
#'   but calculated from a psychoacoustic loudness model, see
#'   \code{\link{getLoudness}}}
#'   \item{specCentroid}{the center of gravity of the frame's spectrum, first
#'     spectral moment (Hz)}
#'   \item{specSlope}{the slope of linear regression fit to the spectrum below
#'     cutFreq (dB/kHz)}
#'   \item{subDep}{estimated depth of subharmonics per frame: 0 = none, 1 = as
#'     strong as f0. NB: this depends critically on accurate pitch tracking}
#'   \item{subRatio}{the ratio of f0 to subharmonics frequency with strength
#'     subDep: 2 = period doubling, 3 = f0 / 3, etc.}
#'   \item{voiced}{is the current STFT frame voiced? TRUE / FALSE}
#' }
#'
#' @references Anikin, A. (2025) Acoustic estimation of voice roughness.
#'   Attention, Perception, & Psychophysics 87: 1771–1787.
#' @export
#' @examples
#' # Detailed documentation: https://cogsci.se/soundgen/acoustic_analysis.html
#'
#' sound = soundgen(sylLen = 300, pitch = c(500, 400, 600),
#'   noise = list(time = c(0, 300), value = c(-40, 0)),
#'   temperature = 0.001,
#'   addSilence = 50)  # NB: always have some silence before and after!!!
#' # playme(sound, 16000)
#' a = analyze(sound, samplingRate = 16000, plot = TRUE)
#' str(a$detailed)  # frame-by-frame
#' a$summary        # summary per sound
#'
#' \dontrun{
#' # For maximum processing speed (just basic spectral descriptives):
#' a = analyze(sound, samplingRate = 16000,
#'   plot = FALSE,         # no plotting
#'   pitchMethods = NULL,  # no pitch tracking
#'   loudness = NULL,      # no loudness analysis
#'   novelty = NULL,       # no novelty analysis
#'   roughness = NULL,     # no roughness analysis
#'   nFormants = 0         # no formant analysis
#' )
#'
#' # Fancy plotting options:
#' a = analyze(sound, samplingRate = 44100, plot = TRUE,
#'   xlab = 'Time, ms', colorTheme = 'seewave', yScale = 'ERB',
#'   contrast = .5, ylim = c(0.05, 8), main = 'My plot',
#'   pitchMethods = c('dom', 'autocor', 'spec', 'hps', 'cep'),
#'   priorMean = NA,  # no prior info at all
#'   pitchDom = list(col = 'red', domThres = .25),
#'   pitchPlot = list(col = 'black', pch = 9, lty = 3, lwd = 3),
#'   extraContour = list(x = 'peakFreq', type = 'b', pch = 4, col = 'brown'),
#'   osc = 'dB', heights = c(2, 1))
#'
#' # Analyze an entire folder in one go, saving spectrograms with pitch contours
#' # plus an html file for easy access
#' s2 = analyze('~/Downloads/temp',
#'   savePlots = TRUE,  # save the spectrograms with pitch contours
#'   showLegend = TRUE, yScale = 'bark',
#'   width = 20, height = 12,
#'   units = 'cm', res = 300, ylim = c(0, 5),
#'   cores = 4)  # use multiple cores to speed up processing
#' s2$summary[, 1:5]
#'
#' # Analyzing ultrasounds (slow but possible, just adjust pitchCeiling)
#' s = soundgen(sylLen = 100, addSilence = 10,
#'   pitch = c(25000, 35000, 30000),
#'   formants = NA, rolloff = -12, rolloffKHz = 0,
#'   pitchSamplingRate = 350000, samplingRate = 350000, windowLength = 5,
#'   pitchCeiling = 45000, invalidArgAction = 'ignore',
#'   plot = TRUE)
#' # s is a bat-like ultrasound inaudible to humans
#'
#' a = analyze(
#'   s, 350000, plot = TRUE,
#'   pitchFloor = 10000, pitchCeiling = 90000, priorMean = NA,
#'   pitchMethods = c('autocor', 'spec'),
#'   # probably shouldn't use pitchMethods = "dom" b/c of likely low-freq noise
#'   windowLength = 5, step = 2.5,
#'   shortestSyl = 10, shortestPause = 10,  # again, very short sounds
#'   interpolPitch = list(win = 10),  # again, very short sounds
#'   smooth = 0.1,  # might need less smoothing if very rapid f0 changes
#'   nFormants = 0, loudness = NULL, roughness = NULL, novelty = NULL)
#' # NB: ignore formants and loudness estimates for such non-human sounds
#' }
analyze = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    dynamicRange = 80,
    silence = 0.04,
    windowLength = 50,
    step = NULL,
    overlap = 50,
    wn = 'gaussian',
    zp = 0,
    cutFreq = NULL,
    nFormants = 3,
    formants = list(),
    loudness = list(SPL_measured = 70),
    roughness = list(msType = '1D', specMethod = 'spectrum', amRes = 1,
                     specFun_pars = list(windowLength = 25, step = 2)),
    novelty = list(specFun = 'melspec', kernelLen = 1000),
    pitchMethods = c('dom', 'autocor'),
    pitchManual = NULL,
    pitchFloor = 75,
    pitchCeiling = 1000,
    priorMean = 300,
    priorSD = 6,
    priorAdapt = TRUE,
    nCands = 1,
    minVoicedCands = NULL,
    pitchDom = list(domThres = 0.1,
                    domSmooth = 220),
    pitchAutocor = list(autocorThres = 0.7,
                        autocorSmooth = 7,
                        autocorUpsample = 25,
                        autocorBestPeak = 0.975,
                        interpol = 'sinc'),
    pitchCep = list(cepThres = 0.75,
                    cepZp = 0),
    pitchSpec = list(specThres = 0.05,
                     specPeak = 0.25,
                     specHNRslope = 0.8,
                     specSmooth = 150,
                     specMerge = 0.1,
                     specSinglePeakCert = 0.4,
                     specRatios = 3),
    pitchHps = list(hpsNum = 5,
                    hpsThres = 0.1,
                    hpsNorm = 2,
                    hpsPenalty = 2),
    pitchZc = list(zcThres = 0.1,
                   zcWin = 5),
    harmHeight = list(harmThres = 3,
                      harmTol = 0.25,
                      harmPerSel = 5),
    subh = list(method = c('cep', 'pitchCands', 'harm')[1],
                nSubh = 5,
                tol = .05,
                nHarm = 5,
                harmThres = 12,
                harmTol = 0.25),
    flux = list(thres = 0.15),
    amRange = c(10, 60),
    fmRange = NULL,
    shortestSyl = 20,
    shortestPause = 60,
    interpolPitch = list(win = 75, tol = 0.3, cert = 0.3),
    certWeight = .5,
    smooth = 1,
    smoothVars = c('pitch', 'dom'),
    summaryFun = c('mean', 'median', 'sd'),
    invalidArgAction = c('adjust', 'abort', 'ignore'),
    reportEvery = NULL,
    cores = 1,
    plot = FALSE,
    osc = c('linear', 'dB', 'none'),
    showLegend = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    pitchPlot = list(col = rgb(0, 0, 1, .75), lwd = 3, showPrior = TRUE),
    extraContour = NULL,
    ylim = NULL,
    xlab = 'Time',
    ylab = NULL,
    main = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  invalidArgAction = match.arg(invalidArgAction)
  osc = match.arg(osc)

  ## Validate the parameter values that do not depend on sound-specific
  ## characteristics like samplingRate and duration
  # Check simple numeric default pars
  simplePars = c('silence', 'certWeight', 'smooth', 'dynamicRange', 'nFormants')
  for (p in simplePars) {
    gp = try(get(p), silent = TRUE)
    if (!inherits(gp, "try-error")) {
      if (is.numeric(gp)) {
        assign(p, validatePars(p, gp, def = defaults_analyze,
                               invalidArgAction = invalidArgAction))
      }
    }
  }
  rm(simplePars, gp, p)

  # Check parameters supplied as lists
  pitchDom_plotPars = pitchAutocor_plotPars =
    pitchCep_plotPars = pitchSpec_plotPars =
    pitchHps_plotPars = pitchZc_plotPars =
    harmHeight_plotPars = NULL  # otherwise CMD check complains
  # Here we specify just the names of pars as c('', '').
  # (values are in defaults_analyze)
  parsToValidate = list(
    harmHeight = c('harmThres', 'harmTol', 'harmPerSel'),
    pitchDom = c('domThres', 'domSmooth'),
    pitchAutocor = c('autocorThres', 'autocorSmooth',
                     'autocorUpsample', 'autocorBestPeak', 'interpol'),
    pitchCep = c('cepThres', 'cepZp'),
    pitchSpec = c('specSmooth', 'specHNRslope', 'specThres',
                  'specPeak', 'specSinglePeakCert', 'specMerge',
                  'specMethod', 'specRatios'),
    pitchHps = c('hpsNum', 'hpsThres', 'hpsNorm', 'hpsPenalty'),
    pitchZc = c('zcThres', 'zcWin')
  )
  for (i in seq_along(parsToValidate)) {
    parGroup_user = get(names(parsToValidate)[i])
    # whatever is not in parsToValidate is interpreted as plotting options
    assign(paste0(names(parsToValidate)[i], '_plotPars'),
           parGroup_user[!names(parGroup_user) %in% parsToValidate[[i]]])
    # if there's nothing to add, it becomes an empty list()
    # now we check the value of those pars that ARE in parsToValidate
    parGroup_user = parGroup_user[names(parGroup_user) %in% parsToValidate[[i]]]
    parGroup_def = parsToValidate[[i]]
    for (p in parGroup_def) {
      if (is.null(parGroup_user[[p]])) {
        # fall back to the default value
        parGroup_user[[p]] = defaults_analyze[p, 'default']
      } else {
        # validate user-defined value
        if (is.numeric(parGroup_user[[p]])) {
          parGroup_user[[p]] = validatePars(
            p, parGroup_user[[p]], defaults_analyze, invalidArgAction)
        }
      }
    }
    assign(noquote(names(parsToValidate)[i]), parGroup_user)
  }
  rm('parsToValidate', 'parGroup_user', 'parGroup_def', 'p', 'i',
     'harmHeight_plotPars')
  if (is.null(pitchSpec$specMethod) || is.na(pitchSpec$specMethod))
    pitchSpec$specMethod = 'commonFactor'
  if (is.null(pitchAutocor$interpol) || is.na(pitchAutocor$interpol))
    pitchAutocor$interpol = 'sinc'


  # Check defaults that depend on other pars or require customized warnings
  if (is.character(pitchMethods) && pitchMethods[1] != '') {
    valid_names = c('dom', 'autocor', 'cep', 'spec', 'hps', 'zc')
    invalid_names = pitchMethods[!pitchMethods %in% valid_names]
    if (length(invalid_names) > 0) {
      message(paste('Ignoring unknown pitch tracking methods:',
                    paste(invalid_names, collapse = ', '),
                    '; valid pitchMethods:',
                    paste(valid_names, collapse = ', ')))
      pitchMethods = pitchMethods[pitchMethods %in% valid_names]
    }
    rm('valid_names', 'invalid_names')
  }

  if (!is.finite(nCands) || nCands < 1) {
    nCands = 1
    warning('"nCands" must be a positive integer; defaulting to 1')
  }
  if (!is.finite(shortestSyl) || shortestSyl < 0) {
    shortestSyl = 0
    warning('shortestSyl must be non-negative; defaulting to 0 ms')
  }
  if (!is.finite(shortestPause) || shortestPause < 0) {
    shortestPause = 0
    warning('shortestPause must be a non-negative number; defaulting to 0 ms')
  }
  if (!is.null(zp)) {
    if (!is.finite(zp) || zp < 0)
      warning('"zp" must be non-negative; defaulting to 0')
  }

  if (!is.null(amRange)) {
    if (length(amRange) != 2 || !is.numeric(amRange) || any(!is.finite(amRange)) ||
        amRange[1] < 0 || amRange[1] >= amRange[2]) {
      if (invalidArgAction == 'abort')
        stop('"amRange" must be a numeric vector of length 2 with 0 <= amRange[1] < amRange[2]')
      else {
        warning('"amRange" is invalid; defaulting to c(10, 60)')
        amRange = c(10, 60)
      }
    }
  }

  if (!is.null(fmRange)) {
    if (length(fmRange) != 2 || !is.numeric(fmRange) || any(!is.finite(fmRange)) ||
        fmRange[1] < 0 || fmRange[1] >= fmRange[2]) {
      if (invalidArgAction == 'abort')
        stop('"fmRange" must be a numeric vector of length 2 with 0 <= fmRange[1] < fmRange[2]')
      else warning('"fmRange" is invalid; will be reset per file based on step')
    }
  }

  # reformat pitchManual, if any
  if (!is.null(pitchManual)) {
    pitchManual_list = formatPitchManual(pitchManual)
  } else {
    pitchManual_list = NULL
  }

  # reformat loudness/novelty etc lists, if any
  if (!is.null(loudness)) {
    if (is.null(loudness$SPL_measured)) loudness$SPL_measured = 70
    if (is.null(loudness$spreadSpectrum)) loudness$spreadSpectrum = TRUE
  }
  if (!is.null(interpolPitch)) {
    if (is.null(interpolPitch$win)) interpolPitch$win = 75
    if (is.null(interpolPitch$tol)) interpolPitch$tol = .3
    if (is.null(interpolPitch$cert)) interpolPitch$cert = .3
  }
  if (is.null(flux$thres)) flux$thres = 0.15

  # match args
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'reportEvery', 'cores',
    'savePlots', 'embed', 'pitchManual', 'summaryFun', 'invalidArgAction')]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.analyze',
    suffix = 'analyze',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          pa$result[[i]],
          summaryFun = summaryFun,
          var_noSummary = c('duration', 'duration_noSilence',
                            'voiced', 'time', 'epoch'))
      }
    }
    idx_failed = which(pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]] [1, ]
        filler[1, ] = NA
      } else {
        stop('Failed to analyze any input')
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = try(cbind(data.frame(file = pa$input$filenames_base),
                          rbind_fill_list(temp)))
    if (inherits(mysum_all, 'try-error')) mysum_all = NULL
  } else {
    mysum_all = NULL
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(list(
    detailed = pa$result,
    summary = mysum_all
  ))
}


#' Analyze per sound
#'
#' Internal soundgen function
#'
#' Called by \code{\link{analyze}} and \code{\link{pitch_app}} to analyze a
#' single sound.
#' @inheritParams analyze
#' @param audio a list returned by \code{readAudio}
#' @noRd
.analyze = function(
    audio,
    dynamicRange = 80,
    silence = 0.04,
    windowLength = 50,
    step = NULL,
    overlap = 50,
    wn = 'gaussian',
    zp = 0,
    cutFreq = NULL,
    nFormants = 3,
    formants = NULL,
    loudness = NULL,
    roughness = NULL,
    novelty = NULL,
    pitchMethods = c('dom', 'autocor'),
    pitchManual_list = NULL,
    pitchFloor = 75,
    pitchCeiling = 1000,
    priorMean = 300,
    priorSD = 6,
    priorAdapt = TRUE,
    nCands = 1,
    minVoicedCands = NULL,
    pitchDom = list(domThres = 0.1,
                    domSmooth = 220),
    pitchAutocor = list(autocorThres = 0.7,
                        autocorSmooth = 7,
                        autocorUpsample = 25,
                        autocorBestPeak = 0.975,
                        interpol = 'sinc'),
    pitchCep = list(cepThres = 0.75,
                    cepZp = 0),
    pitchSpec = list(specThres = 0.05,
                     specPeak = 0.25,
                     specHNRslope = 0.8,
                     specSmooth = 150,
                     specMerge = 0.1,
                     specSinglePeakCert = 0.4,
                     specRatios = 3),
    pitchHps = list(hpsNum = 5,
                    hpsThres = 0.1,
                    hpsNorm = 2,
                    hpsPenalty = 2),
    pitchZc = list(zcThres = 0.1,
                   zcWin = 5),
    harmHeight = list(harmThres = 3,
                      harmTol = 0.25,
                      harmPerSel = 5),
    subh = list(method = c('cep', 'pitchCands', 'harm')[1],
                nSubh = 5,
                tol = .05,
                nHarm = 5,
                harmThres = 12,
                harmTol = 0.25),
    flux = list(thres = 0.15),
    amRange = c(10, 60),
    fmRange = NULL,
    shortestSyl = 20,
    shortestPause = 60,
    interpolPitch = list(win = 75, tol = .3, cert = .3),
    certWeight = .5,
    smooth = 1,
    smoothVars = c('pitch', 'dom'),
    returnPitchCands = FALSE,
    plot = TRUE,
    showLegend = TRUE,
    osc = 'linear',
    pitchPlot = list(col = rgb(0, 0, 1, .75), lwd = 3, showPrior = TRUE),
    pitchDom_plotPars = list(),
    pitchAutocor_plotPars =list(),
    pitchCep_plotPars = list(),
    pitchSpec_plotPars =list(),
    pitchHps_plotPars = list(),
    pitchZc_plotPars = list(),
    extraContour = NULL,
    ylim = NULL,
    xlab = NULL,
    ylab = NULL,
    main = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  extraSpecPars = list(...)
  extraSpecPars$osc = NULL
  ## Validate the parameter values that depend on sound-specific
  ## characteristics like samplingRate and duration
  val = validateWlOvlp(audio, windowLength, step, overlap)
  wl = val$wl; step = val$step; step_points = val$step_points
  nFreqs = wl %/% 2 + 1

  if (is.null(zp)) zp = nextn(wl)
  if (zp != 0 && zp < wl) {
    zp = 0
    message('zp must be > windowLength in samples to have any effect; resetting to 0')
  }
  wl_zp = max(wl, zp)
  nFreqs_zp = wl_zp %/% 2 + 1

  # check parameters that depend on audio$, and thus cannot be checked in analyze()
  if (!is.null(cutFreq) && !any(is.na(cutFreq))) {
    # a single value refers to upper end of the analyzed frequency range
    if (length(cutFreq) == 1) cutFreq = c(0, cutFreq)
    if (is.na(cutFreq[1]) || cutFreq[1] <= 0) cutFreq[1] = 0
    if (is.na(cutFreq[2]) || cutFreq[2] > (audio$samplingRate / 2)) {
      cutFreq[2] = audio$samplingRate / 2
      warning(paste('"cutFreq" should not be above Nyquist;',
                    'resetting to samplingRate/2'))
    }
  }
  if (!is.finite(pitchFloor) || pitchFloor <= 0 ||
      pitchFloor > audio$samplingRate / 2) {
    pitchFloor = 1
    warning(paste('"pitchFloor" must be between 0 and pitchCeiling;',
                  'defaulting to 1 Hz'))
  } # 1 Hz ~ 4 octaves below C0
  if (!is.finite(pitchCeiling) || pitchCeiling > audio$samplingRate / 2) {
    pitchCeiling = audio$samplingRate / 2  # Nyquist
    warning(paste('"pitchCeiling" must be between 0 and Nyquist;',
                  'defaulting to samplingRate / 2'))
  }
  if (pitchFloor > pitchCeiling) {
    pitchFloor = 1
    pitchCeiling = audio$samplingRate / 2
    warning(paste('"pitchFloor" cannot be above "pitchCeiling";',
                  'defaulting to 1 Hz and samplingRate / 2, respectively'))
  }

  if (is.numeric(priorMean) && is.finite(priorMean)) {
    if (priorMean > audio$samplingRate / 2 || priorMean <= 0) {
      priorMean = 300
      warning(paste('"priorMean" must be between 0 and Nyquist;',
                    'defaulting to 300; set to NULL to disable priors completely',
                    'or NA to disabled priors in the first pass'))
    }
  }
  if (is.numeric(priorSD) && is.finite(priorSD)) {
    if (priorSD <= 0) {
      priorSD = 6
      warning('"priorSD" must be positive; defaulting to 6 semitones')
    }
  }
  if (is.null(priorMean) || is.null(priorSD)) priorAdapt = FALSE
  if (!is.null(interpolPitch)) {
    if (is.null(interpolPitch$win)) interpolPitch$win = 7
    if (is.null(interpolPitch$tol)) interpolPitch$tol = 0.3
    if (is.null(interpolPitch$cert)) interpolPitch$cert = 0.3

    if (shortestPause > 0 && interpolPitch$win > 0) {
      if (interpolPitch$win * step < shortestPause / 2) {
        interpolPitch$win = ceiling(shortestPause / 2 / step)
        warning(paste(
          '"interpolPitch$win" reset to', interpolPitch$win,
          ': interpolation must be able to bridge merged voiced fragments'))
      }
    }
    if (interpolPitch$tol <= 0) {
      interpolPitch$tol = 0.3
      warning('"interpolPitch$tol" must be positive; defaulting to 0.3')
    }
  }
  if (shortestPause < step || shortestPause < 1000 / pitchFloor) {
    shortestPause = max(1.5 * step, 1.5 * 1000 / pitchFloor)
    warning(paste0('shortestPause must be > step and 1000/pitchFloor',
                   '; resetting to = ', shortestPause, ' ms'))
  }
  if (is.null(fmRange)) fmRange = c(5, 1000 / step / 2)

  if (!is.null(novelty)) {
    if (is.null(novelty$specFun_pars$windowLength))
      novelty$specFun_pars$windowLength = windowLength
    if (is.null(novelty$specFun_pars$step))
      novelty$specFun_pars$step = step
  }


  if (!is.finite(pitchAutocor$autocorSmooth)) {
    pitchAutocor$autocorSmooth = 2 * ceiling(7 * audio$samplingRate / 44100 / 2) - 1
    # width of smoothing interval, chosen to be proportionate to samplingRate (7
    # for samplingRate 44100), but always an odd number.
    # for(i in seq(16000, 60000, length.out = 10)) {
    #   print(paste(round(i), ':', 2 * ceiling(7 * i / 44100 / 2) - 1))
    # }
  }

  ## NORMALIZATION
  # Adjust silence threshold as proportion of the observed max ampl
  # (has the effect of looking for voiced segments even in very quiet files)
  m = max(abs(audio$sound))
  if (m == 0) {
    stop('nothing to do: the input is silent')
  }
  m_to_scale = m / audio$scale
  silence = silence * m_to_scale

  # calculate loudness before normalizing (most routines in analyze()
  # require scale [-1, 1])
  if (!is.null(loudness)) { # if analyzing loudness
    ldns = try(do.call(.getLoudness, c(list(
      audio = audio[which(!names(audio) %in% c('savePlots', 'embed'))],
      windowLength = windowLength, step = step, plot = FALSE),
      loudness)))

    if (audio$samplingRate < 2000) {
      warning(paste('Sampling rate must be >2 KHz to resolve frequencies of at least 8 barks',
                    'and estimate loudness in sone'))
    } else if (audio$samplingRate > 44100) {
      message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                    '(27 KHz) as inaudible to humans when estimating loudness'))
    }
  }

  # normalize to range from no less than -1 to no more than +1
  audio$sound = audio$sound - mean(audio$sound)
  max_sound = max(abs(audio$sound))
  if (max_sound != 0) audio$sound = audio$sound / max_sound


  ## ANALYSIS
  # Set up filter for calculating pitchAutocor
  filter = winFun(wl, wn = wn)
  # plot(filter, type='l')
  autoCorrelation_filter = acf_fft(filter, center = FALSE)[1:nFreqs]
  # NB: don't center the filter when calculating ACF! Otherwise negative values
  # plot(autoCorrelation_filter, type = 'l')

  ## fft and acf per frame
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  frameBank = getFrameBank(
    sound = audio$sound,
    samplingRate = audio$samplingRate,
    wl = wl,
    wn = wn,
    step = step,
    zp = zp,
    normalize = TRUE,
    filter = NULL,
    padWithSilence = FALSE,
    timeShift = audio$timeShift
  )
  timestamps = as.numeric(colnames(frameBank))

  # fft of each frame
  z_full = mvfft(frameBank)
  if (!inherits(z_full, 'matrix')) z_full = matrix(z_full, ncol = 1)
  s = Mod(z_full[1:nFreqs_zp, ])
  # width of spectral bin, Hz
  bin = audio$samplingRate / nrow(z_full)  # nrow(z_full) is either wl or zp (if zp > 0)
  freqs = (0:(nFreqs_zp - 1)) * bin  # freqs, Hz
  rownames(s) = freqs / 1000  # freqs, KHz
  colnames(s) = timestamps  # time, ms
  # image(t(s))
  # specFlux = getSpectralFlux(s)  # spectral flux - not very useful, IMHO

  # calculate rms amplitude of each frame
  myseq = (timestamps - audio$timeShift * 1000 - windowLength / 2) *
    audio$samplingRate / 1000 + 1
  myseq[1] = 1  # just in case of rounding errors
  l = length(myseq)
  myseq[l] = min(myseq[l], audio$ls - wl)
  # perceived intensity - root mean square of amplitude

  # 'scale' is the max possible amplitude of the input format (e.g., 1 for floats,
  # 32768 for 16-bit ints). Since 'audio$sound' is normalized to [-1, 1] at this
  # point, multiplying RMS by 'm / scale' gives the true RMS as a proportion of
  # the maximum possible dynamic range.
  ampl = vapply(myseq, function(x) {
    sqrt(mean((audio$sound[x:(x + wl - 1)] *
                 m / audio$scale) ^ 2, na.rm = TRUE))
  }, numeric(1))

  # calculate Wiener entropy of each frame within the most relevant vocal range
  # only (up to to cutFreq Hz)
  rowLow = 1 # which(as.numeric(rownames(s)) > 0.05)[1] # 50 Hz
  if (!is.null(cutFreq)) {
    rowHigh = tail(which(freqs <= cutFreq[2]), 1) # 6000 Hz etc
  } else {
    rowHigh = nrow(s)
  }
  if (length(rowHigh) < 1 || !is.finite(rowHigh)) rowHigh = nrow(s)
  # if the frame is too quiet, we will not analyze it
  cond_silence = ampl >= silence & colSums(s) > 0
  # (need both b/c s frames are not 100% synchronized with ampl frames)
  # cond_silence[is.na(cond_silence)] = FALSE  # just in case of weird NAs
  framesToAnalyze = which(cond_silence)

  # save duration of non-silent part of audio
  nf = length(framesToAnalyze)
  if (nf > 0) {
    # the beginning of the first non-silent frame
    time_start = timestamps[framesToAnalyze[1]] - windowLength / 2
    # the end of the last non-silent frame
    time_end = timestamps[framesToAnalyze[nf]] + windowLength / 2
    duration_noSilence = (time_end - time_start) / 1000
  } else {
    duration_noSilence = 0
    message(paste0(
      'The audio is too quiet! No frames above silence = ', silence
    ))
  }

  # autocorrelation for each frame
  autocorBank = matrix(NA, nrow = nFreqs, ncol = ncol(frameBank))
  for (i in which(cond_silence)) {
    # acf is ~10 times slower than FFT
    autoCorrelation_fr = acf_fft(frameBank[1:wl, i])[1:nFreqs]
    # NB: don't include the zeros if zp>0, so just frameBank[1:wl, ]
    # plot(autoCorrelation_fr, type = 'l')

    autocorBank[, i] = autoCorrelation_fr / pmax(autoCorrelation_filter, 1e-10)
    # plot(autocorBank[, i], type = 'l')
    # plot(rownames(autocorBank), autocorBank[, i], type = 'l', log = 'x')
  }
  autocorBank = autocorBank[-1, , drop = FALSE]  # b/c it starts with zero lag (identity)
  rownames(autocorBank) = audio$samplingRate / (1:(nFreqs - 1))
  # plot(rownames(s)[1:50], s[1:50, 8], type = 'l')
  # plot(frameBank[, 8], type = 'l')
  # filled.contour(t(autocorBank))

  ## FORMANTS
  fmts = NULL
  no_formants = FALSE
  if (is.null(nFormants)) nFormants = 10
  # try one frame to see how many formants are returned
  fmts_list = vector('list', length = nf)
  if (nFormants > 0 && nf > 0) {
    # we don't really know how many formants will be returned by phonTools, so
    # we save everything at first, and then trim to nFormants
    for (i in 1:nf) {
      fmts_list[[i]] = try(suppressWarnings(do.call(
        phonTools::findformants,
        c(list(frameBank[1:wl, framesToAnalyze[i]],
               fs = audio$samplingRate, verify = FALSE),
          formants))),
        silent = TRUE)
      if (inherits(fmts_list[[i]], 'try-error')) {
        fmts_list[[i]] = data.frame(formant = NA, bandwidth = NA)[-1, ]
      }
    }
    # check how many formants we will/can save
    nFormants_avail = min(nFormants, max(vapply(fmts_list, nrow, numeric(1))))
    if (nFormants_avail > 0) {
      nFormants = nFormants_avail
      availableRows = seq_len(nFormants)
      fmts = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
      colnames(fmts) = paste0('f', rep(availableRows, each = 2),
                              rep(c('_freq', '_width'), nFormants))
      # iterate through the full formant list and save what's needed
      for (i in 1:nf) {
        ff = fmts_list[[i]]
        if (is.list(ff)) {
          nr = nrow(ff)
          if (nr < nFormants) {
            ff[(nr + 1):nFormants, ] = NA
          }
          temp = matrix(NA, nrow = nFormants, ncol = 2)
          temp[availableRows, ] = as.matrix(ff[availableRows, ])
          fmts[framesToAnalyze[i], ] = matrix(t(temp), nrow = 1)
        }
      }
    } else {
      no_formants = TRUE
    }
  } else if (nFormants > 0 && nf == 0) {
    no_formants = TRUE
  }
  if (no_formants) {
    # no formant analysis
    availableRows = 1:nFormants
    fmts = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
    colnames(fmts) = paste0('f', rep(availableRows, each = 2),
                            rep(c('_freq', '_width'), nFormants))
  }

  ## PITCH and other spectral analysis of each frame from fft
  # set up an empty nested list to save values in - this enables us to analyze
  # only the non-silent and not-too-noisy frames but still have a consistently
  # formatted output
  frameInfo = rep(list(list(
    'pitchCands_frame' = data.frame(
      'pitchCand' = NA,
      'pitchCert' = NA,
      'pitchSource' = NA,
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    'summaries' = data.frame(
      'HNR' = NA,
      'dom' = NA,
      'specCentroid' = NA,
      'peakFreq' = NA,
      'quartile25' = NA,
      'quartile50' = NA,
      'quartile75' = NA,
      'specSlope' = NA,
      'entropyW' = NA,
      'entropySh' = NA
    )
  )), ncol(s))

  for (i in framesToAnalyze) {
    # for each frame that satisfies our condition, do spectral analysis (NB: we
    # do NOT analyze frames that are too quiet, so we only get NA's for those
    # frames, no meanFreq, dom etc!)
    frameInfo[[i]] = analyzeFrame(
      frame = s[, i],
      bin = bin, freqs = freqs,  # prepared in analyze() to save time
      autoCorrelation = autocorBank[, i],
      samplingRate = audio$samplingRate,
      cutFreq = cutFreq,
      trackPitch = cond_silence[i],
      pitchMethods = pitchMethods,
      nCands = nCands,
      pitchDom = pitchDom,
      pitchAutocor = pitchAutocor,
      pitchCep = pitchCep,
      pitchSpec = pitchSpec,
      pitchHps = pitchHps,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling
    )
  }

  # Store the descriptives provided by function analyzeFrame in a dataframe
  result = lapply(frameInfo, function(y) y[['summaries']])
  result = data.frame(matrix(unlist(result), nrow = length(frameInfo), byrow = TRUE))
  colnames(result) = names(frameInfo[[1]]$summaries)
  if (!is.null(fmts)) result = cbind(result, fmts)
  result$ampl = ampl
  result$ampl_noSilence = NA
  if (length(framesToAnalyze) > 0) {
    result$ampl_noSilence[framesToAnalyze] = ampl[framesToAnalyze]
  }
  result$time = as.numeric(colnames(frameBank))
  result$duration_noSilence = duration_noSilence
  result$duration = audio$duration
  # nc = ncol(result)
  nr = nrow(result)

  # add loudness
  if (exists('ldns') && !inherits(ldns, 'try-error')) {
    # use resample instead of just approx() b/c there can be leading and trailing NAs
    result$loudness = .resample(list(sound = ldns$loudness), len = nr,
                                lowPass = FALSE, plot = FALSE)
    result$sharpness = .resample(list(sound = ldns$sharpness), len = nr,
                                 lowPass = FALSE, plot = FALSE)
  } else {
    result[, c('loudness', 'sharpness')] = NA
  }

  # change the order of columns
  first_three = c('duration', 'duration_noSilence', 'time')
  rest = colnames(result)[!colnames(result) %in% first_three]
  result = result[, c(first_three, sort(rest))]

  ## Pitch tracking based on zero crossing rate
  if ('zc' %in% pitchMethods) {
    pitch_zc = do.call(.getPitchZc, c(pitchZc, list(
      audio = audio,
      env = ampl,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling, # priorMean * 2 ^ (priorSD / 12),
      silence = silence)))
    # plot(pitch_zc$time, pitch_zc$pitch, type = 'l')
    pitch_zc_cnt = .resample(list(sound = pitch_zc$pitch), len = nr,
                             lowPass = FALSE, plot = FALSE)
    pitch_zc_cnt[-framesToAnalyze] = NA
    # plot(pitch_zc_cnt, type = 'l')
    pitch_zc_cert = .resample(list(sound = pitch_zc$cert), len = nr,
                              lowPass = FALSE, plot = FALSE)
    # plot(pitch_zc_cert, type = 'l')
    idx_notNA = which(!is.na(pitch_zc_cnt))
    idx_notNA = idx_notNA[idx_notNA %in% framesToAnalyze]
    if (length(idx_notNA) > 0) {
      for (i in idx_notNA) {
        zc_i = data.frame(pitchCand = pitch_zc_cnt[i],
                          pitchCert = pitch_zc_cert[i],
                          pitchSource = 'zc')
        frameInfo[[i]]$pitchCands_frame = rbind(
          frameInfo[[i]]$pitchCands_frame, zc_i)
      }
    }
  }

  ## Postprocessing
  # extract and prepare pitch candidates for the pathfinder algorithm
  pm_woDom = pitchMethods[pitchMethods != 'dom']
  if (length(pm_woDom) > 0) {
    pitchNames = data.frame(pitchMethod = pm_woDom,
                            stringsAsFactors = FALSE)
    pitchNames$pitchName = paste0(
      'pitch',
      toupper(substr(pitchNames$pitchMethod, 1, 1)),
      substr(pitchNames$pitchMethod, 2, nchar(pitchNames$pitchMethod))
    )
  } else {
    pitchNames = list('pitchMethod' = NULL, 'pitchName' = NULL)
  }

  max_cands = max(unlist(lapply(frameInfo, function(y)
    nrow(na.omit(y[['pitchCands_frame']])))))
  if (max_cands == 0) {
    # no pitch candidates at all, purely voiceless
    result[, c('pitch', pitchNames$pitchName)] = NA
    pitchCands_list = list()
  } else {
    # some pitch candidates found
    pitchCands_list = rep(list(matrix(
      NA,
      nrow = max_cands,
      ncol = length(frameInfo),
      dimnames = list(1:max_cands, result$time)
    )), 3)
    names(pitchCands_list) = c('freq', 'cert', 'source')
    for (i in seq_along(frameInfo)) {
      temp = frameInfo[[i]]$pitchCands_frame
      n = nrow(temp)
      if (n > 0) {
        seqn = 1:n
        pitchCands_list[[1]][seqn, i] = temp[, 1]
        pitchCands_list[[2]][seqn, i] = temp[, 2]
        pitchCands_list[[3]][seqn, i] = temp[, 3]
      }
    }

    # divide the file into continuous voiced syllables
    voicedSegments = findVoicedSegments(
      pitchCands_list$freq,
      shortestSyl = shortestSyl,
      shortestPause = shortestPause,
      minVoicedCands = minVoicedCands,
      pitchMethods = pitchMethods,
      step = step
    )

    # add prior
    if (is.numeric(priorMean) && is.numeric(priorSD) &&
        is.finite(priorMean) && is.finite(priorSD)) {
      pitchCert_multiplier = getPrior(priorMean = priorMean,
                                      priorSD = priorSD,
                                      pitchFloor = pitchFloor,
                                      pitchCeiling = pitchCeiling,
                                      pitchCands = pitchCands_list$freq,
                                      plot = FALSE)
      cert_orig = pitchCands_list$cert
      pitchCands_list$cert = pitchCands_list$cert * pitchCert_multiplier
    }

    # for each syllable, impute NAs and find a nice path through pitch candidates
    pitchFinal = rep(NA, ncol(pitchCands_list$freq))
    if (nrow(voicedSegments) > 0) {
      # if we have found at least one putatively voiced syllable
      for (syl in 1:nrow(voicedSegments)) {
        myseq = voicedSegments$segmentStart[syl]:voicedSegments$segmentEnd[syl]
        # compute the optimal path through pitch candidates
        pitchFinal[myseq] = pathfinder(
          pitchCands = pitchCands_list$freq[, myseq, drop = FALSE],
          pitchCert = pitchCands_list$cert[, myseq, drop = FALSE],
          pitchSource = pitchCands_list$source[, myseq, drop = FALSE],
          step = step,
          certWeight = certWeight,
          interpolWin_bin = ceiling(interpolPitch$win / step),
          interpolTol = interpolPitch$tol,
          interpolCert = interpolPitch$cert
        )
      }
    }

    # second pass with adaptive prior
    prior_changed = FALSE
    if (priorAdapt) {
      # revert to original pitchCert
      if (exists('cert_orig'))
        pitchCands_list$cert = cert_orig

      if (any(!is.na(pitchFinal))) {
        pitch_sem = HzToSemitones(pitchFinal[!is.na(pitchFinal)])
        new_mean = mean(pitchFinal, na.rm = TRUE)
        new_sd = sd(pitch_sem)
        if (is.finite(new_sd) && is.finite(new_mean)) {
          priorSD = max(1, new_sd)
          priorMean = new_mean
          pitchCert_multiplier2 = getPrior(priorMean = priorMean,
                                           priorSD = priorSD,
                                           pitchFloor = pitchFloor,
                                           pitchCeiling = pitchCeiling,
                                           pitchCands = pitchCands_list$freq,
                                           plot = FALSE)
          pitchCands_list$cert = pitchCands_list$cert * pitchCert_multiplier2
          prior_changed = TRUE
        }
      }
    }

    if (prior_changed) {
      pitchFinal = rep(NA, ncol(pitchCands_list$freq))
      if (nrow(voicedSegments) > 0) {
        # if we have found at least one putatively voiced syllable
        for (syl in 1:nrow(voicedSegments)) {
          myseq = voicedSegments$segmentStart[syl]:voicedSegments$segmentEnd[syl]
          # compute the optimal path through pitch candidates
          pitchFinal[myseq] = pathfinder(
            pitchCands = pitchCands_list$freq[, myseq, drop = FALSE],
            pitchCert = pitchCands_list$cert[, myseq, drop = FALSE],
            pitchSource = pitchCands_list$source[, myseq, drop = FALSE],
            step = step,
            certWeight = certWeight,
            interpolWin_bin = ceiling(interpolPitch$win / step),
            interpolTol = interpolPitch$tol,
            interpolCert = interpolPitch$cert
          )
        }
      }
    }

    # save optimal pitch track and the best candidates separately for
    # each pitch tracking method
    result$pitch = pitchFinal # optimal pitch track
    if (!is.null(pitchNames$pitchMethod)) {
      for (p in seq_len(nrow(pitchNames))) {
        result[[pitchNames$pitchName[p]]] = vapply(frameInfo, function(x) {
          idx_method = which(x$pitchCands_frame$pitchSource == pitchNames$pitchMethod[p])
          if (length(idx_method) == 0) return(NA_real_)
          idx_maxCert = which.max(x$pitchCands_frame$pitchCert[idx_method])
          if (length(idx_maxCert) == 0) return(NA_real_)
          x$pitchCands_frame$pitchCand[idx_method][idx_maxCert]
        }, numeric(1))
      }
    }
  }

  ## Median smoothing of specified contours (by default pitch & dom)
  if (smooth > 0) {
    points_per_sec = nr / audio$duration
    # smooth of 1 means that smoothing window is ~100 ms
    smoothing_ww = round(smooth * points_per_sec / 10, 0)
    # the larger smooth, the heavier the smoothing (lower tolerance
    # threshold before values are replaced by median over smoothing window).
    # smooth of 1 gives smoothingThres of 4 semitones
    smoothingThres = 4 / smooth
    keep = apply(result[, smoothVars, drop = FALSE], 2, function(x) any(!is.na(x)))
    smoothVars = smoothVars[keep]
    if (length(smoothVars) > 0) {
      result[, smoothVars] = medianSmoother(
        result[, smoothVars, drop = FALSE],
        smoothing_ww = smoothing_ww,
        smoothingThres = smoothingThres
      )
    }
  } else {
    smoothing_ww = 0
    smoothingThres = Inf
  }

  # Convert HNR to dB
  result$HNR = zeroOne_to_dB(result$HNR)

  ## Finalize / update results using the final pitch contour
  # (or the manual pitch contour, if provided)
  pitch_true = result$pitch
  if (!is.null(pitchManual_list)) {
    if (!is.null(names(pitchManual_list)) && audio$filename_base %in% names(pitchManual_list)) {
      # match by filename
      pitch_raw = pitchManual_list[[audio$filename_base]]
    } else if (length(pitchManual_list) == 1 && is.null(names(pitchManual_list))) {
      # single unnamed vector/list, apply to all
      pitch_raw = pitchManual_list[[1]]
    } else {
      pitch_raw = NULL
    }

    if (!is.null(pitch_raw)) {
      # up/downsample pitchManual to the right length
      pitch_true = .resample(list(sound = pitch_raw), len = nr,
                             lowPass = FALSE, plot = FALSE)
    } else {
      message(paste(
        'Failed to find file', audio$filename_base,
        'in the provided pitchManual; using detected pitch contour instead'))
    }
  }

  ## Roughness and AM calculation via a modulation spectrum
  if (is.null(roughness) ||
      (!is.null(roughness$amRes) && roughness$amRes == 0)) {
    # don't analyze the modulation spectrum
    result[, c('roughness', 'fluctuation', 'amMsFreq', 'amMsPurity')] = NA
  } else {
    roughness$amRange = NULL  # b/c already specified as a separate "amRange" arg
    ms = try(do.call(.modulationSpectrum, c(
      list(audio = audio[c('sound', 'samplingRate', 'ls', 'duration')],
           returnMS = FALSE, plot = FALSE, amRange = amRange),
      roughness)))
    if (!inherits(ms, 'try-error')) {
      result$roughness = .resample(list(sound = ms$roughness), len = nr,
                                   lowPass = FALSE, plot = FALSE)
      result$fluctuation = .resample(list(sound = ms$fluctuation), len = nr,
                                     lowPass = FALSE, plot = FALSE)
      result$amMsFreq = .resample(list(sound = ms$amMsFreq), len = nr,
                                  lowPass = FALSE, plot = FALSE)
      result$amMsPurity = .resample(list(sound = ms$amMsPurity), len = nr,
                                    lowPass = FALSE, plot = FALSE)
      result[!cond_silence, c('roughness', 'fluctuation', 'amMsFreq', 'amMsPurity')] = NA
    }
  }

  ## Novelty calculation
  result$novelty = NA
  if (!is.null(novelty)) {
    novel = try(do.call(.ssm, c(
      list(audio = audio[c('sound', 'samplingRate', 'ls', 'duration', 'timeShift')],
           plot = FALSE),
      novelty)))
    if (!inherits(novel, 'try-error') && !is.null(novel$novelty)) {
      result$novelty = .resample(list(sound = novel$detailed$novelty), len = nr,
                                 lowPass = FALSE, plot = FALSE)
      # result$novelty[!cond_silence] = NA  # silent sections can be surprising
    }
  }

  # AM from envelope
  result[, c('amEnvFreq', 'amEnvDep', 'amEnvPurity')] = NA
  if (!is.null(amRange)) {
    # NB: we don't pass overlap b/c getAM_env requires a higher one
    am = try(getAM_env(audio = audio,
                       amRange = amRange,
                       plot = FALSE))
    if (!inherits(am, 'try-error')) {
      result$amEnvFreq = .resample(list(sound = am$freq), len = nr,
                                   lowPass = FALSE, plot = FALSE)
      result$amEnvDep = .resample(list(sound = am$dep), len = nr,
                                  lowPass = FALSE, plot = FALSE)
      result$amEnvPurity = .resample(list(sound = am$purity), len = nr,
                                     lowPass = FALSE, plot = FALSE)
      result[!cond_silence, c('amEnvFreq', 'amEnvDep', 'amEnvPurity')] = NA
    }
  }

  # save spectral descriptives separately for voiced and voiceless frames
  varsToUnv = c(
    'ampl', 'roughness', 'fluctuation', 'amMsFreq', 'amMsPurity',
    'amEnvFreq', 'amEnvDep', 'amEnvPurity', 'novelty', 'entropyW', 'entropySh',
    'dom', 'HNR', 'loudness', 'sharpness', 'peakFreq', 'quartile25',
    'quartile50', 'quartile75', 'specCentroid', 'specSlope'
  )
  result[, paste0(varsToUnv, 'Voiced')] = result[, varsToUnv]

  # update using final / manual pitch
  result = updateAnalyze(
    result = result,
    pitch_true = pitch_true,
    pitchCands_list = pitchCands_list,
    spectrogram = s,
    freqs = freqs,
    bin = bin,
    samplingRate = audio$samplingRate,
    harmHeight_pars = harmHeight,
    subh_pars = subh,
    flux_pars = flux,
    fmRange = fmRange,
    # NB: peakFreq & specCentroid are defined for voiceless frames, but not quartiles
    varsToUnv = paste0(varsToUnv, 'Voiced')
  )

  ## Add pitch contours to the spectrogram
  if (plot) {
    # extra contour
    col_non_Hz = c(
      'amEnvDep', 'amEnvPurity', 'amMsPurity', 'ampl', 'amplVoiced',
      'entropyW', 'entropyWVoiced', 'entropySh', 'entropyShVoiced',
      paste0('f', 1:10, '_width'), 'flux', 'fmDep',
      'harmEnergy', 'harmSlope', 'HNR', 'HNR_voiced', 'CPP',
      'loudness', 'loudnessVoiced', 'sharpness', 'sharpnessVoiced',
      'roughness', 'roughnessVoiced', 'fluctuation', 'fluctuationVoiced',
      'novelty', 'noveltyVoiced', 'specSlope', 'specSlopeVoiced',
      'subDep', 'subRatio'
    )
    cnt = cnt_name = cnt_plotPars = NULL
    if (!is.null(extraContour)) {
      if (is.list(extraContour)) {
        cnt_name = extraContour[[1]]
        cnt_plotPars = extraContour[-1]
      } else {
        cnt_name = extraContour
      }
      valid_cols = colnames(result)[4:ncol(result)]
      valid_cols = valid_cols[valid_cols != 'voiced']
      if (cnt_name %in% valid_cols) {
        cnt = result[, cnt_name]
        if (cnt_name %in% col_non_Hz) {
          # normalize
          if (is.null(ylim)) ylim = c(0, audio$samplingRate / 2 / 1000)
          cnt = zeroOne(cnt) * HzToOther(ylim[2] * 1000, extraSpecPars$yScale)
        }
      } else {
        message(paste0('Valid extraContour names are: ',
                       paste(valid_cols, collapse = ', ')))
      }
    }
    if (is.null(main)) {
      if (audio$filename_base == 'sound') {
        main = ''
      } else {
        main = audio$filename_base
      }
    }

    # we call spectrogram() a second time to get nice silence padding and to add
    # pitch contours internally in spectrogram() - a hassle, but it only takes a
    # few ms, and otherwise it's hard to add pitch contours b/c the y-axis is
    # messed up if spectrogram() calls layout() to add an oscillogram
    try(do.call(.spectrogram, c(list(
      specManual = s,  # so we don't have to redo STFT
      audio = list(
        sound = audio$sound,
        samplingRate = audio$samplingRate,
        scale = audio$scale / m,  # normalize
        ls = audio$ls,
        duration = audio$duration,
        timeShift = audio$timeShift,
        filename = audio$filename,
        filename_base = audio$filename_base
      ),
      dynamicRange = dynamicRange,
      padWithSilence = FALSE,
      plot = TRUE,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      main = main,
      osc = osc,
      internal = list(
        frameBank = frameBank,
        pitch = list(
          pitchCands = pitchCands_list$freq,
          pitchCert = pitchCands_list$cert,
          pitchSource = pitchCands_list$source,
          pitch = result$pitch,
          timestamps = result$time / 1000,  # spetcrogram always plots in s
          candPlot = list(
            dom = pitchDom_plotPars,
            autocor = pitchAutocor_plotPars,
            cep = pitchCep_plotPars,
            spec = pitchSpec_plotPars,
            hps = pitchHps_plotPars,
            zc = pitchZc_plotPars
          ),
          extraContour = cnt,
          extraContour_pars = cnt_plotPars,
          extraContour_warp = isFALSE(cnt_name %in% col_non_Hz),
          pitchPlot = pitchPlot,
          priorMean = priorMean,
          priorSD = priorSD,
          pitchFloor = pitchFloor,
          pitchCeiling = pitchCeiling,
          addToExistingPlot = TRUE,
          showLegend = showLegend,
          ylim = ylim,
          xlab = xlab,
          ylab = ylab,
          main = audio$filename_base,
          timeShift = audio$timeShift
        ))
    ), extraSpecPars))
    )  # end of try()
  }

  if (returnPitchCands) {
    invisible(list(result = result,
                   pitchCands = pitchCands_list,
                   spectrogram = s))
  } else {
    invisible(result)
  }
}
