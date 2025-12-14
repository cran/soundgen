### MAIN FUNCTIONS FOR ACOUSTIC ANALYSIS ###

#' Acoustic analysis
#'
#' Acoustic analysis of one or more sounds: pitch tracking, basic spectral
#' characteristics, formants, estimated loudness (see
#' \code{\link{getLoudness}}), roughness (see \code{\link{modulationSpectrum}}),
#' novelty (see \code{\link{ssm}}), etc. The default values of arguments are
#' optimized for human non-linguistic vocalizations. See
#' vignette('acoustic_analysis', package = 'soundgen') for details. The defaults
#' and reasonable ranges of all arguments can be found in
#' \code{\link{defaults_analyze}}. For high-precision work, first extract and
#' manually correct pitch contours with \code{\link{pitch_app}}, PRAAT, or
#' whatever, and then run \code{analyze(pitchManual = ...)} with these manual
#' contours. For more information, see
#' \url{https://cogsci.se/soundgen/acoustic_analysis.html}
#'
#' Each pitch tracker is controlled by its own list of settings, as follows:
#' \describe{\item{\code{pitchDom} (lowest dominant frequency band)}{\itemize{
#' \item\code{domThres} (0 to 1) to find the lowest dominant frequency band, we
#' do short-term FFT and take the lowest frequency with amplitude at least
#' domThres \item\code{domSmooth} the width of smoothing interval (Hz) for
#' finding \code{dom}}} \item{\code{pitchAutocor} (autocorrelation)}{\itemize{
#' \item \code{autocorThres} voicing threshold (unitless, ~0 to 1)
#' \item\code{autocorSmooth} the width of smoothing interval (in bins) for
#' finding peaks in the autocorrelation function. Defaults to 7 for sampling
#' rate 44100 and smaller odd numbers for lower values of sampling rate \item
#' \code{autocorUpsample} upsamples acf to this resolution (Hz) to improve
#' accuracy in high frequencies \item \code{autocorBestPeak} amplitude of the
#' lowest best candidate relative to the absolute max of the acf }}
#' \item{\code{pitchCep} (cepstrum)}{\itemize{\item \code{cepThres} voicing
#' threshold (unitless, ~0 to 1) \item \code{cepZp} zero-padding of the spectrum
#' used for cepstral pitch detection (final length of spectrum after
#' zero-padding in points, e.g. 2 ^ 13)}} \item{ \code{pitchSpec} (ratio of
#' harmonics - BaNa algorithm)}{\itemize{ \item \code{specThres} voicing
#' threshold (unitless, ~0 to 1) \item \code{specPeak,specHNRslope} when looking
#' for putative harmonics in the spectrum, the threshold for peak detection is
#' calculated as \code{specPeak * (1 - HNR * specHNRslope)} \item specSmooth the
#' width of window for detecting peaks in the spectrum, Hz \item
#' \code{specMerge} pitch candidates within \code{specMerge} semitones are
#' merged with boosted certainty \item \code{specSinglePeakCert} (0 to 1) if F0
#' is calculated based on a single harmonic ratio (as opposed to several ratios
#' converging on the same candidate), its certainty is taken to be
#' \code{specSinglePeakCert}}} \item{ pitchHps (harmonic product
#' spectrum)}{\itemize{\item \code{hpsNum} the number of times to downsample the
#' spectrum \item \code{hpsThres} voicing threshold (unitless, ~0 to 1) \item
#' \code{hpsNorm} the amount of inflation of hps pitch certainty (0 = none)
#' \item \code{hpsPenalty} the amount of penalizing hps candidates in low
#' frequencies (0 = none) }} }  Each of these lists also accepts graphical
#' parameters that affect how pitch candidates are plotted, eg \code{pitchDom =
#' list(domThres = .5, col = 'yellow')}. Other arguments that are lists of
#' subroutine-specific settings include: \describe{ \item{\code{harmonicHeight}
#' (finding how high harmonics reach in the spectrum)}{\itemize{\item
#' \code{harmThres} minimum height of spectral peak, dB \item \code{harmPerSel}
#' the number of harmonics per sliding selection \item \code{harmTol} maximum
#' tolerated deviation of peak frequency from multiples of f0, proportion of f0
#' }} }
#'
#' @seealso \code{\link{pitch_app}} \code{\link{getLoudness}}
#'   \code{\link{segment}} \code{\link{getRMS}}
#'
#' @inheritParams spectrogram
#' @inheritParams getLoudness
#' @param silence (0 to 1 as proportion of max amplitude) frames with RMS
#'   amplitude below \code{silence * max_ampl adjusted by scale} are not
#'   analyzed at all.
#' @param cutFreq if specified, spectral descriptives (peakFreq, specCentroid,
#'   specSlope, and quartiles) are calculated only between \code{cutFreq[1]} and
#'   \code{cutFreq[2]}, Hz. If a single number is given, analyzes frequencies
#'   from 0 to \code{cutFreq}. For ex., when analyzing recordings with varying
#'   sampling rates, set to half the lowest sampling rate to make the spectra
#'   more comparable. Note that "entropyThres" applies only to this frequency
#'   range, which also affects which frames will not be analyzed with
#'   pitchAutocor.
#' @param formants a list of arguments passed to
#'   \code{\link[phonTools]{findformants}} - an external function called to
#'   perform LPC analysis
#' @param nFormants the number of formants to extract per STFT frame (0 = no
#'   formant analysis, NULL = as many as possible)
#' @param loudness a list of parameters passed to \code{\link{getLoudness}} for
#'   measuring subjective loudness, namely \code{SPL_measured, Pref,
#'   spreadSpectrum}. NULL = skip loudness analysis
#' @param roughness a list of parameters passed to
#'   \code{\link{modulationSpectrum}} for measuring roughness. NULL = skip
#'   roughness analysis
#' @param novelty a list of parameters passed to \code{\link{ssm}} for measuring
#'   spectral novelty. NULL = skip novelty analysis
#' @param pitchMethods methods of pitch estimation to consider for determining
#'   pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
#'   'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band, 'hps' =
#'   harmonic product spectrum, NULL = no pitch analysis
#' @param pitchManual manually corrected pitch contour. For a single sound,
#'   provide a numeric vector of any length. For multiple sounds, provide a
#'   dataframe with columns "file" and "pitch" (or path to a csv file) as
#'   returned by \code{\link{pitch_app}}, ideally with the same windowLength and
#'   step as in current call to analyze. A named list with pitch vectors per
#'   file is also OK (eg as returned by pitch_app)
#' @param entropyThres pitch tracking is only performed for frames with Weiner
#'   entropy below \code{entropyThres}, but other spectral descriptives are
#'   still calculated (NULL = analyze everything)
#' @param pitchFloor,pitchCeiling absolute bounds for pitch candidates (Hz)
#' @param priorMean,priorSD specifies the mean (Hz) and standard deviation
#'   (semitones) of gamma distribution describing our prior knowledge about the
#'   most likely pitch values for this file. For ex., \code{priorMean = 300,
#'   priorSD = 6} gives a prior with mean = 300 Hz and SD = 6 semitones (half
#'   an octave). To avoid using any priors, set \code{priorMean = NA, priorAdapt
#'   = FALSE}
#' @param priorAdapt adaptive second-pass prior: if TRUE, optimal pitch contours
#'   are estimated first with a prior determined by \code{priorMean,priorSD}, and
#'   then with a new prior adjusted according to this first-pass pitch contour
#' @param nCands maximum number of pitch candidates per method, normally 1...4
#'   (except for \code{dom}, which returns at most one candidate per frame)
#' @param minVoicedCands minimum number of pitch candidates that have to be
#'   defined to consider a frame voiced (if NULL, defaults to 2 if \code{dom} is
#'   among other candidates and 1 otherwise)
#' @param pitchDom a list of control parameters for pitch tracking using the
#' lowest dominant frequency band or "dom" method; see details and
#' \code{?soundgen:::getDom}
#' @param pitchAutocor a list of control parameters for pitch tracking using the
#'   autocorrelation or "autocor" method; see details and
#'   \code{?soundgen:::getPitchAutocor}
#' @param pitchCep a list of control parameters for pitch tracking using the
#'   cepstrum or "cep" method; see details and \code{?soundgen:::getPitchCep}
#' @param pitchSpec a list of control parameters for pitch tracking using the
#'   BaNa or "spec" method; see details and \code{?soundgen:::getPitchSpec}
#' @param pitchHps a list of control parameters for pitch tracking using the
#'   harmonic product spectrum or "hps" method; see details and
#'   \code{?soundgen:::getPitchHps}
#' @param pitchZc a list of control parameters for pitch tracking based on zero
#'   crossings in bandpass-filtered audio or "zc" method; see
#'   \code{\link{getPitchZc}}
#' @param harmHeight a list of control parameters for estimating how high
#'   harmonics reach in the spectrum; see details and \code{?soundgen:::harmHeight}
#' @param subh a list of control parameters for estimating the strength of
#'   subharmonics per frame - that is, spectral energy at integer ratios of f0:
#'   see \code{?soundgen:::getSHR}
#' @param flux a list of control parameters for calculating feature-based flux
#'   (not spectral flux) passed to \code{\link{getFeatureFlux}}
#' @param amRange target range of frequencies for amplitude modulation, Hz: a
#'   vector of length 2 (affects both \code{amMsFreq} and \code{amEnvFreq})
#' @param fmRange target range of frequencies for analyzing frequency
#'   modulation, Hz (\code{fmFreq}): a vector of length 2
#' @param shortestSyl the smallest length of a voiced segment (ms) that
#'   constitutes a voiced syllable (shorter segments will be replaced by NA, as
#'   if voiceless)
#' @param shortestPause the smallest gap between voiced syllables (ms): large
#'   value = interpolate and merge, small value = treat as separate syllables
#'   separated by a voiceless gap
#' @param interpol a list of parameters (currently \code{win, tol, cert}) passed
#'   to \code{soundgen:::pathfinder} for interpolating missing pitch candidates
#'   (NULL = no interpolation)
#' @param pathfinding method of finding the optimal path through pitch
#'   candidates: 'none' = best candidate per frame, 'fast' = simple heuristic,
#'   'slow' = annealing. See \code{soundgen:::pathfinder}
#' @param annealPars a list of control parameters for postprocessing of
#'   pitch contour with SANN algorithm of \code{\link[stats]{optim}}. This is
#'   only relevant if \code{pathfinding = 'slow'}
#' @param certWeight (0 to 1) in pitch postprocessing, specifies how much we
#'   prioritize the certainty of pitch candidates vs. pitch jumps / the internal
#'   tension of the resulting pitch curve
#' @param snakeStep optimized path through pitch candidates is further
#'   processed to minimize the elastic force acting on pitch contour. To
#'   disable, set \code{snakeStep = 0}
#' @param snakePlot if TRUE, plots the snake
#' @param smooth,smoothVars if \code{smooth} is a positive number, outliers of
#'   the variables in \code{smoothVars} are adjusted with median smoothing.
#'   \code{smooth} of 1 corresponds to a window of ~100 ms and tolerated
#'   deviation of ~4 semitones. To disable, set \code{smooth = 0}
#' @param summaryFun functions used to summarize each acoustic characteristic,
#'   eg "c('mean', 'sd')"; user-defined functions are fine (see examples); NAs
#'   are omitted automatically for mean/median/sd/min/max/range/sum, otherwise
#'   take care of NAs yourself
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range in \code{defaults_analyze}: 'adjust' = reset to default value,
#'   'abort' = stop execution, 'ignore' = throw a warning and continue (may
#'   crash)
#' @param plot if TRUE, produces a spectrogram with pitch contour overlaid
#' @param showLegend if TRUE, adds a legend with pitch tracking methods
#' @param savePlots full path to the folder in which to save the plots (NULL =
#'   don't save, '' = same folder as audio)
#' @param pitchPlot a list of graphical parameters for displaying the final
#'   pitch contour. Set to \code{list(type = 'n')} to suppress
#' @param extraContour name of an output variable to overlap on the pitch
#'   contour plot, eg 'peakFreq' or 'loudness'; can also be a list with extra
#'   graphical parameters, eg \code{extraContour = list(x = 'harmHeight', col =
#'   'red')}
#' @param xlab,ylab,main plotting parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param ... other graphical parameters passed to \code{\link{spectrogram}}
#' @return Returns a list with \code{$detailed} frame-by-frame descriptives and
#'   a \code{$summary} with one row per file, as determined by \code{summaryFun}
#'   (e.g., mean / median / SD of each acoustic variable across all STFT
#'   frames). Output measures include: \describe{\item{duration}{total duration,
#'   s} \item{duration_noSilence}{duration from the beginning of the first
#'   non-silent STFT frame to the end of the last non-silent STFT frame, s (NB:
#'   depends strongly on \code{windowLength} and \code{silence} settings)}
#'   \item{time}{time of the middle of each frame (ms)}
#'   \item{amEnvFreq,amEnvDep}{frequency (Hz) and
#'   depth (0 to 1) of amplitude modulation estimated from a smoothed amplitude
#'   envelope} \item{amMsFreq,amMsPurity}{frequency and purity of amplitude
#'   modulation estimated via \code{\link{modulationSpectrum}}}
#'   \item{ampl}{root mean square of amplitude per frame, calculated as
#'   sqrt(mean(frame ^ 2))} \item{ampl_noSilence}{same as \code{ampl}, but
#'   ignoring silent frames} \item{CPP}{Cepstral Peak Prominence, dB (a measure
#'   of pitch quality, the ratio of the highest peak in the cepstrum to the
#'   regression line drawn through it)} \item{dom}{lowest dominant frequency
#'   band (Hz) (see "Pitch tracking methods / Dominant frequency" in the
#'   vignette)} \item{entropy}{Weiner entropy of the spectrum of the current
#'   frame. Close to 0: pure tone or tonal sound with nearly all energy in
#'   harmonics; close to 1: white noise} \item{entropySh}{Normalized Shannon
#'   entropy of the spectrum of the current frame: 0 = pure tone, 1 = white
#'   noise} \item{f1_freq, f1_width, ...}{the frequency and bandwidth of the
#'   first nFormants formants per STFT frame, as calculated by
#'   phonTools::findformants} \item{flux}{feature-based flux, the rate of change
#'   in acoustic features such as pitch, HNR, etc. (0 = none, 1 = max); "epoch"
#'   is an audio segment between two peaks of flux that exceed a threshold of
#'   \code{flux = list(thres = ...)} (listed in output$detailed only)}
#'   \item{fmFreq}{frequency of frequency modulation (FM) such as vibrato or
#'   jitter, Hz} \item{fmDep}{depth of FM, semitones} \item{fmPurity}{purity or
#'   dominance of the main FM frequency (fmFreq), 0 to 1} \item{harmEnergy}{the
#'   amount of energy in upper harmonics, namely the ratio of total spectral
#'   mass above 1.25 x F0 to the total spectral mass below 1.25 x F0 (dB)}
#'   \item{harmHeight}{how high harmonics reach in the spectrum, based on the
#'   best guess at pitch (or the manually provided pitch values)}
#'   \item{HNR}{harmonics-to-noise ratio (dB), a measure of harmonicity returned
#'   by soundgen:::getPitchAutocor (see "Pitch tracking methods /
#'   Autocorrelation"). If HNR = 0 dB, there is as much energy in harmonics as
#'   in noise} \item{loudness}{subjective loudness, in sone, corresponding to
#'   the chosen SPL_measured - see \code{\link{getLoudness}}}
#'   \item{novelty}{spectral novelty - a measure of how variable the spectrum is
#'   on a particular time scale, as estimated by \code{\link{ssm}}}
#'   \item{peakFreq}{the frequency with maximum spectral power (Hz)}
#'   \item{pitch}{post-processed pitch contour based on all F0 estimates}
#'   \item{quartile25, quartile50, quartile75}{the 25th, 50th, and 75th
#'   quantiles of the spectrum of voiced frames (Hz)} \item{roughness}{the
#'   amount of amplitude modulation, see modulationSpectrum and Anikin 2025}
#'   \item{specCentroid}{the center of gravity of the frame’s spectrum, first
#'   spectral moment (Hz)} \item{specSlope}{the slope of linear regression fit
#'   to the spectrum below cutFreq (dB/kHz)} \item{subDep}{estimated depth of
#'   subharmonics per frame: 0 = none, 1 = as strong as f0. NB: this depends
#'   critically on accurate pitch tracking} \item{subRatio}{the ratio of f0 to
#'   subharmonics frequency with strength subDep: 2 = period doubling, 3 = f0 /
#'   3, etc.} \item{voiced}{is the current STFT frame voiced? TRUE / FALSE}
#' }
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
#' # Take a sound hard to analyze b/c of subharmonics and jitter
#' sound2 = soundgen(sylLen = 900, pitch = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noise = list(time = c(0, 900), value = c(-40, -20)),
#'   subDep = 10, jitterDep = 0.5,
#'   temperature = 0.001, samplingRate = 44100, pitchSamplingRate = 44100)
#' # playme(sound2, 44100)
#' a2 = analyze(sound2, samplingRate = 44100, priorSD = 24,
#'              plot = TRUE, ylim = c(0, 5))
#'
#' # Compare the available pitch trackers
#' analyze(sound2, 44100,
#'   pitchMethods = c('dom', 'autocor', 'spec', 'cep', 'hps', 'zc'),
#'   # don't use priors to see weird pitch candidates better
#'   priorMean = NA, priorAdapt = FALSE,
#'   plot = TRUE, yScale = 'bark')
#'
#' # Fancy plotting options:
#' a = analyze(sound2, samplingRate = 44100, plot = TRUE,
#'   xlab = 'Time, ms', colorTheme = 'seewave', yScale = 'ERB',
#'   contrast = .5, ylim = c(0, 4), main = 'My plot',
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
#'   savePlots = '',  # save in the same folder as audio
#'   showLegend = TRUE, yScale = 'bark',
#'   width = 20, height = 12,
#'   units = 'cm', res = 300, ylim = c(0, 5),
#'   cores = 4)  # use multiple cores to speed up processing
#' s2$summary[, 1:5]
#'
#' # Different options for summarizing the output
#' a = analyze(sound2, 44100,
#'             summaryFun = c('mean', 'range'))
#' a$summary  # one row per sound
#' # ...with custom summaryFun, eg time of peak relative to duration (0 to 1)
#' timePeak = function(x) which.max(x) / length(x)  # without omitting NAs
#' timeTrough = function(x) which.min(x) / length(x)
#' a = analyze(sound2, samplingRate = 16000,
#'             summaryFun = c('mean', 'timePeak', 'timeTrough'))
#' colnames(a$summary)
#'
#' # Analyze a selection rather than the whole sound
#' a = analyze(sound, samplingRate = 16000, from = .1, to = .3, plot = TRUE)
#'
#' # Use only a range of frequencies when calculating spectral descriptives
#' # (ignore everything below 100 Hz and above 8000 Hz as irrelevant noise)
#' a = analyze(sound, samplingRate = 16000, cutFreq = c(100, 8000))
#'
#' ## Amplitude and loudness: analyze() should give the same results as
#' # dedicated functions getRMS() / getLoudness()
#' # Create 1 kHz tone
#' samplingRate = 16000; dur_ms = 50
#' sound3 = sin(2*pi*1000/samplingRate*(1:(dur_ms/1000*samplingRate)))
#' a1 = analyze(sound3, samplingRate = samplingRate, scale = 1,
#'              windowLength = 25, overlap = 50,
#'              loudness = list(SPL_measured = 40),
#'              pitchMethods = NULL, plot = FALSE)
#' a1$detailed$loudness  # loudness per STFT frame (1 sone by definition)
#' getLoudness(sound3, samplingRate = samplingRate, windowLength = 25,
#'             overlap = 50, SPL_measured = 40, scale = 1)$loudness
#' a1$detailed$ampl  # RMS amplitude per STFT frame
#' getRMS(sound3, samplingRate = samplingRate, windowLength = 25,
#'        overlap = 50, scale = 1)$detailed
#' # or even simply: sqrt(mean(sound3 ^ 2))
#'
#' # The same sound as above, but with half the amplitude
#' a_half = analyze(sound3 / 2, samplingRate = samplingRate, scale = 1,
#'                  windowLength = 25, overlap = 50,
#'                  loudness = list(SPL_measured = 40),
#'                  pitchMethods = NULL, plot = FALSE)
#' a1$detailed$ampl / a_half$detailed$ampl  # rms amplitude halved
#' a1$detailed$loudness/ a_half$detailed$loudness
#' # loudness is not a linear function of amplitude
#'
#' # Analyzing ultrasounds (slow but possible, just adjust pitchCeiling)
#' s = soundgen(sylLen = 100, addSilence = 10,
#'   pitch = c(25000, 35000, 30000),
#'   formants = NA, rolloff = -12, rolloffKHz = 0,
#'   pitchSamplingRate = 350000, samplingRate = 350000, windowLength = 5,
#'   pitchCeiling = 45000, invalidArgAction = 'ignore',
#'   plot = TRUE)
#' # s is a bat-like ultrasound inaudible to humans
#' a = analyze(
#'   s, 350000, plot = TRUE,
#'   pitchFloor = 10000, pitchCeiling = 90000, priorMean = NA,
#'   pitchMethods = c('autocor', 'spec'),
#'   # probably shouldn't use pitchMethod = "dom" b/c of likely low-freq noise
#'   windowLength = 5, step = 2.5,
#'   shortestSyl = 10, shortestPause = 10,  # again, very short sounds
#'   interpol = list(win = 10),  # again, very short sounds
#'   smooth = 0.1,  # might need less smoothing if very rapid f0 changes
#'   nFormants = 0, loudness = NULL, roughness = NULL, novelty = NULL)
#' # NB: ignore formants and loudness estimates for such non-human sounds
#'
#' # download 260 sounds from Anikin & Persson (2017)
#' # http://cogsci.se/publications/anikin-persson_2017_nonlinguistic-vocs/260sounds_wav.zip
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp'  # 260 .wav files live here
#' s = analyze(myfolder)  # ~ 10-20 minutes!
#' # s = write.csv(s, paste0(myfolder, '/temp.csv'))  # save a backup
#'
#' # Check accuracy: import manually verified pitch values (our "key")
#' # pitchManual   # "ground truth" of mean pitch per sound
#' # pitchContour  # "ground truth" of complete pitch contours per sound
#' files_manual = paste0(names(pitchManual), '.wav')
#' idx = match(s$file, files_manual)  # in case the order is wrong
#' s$key = pitchManual[idx]
#'
#' # Compare manually verified mean pitch with the output of analyze:
#' cor(s$key, s$summary$pitch_median, use = 'pairwise.complete.obs')
#' plot(s$key, s$summary$pitch_median, log = 'xy')
#' abline(a=0, b=1, col='red')
#'
#' # Re-running analyze with manually corrected contours gives correct
#' pitch-related descriptives like amplVoiced and harmonics (NB: you get it "for
#' free" when running pitch_app)
#' s1 = analyze(myfolder, pitchManual = pitchContour)
#' plot(s$summary$harmonics_median, s1$summary$harmonics_median)
#' abline(a=0, b=1, col='red')
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
    step = windowLength / 2,
    overlap = 50,
    specType = c('spectrum', 'reassign', 'spectralDerivative')[1],
    wn = 'gaussian',
    zp = 0,
    cutFreq = NULL,
    nFormants = 3,
    formants = list(),
    loudness = list(SPL_measured = 70),
    roughness = list(msType = '1D', windowLength = 25, step = 2, amRes = 5),
    novelty = list(input = 'melspec', kernelLen = 100),
    pitchMethods = c('dom', 'autocor'),
    pitchManual = NULL,
    entropyThres = 0.6,
    pitchFloor = 75,
    pitchCeiling = 3500,
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
                harmTol = 0.25,
                amRange = c(10, 200)),
    flux = list(thres = 0.15,
                smoothWin = 100),
    amRange = c(10, 200),
    fmRange = c(5, 1000 / step / 2),
    shortestSyl = 20,
    shortestPause = 60,
    interpol = list(win = 75, tol = 0.3, cert = 0.3),
    pathfinding = c('none', 'fast', 'slow')[2],
    annealPars = list(maxit = 5000, temp = 1000),
    certWeight = .5,
    snakeStep = 0,
    snakePlot = FALSE,
    smooth = 1,
    smoothVars = c('pitch', 'dom'),
    summaryFun = c('mean', 'median', 'sd'),
    invalidArgAction = c('adjust', 'abort', 'ignore')[1],
    reportEvery = NULL,
    cores = 1,
    plot = FALSE,
    osc = 'linear',
    showLegend = TRUE,
    savePlots = NULL,
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
  ## Validate the parameter values that do not depend on sound-specific
  ## characteristics like samplingRate and duration
  # Check simple numeric default pars
  simplePars = c('silence', 'entropyThres', 'domThres',
                 'autocorThres', 'autocorSmooth',
                 'cepThres',
                 'specThres', 'specPeak',
                 'specSinglePeakCert', 'certWeight')
  for (p in simplePars) {
    gp = try(get(p), silent = TRUE)
    if (!inherits(gp, "try-error")) {
      if (is.numeric(gp)) {
        assign(noquote(p),
               validatePars(p, gp, def = defaults_analyze,
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
        parGroup_user[p] = defaults_analyze[p, 'default']
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
  if (is.na(pitchSpec$specMethod)) pitchSpec$specMethod = 'commonFactor'
  if (is.na(pitchAutocor$interpol)) pitchAutocor$interpol = 'sinc'


  # Check defaults that depend on other pars or require customized warnings
  if (is.character(pitchMethods) && pitchMethods[1] != '') {
    valid_names = c('dom', 'autocor', 'cep', 'spec', 'hps', 'zc')
    invalid_names = pitchMethods[!pitchMethods %in% valid_names]
    if (length(invalid_names) > 0) {
      message(paste('Ignoring unknown pitch tracking methods:',
                    paste(invalid_names, collapse = ', '),
                    '; valid pitchMethods:',
                    paste(valid_names, collapse = ', ')))
    }
    rm('valid_names', 'invalid_names')
  }
  if (is.numeric(priorSD)) {
    if (priorSD <= 0) {
      priorSD = 6
      warning('"priorSD" must be positive; defaulting to 6 semitones')
    }
  }
  if (!is.numeric(nCands) | nCands < 1) {
    nCands = 1
    warning('"nCands" must be a positive integer; defaulting to 1')
  } else if (!is.integer(nCands)) {
    nCands = round(nCands)
  }
  if (!is.numeric(shortestSyl) | shortestSyl < 0) {
    shortestSyl = 0
    warning('shortestSyl must be non-negative; defaulting to 0 ms')
  }
  if (!is.numeric(shortestPause) | shortestPause < 0) {
    shortestPause = 0
    warning('shortestPause must be a non-negative number; defaulting to 0 ms')
  }

  # Check non-numeric defaults
  supported_wn = c('bartlett', 'blackman', 'flattop', 'gaussian',
                   'hamming', 'hanning', 'rectangle')
  if (!wn %in% supported_wn) {
    wn = 'gaussian'
    warning(paste('Implemented "wn":',
                  paste(supported_wn, collapse = ', '),
                  '. Defaulting to "gaussian"'))
  }
  if (!pathfinding %in% c('none', 'fast', 'slow')) {
    pathfinding = 'fast'
    warning(paste('Implemented "pathfinding": "none", "fast", "slow";',
                  'defaulting to "fast"'))
  }
  rm(supported_wn)

  # reformat pitchManual, if any
  if (!is.null(pitchManual)) {
    pitchManual_list = formatPitchManual(pitchManual)
  } else {
    pitchManual_list = NULL
  }

  # reformat loudness/novelty etc lists, if any
  if (!is.null(novelty)) {
    if (is.null(novelty$windowLength)) novelty$windowLength = windowLength
    if (is.null(novelty$step)) novelty$step = step
  }
  if (!is.null(loudness)) {
    if (is.null(loudness$SPL_measured)) loudness$SPL_measured = 70
    if (is.null(loudness$Pref)) loudness$Pref = 2e-5
    if (is.null(loudness$spreadSpectrum)) loudness$spreadSpectrum = TRUE
  }
  if (!is.null(interpol)) {
    if (is.null(interpol$win)) interpol$win = 75
    if (is.null(interpol$tol)) interpol$tol = .3
    if (is.null(interpol$cert)) interpol$cert = .3
  }
  if (is.null(flux$thres)) flux$thres = 0.15
  if (is.null(flux$smoothWin)) flux$smoothWin = 100

  # match args
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'reportEvery', 'cores',
    'savePlots', 'pitchPlot', 'pitchManual', 'summaryFun', 'invalidArgAction',
    'loudness', 'roughness', 'novelty', 'subh')]
  # add plot pars correctly, without flattening the lists
  list_pars = c('pitchManual_list', 'pitchPlot',
                'pitchDom_plotPars', 'pitchAutocor_plotPars',
                'pitchCep_plotPars', 'pitchSpec_plotPars',
                'pitchHps_plotPars', 'pitchZc_plotPars',
                'loudness', 'roughness', 'novelty', 'interpol', 'subh')
  for (lp in list_pars) myPars[[lp]] = get(lp)

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.analyze',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "analyze", width = paste0(width, units)))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          pa$result[[i]],
          summaryFun = summaryFun,
          var_noSummary = c('duration', 'duration_noSilence', 'voiced', 'time', 'epoch'))
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
                          data.table::rbindlist(temp, fill = TRUE)))
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
#' @keywords internal
.analyze = function(
    audio,
    dynamicRange = 80,
    silence = 0.04,
    windowLength = 50,
    step = windowLength / 2,
    overlap = 50,
    specType = c('spectrum', 'reassign', 'spectralDerivative')[1],
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
    entropyThres = 0.6,
    pitchFloor = 75,
    pitchCeiling = 3500,
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
                        autocorBestPeak = 0.975),
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
                harmTol = 0.25,
                amRange = c(10, 200)),
    flux = list(thres = 0.15,
                smoothWin = 100),
    amRange = c(10, 200),
    fmRange = c(5, 1000 / step / 2),
    shortestSyl = 20,
    shortestPause = 60,
    interpol = NULL,
    pathfinding = c('none', 'fast', 'slow')[2],
    annealPars = list(maxit = 5000, temp = 1000),
    certWeight = .5,
    snakeStep = 0,
    snakePlot = FALSE,
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
  ## Validate the parameter values that do not depend on sound-specific
  ## characteristics like samplingRate and duration
  if (!is.numeric(windowLength) | windowLength <= 0 |
      windowLength > (audio$duration / 2 * 1000)) {
    windowLength = min(50, round(audio$duration / 2 * 1000))
    step = windowLength / 2
    warning(paste0(
      '"windowLength" must be between 0 and half the sound duration (in ms);
            resetting to ', windowLength, ' ms')
    )
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  # to ensure that the window length in points is a power of 2, say 2048 or 1024:
  # 2^round(log(windowLength*audio$samplingRate/1000)/log(2), 0)

  if (!is.numeric(step)) {
    if (!is.numeric(overlap) | overlap < 0 | overlap > 99) {
      overlap = 50
      warning('If "step" is not specified, overlap must be between 0 and 99%',
              '; overlap reset to 50%')
    } else {
      step = windowLength * (1 - overlap / 100)
    }
  } else {
    if (is.numeric(overlap) & overlap != 50) {  # step specified, overlap != default
      message('"overlap" is ignored if "step" is not NULL')
    }
  }
  if (step <= 0 | step > (audio$duration * 1000)) {
    step = windowLength / 2
    warning('"step" must be between 0 and sound_duration ms;
            defaulting to windowLength / 2')
  }
  if (step > windowLength)
    warning(paste('"step" should normally not be larger than "windowLength" ms:',
                  'you are skipping parts of the sound!'))

  if (shortestPause < step) {
    warning(paste0('shortestPause (', shortestPause,
                   ' ms) is shorter than one STFT step (', step, ' ms)',
                   '; setting shortestPause = ', 1.5 * step, ' ms'))
    shortestPause = 1.5 * step
  }
  if (shortestPause < 1000 / pitchFloor) {
    warning(paste0(
      'shortestPause (', shortestPause,
      ' ms) is shorter than one glottal cycle = 1000 / pitchFloor (',
      round(1000 / pitchFloor), ' ms); setting shortestPause = ',
      round(1.5 * 1000 / pitchFloor), ' ms'))
    shortestPause = round(1.5 * 1000 / pitchFloor)
  }
  if (!is.numeric(zp)) {
    zp = 0
  } else if (zp < 0) {
    zp = 0
    warning('"zp" must be non-negative; defaulting to 0')
  }
  if (!is.null(cutFreq) && !any(is.na(cutFreq))) {
    # a single value refers to upper end of the analyzed frequency range
    if (length(cutFreq) == 1) cutFreq = c(0, cutFreq)
    if (is.na(cutFreq[1]) || cutFreq[1] <= 0) cutFreq[1] = 0
    if (is.na(cutFreq[2]) || cutFreq[2] > (audio$samplingRate / 2)) {
      cutFreq[2] = audio$samplingRate / 2
      warning(paste('"cutFreq" should not be above Nyquist (samplingRate / 2);',
                    'setting cutFreq = NULL'))
    }
  }
  if (!is.numeric(pitchFloor) | pitchFloor <= 0 |
      pitchFloor > audio$samplingRate / 2) {
    pitchFloor = 1
    warning(paste('"pitchFloor" must be between 0 and pitchCeiling;',
                  'defaulting to 1 Hz'))
  } # 1 Hz ~ 4 octaves below C0
  if (!is.numeric(pitchCeiling) | pitchCeiling > audio$samplingRate / 2) {
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
  if (is.numeric(priorMean)) {
    if (priorMean > audio$samplingRate / 2 | priorMean <= 0) {
      priorMean = 300
      warning(paste('"priorMean" must be between 0 and Nyquist;',
                    'defaulting to 300; set to NULL to disable prior'))
    }
  }
  if (!is.null(interpol)) {
    if (shortestPause > 0 & interpol$win > 0) {
      if (interpol$win * step < shortestPause / 2) {
        interpol$win = ceiling(shortestPause / 2 / step)
        warning(paste(
          '"interpol$win" reset to', interpol$win,
          ': interpolation must be able to bridge merged voiced fragments'))
      }
    }
    if (interpol$tol <= 0) {
      interpol$tol = 0.3
      warning('"interpol$tol" must be positive; defaulting to 0.3')
    }
  }

  if (!is.numeric(pitchAutocor$autocorSmooth)) {
    pitchAutocor$autocorSmooth = 2 * ceiling(7 * audio$samplingRate / 44100 / 2) - 1
    # width of smoothing interval, chosen to be proportionate to samplingRate (7
    # for samplingRate 44100), but always an odd number.
    # for(i in seq(16000, 60000, length.out = 10)) {
    #   print(paste(round(i), ':', 2 * ceiling(7 * i / 44100 / 2) - 1))
    # }
  }


  ## NORMALIZATION
  # calculate scaling coefficient for loudness calculation, but don't convert
  # yet, since most routines in analyze() require scale [-1, 1]
  if (!is.null(loudness)) {
    ldns = try(do.call(.getLoudness, c(list(
      audio = audio[which(names(audio) != 'savePlots')],
      windowLength = windowLength, step = step, plot = FALSE),
      loudness))$loudness)
  } else {
    scaleCorrection = NA
  }

  # Adjust silence threshold as proportion of the observed max ampl
  # (has the effect of looking for voiced segments even in very quiet files)
  m = max(abs(audio$sound))
  m_to_scale = m / audio$scale
  silence = silence * m_to_scale
  if (m_to_scale < silence)
    message('The audio is too quiet: max ampl is lower than silence = ', silence)

  if (!is.null(loudness)) {  # if analyzing loudness
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
  audio$sound = audio$sound / max(abs(audio$sound))


  ## ANALYSIS
  # Set up filter for calculating pitchAutocor
  # filter = seewave::ftwindow(2 * windowLength_points, wn = wn) # plot(filter, type='l')
  # powerSpectrum_filter = abs(fft(filter)) ^ 2
  # autoCorrelation_filter = abs(fft(powerSpectrum_filter, inverse = TRUE)) ^ 2
  # autoCorrelation_filter = autoCorrelation_filter[1:windowLength_points]
  # autoCorrelation_filter = autoCorrelation_filter / max(autoCorrelation_filter)

  filter = seewave::ftwindow(windowLength_points, wn = wn) # plot(filter, type='l')
  sp_filter = fft(filter)
  powerSpectrum_filter = Re(sp_filter * Conj(sp_filter))
  autoCorrelation_filter = Re(fft(powerSpectrum_filter, inverse = TRUE))
  autoCorrelation_filter = autoCorrelation_filter[seq_len(windowLength_points / 2)]
  autoCorrelation_filter = autoCorrelation_filter / autoCorrelation_filter[1] # /max(autoCorrelation_filter)
  # plot(autoCorrelation_filter, type = 'l')

  ## fft and acf per frame
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_analyze.png"),
        width = width, height = height, units = units, res = res)
  }
  frameBank = getFrameBank(
    sound = audio$sound,
    samplingRate = audio$samplingRate,
    windowLength_points = windowLength_points,
    wn = wn,
    step = step,
    zp = zp,
    normalize = TRUE,
    filter = NULL,
    padWithSilence = FALSE,
    timeShift = audio$timeShift
  )
  timestamps = as.numeric(colnames(frameBank))

  extraSpecPars = list(...)
  extraSpecPars$osc = NULL
  s = try(do.call(.spectrogram, c(list(
    audio = audio[names(audio) != 'savePlots'], # otherwise spectrogram() plots
    internal = list(frameBank = frameBank),
    dynamicRange = dynamicRange,
    windowLength = windowLength,
    specType = specType,
    zp = zp,
    wn = wn,
    step = step,
    normalize = FALSE,
    output = 'original',
    padWithSilence = FALSE,
    plot = FALSE
  ), extraSpecPars)))
  if (inherits(s, 'try-error')) return(NA)
  # image(t(s))
  bin = audio$samplingRate / 2 / nrow(s)  # width of spectral bin, Hz
  freqs = as.numeric(rownames(s)) * 1000  # central bin freqs, Hz
  # specFlux = getSpectralFlux(s)  # spectral flux - not very useful, IMHO

  # calculate rms amplitude of each frame
  myseq = (timestamps - audio$timeShift * 1000 - windowLength / 2) *
    audio$samplingRate / 1000 + 1
  myseq[1] = 1  # just in case of rounding errors
  l = length(myseq)
  myseq[l] = min(myseq[l], audio$ls - windowLength_points)
  # perceived intensity - root mean square of amplitude
  # (NB: m / scale corrects the scale back to original, otherwise sound is [-1, 1])
  ampl = vapply(myseq, function(x) {
    sqrt(mean((audio$sound[x:(x + windowLength_points - 1)] *
                 m / audio$scale) ^ 2, na.rm = TRUE))
  }, numeric(1))

  # calculate entropy of each frame within the most relevant
  # vocal range only (up to to cutFreq Hz)
  rowLow = 1 # which(as.numeric(rownames(s)) > 0.05)[1] # 50 Hz
  if (!is.null(cutFreq)) {
    rowHigh = tail(which(freqs <= cutFreq[2]), 1) # 6000 Hz etc
  } else {
    rowHigh = nrow(s)
  }
  if (length(rowHigh) < 1 | !is.finite(rowHigh)) rowHigh = nrow(s)
  entropy = apply(s[rowLow:rowHigh, ], 2, getEntropy, type = 'weiner')
  entropySh = apply(s[rowLow:rowHigh, ], 2, getEntropy, type = 'weiner', normalize = TRUE)
  # if the frame is too quiet or too noisy, we will not analyze it
  cond_silence = ampl >= silence & colSums(s) > 0
  # (need both b/c s frames are not 100% synchronized with ampl frames)
  # cond_silence[is.na(cond_silence)] = FALSE  # just in case of weird NAs
  framesToAnalyze = which(cond_silence)
  if (!is.numeric(entropyThres)) entropyThres = Inf
  cond_entropy = cond_silence & entropy < entropyThres
  cond_entropy[is.na(cond_entropy)] = FALSE

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
  autocorBank = matrix(NA, nrow = length(autoCorrelation_filter),
                       ncol = ncol(frameBank))
  for (i in which(cond_entropy)) {
    # acf is ~10 times slower than FFT
    # autocorBank[, i] = acf(frameBank[, i],  # or frame^2
    #                        windowLength_points,
    #                        plot = FALSE)$acf / autoCorrelation_filter
    sp_fr = fft(frameBank[, i])
    powerSpectrum_fr = Re(sp_fr * Conj(sp_fr))
    # Im * conj(Im) is the same as abs(fft())^2, but faster
    autoCorrelation_fr = Re(fft(powerSpectrum_fr, inverse = TRUE))
    autoCorrelation_fr = autoCorrelation_fr[seq_len(windowLength_points/2)]
    autoCorrelation_fr = autoCorrelation_fr / autoCorrelation_fr[1] # /max(autoCorrelation_fr)
    # plot(autoCorrelation_fr, type = 'l')

    autocorBank[, i] = autoCorrelation_fr / autoCorrelation_filter
    # plot(autocorBank[, i], type = 'l')
    # plot(rownames(autocorBank), autocorBank[, i], type = 'l', log = 'x')
  }
  autocorBank = autocorBank[-1, ]  # b/c it starts with zero lag (identity)
  rownames(autocorBank) = audio$samplingRate / (seq_len(nrow(autocorBank)))
  # plot(rownames(s)[1:50], s[1:50, 8], type = 'l')
  # plot(frameBank[, 8], type = 'l')
  # plot(names(autocorBank[1:25, 8]), autocorBank[1:25, 8], type = 'l')


  ## FORMANTS
  fmts = NULL
  no_formants = FALSE
  if (is.null(nFormants)) nFormants = 100
  # try one frame to see how many formants are returned
  fmts_list = vector('list', length = nf)
  if (nFormants > 0 & nf > 0) {
    # we don't really know how many formants will be returned by phonTools, so
    # we save everything at first, and then trim to nFormants
    for (i in seq_len(nf)) {
      fmts_list[[i]] = try(suppressWarnings(do.call(
        phonTools::findformants,
        c(list(frameBank[, framesToAnalyze[i]],
               fs = audio$samplingRate, verify = FALSE),
          formants))),
        silent = TRUE)
      if (inherits(fmts_list[[i]], 'try-error')) {
        fmts_list[[i]] = data.frame(formant = NA, bandwidth = NA)[-1, ]
      }
    }
    # check how many formants we will/can save
    nFormants_avail = min(nFormants, max(sapply(fmts_list, nrow)))
    if (nFormants_avail > 0) {
      nFormants = nFormants_avail
      availableRows = seq_len(nFormants)
      fmts = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
      colnames(fmts) = paste0('f', rep(availableRows, each = 2),
                              rep(c('_freq', '_width'), nFormants))
      # iterate through the full formant list and save what's needed
      for (i in seq_len(nf)) {
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
    availableRows = seq_len(nFormants)
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
      'specSlope' = NA
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
      trackPitch = cond_entropy[i],
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
  result$entropy = entropy
  result$entropySh = entropySh
  result$ampl = result$ampl_noSilence = ampl
  result$ampl_noSilence[-framesToAnalyze] = NA
  result$time = as.numeric(colnames(frameBank))
  result$duration_noSilence = duration_noSilence
  result$duration = audio$duration
  nc = ncol(result); nr = nrow(result)

  # add loudness
  if (exists('ldns') && !inherits(ldns, 'try-error')) {
    result$loudness = .resample(list(sound = ldns), len = nr,
                                lowPass = FALSE, plot = FALSE)
  } else {
    result$loudness = NA
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
    pitch_zc_cnt = getSmoothContour(
      data.frame(time = pitch_zc$time, value = pitch_zc$pitch),
      len = nr, interpol = 'approx', NA_to_zero = FALSE, discontThres = 0)
    pitch_zc_cnt[-framesToAnalyze] = NA
    # plot(pitch_zc_cnt, type = 'l')
    pitch_zc_cert = getSmoothContour(
      data.frame(time = pitch_zc$time, value = pitch_zc$cert),
      len = nr, interpol = 'approx', NA_to_zero = FALSE, discontThres = 0)
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
    nrow(y[['pitchCands_frame']]))))
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
      dimnames = list(seq_len(max_cands), result$time)
    )), 3)
    names(pitchCands_list) = c('freq', 'cert', 'source')
    for (i in seq_along(frameInfo)) {
      temp = frameInfo[[i]]$pitchCands_frame
      n = nrow(temp)
      if (n > 0) {
        seqn = seq_len(n)
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
    if (is.numeric(priorMean) & is.numeric(priorSD)) {
      pitchCert_multiplier = getPrior(priorMean = priorMean,
                                      priorSD = priorSD,
                                      pitchFloor = pitchFloor,
                                      pitchCeiling = pitchCeiling,
                                      pitchCands = pitchCands_list$freq,
                                      plot = FALSE)
      pitchCands_list$cert = pitchCands_list$cert * pitchCert_multiplier
    }

    # for each syllable, impute NA's and find a nice path through pitch candidates
    pitchFinal = rep(NA, ncol(pitchCands_list$freq))
    if (nrow(voicedSegments) > 0) {
      # if we have found at least one putatively voiced syllable
      for (syl in seq_len(nrow(voicedSegments))) {
        myseq = voicedSegments$segmentStart[syl]:voicedSegments$segmentEnd[syl]
        # compute the optimal path through pitch candidates
        pitchFinal[myseq] = pathfinder(
          pitchCands = pitchCands_list$freq[, myseq, drop = FALSE],
          pitchCert = pitchCands_list$cert[, myseq, drop = FALSE],
          pitchSource = pitchCands_list$source[, myseq, drop = FALSE],
          step = step,
          certWeight = certWeight,
          pathfinding = pathfinding,
          annealPars = annealPars,
          interpolWin_bin = ceiling(interpol$win / step),
          interpolTol = interpol$tol,
          interpolCert = interpol$cert,
          snakeStep = snakeStep,
          snakePlot = snakePlot
        )
      }
    }

    # second pass with adaptive prior
    if (priorAdapt) {
      # revert to original pitchCert
      if (exists('pitchCert_multiplier'))
        pitchCands_list$cert = pitchCands_list$cert / pitchCert_multiplier
      if (any(!is.na(pitchFinal))) {
        pitch_sem = HzToSemitones(pitchFinal[!is.na(pitchFinal)])
        priorMean = semitonesToHz(mean(pitch_sem))
        new_sd = sd(pitch_sem)
        if (is.finite(new_sd))
          priorSD = semitonesToHz(new_sd) * 4
        pitchCert_multiplier2 = getPrior(priorMean = priorMean,
                                         priorSD = priorSD,
                                         pitchFloor = pitchFloor,
                                         pitchCeiling = pitchCeiling,
                                         pitchCands = pitchCands_list$freq,
                                         plot = FALSE)
        pitchCands_list$cert = pitchCands_list$cert * pitchCert_multiplier2
      }

      pitchFinal = rep(NA, ncol(pitchCands_list$freq))
      if (nrow(voicedSegments) > 0) {
        # if we have found at least one putatively voiced syllable
        for (syl in seq_len(nrow(voicedSegments))) {
          myseq = voicedSegments$segmentStart[syl]:voicedSegments$segmentEnd[syl]
          # compute the optimal path through pitch candidates
          pitchFinal[myseq] = pathfinder(
            pitchCands = pitchCands_list$freq[, myseq, drop = FALSE],
            pitchCert = pitchCands_list$cert[, myseq, drop = FALSE],
            pitchSource = pitchCands_list$source[, myseq, drop = FALSE],
            step = step,
            certWeight = certWeight,
            pathfinding = pathfinding,
            annealPars = annealPars,
            interpolWin_bin = ceiling(interpol$win / step),
            interpolTol = interpol$tol,
            interpolCert = interpol$cert,
            snakeStep = snakeStep,
            snakePlot = snakePlot
          )
        }
      }
    }

    # save optimal pitch track and the best candidates separately for
    # each pitch tracking method
    result$pitch = pitchFinal # optimal pitch track
    if (!is.null(pitchNames$pitchMethod)) {
      for (p in seq_len(nrow(pitchNames))) {
        result[pitchNames$pitchName[p]] =  as.numeric(lapply(frameInfo, function(x) {
          idx_method = which(x$pitchCands_frame$pitchSource == pitchNames$pitchMethod[p])
          idx_maxCert = which.max(x$pitchCands_frame$pitchCert[idx_method])
          return(x$pitchCands_frame$pitchCand[idx_method] [idx_maxCert])
        }))
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
    keep = apply(result[, smoothVars], 2, function(x) any(!is.na(x)))
    smoothVars = smoothVars[keep]
    if (length(smoothVars) > 0) {
      result[, smoothVars] = medianSmoother(
        result[, smoothVars, drop = FALSE],
        smoothing_ww = smoothing_ww,
        smoothingThres = smoothingThres
      )
    }
  }

  # Convert HNR to dB
  result$HNR = to_dB(result$HNR)

  ## Finalize / update results using the final pitch contour
  # (or the manual pitch contour, if provided)
  pitch_true = result$pitch
  if (!is.null(pitchManual_list)) {
    if (length(pitchManual_list) == 1) {
      pitch_raw = pitchManual_list[[1]]
    } else {
      pitch_raw = pitchManual_list[[audio$filename_base]]
    }
    if (!is.null(pitch_raw)) {
      # up/downsample pitchManual to the right length
      pitch_true = .resample(
        list(sound = pitch_raw),
        len = nr,
        lowPass = FALSE,
        plot = FALSE)
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
    result[, c('roughness', 'amMsFreq', 'amMsPurity')] = NA
  } else {
    ms = do.call(.modulationSpectrum, c(
      list(audio = audio[c('sound', 'samplingRate', 'ls', 'duration')],
           returnMS = FALSE, plot = FALSE, amRange = amRange),
      roughness))
    result$roughness = .resample(list(sound = ms$roughness), len = nr,
                                 lowPass = FALSE, plot = FALSE)
    result$amMsFreq = .resample(list(sound = ms$amMsFreq), len = nr,
                                lowPass = FALSE, plot = FALSE)
    result$amMsPurity = .resample(list(sound = ms$amMsPurity), len = nr,
                                  lowPass = FALSE, plot = FALSE)
    result[!cond_silence, c('roughness', 'amMsFreq', 'amMsPurity')] = NA
  }

  ## Novelty calculation
  if (is.null(novelty)) {
    result$novelty = NA
  } else {
    novel = do.call(.ssm, c(
      list(audio = audio[c('sound', 'samplingRate', 'ls', 'duration')],
           sparse = TRUE, plot = FALSE),
      novelty))$novelty
    result$novelty = .resample(list(sound = novel), len = nr,
                               lowPass = FALSE, plot = FALSE)
    result$novelty[!cond_silence] = NA
  }

  # AM from envelope
  result[, c('amEnvFreq', 'amEnvDep')] = NA
  if (!is.null(amRange)) {
    am = getAM_env(audio = audio,
                   amRange = amRange,
                   overlap = overlap,
                   plot = FALSE)
    result$amEnvFreq = .resample(list(sound = am$freq), len = nr,
                                 lowPass = FALSE, plot = FALSE)
    result$amEnvDep = .resample(list(sound = am$dep), len = nr,
                                lowPass = FALSE, plot = FALSE)
    result[!cond_silence, c('amEnvFreq', 'amEnvDep')] = NA
  }

  # save spectral descriptives separately for voiced and voiceless frames
  varsToUnv = c(
    'ampl', 'roughness', 'amMsFreq', 'amMsPurity', 'amEnvFreq', 'amEnvDep',
    'novelty', 'entropy', 'entropySh', 'dom', 'HNR', 'loudness', 'peakFreq',
    'quartile25', 'quartile50', 'quartile75', 'specCentroid', 'specSlope'
  )
  for (v in varsToUnv) {
    result[, paste0(v, 'Voiced')] = result[, v]
  }

  # update using final / manual pitch
  result = updateAnalyze(
    result = result,
    pitch_true = pitch_true,
    pitchCands_list = pitchCands_list,
    spectrogram = s,
    freqs = freqs,
    bin = bin,
    samplingRate = audio$samplingRate,
    windowLength = windowLength,
    harmHeight_pars = harmHeight,
    subh_pars = subh,
    flux_pars = flux,
    fmRange = fmRange,
    smooth = smooth,
    smoothing_ww = smoothing_ww,
    smoothingThres = smoothingThres,
    # NB: peakFreq & specCentroid are defined for voiceless frames, but not quartiles
    varsToUnv = paste0(varsToUnv, 'Voiced')
  )

  ## Add pitch contours to the spectrogram
  if (plot) {
    # extra contour
    col_non_Hz = c(
      'amEnvDep', 'amMsPurity', 'ampl, amplVoiced',
      'entropy', 'entropyVoiced', 'entropySh', 'entropyShVoiced',
      paste0('f', seq_len(10), '_width'), 'flux', 'fmDep',
      'harmEnergy', 'harmSlope', 'HNR', 'HNR_voiced', 'CPP',
      'loudness', 'loudnessVoiced', 'roughness', 'roughnessVoiced',
      'novelty', 'noveltyVoiced', 'specSlope', 'specSlopeVoiced',
      'subDep', 'subRatio'
    )
    cnt = cnt_name = cnt_plotPars = NULL
    if (!is.null(extraContour)) {
      if (is.list(extraContour)) {
        cnt_name = extraContour[[1]]
        cnt_plotPars = extraContour[2:length(extraContour)]
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
          cnt = zeroOne(cnt, na.rm = TRUE) *
            HzToOther(ylim[2] * 1000, extraSpecPars$yScale)
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
    # pitch contours internally in spectrogram() - a hassle, but it only take a
    # few ms, and otherwise it's hard to add pitch contours b/c the y-axis is
    # messed up if spectrogram() calls layout() to add an oscillogram
    try(do.call(.spectrogram, c(list(
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
      windowLength = windowLength,
      specType = specType,
      zp = zp,
      wn = wn,
      step = step,
      normalize = FALSE,
      output = 'original',
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
          extraContour_warp = (!cnt_name %in% col_non_Hz),
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
  if (is.character(audio$savePlots)) {
    dev.off()
  }

  if (returnPitchCands) {
    invisible(list(result = result,
                   pitchCands = pitchCands_list,
                   spectrogram = s))
  } else {
    invisible(result)
  }
}
