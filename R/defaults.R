###
### D E F A U L T   P A R   V A L U E S   &   P R E S E T S
###

# usethis::use_data(permittedValues, defaults, presets, overwrite = TRUE)


#' Defaults and ranges for soundgen()
#'
#' A dataset containing defaults and ranges of key variables for soundgen() and
#' soundgen_app().
#' Adjust as needed.
#'
#' @format A matrix with 58 rows and 4 columns:
#' \describe{
#'   \item{default}{default value}
#'   \item{low}{lowest permitted value}
#'   \item{high}{highest permitted value}
#'   \item{step}{increment for adjustment}
#'   ...
#' }
"permittedValues"
permittedValues = matrix(c(
  # sliderInput's to be reset for each callType
  'repeatBout', 1, 1, 20, 1,  # default, low, high, step
  'nSyl', 1, 1, 10, 1,
  'sylLen', 300, 20, 5000, 10,
  'pauseLen', 200, 0, 1000, 10,
  'temperature', .025, 0, 1, .025,
  'maleFemale', 0, -1, 1, 0.1,
  'creakyBreathy', 0, -1, 1, 0.1,
  'nonlinBalance', 100, 0, 100, 1,
  'subRatio', 2, 1, 20, 1,
  'subFreq', 0, 0, 1000, 10,
  'subDep', 0, 0, 100, 1,
  'subWidth', 10000, 0, 10000, 10,
  'shortestEpoch', 300, 50, 500, 25,
  'jitterDep', 0, 0, 12, 0.1,
  'jitterLen', 1, 1, 100, 1,
  'vibratoFreq', 5, 1, 20, .5,
  'vibratoDep', 0, 0, 12, 0.125,
  'shimmerDep', 0, 0, 100, 1,
  'shimmerLen', 1, 1, 100, 1,
  'attackLen', 50, 0, 200, 10,
  'glottis', 0, 0, 5000, 5,
  'rolloff', -9, -30, 0, 1,
  'rolloffOct', 0, -30, 3, 1,
  'rolloffParab', 0, -30, 30, 1,
  'rolloffParabHarm', 3, 1, 20, 1,
  'rolloffKHz', -3, -20, 0, 1,
  'lipRad', 6, 0, 20, 1,
  'noseRad', 4, 0, 20, 1,
  'mouthOpenThres', 0, 0, 1, .05,
  'formantDep', 1, 0, 2, .1,
  'formantDepStoch', 1, 0, 2, .05,
  'formantWidth', 1, .1, 10, .1,
  'formantCeiling', 2, 1, 20, .1,
  'formantLocking', 0, 0, 1, .1,
  'amDep', 0, 0, 100, 5,
  'amFreq', 30, 1, 1000, 1,
  'amShape', 0, -1, 1, .025,
  'samplingRate', 16000, 8000, 200000, 100,
  'windowLength', 40, 5, 100, 2.5, # default, low, high, step
  'dynamicRange', 80, 10, 200, 10,
  'rolloffNoise', -4, -100, 100, 1,
  'rolloffNoiseExp', 0, -30, 30, 1,

  # other soundgen settings, which are NOT updateable sliders in soundgen_app()
  'vocalTract', 15.5, 2, 100, .5,
  'overlap', 50, 0, 99, 1,
  'addSilence', 100, 0, 1000, 50,
  'pitchFloor', 1, 1, 1000, 1,
  'pitchCeiling', 3500, 10, 100000, 10,
  'pitchSamplingRate', 16000, 10, 100000, 10,
  'noiseFlatSpec', 1200, 0, 4000, 100,
  'loessSpan', .5, 0, Inf, .001,
  'discontThres', .05, 0, 1, .001,
  'jumpThres', .01, 0, 1, .001,

  # soundgen_app() settings, which are not needed for soundgen()
  'specWindowLength', 40, 5, 100, 2.5,
  'specContrast', .2, -1, 1, .05,
  'specBrightness', 0, -1, 1, .05,
  'spec_ylim', 5, 0, 22, 1,
  'mouthOpening', .5, 0, 1, .05,
  'pitch', 100, 1, 3500, 1,  # set pitch range per species
  'pitchDeltas', 0, -24, 24, 1,  # pitchGlobal range
  'time', 0, 0, 5000, 1,
  'noiseAmpl', 0, -80, 40, 1,  # for plotting - noise ylim
  'oscHeight', -3, -5, 5, 1   # relative size of spectrogram vs oscillogram
), ncol=5, byrow=TRUE)
temp = permittedValues[,1]
permittedValues = apply(permittedValues[,2:5], 2, as.numeric)
colnames(permittedValues) = c('default', 'low', 'high', 'step')
rownames(permittedValues) = temp
permittedValues = as.data.frame(permittedValues)
rm(temp)
# usethis::use_data(permittedValues, overwrite = TRUE)

# a list of default values for Shiny app - mostly the same as for
# soundgen(). NB: if defaults change, this has to be updated!!!
defaults = list(
  repeatBout = 1,
  nSyl = 1,
  sylLen = 300,
  pauseLen = 200,
  temperature = 0.025,
  maleFemale = 0,
  creakyBreathy = 0,
  nonlinBalance = 100,
  subRatio = 2,
  subFreq = 0,
  subDep = 0,
  subWidth = 10000,
  shortestEpoch = 300,
  jitterDep = 0,
  jitterLen = 1,
  vibratoFreq = 5,
  vibratoDep = 0,
  shimmerDep = 0,
  shimmerLen = 1,
  attackLen = 50,
  rolloff = -9,
  rolloffOct = 0,
  rolloffParab = 0,
  rolloffParabHarm = 3,
  rolloffKHz = -3,
  lipRad = 6,
  noseRad = 4,
  mouthOpenThres = 0,
  formantDep = 1,
  formantDepStoch = 1,
  formantWidth = 1,
  amDep = 0,
  amFreq = 30,
  amShape = 0,
  samplingRate = 16000,
  windowLength = 40,
  rolloffNoise = -4,
  rolloffNoiseExp = 0,
  windowLength_points = 512,
  overlap = 75,
  addSilence = 100,
  pitchFloor = 1,
  pitchCeiling = 3500,
  pitchSamplingRate = 16000,
  dynamicRange = 80,
  pitch = list(
    time = c(0, .1, .9, 1),
    value = c(100, 150, 135, 100)
  ),
  pitchGlobal = list(time = c(0, 1), value = c(0, 0)),
  glottis = 0,
  noise = list(time = c(0, 300), value = c(-80, -80)),
  mouth = list(time = c(0, 1), value = c(.5, .5)),
  ampl = list(time = c(0, 1), value = c(0, 0)),
  amplGlobal = list(time = c(0, 1), value = c(0, 0)),
  formants = c(860, 1430, 2900),
  formantsNoise = NA,
  vowelString = NA,
  vocalTract = NA,
  sylLenDep = 1,
  formDrift = .3,
  formDisp = .2,
  pitchDriftDep = .5,
  pitchDriftFreq = .04,
  amplDriftDep = 5,
  subDriftDep = 4,
  rolloffDriftDep = 3,
  pitchDep = .05,
  noiseDep = .1,
  amplDep = .1,
  glottisDep = .1,
  specDep = .1
)
# usethis::use_data(defaults, overwrite = TRUE)


#' Defaults and ranges for analyze()
#'
#' A dataset containing defaults and ranges of key variables for analyze() and
#' pitch_app(). Adjust as needed.
#'
#' @format A matrix with 58 rows and 4 columns:
#' \describe{
#'   \item{default}{default value}
#'   \item{low}{lowest permitted value}
#'   \item{high}{highest permitted value}
#'   \item{step}{increment for adjustment}
#'   ...
#' }
"defaults_analyze"
defaults_analyze = matrix(c(
  'windowLength', 50, 1, 500, 1,  # default, low, high, step
  'step', 25, 1, 500, 1,
  'overlap', 50, 0, 99, 1,
  'dynamicRange', 80, 10, 200, 10,
  'zp', 0, 0, 13, 1,

  'silence', 0.04, 0, 1, .01,
  'entropyThres', 0.6, 0, 1, 0.01,
  'nCands', 3, 1, 10, 1,
  'minVoicedCands', 2, 1, 4, 1,

  'pitchFloor', 75, 1, 1000, 10,
  'pitchCeiling', 2000, 10, 10000, 10,
  'priorMean', 300, 10, 10000, 10,
  'priorSD', 6, 0.1, 24, 1,

  'domThres', 0.1, 0, 1, 0.01,
  'domSmooth', 220, 1, 600, 10,

  'autocorThres', 0.7, 0, 1, 0.01,
  'autocorSmooth', 7, 1, 21, 2,
  'autocorUpsample', 25, 0, 500, 1,
  'autocorBestPeak', .975, 0, 1, .001,

  'cepThres', 0.75, 0, 1, 0.01,
  # 'cepSmooth', 400, 10, 1000, 10,
  'cepZp', 0, 0, 13, 1,

  'specThres', 0.05, 0, 1, 0.01,
  'specPeak', 0.25, 0, 1, 0.01,
  'specHNRslope', 0.8, 0, 5, 0.05,
  'specSmooth', 150, 1, 600, 10,
  'specMerge', 0.1, 0.01, 10, 0.1,
  'specSinglePeakCert', 0.4, 0, 1, 0.01,
  'specRatios', 3, 1, 20, 1,

  'hpsNum', 5, 2, 100, 1,
  'hpsThres', .1, 0, 1, .01,
  'hpsNorm', 2, 0, 10, .1,
  'hpsPenalty', 2, 0, 100, .1,

  'zcThres', .1, 0, 1, .01,
  'zcWin', 5, 3, 25, 2,

  'certWeight', 0.5, 0, 1, 0.01,
  'shortestSyl', 20, 0, 500, 1,
  'shortestPause', 60, 0, 500, 1,
  'smooth', 1, 0, 10, 0.1,
  'snakeStep', 0.05, 0, 2, 0.01,
  'interpolWin', 75, 0, 1000, 5,
  'interpolTol', 0.05, 0.01, 1, 0.01,
  'interpolCert', 0.3, 0, 1, 0.01,

  'harmThres', 3, 0, 100, 1,
  'harmTol', 0.25, 0, 1, 0.01,
  'harmPerSel', 5, 1, 100, 1,

  # pitch_app() settings, which are not needed for analyze()
  'specContrast', .2, -1, 1, .05,
  'specBrightness', 0, -1, 1, .05,
  'blur_freq', 0, -1000, 1000, 10,
  'blur_time', 0, -100, 100, 1,
  'nColors', 30, 10, 1000, 10,
  'reass_cex', .5, .05, 2, .05,
  'reass_windowLength', 15, 1, 200, 1,
  'reass_step', 5, 1, 100, 1,
  'spec_ylim', 5, 0, 22, 0.1,
  'spec_maxPoints', 5.5, 3, 7, .25,
  'spec_cex', 1, .1, 10, .1,
  'osc_maxPoints', 5, 3, 7, .5,
  'osc_height', 100, 25, 500, 25
), ncol=5, byrow=TRUE)
temp = defaults_analyze[,1]
defaults_analyze = apply(defaults_analyze[,2:5], 2, as.numeric)
colnames(defaults_analyze) = c('default', 'low', 'high', 'step')
rownames(defaults_analyze) = temp
defaults_analyze = as.data.frame(defaults_analyze)
rm(temp)
# usethis::use_data(defaults_analyze, overwrite = TRUE)


#' Defaults for plotting with analyze()
#'
#' Default plotting settings for each pitch tracker in analyze() and
#' pitch_app(). Adjust as needed.
#'
#' @format A dataframe with 8 rows and 5 columns:
#' \describe{
#'   \item{method}{pitch tracking method}
#'   \item{col}{color}
#'   \item{pch}{point character}
#'   \item{lwd}{line width}
#'   \item{lty}{line type}
#'   ...
#' }
defaults_analyze_pitchCand = as.data.frame(matrix(c(
  'final', '#0000FFBF', 16, 1, 5, 1,  # final pitch contour
  'manual', 'blue', 18, 2, 1, 1,  # manual pitch candidates
  'dom', 'orange', 3, 2, 1, 1,
  'autocor', 'green', 16, 2, 1, 1,
  'cep', 'violet', 7, 2, 1, 1,
  'spec', 'red', 2, 2, 1, 1,
  'hps', 'brown', 8, 2, 1, 1,
  'zc', 'pink', 0, 2, 1, 1,
  'def', 'black', 1, 2, 1, 1  # default par() for dealing with NAs
), ncol = 6, byrow = TRUE), stringsAsFactors = FALSE)
colnames(defaults_analyze_pitchCand) = c('method', 'col', 'pch', 'cex', 'lwd', 'lty')
for (p in c('pch', 'cex', 'lwd', 'lty')) {
  defaults_analyze_pitchCand[, p] = as.numeric(defaults_analyze_pitchCand[, p])
}
# usethis::use_data(defaults_analyze_pitchCand, overwrite = TRUE)


#' Defaults and ranges for formant_app()
#'
#' Internal soundgen list of defaults.
#'
#' A dataset containing defaults and ranges of key variables for formant_app().
#' Adjust as needed.
#' @keywords internal
#' @format A matrix with 4 columns:
#' \describe{
#'   \item{default}{default value}
#'   \item{low}{lowest permitted value}
#'   \item{high}{highest permitted value}
#'   \item{step}{increment for adjustment}
#'   ...
#' }
"def_form"
def_form = matrix(c(
  'nFormants', 4, 1, 10, 1,       # default, low, high, step
  'silence', 0.04, 0, 1, .01,
  'minformant', 200, 10, 5000, 10,
  'maxbw', 600, 10, 5000, 10,

  'windowLength_lpc', 10, 1, 100, 1,
  'step_lpc', 5, 1, 100, 1,
  'overlap_lpc', 50, 0, 99, 1,
  'dynamicRange_lpc', 80, 10, 200, 10,
  'zp_lpc', 0, 0, 13, 1,

  'spec_ylim', 5, 0, 96, 0.1,
  'specContrast', .2, -1, 1, .05,
  'specBrightness', -.2, -1, 1, .05,
  'blur_freq', 0, -1000, 1000, 10,
  'blur_time', 0, -100, 100, 1,
  'reass_cex', .25, .05, 2, .05,
  'windowLength', 8, 1, 500, 1,
  'step', 2, 1, 100, 1,
  'overlap', 50, 0, 99, 1,
  'dynamicRange', 80, 10, 200, 10,
  'zp', 0, 0, 13, 1,
  'spec_maxPoints', 5.5, 3, 7, .25,

  'osc_height', 100, 25, 5000, 25,
  'osc_maxPoints', 5, 3, 7, .5,

  'spectrum_len', 500, 100, 5000, 25,
  'spectrum_smooth', -1, -2, 0, .05,
  'spectrum_xlim', 5, 0, 96, .1,
  'samplingRate_mult', 0, -3, 3, .1
), ncol = 5, byrow = TRUE)
temp = def_form[,1]
def_form = apply(def_form[,2:5], 2, as.numeric)
colnames(def_form) = c('default', 'low', 'high', 'step')
rownames(def_form) = temp
def_form = as.data.frame(def_form)
rm(temp)
# usethis::use_data(def_form, overwrite = TRUE)
