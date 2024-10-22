#' Add amplitude modulation
#'
#' Adds sinusoidal or logistic amplitude modulation to a sound. This produces
#' additional harmonics in the spectrum at ±am_freq around each original
#' harmonic and makes the sound rough. The optimal frequency for creating a
#' perception of roughness is ~70 Hz (Fastl & Zwicker "Psychoacoustics").
#' Sinusoidal AM creates a single pair of new harmonics, while non-sinusoidal AM
#' creates more extra harmonics (see examples).
#' @inheritParams spectrogram
#' @inheritParams soundgen
#' @param play if TRUE, plays the processed audio
#' @param saveAudio full (!) path to folder for saving the processed audio; NULL
#'   = don't save, '' = same as input folder (NB: overwrites the originals!)
#' @param plot if TRUE, plots the amplitude modulation
#' @export
#' @examples
#' sound1 = soundgen(pitch = c(200, 300), addSilence = 0)
#' s1 = addAM(sound1, 16000, amDep = c(0, 50, 0), amFreq = 75, plot = TRUE)
#' # playme(s1)
#' \dontrun{
#' # Parameters can be specified as in the soundgen() function, eg:
#' s2 = addAM(sound1, 16000,
#'          amDep = list(time = c(0, 50, 52, 200, 201, 300),
#'                       value = c(0, 0, 35, 25, 0, 0)),
#'          plot = TRUE, play = TRUE)
#'
#' # Sinusoidal AM produces exactly 2 extra harmonics at ±am_freq
#' # around each f0 harmonic:
#' s3 = addAM(sound1, 16000, amDep = 30, amFreq = c(50, 80),
#'            amType = 'sine', plot = TRUE, play = TRUE)
#' spectrogram(s3, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # Non-sinusoidal AM produces multiple new harmonics,
#' # which can resemble subharmonics...
#' s4 = addAM(sound1, 16000, amDep = 70, amFreq = 50, amShape = -1,
#'            plot = TRUE, play = TRUE)
#' spectrogram(s4, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # ...but more often look like sidebands
#' sound3 = soundgen(sylLen = 600, pitch = c(800, 1300, 1100), addSilence = 0)
#' s5 = addAM(sound3, 16000, amDep = c(0, 30, 100, 40, 0),
#'            amFreq = 105, amShape = -.3,
#'            plot = TRUE, play = TRUE)
#' spectrogram(s5, 16000, ylim = c(0, 5))
#'
#' # Feel free to add AM stochastically:
#' s6 = addAM(sound1, 16000,
#'            amDep = rnorm(10, 40, 20), amFreq = rnorm(20, 70, 20),
#'            plot = TRUE, play = TRUE)
#' spectrogram(s6, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # If am_freq is locked to an integer ratio of f0, we can get subharmonics
#' # For ex., here is with pitch 400-600-400 Hz (soundgen interpolates pitch
#' # on a log scale and am_freq on a linear scale, so we align them by extracting
#' # a long contour on a log scale for both)
#' con = getSmoothContour(anchors = c(400, 600, 400),
#'                        len = 20, thisIsPitch = TRUE)
#' s = soundgen(sylLen = 1500, pitch = con, amFreq = con/3, amDep = 30,
#'              plot = TRUE, play = TRUE, ylim = c(0, 3))
#'
#' # Process all files in a folder and save the modified audio
#' addAM('~/Downloads/temp', saveAudio = '~/Downloads/temp/AM',
#'       amFreq = 70, amDep = c(0, 50))
#' }
addAM = function(x,
                 samplingRate = NULL,
                 amDep = 25,
                 amFreq = 30,
                 amType = c('logistic', 'sine')[1],
                 amShape = 0,
                 invalidArgAction = c('adjust', 'abort', 'ignore')[1],
                 plot = FALSE,
                 play = FALSE,
                 saveAudio = NULL,
                 reportEvery = NULL,
                 cores = 1) {
  # check the format of AM pars
  anchors = c('amDep', 'amFreq', 'amShape')
  for (anchor in anchors) {
    assign(anchor, reformatAnchors(get(anchor)))
  }
  rm('anchor', 'anchors')

  # match args
  myPars = c(as.list(environment()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    funToCall = '.addAM',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Add AM to a sound
#'
#' Internal soundgen function, see \code{\link{addAM}}.
#'
#' @inheritParams addAM
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.addAM = function(
    audio,
    amDep = 25,
    amFreq = 30,
    amType = c('logistic', 'sine')[1],
    amShape = 0,
    invalidArgAction = c('adjust', 'abort', 'ignore')[1],
    plot = FALSE,
    play = FALSE
) {
  # vectorize
  amPar_vect = c('amDep', 'amFreq', 'amShape')
  # just to get rid of of NOTE on CRAN:
  amDep_vector = amFreq_vector = amShape_vector = vector()
  for (p in amPar_vect) {
    p_unique_value = unique(get(p)$value)
    if (length(p_unique_value) > 1) {
      if (invalidArgAction == 'ignore') {
        valueFloor_p = valueCeiling_p = NULL
      } else {
        valueFloor_p = permittedValues[p, 'low']
        valueCeiling_p = permittedValues[p, 'high']
      }
      p_vectorized = getSmoothContour(
        anchors = get(p),
        len = audio$ls,
        interpol = 'approx',
        valueFloor = valueFloor_p,
        valueCeiling = valueCeiling_p
      )
      # plot(p_vectorized, type = 'l')
      assign(paste0(p, '_vector'), p_vectorized)
    } else {
      assign(paste0(p, '_vector'), p_unique_value)
    }
  }

  # prepare am vector
  if (amType == 'sine') {
    if (length(amFreq_vector) == 1) {
      int = amFreq_vector * seq_len(audio$ls)
    } else {
      int = cumsum(amFreq_vector)
    }
    sig = .5 + .5 * cos(2 * pi * int / audio$samplingRate)
  } else {
    sig = getSigmoid(len = audio$ls,
                     samplingRate = audio$samplingRate,
                     freq = amFreq_vector,
                     shape = amShape_vector)
  }
  # plot(sig, type = 'l')
  # sig is on a scale [0, 1]
  # if (length(sig) != length(amDep_vector)) browser()
  am = 1 - sig * amDep_vector / 100
  sound_am = audio$sound * am

  if (plot) {
    .osc(list(sound = am,
              samplingRate = audio$samplingRate,
              ls = length(am)),
         main = 'Amplitude modulation',
         xlab = 'Time, ms',
         ylab = '',
         ylim = c(0, 1),
         midline = FALSE)
  }
  if (play) playme(sound_am, audio$samplingRate)
  if (is.character(audio$saveAudio)) {
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(sound_am, audio = audio, filename = filename)
  }
  sound_am
}


#' Get Amplitude Modulation
#'
#' Internal soundgen function
#'
#' Measures AM
#' @keywords internal
#' @examples
#' s = soundgen(sylLen = 1500, pitch = c(300, 550, 320, 220),
#'              amFreq = c(50, 120, 100), amDep = c(10, 60, 30))
#' # spectrogram(s)
#' # playme(s)
#' am = soundgen:::getAM_env(audio = soundgen:::readAudio(s, samplingRate = 16000),
#'   amRange = c(20, 200), overlap = 80, plot = TRUE)
#' plot(am$time, am$freq, cex = am$dep * 2)
#' # compare to getAM from modulation spectrum:
#' ms = modulationSpectrum(s, 16000, plot = FALSE)
#' plot(x = seq(1, 1500, length.out = length(ms$amMsFreq)), y = ms$amMsFreq,
#'      cex = 10^(ms$amMsPurity/20) * 10, xlab = 'Time, ms', ylab = 'AM frequency, Hz')
getAM_env = function(audio,
                     amRange = c(20, 100),
                     overlap = 80,
                     parab = TRUE,
                     plot = FALSE) {
  # flatten the envelope of the sound to avoid a dependence of amDep on global,
  # slow changes in amplitude
  # osc(audio$sound, audio$samplingRate)
  wl_slow = round(1 / amRange[1] * audio$samplingRate / 2) # half the period of slow am
  audio$sound = .flatEnv(
    audio[which(!names(audio) == 'savePlots')],
    dynamicRange = 240, windowLength_points = wl_slow)

  # extract amplitude envelope
  wl = round(1 / amRange[2] * audio$samplingRate / 2)  # half a period of fast AM
  sr_new = audio$samplingRate / wl * (100 / (100 - overlap))
  env = try(as.numeric(seewave::env(audio$sound, f = audio$samplingRate, envt = 'hil',
                                    msmooth = c(wl, overlap), plot = FALSE)))
  if (inherits(env, 'try-error')) {
    warning('Failed to calculate amplitude modulation - check amRange')
    return(data.frame(time = NA, freq = NA, dep = NA))
  }
  # osc(env, samplingRate = sr_new)

  # bandpass the envelope to further focus on the frequency range of interest
  env1 = .bandpass(list(sound = env, samplingRate = sr_new),
                   lwr = amRange[1], upr = amRange[2], plot = FALSE)
  # env1 = zeroOne(env1)
  # env1 = env1 - mean(env1)
  # osc(env1, samplingRate = sr_new)

  # STFT of the smoothed envelope to find periodicity per frame
  am = getPeakFreq(env1,
                   samplingRate = sr_new,
                   freqRange = amRange,
                   parab = parab,
                   plot = plot)
  am$dep = am$dep * 5.58
  am$dep[am$dep < 0] = 0
  am$dep[am$dep > 1] = 1
  am
}


#' Get peak frequency
#'
#' Internal soundgen function for finding frequency modulation in pitch
#' contours. Called by analyze().
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' s = soundgen(sylLen = 1000, pitch = 500,
#'   vibratoFreq = c(6, 12), vibratoDep = 2,
#'   temperature = .001, addSilence = 5)
#' an = analyze(s, 16000, step = 5, windowLength = 25,
#'   plot = TRUE, yScale = 'bark')
#' plot(an$detailed$fmFreq, type = 'l')
#' }
getPeakFreq = function(x,
                       samplingRate,
                       freqRange = NULL,
                       parab = TRUE,
                       plot = FALSE) {
  out = data.frame(time = NA, freq = NA, dep = NA)
  # STFT of the amplitude envelope
  # sp1 = tuneR::powspec(x, sr = round(samplingRate),
  #         wintime = 1 / freqRange[1] * 4,
  #         steptime = 1 / freqRange[1] * 4 * .3)
  sp = try(suppressMessages(.spectrogram(
    list(sound = x,
         samplingRate = samplingRate,
         ls = length(x)),
    windowLength = 1000 / freqRange[1] * 4,
    padWithSilence = FALSE,
    normalize = FALSE,
    # often no variation, so getFrameBank returns NaN
    # when trying to normalize the "audio"
    plot = FALSE,
    ylim = c(0, .1))), silent = TRUE)
  # suppressMessages b/c spectrogram complains before returning NA for very
  # short sequences
  if (inherits(sp, 'try-error') || !is.matrix(sp)) return(out)
  nc = ncol(sp)
  nr = nrow(sp)
  if (nc < 1 | nr < 1) return(out)

  # normalize the spectrogram frame-by-frame to get an estimate of amDep
  for (i in 1:nc) {
    sum_i = sum(sp[, i], na.rm = TRUE)
    if (sum_i != 0) sp[, i] = sp[, i] / sum_i
  }

  # focus on the frequency range of interest
  times = as.numeric(colnames(sp))
  freqs = as.numeric(rownames(sp)) * 1000
  if (!is.null(freqRange)) {
    sp = sp[which(freqs >= freqRange[1] & freqs <= freqRange[2]), , drop = FALSE]
    freqs = as.numeric(rownames(sp)) * 1000
    nr = nrow(sp)
  }
  if (nc < 1 | nr < 1) return(out)
  # image(t(sp))
  if (nr == 1) {
    # a single frequency bin left
    return(data.frame(time = as.numeric(colnames(sp)),
                      freq = as.numeric(rownames(sp)),
                      dep = as.numeric(sp)))
  }

  # find peak frequency per frame
  peakFreq = data.frame(time = times, freq = rep(NA, nc), dep = NA)
  bin = freqs[2] - freqs[1]
  for (i in 1:ncol(sp)) {
    sp_i = as.numeric(sp[, i])
    idx_peak = which.max(sp_i)
    applyCorrecton = parab && length(idx_peak) == 1 && idx_peak > 1 & idx_peak < nr
    if (applyCorrecton) {
      # use parabolic correction to improve freq resolution
      threePoints = sp_i[(idx_peak - 1) : (idx_peak + 1)]
      parabCor = parabPeakInterpol(threePoints)
      peakFreq$freq[i] = freqs[idx_peak] + bin * parabCor$p
      peakFreq$dep[i] = parabCor$ampl_p
    } else {
      peakFreq$freq[i] = freqs[idx_peak]
      peakFreq$dep[i] = sp[idx_peak, i]
    }
  }
  # peakFreq$dep = to_dB(peakFreq$dep)
  if (plot) plot(peakFreq$time, peakFreq$freq, type = 'b', cex = peakFreq$amp * 10)
  peakFreq
}
