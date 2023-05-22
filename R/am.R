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
  env = as.numeric(seewave::env(audio$sound, f = audio$samplingRate, envt = 'hil',
                                msmooth = c(wl, overlap), plot = FALSE))
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
  return(am)
}


#' Get peak frequency
#'
#' Internal soundgen function
#'
#' @keywords internal
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
  if (inherits(sp, 'try-error')) return(out)
  if (!is.matrix(sp)) return(out)
  nc = ncol(sp)
  nr = nrow(sp)
  if (nc < 1 | nr < 1) return(out)

  # normalize the spectrogram frame-by-frame to get an estimate of amDep
  for (i in 1:nc) {
    m = max(sp[, i])
    if (is.numeric(m) & m != 0) sp[, i] = sp[, i] / sum(sp[, i])
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
  return(peakFreq)
}
