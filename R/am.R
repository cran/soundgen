#' Get Amplitude Modulation
#'
#' Internal soundgen function
#'
#' Measures AM
#' @keywords internal
#' @examples
#' s = soundgen(sylLen = 1500, pitch = c(300, 550, 320, 220),
#'               amFreq = c(50, 120, 100), amDep = c(10, 60, 30))
#' # spectrogram(s)
#' # playme(s)
#' am = soundgen:::getAM_env(audio = list(sound = s, samplingRate = 16000),
#'   amRange = c(20, 200), overlap = 80, plot = TRUE)
#' # compare getAM from modulation spectrum:
#' ms = modulationSpectrum(s, 16000, plot = FALSE)
#' plot(x = seq(1, 1500, length.out = length(ms$amMsFreq)), y = ms$amMsFreq,
#'      cex = 10^(ms$amMsDep/20) * 10, xlab = 'Time, ms', ylab = 'AM frequency, Hz')
getAM_env = function(audio,
                     amRange = c(20, 100),
                     overlap = 80,
                     parab = TRUE,
                     plot = FALSE) {
  # extract amplitude envelope
  # osc(audio$sound, audio$samplingRate)
  wl = round(1 / amRange[2] * audio$samplingRate / 2)  # half a period of the faster AM
  env = as.numeric(seewave::env(audio$sound, f = audio$samplingRate, envt = 'hil',
                                msmooth = c(wl, overlap), plot = FALSE))
  sr_new = audio$samplingRate / wl * (100 / (100 - overlap))

  # high-pass, normalize the envelope
  env = bandpass(env, samplingRate = sr_new, lwr = amRange[1], plot = FALSE)
  env = env - min(env)
  # env = env - mean(env)
  # env = env / max(env)
  # osc(env, samplingRate = sr_new)

  am = getPeakFreq(env,
                   samplingRate = sr_new,
                   freqRange = amRange,
                   parab = parab,
                   plot = plot)

  # get amDep from inflections after low-pass filtering to 2 x max discovered AM
  # (too tight a filter --> we dampen the apparent amDep)
  ps = bandpass(env, samplingRate = sr_new,
                upr = max(am$freq) * 2,
                action = 'pass', plot = FALSE)
  ps = ps - min(ps)
  infl = findInflections(ps, thres = 0, plot = FALSE)
  amDep = 1 - exp(-abs(diff(log(ps[infl]))))
  amDep_res = resample(amDep, len = nrow(am), lowPass = FALSE)
  amDep_res[amDep_res < 0] = 0
  am$dep = amDep_res  # to_dB(amDep_res)
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
  out = data.frame(time = NA, freq = NA, amp = NA)
  # STFT of the amplitude envelope
  # sp1 = tuneR::powspec(x, sr = round(samplingRate),
  #         wintime = 1 / freqRange[1] * 4,
  #         steptime = 1 / freqRange[1] * 4 * .3)
  sp = try(suppressMessages(spectrogram(x,
                   samplingRate = samplingRate,
                   windowLength = 1000 / freqRange[1] * 4,
                   padWithSilence = FALSE,
                   normalize = FALSE,
                   # often no variation, so getFrameBank returns NaN
                   # when trying to normalize the "audio"
                   plot = FALSE,
                   ylim = c(0, .1))), silent = TRUE)
  # suppressMessages b/c spectrograms complains before returning NA for very
  # short sequences
  if (class(sp)[1] == 'try-error') return(out)
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

  # find peak frequency per frame
  peakFreq = data.frame(time = times, freq = rep(NA, nc), purity = NA)
  bin = freqs[2] - freqs[1]
  for (i in 1:ncol(sp)) {
    sp_i = as.numeric(sp[, i])
    idx_peak = which.max(sp_i)
    applyCorrecton = parab && idx_peak > 1 & idx_peak < nr
    if (applyCorrecton) {
      # use parabolic correction to improve freq resolution
      threePoints = sp_i[(idx_peak - 1) : (idx_peak + 1)]
      parabCor = parabPeakInterpol(threePoints)
      peakFreq$freq[i] = freqs[idx_peak] + bin * parabCor$p
      peakFreq$purity[i] = parabCor$ampl_p
    } else {
      peakFreq$freq[i] = freqs[idx_peak]
      peakFreq$purity[i] = sp[idx_peak, i]
    }
  }
  peakFreq$purity = to_dB(peakFreq$purity)
  if (plot) plot(peakFreq$time, peakFreq$freq, type = 'b', cex = peakFreq$amp * 10)
  return(peakFreq)
}
