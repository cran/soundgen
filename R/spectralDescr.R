#' Get HNR
#'
#' Calculates the harmonics-to-noise ratio (HNR) - that is, the ratio between
#' the intensity (root mean square amplitude) of the harmonic component and the
#' intensity of the noise component. Normally called by \code{\link{analyze}}.
#'
#' @references Boersma, P. (1993). Accurate short-term analysis of the
#'   fundamental frequency and the harmonics-to-noise ratio of a sampled sound.
#'   In Proceedings of the institute of phonetic sciences (Vol. 17, No. 1193,
#'   pp. 97-110).
#'
#' @param x time series (a numeric vector)
#' @param samplingRate sampling rate
#' @param acf_x pre-computed autocorrelation function of input \code{x}, if
#'   already available
#' @param lag.min,lag.max minimum and maximum lag to consider when looking for
#'   peaks in the ACF; lag.min = samplingRate/pitchCeiling, lag.max =
#'   samplingRate/pitchFloor
#' @param interpol method of improving the frequency resolution by interpolating
#'   the ACF: "none" = don't interpolate; "parab" = parabolic interpolation on
#'   three points (local peak and its neighbors); "spline" = spline
#'   interpolation; "sinc" = sin(x)/x interpolation to a continuous function
#'   followed by a search for local peaks using Brent's method
#' @param wn window applied to \code{x} (unless acf_x is provided instead of x)
#'   as well as to the sinc interpolation
#' @param idx_max (internal) the index of the peak to investigate, if already
#'   estimated
#'
#' @return A list of three components: f0 = frequency corresponding to the peak
#'   of the autocorrelation function; max_acf = amplitude of the peak of the
#'   autocorrelation function on a scale of (0, 1); HNR = 10 * log10(x / (1 -
#'   max_acf)).
#'
#' @export
#' @examples
#' signal = sin(2 * pi * 150 * (1:16000)/16000)
#' signal = signal / sqrt(mean(signal^2))
#' noise = rnorm(16000)
#' noise = noise / sqrt(mean(noise^2))
#' SNR = 40
#' s = signal + noise * 10^(-SNR/20)
#' soundgen:::getHNR(s, 16000, lag.min = 16000/1000,
#' lag.max = 16000/75, interpol = 'none')
#' soundgen:::getHNR(s, 16000, lag.min = 16000/1000,
#' lag.max = 16000/75, interpol = 'parab')
#' soundgen:::getHNR(s, 16000, lag.min = 16000/1000,
#' lag.max = 16000/75, interpol = 'spline')
#' soundgen:::getHNR(s, 16000, lag.min = 16000/1000,
#' lag.max = 16000/75, interpol = 'sinc')
getHNR = function(x = NULL,
                  samplingRate = NA,
                  acf_x = NULL,
                  lag.min = 2,
                  lag.max = length(x),
                  interpol = c('none', 'parab', 'spline', 'sinc')[4],
                  wn = 'hanning',
                  idx_max = NULL
) {
  if (!is.null(x)) {
    ## calculate ACF
    len = length(x)
    half_len = floor(len / 2)
    lag.min = round(lag.min)
    lag.max = round(min(lag.max, half_len))
    if (lag.min > half_len) stop('lag.min is too small')
    if (lag.max <= lag.min) stop('lag.max must be > lag.min')

    # prepare a windowing function
    win = seewave::ftwindow(len, wn = wn)

    # calculate ACF of the windowing function
    sp_win = fft(win) / half_len
    powerSpectrum_win = Re(sp_win * Conj(sp_win))
    acf_win = Re(fft(powerSpectrum_win, inverse = TRUE))[seq_len(half_len)]
    acf_win = acf_win / acf_win[1]
    # plot(acf_win, type = 'l')

    ## calculate ACF of the windowed signal
    sp_x = fft(x * win) / half_len
    powerSpectrum_x = Re(sp_x * Conj(sp_x))
    acf_x = Re(fft(powerSpectrum_x, inverse = TRUE))[seq_len(half_len)] / acf_win
    acf_x = acf_x / acf_x[1]
  } else {
    if (is.null(acf_x))
      stop('Please provide either signal (x) or its autocorrelation function (acf_x)')
  }
  # plot(acf_x[lag.min:lag.max], type = 'b')

  ## find the maximum and interpolate the ACF to improve resolution
  if (is.null(idx_max)) {
    idx_max = which.max(acf_x[lag.min:lag.max]) + lag.min - 1
  }
  # if (acf_x[idx_max] < 0) return(NA)  # causes problems in getPitchAutocor()
  if (interpol == 'none') {
    max_acf = acf_x[idx_max]
  } else if (interpol == 'parab') {
    parabInterp = parabPeakInterpol(acf_x[(idx_max - 1) : (idx_max + 1)])
    max_acf = parabInterp$ampl_p
    idx_max = idx_max + parabInterp$p
  } else if (interpol == 'spline') {
    idx = max(lag.min, (idx_max - 10)) : min(lag.max, (idx_max + 10))
    acf_ups = spline(acf_x[idx], n = length(idx) * 100)
    idx_max_ups = which.max(acf_ups$y)
    max_acf = acf_ups$y[idx_max_ups]
    idx_max = idx[1] - 1 + acf_ups$x[idx_max_ups]
  } else if (interpol == 'sinc') {
    # apply a gaussian window to the sinc interpolation as a function of the
    # distance from the max
    half_len = min(250, length(acf_x) / 2)
    idx_left = max(2, idx_max - half_len)
    # max(2, idx_max - min(250, floor(length(acf_x) / 4)))
    if (idx_max > idx_left) {
      win_left = seewave::ftwindow((idx_max - idx_left) * 2, wn = wn)[seq_len(idx_max - idx_left)]
    } else {
      win_left = numeric(0)
    }
    idx_right = min(length(acf_x), idx_max + half_len)
    #   min(length(acf_x), idx_max + min(250, floor(length(acf_x) / 4)))
    if (idx_right > idx_max) {
      win_right = seewave::ftwindow(
        (idx_right - idx_max) * 2, wn = wn)[(idx_right - idx_max) :
                                              ((idx_right - idx_max) * 2)]
    } else {
      win_right = numeric(0)
    }
    win = c(win_left, win_right)
    # win = win / sum(win)
    # plot(win)
    acf_idx = idx_left:idx_right
    # length(win)
    # length(acf_idx)

    if (FALSE) {
      # visual check of sinc interpolation
      d_new = data.frame(x = seq(lag.min, lag.max, by = .1))
      for (i in seq_len(nrow(d_new))) {
        d_new$y[i] = sum(acf_x[acf_idx] * sinc(d_new$x[i] - acf_idx) * win)
      }
      plot(lag.min:lag.max, acf_x[lag.min:lag.max])
      points(d_new, type = 'l', col = 'blue')
    }

    # find max by using the sinc function in optimize (Brent 1973)
    opt = optimize(function(j) {sum(acf_x[acf_idx] * sinc(j - acf_idx) * win)},
                   interval = c(idx_max - 2, idx_max + 2), maximum = TRUE)
    # opt = optimize(function(j) {sum(acf_x[acf_idx] * sinc(j - acf_idx) * win)},
    #                interval = c(lag.min, lag.max), maximum = TRUE)
    max_acf = opt$objective
    idx_max = opt$maximum
  }
  if (max_acf > 1) {
    max_acf = 1 / max_acf
  }
  f0 = samplingRate / idx_max
  list(f0 = f0,
       max_acf = max_acf,
       HNR = to_dB(max_acf))
}


#' Height of harmonics
#'
#' Internal soundgen function
#'
#' Attempts to estimate how high harmonics reach in the spectrum - that is, at
#' what frequency we can still discern peaks at multiples of f0 or, for
#' low-pitched sounds, regularly spaced peaks separated by ~f0.
#' @inheritParams analyzeFrame
#' @param pitch the final pitch estimate for the current frame
#' @param harmThres minimum height of spectral peak, dB
#' @param harmPerSel the number of harmonics per sliding selection
#' @param harmTol maximum tolerated deviation of peak frequency from multiples
#'   of f0, proportion of f0
#' @return Returns the frequency (Hz) up to which we find harmonics
#' @keywords internal
#' @examples
#' s = soundgen(sylLen = 400, addSilence = 0, pitch = 400, noise = -10,
#'   rolloff = -15, jitterDep = .1, shimmerDep = 5, temperature = .001)
#' sp = spectrogram(s, samplingRate = 16000)
#' hh = soundgen:::harmHeight(sp[, 5], pitch = 400,
#'   freqs = as.numeric(rownames(sp)) * 1000, bin = 16000 / 2 / nrow(sp))
#' hh
harmHeight = function(frame,
                      pitch,
                      bin,
                      freqs,
                      harmThres = 3,
                      harmTol = 0.25,
                      harmPerSel = 5) {
  frame_dB = 20 * log10(frame)
  # plot(freqs, frame_dB, type = 'l'); abline(v = pitch, col = 'blue')

  # METHOD 1: look for peaks at multiples of f0
  lh_peaks = harmHeight_peaks(frame_dB, pitch, bin, freqs,
                              harmThres = harmThres,
                              harmTol = harmTol,
                              plot = FALSE)

  # METHODS 2 & 3: look for peaks separated by f0
  lh2 = harmHeight_dif(frame_dB, pitch, bin, freqs,
                       harmThres = harmThres,
                       harmTol = harmTol,
                       harmPerSel = harmPerSel,
                       plot = FALSE)
  lh = median(c(lh_peaks, lh2$lastHarm_dif, lh2$lastHarm_cep), na.rm = TRUE)
  if (lh < pitch) lh = NA
  list(harmHeight = lh,
       harmHeight_peaks = lh_peaks,
       harmHeight_dif = lh2$lastHarm_dif,
       harmHeight_cep = lh2$lastHarm_cep,
       harmSlope = lh2$harmSlope)
}


#' Height of harmonics: peaks method
#'
#' Internal soundgen function
#'
#' Estimates how far harmonics reach in the spectrum by checking how many
#' spectral peaks we can find close to multiples of f0.
#' @inheritParams harmHeight
#' @param plot if TRUE, produces a plot of spectral peaks
#' @keywords internal
harmHeight_peaks = function(frame_dB,
                            pitch,
                            bin,
                            freqs,
                            harmThres = 3,
                            harmTol = 0.25,
                            plot = FALSE) {
  pitch_bin = round(pitch / bin)
  len_frame = length(frame_dB)
  harmSmooth = round(harmTol * pitch / bin)  # from prop of f0 to bins
  nHarm = floor((max(freqs) - harmSmooth * bin) / pitch)
  peakFound = rep(FALSE, nHarm)
  if (plot) plot(freqs, frame_dB, type = 'l')
  for (h in 1:nHarm) {
    # check f0 as well, otherwise may get 2 * f0 although f0 is also below thres
    bin_h = round(pitch * h / bin)

    # b/c of rounding error, and b/c pitch estimates are often slightly off, the
    # true harmonic may lie a bit above or below this bin, so we search for a
    # peak within harmSmooth of where we expect to find it
    idx_peak = which.max(frame_dB[(bin_h - harmSmooth) : (bin_h + harmSmooth)])
    bin_peak = bin_h + idx_peak - harmSmooth - 1

    # compare the peak with the median over ±pitch to check whether the peak is
    # prominent enough
    idx_around = max(1, bin_h - pitch_bin) : (min(len_frame, bin_h + pitch_bin))
    idx_around = idx_around[-h]
    median_around = median(frame_dB[idx_around])
    peakFound[h] = frame_dB[bin_peak] - median_around > harmThres

    if (plot)
      text(freqs[bin_peak], frame_dB[bin_peak], labels = h, pch = 5,
           col = if (peakFound[h]) 'red' else 'blue')
  }

  if (any(peakFound)) {
    absent_harm = which(!peakFound)
    if (length(absent_harm) == 0) {
      # just the last found harmonic peak
      lastHarm = pitch * nHarm
    } else {
      # the last non-missing harmonic peak
      lastHarm = pitch * (absent_harm[1] - 1)
    }
  } else {
    lastHarm = NA
  }
  lastHarm
}


#' Height of harmonics: difference method
#'
#' Internal soundgen function
#'
#' Estimates how far harmonics reach in the spectrum by analyzing the typical
#' distances between spectral peaks in different frequency regions.
#' @inheritParams harmHeight
#' @param plot if TRUE, produces a plot of spectral peaks
#' @keywords internal
harmHeight_dif = function(frame_dB,
                          pitch,
                          bin,
                          freqs,
                          harmThres = 3,
                          harmTol = 0.25,
                          harmPerSel = 5,
                          plot = FALSE) {
  # width of smoothing interval (in bins), forced to be an odd number
  harmSmooth_bins = 2 * ceiling(pitch / bin / 2) - 1

  # find peaks in the smoothed spectrum (much faster than seewave::fpeaks)
  hb = floor(harmSmooth_bins / 2)
  idx = which(vapply(
    (hb + 1):(length(frame_dB) - hb), function(x) {
      frx = frame_dB[(x - hb):(x + hb)]
      frame_dB[x] == max(frx) &&   # peak in the middle
        frame_dB[x] - median(frx[-(hb + 1)]) > harmThres  # and above median w/o the center
    }, logical(1))) + hb
  nPeaks = length(idx)

  # slide a selection along the spectrum starting from f0
  pitch_bins = pitch / bin  # f0 location in bins
  # width of selection in bins (no more than half the frame len)
  sel_bins = min(round(pitch_bins * harmPerSel), length(frame_dB) / 2)
  harmTol_bins = round(pitch_bins * harmTol)  # tolerated deviance in bins
  i = pitch_bins  # start at f0
  pitch_bin_cep = pitch_bin_peaks = vector('logical', 0)
  while (i + sel_bins < length(frame_dB)) {
    end = i + sel_bins - 1

    # count intervals b/w spectral peaks
    d = diff(idx[idx >= i & idx <= end])  # distances b/w peaks
    # median deviation of these distances from expected (f0)
    dp = abs(median(d, na.rm = TRUE) - pitch_bins)
    dp_within_tol = (dp < harmTol_bins)
    pitch_bin_peaks = c(pitch_bin_peaks, dp_within_tol)

    # cepstrum
    sel = as.numeric(frame_dB[i:(i + sel_bins - 1)])
    cep = abs(fft(sel))
    # plot(sel, type = 'l')
    l = length(cep) %/% 2
    cep = cep[seq_len(l)]
    # plot(cep, type = 'l')
    bin_at_pitch = harmPerSel + 1
    # Is there a local max at bin_at_pitch? Any height will do
    peak_at_pitch = (.subset2(cep, bin_at_pitch) > .subset2(cep, bin_at_pitch - 1)) &&
      (.subset2(cep, bin_at_pitch) > .subset2(cep, bin_at_pitch + 1))
    pitch_bin_cep = c(pitch_bin_cep, peak_at_pitch)

    i = round(i + pitch_bins)  # move the sel by one harmonic (f0)
  }

  # Find the middle frequency of the first bin w/o harmonics
  fbwh_peaks = .subset2(which(!pitch_bin_peaks), 1)
  if (is.na(fbwh_peaks)) {
    lastHarm_dif = nPeaks * pitch  # found everywhere - take the top frequency, not middle
  } else {
    lastHarm_dif = (sel_bins / 2 + pitch_bins * (fbwh_peaks - 1)) * bin
  }
  if (!is.na(lastHarm_dif) && lastHarm_dif < pitch) lastHarm_dif = NA

  fbwh_cep = .subset2(which(!pitch_bin_cep), 1)
  if (is.na(fbwh_cep)) {
    lastHarm_cep = nPeaks * pitch
  } else {
    lastHarm_cep = (sel_bins / 2 + pitch_bins * (fbwh_cep - 1)) * bin
  }
  if (!is.na(lastHarm_cep) && lastHarm_cep < pitch) lastHarm_cep = NA

  # calculate harmonic slope
  # (like spectral slope, but only for the confirmed harmonic peaks)
  idx_harms = idx[which(pitch_bin_peaks & pitch_bin_cep)]
  last_harm_bin = median(lastHarm_dif, lastHarm_cep, na.rm = TRUE) / bin
  idx_harms = idx_harms[idx_harms < last_harm_bin]
  if (length(idx_harms) > 1) {
    harms = data.frame(freq = freqs[idx_harms] / 1000, ampl = frame_dB[idx_harms])
    mod = suppressWarnings(lm(ampl ~ freq, harms))
    harmSlope = mod$coefficients[2]
  } else {
    harmSlope = NA
  }

  if (plot) {
    plot(freqs, frame_dB, type = 'l')
    points(freqs[idx_harms], frame_dB[idx_harms], pch = 5, col = 'blue')
    points(freqs[idx[!idx %in% idx_harms]],
           frame_dB[idx[!idx %in% idx_harms]],
           pch = 5, col = 'red')
    abline(mod$coefficients[1], mod$coefficients[2] / 1000, lty = 2, col = 'blue')
  }

  list(lastHarm_cep = lastHarm_cep,
       lastHarm_dif = lastHarm_dif,
       harmSlope = harmSlope)
}


#' Energy in harmonics
#'
#' Internal soundgun function
#'
#' Calculates the % of energy in harmonics based on the provided pitch estimate
#' @param pitch pitch estimates, Hz (vector)
#' @param s spectrogram (ncol = length(pitch))
#' @param coef calculate above pitch * coef
#' @param freqs as.numeric(rownames(s)) * 1000
#' @keywords internal
harmEnergy = function(pitch, s, freqs = NULL, coef = 1.25) {
  if (is.null(freqs)) freqs = as.numeric(rownames(s)) * 1000
  out = rep(NA, length(pitch))
  threshold = coef * pitch
  idx_notNA = which(!is.na(threshold))
  cs = colSums(s)
  out[idx_notNA] = vapply(idx_notNA, function(x) {
    sum(s[freqs > .subset2(threshold, x), x] / .subset2(cs, x))
  }, numeric(1))
  out
}


#' Subharmonics-to-harmonics ratio
#'
#' Internal soundgen function
#'
#' Looks for pitch candidates (among the ones already found if method =
#' 'pitchCands', or using some other pitch-tracking-like techniques such as
#' cepstrum) at integer ratios of f0. If such candidates are found, they are
#' treated as subharmonics. Note that this depends critically on accurate pitch
#' tracking.
#' @inheritParams analyzeFrame
#' @param pitch pitch per frame, Hz
#' @param pitchCands a list of pitch candidates and certainties sent from
#'   analyze()
#' @param method 'cep' = cepstrum, 'pitchCands' = existing pitch candidates
#'   below f0, 'harm' = look for harmonic peaks. Only 'cep' is really working at
#'   the moment.
#' @param nSubh the maximum ratio of f0 / g0 to consider
#' @param tol target frequency (eg f0 / 2) has to be within \code{tol * target}
#'   (eg tol = .05 gives a tolerance of 5\%)
#' @param nHarm for method 'harm' only
#' @inheritParams harmHeight
#' @keywords internal
#' @examples
#' \dontrun{
#' s400 = soundgen(
#'   sylLen = 300, pitch = c(280, 370, 330),
#'   subDep = list(
#'     time = c(0, .5, .51, 1),
#'     value = c(0, 0, 10, 10)
#'   ), subRatio = 3,
#'   smoothing = list(interpol = 'approx'), formants = 'a',
#'   rolloff = -12, addSilence = 50, temperature = .001,
#'   plot = TRUE, ylim = c(0, 2)
#' )
#' s = analyze(s400, samplingRate = 16000,
#'             windowLength =  50, step = 10,
#'             pitchMethods = c('dom', 'autocor', 'hps'), priorMean = NA,
#'             plot = TRUE, ylim = c(0, 3),
#'             extraContour = list('subDep', type = 'b', col = 'brown'))
#' s$detailed[, c('subRatio', 'subDep')]
#'
#' s2 = analyze(s400, samplingRate = 16000,
#'             windowLength =  50, step = 10,
#'             pitchMethods = c('dom', 'autocor', 'hps'), priorMean = NA,
#'             subh = list(method = 'harm'),
#'             plot = TRUE, ylim = c(0, 3),
#'             extraContour = list('subDep', type = 'b', col = 'brown'))
#' s$detailed[, c('subRatio', 'subDep')]
#' }
getSHR = function(
    frame,
    bin,
    freqs,
    pitch,
    pitchCands = NULL,
    samplingRate,
    method = c('cep', 'pitchCands', 'harm')[1],
    nSubh = 5,
    tol = .05,
    nHarm = 5,
    harmThres = 12,
    harmTol = 0.25,
    amRange = c(10, 200)
) {
  # plot(freqs, log(frame), type = 'l')
  best_subh = NA
  subDep = 0
  am = list(amFreq = NA, amDep = NA)
  if (method == 'pitchCands' &
      (is.null(pitchCands) || length(pitchCands$freq) < 2)) {
    method = 'cep'
  }
  if (method == 'pitchCands') {
    ratios = data.frame(r = 1:nSubh, energy = NA)
    for (r in seq_len(nSubh)) {
      pr = pitch / r
      idx = which(abs(pitchCands$freq - pr) / pr < tol)
      if (length(idx) > 0) {
        ratios$energy[r] = mean(pitchCands$cert[idx])
      }
    }
    ratios$extraEnergy = ratios$energy - ratios$energy[1] / ratios$r
    subR = na.omit(ratios[ratios$extraEnergy > 0, ])
    if (nrow(subR) > 0) {
      best_subh = subR$r[which.max(subR$extraEnergy)]
      subDep = ratios$extraEnergy[best_subh] / ratios$energy[best_subh]
    }
  } else if (method == 'cep') {
    # cepstrum
    cep = abs(fft(as.numeric(log(frame))))
    l = length(cep) %/% 2
    seq_len_l = seq_len(l)
    cep = cep[seq_len_l]
    cep[1] = 0
    freqs_cep = samplingRate / seq_len_l / 2
    # plot(freqs_cep, cep, type = 'b', log = 'x')
    bin_at_pitch = which.min(abs(freqs_cep - pitch))
    nToTry = min(nSubh, floor(l / bin_at_pitch))
    ratios = data.frame(r = seq_len(nToTry), energy = NA)
    for (r in ratios$r) {
      ratios$energy[r] = max(cep[(bin_at_pitch * r - 1) : (bin_at_pitch * r + 1)])
    }
    if (FALSE) {
      # we expect the cepstral peak to grow linearly with the density of
      # harmonics, so eg twice as strong at 200 Hz as at 400 Hz. Thus, if some
      # energy is present at f0/r, we penalize its apparent strength by r
      a = rep(c(1, 0), 100)
      max(abs(fft(a) / length(a))) # .5

      b = rep(c(1, 0, 0, 0), 50)
      max(abs(fft(b) / length(b))) # .25

      c = rep(c(1, 0, 0, 0, 0, 0, 0, 0), 25)
      max(abs(fft(c) / length(c))) # .125
    }
    ratios$expected = ratios$energy[1] / ratios$r
    ratios$extraEnergy = ratios$energy - ratios$expected
    subR = na.omit(ratios[ratios$extraEnergy > 0, ])
    if (nrow(subR) > 0) {
      best_subh = subR$r[which.max(subR$extraEnergy)]
      subDep = ratios$extraEnergy[best_subh]/ratios$energy[best_subh]
      subDep[subDep > 1] = 1
      # ad hoc correction to linearize subDep - from simulations with known
      # soundgen(subDep = ...), mod = nls(subDep ~ exp(b * m + c), data = out1,
      # start = list(b = 1, c = 0))  See validate_subDep.R
      subDep = exp(5 * subDep - 5)
    }

    ## calculate AM (cepstral peaks in amRange not harmonically related to pitch)
    # cancel pitch harmonics
    cep1 = cep
    for (i in seq_len(min(10, floor(l / bin_at_pitch)))) {
      idx = which.min(abs(freqs_cep - pitch / i))
      idx = c(idx, idx + 1, idx - 1)
      idx = idx[idx > 1 & idx < l]
      cep1[idx] = 0
    }
    # plot(freqs_cep, cep1, type = 'b', log = 'x')

    # find peaks within amRange
    idx_keep = which(freqs_cep >= amRange[1] & freqs_cep <= amRange[2])
    b = data.frame(
      idx = idx_keep,
      freq = freqs_cep[idx_keep], # samplingRate / idx_keep * zp_corr,  # smth fishy here...
      cep = cep1[idx_keep]
    )
    # plot(b$freq, b$cep, type = 'l', log = 'x')
    idx = which(diff(diff(b$cep) > 0) == -1) + 1
    idx_am = idx[which.max(b$cep[idx])]
    am = list(amFreq = b$freq[idx_am],
              amDep = b$cep[idx_am] / cep[bin_at_pitch])
  } else if (method == 'harm') {
    am = list(amFreq = NA, amDep = NA)
    keep_idx = which(freqs < (pitch * nHarm))
    frame = frame[keep_idx]
    frame = frame / max(frame)
    frame_dB = 20 * log10(frame[keep_idx])
    freqs = freqs[keep_idx]
    n = length(keep_idx)
    # plot(freqs[keep_idx], frame_dB, type = 'l')

    # look for spectral peaks
    hb = 1
    idx_peaks = which(vapply(
      (hb + 1):(length(frame_dB) - hb), function(x) {
        frx = frame_dB[(x - hb):(x + hb)]
        frame_dB[x] == max(frx) && (  # local maximum
          (frame_dB[x] - min(frx) > harmThres) |  # pronounced peak
            (frame_dB[x] > -20)  # or a strong freq bin relative to global max
        )}, logical(1))) + hb

    # plot(freqs, frame_dB, type = 'l')
    # points(freqs[idx_peaks], frame_dB[idx_peaks], col = 'red', pch = 3)
    specPeaks = data.frame('idx' = idx_peaks)
    nr = nrow(specPeaks)

    # parabolic interpolation to get closer to the true peak
    if (nr > 0) {
      for (i in seq_len(nr)) {
        idx_peak = specPeaks$idx[i]
        applyCorrecton = idx_peak > 1 & idx_peak < n
        if (applyCorrecton) {
          threePoints = log10(frame[(idx_peak - 1) : (idx_peak + 1)])
          parabCor = parabPeakInterpol(threePoints)
          specPeaks$freq[i] = freqs[idx_peak] + bin * parabCor$p
          specPeaks$amp[i] = 10 ^ parabCor$ampl_p
        } else {
          specPeaks$freq[i] = freqs[idx_peak]
          specPeaks$amp[i] = frame[idx_peak]
        }
      }
      # specPeaks[1:10, ]

      # indices of possible harmonics and subharmonics
      ratios = data.frame(r = seq_len(nSubh), energy = NA)
      bin_at_pitch = which.min(abs(freqs - pitch))
      lf = length(frame)
      for (r in ratios$r) {
        nToTry = min(50, floor(lf / (bin_at_pitch / r)))
        idx_h = amp_h = rep(0, nToTry)
        for (h in seq_len(nToTry)) {
          # bin_at_h = round(bin_at_pitch / r * h * c(1 - tol, 1 + tol))
          # peaks_range = which(specPeaks$freq > freqs[bin_at_h[1]] &
          #                       specPeaks$freq < freqs[bin_at_h[2]])
          freq_range = pitch / r * h * c(1 - tol, 1 + tol)
          peaks_range = which(specPeaks$freq > freq_range[1] &
                                specPeaks$freq < freq_range[2])
          # specPeaks[peaks_range, ]
          if (length(peaks_range) > 0) {
            idx_h[h] = peaks_range[which.min(abs(specPeaks$freq[peaks_range] -
                                                   mean(freq_range)))]
            amp_h[h] = specPeaks$amp[idx_h[h]]
          }
        }
        if (r == 1) {
          # save indices of f0 harmonics
          idx_pitch = idx_h[idx_h > 0]
        } else {
          # exclude f0 harmonics
          amp_h = amp_h[which(!idx_h %in% idx_pitch)]
        }
        if (length(amp_h) > 0)
          ratios$energy[r] = mean(amp_h)
        # thus: the "energy" is calculated as the mean amplitude of subharmonics
        # (excluding f0 stack)
        # plot(freqs, log(frame), type = 'l')
        # points(specPeaks$freq[idx_h], log(specPeaks$amp[idx_h]), col = 'red', pch = 3)
      }
      # ratios = na.omit(ratios)
      if (nrow(ratios) > 1) {
        idx_best = which.max(ratios$energy[-1]) + 1
        best_subh = ratios$r[idx_best]
        subDep = ratios$energy[idx_best] / ratios$energy[1]
      }
    }
  }
  c(list(subRatio = best_subh, subDep = subDep), am)
}


#' Get flux from features
#'
#' Internal soundgen function
#'
#' Calculates the change in acoustic features returned by analyze() from one
#' STFT frame to the next. Since the features are on different scales, they are
#' normalized depending on their units (but not scaled). Flux is calculated as
#' mean absolute change across all normalized features. Whenever flux exceeds
#' \code{thres}, a new epoch begins.
#' @param an dataframe of results from analyze()
#' @param thres threshold used for epoch detection (0 - 1)
#' @param smoothing_ww if > 1, \code{\link{medianSmoother}} is called on input dataframe
#' @param plot if TRUE, plots the normalized feature matrix and epochs
#' @return Returns a data frame with flux per frame and epoch numbers.
#' @keywords internal
#' @examples
#' an = analyze(soundgen(), 16000)
#' fl = soundgen:::getFeatureFlux(an$detailed, plot = TRUE)
#' \dontrun{
#' # or simply:
#' an = analyze(soundgen(sylLen = 500), 16000, plot = TRUE, ylim = c(0, 8),
#'              extraContour = 'flux', flux = list(smoothWin = 100, thres = .15))
#' }
getFeatureFlux = function(an,
                          thres = 0.1,
                          smoothing_ww = 1,
                          plot = FALSE) {
  if (nrow(an) == 1) return(data.frame(frame = 1, flux = 0, epoch = 1))
  # just work with certain "trustworthy" variables listed in soundgen:::featureFlux_vars
  m = an[, match(featureFlux_vars$feature, colnames(an))]

  # remove columns with nothing but NAs
  nc = ncol(m)
  idx = rep(FALSE, nc)
  for (i in seq_len(nc)) idx[i] = any(is.finite(m[, i]))
  col_keep = which(idx)
  if (length(col_keep) < nc) m = m[, col_keep]

  # log-transform features measured in Hz
  for (i in seq_len(ncol(m))) {
    if (featureFlux_vars$log_transform[i]) {
      m[, i] = log2(m[, i] + 1)  # +1 b/c otherwise 0 produces NA
    }
  }

  # normalize according to unit of measurement (don't z-transform because then
  # even uniform files will show spurious variation - the changes here should be
  # absolute, not relative)
  cm = colMeans(m, na.rm = TRUE)
  cm[which(colnames(m) == 'voiced')] = 0  # voiced
  for (i in seq_len(ncol(m))) {
    # if (featureFlux_vars$feature[i] != 'voiced')
    m[, i] = (m[, i] - cm[i]) / featureFlux_vars$norm_scale[i]
  }
  # m[is.na(m)] = 0   # NAs become 0 (mean)
  m$voiced = as.numeric(m$voiced)
  # summary(m)

  # median smoothing
  if (smoothing_ww > 1) {
    m = medianSmoother(m, smoothing_ww = smoothing_ww, smoothingThres = 0)
  }

  # calculate the average change from one STFT frame to the next and segment into epochs
  nFrames = nrow(m)
  flux = rep(NA, nFrames)
  epoch = rep(1, nFrames)
  for (i in 2:nFrames) {
    cor_i = cor(as.numeric(m[i, ]), as.numeric(m[i - 1, ]), use = 'complete.obs')
    flux[i] = 1 - (cor_i + 1) / 2  # cor_i = -1 gives a flux of 1, 0 -> 0.5, 1 -> 1
    if (is.finite(flux[i]) && flux[i] > thres) {
      epoch[i] = epoch[i - 1] + 1
    } else {
      epoch[i] = epoch[i - 1]
    }
  }

  # plotting
  if (plot) {
    transitions = which(diff(epoch) != 0) - 0.5
    image(as.matrix(m))
    points(seq(0, 1, length.out = length(flux)), flux, type = 'l')
    if (length(transitions) > 0) {
      for (t in transitions) abline(v = t / nFrames)
    }
  }
  data.frame(frame = seq_len(nFrames), flux = flux, epoch = epoch)
}


#' Get spectral flux
#'
#' Internal soundgen function
#'
#' Calculates spectral flux: the average change across all spectral bins from
#' one STFT frame to the next. Spectra are normalized in each frame, so
#' amplitude changes have no effect on flux.
#' @return vector of length ncol(s)
#' @param s raw spectrogram (not normalized): rows = frequency bins, columns = STFT frames
#' @keywords internal
getSpectralFlux = function(s) {
  # normalize
  s = apply(s, 2, function(x) x / max(x)) # normalize
  s[is.na(s)] = 0
  nc = ncol(s)
  flux = rep(0, nc)
  for (c in 2:nc) flux[c] = mean(abs(s[, c] - s[, c - 1]))
  # or as.numeric(dist(rbind(s[, c], s[, c - 1])))
  flux
}
