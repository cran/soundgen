#' Get HNR
#'
#' Calculates the harmonics-to-noise ratio (HNR), that is, the ratio of the
#' intensity of the harmonic component to the intensity of the noise
#' component, following Boersma (1993). Normally called internally by
#' \code{\link{analyze}}, but can also be called directly on a time series.
#'
#' @references Boersma, P. (1993). Accurate short-term analysis of the
#'   fundamental frequency and the harmonics-to-noise ratio of a sampled
#'   sound. In Proceedings of the Institute of Phonetic Sciences (Vol. 17,
#'   No. 1193, pp. 97--110).
#'
#' @param x a numeric vector (time series). Provide either \code{x} or
#'   \code{acf_x}, not both.
#' @param samplingRate sampling rate, Hz
#' @param acf_x pre-computed normalized autocorrelation of \code{x} (e.g.
#'   from \code{\link{acf_fft}}), if already available. If supplied,
#'   \code{x} and \code{wn} are ignored for ACF computation.
#' @param lag_min,lag_max minimum and maximum lag (in samples) to search
#'   for the ACF peak. Defaults: \code{lag_min = 2}, \code{lag_max =
#'   length(x) / 2} (or \code{length(acf_x)} if \code{acf_x} is given).
#' @param interpol method of refining the peak location: \code{'sinc'} =
#'   windowed sinc interpolation with Brent's search (most accurate);
#'   \code{'spline'} = cubic spline upsampling; \code{'parab'} = parabolic
#'   interpolation on three points; \code{'none'} = no interpolation.
#' @param wn window function applied to \code{x} before ACF computation
#'   (ignored when \code{acf_x} is supplied). Also used for the sinc
#'   interpolation kernel.
#' @param idx_max (internal) the lag of the ACF peak, if already known.
#'   Skips the peak search.
#' @param win_sinc (internal) a pre-computed window of length
#'   \code{2 * min(250, floor(length(acf_x) / 2))} for the sinc
#'   interpolation, to avoid rebuilding it on every call. If NULL
#'   (default), the window is computed internally.
#'
#' @return A list: \describe{
#'   \item{f0}{frequency (Hz) corresponding to the ACF peak}
#'   \item{max_acf}{height of the ACF peak, 0 to 1}
#'   \item{HNR}{harmonics-to-noise ratio in dB:
#'     \code{10 * log10(max_acf / (1 - max_acf))}}
#' }
#' @keywords internal
#' @examples
#' signal = sin(2 * pi * 150 * (1:16000) / 16000)
#' signal = signal / sqrt(mean(signal ^ 2))
#' noise = rnorm(16000)
#' noise = noise / sqrt(mean(noise ^ 2))
#' SNR = 40  # ground truth
#' s = signal + noise * 10 ^ (-SNR / 20)
#' soundgen:::getHNR(s, 16000, lag_min = 16000 / 1000,
#'   lag_max = 16000 / 75, interpol = 'none')
#' soundgen:::getHNR(s, 16000, lag_min = 16000 / 1000,
#'   lag_max = 16000 / 75, interpol = 'sinc')
getHNR = function(x = NULL,
                  samplingRate = NA,
                  acf_x = NULL,
                  lag_min = 2,
                  lag_max = NULL,
                  interpol = c('sinc', 'spline', 'parab', 'none'),
                  wn = 'hanning',
                  idx_max = NULL,
                  win_sinc = NULL) {
  interpol = match.arg(interpol)

  ## compute or validate ACF
  if (!is.null(x)) {
    len = length(x)
    n_lags = floor(len / 2)
    lag_min = round(lag_min)
    if (is.null(lag_max)) lag_max = n_lags
    lag_max = round(min(lag_max, n_lags))
    if (lag_min > n_lags)
      stop('lag_min is too large (must be <= length(x) / 2)')
    if (lag_max <= lag_min)
      stop('lag_max must be > lag_min')

    # window the signal, compute linear ACF via acf_fft, correct for window
    win = winFun(len, wn = wn)
    acf_signal = acf_fft(x * win, center = FALSE, padToMult = 2)
    acf_window = acf_fft(win, center = FALSE, padToMult = 2)
    if (length(acf_signal) == 1 && is.na(acf_signal))
      return(list(f0 = NA, max_acf = NA, HNR = NA))
    acf_x = acf_signal[1:n_lags] / acf_window[1:n_lags]
    acf_x = acf_x / acf_x[1]
  } else {
    if (is.null(acf_x))
      stop('Provide either signal (x) or its autocorrelation (acf_x)')
    if (is.null(lag_max)) lag_max = length(acf_x)
  }

  ## locate the ACF peak
  if (is.null(idx_max)) {
    idx_max = which.max(acf_x[lag_min:lag_max]) + lag_min - 1
  }
  # clamp so that parabolic interpolation always has valid neighbors
  idx_max = max(2, min(idx_max, length(acf_x) - 1))

  ## refine the peak by interpolation
  if (interpol == 'none') {
    max_acf = acf_x[idx_max]

  } else if (interpol == 'parab') {
    parabInterp = parabPeakInterpol(acf_x[(idx_max - 1):(idx_max + 1)])
    max_acf = parabInterp$ampl_p
    idx_max = idx_max + parabInterp$p

  } else if (interpol == 'spline') {
    idx = max(lag_min, idx_max - 10):min(lag_max, idx_max + 10)
    acf_ups = spline(acf_x[idx], n = length(idx) * 100)
    idx_max_ups = which.max(acf_ups$y)
    max_acf = acf_ups$y[idx_max_ups]
    idx_max = idx[1] - 1 + acf_ups$x[idx_max_ups]

  } else if (interpol == 'sinc') {
    half_win = min(250, floor(length(acf_x) / 2))
    idx_left  = max(2, idx_max - half_win)
    idx_right = min(length(acf_x), idx_max + half_win)
    acf_idx   = idx_left:idx_right
    n_left    = idx_max - idx_left
    n_right   = idx_right - idx_max

    # build (or extract) a tapered window centered on idx_max
    if (!is.null(win_sinc) && n_left + n_right + 1 <= length(win_sinc)) {
      # win_sinc is a full symmetric window of length 2 * half_win + 1;
      # extract the portion aligned with acf_idx
      centre = half_win + 1  # index of the centre in win_sinc
      win = win_sinc[(centre - n_left):(centre + n_right)]
    } else {
      win_left  = if (n_left  > 0)
        winFun(n_left  * 2, wn = wn)[1:n_left] else numeric(0)
      win_right = if (n_right > 0)
        winFun(n_right * 2, wn = wn)[(n_right + 1):(n_right * 2)] else numeric(0)
      win = c(win_left, 1, win_right)  # center point gets weight 1
    }

    opt = optimize(
      function(j) sum(acf_x[acf_idx] * sinc(j - acf_idx) * win),
      interval = c(idx_max - 2, idx_max + 2),
      maximum = TRUE
    )
    max_acf = opt$objective
    idx_max = opt$maximum
  }

  max_acf = min(max_acf, 1)  # interpolation can overshoot
  f0  = samplingRate / idx_max
  HNR = zeroOne_to_dB(max_acf)  # Boersma 1993: 10 * log10(x / (1 - x))
  list(f0 = f0, max_acf = max_acf, HNR = HNR)
}


#' Height of harmonics
#'
#' Attempts to estimate how high harmonics reach in the spectrum - that is, at
#' what frequency we can still discern peaks at multiples of f0 or, for
#' low-pitched sounds, regularly spaced peaks separated by ~f0.
#' @param pitch the final pitch estimate for the current frame
#' @param harmThres minimum height of spectral peak, dB
#' @param harmPerSel the number of harmonics per sliding selection
#' @param harmTol maximum tolerated deviation of peak frequency from multiples
#'   of f0, proportion of f0
#' @return The frequency (Hz) up to which we find harmonics.
#' @noRd
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
  frame_dB = 20 * log10(frame + 1e-12)
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
  if (is.na(lh) || lh < pitch) lh = NA
  list(harmHeight = lh,
       harmHeight_peaks = lh_peaks,
       harmHeight_dif = lh2$lastHarm_dif,
       harmHeight_cep = lh2$lastHarm_cep,
       harmSlope = lh2$harmSlope)
}


#' Height of harmonics: peaks method
#' Estimates how far harmonics reach in the spectrum by checking how many
#' spectral peaks we can find close to multiples of f0.
#' @param plot if TRUE, produces a plot of spectral peaks
#' @noRd
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
    idx_peak = which.max(frame_dB[max(1, (bin_h - harmSmooth)) :
                                    min(len_frame, (bin_h + harmSmooth))])
    bin_peak = bin_h + idx_peak - harmSmooth - 1

    # compare the peak with the mean over ±pitch to check whether the peak is
    # prominent enough
    idx_around = max(1, bin_h - pitch_bin) : (min(len_frame, bin_h + pitch_bin))
    idx_around = idx_around[idx_around != bin_peak]
    if (length(idx_around) > 0) {
      mean_around = mean(frame_dB[idx_around])
      peakFound[h] = frame_dB[bin_peak] - mean_around > harmThres
    } else {
      peakFound[h] = FALSE
    }

    if (plot)
      text(freqs[bin_peak], frame_dB[bin_peak], labels = h, pch = 5,
           col = if (peakFound[h]) 'red' else 'blue')
  }

  if (any(peakFound)) {
    absent_harm = which(!peakFound)
    if (length(absent_harm) == 0) {
      # just the last found harmonic peak
      lastHarm = pitch * nHarm
    } else if (absent_harm[1] == 1) {
      # first harmonic missing
      lastHarm = NA
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
#' Estimates how far harmonics reach in the spectrum by analyzing the typical
#' distances between spectral peaks in different frequency regions.
#' @param plot if TRUE, produces a plot of spectral peaks
#' @noRd
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
  window_starts = numeric(0)

  # find peaks in the smoothed spectrum
  hb = floor(harmSmooth_bins / 2)
  idx = findPeaks(frame_dB, wl = harmSmooth_bins, thresRel = harmThres)
  nPeaks = length(idx)

  # slide a selection along the spectrum starting from f0
  pitch_bins = pitch / bin  # f0 location in bins
  # width of selection in bins (no more than half the frame len)
  sel_bins = min(round(pitch_bins * harmPerSel), length(frame_dB) / 2)
  harmTol_bins = round(pitch_bins * harmTol)  # tolerated deviance in bins
  i = as.integer(round(pitch_bins))  # start at f0
  pitch_bin_cep = pitch_bin_peaks = vector('logical', 0)
  while (i + sel_bins < length(frame_dB)) {
    window_starts = c(window_starts, i)
    end = i + sel_bins - 1

    # count intervals b/w spectral peaks
    d = diff(idx[idx >= i & idx <= end])  # distances b/w peaks
    if (length(d) < 1) {
      dp_within_tol = FALSE
    } else {
      # median deviation of these distances from expected (f0)
      dp = abs(median(d, na.rm = TRUE) - pitch_bins)
      dp_within_tol = (dp < harmTol_bins)
    }
    pitch_bin_peaks = c(pitch_bin_peaks, dp_within_tol)

    # cepstrum
    sel = as.numeric(frame_dB[i:(i + sel_bins - 1)])
    cep = abs(fft(sel))
    # plot(sel, type = 'l')
    l = length(cep) %/% 2
    cep = cep[1:l]
    # plot(cep, type = 'l')
    bin_at_pitch = harmPerSel + 1
    # Is there a local max at bin_at_pitch? Any height will do
    peak_at_pitch = (cep[bin_at_pitch] > cep[bin_at_pitch - 1]) &&
      (cep[bin_at_pitch] > cep[bin_at_pitch + 1])
    pitch_bin_cep = c(pitch_bin_cep, peak_at_pitch)
    i = as.integer(round(i + pitch_bins))  # move the sel by one harmonic (f0)
  }

  # Find the middle frequency of the last bin with harmonics
  fbwh_peaks = which(!pitch_bin_peaks)[1]
  if (is.na(fbwh_peaks)) {
    lastHarm_dif = max(freqs)  # found everywhere - take the top frequency, not middle
  } else {
    lastHarm_dif = (sel_bins / 2 + pitch_bins * (fbwh_peaks - 1)) * bin
  }
  if (!is.na(lastHarm_dif) && lastHarm_dif < pitch) lastHarm_dif = NA

  fbwh_cep = which(!pitch_bin_cep)[1]
  if (is.na(fbwh_cep)) {
    lastHarm_cep = max(freqs)
  } else {
    lastHarm_cep = (sel_bins / 2 + pitch_bins * (fbwh_cep - 1)) * bin
  }
  if (!is.na(lastHarm_cep) && lastHarm_cep < pitch) lastHarm_cep = NA

  # calculate harmonic slope
  # (like spectral slope, but only for the confirmed harmonic peaks)
  idx_harms = integer(0)
  for (k in seq_along(pitch_bin_peaks)) {
    if (isTRUE(pitch_bin_peaks[k]) && isTRUE(pitch_bin_cep[k])) {
      ws = window_starts[k]
      we = ws + sel_bins - 1
      idx_harms = c(idx_harms, idx[idx >= ws & idx <= we])
    }
  }
  idx_harms = unique(sort(idx_harms))
  med_harm = median(c(lastHarm_dif, lastHarm_cep), na.rm = TRUE)
  if (is.finite(med_harm)) {
    last_harm_bin = ceiling(med_harm / bin)
    idx_harms = idx_harms[idx_harms < last_harm_bin]
  } else {
    idx_harms = integer(0)
  }
  if (length(idx_harms) > 1) {
    harms = data.frame(freq = freqs[idx_harms] / 1000, ampl = frame_dB[idx_harms])
    harmSlope = cov(harms$freq, harms$ampl) / var(harms$freq)
  } else {
    harmSlope = NA
  }

  if (plot) {
    plot(freqs, frame_dB, type = 'l')
    points(freqs[idx_harms], frame_dB[idx_harms], pch = 5, col = 'blue')
    points(freqs[idx[!idx %in% idx_harms]],
           frame_dB[idx[!idx %in% idx_harms]],
           pch = 5, col = 'red')
    mod = lm(ampl ~ freq, harms)
    abline(mod$coefficients[1], mod$coefficients[2] / 1000, lty = 2, col = 'blue')
  }

  list(lastHarm_cep = lastHarm_cep,
       lastHarm_dif = lastHarm_dif,
       harmSlope = harmSlope)
}


#' Energy in harmonics
#' Calculates the % of energy in harmonics based on the provided pitch estimate
#' @param pitch pitch estimates, Hz (vector)
#' @param s spectrogram (ncol = length(pitch))
#' @param coef calculate above pitch * coef
#' @param freqs as.numeric(rownames(s)) * 1000
#' @noRd
harmEnergy = function(pitch, s, freqs = NULL, coef = 1.25) {
  if (is.null(freqs)) freqs = as.numeric(rownames(s)) * 1000
  out = rep(NA, length(pitch))
  threshold = coef * pitch
  idx_notNA = which(!is.na(threshold))
  cs = colSums(s)
  out[idx_notNA] = vapply(idx_notNA, function(x) {
    if (cs[x] == 0) return(NA)
    sum(s[freqs > threshold[x], x] / cs[x])
  }, numeric(1))
  out
}


#' Subharmonics-to-harmonics ratio
#'
#' Looks for pitch candidates (among the ones already found if method =
#' 'pitchCands', or using some other pitch-tracking-like techniques such as
#' cepstrum) at integer ratios of f0. If such candidates are found, they are
#' treated as subharmonics. Note that this depends critically on accurate pitch
#' tracking.
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
#' @noRd
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
#' s2$detailed[, c('subRatio', 'subDep')]
#' }
getSHR = function(
    frame,
    bin,
    freqs,
    pitch,
    pitchCands = NULL,
    samplingRate,
    method = c('cep', 'pitchCands', 'harm'),
    nSubh = 5,
    tol = .05,
    nHarm = 5,
    harmThres = 3,
    harmTol = 0.25
) {
  # plot(freqs, log(frame), type = 'l')
  method = match.arg(method)
  best_subh = NA
  subDep = 0
  am = list(amFreq = NA, amDep = NA)
  if (method == 'pitchCands' &&
      (is.null(pitchCands) || length(pitchCands$freq) < 2)) {
    method = 'cep'
  }
  if (method == 'pitchCands') {
    ratios = data.frame(r = 1:nSubh, energy = NA)
    for (r in 1:nSubh) {
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
    cep = abs(fft(as.numeric(log(pmax(frame, 1e-6)))))
    l = length(cep) %/% 2
    seq_len_l = 1:l
    cep = cep[seq_len_l]
    cep[1] = 0
    freqs_cep = samplingRate / seq_len_l / 2
    # plot(freqs_cep, cep, type = 'b', log = 'x')
    bin_at_pitch = which.min(abs(freqs_cep - pitch))
    nToTry = min(nSubh, floor(l / bin_at_pitch))
    ratios = data.frame(r = 1:nToTry, energy = NA)
    for (r in ratios$r) {
      ratios$energy[r] = max(cep[(bin_at_pitch * r - 1) :
                                   min(l, (bin_at_pitch * r + 1))])
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
  } else if (method == 'harm') {
    am = list(amFreq = NA, amDep = NA)
    keep_idx = which(freqs < (pitch * nHarm))
    frame = frame[keep_idx]
    frame = frame / max(frame)
    frame_dB = 20 * log10(frame + 1e-12)
    freqs = freqs[keep_idx]
    n = length(keep_idx)
    # plot(freqs[keep_idx], frame_dB, type = 'l')

    # look for spectral peaks
    hb = 1
    idx_peaks = findPeaks(frame_dB, wl = 3, thres = -20,
                          thresRel = harmThres, dropFirstLast = TRUE)
    # plot(freqs, frame_dB, type = 'l')
    # points(freqs[idx_peaks], frame_dB[idx_peaks], col = 'red', pch = 8)
    specPeaks = data.frame('idx' = idx_peaks)
    nr = nrow(specPeaks)

    if (nr > 0) {
      for (i in 1:nr) {
        idx_peak = specPeaks$idx[i]
        applyCorrection = idx_peak > 1 & idx_peak < n
        if (applyCorrection) {
          # parabolic interpolation to get closer to the true peak
          threePoints = log10(frame[(idx_peak - 1) : (idx_peak + 1)] + 1e-10)
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
      ratios = data.frame(r = 1:nSubh, energy = NA)
      bin_at_pitch = which.min(abs(freqs - pitch))
      lf = length(frame)
      for (r in ratios$r) {
        nToTry = min(50, floor(lf / (bin_at_pitch / r)))
        idx_h = amp_h = rep(0, nToTry)
        for (h in 1:nToTry) {
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
          amp_h = amp_h[which(idx_h > 0 & !idx_h %in% idx_pitch)]
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
        if (length(idx_best) > 0) {
          best_subh = ratios$r[idx_best]
          subDep = ratios$energy[idx_best] / ratios$energy[1]
        }
      }
    }
  }
  c(list(subRatio = best_subh, subDep = subDep), am)
}


#' Get flux from features
#' Calculates the change in acoustic features returned by analyze() from one
#' STFT frame to the next. Since the features are on different scales, they are
#' normalized depending on their units (but not scaled). Flux is calculated as
#' mean absolute change across all normalized features. Whenever flux exceeds
#' \code{thres}, a new epoch begins.
#' @param an dataframe of results from analyze()
#' @param thres threshold used for epoch detection (0 - 1)
#' @param smoothing_ww if > 1, \code{\link{medianSmoother}} is called on input dataframe
#' @param plot if TRUE, plots the normalized feature matrix and epochs
#' @return A dataframe with flux per frame and epoch numbers.
#' @noRd
#' @examples
#' \dontrun{
#' s = soundgen()
#' an = analyze(s, 16000)
#' fl = soundgen:::getFeatureFlux(an$detailed, plot = TRUE)
#'
#' # or simply:
#' an = analyze(s, 16000, plot = TRUE, ylim = c(0, 8),
#'              extraContour = 'flux', flux = list(smoothWin = 100, thres = .15))
#' }
getFeatureFlux = function(an,
                          thres = 0.1,
                          smoothing_ww = 1,
                          plot = FALSE) {
  if (nrow(an) == 1) return(data.frame(frame = 1, flux = 0, epoch = 1))
  # just work with certain "trustworthy" variables listed in soundgen:::featureFlux_vars
  m = an[, which(colnames(an) %in% featureFlux_vars$feature), drop = FALSE]

  # remove columns with nothing but NAs
  idx_rm = which(apply(m, 2, function(x) all(is.na(x))))
  if (length(idx_rm) > 1) m = m[, -idx_rm, drop = FALSE]
  fv = featureFlux_vars[match(colnames(m), featureFlux_vars$feature), ]

  # log-transform features measured in Hz
  cols_to_log = fv$feature[fv$log_transform]
  m[, cols_to_log] = log2(m[, cols_to_log] + 1)  # # +1 b/c otherwise 0 produces NA

  # normalize according to unit of measurement (don't z-transform because then
  # even uniform files will show spurious variation - the changes here should be
  # absolute, not relative)
  cm = colMeans(m, na.rm = TRUE)
  if ("voiced" %in% names(cm)) cm["voiced"] = 0  # voiced
  for (i in seq_len(ncol(m))) {
    # if (fv$feature[i] != 'voiced')
    m[, i] = (m[, i] - cm[i]) / fv$norm_scale[i]
  }
  # m[is.na(m)] = 0   # NAs become 0 (mean)
  # m$voiced = as.numeric(m$voiced)
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
    if (is.na(cor_i)) {
      # constant or zero-variance features -> no flux
      flux[i] = 0
    } else {
      # cor_i = -1 gives a flux of 1, 0 -> 0.5, 1 -> 0
      flux[i] = 1 - (cor_i + 1) / 2
    }
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
  data.frame(frame = 1:nFrames, flux = flux, epoch = epoch)
}


#' Get spectral flux
#' Calculates spectral flux: the average change across all spectral bins from
#' one STFT frame to the next. If spectra are normalized in each frame,
#' amplitude changes have no effect on flux.
#' @return a non-negative numeric vector of length \code{ncol(s)}
#' @param s raw spectrogram (not normalized): rows = frequency bins, columns = STFT frames
#' @noRd
getSpectralFlux = function(s, normalize = FALSE, takeLog = TRUE, eps = 1e-6) {
  if (normalize)
    s = apply(s, 2, function(x) x / max(x, eps)) # normalize, avoid /0
  s[is.na(s)] = 0
  mx = max(s)
  if (mx == 0) return(rep(0, ncol(s)))
  if (takeLog) {
    eps = eps * mx
    s = log(pmax(s, eps))
  }
  nc = ncol(s)
  flux = rep(0, nc)
  flux[2:nc] = colMeans(abs(s[, 2:nc, drop=FALSE] - s[, 1:(nc-1), drop=FALSE]))
  # or as.numeric(dist(rbind(s[, c], s[, c - 1])))
  flux
}
