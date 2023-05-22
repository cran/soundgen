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
  return(list(harmHeight = lh,
              harmHeight_peaks = lh_peaks,
              harmHeight_dif = lh2$lastHarm_dif,
              harmHeight_cep = lh2$lastHarm_cep,
              harmSlope = lh2$harmSlope))
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

  return(lastHarm)
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
  temp = zoo::rollapply(
    zoo::as.zoo(frame_dB),
    width = harmSmooth_bins,
    align = 'center',
    function(x) {
      middle = ceiling(length(x) / 2)
      median_around = median(x[-middle])
      which.max(x) == middle &   # peak in the middle
        x[middle] - median_around > harmThres  # and high enough
    }
  )
  idx = zoo::index(temp)[zoo::coredata(temp)]
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
    cep = cep[1:l]
    # plot(cep, type = 'l')
    bin_at_pitch = harmPerSel + 1
    # Is there a local max at bin_at_pitch? Any height will do
    peak_at_pitch = (cep[bin_at_pitch] > cep[bin_at_pitch - 1]) &
      (cep[bin_at_pitch] > cep[bin_at_pitch + 1])
    pitch_bin_cep = c(pitch_bin_cep, peak_at_pitch)

    i = round(i + pitch_bins)  # move the sel by one harmonic (f0)
  }

  # Find the middle frequency of the first bin w/o harmonics
  fbwh_peaks = which(!pitch_bin_peaks)[1]
  if (is.na(fbwh_peaks)) {
    lastHarm_dif = nPeaks * pitch  # found everywhere - take the top frequency, not middle
  } else {
    lastHarm_dif = (sel_bins / 2 + pitch_bins * (fbwh_peaks - 1)) * bin
  }
  if (!is.na(lastHarm_dif) && lastHarm_dif < pitch) lastHarm_dif = NA

  fbwh_cep = which(!pitch_bin_cep)[1]
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

  return(list(lastHarm_cep = lastHarm_cep,
              lastHarm_dif = lastHarm_dif,
              harmSlope = harmSlope))
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
  threshold = coef * pitch
  he = apply(matrix(1:ncol(s)), 1, function(x) {
    ifelse(is.na(threshold[x]),
           NA,
           sum(s[freqs > threshold[x], x]) / sum(s[, x]))
  })
  return(he)
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
#' s400 = soundgen(
#'   sylLen = 300, pitch = c(280, 370, 330),
#'   subDep = list(
#'     time = c(0, .5, .51, 1),
#'     value = c(0, 0, 10, 10)
#'   ), subRatio = 3,
#'   smoothing = list(interpol = 'approx'), formants = 'a',
#'   rolloff = -12, addSilence = 50, temperature = .001,
#' )
#' s = analyze(s400, samplingRate = 16000,
#'             windowLength =  50, step = 10,
#'             pitchMethods = c('dom', 'autocor', 'hps'), priorMean = NA,
#'             plot = TRUE, ylim = c(0, 3),
#'             extraContour = list('subDep', type = 'b', col = 'brown'))
#' s$detailed[, c('subRatio', 'subDep')]
subhToHarm = function(
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
    cep = abs(fft(as.numeric(log(frame))))
    l = length(cep) %/% 2
    cep = cep[1:l]
    cep[1] = 0
    freqs_cep = samplingRate / (1:l) / 2
    # plot(freqs_cep, cep, type = 'b', log = 'x')
    bin_at_pitch = which.min(abs(freqs_cep - pitch))
    nToTry = min(nSubh, floor(l / bin_at_pitch))
    ratios = data.frame(r = 1:nToTry, energy = NA)
    for (r in 1:nToTry) {
      ratios$energy[r] = max(cep[(bin_at_pitch * r - 1) : (bin_at_pitch * r + 1)])
    }
    ratios$extraEnergy = ratios$energy - ratios$energy[1] / ratios$r
    subR = na.omit(ratios[ratios$extraEnergy > 0, ])
    if (nrow(subR) > 0) {
      best_subh = subR$r[which.max(subR$extraEnergy)]
      subDep = ratios$extraEnergy[best_subh] / ratios$energy[best_subh]
      subDep[subDep > 1] = 1
      # ad hoc correction to linearize subDep - from simulations with known
      # soundgen(subDep = ...), mod = nls(subDep ~ exp(b * m + c), data = out1,
      # start = list(b = 1, c = 0))  See validate_subDep.R
      subDep = exp(5 * subDep - 5)
    }

    ## calculate AM (cepstral peaks in amRange not harmonically related to pitch)
    # cancel pitch harmonics
    cep1 = cep
    for (i in 1:min(10, floor(l / bin_at_pitch))) {
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
    temp = zoo::rollapply(zoo::as.zoo(frame_dB),
                          width = 3,  # parameter - see hps or smth
                          align = 'center',
                          function(x) {
                            middle = ceiling(length(x) / 2)
                            which.max(x) == middle && (  # local maximum
                              (x[middle] - min(x) > harmThres) |  # pronounced peak
                                (x[middle] > -20)  # or a strong freq bin relative to global max
                            )
                          })
    idx_peaks = zoo::index(temp)[zoo::coredata(temp)]
    # plot(freqs, frame_dB, type = 'l')
    # points(freqs[idx_peaks], frame_dB[idx_peaks], col = 'red', pch = 3)
    specPeaks = data.frame('idx' = idx_peaks)
    nr = nrow(specPeaks)

    # parabolic interpolation to get closer to the true peak
    if (nr > 0) {
      for (i in 1:nr) {
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
      ratios = data.frame(r = 1:nSubh, energy = NA)
      bin_at_pitch = which.min(abs(freqs - pitch))
      lf = length(frame)
      for (r in 1:nSubh) {
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
  return(c(list(subRatio = best_subh, subDep = subDep), am))
}
