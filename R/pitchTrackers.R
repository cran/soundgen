#' Get lowest dominant frequency band
#'
#' Internal soundgen function.
#'
#' Calculate the lowest frequency band in the spectrum above pitchFloor whose
#' amplitude exceeds a certain threshold.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param domThres (0 to 1) to find the lowest dominant frequency band, we
#'   do short-term FFT and take the lowest frequency with amplitude at least
#'   domThres
#' @param domSmooth the width of smoothing interval (Hz) for finding
#'   \code{dom}
#' @return Returns a list of $dom (NA or numeric) and $dom_array
#'   (either NULL or a dataframe of pitch candidates).
#' @keywords internal
getDom = function(frame,
                  bin,
                  freqs,
                  domSmooth,
                  domThres,
                  pitchFloor,
                  pitchCeiling
) {
  dom_array = data.frame(
    'pitchCand' = numeric(),
    'pitchCert' = numeric(),
    'pitchSource' = character(),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  dom = NA
  # width of smoothing interval (in bins), forced to be an odd number
  domSmooth_bins = 2 * ceiling(domSmooth / bin / 2) - 1

  # find peaks in the smoothed spectrum (much faster than seewave::fpeaks)
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = domSmooth_bins,
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = domThres)
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  pitchFloor_idx = max(1, which(freqs > pitchFloor)[1], na.rm = TRUE)
  pitchCeiling_idx = min(length(frame), which(freqs > pitchCeiling)[1], na.rm = TRUE)
  idx_peak = idx[which(idx > pitchFloor_idx & idx < pitchCeiling_idx)[1]]
  # parabolic interpolation to get closer to the true peak
  applyCorrection = !is.na(idx_peak) &&
    length(idx_peak) == 1 &&
    (idx_peak > 1 & idx_peak < length(frame))
  if (applyCorrection) {
    threePoints = log10(frame[(idx_peak - 1) : (idx_peak + 1)])
    parabCor = parabPeakInterpol(threePoints)
    dom = freqs[idx_peak] + bin * parabCor$p
    dom_ampl = 10 ^ parabCor$ampl_p
    if (dom_ampl > 1) dom_ampl = 1  # cap at 1
  } else {
    dom = freqs[idx_peak]
    dom_ampl = frame[idx_peak]
  }

  if (length(idx) > 0) {
    # lowest dominant freq band - we take the first frequency in the spectrum at
    # least /domThres/ % of the amplitude of peak frequency, but high
    # enough to be above pitchFloor (and below pitchCeiling)
    dom_array = data.frame(
      'pitchCand' = dom,
      'pitchCert' = dom_ampl,
      'pitchSource' = 'dom',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  return(list(dom_array = dom_array, dom = dom))
}


#' Autocorrelation pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by looking for peaks in the autocorrelation
#' function (time domain analysis). Modified PRAAT's algorithm. See Boersma, P.
#' (1993). Accurate short-term analysis of the fundamental frequency and the
#' harmonics-to-noise ratio of a sampled sound. In Proceedings of the institute
#' of phonetic sciences (Vol. 17, No. 1193, pp. 97-110).
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @inheritParams getHNR
#' @param autocorThres voicing threshold (unitless, ~0 to 1)
#' @param autocorSmooth the width of smoothing interval (in bins) for
#'   finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'   rate 44100 and smaller odd numbers for lower values of sampling rate
#' @param autocorUpsample upsamples acf to this resolution (Hz) to improve
#'   accuracy in high frequencies
#' @param autocorBestPeak amplitude of the lowest best candidate relative to the
#'   absolute max of the acf
#' @return Returns a list of $HNR (NA or numeric) and $pitchAutocor_array
#'   (either NULL or a dataframe of pitch candidates).
#' @keywords internal
getPitchAutocor = function(autoCorrelation,
                           samplingRate,
                           nCands,
                           autocorThres,
                           autocorSmooth = NULL,
                           autocorUpsample,
                           autocorBestPeak,
                           pitchFloor,
                           pitchCeiling,
                           interpol = 'sinc',
                           wn = 'hanning') {
  # autoCorrelation = autocorBank[, 13]
  pitchAutocor_array = NULL
  HNR = NA

  # don't consider candidates above nyquist / 2 b/c there's not enough
  # resolution in acf that high up
  pitchCeiling = min(pitchCeiling, samplingRate / 4)

  orig = data.frame('freq' = as.numeric(names(autoCorrelation)),
                    'amp' = autoCorrelation)
  rownames(orig) = NULL
  a = orig[orig$freq > pitchFloor &
             orig$freq < pitchCeiling, , drop = FALSE]
  # plot(a$freq, a$amp, type='b', log = 'x')

  # upsample to improve resolution in higher frequencies
  if (autocorUpsample > 0) {
    upsample_from_bin = 1  # in Hz, it's samplingRate / (upsample_from_bin + 1)
    # same as which(a$freq < samplingRate / 4)[1], etc.
    upsample_to_bin = which(diff(a$freq) > -autocorUpsample)[1]
    upsample_len = round((a$freq[upsample_from_bin] - a$freq[upsample_to_bin]) /
                           autocorUpsample)
    if (!is.na(upsample_len) &&
        pitchCeiling > a$freq[upsample_to_bin] & upsample_len > 1) {
      # Boersma recommends interpolating with sin(x) / x, but here we simply
      # call spline() - completely agnostic
      temp = spline(a$amp[upsample_from_bin:upsample_to_bin],
                    n = upsample_len,
                    x = a$freq[upsample_from_bin:upsample_to_bin])
      # points(temp$x, temp$y, type = 'p', cex = .25, col = 'red')
      a = rbind(a[1:upsample_from_bin, ],
                data.frame(freq = rev(temp$x), amp = rev(temp$y)),
                a[(upsample_to_bin + 1):nrow(a), ])
    }
  }

  # HNR = max(a$amp) # HNR is here defined as the maximum autocorrelation
  # within the specified pitch range. It is also measured for the frames which
  # are later classified as unvoiced (i.e. HNR can be <voicedThres)

  # find peaks in the corrected autocorrelation function
  a_zoo = zoo::as.zoo(a$amp)
  temp = zoo::rollapply(a_zoo,
                        width = autocorSmooth,
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = autocorThres)
                          # width = 7 chosen by optimization, but it doesn't make that much difference anyhow
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  autocorPeaks = a[idx, ]

  if (nrow(autocorPeaks) > 0) {
    # if some peaks are found...
    # we are only interested in frequencies above half of the best candidate
    # (b/c otherwise we get false subharmonics)
    autocorPeaks$amp[autocorPeaks$amp > 1] = 1
    idx_bestFreq = which(autocorPeaks$amp > (autocorBestPeak * max(autocorPeaks$amp)))[1]
    bestFreq = autocorPeaks$freq[idx_bestFreq]
    # bestFreq = autocorPeaks$freq[which.max(autocorPeaks$amp)]
    if (!is.na(bestFreq)) {
      autocorPeaks = try(autocorPeaks[autocorPeaks$freq > bestFreq / 1.8,
                                      , drop = FALSE], silent = TRUE)
      # otherwise we get false subharmonics
      autocorPeaks = try(autocorPeaks[order(autocorPeaks$amp, decreasing = TRUE),
                                      , drop = FALSE], silent = TRUE)
    }
    if (!inherits(autocorPeaks, 'try-error')) {
      if (nrow(autocorPeaks) > 0) {
        # if some peaks satisfy all criteria, return them:
        nr_an = min(nrow(autocorPeaks), nCands)
        for (r in 1:nr_an) {
          gh_r = getHNR(acf_x = autoCorrelation,
                        samplingRate = samplingRate,
                        lag.min = round(samplingRate / pitchCeiling),
                        lag.max = round(samplingRate / pitchFloor),
                        idx_max = round(samplingRate / autocorPeaks$freq[r]),
                        interpol = interpol,
                        wn = wn)
          paa = try(rbind(
            pitchAutocor_array,
            data.frame('pitchCand' = gh_r$f0,
                       'pitchCert' = gh_r$max_acf,
                       'pitchSource' = 'autocor',
                       stringsAsFactors = FALSE,
                       row.names = NULL)
          ))
          if (!inherits(paa, 'try-error'))
            pitchAutocor_array = paa
        }
        HNR = max(pitchAutocor_array$pitchCert)
      }
    }
  }
  return(list(pitchAutocor_array = pitchAutocor_array,
              HNR = HNR))
}


#' Cepstral pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by looking for peaks in the cepstrum.
#' See http://www.phon.ucl.ac.uk/courses/spsci/matlab/lect10.html
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param cepThres voicing threshold (unitless, ~0 to 1)
#' @param cepZp zero-padding of the spectrum used for cepstral pitch detection
#'   (final length of spectrum after zero-padding in points, e.g. 2 ^ 13)
#' @param tol tolerance when removing false subharmonics
#' @param specMerge tolerance when removing similar candidates, oct
#' @return Returns either NULL or a dataframe of pitch candidates and CPP.
#' @keywords internal
getPitchCep = function(frame,
                       samplingRate,
                       bin,
                       nCands,
                       cepThres,
                       cepZp,
                       pitchFloor,
                       pitchCeiling,
                       tol = .05,
                       specMerge = 2/12) {
  pitchCep_array = NULL

  if (cepZp < length(frame)) {
    frameZP = frame
  } else {
    zp = rep(0, (cepZp - length(frame)) / 2)
    frameZP = c(zp, frame, zp)
  }
  # if (!is.null(logSpec) && !is.na(logSpec) && logSpec)
  frameZP = log(frameZP + 1e-6) # 1e-6 is -120 dB
  # plot(frameZP, type = 'l', log = 'x')
  # plot(as.numeric(names(frameZP)), frameZP, type = 'l', log = 'x')

  # ifft of log-fft = cepstrum
  cepstrum = abs(fft(as.numeric(frameZP), inverse = TRUE))
  # cepstrum = Re(fft(as.numeric(frameZP)))  # basically the same
  l = length(cepstrum) %/% 2

  # calculate quefrencies
  zp_corr = (length(frameZP) / length(frame))
  # bin = diff(as.numeric(names(frameZP)[1:2])) * 1000
  bin_q = 1 / bin / length(cepstrum) * zp_corr
  q = (0:(l - 1)) * bin_q
  f = 1 / q
  cepstrum = cepstrum[1:l]
  # plot(q[-1], cepstrum[-1], type = 'l')
  # plot(f[-1], cepstrum[-1], type = 'l', log = 'x')

  # focus on the range of frequencies from pitchFloor to pitchCeiling
  idx_keep = which(f >= pitchFloor & f <= pitchCeiling)
  b = data.frame(
    idx = idx_keep,
    q = q[idx_keep],
    freq = f[idx_keep], # samplingRate / idx_keep * zp_corr,  # smth fishy here...
    cep = cepstrum[idx_keep]
  )
  # plot(b$freq, b$cep, type = 'l', log = 'x')

  # find local maxima
  idx = which(diff(diff(b$cep) > 0) == -1) + 1

  # if some peaks are found...
  if (length(idx) > 0) {
    # fit a regression line to cepstrum in the target range of pitch freqs
    regr = lm(cep ~ freq, b[-idx, ])  # remove peaks when fitting regression
    pred = predict(regr, newdata = b)
    pred[pred < 0] = min(b$cep)  # otherwise log(negative number) = error for CPP
    # plot(b$freq, b$cep, type = 'l', log = 'x')
    # points(b$freq, pred, type = 'l', col = 'blue')
    # plot(b$q, b$cep, type = 'l')
    # points(b$q, pred, type = 'l', col = 'blue')

    cepPeaks = b[idx, ]

    # parabolic interpolation to improve resolution
    for (i in 1:nrow(cepPeaks)) {
      idx_peak = which(b$idx == cepPeaks$idx[i])
      applyCorrecton = idx_peak > 1 & idx_peak < l
      if (applyCorrecton) {
        threePoints = b$cep[(idx_peak - 1) : (idx_peak + 1)]
        parabCor = parabPeakInterpol(threePoints)
        cepPeaks$q[i] = (cepPeaks$idx[i] - 1 + parabCor$p) * bin_q * zp_corr
        cepPeaks$cep[i] = parabCor$ampl_p
      }
    }
    cepPeaks$freq = 1 / cepPeaks$q  # need to recalculate from adjusted q

    # calculate Cepstral Peak Prominence
    cepPeaks$CPP = 20 * log10(cepPeaks$cep / pred[idx])

    # remap CPP in dB to pitchCert on a [0, 1] scale
    # a = seq(0, 50, length.out = 500)
    # b = 1 / (1 + exp(-log2(a / 6)))
    # plot(a, b)  # 0.5 at 6 dB, doubles for every 6 dB
    CPP_non_neg = cepPeaks$CPP
    CPP_non_neg[CPP_non_neg <= 1e-6] = 1e-6
    cepPeaks$pitchCert = 1 / (1 + exp(-log2(CPP_non_neg / 6)))
    cepPeaks = cepPeaks[which(cepPeaks$pitchCert > cepThres), ]
    nr = nrow(cepPeaks)

    if (nr > 0) {
      # remove harmonic quefrencies, keeping only the highest (rather hacky;
      # improves accuracy for high f0 at some cost for low f0)
      if (is.finite(tol)) {
        idx_remove = numeric(0)
        for (p in 1:min(nr, 5)) {  # if more than 5 first peaks, risk removing everything in low freqs
          ratios = cepPeaks$freq[p] / cepPeaks$freq[(p + 1):nr]
          ratios_int = round(ratios)
          idx_remove = c(idx_remove, p + which(
            ratios < 4 &   # again, otherwise removes too much in low freqs
              abs(ratios - ratios_int) < tol))
        }
        if (length(idx_remove) > 0)
          cepPeaks = cepPeaks[-unique(idx_remove), ]
      }

      # remove similar pitch candidates (double peaks)
      c = 1
      while (c + 1 <= nrow(cepPeaks)) {
        if (abs(log2(cepPeaks$freq[c] /
                     cepPeaks$freq[c + 1])) < specMerge) {
          if (cepPeaks$CPP[c] > cepPeaks$CPP[c + 1]) {
            cepPeaks = cepPeaks[-(c + 1), ]
          } else {
            cepPeaks = cepPeaks[-c, ]
          }
        } else {
          c = c + 1
        }
      }

      # save nCands best candidates
      ord = order(cepPeaks$CPP, decreasing = TRUE)
      cepPeaks = cepPeaks[ord[1:nCands], ]

      pitchCep_array = data.frame(
        'pitchCand' = cepPeaks$freq,
        'pitchCert' = cepPeaks$pitchCert,
        'pitchSource' = 'cep',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  }
  return(pitchCep_array)
}

#' Get Cepstral Peak Prominence
#'
#' Internal soundgen function.
#'
#' Calculates Cepstral Peak Prominence from the spectrum of an STFT frame.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param pitch pitch of this frame, Hz
#' @param bin spectral frequency bin width, Hz
#' @return Returns either NA or CPP in dB.
#' @keywords internal
getCPP = function(frame,
                  samplingRate,
                  pitch,
                  bin,
                  prox_semitones = 3) {
  CPP = NA
  log_spectrum = log(frame + 1e-6) # 1e-6 is -120 dB
  # plot(log_spectrum, type = 'l', log = 'x')
  # plot(as.numeric(names(log_spectrum)), log_spectrum, type = 'l', log = 'x')

  # cepstrum is ifft of log-spectrum
  cepstrum = abs(fft(as.numeric(log_spectrum), inverse = TRUE))
  # cepstrum = Re(fft(as.numeric(frame)))  # basically the same
  l = length(cepstrum) %/% 2

  # calculate quefrencies
  # bin = diff(as.numeric(names(log_spectrum)[1:2])) * 1000
  bin_q = 1 / bin / length(cepstrum)
  q = (0:(l - 1)) * bin_q
  f = 1 / q
  cepstrum = cepstrum[1:l]
  # plot(q[-1], cepstrum[-1], type = 'l')
  # plot(f[-1], cepstrum[-1], type = 'l', log = 'x')

  b = data.frame(
    idx = 2:l,  # discard the lowest freq (first q value)
    q = q[-1],
    freq = f[-1], # samplingRate / idx_keep * zp_corr,  # smth fishy here...
    cep = cepstrum[-1]
  )
  # plot(b$freq, b$cep, type = 'l', log = 'x')

  # find local maxima
  mult = 2 ^ (prox_semitones / 12)
  idx_target = which(b$freq > (pitch / mult) &
                       b$freq < (pitch * mult))
  local_peaks = which(diff(diff(b$cep[idx_target]) > 0) == -1) + idx_target[1]

  # if some peaks are found...
  if (length(local_peaks) > 0) {
    # fit a regression line to the entire cepstrum
    all_peaks = which(diff(diff(b$cep[idx_target]) > 0) == -1) + 1
    regr = lm(cep ~ freq, b[-all_peaks, ])  # remove peaks when fitting regression
    pred = predict(regr, newdata = b)
    pred[pred < 0] = min(b$cep)  # otherwise log(negative number) = error for CPP
    # plot(b$freq, b$cep, type = 'l', log = 'x')
    # points(b$freq, pred, type = 'l', col = 'blue')
    # plot(b$q, b$cep, type = 'l')
    # points(b$q, pred, type = 'l', col = 'blue')

    cepPeaks = b[local_peaks, ]

    # parabolic interpolation to improve resolution
    for (i in 1:nrow(cepPeaks)) {
      idx_peak = which(b$idx == cepPeaks$idx[i])
      applyCorrecton = idx_peak > 1 & idx_peak < l
      if (applyCorrecton) {
        threePoints = b$cep[(idx_peak - 1) : (idx_peak + 1)]
        parabCor = parabPeakInterpol(threePoints)
        cepPeaks$q[i] = (cepPeaks$idx[i] - 1 + parabCor$p) * bin_q
        cepPeaks$cep[i] = parabCor$ampl_p
      }
    }
    cepPeaks$freq = 1 / cepPeaks$q  # need to recalculate from adjusted q
    cepPeaks$CPP = 20 * log10(cepPeaks$cep / pred[cepPeaks$idx])
    CPP = max(cepPeaks$CPP)
  }
  return(CPP)
}


#' BaNa pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by detecting several putative harmonics and
#' either finding their highest common factor (specMethod = "commonFactor") or
#' comparing their ratios (specMethod = "BaNa"). For the highest common factor
#' method, see Howard & Angus (2017) "Acoustics and psychoacoustics" (section
#' 3.2.1). For BaNa, see Ba et al. (2012) "BaNa: A hybrid approach for noise
#' resilient pitch detection." Statistical Signal Processing Workshop (SSP),
#' 2012 IEEE.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param bin the width of spectral bin in \code{frame}, Hz
#' @param HNR harmonics-to-noise ratio returned by \code{\link{getPitchAutocor}}
#' @param specMethod "commonFactor" = highest common factor of putative
#'   harmonics, "BaNa" = ratio of putative harmonics
#' @param specRatios for method = "commonFactor", the number of harmonics AND
#'   integer fractions to consider
#' @param specMerge pitch candidates within \code{specMerge} semitones are
#'   merged with boosted certainty
#' @param specThres voicing threshold (unitless, ~0 to 1)
#' @param specPeak,specHNRslope when looking for putative harmonics in
#'   the spectrum, the threshold for peak detection is calculated as
#'   \code{specPeak * (1 - HNR * specHNRslope)}
#' @param specSmooth the width of window for detecting peaks in the spectrum, Hz
#' @param specSinglePeakCert (0 to 1) if f0 is calculated based on a single
#'   harmonic ratio (as opposed to several ratios converging on the same
#'   candidate), its certainty is taken to be \code{specSinglePeakCert}
#' @param nCands number of pitch candidates pre frame (specMethod =
#'   "commonFactor" always returns a single candidate)
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchSpec = function(frame,
                        bin,
                        freqs,
                        specMethod = c('commonFactor', 'BaNa')[1],
                        specRatios,
                        specSmooth,
                        specThres,
                        specMerge,
                        specPeak,
                        specHNRslope,
                        HNR = NULL,
                        specSinglePeakCert,
                        pitchFloor,
                        pitchCeiling,
                        nCands
) {
  if (specMethod == 'commonFactor') nCands = 1
  # nCands = 1, otherwise returns subh with the same apparent certainty
  pitchSpec_array = NULL
  n = length(frame)
  width = max(3, 2 * ceiling((specSmooth / bin + 1) * 20 / bin / 2) - 1)
  # to be always ~100 Hz, regardless of bin, but an odd number
  if (!is.numeric(HNR)) {
    specPitchThreshold = specPeak # if HNR is NA, the sound is
    # probably a mess, so we play safe by only looking at very strong harmonics
  } else {
    # for noisy sounds the threshold is high to avoid false sumharmonics etc,
    # for tonal sounds it is low to catch weak harmonics
    specPitchThreshold = specPeak * (1 - HNR * specHNRslope)
  }

  # find peaks in the spectrum (hopefully harmonics)
  # plot(freqs, frame, type = 'l')
  # plot(freqs, frame, type = 'l', log = 'xy')
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = width,
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = specPitchThreshold)
                          # plot(zoo::as.zoo(frame), type='l')
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  specPeaks = data.frame('idx' = idx)
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
    specPeaks = specPeaks[specPeaks$freq > pitchFloor, ]
  }

  nr = nrow(specPeaks)
  if (nr == 1) {
    if (specPeaks$freq < pitchCeiling & specPeaks$freq > pitchFloor) {
      pitchSpec = specPeaks$freq
      pitchCert = specSinglePeakCert
      pitchSpec_array = data.frame(
        'pitchCand' = pitchSpec,
        'pitchCert' = pitchCert,
        'pitchSource' = 'spec',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  } else if (nr > 1) {
    if (specMethod == 'commonFactor') {
      # analyze specRatios lowest harmonics
      specPeaks = specPeaks[1:min(specRatios, nrow(specPeaks)), ]
      # Find all possible integer fractions of these putative harmonics.
      # True pitch is the largest common factor
      pitchCand = as.numeric(apply(specPeaks[, 'freq', drop = FALSE], 1,
                                   function(x) x / (1:specRatios)))
    } else if (specMethod == 'BaNa') {
      # A modified version of BaNa algorithm follows
      # analyze five lowest harmonics
      specPeaks = specPeaks[1:min(5, nrow(specPeaks)), ]

      seq1 = 2:nrow(specPeaks)
      seq2 = 1:(nrow(specPeaks) - 1)
      n = length(seq1) * length(seq2)
      temp = data.frame (
        'harmonicA' = rep(0, n),
        'harmonicB' = rep(0, n),
        'AtoB_ratio' = rep(0, n)
      )
      counter = 1
      for (i in seq1) {
        for (j in seq2) {
          temp$harmonicA[counter] = i
          temp$harmonicB[counter] = j
          # get ratios of discovered harmonics to each other
          temp$AtoB_ratio[counter] = specPeaks$freq[i] / specPeaks$freq[j]
          counter = counter + 1
        }
      }
      temp = temp[temp$harmonicA > temp$harmonicB, ]

      pitchCand = numeric()
      for (i in 1:nrow(temp)) {
        # for each ratio that falls within the limits specified outside this
        # function in a dataframe called "ratios", calculate the corresponding
        # pitch. If several ratios suggest the same pitch, that's our best guess
        idx = which(temp$AtoB_ratio[i] > BaNaRatios$value_low &
                      temp$AtoB_ratio[i] < BaNaRatios$value_high)
        divLow = BaNaRatios$divide_lower_by[idx]
        pitchCand = c(pitchCand,
                      as.numeric(specPeaks$freq[temp$harmonicB[i]] / divLow))
      }
      # add pitchCand based on the most common distances between harmonics
      # pitchCand = c(pitchCand, diff(specPeaks[,1]))
    }
    pitchCand = sort(pitchCand[pitchCand > pitchFloor &
                                 pitchCand < pitchCeiling])
    if (length(pitchCand) > 0) {
      pitchSpec_array = data.frame(
        'pitchCand' = pitchCand,
        'specAmplIdx' = 1,
        'pitchSource' = 'spec',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      c = 1
      while (c + 1 <= nrow(pitchSpec_array)) {
        if (abs(log2(pitchSpec_array$pitchCand[c] /
                     pitchSpec_array$pitchCand[c + 1])) < specMerge / 12) {
          # merge cands within specMerge into one "super-candidate"
          # and give this new super-candidate a certainty boost
          pitchSpec_array$specAmplIdx[c] = pitchSpec_array$specAmplIdx[c] + 1
          pitchSpec_array$pitchCand[c] = mean(c(
            pitchSpec_array$pitchCand[c],
            pitchSpec_array$pitchCand[c + 1]
          ))
          pitchSpec_array = pitchSpec_array[-(c + 1), ]
        } else {
          c = c + 1
        }
      }
      pitchSpec_array = pitchSpec_array[pitchSpec_array$specAmplIdx > 1, ]
      pitchSpec_array$pitchCert = specSinglePeakCert +
        (1 / (1 + exp(-(pitchSpec_array$specAmplIdx - 1))) - 0.5) * 2 *
        (1 - specSinglePeakCert) # normalization. Visualization:
      # a = 1:15
      # b = specSinglePeakCert + (1 / (1 + exp(-(a - 1))) - 0.5) * 2 *
      # (1 - specSinglePeakCert)
      # plot(a, b, type = 'l')
      pitchSpec_array = pitchSpec_array[
        order(pitchSpec_array$pitchCert, pitchSpec_array$pitchCand, decreasing = TRUE),
        c('pitchCand', 'pitchCert', 'pitchSource')
      ]
    }
  }
  if (!is.null(pitchSpec_array)) {
    if (sum(!is.na(pitchSpec_array)) > 0) {
      pitchSpec_array = pitchSpec_array[pitchSpec_array$pitchCert > specThres,
                                        , drop = FALSE]
      # how many pitchSpec candidates to use (max)
      pitchSpec_array = pitchSpec_array[1:min(nrow(pitchSpec_array), nCands), ]
    }
  }

  return(pitchSpec_array)
}


#' Harmonic product spectrum
#'
#' Internal soundgen function.
#'
#' Estimates pitch per frame using the harmonic product spectrum. Algorithm:
#' downsample the spectrum repeatedly padding with 0 to the original length,
#' then multiply the resulting scaled spectra. This has the effect of
#' emphasizing f0, which should hopefully become the highest spectral peak. See
#' https://cnx.org/contents/i5AAkZCP@2/Pitch-Detection-Algorithms
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param hpsThres voicing threshold (unitless, ~0 to 1)
#' @param hpsNum the number of times the spectrum is downsampled
#' @param hpsNorm the amount of inflation of hps pitch certainty (0 = none)
#' @param hpsPenalty the amount of penalizing hps candidates in low frequencies
#'   (0 = none)
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchHps = function(frame,
                       freqs,
                       bin,
                       hpsThres,
                       hpsNum,
                       hpsNorm,
                       hpsPenalty,
                       pitchFloor,
                       pitchCeiling) {
  pitchHps_array = NULL
  n = length(frame)
  # take log to avoid multiplying large numbers
  frame_log = log(frame)  # NB: frame is normalize (max 1)
  # plot(freqs, frame_log, type = 'l')

  # Downsample the spectrum and pad with log(0) to the old length
  spectra = data.frame(s1 = frame_log)
  for (i in 2:hpsNum) {
    n1 = round(n / i)
    idx = seq(1, n, length.out = n1)
    spectra[, i] = c(frame_log[idx], rep(-Inf, (n - n1)))
  }

  # Multiply the spectra (add logs, then exponentiate)
  spec_hps = exp(apply(spectra, 1, sum) / hpsNum ^ hpsNorm)
  # Note: the /hpsNum part is normalization to make pitchCert more
  # reasonable for hps method. Alternative: simply normalize to 1:
  # spec_hps = spec_hps / max(spec_hps)
  # plot(freqs, spec_hps, type = 'l')

  # Focus on the area within [pitchFloor, pitchCeiling]
  # NB: if this is done before downsampling & multiplying the spectra,
  # resolution seems to suffer
  idx = which(freqs > pitchFloor & freqs < pitchCeiling)
  spec_hps = spec_hps[idx]
  freqs_hps = freqs[idx]
  # plot(freqs_hps, spec_hps, type = 'l')

  # Find peaks in the spectrum (hopefully harmonics)
  temp = zoo::rollapply(zoo::as.zoo(spec_hps),
                        width = 3,  # any peak will do
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = hpsThres)
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]

  if (length(idx) > 0) {
    idx = idx[order(spec_hps[idx], decreasing = TRUE)]
    acceptedHpsPeaks = idx[1]
    # acceptedHpsPeaks = idx[1:min(length(idx), nCands)]  # bad results
    if (length(acceptedHpsPeaks) > 0) {
      # if some peaks are found...
      hpsPeaks = data.frame(idx = acceptedHpsPeaks)
      # parabolic interpolation to get closer to the true peak
      for (i in 1:nrow(hpsPeaks)) {
        idx_peak = idx[i]
        applyCorrecton = idx_peak > 1 & idx_peak < n
        if (applyCorrecton) {
          threePoints = as.numeric(log10(spec_hps[(idx_peak - 1) : (idx_peak + 1)]))
          parabCor = parabPeakInterpol(threePoints)
          hpsPeaks$freq[i] = freqs_hps[idx_peak] + bin * parabCor$p
          hpsPeaks$amp[i] = 10 ^ parabCor$ampl_p
        } else {
          hpsPeaks$freq[i] = freqs_hps[idx_peak]
          hpsPeaks$amp[i] = spec_hps[idx_peak]
        }
      }
    }

    # Penalize low-frequency candidates b/c hps is more accurate for f0 values
    # that are high relative to spectral resolution. Illustration:
    # fr = seq(pitchFloor, pitchCeiling, length.out = n)
    # b = 1:n
    # coef = 1 - 1/exp((b - 1) / hpsPenalty)
    # plot(b, coef, type = 'l', log = 'x')
    # plot(fr, coef, type = 'l', log = 'x')
    coef = 1 - 1 / exp((hpsPeaks$idx - 1) / hpsPenalty)
    hpsPeaks$amp = hpsPeaks$amp * coef

    # Format the output
    pitchHps_array = data.frame(
      'pitchCand' = hpsPeaks$freq,
      'pitchCert' = hpsPeaks$amp,
      'pitchSource' = 'hps',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  return(pitchHps_array)
}


#' Zero-crossing rate
#'
#' A less precise, but very quick method of pitch tracking based on measuring
#' zero-crossing rate in low-pass-filtered audio. Recommended for processing
#' long recordings with typical pitch values well below the first formant
#' frequency, such as speech. Calling this function is considerably faster than
#' using the same pitch-tracking method in \code{\link{analyze}}. Note that,
#' unlike analyze(), it returns the times of individual zero crossings
#' (hopefully corresponding to glottal cycles) instead of pitch values at fixed
#' time intervals.
#'
#' Algorithm: the audio is bandpass-filtered from \code{pitchFloor} to \code{pitchCeiling}, and the timing of all zero crossings is saved. This is not enough, however, because unvoiced sounds like white noise also have plenty of zero crossings. Accordingly, an attempt is made to detect voiced segments (or steady musical tones, etc.) by looking for stable regions, with several zero-crossings at relatively regular intervals (see parameters \code{zcThres} and \code{zcWin}). Very quiet parts of audio are also treated as not having a pitch.
#' @seealso \code{\link{analyze}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @inheritParams analyze
#' @param zcThres pitch candidates with certainty below this value are treated
#'   as noise and set to NA (0 = anything goes, 1 = pitch must be perfectly
#'   stable over \code{zcWin})
#' @param zcWin certainty in pitch candidates depends on how stable pitch is
#'   over \code{zcWin} glottal cycles (odd integer > 3)
#' @param silence minimum root mean square (RMS) amplitude, below which pitch
#'   candidates are set to NA (NULL = don't consider RMS amplitude)
#' @param envWin window length for calculating RMS envelope, ms
#'
#' @return Returns a dataframe containing \describe{\item{time}{time stamps of
#'   all zero crossings except the last one, after bandpass-filtering}
#'   \item{pitch}{pitch calculated from the time between consecutive zero
#'   crossings} \item{cert}{certainty in each pitch candidate calculated from
#'   local pitch stability, 0 to 1}}
#'
#' @export
#' @examples
#' data(sheep, package = 'seewave')
#' # spectrogram(sheep)
#' zc = getPitchZc(sheep, pitchCeiling = 250)
#' plot(zc$detailed[, c('time', 'pitch')], type = 'b')
#'
#' # Convert to a standard pitch contour sampled at regular time intervals:
#' pitch = getSmoothContour(
#'   anchors = data.frame(time = zc$detailed$time, value = zc$detailed$pitch),
#'   len = 1000, NA_to_zero = FALSE, discontThres = 0)
#' spectrogram(sheep, extraContour = pitch, ylim = c(0, 2))
#'
#' \dontrun{
#' # process all files in a folder
#' zc = getPitchZc('~/Downloads/temp')
#' zc$summary
#' }
getPitchZc = function(x,
                      samplingRate = NULL,
                      scale = NULL,
                      from = NULL,
                      to = NULL,
                      pitchFloor = 50,
                      pitchCeiling = 400,
                      zcThres = .1,
                      zcWin = 5,
                      silence = .04,
                      envWin = 5,
                      summaryFun = c('mean', 'sd'),
                      reportEvery = NULL) {
  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in%
                    c('x', 'samplingRate', 'scale', 'from', 'to',
                      'summaryFun', 'reportEvery')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    scale = scale,
                    from = from,
                    to = to,
                    funToCall = '.getPitchZc',
                    myPars = myPars,
                    reportEvery = reportEvery
  )

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          pa$result[[i]] [, c('pitch', 'cert')],
          summaryFun = summaryFun,
          var_noSummary = NULL)
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
    mysum_all = cbind(data.frame(file = pa$input$filenames_base),
                      do.call('rbind', temp))
  } else {
    mysum_all = NULL
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(list(
    detailed = pa$result,
    summary = mysum_all
  ))
}


#' Zero-crossing rate per sound
#'
#' Internal soundgen function called by \code{\link{getPitchZc}}.
#' @param audio a list returned by \code{readAudio}
#' @inheritParams getPitchZc
#' @param env precalculated envelope (when called internally by .analyze())
#' @param certMethod method of calculating pitch certainty: 'autocor' =
#'   autocorrelation of pitch estimates per zc over window (a measure of curve
#'   smoothness), 'variab' = variability of pitch estimates per zc over window
#' @keywords internal
.getPitchZc = function(audio,
                       pitchFloor,
                       pitchCeiling,
                       zcThres,
                       zcWin = 5,
                       silence = .04,
                       env = NULL,
                       envWin = 5,
                       certMethod = c('autocor', 'variab')[2]) {
  ## Find zero crossing in bandpass-filtered audio
  audio_lowpass = .bandpass(audio, lwr = pitchFloor, upr = pitchCeiling)
  zc = which(diff(sign(audio_lowpass)) == 2)
  len_zc = length(zc)
  if (len_zc < 2) return(data.frame(time = NA, pitch = NA, cert = NA)[-1, ])
  len_pitch = len_zc - 1
  pitch_zc = audio$samplingRate / diff(zc)
  time_zc = zc[1:len_pitch] / audio$samplingRate * 1000
  # plot(time_zc, pitch_zc, type = 'l')

  ## Calculate zc pitch certainty over sliding window
  win = seewave::ftwindow(wl = zcWin, wn = 'gaussian')
  half_wl = floor(zcWin / 2)
  if (certMethod == 'autocor') {
    # Method 1: autocorrelation of pitch
    pitch_padded = c(rep(NA, zcWin), pitch_zc, rep(NA, zcWin))
    cert = rep(0, len_pitch)
    for (i in 1:len_pitch) {
      frame = pitch_padded[(i + zcWin - half_wl) : (i + zcWin + half_wl)]
      A = frame[-zcWin]
      B = frame[-1]
      cert[i] = sum(A * B) / sqrt(sum(A ^ 2) * sum(B ^ 2))  # cor(frame[-length(frame)], frame[-1])
      # df_temp = data.frame(x = 1:length(frame), y = frame)
      # mod = summary(lm(y ~ x, df_temp))
      # cert[i] = mod$r.squared
    }
    # plot(cert, type = 'l')
  } else if (certMethod == 'variab') {
    # Method 2: variability of pitch derivative
    diff_pitch = c(0, abs(diff(log2(pitch_zc))) * 12)  # in semitones
    diff_pitch_padded = c(rep(1e6, zcWin), diff_pitch, rep(1e6, zcWin))
    deviation = cert = rep(0, len_pitch)
    for (i in 1:len_pitch) {
      frame = diff_pitch_padded[(i + zcWin - half_wl) : (i + zcWin + half_wl)]
      deviation[i] = sum(frame * win)
    }
    # plot(deviation, type = 'l', ylim = c(0, 24))
    cert = 1 / (deviation + 1)
    # plot(cert, type = 'l')
  }

  ## Amplitude envelope - needed to unvoice very quiet sections
  if (!is.null(silence)) {
    if (is.null(env)) env = .getRMS(audio, windowLength = envWin, plot = FALSE)
    env = .resample(list(sound = env), mult = len_pitch / length(env))
    # plot(env, type = 'l')
    cond_silence = env < silence
  } else {
    cond_silence = rep(FALSE, len_pitch)
  }

  ## Set quiet, unsteady, or impossibly low/high zc to NA
  pitch_zc[cert < zcThres |
             cond_silence |
             pitch_zc < pitchFloor |
             pitch_zc > pitchCeiling] = NA
  return(data.frame(time = time_zc, pitch = pitch_zc, cert = cert))
}
