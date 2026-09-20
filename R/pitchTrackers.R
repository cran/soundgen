#' Zero-crossing rate
#'
#' A less precise, but very quick method of pitch tracking based on measuring
#' zero-crossing rate in bandpass-filtered audio. Recommended for processing
#' long recordings with typical pitch values well below the first formant
#' frequency, such as speech. Calling this function is considerably faster than
#' using the same pitch-tracking method in \code{\link{analyze}}. Note that,
#' unlike analyze(), it returns the times of individual zero crossings
#' (hopefully corresponding to glottal cycles) instead of pitch values at fixed
#' time intervals.
#'
#' Algorithm: the audio is bandpass-filtered from \code{pitchFloor} to
#' \code{pitchCeiling}, and the timing of all zero crossings is saved. This is
#' not enough, however, because aperiodic sounds like white noise also have
#' plenty of zero crossings. Accordingly, an attempt is made to detect voiced
#' segments (or steady musical tones, etc.) by looking for stable regions, with
#' several zero-crossings at relatively regular intervals (see parameters
#' \code{zcThres} and \code{zcWin}). Very quiet parts of audio are also treated
#' as not having a pitch.
#' @seealso \code{\link{analyze}}
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams analyze
#' @param zcThres pitch candidates with certainty below this value are treated
#'   as noise and set to NA (0 = anything goes, 1 = pitch must be perfectly
#'   stable over \code{zcWin})
#' @param zcWin certainty in pitch candidates depends on how stable pitch is
#'   over \code{zcWin} glottal cycles (odd integer > 3)
#' @param silence minimum root mean square (RMS) amplitude, below which pitch
#'   candidates are set to NA (NULL = don't consider RMS amplitude)
#' @param envWin window length for calculating RMS envelope, ms
#' @param certMethod method of calculating pitch certainty: 'variab' =
#'   variability of pitch estimates per zc over window (default); 'autocor' =
#'   autocorrelation of pitch estimates per zc over window (a measure of curve
#'   smoothness)
#'
#' @return A list with descriptives per file (@summary) and per frame
#'   (@detailed), including \describe{\item{time}{time stamps of all zero
#'   crossings except the last one, after bandpass-filtering}
#'   \item{pitch}{pitch calculated from the time between consecutive zero
#'   crossings} \item{cert}{certainty in each pitch candidate calculated from
#'   local pitch stability, 0 to 1}}
#'
#' @keywords internal
#' @examples
#' data(speechEx, package = 'soundgen')
#' # spectrogram(speechEx)
#' zc = soundgen:::getPitchZc(speechEx, pitchCeiling = 250)
#' plot(zc$detailed[, c('time', 'pitch')], type = 'b')
#' spectrogram(speechEx, extraContour = zc$detailed$pitch, ylim = c(0, 2))
#'
#' \dontrun{
#' # process all files in a folder
#' zc = soundgen:::getPitchZc('~/Downloads/temp')
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
                      certMethod = c('variab', 'autocor'),
                      summaryFun = c('mean', 'sd'),
                      reportEvery = NULL) {
  # match args
  certMethod = match.arg(certMethod)
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
    for (i in seq_len(pa$input$n)) {
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
#' @noRd
.getPitchZc = function(audio,
                       pitchFloor,
                       pitchCeiling,
                       zcThres,
                       zcWin = 5,
                       silence = .04,
                       env = NULL,
                       envWin = 5,
                       certMethod = c('variab', 'autocor')) {
  certMethod = match.arg(certMethod)
  if (zcWin < 3 || (zcWin/2 == floor(zcWin/2))) {
    warning('zcWin must be an odd integer >= 3; resetting to 5')
    zcWin = 5
  }

  ## Find zero crossing in bandpass-filtered audio
  audio_filt = .bandpass(audio, lwr = pitchFloor, upr = pitchCeiling)
  zc = which(diff(sign(audio_filt)) == 2)
  len_zc = length(zc)
  if (len_zc < 2) return(data.frame(time = NA, pitch = NA, cert = NA)[-1, ])
  len_pitch = len_zc - 1
  pitch_zc = audio$samplingRate / diff(zc)
  sp = 1:len_pitch
  time_zc = zc[sp] / audio$samplingRate * 1000
  # plot(time_zc, pitch_zc, type = 'l')

  ## Calculate zc pitch certainty over sliding window
  win = winFun(zcWin, wn = 'gaussian', normalize = 'amplitude')
  half_wl = floor(zcWin / 2)
  if (certMethod == 'autocor') {
    # Method 1: autocorrelation of pitch
    pitch_padded = c(rep(pitch_zc[1], zcWin),
                     pitch_zc,
                     rep(pitch_zc[len_pitch], zcWin))
    cert = rep(0, len_pitch)
    for (i in sp) {
      frame = pitch_padded[(i + zcWin - half_wl) : (i + zcWin + half_wl)]
      A = frame[-zcWin]
      B = frame[-1]
      cert[i] = sum(A * B) / sqrt(sum(A ^ 2) * sum(B ^ 2))  # cor(frame[-length(frame)], frame[-1])
      # df_temp = data.frame(x = seq_along(frame), y = frame)
      # mod = summary(lm(y ~ x, df_temp))
      # cert[i] = mod$r.squared
    }
    # plot(cert, type = 'l')
  } else if (certMethod == 'variab') {
    # Method 2: variability of pitch derivative
    diff_pitch = c(0, abs(diff(log2(pitch_zc))) * 12)  # in semitones
    diff_pitch_padded = c(rep(1e6, zcWin), diff_pitch, rep(1e6, zcWin))
    deviation = cert = rep(0, len_pitch)
    for (i in sp) {
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
  data.frame(time = time_zc, pitch = pitch_zc, cert = cert)
}


#' Get lowest dominant frequency band
#' Calculate the lowest frequency band in the spectrum above pitchFloor whose
#' amplitude exceeds a certain threshold. We take the first frequency in the
#' spectrum that is at least \code{domThres} % of the amplitude of peak
#' frequency, but high enough to be above pitchFloor (and below pitchCeiling).
#' @param domThres (0 to 1) to find the lowest dominant frequency band, we
#'   do short-term FFT and take the lowest frequency with amplitude at least
#'   domThres
#' @param domSmooth the width of smoothing interval (Hz) for finding
#'   \code{dom}
#' @return A list of $dom (NA or numeric) and $dom_array (either an empty
#'   dataframe or a dataframe of pitch candidates).
#' @noRd
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
  len = length(frame)
  # width of smoothing interval (in bins), forced to be an odd number
  domSmooth_bins = max(1, 2 * ceiling(domSmooth / bin / 2) - 1)

  # find the lowest peak in the spectrum
  # much faster than getting all peaks with
  # findPeaks(frame, wl = domSmooth_bins, thres = domThres)[1]
  idx_peak = NULL
  halfwl = domSmooth_bins %/% 2
  valid_idx = which(freqs >= pitchFloor & freqs <= pitchCeiling)
  if (length(valid_idx) == 0) return(list(dom_array = dom_array, dom = dom))

  for (i in valid_idx) {
    frx = max(1, (i - halfwl)):min(len, (i + halfwl))
    frx_wo_i = frx[frx != i]
    cond = frame[i] > domThres && !any(frame[frx_wo_i] > frame[i])
    if (cond) {
      idx_peak = i
      break
    }
  }

  if (length(idx_peak) > 0) {
    # parabolic interpolation to get closer to the true peak
    applyCorrection = !is.na(idx_peak) &&
      length(idx_peak) == 1 &&
      (idx_peak > 1 & idx_peak < len)
    if (applyCorrection) {
      threePoints = log10(pmax(frame[(idx_peak - 1) : (idx_peak + 1)], 1e-10))
      parabCor = parabPeakInterpol(threePoints)
      dom = freqs[idx_peak] + bin * parabCor$p
      dom_ampl = 10 ^ parabCor$ampl_p
      if (dom_ampl > 1) dom_ampl = 1  # cap at 1
    } else {
      dom = freqs[idx_peak]
      dom_ampl = frame[idx_peak]
    }

    dom_array = data.frame(
      'pitchCand' = dom,
      'pitchCert' = dom_ampl,
      'pitchSource' = 'dom',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  list(dom_array = dom_array, dom = dom)
}


#' Autocorrelation pitch tracker
#' Attempts to find F0 of a frame by looking for peaks in the autocorrelation
#' function (time domain analysis). Modified PRAAT's algorithm. See Boersma, P.
#' (1993). Accurate short-term analysis of the fundamental frequency and the
#' harmonics-to-noise ratio of a sampled sound. In Proceedings of the institute
#' of phonetic sciences (Vol. 17, No. 1193, pp. 97-110).
#' @param autocorThres voicing threshold (unitless, ~0 to 1)
#' @param autocorSmooth the width of smoothing interval (in bins) for
#'   finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'   rate 44100 and smaller odd numbers for lower values of sampling rate
#' @param autocorUpsample upsamples acf to this resolution (Hz) to improve
#'   accuracy in high frequencies
#' @param autocorBestPeak amplitude of the lowest best candidate relative to the
#'   absolute max of the acf
#' @return A list of $HNR (NA or numeric) and $pitchAutocor_array (either NULL
#'   or a dataframe of pitch candidates).
#' @noRd
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
        pitchCeiling > a$freq[upsample_to_bin] && upsample_len > 1) {
      # Boersma recommends interpolating with sin(x) / x, but here we simply
      # call spline() - completely agnostic
      temp = spline(a$amp[upsample_from_bin:upsample_to_bin],
                    n = upsample_len,
                    x = a$freq[upsample_from_bin:upsample_to_bin])
      # points(temp$x, temp$y, type = 'p', cex = .25, col = 'red')
      a = rbind(data.frame(freq = rev(temp$x), amp = rev(temp$y)),
                a[(upsample_to_bin + 1):nrow(a), ])
    }
  }

  # HNR = max(a$amp) # HNR is here defined as the maximum autocorrelation
  # within the specified pitch range. It is also measured for the frames which
  # are later classified as voiceless (i.e. HNR can be <voicedThres)

  # find peaks in the corrected autocorrelation function; default width = 7
  # chosen by optimization, but it doesn't make that much difference anyhow
  idx = findPeaks(a$amp, wl = autocorSmooth, thres = autocorThres, dropFirstLast = TRUE)
  autocorPeaks = a[idx, ]

  if (length(idx) > 0) {
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
    }

    if (!inherits(autocorPeaks, 'try-error')) {
      autocorPeaks = autocorPeaks[order(autocorPeaks$amp, decreasing = TRUE),, drop = FALSE]
      if (nrow(autocorPeaks) > 0) {
        nr_an = min(nrow(autocorPeaks), nCands)

        ## avoid repeated calculations inside the loop
        lag_min = round(samplingRate / pitchCeiling)
        lag_max = round(samplingRate / pitchFloor)
        half_win_sinc = min(250, floor(length(autoCorrelation) / 2))
        if (interpol == 'sinc') {
          win_sinc_precomp = winFun(half_win_sinc * 2 + 1, wn = wn)
        } else {
          win_sinc_precomp = NULL
        }

        for (r in 1:nr_an) {
          gh_r = getHNR(acf_x = autoCorrelation,
                        samplingRate = samplingRate,
                        lag_min = lag_min,
                        lag_max = lag_max,
                        idx_max = round(samplingRate / autocorPeaks$freq[r]),
                        interpol = interpol,
                        wn = wn,
                        win_sinc = win_sinc_precomp)
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
        if (!is.null(pitchAutocor_array)) {
          HNR = max(pitchAutocor_array$pitchCert)
        }
      }
    }
    list(pitchAutocor_array = pitchAutocor_array, HNR = HNR)
  }
  list(pitchAutocor_array = pitchAutocor_array, HNR = HNR)
}


#' Cepstral pitch tracker
#' Attempts to find F0 of a frame by looking for peaks in the cepstrum.
#' See http://www.phon.ucl.ac.uk/courses/spsci/matlab/lect10.html
#' @param cepThres voicing threshold (unitless, ~0 to 1)
#' @param cepZp zero-padding of the spectrum used for cepstral pitch detection
#'   (final length of spectrum after zero-padding in points, e.g. 2 ^ 13)
#' @param tol tolerance when removing false subharmonics
#' @param specMerge tolerance when removing similar candidates, semitones
#' @return Either NULL or a dataframe of pitch candidates and CPP.
#' @noRd
getPitchCep = function(frame,
                       samplingRate,
                       bin = NULL,
                       nCands,
                       cepThres,
                       cepZp,
                       pitchFloor,
                       pitchCeiling,
                       tol = .05,
                       specMerge = 2) {
  pitchCep_array = NULL
  if (is.null(bin)) bin = diff(as.numeric(names(frame)[1:2])) * 1000

  if (cepZp < length(frame)) {
    frameZP = frame
  } else {
    zp = rep(0, cepZp - length(frame))
    frameZP = c(frame, zp)
  }
  # if (!is.null(logSpec) && !is.na(logSpec) && logSpec)
  frameZP = log(pmax(frameZP, 1e-6)) # 1e-6 is -120 dB
  # plot(as.numeric(names(frameZP)), frameZP, type = 'l', log = 'x')

  # ifft of log-fft = cepstrum
  cepstrum = abs(fft(as.numeric(frameZP), inverse = TRUE))
  # cepstrum = abs(fft(as.numeric(frameZP)))  # basically the same
  l = length(cepstrum) %/% 2

  # calculate quefrencies
  # bin_q = 2 / samplingRate #
  bin_q = 1 / bin / length(cepstrum)
  q = (0:(l - 1)) * bin_q
  f = 1 / q
  cepstrum = cepstrum[1:l]
  # plot(q[-1], cepstrum[-1], type = 'l')
  # plot(f[-1], cepstrum[-1], type = 'l', log = 'x')

  # focus on the range of frequencies from pitchFloor to pitchCeiling
  idx_keep = which(f >= pitchFloor & f <= pitchCeiling)
  l = length(idx_keep)
  b = data.frame(
    idx = idx_keep,
    q = q[idx_keep],
    pitchCand = f[idx_keep],
    cep = cepstrum[idx_keep]
  )
  # plot(b$pitchCand, b$cep, type = 'l', log = 'x')

  # find local maxima
  idx_peaks = findPeaks(b$cep, dropFirstLast = TRUE)

  # if some peaks are found...
  if (length(idx_peaks) > 0) {
    cepPeaks = b[idx_peaks, ]

    # fit a regression line to cepstrum in the target range of pitch freqs
    b1 = b[-idx_peaks, ]
    vbp = var(b1$pitchCand)
    if (nrow(b1) > 1 && vbp > 0) {
      slope = cov(b1$pitchCand, b1$cep) / vbp
      intercept = mean(b1$cep) - mean(b1$pitchCand) * slope
    } else {
      slope = 0
      intercept = mean(b$cep)
    }
    pred = intercept + slope * b$pitchCand
    pred[pred < 1e-6] = 1e-6  # otherwise log(negative number) = error for CPP

    # parabolic interpolation to improve resolution
    for (i in 1:nrow(cepPeaks)) {
      idx_peak = which(b$idx == cepPeaks$idx[i])
      applyCorrection = idx_peak > 1 & idx_peak < l
      if (applyCorrection) {
        threePoints = b$cep[(idx_peak - 1) : (idx_peak + 1)]
        parabCor = parabPeakInterpol(threePoints)
        cepPeaks$q[i] = (cepPeaks$idx[i] - 1 + parabCor$p) * bin_q
        cepPeaks$cep[i] = parabCor$ampl_p
      }
    }
    cepPeaks$pitchCand = 1 / cepPeaks$q  # need to recalculate from adjusted q

    # calculate Cepstral Peak Prominence
    cepPeaks$CPP = 20 * log10(cepPeaks$cep / pred[idx_peaks])

    # remap CPP in dB to pitchCert on a [0, 1] scale
    # a = seq(0, 50, length.out = 500)
    # b = 1 / (1 + exp(-log2(a / 6)))
    # plot(a, b)  # 0.5 at 6 dB, doubles for every 6 dB
    CPP_non_neg = cepPeaks$CPP
    CPP_non_neg[CPP_non_neg <= 1e-6] = 1e-6
    cepPeaks$pitchCert = 1 / (1 + exp(-log2(CPP_non_neg / 6)))
    cepPeaks = cepPeaks[which(cepPeaks$pitchCert > cepThres), ]
    nr = nrow(cepPeaks)

    if (nr > 1) {
      # remove harmonic quefrencies, keeping only the highest (rather hacky;
      # improves accuracy for high f0 at some cost for low f0)
      if (is.finite(tol)) {
        idx_remove = numeric(0)
        for (p in 1:min((nr - 1), 5)) {
          # if more than 5 first peaks, risk removing everything in low freqs
          ratios = cepPeaks$pitchCand[p] / cepPeaks$pitchCand[(p + 1):nr]
          ratios_int = round(ratios)
          idx_remove = c(idx_remove, p + which(
            ratios < 4 &   # again, otherwise removes too much in low freqs
              abs(ratios - ratios_int) < tol))
        }
        if (length(idx_remove) > 0)
          cepPeaks = cepPeaks[-unique(idx_remove), ]
      }
    }

    # remove similar pitch candidates (double peaks)
    if (nrow(cepPeaks) > 1) {
      specMerge_oct = specMerge / 12
      c = 1
      while (c + 1 <= nrow(cepPeaks)) {
        d = abs(log2(cepPeaks$pitchCand[c]) - log2(cepPeaks$pitchCand[c + 1]))
        if (d < specMerge_oct) {
          if (cepPeaks$CPP[c] > cepPeaks$CPP[c + 1]) {
            cepPeaks = cepPeaks[-(c + 1), ]
          } else {
            cepPeaks = cepPeaks[-c, ]
          }
        } else {
          c = c + 1
        }
      }
    }

    if (nrow(cepPeaks) > 0) {
      # save nCands best candidates
      ord = order(cepPeaks$CPP, decreasing = TRUE)
      cepPeaks = cepPeaks[ord[1:min(nCands, length(ord))], ]

      pitchCep_array = data.frame(
        'pitchCand' = cepPeaks$pitchCand,
        'pitchCert' = cepPeaks$pitchCert,
        'pitchSource' = 'cep',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  }
  pitchCep_array
}


#' Get Cepstral Peak Prominence
#' @param pitch pitch of this frame, Hz
#' @param bin spectral frequency bin width, Hz
#' @return Either NA or CPP in dB.
#' @noRd
getCPP = function(frame,
                  samplingRate,
                  bin = NULL,
                  pitch,
                  prox_semitones = 3) {
  CPP = NA
  if (is.null(bin)) bin = diff(as.numeric(names(frame)[1:2])) * 1000
  log_spectrum = log(frame + 1e-6) # 1e-6 is -120 dB

  # cepstrum is ifft of log-spectrum
  cepstrum = abs(fft(as.numeric(log_spectrum), inverse = TRUE))
  l = length(cepstrum) %/% 2

  # calculate quefrencies
  bin_q = 1 / bin / length(cepstrum)
  q = (0:(l - 1)) * bin_q
  f = 1 / q
  cepstrum = cepstrum[1:l]
  b = data.frame(
    idx = 2:l,  # discard the lowest freq (first q value)
    q = q[-1],
    freq = f[-1],
    cep = cepstrum[-1]
  )
  # plot(b$freq, b$cep, type = 'l', log = 'x')

  # find local maxima
  idx_peaks = findPeaks(b$cep, dropFirstLast = TRUE)
  mult = 2 ^ (prox_semitones / 12)
  in_range = which(b$freq > (pitch / mult) & b$freq < (pitch * mult))
  if (length(in_range) == 0) return(NA)
  target_range = range(in_range)
  local_peaks = idx_peaks[which(idx_peaks > target_range[1] &
                                  idx_peaks < target_range[2])]

  # if some peaks are found...
  if (length(local_peaks) > 0) {
    cepPeaks = b[local_peaks, ]

    # fit a regression line to the entire cepstrum, except for the peaks
    b1 = b[-idx_peaks, ]
    vbf = var(b1$freq)
    if (nrow(b1) > 1 && vbf > 0) {
      slope = cov(b1$freq, b1$cep) / vbf
      intercept = mean(b1$cep) - mean(b1$freq) * slope
    } else {
      slope = 0
      intercept = mean(b$cep)
    }
    pred = intercept + slope * b$freq
    pred[pred < 1e-6] = 1e-6  # otherwise log(negative number) = error for CPP

    # parabolic interpolation to improve resolution
    for (i in 1:nrow(cepPeaks)) {
      idx_peak = which(b$idx == cepPeaks$idx[i])
      applyCorrection = idx_peak > 1 & idx_peak < l
      if (applyCorrection) {
        threePoints = b$cep[(idx_peak - 1) : (idx_peak + 1)]
        parabCor = parabPeakInterpol(threePoints)
        cepPeaks$q[i] = (cepPeaks$idx[i] - 1 + parabCor$p) * bin_q
        cepPeaks$cep[i] = parabCor$ampl_p
      }
    }
    cepPeaks$freq = 1 / cepPeaks$q  # need to recalculate from adjusted q
    pos_in_b = match(cepPeaks$idx, b$idx)
    cepPeaks$CPP = 20 * log10(cepPeaks$cep / pred[pos_in_b])
    CPP = max(cepPeaks$CPP)
    CPP
  } else {
    NA
  }
}


#' BaNa pitch tracker
#' Attempts to find F0 of a frame by detecting several putative harmonics and
#' either finding their highest common factor (specMethod = "commonFactor") or
#' comparing their ratios (specMethod = "BaNa"). For the highest common factor
#' method, see Howard & Angus (2017) "Acoustics and psychoacoustics" (section
#' 3.2.1). For BaNa, see Ba et al. (2012) "BaNa: A hybrid approach for noise
#' resilient pitch detection." Statistical Signal Processing Workshop (SSP),
#' 2012 IEEE.
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
#' @param nCands number of pitch candidates per frame (specMethod =
#'   "commonFactor" always returns a single candidate)
#' @return Either NULL or a dataframe of pitch candidates.
#' @noRd
getPitchSpec = function(frame,
                        bin,
                        freqs,
                        specMethod = c('BaNa', 'commonFactor'),
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
  specMethod = match.arg(specMethod)
  if (specMethod == 'commonFactor') nCands = 1
  # nCands = 1, otherwise returns subh with the same apparent certainty
  pitchSpec_array = NULL
  n = length(frame)
  width = max(3, 2 * floor(specSmooth / bin / 2) + 1)  # always odd
  if (!is.numeric(HNR) || is.na(HNR)) {
    specPitchThreshold = specPeak # if HNR is NA, the sound is
    # probably a mess, so we play safe by only looking at very strong harmonics
  } else {
    # for noisy sounds the threshold is high to avoid false sumharmonics etc,
    # for tonal sounds it is low to catch weak harmonics
    specPitchThreshold = specPeak * (1 - HNR * specHNRslope)
  }

  # find peaks in the spectrum (hopefully, harmonics)
  # plot(freqs, frame, type = 'l', log = 'x')
  # plot(freqs, frame, type = 'l', log = 'xy')
  idx = findPeaks(frame, wl = width, thres = specPitchThreshold, dropFirstLast = TRUE)
  specPeaks = data.frame('idx' = idx)
  nr = length(idx)

  # parabolic interpolation to get closer to the true peak
  if (nr > 0) {
    for (i in 1:nr) {
      idx_peak = specPeaks$idx[i]
      applyCorrection = idx_peak > 1 & idx_peak < n
      if (applyCorrection) {
        threePoints = log10(frame[(idx_peak - 1) : (idx_peak + 1)] + 1e-12)
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
    if (specPeaks$freq < pitchCeiling && specPeaks$freq > pitchFloor) {
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
      seq_1_specRatios = 1:specRatios
      pitchCand = as.numeric(apply(specPeaks[, 'freq', drop = FALSE], 1,
                                   function(x) x / seq_1_specRatios))
    } else if (specMethod == 'BaNa') {
      # A modified version of BaNa algorithm follows
      # analyze five lowest harmonics
      specPeaks = specPeaks[1:min(5, nrow(specPeaks)), ]
      pairs = t(combn(nrow(specPeaks), 2))
      temp = data.frame(
        harmonicA = pairs[, 2],
        harmonicB = pairs[, 1],
        AtoB_ratio = specPeaks$freq[pairs[, 2]] / specPeaks$freq[pairs[, 1]]
      )

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
      specMerge_oct = specMerge / 12
      while (c + 1 <= nrow(pitchSpec_array)) {
        d = abs(log2(pitchSpec_array$pitchCand[c]) - log2(pitchSpec_array$pitchCand[c + 1]))
        if (d < specMerge_oct) {
          # merge cands within specMerge into one "super-candidate"
          # and give this new super-candidate a certainty boost
          pitchSpec_array$specAmplIdx[c] = pitchSpec_array$specAmplIdx[c] + 1
          pitchSpec_array$pitchCand[c] = exp(mean(log(c(
            pitchSpec_array$pitchCand[c],
            pitchSpec_array$pitchCand[c + 1]
          ))))  # geometric mean since it's log-freqs
          pitchSpec_array = pitchSpec_array[-(c + 1), ]
        } else {
          c = c + 1
        }
      }
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
    if (nrow(pitchSpec_array) > 0) {
      pitchSpec_array = pitchSpec_array[pitchSpec_array$pitchCert > specThres,
                                        , drop = FALSE]
      # how many pitchSpec candidates to use (max)
      pitchSpec_array = pitchSpec_array[1:min(nrow(pitchSpec_array), nCands), ]
    }
  }
  pitchSpec_array
}


#' Harmonic product spectrum
#' Estimates pitch per frame using the harmonic product spectrum. Algorithm:
#' downsample the spectrum repeatedly padding with 0 to the original length,
#' then multiply the resulting scaled spectra. This has the effect of
#' emphasizing f0, which should hopefully become the highest spectral peak. See
#' https://cnx.org/contents/i5AAkZCP@2/Pitch-Detection-Algorithms
#' @param hpsThres voicing threshold (unitless, ~0 to 1)
#' @param hpsNum the number of times the spectrum is downsampled
#' @param hpsNorm the amount of inflation of hps pitch certainty (0 = none)
#' @param hpsPenalty the amount of penalizing hps candidates in low frequencies
#'   (0 = none)
#' @return Either NULL or a dataframe with a single pitch candidate.
#' @noRd
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
  frame_log = log(pmax(frame, 1e-6))  # NB: frame is normalized (max 1)
  # plot(freqs, frame_log, type = 'l')

  # Downsample the spectrum and pad with log(0) to the old length
  spectra = matrix(-Inf, nrow = n, ncol = hpsNum)
  spectra[, 1] = frame_log
  if (hpsNum > 1) {
    for (i in 2:hpsNum) {
      n1 = max(1, round(n / i))
      idx = round(seq(1, n, length.out = n1))
      spectra[, i] = c(frame_log[idx], rep(-Inf, (n - n1)))
    }
  }
  # Multiply the spectra (add logs, then exponentiate)
  spec_hps = exp(rowSums(spectra) / hpsNum ^ hpsNorm)

  # Note: the /hpsNum part is normalization to make pitchCert more
  # reasonable for hps method. Alternative: simply normalize to 1:
  # spec_hps = spec_hps / max(spec_hps)
  # plot(freqs, spec_hps, type = 'l')

  # Focus on the area within [pitchFloor, pitchCeiling]
  # NB: if this is done before downsampling & multiplying the spectra,
  # resolution seems to suffer
  idx = which(freqs > pitchFloor & freqs < pitchCeiling)
  spec_hps = spec_hps[idx]
  n_hps = length(spec_hps)
  freqs_hps = freqs[idx]
  # plot(freqs_hps, spec_hps, type = 'l')

  # Find peaks in the spectrum (hopefully, harmonics)
  idx = findPeaks(spec_hps, wl = 3, # any peak will do
                  thres = hpsThres,
                  dropFirstLast = TRUE)
  if (length(idx) > 0) {
    idx = idx[order(spec_hps[idx], decreasing = TRUE)]
    acceptedHpsPeaks = idx[1]
    # acceptedHpsPeaks = idx[1:min(length(idx), nCands)]  # bad results
    if (length(acceptedHpsPeaks) > 0) {
      # if some peaks are found...
      hpsPeaks = data.frame(idx = acceptedHpsPeaks)
      # parabolic interpolation to get closer to the true peak
      for (i in 1:nrow(hpsPeaks)) {
        idx_peak = hpsPeaks$idx[i]
        applyCorrection = idx_peak > 1 & idx_peak < n_hps
        if (applyCorrection) {
          threePoints = as.numeric(log10(spec_hps[(idx_peak - 1) : (idx_peak + 1)] + 1e-12))
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
    if (hpsPenalty == 0) {
      coef = rep(1, nrow(hpsPeaks))
    } else {
      coef = 1 - 1 / exp((hpsPeaks$idx - 1) / hpsPenalty)
    }
    hpsPeaks$amp = hpsPeaks$amp * coef
    if (all(hpsPeaks$amp <= 0)) return(NULL)

    # Format the output
    pitchHps_array = data.frame(
      'pitchCand' = hpsPeaks$freq,
      'pitchCert' = hpsPeaks$amp,
      'pitchSource' = 'hps',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  pitchHps_array
}
