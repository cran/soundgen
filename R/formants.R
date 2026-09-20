#' Formant filter
#'
#' Prepares a frequency-domain filter for adding formants to a sound. Formants
#' are specified as a list containing time, frequency, amplitude, and width
#' values for each vocal tract resonance (see examples). For more information,
#' see \url{https://cogsci.se/soundgen/sound_generation.html}.
#' @param nr number of frequency bins (half the FFT window size)
#' @param nc the number of time steps for Fourier transform
#' @inheritParams soundgen
#' @param formants a character string like "aaui" referring to default presets
#'   for speaker "M1"; a vector of formant frequencies; or a list of formant
#'   times, frequencies, amplitudes, and bandwidths, with a single value of each
#'   for static or multiple values of each for moving formants. \code{formants =
#'   NA} defaults to schwa if \code{temperature > 0} and \code{vocalTract} is
#'   specified. Time stamps for formants and mouth opening can be specified in
#'   ms or any other arbitrary scale.
#' @param formDrift scale factor regulating the effect of temperature on the
#'   depth of random drift of all formants (user-defined and stochastic): the
#'   higher, the more formants drift at a given temperature
#' @param formDisp scale factor regulating the effect of temperature on the
#'   irregularity of the dispersion of stochastic formants: the higher, the more
#'   unevenly stochastic formants are spaced at a given temperature
#' @param speedSound speed of sound in warm air, cm/s. Stevens (2000) "Acoustic
#'   phonetics", p. 138
#' @param openMouthBoost amplify the voice when the mouth is open by
#'   \code{openMouthBoost} dB
#' @param smoothing list of parameters passed to soundgen:::getSmoothContour
#' @param output "simple" returns just the spectral filter, while "detailed"
#'   also returns a data.frame of formant frequencies over time (needed for
#'   internal purposes such as formant locking)
#' @param plot if TRUE, produces a plot of the spectral envelope
#' @param duration duration of the sound, ms (for plotting and column names in
#'   the output)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   or another color theme (e.g. 'heat.colors')
#' @param col actual colors, e.g., rev(rainbow(100)) - see ?hcl.colors for colors
#'   in base R (overrides colorTheme)
#' @param xlab,ylab labels of axes
#' @param ... other graphical parameters passed on to \code{image()}
#' @export
#' @return Spectral filter on a linear scale (not dB): a matrix with frequency
#'   bins in rows and time steps in columns. Accordingly, rownames of the output
#'   give central frequency of each bin (in kHz), while colnames give time
#'   stamps (in ms if duration is specified, otherwise 0 to 1).
#' @examples
#' # [a] with only F1-F3 visible, with no stochasticity
#' e = getFormantFilter(nr = 512, nc = 50, duration = 300,
#'   formants = 'a', temperature = 0, plot = TRUE, col = heat.colors(150))
#' # image(t(e))  # to plot the output on a linear scale instead of dB
#'
#' # some "wiggling" of specified formants plus extra formants on top
#' e = getFormantFilter(nr = 512, nc = 50,
#'   formants = c(860, 1430, 2900),
#'   temperature = 0.1, formantDepStoch = 1, plot = TRUE)
#'
#' # a schwa based on variable length of vocal tract
#' e = getFormantFilter(nr = 512, nc = 50, formants = NA,
#'   vocalTract = list(time = c(0, .4, 1), value = c(13, 18, 17)),
#'   temperature = .1, plot = TRUE)
#'
#' # no formants at all, only lip radiation
#' e = getFormantFilter(nr = 512, nc = 1, lipRad = 6,
#'   formants = NA, temperature = 0, plot = FALSE)
#' plot(e[, 1], type = 'l')              # linear scale
#' plot(20 * log10(e[, 1]), type = 'l')  # dB scale - 6 dB/oct
#'
#' # mouth opening
#' e = getFormantFilter(nr = 512, nc = 50,
#'   vocalTract = 16, plot = TRUE, lipRad = 6, noseRad = 4,
#'   mouth = data.frame(time = c(0, .5, 1), value = c(0, 0, .5)))
#'
#' # scale formant amplitude and/or bandwidth
#' e1 = getFormantFilter(nr = 512, nc = 1,
#'   formants = 'a', formantWidth = 1, formantDep = 1)  # defaults
#' e2 = getFormantFilter(nr = 512, nc = 1,
#'   formants = 'a', formantWidth = 1.5, formantDep = 1.5)
#' plot(as.numeric(rownames(e2)), 20 * log10(e2[, 1]),
#'      type = 'l', xlab = 'KHz', ylab = 'dB', col = 'red', lty = 2)
#' points(as.numeric(rownames(e1)), 20 * log10(e1[, 1]), type = 'l')
#'
#' # manual specification of formants
#' e3 = getFormantFilter(
#'   nr = 512, nc = 50, samplingRate = 16000, plot = TRUE,
#'   formants = list(
#'     f1 = list(freq = c(900, 500), amp = c(30, 35), width = c(80, 50)),
#'     f2 = list(freq = c(1900, 2500), amp = c(25, 30), width = 100),
#'     f3 = list(freq = 3400, amp = 30, width = 120)
#' ))
#'
#' # extra zero-pole pair (doesn't affect estimated VTL and thus the extra
#' # formants added on top)
#' e4 = getFormantFilter(
#'   nr = 512, nc = 50, samplingRate = 16000, plot = TRUE,
#'   formants = list(
#'     f1 = list(freq = c(900, 500), amp = c(30, 35), width = c(80, 50)),
#'     f1.5 = list(freq = 1300, amp = -15),
#'     f1.7 = list(freq = 1500, amp = 15),
#'     f2 = list(freq = c(1900, 2500), amp = c(25, 30), width = 100),
#'     f3 = list(freq = 3400, amp = 30, width = 120)
#' ))
#' plot(as.numeric(rownames(e4)), 20 * log10(e3[, ncol(e3)]),
#'      type = 'l', xlab = 'KHz', ylab = 'dB')
#' points(as.numeric(rownames(e4)), 20 * log10(e4[, ncol(e4)]),
#'        type = 'l', col = 'red', lty = 2)
getFormantFilter = function(
    nr,
    nc,
    formants = NA,
    formantDep = 1,
    formantWidth = 1,
    lipRad = 6,
    noseRad = 4,
    mouth = NA,
    mouthOpenThres = 0,
    openMouthBoost = 0,
    vocalTract = NULL,
    temperature = 0.025,
    formDrift = .3,
    formDisp = .2,
    formantDepStoch = 1,
    formantCeiling = NULL,
    samplingRate = 16000,
    speedSound = 35400,
    smoothing = list(interpol = 'splineFC'),
    output = c('simple', 'detailed'),
    plot = FALSE,
    duration = NULL,
    colorTheme = 'bw',
    col = NULL,
    xlab = 'Time',
    ylab = 'Frequency, kHz',
    ...
) {
  # standard formatting
  output = match.arg(output)
  formants = reformatFormants(formants)
  if (!is.null(vocalTract)) {
    if (!any(is.na(vocalTract))) {
      if (is.list(vocalTract) ||
          (is.numeric(vocalTract) && length(vocalTract) > 1)) {
        vocalTract = do.call(getSmoothContour, c(smoothing, list(
          vocalTract,
          len = nc,
          valueFloor = permittedValues['vocalTract', 'low'],
          valueCeiling = permittedValues['vocalTract', 'high'],
          plot = FALSE
        )))  # vocalTract is now either NULL/NA or numeric of length nc
      }
    }
  }

  ## estimate vocal tract length
  if (!is.list(vocalTract) && !is.numeric(vocalTract) && is.list(formants)) {
    # if we don't know vocalTract, but at least one formant is defined,
    # we guess the length of vocal tract
    vocalTract = estimateVTL(formants = formants,
                             speedSound = speedSound,
                             checkFormat = TRUE)  # may need to remove non-integer
  }

  # if formants = NA / NULL or if there's something wrong with it,
  # reformatFormants returns NA, and we fall back on vocalTract to make a schwa
  if (!is.list(formants) && is.numeric(vocalTract) &&
      temperature > 0 && formantDep > 0 && formantDepStoch > 0) {
    freq = speedSound / 4 / vocalTract
    formants = list('f1' = data.frame(
      'time' = seq(0, 1, length.out = length(freq)),
      'freq' = freq,
      'amp' = NA,
      'width' = getBandwidth(freq)  # corrected Tappert, Martony, and Fant (TMF)-1963
    ))
  }

  # convert formant freqs and widths from Hz to bins
  if (nr %/% 2 == 0) {
    # even number of bins
    nr_fullspec = (nr - 1) * 2 + 1
  } else {
    # odd number of bins
    nr_fullspec = (nr - 1) * 2
  }
  bin_width = samplingRate / nr_fullspec # Hz


  ### START OF FORMANTS
  if (is.list(formants)) {
    ## Upsample to the length of fft steps
    formants_upsampled = vector('list', length = length(formants))
    for (f in seq_along(formants)) {
      formant_f = as.data.frame(interpolMatrix(
        as.matrix(formants[[f]]), nr = nc, interpol = 'approx'))
      # time must be [0, 1]
      if (!any(formant_f$time != 0))
        formant_f$time = seq(0, 1, length.out = nc)
      formant_f$freq = formant_f$freq * vocalTract[1] / vocalTract
      formants_upsampled[[f]] = formant_f
    }
    names(formants_upsampled) = names(formants)
    nFormants = length(formants)
    amplScaleFactor = rep(1, nFormants)
    # non-integer formants like "f1.4" refer to extra zero-pole pairs.
    # They should not be considered for VTL estimation or for adding formants
    non_integer_formants = grepl('.', names(formants_upsampled), fixed = TRUE)
    nFormants_integer = length(formants_upsampled) - sum(non_integer_formants)

    ## Stochastic part (only for temperature > 0)
    if (temperature > 0) {
      if (formDisp == 0) formDisp = 1e-6  # otherwise division by 0

      # add extra formants above the specified ones, assuming a uniform tube
      if (!is.numeric(formantDepStoch)) formantDepStoch = 1
      if (!is.numeric(vocalTract) && length(formants) > 1 &&
          formantDepStoch > 0 && formantDep > 0) {
        ff = vapply(formants[!non_integer_formants], function(x) x$freq[1], numeric(1))
        formantDispersion = getdF(ff,
                                  speedSound = speedSound,
                                  method = 'regression')
      } else if (is.numeric(vocalTract)) {
        formantDispersion = speedSound / (2 * vocalTract)
      } else {
        formantDispersion = NA # making sdG also NA, ie extra formants not added
      }
      sdG = formantDispersion * temperature * formDisp

      if (!any(is.na(sdG))) {
        # formant_f = (2 * f - 1) / 2 * formantDispersion,
        # therefore, to generate formants to 2 * Nyquist
        # 2 * nyquist = (2 * nExtraFormants - 1) / 2 * formantDispersion
        # Solving for nExtraFormants gives (nyquist * 4 / formantDispersion + 1) / 2:
        if (is.null(formantCeiling)) {
          nExtraFormants = round(
            (samplingRate / min(formantDispersion) + 1) / 2
          ) - nFormants_integer
        } else {
          nExtraFormants = round(
            (samplingRate * formantCeiling / min(formantDispersion) + 1) / 2
          ) - nFormants_integer
        }
        if (is.numeric(nExtraFormants) && nExtraFormants > 0) {
          # if we are going to add extra formants
          nf = length(formantDispersion)
          extraFreqs = extraWidths = matrix(NA, nrow = nf, ncol = nExtraFormants)
          extraAmps = rgamma(
            nExtraFormants,
            # mean = formantDepStoch, sd = formantDepStoch * temperature
            1 / temperature ^ 2,
            1 / (formantDepStoch * temperature ^ 2)
          )
          amplScaleFactor = c(amplScaleFactor, extraAmps)
          for (frame in 1:nf) {
            # once for static vtl, for each frame in 1:nc otherwise
            idx = (nFormants_integer + 1) : (nFormants_integer + nExtraFormants)
            extraFreqs_regular = (2 * idx - 1) / 2 * formantDispersion[frame]
            extraFreqs[frame, ] = rgamma(
              nExtraFormants,
              # mean = extraFreqs_regular, sd = sdG
              extraFreqs_regular ^ 2 / sdG[frame] ^ 2,
              extraFreqs_regular / sdG[frame] ^ 2
            )
            extraWidths[frame, ] = getBandwidth(extraFreqs[frame, ])
          }

          formants_upsampled = c(formants_upsampled, vector('list', nExtraFormants))
          for (f in 1:nExtraFormants) {
            formants_upsampled[[nFormants + f]] = data.frame (
              'time' = formants_upsampled[[1]][, 'time'],
              'freq' = extraFreqs[, f],
              'amp' = NA,
              'width' = extraWidths[, f]
            )
          }
        }
      }

      # wiggle both user-specified and stochastic formants
      nFormants = length(formants_upsampled)
      for (f in 1:nFormants) {
        for (c in 2:4) {
          # wiggle freq, ampl and bandwidth independently
          if (all(is.na(formants_upsampled[[f]][, c]))) next
          rw = getRandomWalk(
            len = nc,
            rw_range = temperature * formDrift,
            rw_smoothing = 0.3,
            trend = rnorm(1)
          )
          # if nc == 1, returns one number close to 1
          if (length(rw) > 1) {
            # for actual random walks, make sure mean is 1
            rw = rw - mean(rw) + 1
          }
          formants_upsampled[[f]][, c] = formants_upsampled[[f]][, c] * rw
        }
      } # end of wiggling formants
    } # end of if temperature > 0

    ## Deterministic part
    for (f in seq_along(formants_upsampled)) {
      formants_upsampled[[f]][, 'freq'] =
        formants_upsampled[[f]][, 'freq'] / bin_width
      # frequencies expressed in bin indices (how many bin widths above the first bin)
      formants_upsampled[[f]][, 'width'] =
        formants_upsampled[[f]][, 'width'] / bin_width * formantWidth
    }

    # mouth opening
    if (length(mouth) < 1 || any(is.na(mouth))) {
      mouthOpening_upsampled = rep(0.5, nc)
      # defaults to mouth half-open the whole time
      mouthOpen_binary = rep(1, nc)
    } else {
      mouthOpening_upsampled = do.call(getSmoothContour, c(smoothing, list(
        len = nc,
        anchors = mouth,
        valueFloor = permittedValues['mouthOpening', 'low'],
        valueCeiling = permittedValues['mouthOpening', 'high'],
        plot = FALSE
      )))
      # mouthOpening_upsampled[mouthOpening_upsampled < mouthOpenThres] = 0
      mouthOpen_binary = ifelse(mouthOpening_upsampled > mouthOpenThres, 1, 0)
    }
    # plot(mouthOpening_upsampled, type = 'l')

    # adjust formants for mouth opening
    adjustment_bins = 0
    if (!is.null(vocalTract)) {
      if (!any(is.na(vocalTract))) {
        # is.finite() returns F for NaN, NA, inf, etc
        adjustment_hz = (mouthOpening_upsampled - 0.5) * speedSound /
          (4 * vocalTract) # speedSound = 35400 cm/s, speed of sound in warm
        # air. The formula for mouth opening is adapted from Moore (2016) "A
        # Real-Time Parametric General-Purpose Mammalian Vocal Synthesiser".
        # mouthOpening = .5 gives no modification (neutral, "default" position).
        # Basically we could assume a closed-closed tube for closed mouth and a
        # closed-open tube for open mouth, but since formants can be specified
        # rather than calculated based on vocalTract, we just subtract half the
        # total difference between open and closed tubes in Hz from each formant
        # value as the mouth goes from half-open (neutral) to fully closed, or
        # we add half that value as the mouth goes from neutral to max open. NB:
        # so "closed" is actually "half-closed", and we assume that nostrils are
        # always open (so not really a closed-closed tube)
        adjustment_bins = adjustment_hz / bin_width
      }
    }
    for (f in 1:nFormants) {
      formants_upsampled[[f]][, 'freq'] =
        formants_upsampled[[f]][, 'freq'] + adjustment_bins
      # force each formant frequency to be positive (min 1 bin)
      formants_upsampled[[f]][, 'freq'] [formants_upsampled[[f]][, 'freq'] < 1] = 1
    }

    # nasalize the parts with closed mouth: see Hawkins & Stevens (1985);
    # http://www.cslu.ogi.edu/tutordemos/SpectrogramReading/cse551html/cse551/node35.html
    if (!any(is.na(formants_upsampled[[1]]$freq))) {
      nasalizedIdx = which(mouthOpen_binary == 0) # or specify a separate
      # increase F1 bandwidth to 175 Hz
      formants_upsampled$f1[nasalizedIdx, 'width'] = 175 / bin_width
      # nasalization contour
      if (length(nasalizedIdx) > 0) {
        # add a pole
        formants_upsampled$fnp = formants_upsampled$f1
        formants_upsampled$fnp[, 'amp'] = 0
        formants_upsampled$fnp[nasalizedIdx, 'amp'] = NA
        formants_upsampled$fnp[nasalizedIdx, 'width'] =
          formants_upsampled$f1[nasalizedIdx, 'width'] * 2 / 3
        formants_upsampled$fnp[nasalizedIdx, 'freq'] =
          ifelse(
            formants_upsampled$f1[nasalizedIdx, 'freq'] > 550 / bin_width,
            formants_upsampled$f1[nasalizedIdx, 'freq'] - 250 / bin_width,
            formants_upsampled$f1[nasalizedIdx, 'freq'] + 250 / bin_width
          )
        # 250 Hz below or above F1, depending on whether F1 is above or below
        # 550 Hz

        # add a zero
        formants_upsampled$fnz = formants_upsampled$f1
        formants_upsampled$fnz[, 'amp'] = 0
        formants_upsampled$fnz[nasalizedIdx, 'amp'] = -20
        formants_upsampled$fnz[nasalizedIdx, 'freq'] =
          (formants_upsampled$fnp[nasalizedIdx, 'freq'] +
             formants_upsampled$f1[nasalizedIdx, 'freq']) / 2  # midway between
        # f1 and fnp
        formants_upsampled$fnz[nasalizedIdx, 'width'] =
          formants_upsampled$fnp[nasalizedIdx, 'width']
        # modify f1
        formants_upsampled$f1[nasalizedIdx, 'amp'] =
          formants_upsampled$f1[nasalizedIdx, 'amp'] * 4 / 5
        formants_upsampled$f1[nasalizedIdx, 'width'] =
          formants_upsampled$f1[nasalizedIdx, 'width'] * 5 / 4
        nFormants = length(formants_upsampled)
        amplScaleFactor = c(amplScaleFactor, .5, .5)
        # make the added zero-pole half as strong as ordinary formants
      }
    }

    # Add formants to spectrogram (Stevens 2000, Ch. 3, ~p. 137)
    freqs_bins = 1:nr
    poles = 1:nFormants
    zeros = as.numeric(which(vapply(
      formants_upsampled, function(x) any(x[, 'amp'] < 0), logical(1)
    )))
    if (length(zeros) > 0) {
      poles = poles[-zeros]
      for (z in zeros)
        formants_upsampled[[z]]$amp = -formants_upsampled[[z]]$amp
      # need to have positive amp values (we know which ones are zeros)
    }
    s = complex(real = 0, imaginary = 2 * pi * freqs_bins)
    formantFilter_list = vector('list', nFormants)
    for (f in 1:nFormants) {
      # vectors of length nc
      pf = 2 * pi * formants_upsampled[[f]]$freq
      bp = -formants_upsampled[[f]]$width * pi
      sf = complex(real = bp, imaginary = pf)
      sfc = Conj(sf)
      # vectors of length nc
      formant = matrix(0, nrow = nr, ncol = nc)
      for (c in 1:nc) {
        pole = any(poles == f)
        numerator = sf[c] * sfc[c]  # scalar
        denominator = (s - sf[c]) * (s - sfc[c])  # vector of length nr
        if (pole) {
          tns =  numerator / denominator  # pole
        } else {
          tns = denominator / numerator   # zero
        }
        formant[, c] = log10(abs(tns))
        if (is.na(formants_upsampled[[f]]$amp[c])) {
          # just convert to dB
          formant[, c] = formant[, c] * 20 * amplScaleFactor[f]
        } else {
          # normalize ampl to be exactly as specified in dB
          m = if (pole) max(formant[, c]) else max(1e-10, abs(min(formant[, c])))
          formant[, c] = formant[, c] / m *
            formants_upsampled[[f]]$amp[c] * amplScaleFactor[f]
          # amplScaleFactor is 1 for user-specified and formantDepStoch otherwise
        }
      }
      # plot(formant[, c], type = 'l')
      formantFilter_list[[f]] = formant
    }
    formantFilter = Reduce(`+`, formantFilter_list) * formantDep
  } else {
    mouthOpen_binary = rep(1, nc)
    mouthOpening_upsampled = rep(0.5, nc)
    formantFilter = matrix(0, nrow = nr, ncol = nc)
  }
  # plot(formantFilter[, 1], type = 'l')

  # save frequency and time stamps
  freqs = seq(0, samplingRate / 2, length.out = nr)  # Hz
  rownames(formantFilter) = freqs / 1000 # kHz
  if (is.numeric(duration)) {
    colnames(formantFilter) = seq(0, duration, length.out = nc)
  } else {
    colnames(formantFilter) = seq(0, 1, length.out = nc)
  }
  # plot(freqs, formantFilter[, 1], type = 'l')
  # image(t(formantFilter))

  # add empirical correction for not adding higher formants
  if (is.null(formantCeiling) && is.list(formants)) {
    if (length(vocalTract) == 1) {
      # the numbers below represent an even faster empirical correction
      # (coefficients estimated with nonlinear regression fit to spectral slopes
      # with increasing formantCeiling)
      # rolloffAdjust = 2^(0.6731 + 1.08 * log2(vocalTract) - 2.044 * log2(nr) +
      #                      4.769e-5 * samplingRate + 2.03 * log2(1:nr))
      rolloffAdjust = aboveNyquistCorrection(
        freqs_hz = freqs,
        vocalTract = vocalTract,
        speedSound = speedSound,
        samplingRate = samplingRate,
        K = 10 * nFormants_integer
      )
      formantFilter = formantFilter + rolloffAdjust  # adds to each column
    } else {
      # const_term = 0.6731 - 2.044 * log2(nr) + 4.769e-5 * samplingRate + 2.03 * log2(1:nr)
      # rolloffAdjust = vapply(
      #   vocalTract,
      #   function(x) 2^(const_term + 1.08 * log2(x)),
      #   numeric(nr))
      K = 10 * nFormants_integer
      rolloffAdjust = vapply(
        vocalTract,
        function(x) aboveNyquistCorrection(
          freqs_hz = freqs,
          vocalTract = x,
          speedSound = speedSound,
          samplingRate = samplingRate,
          K = K
        ),
        numeric(nr))
      formantFilter = formantFilter + rolloffAdjust
    }
  }
  # END OF FORMANTS

  # add lip radiation when the mouth is open and nose radiation when the mouth
  # is closed
  lip_dB = lipRad * log2(1:nr) # vector of length nr
  nose_dB = noseRad * log2(1:nr)
  # plot(lip_dB, type = 'l'); plot(nose_dB, type = 'l')
  formantFilter = formantFilter +
    outer(lip_dB, mouthOpen_binary) +
    outer(nose_dB, 1 - mouthOpen_binary) +
    openMouthBoost * matrix(mouthOpen_binary, nr, nc, byrow = TRUE)
  # plot(formantFilter[, 1], type = 'l')

  # convert from dB to linear multiplier of power spectrum
  formantFilter_lin = 10 ^ (formantFilter / 20)
  # plot(formantFilter_lin[, 1], type = 'l')

  if (plot) {
    if (is.null(col)) {
      colfunc = switchColorTheme(colorTheme)
      col = colfunc(100)
    }
    image(x = as.numeric(colnames(formantFilter)),
          y = as.numeric(rownames(formantFilter)),
          z = t(formantFilter),
          xlab = xlab,
          ylab = ylab,
          col = col,
          ...)
  }

  if (output == 'detailed') {
    if (exists('formants_upsampled')) {
      formantSummary = as.data.frame(matrix(NA, nrow = nFormants, ncol = nc))
      for (i in 1:nFormants) {
        formantSummary[i, ] = (formants_upsampled[[i]]$freq - 1) *
          bin_width + bin_width / 2  # from bins back to Hz
      }
      max_freqs = apply(formantSummary, 1, max)  # save only to Nyquist
      formantSummary = formantSummary[which(max_freqs < (samplingRate / 2)), ]
      colnames(formantSummary) = colnames(formantFilter)
    } else {
      formantSummary = NULL
    }
    invisible(list(formantSummary = formantSummary,
                   specEnv = formantFilter_lin,
                   specEnv_dB = formantFilter))
  } else {
    invisible(formantFilter_lin)
  }
}


#' Correct for the aggregate contribution of unmodeled resonances above Nyquist.
#' Main idea: every above-ceiling pole adds a small parabolic-in-f boost, and
#' the total correction is the sum over all of them.
#' @noRd
#' @return A correction vector in dB of length nr
aboveNyquistCorrection = function(
    freqs_hz, vocalTract, speedSound,
    samplingRate, formantCeiling = NULL,
    K = 100) {
  dF = speedSound / (2 * vocalTract)
  ceilingFreq = if (is.null(formantCeiling)) samplingRate / 2
  else formantCeiling * samplingRate / 2

  # first uniform-tube pole strictly above the ceiling
  nStart = floor((2 * ceilingFreq / dF + 1) / 2) + 1
  f2 = freqs_hz^2
  corr = numeric(length(freqs_hz))

  if (K > 0) {
    for (j in 0:(K - 1)) {
      n = nStart + j
      Fn = (2 * n - 1) * dF / 2
      Bn = getBandwidth(Fn)
      Fn2 = Fn^2
      # exact |H_n(f)|^2 = Fn^4 / ((Fn^2 - f^2)^2 + Bn^2 f^2)
      corr = corr + 10 * log10(Fn2^2 / ((Fn2 - f2)^2 + Bn^2 * f2))
    }
  }
  corr   # dB, length nr
}


#' Add formants
#'
#' A spectral filter that either adds or removes formants from a sound - that
#' is, amplifies or dampens certain frequency bands, as in human vowels. See
#' \code{\link{soundgen}} and \code{\link{getFormantFilter}} for more
#' information. With \code{action = 'remove'} this function can perform inverse
#' filtering to remove formants and obtain raw glottal output, provided that you
#' can specify the correct formant structure. Instead of formants, any arbitrary
#' spectral filtering function can be applied using the \code{formantFilter}
#' argument (e.g., for a low/high/bandpass filter).
#'
#' Algorithm: converts input from a time series (time domain) to a spectrogram
#' (frequency domain) through short-time Fourier transform (STFT), multiplies by
#' the spectral filter containing the specified formants, and transforms back to
#' a time series via inverse STFT. This is a subroutine for voice synthesis in
#' \code{\link{soundgen}}, but it can also be applied to a recording.
#'
#' @seealso \code{\link{getFormantFilter}} \code{\link{transplantFormants}}
#'   \code{\link{soundgen}}
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams soundgen
#' @param action 'add' = add formants to the sound (default), 'remove' = remove
#'   formants (inverse filtering)
#' @param specificity a way to sharpen or blur the spectral envelope (spectrum ^
#'   specificity) : 1 = no change, >1 = sharper, <1 = blurred
#' @param formantFilter (optional): as an alternative to specifying formant
#'   frequencies, we can provide the exact filter - a vector of non-negative
#'   numbers specifying the amplitude in each frequency bin on a linear scale. A
#'   matrix specifying the filter for each STFT step with frequency bins in rows
#'   and STFT frames in columns is also accepted. The easiest way to create this
#'   matrix is to call \code{\link{getFormantFilter}} or to use the spectrum
#'   of a recorded sound
#' @param zFun (optional) an arbitrary function to apply to the spectrogram
#'   prior to iSTFT, where "z" is the spectrogram - a matrix of complex values
#'   (see examples)
#' @param dB if NULL (default), the spectral envelope is applied on the original
#'   scale; otherwise, it is set to range up to 10^(dB / 20)
#' @param formDrift,formDisp scaling factors for the effect of temperature on
#'   formant drift and dispersal, respectively
#' @param ... extra parameters passed to \code{zFun}
#'
#' @return The filtered waveform as a numeric vector of the original length with
#'   the original sampling rate, or a list if there are multiple inputs.
#' @export
#' @examples
#' sound = c(rep(0, 1000), rnorm(8000) * 2 - 1, rep(0, 1000))  # white noise
#' # NB: pad with silence to avoid artifacts if removing formants
#' # playme(sound)
#' # spectrogram(sound, samplingRate = 16000)
#'
#' # add F1 = 900, F2 = 1300 Hz
#' sound_filtered = addFormants(sound, samplingRate = 16000,
#'                              formants = c(900, 1300))
#' # playme(sound_filtered)
#' # spectrogram(sound_filtered, samplingRate = 16000)
#'
#' # ...and remove them again (assuming we know what the formants are)
#' sound_inverse_filt = addFormants(sound_filtered,
#'                                  samplingRate = 16000,
#'                                  formants = c(900, 1300),
#'                                  action = 'remove')
#' # playme(sound_inverse_filt)
#' # spectrogram(sound_inverse_filt, samplingRate = 16000)
#'
#' \dontrun{
#' ## Perform some user-defined manipulation of the spectrogram with zFun
#' # Ex.: noise removal - silence all bins 50 dB below the max value
#' s_noisy = soundgen(sylLen = 200, addSilence = 0,
#'                    noise = list(time = c(-100, 300), value = -20))
#' spectrogram(s_noisy, 16000)
#' # playme(s_noisy)
#' zFun = function(z, cutoff = -50) {
#'   az = abs(z)
#'   thres = max(az) * 10 ^ (cutoff / 20)
#'   z[which(az < thres)] = 0
#'   return(z)
#' }
#' s_denoised = addFormants(s_noisy, samplingRate = 16000,
#'                          formants = NA, zFun = zFun, cutoff = -40)
#' spectrogram(s_denoised, 16000)
#' # playme(s_denoised)
#'
#' # If neither formants nor formantFilter are defined, only lipRad has an effect
#' # For ex., we can boost low frequencies by 6 dB/oct
#' noise = rnorm(8000)
#' noise1 = addFormants(noise, 16000, lipRad = -6)
#' meanSpectrum(noise1, 16000, yScale = 'max0')
#'
#' # Arbitrary spectra can be defined with formantFilter. For ex., we can
#' # have a flat spectrum up to 2 kHz (Nyquist / 4) and -3 dB/kHz above:
#' freqs = seq(0, 16000 / 2, length.out = 100)
#' n = length(freqs)
#' idx = (n / 4):n
#' sp_dB = c(rep(0, n / 4 - 1), (freqs[idx] - freqs[idx[1]]) / 1000 * (-3))
#' plot(freqs, sp_dB, type = 'b')
#' noise2 = addFormants(noise, 16000, lipRad = 0, formantFilter = 10 ^ (sp_dB / 20))
#' meanSpectrum(noise2, 16000, yScale = 'max0')
#'
#' ## Use the spectral envelope of another recording
#' # (NB: this can also be achieved with a single call to transplantFormants)
#' sound_orig = soundgen(sylLen = 300, formants = 'a', addSilence = 5)
#' samplingRate = 16000
#' # playme(sound_orig, samplingRate)
#'
#' # get a few pitch anchors to reproduce the original intonation
#' pitch = analyze(sound_orig, samplingRate = samplingRate,
#'   pitchMethod = c('autocor', 'dom'))$detailed$pitch
#' pitch = pitch[!is.na(pitch)]
#'
#' # extract a frequency-smoothed version of the original spectrogram
#' # to use as filter
#' specEnv_orig = spectrogram(sound_orig, blur = c(300, 50),
#'  samplingRate = samplingRate, output = 'original', plot = TRUE)
#'
#' # Synthesize source only, with flat spectrum
#' sound_unfilt = soundgen(sylLen = 2500, pitch = pitch,
#'   rolloff = 0, rolloffOct = 0,
#'   temperature = 0, formants = NULL, lipRad = 0,
#'   samplingRate = samplingRate,
#'   invalidArgAction = 'ignore')  # prevent soundgen from increasing samplingRate
#' # playme(sound_unfilt, samplingRate)
#' # meanSpectrum(sound_unfilt, samplingRate, yScale = 'max0')  # ~flat
#'
#' # Force spectral envelope to the shape of target
#' sound_filt = addFormants(sound_unfilt, formants = NULL,
#'   formantFilter = specEnv_orig, samplingRate = samplingRate)
#' # playme(sound_filt, samplingRate)  # playme(sound_orig, samplingRate)
#' # spectrogram(sound_filt, samplingRate)  # spectrogram(sound_orig, samplingRate)
#'
#' # The spectral envelope is now similar to the original recording. Compare:
#' par(mfrow = c(1, 2))
#' meanSpectrum(sound_orig, samplingRate, yScale = 'max0', alim = c(-50, 20))
#' meanSpectrum(sound_filt, samplingRate, yScale = 'max0', alim = c(-50, 20))
#' par(mfrow = c(1, 1))
#' }
addFormants = function(
    x,
    samplingRate = NULL,
    formants = NULL,
    formantFilter = NULL,
    action = c('add', 'remove'),
    dB = NULL,
    specificity = 1,
    zFun = NULL,
    vocalTract = NA,
    formantDep = 1,
    formantDepStoch = 1,
    formantWidth = 1,
    formantCeiling = NULL,
    lipRad = 6,
    noseRad = 4,
    mouthOpenThres = 0,
    mouth = NA,
    temperature = 0.025,
    formDrift = 0.3,
    formDisp = 0.2,
    smoothing = list(interpol = 'splineFC'),
    windowLength = 50,
    step = NULL,
    overlap = 75,
    wn = "gaussian",
    normalize = c('orig', 'max', 'none'),
    play = FALSE,
    saveAudio = FALSE,
    reportEvery = NULL,
    cores = 1,
    ...
) {
  action = match.arg(action)
  normalize = match.arg(normalize)
  formants = reformatFormants(formants)
  mouth = reformatAnchors(mouth)

  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.addFormants',
                    suffix = 'addFormants',
                    saveAudio = saveAudio,
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


#' Add formants per sound
#' @noRd
.addFormants = function(
    audio,
    formants = NULL,
    formantFilter = NULL,
    action = c('add', 'remove'),
    dB = NULL,
    specificity = 1,
    zFun = NULL,
    vocalTract = NA,
    formantDep = 1,
    formantDepStoch = 1,
    formantWidth = 1,
    formantCeiling = NULL,
    lipRad = 6,
    noseRad = 4,
    mouthOpenThres = 0,
    mouth = NA,
    temperature = 0.025,
    formDrift = 0.3,
    formDisp = 0.2,
    smoothing = list(),
    windowLength = 50,
    step = NULL,
    overlap = 75,
    dynamicRange = 120,
    wn = "gaussian",
    normalize = c('max', 'orig', 'none'),
    play = FALSE,
    ...
) {
  action = match.arg(action)
  normalize = match.arg(normalize)
  dynamicRange_lin = 10 ^ (-dynamicRange / 20)
  if (is.null(audio$ls)) audio$ls = length(audio$sound)
  val = validateWlOvlp(audio, windowLength, step, overlap)
  overlap = val$overlap

  # prepare vocal tract filter (formants + some spectral noise + lip radiation)
  if (any(!is.finite(audio$sound)) ||
      !any(audio$sound != 0) ||
      (isTRUE(is.na(formants)) &&
       is.na(vocalTract)[1] &&
       is.null(formantFilter) &&
       (is.na(lipRad)[1] || lipRad == 0)) ||
      audio$ls < 3) {
    # otherwise fft glitches
    soundFiltered = audio$sound
  } else {
    wl = nextn(round(windowLength * audio$samplingRate / 1000))
    # NB: wl is automatically increased to the closest number with many factors to speed up FFT
    step = max(1, round(wl - (overlap * wl / 100)))

    # pad input with ~wl of 0 to avoid softening the attack
    zp_left = wl
    new_length = audio$ls + 2 * wl
    # we want the new length to be such that stft_simple wouldn't drop any points,
    # for which we need: (length(sound) - wl) %% step == 0. So:
    new_length = ceiling((new_length - wl) / step) * step + wl
    zp_right = new_length - audio$ls - zp_left
    sound = c(rep(0, zp_left),
              audio$sound,
              rep(0, zp_right))

    # STFT
    z = stft_simple(sound, wl = wl, step = step, wn = wn, zp = 0)
    nr = nrow(z) %/% 2 + 1
    z = z[seq_len(nr), ]
    nc = ncol(z)

    # are formants moving or stationary?
    # (basically always moving, unless temperature = 0 and nothing else changes)
    if (is.null(formantFilter)) {
      if (temperature > 0) {
        movingFormants = TRUE
      } else {
        if (is.list(formants)) {
          max_n_anchors = unlist(lapply(formants,
                                        function(x) vapply(x, length, numeric(1))))
          movingFormants = max(max_n_anchors) > 1
        } else {
          movingFormants = FALSE
        }
        if (is.list(mouth)) {
          if (sum(mouth$value != .5) > 0) {
            movingFormants = TRUE
          }
        }
        if (is.list(vocalTract)) {
          if (length(vocalTract$value) > 1) {
            movingFormants = TRUE
          }
        }
      }
      nInt = ifelse(movingFormants, nc, 1)

      # prepare the filter
      formantFilter = getFormantFilter(
        nr = nr,
        nc = nInt,
        formants = formants,
        formantDep = formantDep,
        formantDepStoch = formantDepStoch,
        formantWidth = formantWidth,
        formantCeiling = formantCeiling,
        lipRad = lipRad,
        noseRad = noseRad,
        mouthOpenThres = mouthOpenThres,
        mouth = mouth,
        temperature = temperature,
        formDrift = formDrift,
        formDisp = formDisp,
        smoothing = smoothing,
        samplingRate = audio$samplingRate,
        vocalTract = vocalTract
      )
    } else {  # user-provided formantFilter
      # if a vector, becomes a matrix with one row
      if (is.matrix(formantFilter) && ncol(formantFilter) > 1) {
        nInt = nc
        movingFormants = TRUE
      } else {  # vector
        formantFilter = matrix(formantFilter, ncol = 1)
        nInt = 1
        movingFormants = FALSE
      }
      formantFilter = interpolMatrix(
        formantFilter,
        nr = nr,
        nc = nInt,
        interpol = 'approx'
      )
    }
    # image(t(formantFilter))

    # filtering
    if (is.finite(specificity) && specificity != 1)
      formantFilter = formantFilter ^ specificity
    if (!is.null(dB) && is.numeric(dB)) {
      # rescale the filter (needed for noise removal)
      max_filt = max(formantFilter, na.rm = TRUE)
      if (is.finite(max_filt) && max_filt > 0) {
        se_resc = formantFilter / max_filt * max(abs(z), na.rm = TRUE) *
          10 ^ (dB / 20)
        se_resc[se_resc < 1] = 1
        formantFilter = se_resc
      }
    }

    if (action == 'add') {
      if (movingFormants) {
        z = z * formantFilter
      } else {
        z = sweep(z, MARGIN = 1, as.vector(formantFilter), `*`)
      }
    } else if (action == 'remove') {
      if (movingFormants) {
        z = z / pmax(formantFilter, 1e-10)  # avoid division by 0
      } else {
        z = sweep(z, MARGIN = 1, as.vector(pmax(formantFilter, 1e-10)), `/`)
      }
    }

    # apply some arbitrary function to the spectrogram before iSTFT
    if (!is.null(zFun) && is.function(zFun))
      z = do.call(zFun, list(z = z, ...))

    # anything under dynamicRange becomes 0
    z[!is.finite(z)] = 0
    if (is.finite(dynamicRange_lin)) {
      abs_z = abs(z)
      mz = max(abs_z)
      if (mz > 0)
        z[abs_z / mz <= dynamicRange_lin] = 0
    }

    # i-STFT
    soundFiltered = istft_simple(
      z,
      wl = wl, step = step,
      wn = wn, type = "half"
    )
    # spectrogram(soundFiltered, audio$samplingRate)

    # remove zero padding and then add a quick linear fade
    soundFiltered = soundFiltered[(zp_left + 1):
                                    (zp_left + audio$ls)]
    soundFiltered = .fade(list(sound = soundFiltered, ls = audio$ls),
                          fadeIn_points = 10, fadeOut_points = 10, shape = 'lin')

    # normalize
    if (normalize == 'max' || normalize == TRUE) {
      ms = max(abs(soundFiltered))
      if (is.finite(ms) && ms > 0) {
        # soundFiltered = soundFiltered - mean(soundFiltered)
        soundFiltered = soundFiltered / ms * audio$scale
      }
    } else if (normalize == 'orig') {
      ms = max(abs(soundFiltered))
      if (is.finite(ms) && ms > 0) {
        soundFiltered = soundFiltered / ms * audio$scale_used
      }
    }
    # osc(soundFiltered, audio$samplingRate)
  }

  if (isTRUE(play)) {
    playme(soundFiltered, audio$samplingRate)
  } else if (is.character(play)) {
    playme(soundFiltered, audio$samplingRate, player = play)
  }

  # save audio
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(soundFiltered, audio = audio, filename = filename)
  }
  # spectrogram(soundFiltered, audio$samplingRate, ylim = c(0, 4))
  # playme(soundFiltered, audio$samplingRate)
  invisible(soundFiltered)
}


#' Transplant formants
#'
#' Takes the general spectral envelope of one sound (\code{donor}) and
#' "transplants" it onto another sound (\code{recipient}). For biological sounds
#' like speech or animal vocalizations, this has the effect of replacing the
#' formants in the recipient sound while preserving the original intonation and
#' (to some extent) voice quality. Note that the amount of spectral smoothing
#' (specified with \code{freqWindow}) is a crucial parameter: too little
#' smoothing, and noise between harmonics will be amplified, creating artifacts;
#' too much, and formants may be missed. The default is to set \code{freqWindow}
#' to the estimated median pitch, but this is time-consuming and error-prone, so
#' set it to a reasonable value manually if possible; if pitch detection fails,
#' \code{freqWindow} defaults to 400 Hz. Also ensure that both sounds have the
#' same sampling rate. You may want to \code{\link{fade}} the output a little (a
#' very short linear fade-in/out is applied internally).
#'
#' Algorithm: makes spectrograms of both sounds, flattens the recipient
#' spectrogram by dividing out its smoothed spectral envelope (obtained with
#' \code{\link{getSpecEnv}}), smooths the donor spectrogram (or interpolates the
#' supplied filter matrix) with \code{\link{getSpecEnv}}, multiplies the
#' spectrograms, and transforms back into time domain with inverse STFT. To
#' avoid amplifying noise, spectral bins more than \code{dynamicRange} dB below
#' the peak of their frame are left untouched, and the original amplitude of
#' each recipient frame is preserved. Anything more than \code{dynamicRange} dB
#' below the global maximum is then zeroed out.
#'
#' @seealso \code{\link{transplantEnv}} \code{\link{getFormantFilter}}
#'   \code{\link{addFormants}} \code{\link{getSpecEnv}}
#'   \code{\link{shiftFormants}} \code{\link{shiftPitch}}
#'
#' @inheritParams .roxygen_defaults
#' @param donor either the sound that provides the formants (vector, Wave, or
#'   file) or the desired spectral filter (matrix) as returned by
#'   \code{\link{getFormantFilter}} or \code{\link{spectrogram}} - linear
#'   amplitude, frequency in rows, time in columns
#' @param recipient the sound that receives the formants (vector, Wave, or file)
#' @param samplingRate sampling rate (Hz) of both \code{donor} and
#'   \code{recipient}, which must match
#' @param freqWindow the width of spectral smoothing window, Hz: a single
#'   positive number, roughly the expected spacing between harmonics. Defaults
#'   to the median pitch of the donor (or of the recipient if donor is a filter
#'   matrix); if pitch detection fails, defaults to 400 Hz with a message
#' @param specEnvMethod the method of extracting a smoothed spectral envelope:
#'   "cepstral" = Gaussian liftering of the real cepstrum (default), "gauss" =
#'   Gaussian blur of the log spectrum, "movavg" = moving average of the log
#'   spectrum, "peak" = moving maximum (upper envelope). See
#'   \code{\link{getSpecEnv}} for details
#' @param normalize "orig" = same as donor / recipient (default), "max" = max
#'   possible amplitude of the donor given its scale (or of the recipient if
#'   donor is a filter matrix), "none" = no normalization
#'
#' @return The filtered waveform as a numeric vector with the original sampling
#'   rate, on a scale determined by the \code{normalize} argument and with the
#'   same duration as \code{recipient}.
#' @export
#' @examples
#' rec = rnorm(5000)  # white noise
#' donor = soundgen()  # voiced /a/
#' whisper = transplantFormants(donor = donor, recipient = rec,
#'   samplingRate = 16000, freqWindow = 300)  # whispered /a/
#' # playme(whisper)
#' meanSpectrum(whisper, 16000)
#'
#' \dontrun{
#' # Objective: take formants from one sound and apply them to another
#' s_orig = soundgen(pitch = 100, formants = 'ai')
#'
#' recipient = soundgen(
#'   sylLen = 1200,
#'   pitch = c(100, 300, 250, 200),
#'   vibratoFreq = 9, vibratoDep = 1,
#'   formants = NULL,
#'   addSilence = 180,
#'   samplingRate = 16000,  # same as donor
#'   invalidArgAction = 'ignore')  # force to keep the low samplingRate
#' playme(recipient, 16000)
#' spectrogram(recipient, 16000)
#'
#' s1 = transplantFormants(
#'   donor = s_orig,
#'   recipient = recipient,
#'   samplingRate = 16000)
#' playme(s1, 16000)
#' spectrogram(s1, 16000)
#'
#' # The spectral envelope of s1 will be similar to that of the original on a
#' # frequency scale determined by freqWindow. Compare the spectra:
#' par(mfrow = c(1, 2))
#' meanSpectrum(s_orig, 16000, yScale = 'max0', ylim = c(-50, 0), main = 'Donor')
#' meanSpectrum(s1, 16000, yScale = 'max0', ylim = c(-50, 0),
#'              main = 'Processed recipient')
#' par(mfrow = c(1, 1))
#'
#' # if needed, transplant amplitude envelopes as well:
#' s2 = transplantEnv(donor = s_orig, recipient = s1,
#'                    samplingRateR = 16000, samplingRateD = 16000,
#'                    windowLength = 10)
#' playme(s2, 16000)
#' spectrogram(s2, 16000)
#' }
transplantFormants = function(
    donor,
    recipient,
    samplingRate = NULL,
    freqWindow = NULL,
    specEnvMethod = c('cepstral', 'gauss', 'movavg', 'peak'),
    dynamicRange = 80,
    windowLength = 50,
    step = NULL,
    overlap = 75,
    wn = 'gaussian',
    normalize = c('orig', 'max', 'none')) {
  specEnvMethod = match.arg(specEnvMethod)
  normalize = match.arg(normalize)
  donor_is_matrix = is.matrix(donor)
  if (is.null(step)) {
    if (is.null(overlap)) {
      stop('Need to specify either step or overlap')
    } else {
      if (!is.finite(overlap) || length(overlap) > 1 ||
          any(overlap < 0) || any(overlap >= 100))
        stop('overlap must be >=0 and < 100%')
      step = windowLength * (1 - overlap / 100)
    }
  }
  if (!is.finite(step) || step <= 0) stop('step must be positive')
  if (is.finite(dynamicRange) && dynamicRange <= 0)
    stop('dynamicRange must be a finite positive number')
  dynamicRange_lin = 10 ^ (-dynamicRange / 20)
  if (!is.finite(dynamicRange_lin)) dynamicRange_lin = NA

  # Read inputs
  recipient = readAudio(recipient,
                        input = checkInputType(recipient),
                        samplingRate = samplingRate)
  if (!donor_is_matrix) {
    # donor is a sound
    donor = readAudio(donor,
                      input = checkInputType(donor),
                      samplingRate = samplingRate)
    # Check that both sounds have the same sampling rate
    if (donor$samplingRate != recipient$samplingRate) {
      stop('Please use two sounds with the same sampling rate')
    }
  }
  if (isTRUE(recipient$failed) || isTRUE(donor$failed))
    stop('Failed to read the donor or the recipient')
  samplingRate = recipient$samplingRate  # donor may be a matrix (filter)

  # Choose the width of smoothing window (before padding the recipient)
  if (is.numeric(freqWindow)) {
    if (length(freqWindow) != 1 || !is.finite(freqWindow) || freqWindow <= 0)
      stop('freqWindow must be a single positive number (Hz)')
  } else {
    if (donor_is_matrix) {
      # set freqWindow to the median pitch of recipient
      anal = analyze(recipient$sound, samplingRate, plot = FALSE)
    } else {
      # set freqWindow to the median pitch of donor
      anal = analyze(donor$sound, samplingRate, plot = FALSE)
    }
    freqWindow = median(anal$detailed$pitch, na.rm = TRUE)
    if (!is.finite(freqWindow)) {
      freqWindow = 400
      message('Failed to determine freqWindow based on pitch; defaulting to 400 Hz')
    }
  }

  # STFT parameters
  if (!is.finite(windowLength) || windowLength <= 0)
    stop('windowLength must be positive')
  wl = nextn(round(windowLength / 1000 * samplingRate))
  if (wl < 4)
    stop('windowLength is too short for the given samplingRate (need at least 4 points)')
  step_points = max(1, round(step / 1000 * samplingRate))
  if (recipient$ls < wl) {
    # recipient shouldn't be shorter than wl, or stft_simple will reset wl
    recipient$sound = c(recipient$sound, rep(0, wl - recipient$ls))
    recipient$ls = wl
  }

  # pad recipient with ~wl of 0 to avoid softening the attack
  zp_left = wl
  new_length = recipient$ls + 2 * wl
  # we want the new length to be such that stft_simple wouldn't drop any points,
  # for which we need: (length(sound) - wl) %% step_points == 0. So:
  new_length = ceiling((new_length - wl) / step_points) * step_points + wl
  zp_right = new_length - recipient$ls - zp_left
  recipient$sound = c(rep(0, zp_left),
                      recipient$sound,
                      rep(0, zp_right))

  # STFT recipient
  spec_recipient = stft_simple(recipient$sound,
                               samplingRate = samplingRate,
                               wl = wl,
                               step = step_points,
                               wn = wn,
                               zp = 0)
  spec_recipient = spec_recipient[1:(nrow(spec_recipient) %/% 2 + 1), ]
  nr = nrow(spec_recipient)
  nc = ncol(spec_recipient)

  # STFT donor
  if (!donor_is_matrix) {
    # donor is a sound (pad with 0 to avoid artifacts w/o dropping any samples)
    spec_donor = stft_simple(c(rep(0, wl), donor$sound, rep(0, wl)),
                             samplingRate = samplingRate,
                             wl = wl,
                             step = step_points,
                             wn = wn,
                             zp = 0)
    spec_donor = Mod(spec_donor[1:(nrow(spec_donor) %/% 2 + 1), ])
    # Make sure the donor spec has the same dimensions as the recipient spec
    spec_donor_rightDim = interpolMatrix(spec_donor, nr = nr, nc = nc)
  } else {
    # donor is a matrix (spectrogram giving the desired formant structure)
    spec_donor_rightDim = interpolMatrix(donor, nr = nr, nc = nc)
  }
  rownames(spec_donor_rightDim) = rownames(spec_recipient)

  # Width of smoothing window, in bins
  freqRange_kHz = diff(range(as.numeric(rownames(spec_recipient))))
  freqBin_Hz = freqRange_kHz * 1000 / (nr - 1)
  freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
  if (freqWindow_bins < 3) {
    message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to 3 bins'))
    freqWindow_bins = 3
  }
  if (freqWindow_bins > nr) {
    message(paste('freqWindow cannot exceed the Nyquist frequency;
                  resetting to', floor(freqBin_Hz * nr)))
    freqWindow_bins = nr
  }

  # Flatten the recipient spectrogram and impose the donor's spectral envelope
  # in one pass; bins more than dynamicRange dB below the frame peak are left
  # untouched, so as not to amplify noise
  spec_recipient_new = .warpSpecEnv(
    spec_recipient,
    env_donor = getSpecEnv(spec_donor_rightDim,
                           freqWindow_bins = freqWindow_bins,
                           method = specEnvMethod),
    freqWindow_bins = freqWindow_bins,
    dynamicRange = dynamicRange,
    specEnvMethod = specEnvMethod,
    quiet = 'keep')
  # image(t(log(abs(spec_recipient_new))))

  # anything under dynamicRange becomes 0
  spec_recipient_new[!is.finite(spec_recipient_new)] = 0
  if (is.finite(dynamicRange_lin)) {
    abs_z = abs(spec_recipient_new)
    mz = max(abs_z)
    if (mz > 0)
      spec_recipient_new[abs_z / mz <= dynamicRange_lin] = 0
  }

  # Reconstruct the audio with i-STFT
  recipient_new = istft_simple(spec_recipient_new,
                               wl = wl,
                               step = step_points,
                               wn = wn, type = "half",
                               wnSyn = "wola")

  # remove zero padding and then add a quick linear fade
  recipient_new = recipient_new[(zp_left + 1):(zp_left + recipient$ls)]
  recipient_new = .fade(list(sound = recipient_new, ls = recipient$ls),
                        fadeIn_points = 10, fadeOut_points = 10, shape = 'lin')

  # normalize
  if (donor_is_matrix) {
    scale_max = recipient$scale
    scale_orig = recipient$scale_used
  } else {
    scale_max = donor$scale
    scale_orig = donor$scale_used
  }
  ms = max(abs(recipient_new))
  if (normalize == 'max') {
    if (is.finite(ms) && ms > 0)
      recipient_new = recipient_new / ms * scale_max
  } else if (normalize == 'orig') {
    if (is.finite(ms) && ms > 0)
      recipient_new = recipient_new / ms * scale_orig
  }
  # spectrogram(donor$sound, samplingRate)
  # spectrogram(recipient_new, samplingRate)
  invisible(recipient_new)
}
