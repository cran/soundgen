### UTILITIES FOR ACOUSTIC ANALYSIS ###

#' Analyze fft frame
#'
#' Internal soundgen function.
#'
#' This function performs the heavy lifting of pitch tracking and acoustic
#' analysis in general: it takes the spectrum of a single fft frame as input and
#' analyzes it.
#' @param frame the abs spectrum of a frame, as returned by
#'   \code{\link[stats]{fft}}
#' @param bin spectrogram bin width, Hz
#' @param freqs frequency per bin of spectrogram
#' @param autoCorrelation pre-calculated autocorrelation of the input frame
#'   (computationally more efficient than to do it here)
#' @param samplingRate sampling rate (Hz)
#' @param trackPitch if TRUE, attempt to find F0 in this frame (FALSE if entropy
#'   is above some threshold - specified in \code{\link{analyze}})
#' @inheritParams analyze
#' @return Returns a list with two components: $pitchCands_frame contains pitch
#'   candidates for the frame, and $summaries contains other acoustic predictors
#'   like HNR, specSlope, etc.
#' @keywords internal
analyzeFrame = function(frame, bin, freqs,
                        autoCorrelation = NULL,
                        samplingRate,
                        cutFreq,
                        trackPitch = TRUE,
                        pitchMethods = c('dom', 'autocor'),
                        nCands,
                        pitchDom = list(),
                        pitchAutocor = list(),
                        pitchCep = list(),
                        pitchSpec = list(),
                        pitchHps = list(),
                        pitchFloor,
                        pitchCeiling) {
  absSpec = data.frame(freq = freqs,
                       amp = frame)
  # Cut spectral band from pitchFloor to cutFreq Hz (used for spectral
  # descriptives only - pitch tracking is always done with the full spectrum)
  if (is.null(cutFreq)) {
    absSpec_cut = absSpec
  } else {
    absSpec_cut = absSpec[absSpec$freq > cutFreq[1] &
                            absSpec$freq < cutFreq[2], ]
    # Above 5-6 kHz or so, spectral energy depends too much on the original
    # sampling rate, noises etc. Besides, those frequencies are not super
    # relevant to human vocalizations in any case. So we cut away all info above
    # 5 kHz before we calculate quartiles of spectral energy
  }

  ## DESCRIPTIVES
  # plot(absSpec_cut$freq, absSpec_cut$amp, type = 'l')
  amplitude = sum(absSpec_cut$amp)
  absSpec_cut$w = absSpec_cut$amp / amplitude
  specCentroid = sum(absSpec_cut$freq * absSpec_cut$w)
  peakFreq = absSpec_cut$freq[which.max(absSpec_cut$amp)]

  # quartiles of spectral energy distribution
  cums = cumsum(absSpec_cut$amp)
  quartile25 = absSpec_cut$freq[min(which(cums >= 0.25 * amplitude))]
  quartile50 = absSpec_cut$freq[min(which(cums >= 0.5 * amplitude))]
  quartile75 = absSpec_cut$freq[min(which(cums >= 0.75 * amplitude))]

  # get spectral slope in dB/kHz
  absSpec_cut$amp_dB = 20 * log10(absSpec_cut$amp)
  absSpec_cut$amp_dB = absSpec_cut$amp_dB - max(absSpec_cut$amp_dB)
  # plot(absSpec_cut$freq, absSpec_cut$amp_dB, type = 'l')
  specSlope = summary(lm(amp_dB ~ freq, data = absSpec_cut))$coef[2, 1] * 1000

  ## PITCH TRACKING
  frame = frame / max(frame) # plot(frame, type='l')

  # lowest dominant frequency band
  if (trackPitch & any(pitchMethods == 'dom')) {
    d = do.call(getDom,
                c(pitchDom,
                  list(frame = frame,
                       bin = bin,
                       freqs = freqs,
                       pitchFloor = pitchFloor,
                       pitchCeiling = pitchCeiling)))
    pitchCands_frame = d$dom_array
    dom = d$dom
  } else {
    pitchCands_frame = data.frame(
      'pitchCand' = numeric(),
      'pitchCert' = numeric(),
      'pitchSource' = character(),
      stringsAsFactors = FALSE,
      row.names = NULL
    )    # initialize an empty dataframe
    dom = NA
  }

  # autocorrelation (PRAAT)
  if (trackPitch & 'autocor' %in% pitchMethods) {
    pa = do.call(getPitchAutocor,
                 c(pitchAutocor,
                   list(autoCorrelation = autoCorrelation,
                        samplingRate = samplingRate,
                        nCands = nCands,
                        pitchFloor = pitchFloor,
                        pitchCeiling = pitchCeiling)))
    if(!is.null(pa$pitchAutocor_array)) {
      pitchCands_frame = rbind(pitchCands_frame, pa$pitchAutocor_array)
    }
    HNR = pa$HNR
  } else {
    HNR = NA
  }

  # cepstrum
  if (trackPitch & 'cep' %in% pitchMethods) {
    pitchCep_array = do.call(getPitchCep,
                             c(pitchCep,
                               list(frame = frame,
                                    samplingRate = samplingRate,
                                    bin = bin,
                                    nCands = nCands,
                                    pitchFloor = pitchFloor,
                                    pitchCeiling = pitchCeiling)))
    if(!is.null(pitchCep_array)) {
      pitchCands_frame = rbind(pitchCands_frame, pitchCep_array)
    }
  }

  # spectral: ratios of harmonics (BaNa)
  if (trackPitch & 'spec' %in% pitchMethods) {
    pitchSpec_array = do.call(getPitchSpec,
                              c(pitchSpec,
                                list(frame = frame,
                                     bin = bin,
                                     freqs = freqs,
                                     HNR = NULL,
                                     nCands = nCands,
                                     pitchFloor = pitchFloor,
                                     pitchCeiling = pitchCeiling)))
    if(!is.null(pitchSpec_array))
      pitchCands_frame = rbind(pitchCands_frame, pitchSpec_array)
  }

  # harmonic product spectrum (hps)
  if (trackPitch & 'hps' %in% pitchMethods) {
    pitchHps_array = do.call(getPitchHps,
                             c(pitchHps,
                               list(frame = frame,
                                    freqs = freqs,
                                    bin = bin,
                                    # nCands = nCands,
                                    pitchFloor = pitchFloor,
                                    pitchCeiling = pitchCeiling)))
    if(!is.null(pitchHps_array))
      pitchCands_frame = rbind(pitchCands_frame, pitchHps_array)
  }

  # some adjustments of pitch candidates
  if (nrow(pitchCands_frame) > 0) {
    pitchCands_frame[, 1:2] = apply(pitchCands_frame[, 1:2],
                                    2,
                                    function(x) as.numeric(x))
    # otherwise they become characters after rbind
  }
  if (nrow(pitchCands_frame[pitchCands_frame$pitchSource == 'dom', ]) > 0 &
      !is.na(HNR)) {
    pitchCands_frame$pitchCert[pitchCands_frame$pitchSource == 'dom'] =
      1 / (1 + exp(3 * HNR - 1)) # dom is worth more for noisy sounds,
    # but its weight approaches ~0.2 as HNR approaches 1
    # (NB: this is before HNR is converted to dB). Visualization:
    # a = seq(0, 1, length.out = 100)
    # b = 1 / (1 + exp(3 * a - 1))
    # plot (a, b, ylim = c(0, 1))
  }

  list(
    'pitchCands_frame' = pitchCands_frame,
    'summaries' = data.frame(
      HNR = HNR,
      dom = dom,
      specCentroid = specCentroid,
      peakFreq = peakFreq,
      quartile25 = quartile25,
      quartile50 = quartile50,
      quartile75 = quartile75,
      specSlope = specSlope
    )
  )
}


#' Get prior for pitch candidates
#'
#' Prior for adjusting the estimated pitch certainties in \code{\link{analyze}}.
#' For ex., if primarily working with speech, we could prioritize pitch
#' candidates in the expected pitch range (100-1000 Hz) and decrease our
#' confidence in candidates with very high or very low frequency as unlikely but
#' still remotely possible. You can think of this as a "soft" alternative to
#' setting absolute pitch floor and ceiling. Algorithm: the multiplier for each
#' pitch candidate is the density of prior distribution with mean = priorMean
#' (Hz) and sd = priorSD (semitones) normalized so max = 1 over [pitchFloor,
#' pitchCeiling]. Useful for previewing the prior given to
#' \code{\link{analyze}}.
#'
#' @seealso \code{\link{analyze}} \code{\link{pitch_app}}
#'
#' @return Returns a numeric vector of certainties of length \code{len} if
#'   pitchCands is NULL and a numeric matrix of the same dimensions as
#'   pitchCands otherwise.
#' @inheritParams analyze
#' @param len the required length of output vector (resolution)
#' @param distribution the shape of prior distribution on the musical scale:
#'   'normal' (mode = priorMean) or 'gamma' (skewed to lower frequencies)
#' @param plot if TRUE, plots the prior
#' @param ... additional graphical parameters passed on to plot()
#' @param pitchCands a matrix of pitch candidate frequencies (for internal
#'   soundgen use)
#' @export
#' @examples
#' soundgen:::getPrior(priorMean = 150,  # Hz
#'                     priorSD = 2)      # semitones
#' soundgen:::getPrior(150, 6)
#' s = soundgen:::getPrior(450, 24, pitchCeiling = 6000)
#' plot(s, type = 'l')
getPrior = function(priorMean,
                    priorSD,
                    distribution = c('normal', 'gamma')[1],
                    pitchFloor = 75,
                    pitchCeiling = 3000,
                    len = 100,
                    plot = TRUE,
                    pitchCands = NULL,
                    ...) {
  freqs = seq(HzToSemitones(pitchFloor),
              HzToSemitones(pitchCeiling),
              length.out = len)
  if (is.numeric(priorMean) & is.numeric(priorSD)) {
    priorMean_semitones = HzToSemitones(priorMean)
    if (distribution == 'normal') {
      prior_normalizer = dnorm(freqs, priorMean_semitones, priorSD)
    } else if (distribution == 'gamma') {
      shape = priorMean_semitones ^ 2 / priorSD ^ 2
      rate = priorMean_semitones / priorSD ^ 2
      prior_normalizer = dgamma(
        freqs,
        shape = shape,
        rate = rate
      )
    }
    prior_norm_max = max(prior_normalizer)
    prior = prior_normalizer / prior_norm_max
  } else {
    # flat prior
    prior = rep(1, len)
  }

  out = data.frame(freq = semitonesToHz(freqs),
                   prob = prior)
  if (plot) {
    plot(out, type = 'l', log = 'x',
         xlab = 'Frequency, Hz',
         ylab = 'Multiplier of certainty',
         main = 'Prior belief in pitch values',
         ...
    )
  }
  if (!is.null(pitchCands)) {
    if (is.numeric(priorMean) & is.numeric(priorSD)) {
      if (distribution == 'normal') {
        pitchCert_multiplier = dnorm(
          HzToSemitones(pitchCands), priorMean_semitones, priorSD
        ) / prior_norm_max
      } else if (distribution == 'gamma') {
        pitchCert_multiplier = dgamma(
          HzToSemitones(pitchCands), shape, rate
        ) / prior_norm_max
      }
    } else {
      pitchCert_multiplier = matrix(1, nrow = nrow(pitchCands),
                                    ncol = ncol(pitchCands))
    }
    invisible(pitchCert_multiplier)
  } else {
    invisible(out)
  }
}


#' Summarize the output of analyze()
#'
#' Internal soundgen function
#' @param result dataframe returned by analyze(summary = FALSE)
#' @param summaryFun summary functions
#' @param var_noSummary variables that should not be summarized
#' @keywords internal
summarizeAnalyze = function(
    result,
    summaryFun = c('mean', 'sd'),
    var_noSummary = c('duration', 'duration_noSilence', 'voiced', 'time', 'epoch')
) {
  if (is.character(var_noSummary)) {
    vars = colnames(result)[!colnames(result) %in% var_noSummary]
  } else {
    vars = colnames(result)
  }
  ls = length(summaryFun)
  lv = length(vars)
  vars_f = paste0(rep(vars, each = ls), '_', rep(summaryFun, each = lv))

  # pre-parse summary function names to speed things up
  functions = vector('list', length(summaryFun))
  for (f in seq_along(summaryFun)) {
    functions[[f]] = eval(parse(text = summaryFun[f]))
  }

  # apply the specified summary function to each column of result
  out = list()
  for (v in vars) {
    for (s in seq_len(ls)) {
      # remove NAs for the most common summary functions
      if (summaryFun[s] %in% c('mean', 'median', 'sd', 'min', 'max', 'range', 'sum')) {
        var_values = na.omit(result[, v])
      } else {
        var_values = result[, v]
      }
      var_f_name = paste0(v, '_', summaryFun[s])
      if (any(is.finite(var_values))) {
        # not finite, eg NA or -Inf - don't bother to calculate
        mySummary = do.call(functions[[s]], list(var_values))  # NAs already removed
        # for smth like range, collapse and convert to character
        if (length(mySummary) > 1) {
          mySummary = paste0(mySummary, collapse = ', ')
        }
        out[[var_f_name]] = mySummary
      } else {
        out[[var_f_name]] = NA
      }
    }
  }

  ## global measures that are not summarized per frame
  if (is.character(var_noSummary)) {
    # called from analyze()
    temp = result[1, c('duration', 'duration_noSilence')]
    len_voiced = length(which(!is.na(result$pitch)))
    temp$voiced = len_voiced / nrow(result)
    temp$voiced_noSilence = len_voiced / sum(!is.na(result$peakFreq))
    # could use sum(result$ampl > silence), but silence is re-set dynamically in analyze
    out = c(temp, out)
  }

  as.data.frame(out)
}


#' Update analyze
#'
#' Internal soundgen function
#'
#' Updates the output of analyze using manual pitch. Called by pitch_app().
#' @param result the matrix of results returned by analyze()
#' @param pitch_true manual pitch contour of length nrow(result), with NAs
#' @param spectrogram spectrogram with ncol = nrow(result)
#' @param freqs frequency labels of spectrogram bins
#' @param bin spectrogram bin width
#' @param harmHeight_pars same as argument "harmHeight" to analyze() - a list of
#'   settings passed to soundgen:::harmHeight()
#' @param smooth,smoothing_ww,smoothingThres smoothing parameters
#' @param varsToUnv set these variables to NA in voiceless frames
#' @keywords internal
updateAnalyze = function(
    result,
    pitch_true,
    pitchCands_list = NULL,
    spectrogram,
    freqs = NULL,
    bin = NULL,
    samplingRate = NULL,
    windowLength = NULL,
    harmHeight_pars = list(),
    subh_pars = list(),
    flux_pars = list(),
    fmRange = NULL,
    smooth,
    smoothing_ww,
    smoothingThres,
    varsToUnv = NULL
) {
  # remove all pitch-related columns except dom
  result = result[-which(grepl('pitch', colnames(result)))]
  result$pitch = pitch_true

  # Finalize voicing (some measures are only reported for voiced frames)
  result$voiced = !is.na(pitch_true)
  voiceless_frames = which(!result$voiced)
  result[voiceless_frames, varsToUnv] = NA

  # Calculate how far harmonics reach in the spectrum and how strong they are
  # relative to f0
  result[, c('harmEnergy', 'harmHeight', 'subRatio', 'subDep', 'CPP')] = NA
  voiced_frames = which(result$voiced)
  len_voiced = length(voiced_frames)
  if (len_voiced > 0) {
    if (is.null(freqs)) freqs = as.numeric(rownames(spectrogram)) * 1000
    if (is.null(bin)) bin = freqs[2] - freqs[1]

    # Calculate the % of energy in harmonics based on the final pitch estimates
    result$harmEnergy = to_dB(harmEnergy(
      pitch = result$pitch,
      s = spectrogram,
      freqs = freqs))
    # Calculate how high harmonics reach in the spectrum
    for (f in voiced_frames) {
      temp = try(do.call('harmHeight', c(
        harmHeight_pars,
        list(frame = spectrogram[, f],
             bin = bin,
             freqs = freqs,
             pitch = result$pitch[f]
        ))), silent = TRUE)
      if (!inherits(temp, 'try-error')) {
        result$harmHeight[f] = temp$harmHeight
        # result$harmSlope[f] = temp$harmSlope
        # not super meaningful - often too few harmonics, strong formants, etc
      }
    }

    # Calculate subharmonics-to-harmonics ratio
    for (f in voiced_frames) {
      temp = try(do.call('getSHR', c(
        subh_pars,
        list(frame = spectrogram[, f],
             bin = bin,
             freqs = freqs,
             samplingRate = samplingRate,
             pitch = result$pitch[f],
             pitchCands = data.frame(freq = pitchCands_list$freq[, f],
                                     cert = pitchCands_list$cert[, f])
        ))), silent = TRUE)
      if (!inherits(temp, 'try-error')) {
        result[f, c('subRatio', 'subDep')] = temp[c('subRatio', 'subDep')]
      }
    }
    # # result[, c('subRatio', 'subDep')]
    if (smooth > 0) {
      result$harmHeight = medianSmoother(
        result[, 'harmHeight', drop = FALSE],
        smoothing_ww = smoothing_ww,
        smoothingThres = smoothingThres)[, 1]
    }

    # Calculate Cepstral Peak Prominence for the final pitch contour
    for (f in voiced_frames) {
      result$CPP[f] = getCPP(
        frame = spectrogram[, f],
        samplingRate = samplingRate,
        pitch = pitch_true[f],
        bin = bin
      )
    }
  }

  # calculate flux from features
  if (!is.null(flux_pars$smoothWin)) {
    flux_pars$smoothing_ww = round(flux_pars$smoothWin / windowLength)
  } else {
    flux_pars$smoothing_ww = 1
  }
  flux_pars$smoothWin = NULL
  flux = do.call(getFeatureFlux, c(flux_pars, list(an = result)))
  result[, c('flux', 'epoch')] = flux[, c('flux', 'epoch')]

  # calculate FM
  result[, c('fmFreq', 'fmPurity', 'fmDep')] = NA
  if (length(voiced_frames) > 1) {
    step = result$time[2] - result$time[1]
    if (is.null(fmRange)) {
      # calculate reasonable defaults for FM frequency range
      fmRange = c(5, 1000 / step / 2)
    }
    nr = nrow(result)

    # interpolate NAs in pitch contour
    env = intplNA(pitch_true)
    # sp = spectrogram(env, samplingRate = 1000 / step, windowLength = 1000 / fmRange[1] * 4)

    # get peak frequency (in this case the most pronounced FM)
    fm = getPeakFreq(env,
                     samplingRate = 1000 / step,
                     freqRange = fmRange,
                     plot = FALSE)

    if (any(!is.na(fm$freq))) {
      # get FM from inflections to evaluate fmDep in semitones
      ps = .bandpass(list(sound = env, samplingRate = 1000/step),
                     lwr = min(fm$freq), upr = max(fm$freq),
                     action = 'pass', plot = FALSE)
      infl = findInflections(ps, thres = 0, plot = FALSE)
      # amFreq = 1000 / (step * diff(infl) * 2)
      # too noisy w/o bandpass, therefore need FFT first
      fmDep = abs(diff(HzToSemitones(ps[infl]))) / 2

      # fm should be the same length as pitch
      result$fmFreq = .resample(list(sound = fm$freq), len = nr,
                                lowPass = FALSE, plot = FALSE)
      result$fmPurity = .resample(list(sound = fm$purity), len = nr,
                                  lowPass = FALSE, plot = FALSE)
      result$fmDep = .resample(list(sound = fmDep), len = nr,
                               lowPass = FALSE, plot = FALSE)
      result[voiceless_frames, c('fmFreq', 'fmPurity', 'fmDep')] = NA
    }
  } else {

  }
  # plot(result$time, result$fmFreq, type = 'b', cex = result$fmPurity * 10)
  # plot(result$time, result$fmDep, type = 'b', cex = result$fmPurity * 10)

  # Arrange columns in alphabetical order (except the first three)
  result[, c(1:3, 3 + order(colnames(result)[4:ncol(result)]))]
}


#' Format pitchManual
#'
#' Internal soundgen function
#'
#' @param pitchManual dataframe produced by analyze() or pitch_app(), path to a
#'   .csv file in which this dataframe is stored, a named list with a numeric
#'   vector of pitch values per sound, or a numeric vector
#' @return A named list of pitch contours.
#' @keywords internal
#' @examples
#' soundgen:::formatPitchManual(c(NA, 120, 180, NA))
#' soundgen:::formatPitchManual('NA, 120, 180, NA')
#' soundgen:::formatPitchManual(list(
#'   'myfile.wav' = list(pitch = c(NA, 120, 180, NA))
#' ))
#' soundgen:::formatPitchManual(data.frame(file = c('file1.wav', 'file2.wav'),
#'                                         pitch = c('NA, 120', '180, NA')))
#' soundgen:::formatPitchManual('adja')
formatPitchManual = function(pitchManual) {
  pitchManual_list = NULL
  failed = FALSE
  if (is.character(pitchManual)) {
    if (file.exists(pitchManual)) {
      # path to csv
      pitchManual_df = try(read.csv(pitchManual)[, c('file', 'pitch')])
      if (inherits(pitchManual_df, 'type-error')) {
        # problem opening file
        failed = TRUE
      } else {
        # file OK
        pitchManual_list = vector('list', nrow(pitchManual_df))
        names(pitchManual_list) = pitchManual_df$file
        for (i in seq_len(nrow(pitchManual_df))) {
          pitchManual_list[[i]] = suppressWarnings(as.numeric(unlist(strsplit(
            as.character(pitchManual_df$pitch[[i]]), ','))))
        }
      }
    } else {
      # just a string - try to convert to numeric
      temp = try(suppressWarnings(as.numeric(unlist(strsplit(
        as.character(pitchManual), ',')))))
      if (inherits(temp, 'try-error') || !any(!is.na(temp))) {
        failed = TRUE
        pitchManual_list = NULL
      } else {
        pitchManual_list = list(sound = temp)
      }
    }
  } else if (is.list(pitchManual)) {
    # list or dataframe
    if (!is.null(pitchManual$file) && !is.null(pitchManual$pitch) &&
        is.character(pitchManual$pitch[1])) {
      # output of analyze() imported as dataframe
      pitchManual_df = as.data.frame(pitchManual)
      pitchManual_list = vector('list', nrow(pitchManual_df))
      names(pitchManual_list) = pitchManual_df$file
      for (i in seq_len(nrow(pitchManual_df))) {
        pitchManual_list[[i]] = suppressWarnings(as.numeric(unlist(strsplit(
          as.character(pitchManual_df$pitch[[i]]), ','))))
      }
    } else {
      # preformatted list - return as is
      pitchManual_list = lapply(pitchManual, function(x) x$pitch)
    }
  } else if (is.numeric(pitchManual)) {
    # numeric vector (pitch contour of a single sound)
    pitchManual_list = list(sound = pitchManual)
  } else {
    failed = TRUE
  }

  if (failed) {
    warning(paste(
      "pitchManual not recognized; should be a numeric vector, named list,",
      "csv file or dataframe containing the output of analyze() with columns",
      "'file' and 'pitch', or a named list with pitch contours per file"))
  }

  pitchManual_list
}
