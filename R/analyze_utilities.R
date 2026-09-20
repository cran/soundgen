### UTILITIES FOR ACOUSTIC ANALYSIS ###

#' Analyze fft frame
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
#' @return A list with two components: $pitchCands_frame contains pitch
#'   candidates for the frame, and $summaries contains other acoustic predictors
#'   like HNR, specSlope, etc.
#' @noRd
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
  frameSpec = data.frame(freq = freqs,
                         amp = frame,
                         energy = frame^2)
  # Cut spectral band from pitchFloor to cutFreq Hz (used for spectral
  # descriptives only - pitch tracking is always done with the full spectrum)
  if (is.null(cutFreq)) {
    frameSpec_cut = frameSpec
    idx_keep = 1:length(frame)
  } else {
    idx_keep = which(freqs >= cutFreq[1] & freqs <= cutFreq[2])
    frameSpec_cut = frameSpec[idx_keep, ]
    # Above 5-6 kHz or so, spectral energy depends too much on the original
    # sampling rate, noises etc. Besides, those frequencies are not super
    # relevant to human vocalizations in any case. So we cut away all info above
    # 5 kHz before we calculate quartiles of spectral energy
  }

  ## DESCRIPTIVES
  total_energy = sum(frameSpec_cut$energy)
  if (total_energy == 0) {  # empty frame
    return(list(
      'pitchCands_frame' = data.frame(
        'pitchCand' = numeric(), 'pitchCert' = numeric(),
        'pitchSource' = character(), stringsAsFactors = FALSE, row.names = NULL
      ),
      'summaries' = data.frame(
        HNR = NA, dom = NA, specCentroid = NA, peakFreq = NA,
        quartile25 = NA, quartile50 = NA, quartile75 = NA,
        specSlope = NA, entropyW = NA, entropySh = NA
      )
    ))
  }
  frameSpec_cut$w = frameSpec_cut$energy / total_energy
  specCentroid = sum(frameSpec_cut$freq * frameSpec_cut$w)
  peakFreq = frameSpec_cut$freq[which.max(frameSpec_cut$energy)]

  # quartiles of spectral energy distribution
  cums = cumsum(frameSpec_cut$energy)
  quartile25 = frameSpec_cut$freq[min(which(cums >= 0.25 * total_energy))]
  quartile50 = frameSpec_cut$freq[min(which(cums >= 0.5 * total_energy))]
  quartile75 = frameSpec_cut$freq[min(which(cums >= 0.75 * total_energy))]

  # spectral slope in dB/kHz
  frameSpec_cut$intens_dB = 20 * log10(frameSpec_cut$amp)
  frameSpec_cut$intens_dB = frameSpec_cut$intens_dB - max(frameSpec_cut$intens_dB)
  frameSpec_cut$intens_dB[!is.finite(frameSpec_cut$intens_dB)] = NA
  specSlope = cov(frameSpec_cut$freq, frameSpec_cut$intens_dB, use = 'complete.obs') /
    var(frameSpec_cut$freq) * 1000  # same as slope from lm(), but faster

  # spectral entropy
  entropySh = getEntropy(frameSpec$energy, type = 'shannon', normalize = TRUE)
  entropyW = getEntropy(frameSpec$energy, type = 'wiener')

  ## PITCH TRACKING
  frame = frame / max(frame)

  # lowest dominant frequency band
  if (trackPitch && 'dom' %in% pitchMethods) {
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
  if (trackPitch && 'autocor' %in% pitchMethods) {
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
  if (trackPitch && 'cep' %in% pitchMethods) {
    pitchCep_array = do.call(getPitchCep,
                             c(pitchCep,
                               list(frame = frame[idx_keep],
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
  if (trackPitch && 'spec' %in% pitchMethods) {
    pitchSpec_array = do.call(getPitchSpec,
                              c(pitchSpec,
                                list(frame = frame[idx_keep],
                                     bin = bin,
                                     freqs = freqs[idx_keep],
                                     HNR = HNR,
                                     nCands = nCands,
                                     pitchFloor = pitchFloor,
                                     pitchCeiling = pitchCeiling)))
    if(!is.null(pitchSpec_array))
      pitchCands_frame = rbind(pitchCands_frame, pitchSpec_array)
  }

  # harmonic product spectrum (hps)
  if (trackPitch && 'hps' %in% pitchMethods) {
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
    pitchCands_frame$pitchCand = as.numeric(pitchCands_frame$pitchCand)
    pitchCands_frame$pitchCert = as.numeric(pitchCands_frame$pitchCert)
    # otherwise they become characters after rbind
  }
  if (nrow(pitchCands_frame[pitchCands_frame$pitchSource == 'dom', ]) > 0 &&
      !is.na(HNR)) {
    pitchCands_frame$pitchCert[pitchCands_frame$pitchSource == 'dom'] =
      1 / (1 + exp(3 * HNR - 1)) # dom is worth more for noisy sounds,
    # but its weight approaches ~0.12 as HNR approaches 1
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
      specSlope = specSlope,
      entropyW = entropyW,
      entropySh = entropySh
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
#' (Hz) and sd = priorSD (semitones) normalized so max = 1 over \code{[pitchFloor,
#' pitchCeiling]}. Useful for previewing the prior given to
#' \code{\link{analyze}}.
#'
#' @seealso \code{\link{analyze}} \code{\link{pitch_app}}
#'
#' @return a data frame with columns `freq` and `prob` if pitchCands is NULL and
#'   a numeric matrix of the same dimensions as pitchCands otherwise.
#' @inheritParams analyze
#' @param len the required length of output vector (resolution)
#' @param distribution the shape of prior distribution on the musical scale:
#'   'normal' (mode = priorMean) or 'gamma' (skewed to lower frequencies)
#' @param plot if TRUE, plots the prior
#' @param ... additional graphical parameters passed on to plot()
#' @param pitchCands a matrix of pitch candidate frequencies (for internal
#'   soundgen use)
#' @noRd
#' @examples
#' soundgen:::getPrior(priorMean = 150,  # Hz
#'                     priorSD = 2)      # semitones
#' soundgen:::getPrior(150, 6)
#' soundgen:::getPrior(100, 24, 'gamma')
#' s = soundgen:::getPrior(450, 12, pitchCeiling = 2000)
#' plot(s, type = 'l')
getPrior = function(priorMean,
                    priorSD,
                    distribution = c('normal', 'gamma'),
                    pitchFloor = 75,
                    pitchCeiling = 3000,
                    len = 100,
                    plot = TRUE,
                    pitchCands = NULL,
                    ...) {
  distribution = match.arg(distribution)
  freqs = seq(HzToSemitones(pitchFloor),
              HzToSemitones(pitchCeiling),
              length.out = len)
  if (!is.null(priorMean) && is.finite(priorMean) &&
      !is.null(priorSD) && is.finite(priorSD)) {
    priorMean_semitones = HzToSemitones(priorMean)
    if (distribution == 'normal') {
      prior_normalizer = dnorm(freqs, priorMean_semitones, priorSD)
      prior_norm_max = dnorm(priorMean_semitones, priorMean_semitones, priorSD)
      # peak; or just 1/(priorSD * sqrt(2π))
    } else if (distribution == 'gamma') {
      shape = priorMean_semitones ^ 2 / priorSD ^ 2
      rate = priorMean_semitones / priorSD ^ 2
      prior_normalizer = dgamma(
        freqs,
        shape = shape,
        rate = rate
      )
      prior_norm_max = max(prior_normalizer)
    }
    if (!is.finite(prior_norm_max) || prior_norm_max <= 0) {
      # fall back on flat prior
      prior = rep(1, len)
      prior_norm_max = 1
    } else {
      prior = prior_normalizer / prior_norm_max
    }
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
    if (is.numeric(priorMean) && is.finite(priorMean) &&
        is.numeric(priorSD) && is.finite(priorSD)) {
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
#' @param result dataframe returned by analyze(summary = FALSE)
#' @param summaryFun summary functions
#' @param var_noSummary variables that should not be summarized
#' @return A dataframe of descriptives.
#' @noRd
summarizeAnalyze = function(
    result,
    summaryFun = c('mean', 'sd'),
    var_noSummary = c('duration', 'duration_noSilence', 'voiced', 'time', 'epoch')
) {
  if (is.null(summaryFun) || length(summaryFun) < 1) return (NA)
  if (nrow(result) == 0) return(NA)
  if (is.character(var_noSummary)) {
    vars = colnames(result)[!colnames(result) %in% var_noSummary]
  } else {
    vars = colnames(result)
  }
  ls = length(summaryFun)
  lv = length(vars)
  # vars_f = paste0(rep(vars, each = ls), '_', rep(summaryFun, each = lv))

  # pre-parse summary function names to speed things up
  functions = vector('list', length(summaryFun))
  for (f in seq_along(summaryFun)) {
    functions[[f]] = match.fun(summaryFun[f])
  }

  # apply the specified summary function to each column of result
  out = list()
  for (v in vars) {
    for (s in 1:ls) {
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
#' Updates the output of analyze using manual pitch. Called by pitch_app().
#' @param result the matrix of results returned by analyze()
#' @param pitch_true manual pitch contour of length nrow(result), with NAs
#' @param spectrogram spectrogram with ncol = nrow(result)
#' @param freqs frequency labels of spectrogram bins
#' @param bin spectrogram bin width
#' @param harmHeight_pars same as argument "harmHeight" to analyze() - a list of
#'   settings passed to soundgen:::harmHeight()
#' @param varsToUnv set these variables to NA in voiceless frames
#' @return An updated version of the input ("result").
#' @noRd
updateAnalyze = function(
    result,
    pitch_true,
    pitchCands_list = NULL,
    spectrogram,
    freqs = NULL,
    bin = NULL,
    samplingRate = NULL,
    harmHeight_pars = list(),
    subh_pars = list(),
    flux_pars = list(),
    fmRange = NULL,
    varsToUnv = NULL
) {
  # remove all pitch-related columns except dom
  result = result[-which(grepl('pitch', colnames(result)))]
  result$pitch = pitch_true
  step = diff(as.numeric(colnames(spectrogram)[1:2]))
  if (length(step) == 0 || !is.finite(step)) {
    step = result$time[2] - result$time[1]  # fallback
  }

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
    result$harmEnergy = zeroOne_to_dB(harmEnergy(
      pitch = result$pitch,
      s = spectrogram,
      freqs = freqs))
    # Calculate how high harmonics reach in the spectrum
    for (f in voiced_frames) {
      frame_f = spectrogram[, f]

      result$harmHeight[f] = do.call('harmHeight', c(
        harmHeight_pars,
        list(frame = frame_f,
             bin = bin,
             freqs = freqs,
             pitch = result$pitch[f]
        )))$harmHeight
      # result$harmSlope[f] = temp$harmSlope
      # not super meaningful - often too few harmonics, strong formants, etc

      # Calculate subharmonics-to-harmonics ratio
      if (!is.null(pitchCands_list)) {
        result[f, c('subRatio', 'subDep')] = do.call('getSHR', c(
          subh_pars,
          list(frame = frame_f,
               bin = bin,
               freqs = freqs,
               samplingRate = samplingRate,
               pitch = result$pitch[f],
               pitchCands = data.frame(freq = pitchCands_list$freq[, f],
                                       cert = pitchCands_list$cert[, f])
          ))) [c('subRatio', 'subDep')]
      }

      # Calculate Cepstral Peak Prominence for the final pitch contour
      result$CPP[f] = getCPP(
        frame = frame_f,
        samplingRate = samplingRate,
        bin = bin,
        pitch = pitch_true[f],
      )
    }
  }

  # calculate flux from features
  if (!is.null(flux_pars$smoothWin)) {
    flux_pars$smoothing_ww = round(flux_pars$smoothWin / step)
  } else {
    flux_pars$smoothing_ww = 1
  }
  flux_pars$smoothWin = NULL
  flux = do.call(getFeatureFlux, c(flux_pars, list(an = result)))
  result[, c('flux', 'epoch')] = flux[, c('flux', 'epoch')]

  # calculate FM
  result[, c('fmFreq', 'fmPurity', 'fmDep')] = NA
  if (length(voiced_frames) > 1) {
    if (is.null(fmRange)) {
      # calculate reasonable defaults for FM frequency range
      fmRange = c(5, 1000 / step / 2)
    }
    nr = nrow(result)

    # interpolate NAs in pitch contour
    env = interpolateNA(pitch_true)

    # get peak frequency (in this case the most pronounced FM)
    fm = getPeakFreq(env,
                     samplingRate = 1000 / step,
                     freqRange = fmRange,
                     plot = FALSE)

    if (any(!is.na(fm$freq))) {
      # get FM from inflections to evaluate fmDep in semitones
      mean_freq = mean(fm$freq, na.rm = TRUE)
      # filter from min to max fm$freq or within ±2 semitones of its mean
      lwr = min(mean_freq / 2^(2/12), min(fm$freq, na.rm = TRUE))
      upr = max(mean_freq * 2^(2/12), max(fm$freq, na.rm = TRUE))
      # (if fmFreq ~= const, we end up with a very narrow bandwidth)
      ps = .bandpass(list(sound = env, samplingRate = 1000/step),
                     lwr = lwr, upr = upr, bw = mean_freq / 10,
                     action = 'pass', plot = FALSE)
      infl = findInflections(ps, thres = 0, plot = FALSE)
      # too noisy w/o bandpass, therefore need FFT first
      if (length(infl) >= 2) {
        fmDep = abs(diff(HzToSemitones(ps[infl]))) / 2
      } else {
        fmDep = numeric(0)
      }

      # fm should be the same length as pitch
      result$fmFreq = .resample(list(sound = fm$freq), len = nr,
                                lowPass = FALSE, plot = FALSE)
      result$fmPurity = .resample(list(sound = fm$purity), len = nr,
                                  lowPass = FALSE, plot = FALSE)
      result$fmDep = if (length(fmDep) > 0)
        .resample(list(sound = fmDep), len = nr, lowPass = FALSE, plot = FALSE)
      else rep(NA, nr)
      result[voiceless_frames, c('fmFreq', 'fmPurity', 'fmDep')] = NA
    }
  }

  # Arrange columns in alphabetical order (except the first three)
  result[, c(1:3, 3 + order(colnames(result)[4:ncol(result)]))]
}


#' Format pitchManual
#' @param pitchManual dataframe produced by analyze() or pitch_app(), path to a
#'   .csv file in which this dataframe is stored, a named list with a numeric
#'   vector of pitch values per sound, or a numeric vector
#' @return A named list of pitch contours.
#' @noRd
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
      if (inherits(pitchManual_df, 'try-error')) {
        # problem opening file
        failed = TRUE
      } else {
        # file OK
        pitchManual_list = vector('list', nrow(pitchManual_df))
        names(pitchManual_list) = pitchManual_df$file
        for (i in seq_len(nrow(pitchManual_df))) {
          p = pitchManual_df$pitch[[i]]
          if (is.character(p)) p = unlist(strsplit(p, ','))
          pitchManual_list[[i]] = suppressWarnings(as.numeric(unlist(p)))
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
