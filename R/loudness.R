#' Get loudness
#'
#' Estimates subjective loudness and sharpness of audio. Based on EMBSD speech
#' quality measure, particularly the MATLAB code in Yang (1999) and Timoney et
#' al. (2004). Note that there are many ways to estimate loudness and many other
#' factors, ignored by this model, that could influence subjectively experienced
#' loudness. Please treat the output with a healthy dose of skepticism! Also
#' note that the absolute value of calculated loudness critically depends on the
#' chosen "measured" sound pressure level (SPL). \code{getLoudness} estimates
#' how loud a sound will be experienced if it is played back at an SPL of
#' SPL_measured dB. The most meaningful way to use the output is to compare the
#' loudness of several sounds analyzed with identical settings or of different
#' segments within the same recording.
#'
#' Algorithm: calibrates the sound to the desired SPL (Timoney et al., 2004),
#' extracts a spectrogram with frequencies on the bark scale, optionally spreads
#' the spectrum to account for frequency masking across the critical bands
#' (Yang, 1999), converts dB to phon by using standard equal loudness curves
#' (ISO 226), converts phon to sone (Timoney et al., 2004), sums across all
#' critical bands, and applies a correction coefficient to standardize output.
#' Calibrated so as to return a loudness of 1 sone for a 1 kHz pure tone with
#' SPL of 40 dB and \code{spreadSpectrum = FALSE}. Sharpness is calculated as
#' the weighted first moment of specific loudness on the Bark scale.
#'
#' @seealso \code{\link{getRMS}} \code{\link{analyze}}
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams analyze
#' @param SPL_measured sound pressure level at which the sound is presented
#'   relative to some reference (the conventional threshold is 2e-5 Pa), dB
#' @param input "spec" = power spectrogram warped to bark scale with
#'   \code{\link[tuneR]{audspec}}, "audSpec" = auditory spectrogram produced by
#'   convolving the signal with a bank of gammatone filters using
#'   \code{\link{audSpectrogram}} (much slower, but more physiologically
#'   accurate)
#' @param spreadSpectrum if TRUE, applies a spreading function to account for
#'   frequency masking
#' @param sharpnessMethod the method of calculating sharpness (mostly differ in
#'   weighting functions; only "aures" depends on SPL)
#' @param mar margins of the spectrogram
#' @param main plot title
#' @param ... other plotting parameters passed to \code{\link{spectrogram}}
#' @return A list with two top-level elements: \code{$detailed} and
#'   \code{$summary}.
#'
#'   \code{$detailed} contains per-file results. If multiple sounds are
#'   analyzed, \code{$detailed} is a list of per-sound lists. If a single
#'   sound is analyzed, it is simplified to a single list. Each list contains:
#'   \describe{
#'     \item{loudness}{a vector of loudness in sone units per STFT frame}
#'     \item{specSone}{spectrum in bark-sone: a matrix of loudness values in
#'       sone, with frequency on the bark scale in rows and time (STFT frames)
#'       in columns}
#'     \item{loudnessPhon, specPhon}{same in phon instead of sone units}
#'     \item{sharpness}{a vector of sharpness in acum units per STFT frame}
#'     \item{audSpec}{auditory spectrogram used to calculate loudness and
#'       sharpness}
#'   }
#'
#'   \code{$summary} is a dataframe of summary loudness measures (one row per
#'   file). If \code{summaryFun} is \code{NULL}, \code{$summary} is \code{NULL}.
#' @references \itemize{
#'   \item ISO 226 as implemented by Jeff Tackett (2005) on
#'   https://www.mathworks.com/matlabcentral/fileexchange/
#'   7028-iso-226-equal-loudness-level-contour-signal
#'   \item Timoney, J., Lysaght, T., Schoenwiesner, M., & MacManus, L. (2004).
#'   Implementing loudness models in matlab.
#'   \item Yang, W. (1999). Enhanced Modified Bark Spectral Distortion (EMBSD):
#'   An Objective Speech Quality Measure Based on Audible Distortion and
#'   Cognitive Model. Temple University. }
#' @export
#' @examples
#' sounds = list(
#'   noise_1KHz = soundgen:::zeroOne(bandpass(rnorm(8000), 16000,
#'     lwr = 900, upr = 1100)) * 2 -1, # narrow-band noise at 1 KHz
#'   white_noise = runif(8000, -1, 1),  # white noise
#'   white_noise2 = runif(8000, -1/2, 1/2),  # ~6 dB quieter
#'   pure_tone_1KHz = sin(2*pi*1000/16000*(1:8000)),  # pure tone at 1 kHz
#'   pure_tone_100Hz = sin(2*pi*100/16000*(1:8000))  # pure tone at 100 Hz
#' )
#' # playme(sounds)
#' l = getLoudness(
#'     x = sounds, samplingRate = 16000, scale = 1,
#'     windowLength = 40, step = NULL, input = c('spec', 'audSpec')[1],
#'     overlap = 50, SPL_measured = 60,
#'     plot = FALSE)
#' l$summary
#' # loudness depends on amplitude if "scale" is provided (cf. sounds 2 and 3)
#' # narrowband noise / tone at 1 kHz, 60 dB: sharpness ~=1 acum, loudness ~=4 sone
#'
#' # a steady glissando from 125 to 8000 Hz (constant on a musical scale)
#' pitch = exp(seq(log(125), log(8000), length.out = 16000))
#' s = sinpi(2 * cumsum(pitch) / 16000)
#' l1 = getLoudness(s, samplingRate = 16000, SPL_measured = 70)
#' # steady SPL, but variable loudness
#'
#' # The estimated loudness and sharpness depend on target SPL
#' l2 = getLoudness(s, samplingRate = 16000, SPL_measured = 40, plot = FALSE)
#' l1$summary$loudness_mean
#' l2$summary$loudness_mean
#'
#' # ...but not (much) on windowLength and samplingRate
#' l3 = getLoudness(s, samplingRate = 16000, SPL_measured = 40,
#'   windowLength = 50, plot = FALSE)
#' l3$summary$loudness_mean
#'
#' \dontrun{
#' # Using auditory spectrogram as input instead of STFT (slower)
#' l4 = getLoudness(s, samplingRate = 16000, SPL_measured = 40, input = 'audSpec')
#' l4$summary$loudness_mean
#'
#' # Process all audio files in a folder
#' l5 = getLoudness('~/Downloads/temp', savePlots = TRUE)
#' l5$summary
#' }
getLoudness = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    input = c('spec', 'audSpec'),
    windowLength = 50,
    step = NULL,
    overlap = 50,
    SPL_measured = 70,
    spreadSpectrum = FALSE,
    sharpnessMethod = c('aures', 'DIN45692', 'bismarck'),
    summaryFun = c('mean', 'median', 'sd'),
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    main = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    mar = c(5.1, 4.1, 4.1, 4.1),
    ...) {
  input = match.arg(input)
  sharpnessMethod = match.arg(sharpnessMethod)

  ## Prepare a list of arguments to pass to .getLoudness()
  myPars = c(as.list(environment()), list(...))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'savePlots', 'embed', 'reportEvery', 'cores', 'summaryFun')]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.getLoudness',
    suffix = 'getLoudness',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  if (pa$input$n == 0) stop('Failed to analyze any input')

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
      if (!pa$input$failed[i]) {
        res = pa$result[[i]]
        temp[[i]] = summarizeAnalyze(
          data.frame(loudness = res$loudness,
                     loudnessPhon = res$loudnessPhon,
                     sharpness = res$sharpness),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]][1, ]
        filler[1, ] = NA
      } else {
        # All files failed: generate a safe NA filler
        filler = try(summarizeAnalyze(
          data.frame(loudness = NA, loudnessPhon = NA, sharpness = NA),
          summaryFun = summaryFun,
          var_noSummary = NULL))
        if (inherits(filler, 'try-error')) {
          filler = data.frame(matrix(NA, nrow = 1, ncol = 3 * length(summaryFun)))
        } else {
          filler[1, ] = NA
        }
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = try(cbind(data.frame(file = pa$input$filenames_base),
                          rbind_fill_list(temp)))
    if (inherits(mysum_all, 'try-error')) mysum_all = NULL
  } else {
    mysum_all = NULL
  }

  # prepare detailed output
  detailed = vector('list', pa$input$n)
  names(detailed) = pa$input$filenames_base
  for (i in seq_len(pa$input$n)) {
    if (pa$input$failed[i]) {
      detailed[[i]] = list(loudness = NA, specSone = NA, loudnessPhon = NA,
                           specPhon = NA, sharpness = NA, audSpec = NA)
    } else {
      detailed[[i]] = pa$result[[i]]
    }
  }
  if (pa$input$n == 1) detailed = detailed[[1]]

  invisible(list(
    detailed = detailed,
    summary = mysum_all
  ))
}


#' Loudness per sound
#' @noRd
.getLoudness = function(
    audio,
    input = 'spec',
    windowLength = 50,
    step = NULL,
    overlap = 50,
    SPL_measured = 70,
    spreadSpectrum = FALSE,
    sharpnessMethod = 'aures',
    plot = FALSE,
    savePlots = NULL,
    main = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    mar = c(5.1, 4.1, 4.1, 4.1),
    ...) {
  val = validateWlOvlp(audio, windowLength, step, overlap)
  step = val$step
  windowLength = val$windowLength
  if (audio$samplingRate < 2000) {
    warning(paste('samplingRate of', audio$samplingRate, 'is too low;',
                  'need a Nyquist of at least 8 barks (1 kHz)'))
    # Return complete list of NAs to prevent downstream errors
    return(list(specSone = NA, loudness = NA, loudnessPhon = NA,
                specPhon = NA, sharpness = NA, audSpec = NA))
  }

  # scale to dB SPL
  sound_scaled = scaleSPL(audio$sound,
                          scale = audio$scale,
                          SPL_measured = SPL_measured)

  # get auditory spectrum with 1 filter/bark
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }

  if (input == 'audSpec') {
    maxFreq_bark = floor(HzToOther(audio$samplingRate / 2, 'bark'))
    audio1 = audio
    audio1$sound = sound_scaled

    audSpec_res = try(.audSpectrogram(
      audio1,
      nFilters = maxFreq_bark * 6,
      step = step,
      bandwidth = NULL,
      yScale = 'bark',
      filterType = 'gammatone',
      envelope = 'rms',
      minFreq = otherToHz(1/2, 'bark'),
      maxFreq = otherToHz(maxFreq_bark, 'bark'),
      plot = FALSE, plotFilters = FALSE,
      output = c('audSpec', 'filters')), silent = TRUE)

    if (inherits(audSpec_res, 'try-error'))
      return(list(specSone = NA, loudness = NA, loudnessPhon = NA,
                  specPhon = NA, sharpness = NA, audSpec = NA))

    audSpec = audSpec_res$audSpec^2
    filters_aud = audSpec_res$filters

    # Normalize filterbank energy to match time-domain RMS
    time_msq = mean(sound_scaled^2)
    spec_sum_mean = mean(colSums(audSpec))
    if (spec_sum_mean > 0) audSpec = audSpec * (time_msq / spec_sum_mean)

    audSpec[, c(1, ncol(audSpec))] = 0  # artifacts in first/last frame

    # Use filter table for exact center frequencies and bandwidths
    if (!is.null(filters_aud) && nrow(filters_aud) == nrow(audSpec)) {
      freqs_kHz = filters_aud$cf / 1000
      freqs_bark = HzToOther(filters_aud$cf, 'bark')

      # filter bandwidth in Bark, based on the actual lower/upper bounds
      filterWidth_bark = HzToOther(filters_aud$to, 'bark') -
        HzToOther(filters_aud$from, 'bark')

      # safety check
      filterWidth_bark[!is.finite(filterWidth_bark) | filterWidth_bark <= 0] = 1
    } else {
      # fallback: should not normally be needed
      freqs_kHz = as.numeric(rownames(audSpec))
      freqs_bark = HzToOther(freqs_kHz * 1000, 'bark')
      filterWidth_bark = rep(1, length(freqs_bark))
    }
  } else if (input == 'spec') {
    powerSpec = tuneR::powspec(
      sound_scaled, sr = audio$samplingRate,
      wintime = windowLength / 1000, steptime = step / 1000,
      dither = FALSE)

    audSpec = try(tuneR::audspec(
      powerSpec,
      sr = audio$samplingRate,
      fbtype = 'bark')$aspectrum, silent = TRUE)
    if (inherits(audSpec, 'try-error'))
      return(list(specSone = NA, loudness = NA, loudnessPhon = NA,
                  specPhon = NA, sharpness = NA, audSpec = NA))

    freqs_bark = seq_len(nrow(audSpec))
    freqs_kHz = otherToHz(freqs_bark, 'bark') / 1000

    # for the STFT pipeline, each row is treated as one Bark band
    filterWidth_bark = rep(1, length(freqs_bark))

    # Normalize audSpec so its sum matches time-domain energy.
    # This ensures dB SPL calculation is correct and removes windowLength dependence.
    time_msq = mean(sound_scaled^2)
    aud_sum = mean(colSums(audSpec))
    if (is.finite(aud_sum) && aud_sum > 0) {
      audSpec = audSpec * (time_msq / aud_sum)
    }
  }

  # throw away very high frequencies
  if (audio$samplingRate > 44100 && max(freqs_bark) > 27) {
    message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                  '(inaudible to humans)'))
    keep_idx = which(freqs_bark <= 27)
    audSpec = audSpec[keep_idx, , drop = FALSE]
    freqs_bark = freqs_bark[keep_idx]
    freqs_kHz = freqs_kHz[keep_idx]
    filterWidth_bark = filterWidth_bark[keep_idx]
  }

  # apply spreading function (Vectorized matrix multiplication)
  if (spreadSpectrum) {
    n_bands = nrow(audSpec)
    # Ensure spreadSpecCoef is available and correctly sized
    if (exists("spreadSpecCoef") && is.matrix(spreadSpecCoef)) {
      dim_c = min(n_bands, nrow(spreadSpecCoef), ncol(spreadSpecCoef))
      coef_mat = matrix(0, nrow = n_bands, ncol = n_bands)
      coef_mat[1:dim_c, 1:dim_c] = spreadSpecCoef[1:dim_c, 1:dim_c, drop = FALSE]
      audSpec = coef_mat %*% audSpec
    } else {
      # Fallback to loop if matrix not available
      nonZeroCols = which(colSums(audSpec) > 0)
      for (c in nonZeroCols) {
        audSpec[, c] = spreadSpec(audSpec[, c])
      }
    }
  }

  # Precompute matrices for phon curves for vectorized lookup
  nr_bark = nrow(audSpec)
  n_curves = length(phonCurves)
  phonCurves_freqs = as.numeric(names(phonCurves))

  # Handle resampling of phonCurves if audSpec has different number of bands
  # NB: interpolate on the bark scale, not Hz
  orig_bark = phonCurves[[1]]$freq_bark
  all_spl = matrix(0, nrow = nr_bark, ncol = n_curves)
  all_thres = matrix(0, nrow = nr_bark, ncol = n_curves)
  if (nr_bark != length(orig_bark)) {
    for (p in seq_len(n_curves)) {
      all_spl[, p] = approx(
        x = orig_bark,
        y = phonCurves[[p]]$spl,
        xout = freqs_bark,
        rule = 2
      )$y

      all_thres[, p] = approx(
        x = orig_bark,
        y = phonCurves[[p]]$hearingThres_dB,
        xout = freqs_bark,
        rule = 2
      )$y
    }
  } else {
    for (p in seq_len(n_curves)) {
      all_spl[, p] = phonCurves[[p]]$spl
      all_thres[, p] = phonCurves[[p]]$hearingThres_dB
    }
  }

  # convert spectrum to phon and sone (Fully Vectorized)
  y = 10 * log10(audSpec) # matrix of dB SPL

  n_curve_at_1kHz = which.min(abs(freqs_kHz - 1))
  spl_at_1kHz = y[n_curve_at_1kHz, ] # vector of SPL at 1kHz for each frame

  # Find closest phon curve for each frame
  phon_indices = findInterval(spl_at_1kHz, phonCurves_freqs)
  phon_indices[phon_indices < 1] = 1
  phon_indices[phon_indices > n_curves] = n_curves

  # Vectorized lookup of SPL corrections
  spl_correction = all_spl[, phon_indices, drop = FALSE]
  spl_at_1kHz_correction = all_spl[n_curve_at_1kHz, phon_indices]

  # Calculate y_phon (using sweep to correctly add the vector to columns)
  y_phon = sweep(y, 2, spl_at_1kHz_correction, "+") - spl_correction

  # Thresholding: y is the spectrum in dB SPL, y_phon is the spectrum in phon
  # thres_correction is hearing threshold in dB SPL
  thres_correction = all_thres[, phon_indices, drop = FALSE]
  mask = y < thres_correction | y_phon <= 0
  y_phon[mask] = 0

  specPhon = y_phon
  specSone = phonToSone(y_phon) # phonToSone handles matrices natively

  # empirical calibration (see commented-out code below the function)
  if (input == 'audSpec') {
    offset_band = 0.638 * filterWidth_bark / sum(filterWidth_bark)
    specSone = specSone * 0.142 + offset_band
  } else if (input == 'spec') {
    offset_band = 0.273 * filterWidth_bark / sum(filterWidth_bark)
    specSone = specSone * 0.366 + offset_band
  }
  specSone[mask] = 0  # reset silent frames to 0 (changes a bit after the calibration)

  # Integration: Loudness is the sum of specific loudness across critical bands
  loudness = colSums(specSone)
  loudnessPhon = soneToPhon(loudness)

  # calculate sharpness
  # For input = 'audSpec', specSone is currently best interpreted as loudness
  # per filter band. getSharpness() expects specific loudness in sone/Bark,
  # so we divide by the filter bandwidth in Bark. getSharpness() then multiplies
  # by barkWidth again during integration, so the denominator remains equivalent
  # to colSums(specSone).
  if (input == 'audSpec') {
    specSone_sharpness = sweep(specSone, 1, filterWidth_bark, FUN = '/')
    barkWidth_sharpness = filterWidth_bark
  } else {
    specSone_sharpness = specSone
    barkWidth_sharpness = filterWidth_bark
  }
  loudness_sharpness = colSums(specSone_sharpness * barkWidth_sharpness)

  sharpness = getSharpness(
    specSone_sharpness,
    method = sharpnessMethod,
    loudness = loudness_sharpness,
    bark = freqs_bark,
    barkWidth = barkWidth_sharpness
  )

  # plotting
  if (plot) {
    if (is.null(ylim)) ylim = c(0, audio$samplingRate / 2 / 1000)
    if (is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }
    max_l = max(loudness, na.rm = TRUE)
    if (is.na(max_l) || max_l == 0) {
      loudness_norm = rep(0, length(loudness))
    } else {
      loudness_norm = loudness / max_l * ylim[2] * 1000
    }
    .spectrogram(
      audio[which(names(audio) != 'savePlots')],
      windowLength = windowLength, step = step,
      output = 'original',
      padWithSilence = FALSE,
      plot = TRUE, mar = mar, ylim = ylim,
      extraContour = list(x = loudness_norm, col = 'blue'),
      ...)
  }

  list(loudness = loudness, specSone = specSone,
       loudnessPhon = loudnessPhon, specPhon = specPhon,
       sharpness = sharpness, audSpec = audSpec)
}


# ## EMPIRICAL CALIBRATION OF LOUDNESS (SONE) RETURNED BY getLoudness()
# wl = expand.grid(windowLength = seq(10, 150, by = 10),
#                  samplingRate = c(16000, 24000, 32000, 44000))
# for (i in 1:nrow(wl)) {
#   sound = sin(2*pi*1000/wl$samplingRate[i]*(1:20000))
#   # sound = rnorm(20000)
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = wl$samplingRate[i], input = c('spec', 'audSpec')[1], windowLength = wl$windowLength[i], step = NULL, overlap = 0, SPL_measured = 40, plot = FALSE)$loudnessPhon[1]
# }
# # plot(wl$windowLength, wl$loudness)
# library(ggplot2)
# ggplot(wl, aes(x = windowLength, y = loudness, color = as.factor(samplingRate))) +
#   geom_point() +
#   geom_line()

# # CHECKING THE CALIBRATION
# samplingRate = 16000
# sound = sin(2*pi*1000/samplingRate*(1:20000))
# # sound = rnorm(20000)
# cal = data.frame(SPL_measured = seq(40, 90, by = 10))
# cal$loudness_expected = c(1, 2, 4, 8, 16, 32)
# for (i in 1:nrow(cal)) {
#   cal$loudness[i] = mean(getLoudness(x = sound, samplingRate = samplingRate, input = c('spec', 'audSpec')[1], windowLength = 25, step = 15, SPL_measured = cal$SPL_measured[i], plot = FALSE)$loudness)
# }
# cal  # loudness should be 1, 2, 4, 8, 16 sone
# cal$loudness / cal$loudness[1]
# plot(cal$loudness_expected, cal$loudness, type = 'b')
# plot(cal$loudness_expected, cal$loudness, type = 'b', log = 'xy')
#
# ## calibration
# mod = nls(loudness_expected ~ a + b * loudness ^ c, cal, start = list(a = 0, b = 1, c = 1))
# plot(cal$loudness, cal$loudness_expected)
# lines(cal$loudness, predict(mod, list(loudness = cal$loudness)))
# summary(mod)
# mod2 = lm(loudness_expected ~ loudness, cal)
# plot(cal$loudness, cal$loudness_expected)
# lines(cal$loudness, predict(mod2, list(loudness = cal$loudness)))
# summary(mod2)

### calibrating sharpness
# sounds = list(
#   noise_1KHz = bandpass(rnorm(8000), 16000, lwr = 900, upr = 1100),  # narrow-band noise at 1 KHz
#   white_noise = rnorm(8000, -1, 1),  # white noise
#   white_noise2 = rnorm(8000, -1, 1) / 2,  # ~6 dB quieter
#   pure_tone_1KHz = sin(2*pi*1000/16000*(1:8000)),  # pure tone at 1 kHz
#   pure_tone_100Hz = sin(2*pi*100/16000*(1:8000))  # pure tone at 100 Hz
# )
# playme(sounds)
# l = getLoudness(
#     x = sounds, samplingRate = 16000, scale = 1,
#     windowLength = 40, step = NULL, input = c('spec', 'audSpec')[2],
#     overlap = 50, SPL_measured = 60,
#     plot = FALSE)
# l$summary
