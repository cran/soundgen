#' Shift formants
#'
#' Raises or lowers formants (resonance frequencies), changing the voice quality
#' or timbre of the sound without changing its pitch, statically or dynamically.
#' Note that this is only possible when the fundamental frequency f0 is lower
#' than the formant frequencies. For best results, \code{freqWindow} should be
#' no lower than f0 and no higher than formant bandwidths. Obviously, this is
#' impossible for many signals, so just try a few reasonable values, like ~200
#' Hz for speech. If \code{freqWindow} is not specified, it is set to the
#' median detected f0, which is slow and requires detectable pitch; if pitch
#' detection fails, \code{freqWindow} defaults to 400 Hz with a message.
#'
#' Algorithm: phase vocoder. In the frequency domain, we separate the complex
#' spectrum of each STFT frame into two parts. The "receiver" is the flattened
#' or smoothed complex spectrum, where smoothing is achieved by obtaining a
#' smoothed envelope of the magnitude spectrum (the amount of smoothing is
#' controlled by \code{freqWindow}) and then dividing the complex spectrum by
#' this envelope. To avoid amplifying noise, bins more than \code{dynamicRange}
#' dB below the frame peak are divided by a floored envelope. This division
#' basically removes the formants from the signal. The second component,
#' "donor", is a scaled and interpolated version of the same smoothed magnitude
#' envelope as above - these are the formants shifted up or down. Warping can be
#' easily implemented instead of simple scaling if nonlinear spectral
#' transformations are required. We then multiply the "receiver" and "donor"
#' spectrograms and reconstruct the audio with iSTFT.
#'
#' @seealso \code{\link{shiftPitch}} \code{\link{transplantFormants}}
#'   \code{\link{getSpecEnv}}
#'
#' @inheritParams .roxygen_defaults
#' @param multFormants 1 = no change, >1 = raise formants (eg 1.1 = 10\% up, 2 =
#'   one octave up), <1 = lower formants. Anchor format accepted (see
#'   \code{\link{soundgen}})
#' @param freqWindow the width of spectral smoothing window, Hz: a single
#'   positive number, roughly the expected spacing between harmonics. Defaults
#'   to median detected f0
#' @param specEnvMethod the method of extracting a smoothed spectral envelope:
#'   "cepstral" = Gaussian liftering of the real cepstrum (default), "gauss" =
#'   Gaussian blur of the log spectrum, "movavg" = moving average of the log
#'   spectrum, "peak" = moving maximum (upper envelope). See
#'   \code{\link{getSpecEnv}} for details
#' @param interpol the method for interpolating scaled spectra: any method
#'   supported by \code{\link{interpolate}}, defaults to "splineFC"
#'
#' @return The processed waveform as a numeric vector with the original sampling
#'   rate (a list if there are multiple inputs).
#' @export
#' @examples
#' data('speechEx', package = 'soundgen')
#' # playme(speechEx)
#' # spectrogram(speechEx)
#'
#' # Lower formants by 4 semitones or ~20% = 2 ^ (-4 / 12)
#' speech1 = shiftFormants(speechEx, multFormants = 2 ^ (-4 / 12), freqWindow = 150)
#' # playme(speech1, speechEx@samp.rate)
#' # spectrogram(speech1, speechEx@samp.rate)
#'
#' orig = meanSpectrum(speechEx, plot = FALSE)
#' shifted = meanSpectrum(speech1, speechEx@samp.rate, plot = FALSE)
#' plot(ampl ~ freq, orig, log = 'y', type = 'l')
#' lines(ampl ~ freq, shifted, col = 'blue')
#'
#' # dynamic change: raise formants at the beginning, lower at the end
#' speech2 = shiftFormants(speechEx, multFormants = c(1.3, .7), freqWindow = 150)
#' # playme(speech2, speechEx@samp.rate)
#' # spectrogram(speech2, speechEx@samp.rate)
shiftFormants = function(
    x,
    multFormants,
    samplingRate = NULL,
    freqWindow = NULL,
    dynamicRange = 80,
    windowLength = 50,
    step = NULL,
    overlap = 75,
    wn = 'gaussian',
    specEnvMethod = c('cepstral', 'gauss', 'movavg', 'peak'),
    interpol = 'splineFC',
    normalize = c('orig', 'max', 'none'),
    play = FALSE,
    saveAudio = FALSE,
    reportEvery = NULL,
    cores = 1) {
  # match args
  specEnvMethod = match.arg(specEnvMethod)
  normalize = match.arg(normalize)
  multFormants = reformatAnchors(multFormants)
  if (any(!is.finite(multFormants$value)) || any(multFormants$value <= 0))
    stop('multFormants must be positive')
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.shiftFormants',
                    suffix = 'shiftFormants',
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


#' Shift formants per sound
#'
#' Internal soundgen function called by \code{\link{shiftFormants}}
#' @inheritParams shiftFormants
#' @param audio a list returned by \code{readAudio}
#' @param spec precomputed spectrogram: a complex matrix of positive frequencies
#'   with nrow = wl %/% 2 + 1
#' @noRd
.shiftFormants = function(
    audio,
    multFormants,
    freqWindow = NULL,
    spec = NULL,
    dynamicRange = 80,
    windowLength = 50,
    step = NULL,
    overlap = 75,
    wn = 'gaussian',
    specEnvMethod = 'cepstral',
    interpol = 'splineFC',
    normalize = 'orig',
    play = FALSE) {
  if (!any(multFormants$value != 1)) {
    message('Nothing to do')
    soundFiltered = audio$sound
  } else {
    if (!is.finite(windowLength) || windowLength <= 0) stop('windowLength must be positive')
    val = validateWlOvlp(audio, windowLength, step, overlap)
    wl = val$wl; step = val$step; step_points = val$step_points

    # Get a spectrogram
    if (is.null(spec)) {
      spec = stft_simple(audio$sound, wl = wl,
                         step = step_points, wn = wn)
      spec = spec[1:(wl %/% 2 + 1), ]
      # image(t(log(abs(spec))))
    } else {
      # validate a precomputed spectrogram
      if (!is.matrix(spec) || !is.complex(spec) || ncol(spec) < 1 ||
          nrow(spec) != wl %/% 2 + 1)
        stop(paste('spec must be a complex matrix of positive frequencies with',
                   'nrow = wl %/% 2 + 1, matching windowLength',
                   'and samplingRate'))
    }

    # Choose the width of smoothing window for the "recipient"
    if (is.null(freqWindow)) {
      anal = analyze(audio$sound, audio$samplingRate, plot = FALSE)
      freqWindow = median(anal$detailed$pitch, na.rm = TRUE)
    }
    if (!is.finite(freqWindow)) {
      freqWindow = 400
      message('Failed to determine freqWindow based on pitch; defaulting to 400 Hz')
    }

    freqRange_kHz = audio$samplingRate / 2 / 1000 # diff(range(as.numeric(rownames(spec))))
    freqBin_Hz = freqRange_kHz * 1000 / (nrow(spec) - 1)
    freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
    if (freqWindow_bins < 3) {
      message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz * 3)))
      freqWindow_bins = 3
    }
    if (freqWindow_bins > nrow(spec)) {
      message(paste('freqWindow cannot exceed the Nyquist frequency;
                  resetting to', floor(freqBin_Hz * nrow(spec))))
      freqWindow_bins = nrow(spec)
    }

    # Flatten the recipient and impose the warped envelope, in one pass
    spec_new = .warpSpecEnv(spec,
                            ratio = multFormants,
                            freqWindow_bins = freqWindow_bins,
                            dynamicRange = dynamicRange,
                            specEnvMethod = specEnvMethod,
                            interpol = interpol,
                            quiet = 'divide')
    # image(t(log(abs(spec_new))))

    # Reconstruct the audio
    soundFiltered = istft_simple(spec_new, wl = wl,
                                 step = step_points, wn = wn)

    # normalize
    if (normalize == 'max') {
      # soundFiltered = soundFiltered - mean(soundFiltered)
      max_val = max(abs(soundFiltered))
      if (is.finite(max_val) && max_val > 0)
        soundFiltered = soundFiltered / max_val * audio$scale
    } else if (normalize == 'orig') {
      max_val = max(abs(soundFiltered))
      if (is.finite(max_val) && max_val > 0)
        soundFiltered = soundFiltered / max_val * audio$scale_used
    }
  }

  if (isTRUE(play)) {
    playme(soundFiltered, samplingRate = audio$samplingRate)
  } else if (is.character(play)) {
    playme(soundFiltered, samplingRate = audio$samplingRate, player = play)
  }
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(soundFiltered, audio = audio, filename = filename)
  }

  soundFiltered
}


#' Warp spectral envelope
#' Flattens the spectral envelope of a complex spectrogram and imposes a
#' (possibly warped) donor envelope on it, in a single pass. Shared by
#' \code{.shiftFormants}, \code{.shiftPitch}, and \code{transplantFormants}.
#' @param spec complex matrix of positive frequencies (rows = frequency bins,
#'   columns = time)
#' @param ratio formant shift ratio per frame (>1 = raise); used to warp the
#'   envelope of \code{spec} itself when \code{env_donor} is NULL
#' @param env precomputed envelope of \code{spec}; if NULL, computed with
#'   \code{getSpecEnv}
#' @param env_donor envelope to impose; if NULL, the envelope of \code{spec}
#'   warped by \code{ratio} with \code{warpMatrix}
#' @param quiet "divide" = divide all bins by the floored envelope (donor is the
#'   same sound); "keep" = leave bins more than dynamicRange dB below the frame
#'   peak untouched (safer when the donor is a different sound)
#' @noRd
.warpSpecEnv = function(spec,
                        ratio = 1,
                        env = NULL,
                        env_donor = NULL,
                        freqWindow_bins = 10,
                        dynamicRange = 80,
                        specEnvMethod = 'cepstral',
                        interpol = 'splineFC',
                        quiet = c('divide', 'keep')) {
  quiet = match.arg(quiet)
  nr = nrow(spec)
  nc = ncol(spec)
  magn = abs(spec)
  if (is.null(env))
    env = getSpecEnv(magn, freqWindow_bins = freqWindow_bins,
                     method = specEnvMethod)
  if (is.null(env_donor))
    env_donor = warpMatrix(env, scaleFactor = ratio, interpol = interpol)
  sc_mat = matrix(apply(magn, 2, max), nrow = nr, ncol = nc, byrow = TRUE)
  floor_mat = sc_mat * 10 ^ (-dynamicRange / 20)
  env_safe = pmax(env, floor_mat)
  env_safe[!is.finite(env_safe) | env_safe <= 0] = 1
  if (quiet == 'divide') {
    spec_new = spec / env_safe * env_donor
  } else {
    cor_coef = sc_mat / env_safe
    cor_coef[env <= floor_mat] = 1  # don't touch very quiet bins
    spec_new = spec * cor_coef * env_donor
  }
  spec_new
}
