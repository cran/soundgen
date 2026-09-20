#' Shift pitch
#'
#' Raises or lowers pitch with or without also shifting the formants (resonance
#' frequencies) and performing a time-stretch. The three operations (pitch
#' shift, formant shift, and time stretch) are independent and can be performed
#' in any combination, statically or dynamically. \code{shiftPitch} can also be
#' used to shift formants without changing pitch or duration, but the dedicated
#' \code{\link{shiftFormants}} is faster for that task. Likewise, use the much
#' faster \code{\link{timeStretch}} for slowing down or speeding up a recording
#' without preserving either pitch or formants. Tip: increase overlap to >90\%
#' for best quality.
#'
#' Algorithm: phase vocoder. Pitch shifting is accomplished by performing a time
#' stretch (at present, with horizontal or adaptive phase propagation) followed
#' by resampling. This shifts both pitch and formants; to preserve the original
#' formant frequencies or modify them independently of pitch, the recipient's
#' spectral envelope is flattened and the donor envelope is imposed on the same
#' spectrogram before iSTFT. See Prusa 2017 "Phase vocoder done right", Royer
#' 2019 "Pitch-shifting algorithm design and applications in music".
#'
#' @seealso \code{\link{shiftFormants}} \code{\link{transplantFormants}}
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams shiftFormants
#' @param dynamicRange dynamic range (dB) that silences spectrogram regions
#'   below \code{-dyamicRange} dB and also control the \code{tol} parameter in
#'   \code{istft_timevar}
#' @param multPitch 1 = no change, >1 = raise pitch (eg 1.1 = 10\% up, 2 = one
#'   octave up), <1 = lower pitch. Anchor format accepted for multPitch /
#'   multFormants / timeStretch (see \code{\link{soundgen}})
#' @param multFormants 1 = no change, >1 = raise formants (eg 1.1 = 10\% up, 2 =
#'   one octave up), <1 = lower formants. The default behavior is for formants
#'   to follow pitch with \code{multFormants = multPitch}
#' @param timeStretch 1 = no change, >1 = longer, <1 = shorter
#' @param freqWindow the width of spectral smoothing window, Hz - see
#'   \code{\link{shiftFormants}} for discussion and examples. Defaults to
#'   detected f0 prior to pitch shifting times \code{mean(multPitch)}. Note that
#'   formant shifting is applied AFTER pitch shifting, so \code{freqWindow}
#'   should be multiplied by \code{multPitch} compared to what you would use on
#'   the original audio
#' @param propagation the method for propagating phase: "time" (default) =
#'   single-pass horizontal propagation, several times faster; "adaptive" =
#'   a modified "vocoder done right" (Prusa & Holighaus 2017), high-quality
#'   but relatively slow
#' @param interpol the method for interpolating scaled spectra and anchors: any
#'   method supported by \code{\link{interpolate}}, defaults to "splineFC"
#' @param preserveEnv if TRUE, transplants the amplitude envelope from the
#'   original to the modified sound with \code{\link{transplantEnv}}. Mostly
#'   this makes sense if there is no time stretching
#' @param transplantEnv_pars a list of parameters passed on to
#'   \code{\link{transplantEnv}} if \code{preserveEnv = TRUE}
#'
#' @return The processed waveform as a numeric vector (a list if there are
#'   multiple inputs).
#' @export
#' @examples
#' data(speechEx, package = 'soundgen')
#' samplingRate = speechEx@samp.rate
#' # playme(speechEx)
#' spectrogram(speechEx, yScale = 'ERB')
#'
#' # raise pitch, lower formants
#' s1 = shiftPitch(speechEx, freqWindow = 200, multPitch = 1.2, multFormants = .85)
#' # spectrogram(s1, samplingRate, yScale = 'ERB')
#' # playme(s1, samplingRate)
#'
#' \dontrun{
#' # Tips for best quality: high overlap (slow), pad with silence
#' # and fade a bit before processing; try adaptive phase propagation (slow)
#' s = c(
#'   rep(0, 1000),
#'   fade(speechEx@left, samplingRate, fadeIn = 50, fadeOut = 50),
#'   rep(0, 1000)
#' )
#' s1a = shiftPitch(s, samplingRate, freqWindow = 200, multPitch = 1.2,
#'   multFormants = .85, propagation = 'adaptive', overlap = 95)
#' spectrogram(s1a, samplingRate, yScale = 'ERB')
#' playme(s1a, samplingRate)
#' # cf:
#' playme(s1, samplingRate)
#'
#' ## Dynamic manipulations
#' # Add a chevron-shaped contour to both pitch and formants
#' s2 = shiftPitch(speechEx, multPitch = c(1.1, 1.3, .8))
#' playme(s2, samplingRate)
#' spectrogram(s2, samplingRate, yScale = 'ERB')
#'
#' # Time-stretch only the middle
#' s3 = shiftPitch(speechEx, overlap = 95, timeStretch = list(
#'   time = c(0, .25, .31, .5, .55, 1),
#'   value = c(1, 1, 3, 3, 1, 1))
#' )
#' playme(s3, samplingRate)
#'
#' # Raise pitch and formants by 3 semitones, shorten
#' s4 = shiftPitch(speechEx, multPitch = 2 ^ (3 / 12), timeStretch = 0.75)
#' playme(s4, samplingRate)
#' spectrogram(s4, samplingRate, yScale = 'ERB')
#'
#' # Just speed up
#' shiftPitch(speechEx, multPitch = 1, timeStretch = 0.75, play = TRUE)
#'
#' # Raise pitch, preserve formants
#' s5 = shiftPitch(speechEx, multPitch = 1.2, multFormants = 1, freqWindow = 150)
#' playme(s5, samplingRate)
#' spectrogram(s5, samplingRate, yScale = 'ERB')
#'
#' # Only modify voiced frames, preserving consonants / breathing etc
#' s1 = soundgen(nSyl = 2, sylLen = 300, pauseLen = 500, pitch = c(250, 200))
#' s2 = soundgen(sylLen = 150, noise = 0, pitch = NA, formants = list(f1 = 5000))
#' s = addVectors(s1, s2, insertionPoint = 16000 * .5)
#' s = s + rnorm(length(s), 0, .01)
#' spectrogram(s, 16000, yScale = 'ERB')
#' playme(s, 16000)  # we want to ignore the central /tch/ when shifting f0
#' # run analyze to get f0 contours, correct manually with pitch_app if needed
#' an = analyze(s, 16000, windowLength = 25, step = 5, plot = TRUE, yScale = 'ERB')
#' pitch = an$detailed$pitch
#' multPitch = ifelse(is.na(pitch), 1, 1.25)
#' s_shifted = fade(shiftPitch(s, 16000, multPitch = multPitch, wn = 'hanning', overlap = 90))
#' spectrogram(s_shifted, 16000, yScale = 'ERB')
#' playme(s_shifted, 16000)  # the /tch/ in the middle is unaffected by pitch-shifting
#' }
shiftPitch = function(
    x,
    samplingRate = NULL,
    multPitch = 1,
    multFormants = multPitch,
    timeStretch = 1,
    freqWindow = NULL,
    dynamicRange = 80,
    windowLength = 40,
    step = NULL,
    overlap = 90,
    wn = 'hanning',
    interpol = 'splineFC',
    propagation = c('time', 'adaptive'),
    specEnvMethod = c('cepstral', 'gauss', 'movavg', 'peak'),
    preserveEnv = FALSE,
    transplantEnv_pars = list(windowLength = 10),
    normalize = c('orig', 'max', 'none'),
    play = FALSE,
    saveAudio = FALSE,
    reportEvery = NULL,
    cores = 1) {
  propagation = match.arg(propagation)
  normalize = match.arg(normalize)
  specEnvMethod = match.arg(specEnvMethod)
  multPitch = reformatAnchors(multPitch)
  multFormants = reformatAnchors(multFormants)
  timeStretch = reformatAnchors(timeStretch)
  if (any(multPitch$value <= 0) || any(multFormants$value <= 0) ||
      any(timeStretch$value <= 0)) stop('multipliers must be positive')

  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.shiftPitch',
                    suffix = 'shiftPitch',
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


#' Shift pitch per sound
#' @param audio a list returned by \code{readAudio}
#' @noRd
.shiftPitch = function(
    audio,
    multPitch,
    multFormants,
    timeStretch,
    freqWindow = NULL,
    dynamicRange = 80,
    windowLength = 40,
    step = NULL,
    overlap = 90,
    wn = 'hanning',
    interpol = 'splineFC',
    propagation = 'time',
    specEnvMethod = 'cepstral',
    preserveEnv = FALSE,
    transplantEnv_pars = list(),
    normalize = 'orig',
    play = FALSE) {
  if (!(any(multPitch$value != 1) ||
        any(multFormants$value != 1) ||
        any(timeStretch$value != 1))) {
    message('Nothing to do: all multipliers = 1')
    soundFiltered = audio$sound
  } else {
    val = validateWlOvlp(audio, windowLength, step, overlap)
    wl = val$wl; step = val$step; step_points = val$step_points

    # Get a spectrogram
    spec = stft_simple(audio$sound, wl = wl,
                       step = step_points, wn = wn)
    spec = spec[1:(wl %/% 2 + 1), ]
    nr = nrow(spec)
    nc = ncol(spec)
    magn = abs(spec)

    pitch_or_stretch = any(multPitch$value != 1) || any(timeStretch$value != 1)
    formants_differ = (nrow(multFormants) != nrow(multPitch)) ||
      any(multFormants$value != multPitch$value)

    ## Time-stretching / pitch-shifting (phase vocoder)
    if (pitch_or_stretch) {
      bin_width = audio$samplingRate / wl
      freqs = (0:(nr - 1)) * bin_width
      step_s = step_points / audio$samplingRate
      multPitch_vector = getSmoothContour(multPitch, len = nc - 1,
                                          interpol = interpol)
      # Quantize the pitch ratio to what the resampler can realize
      target_len = pmax(3, round(wl / multPitch_vector))
      multPitch_vector = (wl - 1) / (target_len - 1)
      timeStretch_vector = getSmoothContour(timeStretch, len = nc - 1,
                                            interpol = interpol)
      # unwrap the phase, calculate instantaneous frequency - see Prusa 2017,
      # Royer 2019
      phase_new = dPhase(phase = Arg(spec),
                         magn = magn,
                         step_s = step_s,
                         freqs = freqs,
                         alpha = multPitch_vector * timeStretch_vector,
                         propagation = propagation,
                         multPitch = multPitch_vector)  # bin-width scaling in phasePropagate
      spec1 = matrix(complex(modulus = magn, argument = phase_new), nrow = nr)
    } else {
      spec1 = spec
      multPitch_vector = rep(1, nc - 1)
      timeStretch_vector = rep(1, nc - 1)
    }

    ## Warp formants in the same pass, unless they shift with pitch anyway
    if (formants_differ) {
      multFormants_vector = getSmoothContour(multFormants, len = nc - 1,
                                             interpol = interpol)
      # Choose the width of smoothing window (of the original, unshifted sound)
      if (is.null(freqWindow)) {
        anal = analyze(audio$sound, audio$samplingRate, plot = FALSE)
        freqWindow = median(anal$detailed$pitch, na.rm = TRUE) * mean(multPitch_vector)
        if (!is.finite(freqWindow) || freqWindow <= 0)
          stop(paste('No usable f0 detected for automatic freqWindow;',
                     'please specify freqWindow manually (in Hz)'))
      } else {
        if (length(freqWindow) != 1 || !is.finite(freqWindow) || freqWindow <= 0)
          stop('freqWindow must be a single positive number (Hz)')
      }
      freqRange_kHz = audio$samplingRate / 2 / 1000
      freqBin_Hz = freqRange_kHz * 1000 / (nr - 1)
      freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
      if (freqWindow_bins < 3) {
        message(paste('freqWindow has to be at least 3 bins wide;
                    resetting to', ceiling(freqBin_Hz * 3)))
        freqWindow_bins = 3
      }
      if (freqWindow_bins > nr) {
        message(paste('freqWindow cannot exceed the Nyquist frequency;
                    resetting to', floor(freqBin_Hz * nr)))
        freqWindow_bins = nr
      }
      # resampling by multPitch will warp all frequencies (incl. formants) by
      # multPitch; compensate now so that final formants = multFormants
      spec1 = .warpSpecEnv(spec1,
                           ratio = multFormants_vector / multPitch_vector,
                           freqWindow_bins = freqWindow_bins,
                           dynamicRange = dynamicRange,
                           specEnvMethod = specEnvMethod,
                           interpol = interpol,
                           quiet = 'divide')
    }
    # image(t(log(abs(spec1))))

    # Reconstruct the audio
    if (pitch_or_stretch) {
      soundFiltered = istft_timevar(
        spec1,
        step = step_points,
        wl = wl,
        wn = wn,
        multPitch = multPitch_vector,
        timeStretch = timeStretch_vector,
        fade = TRUE,
        tol =  10^(-dynamicRange/20))
    } else {
      soundFiltered = istft_simple(spec1, wl = wl,
                                   step = step_points, wn = wn)
    }
    # playme(soundFiltered, audio$samplingRate)
    # spectrogram(soundFiltered, audio$samplingRate)

    # Transplant amplitude envelope from the original
    if (preserveEnv) {
      soundFiltered = do.call(transplantEnv, c(
        transplantEnv_pars,
        list(donor = audio$sound,
             samplingRateR = audio$samplingRate,
             samplingRateD = audio$samplingRate,
             recipient = soundFiltered)))
    }

    # normalize
    max_val = max(abs(soundFiltered))
    if (normalize == 'max') {
      if (is.finite(max_val) && max_val > 0)
        soundFiltered = soundFiltered / max_val * audio$scale
    } else if (normalize == 'orig') {
      if (is.finite(max_val) && max_val > 0)
        soundFiltered = soundFiltered / max_val * audio$scale_used
    }
  }

  # Play, save, return
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


#' Phase derivatives
#' Calculates partial derivatives of phase with respect to time and frequency,
#' then propagates phase to create a time-stretched/pitch-shifted phase matrix.
#' @param phase,magn phase and magnitude of a spectrogram (matrices: freq × time)
#' @param step_s time step between frames in seconds
#' @param freqs vector of center frequencies for each bin (Hz)
#' @param alpha stretch factors for each time step (length = nc - 1)
#' @param propagation method: "time" = horizontal phase propagation (default),
#'   "adaptive" = experimental "vocoder done right" algorithm
#' @param tol tolerance threshold for adaptive propagation
#' @param nr,nc dimensions of input spectrogram (nrow, ncol)
#' @noRd
dPhase = function(phase,
                  magn,
                  step_s,
                  freqs,
                  alpha,
                  propagation = 'time',
                  multPitch = 1,
                  tol = 10^(-6),
                  nr = nrow(phase),
                  nc = ncol(phase)) {

  # Calculate horizontal partial derivative of phase (dTime)
  # This gives the instantaneous frequency at each time-frequency bin
  # We compute derivatives at the TRANSITIONS between frames, not at frames themselves

  # Expected phase advance for a bin at its center frequency
  expected_advance = step_s * 2 * pi * freqs  # length = nr

  # Compute phase differences between adjacent frames
  # dp_hor[, i] is the derivative from frame i to frame i+1
  dp_hor = matrix(0, nrow = nr, ncol = nc - 1)
  for (i in seq_len(nc - 1)) {
    # Phase difference between frame i+1 and frame i
    phase_diff = phase[, i + 1] - phase[, i]

    # The actual frequency is the expected frequency plus deviation
    # princarg maps the phase difference to [-pi, pi] to handle wrapping
    dp_hor[, i] = expected_advance + princarg(phase_diff - expected_advance)
  }

  # Calculate vertical partial derivative of phase (dFrequency)
  # This gives the group delay at each time-frequency bin
  bin_width = freqs[2] - freqs[1]  # Hz
  dp_ver = matrix(0, nrow = nr, ncol = nc)

  # First frequency bin: forward difference only
  dp_ver[1, ] = princarg(phase[2, ] - phase[1, ]) / bin_width

  # Interior frequency bins: centered differences
  for (i in 2:(nr - 1)) {
    # Average of forward and backward differences for better accuracy
    dp_down = princarg(phase[i, ] - phase[i - 1, ]) / bin_width
    dp_up = princarg(phase[i + 1, ] - phase[i, ]) / bin_width
    dp_ver[i, ] = (dp_down + dp_up) / 2
  }

  # Last frequency bin: backward difference only
  dp_ver[nr, ] = princarg(phase[nr, ] - phase[nr - 1, ]) / bin_width

  # Propagate phase using the calculated derivatives
  phase_new = phase  # Initialize with original phase

  if (propagation == 'time') {
    # Horizontal propagation: integrate frequency along time
    # phase_new[, i] = phase_new[, i-1] + dp_hor[, i-1] * alpha[i-1]
    for (i in 2:nc) {
      phase_new[, i] = phase_new[, i - 1] + dp_hor[, i - 1] * alpha[i - 1]
    }

  } else if (propagation == 'adaptive') {
    # Adaptive propagation using "vocoder done right" algorithm
    # For each frame, propagate phase horizontally first, then vertically
    # based on magnitude peaks
    for (i in 2:nc) {
      phase_new[, i] = phasePropagate(
        i = i,
        dp_hor = dp_hor,
        dp_ver = dp_ver,
        magn = magn,
        phase_new = phase_new,
        tol = tol,
        bin_width = bin_width * multPitch[i - 1],
        alpha = alpha[i - 1]
      )
    }
  }

  return(phase_new)
}


#' Propagate phase
#' Propagates phase using the "vocoder done right" algorithm, as in Prusa &
#' Holighaus 2017 "Phase vocoder done right".
#' @inheritParams dPhase
#' @param i analyzed frame
#' @param dp_hor,dp_ver time and frequency partial derivatives of phase
#' @param phase_new matrix for storing the new phase
#' @param bin_width width of frequency bin, Hz
#' @noRd
phasePropagate = function(i,
                          dp_hor,
                          dp_ver,
                          magn,
                          phase_new,
                          tol,
                          bin_width,   # scalar: base bin width * multPitch[i - 1]
                          alpha) {     # scalar: alpha[i - 1]
  nr = nrow(dp_hor)
  nh = ncol(dp_hor)  # number of hops = nc - 1
  out = runif(nr, -pi, pi)
  abstol = tol * max(magn[, c(i, i - 1)])

  bins_todo = magn[, i] > abstol
  if (!any(bins_todo)) return(out)

  # instantaneous frequency at the two ends of hop [i-1, i]: centered estimates
  # where possible, one-sided at the edges (dp_hor[, k] exists only for k <= nh)
  w_prev = if (i - 2 >= 1) {
    (dp_hor[, i - 2] + dp_hor[, i - 1]) / 2
  } else {
    dp_hor[, i - 1]
  }
  w_cur = if (i <= nh) {
    (dp_hor[, i - 1] + dp_hor[, i]) / 2
  } else {
    dp_hor[, i - 1]
  }
  adv_hor = alpha * (w_prev + w_cur) / 2

  # pre-allocate heap: each bin can be pushed once horizontally + twice vertically
  max_heap_size = 3 * nr + 10
  heap_frame = integer(max_heap_size)
  heap_bin = integer(max_heap_size)
  heap_magn = numeric(max_heap_size)
  heap_size = 0

  bins_cur = which(bins_todo)
  n_init = length(bins_cur)
  heap_frame[1:n_init] = i - 1
  heap_bin[1:n_init] = bins_cur
  heap_magn[1:n_init] = magn[bins_cur, i - 1]
  heap_size = n_init

  counter = 0
  max_counter = sum(bins_todo) * 3 + 10

  while (heap_size > 0 && any(bins_todo)) {
    counter = counter + 1
    if (counter > max_counter) break

    idx_max = which.max(heap_magn[1:heap_size])
    top_frame = heap_frame[idx_max]
    top_bin = heap_bin[idx_max]

    heap_frame[idx_max] = heap_frame[heap_size]
    heap_bin[idx_max] = heap_bin[heap_size]
    heap_magn[idx_max] = heap_magn[heap_size]
    heap_size = heap_size - 1

    if (top_frame == i - 1) {
      # horizontal propagation from frame i-1 to frame i
      if (bins_todo[top_bin]) {
        out[top_bin] = phase_new[top_bin, i - 1] + adv_hor[top_bin]
        bins_todo[top_bin] = FALSE

        heap_size = heap_size + 1
        heap_frame[heap_size] = i
        heap_bin[heap_size] = top_bin
        heap_magn[heap_size] = magn[top_bin, i]
      }
    } else if (top_frame == i) {
      # vertical propagation upward
      if (top_bin < nr && bins_todo[top_bin + 1]) {
        out[top_bin + 1] = out[top_bin] +
          bin_width * (dp_ver[top_bin, i] + dp_ver[top_bin + 1, i]) / 2
        bins_todo[top_bin + 1] = FALSE

        heap_size = heap_size + 1
        heap_frame[heap_size] = i
        heap_bin[heap_size] = top_bin + 1
        heap_magn[heap_size] = magn[top_bin + 1, i]
      }
      # vertical propagation downward
      if (top_bin > 1 && bins_todo[top_bin - 1]) {
        out[top_bin - 1] = out[top_bin] -
          bin_width * (dp_ver[top_bin - 1, i] + dp_ver[top_bin, i]) / 2
        bins_todo[top_bin - 1] = FALSE

        heap_size = heap_size + 1
        heap_frame[heap_size] = i
        heap_bin[heap_size] = top_bin - 1
        heap_magn[heap_size] = magn[top_bin - 1, i]
      }
    }
  }
  out
}
