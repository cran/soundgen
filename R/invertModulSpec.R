#' Filter sound by modulation spectrum
#'
#' Manipulates the modulation spectrum (MS) of a sound to attenuate or
#' amplify certain frequencies of amplitude modulation (AM) and frequency
#' modulation (FM). Algorithm: produces a modulation spectrum with
#' \code{\link{modulationSpectrum}}, modifies it with \code{\link{filterMS}},
#' converts the modified MS back to a spectrogram, and finally inverts the
#' spectrogram with \code{\link{invertSpectrogram}}, thus producing a sound
#' with (approximately) the desired characteristics of the MS. Note that the
#' last step of inverting the spectrogram introduces some noise, so the
#' resulting MS is not precisely the same as the intermediate filtered version.
#' In practice this means that some residual energy will still be present in the
#' filtered-out frequency range (see examples).
#'
#' By default, the filter mask is smoothed to avoid abrupt cutoffs. Smoothing is
#' specified in physical units: FM in cycles/kHz and AM in Hz. Set \code{smooth
#' = NA} to disable smoothing. If \code{logSpec = TRUE}, filtering is performed
#' on the modulation spectrum of the log-magnitude spectrogram, which is often
#' more robust and more physiologically plausible than filtering linear
#' magnitudes. If \code{initialPhase = 'orig'}, the phase of the original sound
#' is used. With \code{nIter = 0}, the original phase is used directly; with
#' \code{nIter > 0}, the original phase is used to initialize the Griffin-Lim
#' iterations.
#'
#' @section Choosing filtering settings:
#' MS filtering operates globally on the spectrogram's envelope, so any
#' transient whose modulation content overlaps the filtered band —
#' onsets/offsets, plosives, amplitude steps — is partially removed together
#' with the target, producing a ripple that decays inward from the transient.
#' Smoothing (blurring the filter mask before applying it) shortens the ripple's
#' tails but cannot eliminate it completely. Two parameters control the
#' trade-off between successful filtering and introducing spectro-temporal
#' artifacts: (1) \code{dynamicRange}: lower values shrink the silence-to-sound
#' step in the log domain and lift the linear target above the zero-clipping
#' knee, suppressing edge ripple at the cost of a uniform residual noise floor,
#' most visible in silent regions, and (2) \code{dB}: finite attenuation, e.g.
#' 20–40 dB, removes proportionally less of the transients' in-band content than
#' dB = Inf. Use \code{dB = Inf} with a large \code{dynamicRange} for steady,
#' sustained sounds and finite dB with dynamicRange = 60–80 for sounds with
#' transients, such as speech. Use \code{logSpec = TRUE} for manipulating the
#' temporal envelope / AM in a perceptually meaningful manner. \code{logSpec =
#' FALSE} weights components by absolute magnitude and avoids amplifying quiet
#' regions, but shows stronger edge artifacts. Choose \code{protect = c('level',
#' 'meanSpectrum')} (the default setting) when filtering AM. Do not protect
#' \code{'envelope'}, which contains the very AM being removed. Set
#' \code{protect = c('level', 'envelope')} when filtering FM, so as to keep the
#' natural amplitude contour. Drop \code{'meanSpectrum'} only if you also want
#' to flatten the static spectral shape. Whenever the condition includes 0 on an
#' axis (e.g. removing slow AM with abs(am) < 3), keep the corresponding
#' protection to avoid a collapsed, "AC-coupled" reconstruction. For mild
#' filtering, \code{initialPhase = 'orig'} with \code{nIter = 0} is fast and
#' preserves the original phase; increase \code{nIter} for stronger filtering.
#'
#' @seealso \code{\link{invertSpectrogram}}
#'
#' @return Filtered audio as a numeric vector normalized to \code{[-1, 1]} with
#'   the same sampling rate as input.
#'
#' @inheritParams modulationSpectrum
#' @inheritParams invertSpectrogram
#' @inheritParams .roxygen_defaults
#' @param ms a modulation spectrum: matrix of real or complex values, AM in
#'   columns (Hz), FM in rows (cycles/kHz)
#' @param amCond,fmCond condition on amplitude and frequency modulation:
#'   character string, expression, or function (see examples)
#' @param jointCond character string, expression, or function with a joint
#'   condition on am and fm
#' @param action 'stop' or 'pass'
#' @param dB a positive number giving the strength of effect in dB (defaults to
#'   Inf - complete removal of selected frequencies)
#' @param protect which zero-modulation components to protect from filtering:
#'   'level' = the (0, 0) cell (overall mean level / gain); 'meanSpectrum' = the
#'   (am = 0) column (time-averaged spectrum); 'envelope' = the (fm = 0) row
#'   (broadband temporal envelope, including any AM). TRUE protects all three;
#'   FALSE, NULL or NA protects none. NB: protecting 'envelope' re-injects
#'   temporal modulation when filtering AM; protecting 'meanSpectrum' preserves
#'   static spectral modulation when filtering FM
#' @param dynamicRange the minimum possible value of the magnitude spectrogram
#'   (dB, must be positive or NA). 80 (default) = 80 dB below the global
#'   maximum, NA or Inf = no floor (allow zero values). Lowering the dynamic
#'   range (e.g., to 60 dB) attenuates ripple effects caused by filtering, but
#'   adds some uniform residual noise at the floor level
#' @param smooth smoothing of the MS filter mask. NULL = default smoothing
#'   (~3 bins); NA = no smoothing; numeric vector of length 2 = smoothing
#'   bandwidth for FM and AM in native units (FM: cycles/kHz, AM: Hz). A single
#'   number is not allowed because FM and AM have different units
#' @param warnAsymmetric if TRUE, warns if the MS filter mask is asymmetric
#'   with respect to zero AM/FM. Asymmetric filters can produce complex-valued
#'   inverse transforms and phase-related artifacts
#' @param initialPhase initial phase estimate: "orig" (default) = phase of the
#'   original sound; "spsi" = single-pass spectrogram inversion (Beauregard et
#'   al., 2015); "random" = uniformly distributed noise; "zero" = all phases
#'   zero. If \code{nIter = 0} and \code{initialPhase = "orig"}, the original
#'   phase is used directly
#' @param plot if TRUE, produces a triple plot: original MS, filtered MS, and
#'   the MS of the output sound
#' @export
#' @examples
#' # Create a sound to be filtered
#' s = soundgen(sylLen = 500, samplingRate = 16000,
#'     amFreq = 25, amDep = 50,
#'     addSilence = 50, plot = TRUE)
#' # playme(s, 16000)
#'
#' # Filter to remove the rapid AM at 25 Hz
#' s_filt = fade(filterSoundByMS(s, samplingRate = 16000,
#'   amCond = 'abs(am) > 22 & abs(am) < 28',
#'   action = 'stop',
#'   plot = TRUE))
#' # NB: clicks are often introduced at the onset and offset - fade and renormalize
#' s_filt = s_filt / max(abs(s_filt))
#' # playme(s_filt, samplingRate = 16000)
#' spectrogram(s_filt, 16000, windowLength = 25)
#'
#' # Using filterMS()
#' ms = modulationSpectrum(s, 16000, returnComplex = TRUE)$detailed$complex
#'
#' # Remove all AM over 25 Hz
#' filterMS(ms, amCond = 'abs(am) > 25', protect = FALSE)
#' filterMS(ms, amCond = 'abs(am) > 25', protect = TRUE)
#' filterMS(ms, amCond = 'abs(am) > 25', protect = TRUE, smooth = NA)
#'
#' # amCond and fmCond are OR-conditions
#' filterMS(ms, amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5', action = 'stop')
#' filterMS(ms, amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5', action = 'pass')
#' filterMS(ms, amCond = 'abs(am) > 10 & abs(am) < 25', action = 'stop')
#'
#' # jointCond is more flexible
#' filterMS(ms, jointCond = 'am * fm < 5', action = 'stop')
#' filterMS(ms, jointCond = 'am^2 + (fm*3)^2 < 200', action = 'pass')
#'
#' # So:
#' filterMS(ms, jointCond = 'abs(am) > 5 | abs(fm) < 5')  # general
#' # ...is the same as:
#' filterMS(ms, amCond = 'abs(am) > 5', fmCond = 'abs(fm) < 5')  # faster
#'
#' \dontrun{
#' data('speechEx', package = 'soundgen')
#' samplingRate = speechEx@samp.rate
#' playme(speechEx)
#' spectrogram(speechEx)
#'
#' # Remove AM above 3 Hz from a bit of speech (removes most temporal details)
#' s_filt1 = fade(filterSoundByMS(speechEx, amCond = 'abs(am) > 3',
#'                           action = 'stop',
#'                           smooth = c(0.5, 2)))
#' spectrogram(s_filt1, samplingRate = samplingRate)
#' playme(s_filt1, samplingRate)
#'
#' # Remove FM above 3 cycles/kHz (masks f0 but preserves formants)
#' s_filt3 = fade(filterSoundByMS(speechEx,
#'   fmCond = 'abs(fm) > 3', action = 'stop',
#'   logSpec = TRUE, smooth = c(1, 5),
#'   protect = c('level', 'envelope')))
#' spectrogram(s_filt3, samplingRate = samplingRate)
#' playme(s_filt3, samplingRate)
#'
#' # Joint spectro-temporal filtering with a symmetric condition
#' s_filt4 = fade(filterSoundByMS(speechEx,
#'   jointCond = 'am^2 + (fm * 3)^2 < 300', action = 'stop',
#'   logSpec = TRUE, dynamicRange = 60, smooth = c(1, 10),
#'   protect = c('level', 'meanSpectrum')))
#' spectrogram(s_filt4, samplingRate = samplingRate)
#' playme(s_filt4, samplingRate)
#'
#' # Process all files in a folder, save filtered audio and plots
#' s_filt = filterSoundByMS('~/Downloads/temp2',
#'   saveAudio = TRUE, savePlots = TRUE,
#'   amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5',
#'   action = 'stop', smooth = c(0.5, 5),
#'   nIter = 10)
#' }
filterSoundByMS = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    logSpec = TRUE,
    windowLength = 25,
    step = NULL,
    overlap = 80,
    wn = 'hanning',
    amCond = NULL,
    fmCond = NULL,
    jointCond = NULL,
    action = c('stop', 'pass'),
    dynamicRange = 80,
    dB = Inf,
    smooth = NULL,
    protect = c('level', 'meanSpectrum'),
    warnAsymmetric = TRUE,
    initialPhase = c('orig', 'spsi', 'random', 'zero'),
    nIter = 50,
    reportEvery = NULL,
    cores = 1,
    play = FALSE,
    saveAudio = FALSE,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  action = match.arg(action)
  initialPhase = match.arg(initialPhase)

  ## Prepare a list of arguments to pass to .filterSoundByMS()
  myPars = mget(names(formals()), sys.frame(sys.nframe()))

  # exclude unnecessary args
  drop_args = c('x', 'samplingRate', 'from', 'to', 'savePlots',
                'saveAudio', 'embed', 'reportEvery', 'cores')
  myPars = myPars[!names(myPars) %in% drop_args]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.filterSoundByMS',
    suffix = 'filterSoundByMS',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots,
    saveAudio = saveAudio
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units),
                  changesAudio = saveAudio, embed = embed))

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Filter a single sound by MS
#' @noRd
.filterSoundByMS = function(
    audio,
    logSpec = TRUE,
    windowLength = 25,
    step = NULL,
    overlap = 80,
    wn = 'hamming',
    amCond = NULL,
    fmCond = NULL,
    jointCond = NULL,
    action = c('stop', 'pass'),
    dynamicRange = 80,
    dB = Inf,
    smooth = NULL,
    protect = c('level', 'meanSpectrum'),
    warnAsymmetric = TRUE,
    initialPhase = c('orig', 'spsi', 'random', 'zero'),
    nIter = 50,
    play = FALSE,
    plot = TRUE,
    savePlots = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  action = match.arg(action)
  initialPhase = match.arg(initialPhase)
  val = validateWlOvlp(audio, windowLength, step, overlap)
  wl = val$wl; step = val$step; step_points = val$step_points

  if (logSpec && (!is.finite(dynamicRange) || dynamicRange <= 0))
    stop(paste('logSpec = TRUE requires a finite, positive dynamicRange (a floor);',
               'otherwise log(0) = -Inf'))

  # Get the original complex spectrogram and its magnitude
  spec_full_orig = stft_simple(audio$sound, samplingRate = audio$samplingRate,
                               wl = wl,
                               step = step_points, wn = wn)
  nr = nrow(spec_full_orig) %/% 2 + 1
  spec_for_ms = Mod(spec_full_orig[1:nr, , drop = FALSE])

  # floor at -dynamicRange dB relative to max, log-transform if requested
  spec_for_ms = floor_log(spec_for_ms,
                          takeLog = logSpec,
                          dynamicRange = dynamicRange)

  # The sound may be too short or silent for MS filtering
  if (nr < 2 || ncol(spec_for_ms) < 2 || max(spec_for_ms) <= 0) {
    warning(paste('The sound is too short or silent for MS filtering;',
                  'returning the original sound'))
    return(audio$sound)
  }

  # Get a modulation spectrum
  ms = specToMS(spec_for_ms, center = TRUE)$ms_centered

  # Filter as needed
  ms_filt_obj = filterMS(ms,
                         amCond = amCond,
                         fmCond = fmCond,
                         jointCond = jointCond,
                         action = action,
                         dB = dB,
                         smooth = smooth,
                         protect = protect,
                         warnAsymmetric = warnAsymmetric,
                         plot = FALSE)
  ms_filt = ms_filt_obj$ms

  # Convert the filtered MS back to a spectrogram with an inverse 2D FFT.
  # We do this inline instead of calling msToSpec() because msToSpec() takes
  # Mod(), which is fine for linear magnitude but inappropriate for log-MS.
  ms_filt_uncnt = centerMS(ms_filt, inverse = TRUE)
  spec_rec_complex = fft(ms_filt_uncnt, inverse = TRUE) /
    (nrow(ms_filt_uncnt) * ncol(ms_filt_uncnt))

  # A perfectly symmetric (Hermitian) filter yields a real inverse. A
  # non-negligible imaginary part means the filter is asymmetric, in which
  # case the inverse is not a valid spectrogram and we need a fallback.
  mag_max = max(Mod(spec_rec_complex))
  im_ratio = if (mag_max > 0) max(abs(Im(spec_rec_complex))) / mag_max else 0
  tol_imag = 1e-6

  if (im_ratio <= tol_imag) {
    # The filter is (nearly) Hermitian: the inverse is real
    spec_rec = Re(spec_rec_complex)
  } else if (!logSpec) {
    # Asymmetric filter on a linear magnitude spectrogram: fall back to the
    # Mod() projection, which always yields a non-negative magnitude surface
    warning(paste0('The filtered MS is not Hermitian (relative imaginary part = ',
                   format(im_ratio, digits = 3),
                   '); falling back to msToSpec()'))
    spec_rec = msToSpec(ms_filt)
  } else {
    # Asymmetric filter on a log spectrogram: Mod() would fold negative log
    # values, so instead take the real part, which is equivalent to applying
    # the symmetrized filter (H + H_rev) / 2
    warning(paste0('The filtered MS is not Hermitian (relative imaginary part = ',
                   format(im_ratio, digits = 3),
                   '); using the real part, i.e. the symmetrized filter'))
    spec_rec = Re(spec_rec_complex)
  }
  spec_rec[!is.finite(spec_rec)] = 0

  if (logSpec) {
    spec_filt = spec_rec
    specScale_inv = 'log'

    # For original-phase reconstruction we need linear magnitude
    spec_mag_target = exp(spec_filt)
    spec_mag_target[!is.finite(spec_mag_target)] = 0
  } else {
    spec_filt = pmax(spec_rec, 0)
    specScale_inv = 'spec'

    # For original-phase reconstruction we need linear magnitude
    spec_mag_target = spec_filt
  }
  rownames(spec_filt) = rownames(spec_mag_target) = rownames(spec_for_ms)
  colnames(spec_filt) = colnames(spec_mag_target) = colnames(spec_for_ms)

  # The filter may have removed all energy (e.g. action = 'pass' with a
  # condition that matches nothing: inverting an all- zero spectrogram would
  # produce NaN at the normalization step
  if (max(spec_mag_target) <= 0) {
    warning('The MS filter removed all energy; returning silence')
    return(rep(0, audio$ls))
  } else if (initialPhase == 'orig') {
    # Build the full target magnitude with mirrored negative frequencies
    if (wl %% 2 == 0) {  # even wl
      spec_target_full = rbind(spec_mag_target,
                               spec_mag_target[(nr - 1):2, , drop = FALSE])
    } else {  # odd wl
      spec_target_full = rbind(spec_mag_target,
                               spec_mag_target[nr:2, , drop = FALSE])
    }

    # Original phase as an initial guess / direct reconstruction
    phase_orig = Arg(spec_full_orig)

    # For real output, DC and Nyquist must have zero phase
    phase_orig[1, ] = 0
    if (wl %% 2 == 0) {
      phase_orig[nr, ] = 0
    }

    if (nIter > 0) {
      gl = guessPhase_GL(spec = spec_target_full,
                         phase = phase_orig,
                         nIter = nIter,
                         samplingRate = audio$samplingRate,
                         wl = wl,
                         step_points = step_points,
                         wn = wn,
                         verbose = FALSE,
                         plotError = FALSE)
      phase_use = gl$phase
    } else {
      phase_use = phase_orig
    }

    # Euler's formula - get complex spectrum from magnitude and phase
    spec_complex_out = spec_target_full * exp(1i * phase_use)

    # Inverse STFT
    s_new = istft_simple(spec_complex_out,
                         wl = wl,
                         step = step_points,
                         wn = wn,
                         type = 'full')

    max_s_new = max(abs(s_new))
    if (max_s_new > 0) s_new = s_new / max_s_new
  } else {
    s_new = invertSpectrogram(
      spec_filt,
      samplingRate = audio$samplingRate,
      windowLength = windowLength,
      step = step,
      wn = wn,
      specScale = specScale_inv,
      initialPhase = initialPhase,
      nIter = nIter,
      normalize = TRUE,
      play = FALSE,
      verbose = FALSE,
      plotError = FALSE
    )
  }

  if (isTRUE(play)) {
    playme(s_new, audio$samplingRate)
  } else if  (is.character(play)) {
    playme(s_new, audio$samplingRate, player = play)
  }

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }

  # save audio
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(s_new * audio$scale, audio = audio, filename = filename)
  }

  if (plot) {
    # Get an MS of the new sound in the same domain as the target MS
    spec_actual_full = stft_simple(s_new, samplingRate = audio$samplingRate,
                                   wl = wl,
                                   step = step_points, wn = wn)
    spec_actual_for_ms = floor_log(
      Mod(spec_actual_full[1:nr, , drop = FALSE]),
      takeLog = logSpec,
      dynamicRange = dynamicRange
    )
    ms_actual = specToMS(spec_actual_for_ms, center = TRUE)$ms_centered

    # Calculate the error based on target vs reconstructed MS
    ms_nr = min(nrow(ms_filt), nrow(ms_actual))
    ms_nc = min(ncol(ms_filt), ncol(ms_actual))
    m1 = abs(ms_filt[1:ms_nr, 1:ms_nc, drop = FALSE])
    m2 = abs(ms_actual[1:ms_nr, 1:ms_nc, drop = FALSE])

    max_m1 = max(m1)
    max_m2 = max(m2)
    if (max_m1 > 0) m1 = m1 / max_m1
    if (max_m2 > 0) m2 = m2 / max_m2

    denom = sum(m1 ^ 2)
    if (denom > 0) {
      err = sum((m1 - m2) ^ 2) / denom * 100
    } else {
      err = 0
    }

    par(mfrow = c(1, 3))
    on.exit(par(mfrow = c(1, 1)), add = TRUE)

    image(x = as.numeric(colnames(ms)),
          y = as.numeric(rownames(ms)),
          z = t(log(abs(ms) + 1e-10)),
          main = 'Original MS',
          xlab = '', ylab = 'FM, cycle/kHz')

    image(x = as.numeric(colnames(ms_filt)),
          y = as.numeric(rownames(ms_filt)),
          z = t(log(abs(ms_filt) + 1e-10)),
          main = 'Filtered MS',
          xlab = 'AM, Hz', ylab = '')

    image(x = as.numeric(colnames(ms_actual)),
          y = as.numeric(rownames(ms_actual)),
          z = t(log(abs(ms_actual) + 1e-10)),
          main = paste0('Achieved MS \nSquared error = ', round(err, 1), '%'),
          xlab = '', ylab = '')
  }

  invisible(s_new)
}


#' @rdname filterSoundByMS
#' @export
filterMS = function(ms,
                    amCond = NULL,
                    fmCond = NULL,
                    jointCond = NULL,
                    action = c('stop', 'pass'),
                    dB = Inf,
                    smooth = NULL,
                    protect = c('level', 'meanSpectrum'),
                    warnAsymmetric = TRUE,
                    plot = TRUE) {
  action = match.arg(action)
  ms = as.matrix(ms)
  nr = nrow(ms)
  nc = ncol(ms)
  if (nr < 1 || nc < 1)
    stop('ms must have at least one row and one column')

  am = as.numeric(colnames(ms))
  fm = as.numeric(rownames(ms))
  if (length(am) != nc || any(!is.finite(am)))
    am = seq_len(nc) - 1
  if (length(fm) != nr || any(!is.finite(fm)))
    fm = seq_len(nr) - 1

  # Convenient logical input and validation
  if (isTRUE(protect)) {
    protect = c('level', 'meanSpectrum', 'envelope')
  } else if (isFALSE(protect) || is.null(protect) || all(is.na(protect))) {
    protect = character(0)
  } else {
    protect = match.arg(protect,
                        choices = c('level', 'meanSpectrum', 'envelope'),
                        several.ok = TRUE)
  }

  # No conditions: no filtering
  hasCond = !is.null(jointCond) || !is.null(amCond) || !is.null(fmCond)
  if (!hasCond) {
    filter = matrix(1, nrow = nr, ncol = nc)
    rownames(filter) = rownames(ms)
    colnames(filter) = colnames(ms)
    out = ms
    if (plot) {
      out_plot = if (is.complex(out)) Mod(out) else abs(out)
      image(x = am,
            y = fm,
            z = t(log(out_plot + 1e-10)))
    }
    return(invisible(list(filter = filter, ms = out)))
  }

  # Set up an empty filter matrix
  if (is.finite(dB) && abs(dB) > .Machine$double.eps) {
    m = 10 ^ (-abs(dB) / 20)
  } else {
    m = 0
  }
  if (action == 'stop') {
    filter = matrix(1, nrow = nr, ncol = nc)
  } else if (action == 'pass') {
    filter = matrix(m, nrow = nr, ncol = nc)
  }
  rownames(filter) = rownames(ms)
  colnames(filter) = colnames(ms)

  # Calculate the affected region
  if (!is.null(jointCond)) {  # use only jointCond
    AM = matrix(am, nrow = nr, ncol = nc, byrow = TRUE)
    FM = matrix(fm, nrow = nr, ncol = nc, byrow = FALSE)

    affectedRegion = try(
      filterMS_evalCond(jointCond, vars = list(am = AM, fm = FM), nArgs = 2),
      silent = TRUE)
    if (inherits(affectedRegion, 'try-error'))
      stop('jointCond must be a valid condition on am and fm')

    if (length(affectedRegion) == 1)
      affectedRegion = matrix(affectedRegion, nrow = nr, ncol = nc)
    if (length(affectedRegion) != nr * nc)
      stop('jointCond must return a logical matrix of the same dimensions as ms')

    affectedRegion = matrix(as.logical(affectedRegion), nrow = nr, ncol = nc)
    affectedRegion[is.na(affectedRegion)] = FALSE

    if (action == 'stop') {
      filter[affectedRegion] = m
    } else if (action == 'pass') {
      filter[affectedRegion] = 1
    }
  } else {  # use separate conditions for am & fm
    idx_col = integer(0)
    idx_row = integer(0)

    if (!is.null(amCond)) {
      res_am = try(
        filterMS_evalCond(amCond, vars = list(am = am), nArgs = 1),
        silent = TRUE)
      if (inherits(res_am, 'try-error'))
        stop('amCond must be a valid condition on am')
      idx_col = filterMS_condToIdx(res_am, nc, 'amCond')
    }

    if (!is.null(fmCond)) {
      res_fm = try(
        filterMS_evalCond(fmCond, vars = list(fm = fm), nArgs = 1),
        silent = TRUE)
      if (inherits(res_fm, 'try-error'))
        stop('fmCond must be a valid condition on fm')
      idx_row = filterMS_condToIdx(res_fm, nr, 'fmCond')
    }

    if (action == 'stop') {
      if (length(idx_row) > 0) filter[idx_row, ] = m
      if (length(idx_col) > 0) filter[, idx_col] = m
    } else if (action == 'pass') {
      if (length(idx_row) > 0) filter[idx_row, ] = 1
      if (length(idx_col) > 0) filter[, idx_col] = 1
    }
  }

  # Locate the zero-modulation axes
  i0 = which.min(abs(fm))
  j0 = which.min(abs(am))

  # Protect the requested components before smoothing, so that the transition
  # is shaped around the protected region
  if ('envelope' %in% protect) filter[i0, ] = 1
  if ('meanSpectrum' %in% protect) filter[, j0] = 1
  if ('level' %in% protect) filter[i0, j0] = 1

  # Check symmetry before smoothing: smoothing is done in matrix-index space and
  # can introduce small edge-related asymmetries that are not user-intended.
  isSym = filterMS_checkSym(filter, fm = fm, am = am)
  if (warnAsymmetric && isFALSE(isSym)) {
    warning(paste(
      'The MS filter is not symmetric with respect to (FM, AM) = (0, 0).',
      'Asymmetric filters can produce a complex-valued inverse transform',
      'and phase-related artifacts. Consider using abs(am)/abs(fm)',
      'or otherwise symmetrizing the filter condition.'))
  }

  # Smooth the filter mask to avoid abrupt cutoffs.
  # smooth = NULL: default smoothing (~3 bins)
  # smooth = NA: no smoothing
  # smooth = c(FM, AM): smoothing in native units:
  #   - FM: cycles/kHz
  #   - AM: Hz
  doSmooth = TRUE
  smooth_bins = c(0, 0)

  if (is.null(smooth)) {
    smooth_bins = c(if (nr >= 3) 3 else 0,
                    if (nc >= 3) 3 else 0)
  } else {
    if (!isTRUE(smooth) || all(is.na(smooth))) {
      doSmooth = FALSE
    } else {
      smooth = as.numeric(smooth)
      if (length(smooth) == 1)
        stop('smooth must be NULL, NA, or a numeric vector of length 2')
      if (length(smooth) != 2)
        stop('smooth must be NULL, NA, or a numeric vector of length 2')
      if (any(is.na(smooth)) || !all(is.finite(smooth)) || any(smooth < 0))
        stop('smooth must contain non-negative finite values')

      bw_fm = filterMS_getBinWidth(fm)
      bw_am = filterMS_getBinWidth(am)

      if (smooth[1] > 0 && nr >= 3)
        smooth_bins[1] = max(3, round(smooth[1] / bw_fm))
      if (smooth[2] > 0 && nc >= 3)
        smooth_bins[2] = max(3, round(smooth[2] / bw_am))
    }
  }

  smooth_bins = pmax(0, round(smooth_bins))
  if (doSmooth && max(smooth_bins) > 0 && max(filter) > min(filter)) {
    filter = gaussianSmooth2D(filter, kernelSize = smooth_bins, action = 'blur')
    filter[filter < m] = m
    filter[filter > 1] = 1

    # If the original mask was symmetric, make sure smoothing does not break it
    if (isTRUE(isSym))
      filter = filterMS_symmetrize(filter)
  }

  # Make sure the protected components are protected after smoothing as well
  if ('envelope' %in% protect) filter[i0, ] = 1
  if ('meanSpectrum' %in% protect) filter[, j0] = 1
  if ('level' %in% protect) filter[i0, j0] = 1

  # Multiply the original ms by the prepared filter
  out = ms * filter

  if (plot) {
    out_plot = if (is.complex(out)) Mod(out) else abs(out)
    image(x = am,
          y = fm,
          z = t(log(out_plot + 1e-10)))
  }

  invisible(list(filter = filter, ms = out))
}


#' Helper: fold-based symmetry check.
#' For centered MS matrices with even dimensions, the first row/column is the
#' unpaired negative Nyquist bin and is excluded before folding.
#' @noRd
filterMS_checkSym = function(m, fm, am, tol = 1e-8) {
  # Symmetry can only be assessed if the axes look centered
  if (!(any(fm < 0) && any(fm > 0) && any(am < 0) && any(am > 0)))
    return(NA)

  rows = seq_len(nrow(m))
  cols = seq_len(ncol(m))

  if (length(rows) %% 2 == 0)
    rows = rows[-1]
  if (length(cols) %% 2 == 0)
    cols = cols[-1]

  if (length(rows) == 0 || length(cols) == 0)
    return(NA)

  sub = m[rows, cols, drop = FALSE]
  if (nrow(sub) <= 1 && ncol(sub) <= 1)
    return(TRUE)

  sub_rev = sub[nrow(sub):1, ncol(sub):1, drop = FALSE]
  diff = max(abs(sub - sub_rev), na.rm = TRUE)
  is.finite(diff) && diff <= tol
}

#' Helper: force centrosymmetry by averaging with the 180-degree folded version
#' @noRd
filterMS_symmetrize = function(m) {
  rows = seq_len(nrow(m))
  cols = seq_len(ncol(m))

  if (length(rows) %% 2 == 0)
    rows = rows[-1]
  if (length(cols) %% 2 == 0)
    cols = cols[-1]

  if (length(rows) == 0 || length(cols) == 0)
    return(m)

  sub = m[rows, cols, drop = FALSE]
  if (nrow(sub) <= 1 && ncol(sub) <= 1)
    return(m)

  sub_rev = sub[nrow(sub):1, ncol(sub):1, drop = FALSE]
  m[rows, cols] = (sub + sub_rev) / 2
  m
}

#' Evaluate a condition in a vectorized way
#' @noRd
filterMS_evalCond = function(cond, vars, nArgs = 1) {
  if (is.null(cond))
    return(NULL)

  # User-supplied function
  if (is.function(cond)) {
    if (nArgs == 2) {
      return(cond(vars$am, vars$fm))
    } else {
      return(cond(vars[[1]]))
    }
  }

  # Character string: may be a function name or an expression
  if (is.character(cond)) {
    fun = try(match.fun(cond), silent = TRUE)
    if (!inherits(fun, 'try-error') && is.function(fun)) {
      if (nArgs == 2) {
        return(fun(vars$am, vars$fm))
      } else {
        return(fun(vars[[1]]))
      }
    }
    cond = parse(text = cond)
  }

  # Expression / call
  res = eval(cond, envir = vars)
  if (is.function(res)) {
    if (nArgs == 2) {
      return(res(vars$am, vars$fm))
    } else {
      return(res(vars[[1]]))
    }
  }
  res
}

#' Convert a logical vector or a vector of indices to valid indices
#' @noRd
filterMS_condToIdx = function(res, n, what) {
  if (is.null(res))
    return(integer(0))

  if (is.logical(res)) {
    if (length(res) == 1)
      res = rep(res, n)
    if (length(res) != n)
      stop(paste(what, 'must return a logical vector of length', n))
    res[is.na(res)] = FALSE
    return(which(res))
  }

  idx = suppressWarnings(as.integer(round(res)))
  idx = idx[is.finite(idx) & idx >= 1 & idx <= n]
  unique(idx)
}

#' Estimate bin width from axis labels
#' @noRd
filterMS_getBinWidth = function(x) {
  x = sort(unique(x))
  if (length(x) < 2)
    return(1)
  d = median(diff(x))
  if (!is.finite(d) || d == 0)
    return(1)
  abs(d)
}
