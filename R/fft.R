# FFT

#' Spectrum
#'
#' \code{spectrum} computes the frequency spectrum of a sound using the Fast
#' Fourier Transform (FFT). For a smoother appearance and faster processing of
#' long sounds, \code{\link{meanSpectrum}} computes the time-averaged spectrum
#' of successive windows. See the "spec" and "meanspec" functions in the seewave
#' package for more plotting options. NB: soundgen::spectrum() masks
#' stats::spectrum().
#'
#' @inheritParams .roxygen_defaults
#' @param yScale scale of the y-axis: \code{"linear"} for linear amplitude,
#'   \code{"power"} for power spectrum, \code{"dB"} for decibels (20*log10),
#'   \code{"max0"} for dB with the maximum set to 0 dB
#' @param plot if TRUE, plots the spectrum
#' @param xlab,ylab,main graphical parameters for plotting
#' @param ... other graphical parameters passed to \code{plot()}
#'
#' @return A dataframe with two columns: \code{freq} (frequency in kHz) and
#'   \code{ampl} (amplitude, in units determined by \code{yScale}).
#' @export
#' @examples
#' # 500 Hz tone
#' sound = cos(2 * pi * 500 * (1:4000) / 16000) + rnorm(4000, 0, .05)
#'
#' # Spectrum on linear scale
#' spectrum(sound, samplingRate = 16000, yScale = 'linear')
#' meanSpectrum(sound, samplingRate = 16000, yScale = 'linear')
#'
#' # dB scale with custom labels
#' spectrum(sound, samplingRate = 16000, yScale = 'dB', col = 'blue',
#'          xlab = 'Frequency (kHz)', ylab = 'Amplitude (dB)')
#'
#' # max0 scale with custom y-limits and extra graphical pars
#' meanSpectrum(sound, samplingRate = 16000, yScale = 'max0',
#'   xlim = c(0, 2), ylim = c(-50, 5), lty = 2, lwd = 3, col = 'blue')
#'
#' # Return data without plotting
#' ms = meanSpectrum(sound, samplingRate = 16000, plot = FALSE)
#' head(ms)
#'
#' # If windowLength is longer than the sound, meanSpectrum() = spectrum()
#' spectrum(sound, 16000)
#' meanSpectrum(sound, 16000, windowLength = 5000)
#'
#' \dontrun{
#' # Process all .wav files in a folder
#' spectrum('~/Downloads/temp', savePlots = TRUE, yScale = 'dB')
#' }
spectrum = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    zp = NULL,
    yScale = c('linear', 'power', 'dB', 'max0'),
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    reportEvery = NULL,
    cores = 1,
    ...
) {
  yScale = match.arg(yScale)

  # collect all arguments that will be passed to .spectrum
  myPars = c(as.list(environment()), list(...))
  # exclude arguments that are only used by the top-level dispatcher
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to',
    'reportEvery', 'cores', 'savePlots', 'embed'
  )]

  # call the internal workhorse through processAudio
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.spectrum',
    suffix = 'spectrum',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # optional HTML wrapper for multiple saved plots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))
  }

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Spectrum per sound
#' @noRd
.spectrum = function(
    audio,
    zp = NULL,
    yScale = 'linear',
    plot = FALSE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  x = audio$sound
  samplingRate = audio$samplingRate
  n = if (is.null(audio$ls)) length(x) else audio$ls

  # zero‑padding (number of zeros to add)
  if (is.null(zp)) {
    zp = nextn(n)
    if (zp > n) {
      x = c(x, rep(0, zp - n))
      n = zp
    }
  }

  # FFT
  spec_complex = fft(x)
  # positive frequencies only
  n_pos = n %/% 2 + 1
  ampl = Mod(spec_complex[1:n_pos])
  bin_width = samplingRate / n
  freq_kHz = (0:(n_pos - 1)) * bin_width / 1000

  # rescale amplitude if needed
  if (yScale == 'power') {
    ampl = ampl^2
  } else if (yScale == 'dB') {
    # avoid log(0) with pmax(ampl, .Machine$double.eps)
    ampl = 20 * log10(pmax(ampl, .Machine$double.eps))
  } else if (yScale == 'max0') {
    ampl = 20 * log10(pmax(ampl, .Machine$double.eps))
    ampl = ampl - max(ampl)
  }
  spec_df = data.frame(freq = freq_kHz, ampl = ampl)
  rownames(spec_df) = NULL

  # plotting
  # open file device when saving
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }

  if (plot) {
    # title
    if (is.null(main)) {
      main = if (audio$filename_noExt == 'sound') '' else audio$filename_noExt
    }

    # axis labels
    if (is.null(xlab)) {
      xlab = if (!is.null(audio$samplingRate)) 'Frequency, kHz' else 'Frequency, bins'
    }
    if (is.null(ylab)) {
      ylab = if (yScale %in% c('dB', 'max0')) 'dB' else ''
    }

    plot(spec_df, type = 'l', main = main, xlab = xlab, ylab = ylab, ...)
  }

  invisible(spec_df)
}



#' @rdname spectrum
#' @export
meanSpectrum = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    windowLength = 50,
    step = windowLength / 2,
    overlap = NULL,
    wn = 'gaussian',
    zp = NULL,
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    yScale = c('linear', 'power', 'dB', 'max0'),
    savePlots = FALSE,
    embed = FALSE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  yScale = match.arg(yScale)
  # collect all arguments that will be passed to .meanSpectrum
  myPars = c(as.list(environment()), list(...))
  # exclude arguments that are only used by the top-level dispatcher
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to',
    'reportEvery', 'cores', 'savePlots', 'embed'
  )]

  # call the internal workhorse through processAudio
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.meanSpectrum',
    suffix = 'meanSpectrum',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # optional HTML wrapper for multiple saved plots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))
  }

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Mean spectrum per sound
#' @noRd
.meanSpectrum = function(
    audio,
    windowLength = 50,
    step = windowLength / 2,
    overlap = NULL,
    wn = 'gaussian',
    zp = NULL,
    plot = FALSE,
    yScale = 'linear',
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  if (is.null(step)) {
    if (is.null(overlap)) {
      stop('Need to specify either step or overlap')
    } else {
      step = windowLength * (1 - overlap / 100)
    }
  }
  step_points = max(1, round(step / 1000 * audio$samplingRate))
  wl = round(windowLength / 1000 * audio$samplingRate)
  if (is.null(zp)) {
    zp = nextn(wl) # 2^ceiling(log2(wl))
    if (zp == wl) zp = 0
  }

  # compute the spectrogram and take column means of the magnitude
  n = max(wl, zp)
  if (n >= length(audio$sound)) {
    meanspec = .spectrum(audio, yScale = yScale, plot = FALSE)
  } else {
    spec = stft_simple(
      audio$sound,
      samplingRate = audio$samplingRate,
      wl = wl,
      step = step_points,
      wn = wn,
      zp = zp
    )[1:(n %/% 2 + 1), , drop = FALSE]

    # rescale amplitude if needed
    freq = as.numeric(rownames(spec))
    if (yScale == 'linear') {
      meanspec = data.frame(freq = freq, ampl = rowMeans(Mod(spec)))
    } else if (yScale == 'power') {
      spec_power = Re(spec * Conj(spec))  # or Mod()^2
      meanspec = data.frame(freq = freq, ampl = rowMeans(spec_power))
    } else if (yScale == 'dB') {
      ampl = pmax(rowMeans(Mod(spec)), .Machine$double.eps)
      meanspec = data.frame(freq = freq, ampl = 20 * log10(ampl))
    } else if (yScale == 'max0') {
      ampl = pmax(rowMeans(Mod(spec)), .Machine$double.eps)
      ampl_dB = 20 * log10(ampl)
      meanspec = data.frame(freq = freq, ampl = ampl_dB - max(ampl_dB))
    }
    rownames(meanspec) = NULL
  }

  # plotting
  # open file device when saving
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }

  if (plot) {
    # title
    if (is.null(main)) {
      main = if (audio$filename_noExt == 'sound') '' else audio$filename_noExt
    }

    # axis labels
    if (is.null(xlab)) {
      xlab = if (!is.null(audio$samplingRate)) 'Frequency, kHz' else 'Frequency, bins'
    }
    if (is.null(ylab)) {
      ylab = if (yScale %in% c('dB', 'max0')) 'dB' else ''
    }

    plot(meanspec, type = 'l', main = main, xlab = xlab, ylab = ylab, ...)
  }

  # return the data frame when asked
  invisible(meanspec)
}


#' Get spectral envelope
#'
#' Calculates a smoothed envelope of a magnitude spectrum or of each column of
#' a spectrogram. This is good for removing the fine structure produced by
#' harmonics of f0 and leaving only the overall spectral contour produced by
#' resonances (formants). This is the source-filter separation step used by
#' \code{\link{shiftFormants}}. All methods except "peak" smooth the
#' log-magnitude spectrum and return the envelope on the original linear scale,
#' with the same dimensions as the input.
#'
#' The amount of smoothing is controlled by \code{freqWindow_bins}, which should
#' normally equal the expected spacing between harmonics (f0, in bins): spectral
#' details that vary on a faster scale are treated as source fine structure and
#' removed, while slower variations (formants) are retained. For high-pitched or
#' variable calls, increase \code{freqWindow_bins} to smooth more. Methods:
#' \itemize{
#' \item "cepstral" (default): Gaussian low-pass liftering of the real cepstrum
#'   (FFT of the log spectrum). The harmonic ripple, which has a period of
#'   \code{freqWindow_bins} bins, is attenuated to ~exp(-4) = 2% of its original
#'   amplitude, while the formant envelope is preserved. By the convolution
#'   theorem, this is equivalent to Gaussian smoothing of the log spectrum with
#'   SD = \code{freqWindow_bins} / 2, but computed in the quefrency domain.
#' \item "gauss": Gaussian blur of the log spectrum along the frequency axis
#'   with SD = \code{freqWindow_bins} / 2 - the same low-pass filter as
#'   "cepstral", but with explicit edge padding instead of circular (wrap-around)
#'   filtering.
#' \item "movavg": moving average of the log spectrum with a rectangular window of
#'   width \code{freqWindow_bins} (rounded up to an odd number).
#' \item "peak": moving maximum (morphological dilation), i.e. the upper
#'   envelope of the spectrum with a window of width \code{freqWindow_bins}.
#'   Much slower than the other methods on long inputs.
#' }
#' The edges of the spectrum are handled by padding (repeating the edge values)
#' before smoothing and trimming afterwards, so the output always has the same
#' length as the input. If \code{spec} is a matrix (rows = frequency bins,
#' columns = time frames), all frames are smoothed at once.
#'
#' @param spec numeric vector (magnitude spectrum of one frame) or matrix (rows
#'   = frequency bins, columns = time frames), such as a spectrogram returned by
#'   \code{\link{stft_simple}} or \code{\link{spectrogram}}. Must have at least
#'   3 rows (frequency bins) and only finite values; zeros and negative values
#'   are floored at 1e-10 before taking the log. The input spectrum is expected
#'   to be on a linear, not logarithmic (dB) scale
#' @param freqs frequency labels corresponding to \code{spec}, in kHz (not Hz!);
#'   not needed if \code{freqWindow_bins} is provided
#' @param freqWindow,freqWindow_bins the width of the smoothing window, in Hz
#'   (not kHz!) or frequency bins (>0): for example, if we are trying to smooth
#'   away the harmonics of f0 and leave only formants, \code{freqWindow} must
#'   exceed the expected spacing between harmonics. Larger values produce
#'   smoother envelopes. If \code{freqWindow_bins} is provided, it overrides
#'   \code{freqWindow}
#' @param method the method of smoothing: "cepstral" = Gaussian liftering of
#'   the real cepstrum (default), "gauss" = Gaussian blur of the log spectrum,
#'   "movavg" = moving average of the log spectrum, "peak" = moving maximum
#'   (upper envelope)
#' @param plot if TRUE, produces a simple plot of the original spectrum and the
#'   extracted envelope
#'
#' @return The spectral envelope on the original (linear magnitude) scale, as a
#'   numeric vector or matrix with the same dimensions as \code{spec}.
#' @seealso \code{\link{getEnv}} for the temporal envelope of a waveform;
#'   \code{\link{shiftFormants}}, which uses \code{getSpecEnv} to shift formants
#' @export
#' @examples
#' # Synthetic spectrum: three formants plus harmonics 20 bins apart
#' N = 512
#' freq = 1:N
#' true_envelope = exp(-.5 * ((freq - 100) / 20)^2) +
#'   exp(-.5 * ((freq - 250) / 30)^2) +
#'   exp(-.5 * ((freq - 400) / 40)^2)
#' spectrum = true_envelope * (0.5 + 0.5 * abs(sin(pi * freq / 20)))
#'
#' plot(freq, spectrum, type = 'l', log = 'y',
#'      main = 'Spectral envelope', xlab = 'Frequency, bins')
#' lines(freq, true_envelope, col = 'grey60', lwd = 4)
#' lines(getSpecEnv(spectrum, freqWindow_bins = 20, method = 'cepstral'),
#'       col = 'red', lwd = 2)
#' lines(getSpecEnv(spectrum, freqWindow_bins = 20, method = 'gauss'),
#'       col = 'orange', lwd = 2, lty = 2)
#' lines(getSpecEnv(spectrum, freqWindow_bins = 20, method = 'movavg'),
#'       col = 'blue', lwd = 2, lty = 3)
#' lines(getSpecEnv(spectrum, freqWindow_bins = 20, method = 'peak'),
#'       col = 'green', lwd = 2, lty = 4)
#' legend('bottom',
#'        legend = c('raw', 'truth', 'cepstral', 'gauss', 'movavg', 'peak'),
#'        col = c('black', 'grey60', 'red', 'orange', 'blue', 'green'),
#'        lwd = c(1, 4, 2, 2, 2, 2), lty = c(1, 1, 1, 2, 3, 4), bty = 'n')
#'
#' # Smoothed spectral envelope of a single vowel
#' data(speechEx, package = 'soundgen')
#' spec = spectrum(speechEx, from = .15, to = .3, plot = FALSE)
#' env = getSpecEnv(spec, freqWindow = 500, plot = TRUE)
#'
#' # Smooth a whole spectrogram at once (matrix input):
#' spec = stft_simple(speechEx@left[1:16000],
#'                    samplingRate = speechEx@samp.rate,
#'                    wl = 512, step = 256)
#' spec = Mod(spec[1:(nrow(spec) %/% 2 + 1), ])
#' env = getSpecEnv(spec, freqWindow = 500, plot = TRUE)
getSpecEnv = function(spec,
                      freqs = NULL,
                      freqWindow = NULL,
                      freqWindow_bins = NULL,
                      method = c('cepstral', 'gauss', 'movavg', 'peak'),
                      plot = FALSE) {
  method = match.arg(method)
  if (is.list(spec)) {
    if (!is.null(spec$freq) && !is.null(spec$ampl)) {
      freqs = spec$freq
      spec = as.matrix(spec$ampl)
    } else {
      stop('if "spec" is a list or dataframe, it must contain "freq" and "ampl"')
    }
  } else {
    spec = as.matrix(spec)  # columns = frames; vectorized for speed in shiftFormants
  }
  N = nrow(spec)
  if (N < 3) stop('Spectrum must have at least 3 frequency bins')

  if (is.null(freqs)) {
    freqs = as.numeric(rownames(spec))
  }
  if (is.null(freqWindow_bins)) {
    if (is.null(freqWindow) || is.null(freqs)) {
      stop('You must provide either freqWindow_bins or both freqWindow and freqs')
    } else {
      freq_step = median(diff(freqs))
      freqWindow_bins = max(1, round(freqWindow / freq_step / 1000))
    }
  }
  P = max(1, round(freqWindow_bins))  # expected harmonic spacing, bins
  log_spec = log(pmax(spec, 1e-10)) # avoid log(0)

  if (method == 'cepstral') {
    # pad edges: the log-spectrum is highly non-periodic (steep tilt at both
    # ends), and wrap-around would otherwise ring across the whole curve
    pad = P
    log_spec_pad = rbind(log_spec[rep(1, pad), , drop = FALSE],
                         log_spec,
                         log_spec[rep(N, pad), , drop = FALSE])
    M = nrow(log_spec_pad)

    # Gaussian low-pass lifter in the quefrency domain. The harmonic ripple
    # (period = P bins) lives at quefrency M / P; a lifter width of
    # w = M / (2 * P) attenuates it by exp(-4) ~ 2% while preserving the
    # slower-varying formant envelope. By the convolution theorem this equals
    # Gaussian smoothing of the log-spectrum with SD = P / 2 bins.
    w = M / (2 * P)
    q = pmin(0:(M - 1), M - 0:(M - 1))  # circular quefrency, bins
    lifter = matrix(exp(-(q / w) ^ 2), nrow = M, ncol = ncol(log_spec_pad))

    cep = mvfft(log_spec_pad) * lifter
    smooth_log = Re(mvfft(cep, inverse = TRUE)) / M
    env = exp(smooth_log[(pad + 1):(pad + N), , drop = FALSE])
  } else if (method == 'gauss') {
    # blur the log-spectrogram along the frequency axis (the dual of the
    # cepstral lifter, but with explicit edge handling instead of wrap-around)
    sigma = max(P / 2, .5)
    half = ceiling(3 * sigma)
    k = exp(-.5 * ((-half:half) / sigma) ^ 2)
    k = k / sum(k)

    log_spec_pad = rbind(log_spec[rep(1, half), , drop = FALSE],
                         log_spec,
                         log_spec[rep(N, half), , drop = FALSE])
    smooth_log = as.matrix(filter(log_spec_pad, filter = k, sides = 2))
    env = exp(smooth_log[(half + 1):(half + N), , drop = FALSE])
  } else if (method == 'movavg') {
    w = max(3, P)
    if (w %% 2 == 0) w = w + 1  # odd window for perfect symmetry
    pad = w %/% 2
    log_spec_pad = rbind(log_spec[rep(1, pad), , drop = FALSE],
                         log_spec,
                         log_spec[rep(N, pad), , drop = FALSE])
    smooth_log = as.matrix(filter(log_spec_pad, filter = rep(1 / w, w), sides = 2))
    env = exp(smooth_log[(pad + 1):(pad + N), , drop = FALSE])
  } else if (method == 'peak') {
    w = max(1, P)
    if (w %% 2 == 0) w = w + 1
    half_w = w %/% 2
    # rolling maximum = upper (peak) envelope
    get_rolling_max = function(v, half_w) {
      n = length(v)
      sapply(1:n, function(i) max(v[max(1, i - half_w) : min(n, i + half_w)]))
    }
    env = apply(spec, 2, get_rolling_max, half_w = half_w)
    if (!is.matrix(env)) env = matrix(env, ncol = 1)
  }

  if (!is.null(freqs)) rownames(env) = freqs

  if (plot) {
    if (is.null(freqs)) {
      freqs = 1:length(env)
      freq_lab = 'Frequency, scale unknown'
    } else {
      freq_lab = 'Frequency, kHz'
    }
    if (ncol(spec) == 1) {
      plot(freqs, spec[, 1], type = 'l', log = 'y',
           xlab = freq_lab, ylab = 'Log-magnitude')
      lines(freqs, env, col = 'blue', lwd = 3)
    } else if (ncol(spec) > 1) {
      filled.contour.mod(
        x = as.numeric(colnames(env)),
        y = as.numeric(rownames(env)),
        z = t(log(env)),
        main = 'Smoothed spectral envelope',
        xlab = 'Time, ms', ylab = freq_lab)
    }
  }

  if (ncol(spec) == 1) env = env[, 1]  # matrix to numeric preserving rownames
  env
}


#' Autocorrelation with FFT
#'
#' Analogous to \code{\link[stats]{acf}}, but based on the Fast Fourier
#' Transform (Wiener-Khinchin theorem) and 5-10 times faster, especially for
#' long input vectors.
#'
#' @param x numeric vector
#' @param center if TRUE (default), x is centered before padding with 0
#' @param padToMult pad with 0 to the smallest power of 2 above
#'   \code{padToMult * length(x) - 1}, must be >=1 (padToMult = 1 means no
#'   padding, leading to circular ACF)
#' @return Numeric vector that is usually longer than input because it is padded
#'   with zeros to the next power of two. Constant and zero inputs return NA.
#' @export
#' @examples
#' len = 200
#' x = sin(2 * pi * 100 * (1:len) / 1000) + rnorm(len, 0, .5)
#' plot(x, type = 'l')
#' aut = acf(x, lag.max = len/2)
#' aut2 = acf_fft(x)
#' points(0:100, aut2[1:(len/2+1)], type = 'l', col = 'blue')
#' aut$acf[1:10]
#' aut2[1:10]
#'
#' # compare execution time
#' system.time(for (i in 1:100) acf(x, lag.max = len/2, plot = FALSE))
#' system.time(for (i in 1:100) acf_fft(x))
acf_fft = function(x, center = TRUE, padToMult = 2) {
  if (is.null(x) || any(!is.finite(x))) return(NA)
  if (center) x = x - mean(x)
  if (!any(x != 0)) return(NA_real_)  # check after centering
  len = length(x)

  # pad with 0 to 2^integer
  if (padToMult > 1) {
    n = max(len, 2^ceiling(log2(padToMult * len - 1)))
    x_padded = c(x, rep(0, n - len))
  } else {
    x_padded = x
    n = len
  }
  spectrum = fft(x_padded)
  power_spectrum = Re(spectrum * Conj(spectrum))
  # same as Mod(spectrum^2), but faster
  autocor = Re(fft(power_spectrum, inverse = TRUE))
  # / n - not needed as we normalize anyway
  if (!is.finite(autocor[1]) || autocor[1] == 0) {
    NA
  } else {
    autocor / autocor[1]
  }
}


#' Hilbert transforms
#'
#' \code{hilbert_exact} treats the input as one period of an infinite periodic
#' signal (circular convolution), while \code{hilbert_approx} pads the input on
#' both sides to a good length for FFT, which is faster at the cost of possible
#' slight artifacts at the edges (~linear convolution). If the input is <3
#' samples long, the envelope is calculated simply as \code{Mod(x)}. Note: only
#' \code{hilbert_approx} is fast enough to be used for extracting the envelopes
#' of typical audio.
#' @param x numeric vector
#' @return A list with two components: \describe{\item{hilbert}{Hilbert
#'   transform} \item{envelope}{Instantaneous amplitude of the analytic signal
#'   (envelope)}}
#' @export
#' @examples
#' # signal: amplitude-modulated sine wave
#' t = seq(0, 1, length.out = 477)
#' carrier = cos(2 * pi * 50 * t)
#' modulator = 1 + 0.5 * cos(2 * pi * 5 * t)
#' s = modulator * carrier  # amplitude modulated signal
#' hil_exact = hilbert_exact(s)
#' hil_approx = hilbert_approx(s)
#' plot(s, type = 'l')
#' points(hil_exact$envelope, type = 'l', col = 'blue')
#' points(hil_approx$envelope, type = 'l', col = 'red')
hilbert_exact = function(x) {
  N = length(x)
  if (N < 3) return(list(hilbert = rep(0, N), envelope = abs(x)))
  X = fft(x)

  # Build the multiplier -i * sign(ω) with 0 at DC and Nyquist
  h = rep(0i, N)
  if (N %% 2 == 0) {
    # even length
    h[2:(N/2)] = -1i  # positive frequencies
    h[(N/2 + 2):N] =  1i  # negative frequencies
    # DC (1) and Nyquist (N/2 + 1) stay 0
  } else {
    # odd length
    mid = (N + 1) / 2
    h[2:mid]  = -1i
    h[(mid + 1):N] = 1i
  }

  y = Re(fft(X * h, inverse = TRUE)) / N  # Im is just rounding noise
  envelope = sqrt(x^2 + y^2)
  return(list(hilbert = y, envelope = envelope))
}

#' @rdname hilbert_exact
#' @export
hilbert_approx = function(x) {
  N0 = length(x)
  if (N0 < 3) return(list(hilbert = rep(0, N0), envelope = abs(x)))
  N = nextn(N0) # 2^(ceiling(log2(N0)))
  if (N > N0) {
    pad_left = (N - N0) %/% 2
    pad_right = N - N0 - pad_left
    x2 = c(rep(0, pad_left), x, rep(0, pad_right))
    X = fft(x2)
  } else {
    N = N0
    X = fft(x)
  }

  # Build the multiplier -i * sign(ω) with 0 at DC and Nyquist
  h = rep(0i, N)
  if (N %% 2 == 0) {
    # even length
    h[2:(N/2)] = -1i  # positive frequencies
    h[(N/2 + 2):N] =  1i  # negative frequencies
    # DC (1) and Nyquist (N/2 + 1) stay 0
  } else {
    # odd length
    mid = (N + 1) / 2
    h[2:mid]  = -1i
    h[(mid + 1):N] = 1i
  }

  if (N > N0) {
    y = (Re(fft(X * h, inverse = TRUE)) / N) [(pad_left + 1):(N - pad_right)]
  } else {
    y = Re(fft(X * h, inverse = TRUE)) / N
  }
  envelope = sqrt(x^2 + y^2)
  list(hilbert = y, envelope = envelope)
}
