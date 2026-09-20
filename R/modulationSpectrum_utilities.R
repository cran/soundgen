#' Plot modulation spectrum
#'
#' Plots a single modulation spectrum returned by
#' \code{\link{modulationSpectrum}}. The result is the same as the plot produced
#' by \code{\link{modulationSpectrum}}, but calling \code{plotMS} is handy for
#' processed modulation spectra - for instance, for plotting the difference
#' between the modulation spectra of two sounds or groups of sounds.
#'
#' @inheritParams modulationSpectrum
#' @param ms modulation spectrum - a matrix with temporal modulation in columns
#'   and spectral modulation in rows, as returned by
#'   \code{\link{modulationSpectrum}}
#' @param X,Y colnames and rownames of \code{ms}, respectively
#' @param audio (internal) a list of audio attributes
#' @param extraY if TRUE, another Y-axis is plotted on the right showing 1 /
#'   (Y-axis value) as a frequency in Hz (assuming that ms is derived from an
#'   ordinary STFT with linearly spaced frequency bins)
#' @return Does not return anything, just plots the MS.
#' @export
#' @examples
#' ms1 = modulationSpectrum(runif(4000), samplingRate = 16000, plot = TRUE)
#' plotMS(ms1$detailed$processed)  # identical to above
#'
#' # compare two modulation spectra
#' ms2 = modulationSpectrum(soundgen(sylLen = 100, addSilence = 0),
#'                          samplingRate = 16000)
#' # ensure the two matrices have the same dimensions
#' ms2_resized = soundgen:::interpolMatrix(ms2$detailed$original,
#'   nr = nrow(ms1$detailed$original), nc = ncol(ms1$detailed$original))
#' # plot the difference
#' plotMS(log(ms1$detailed$original / ms2_resized), quantiles = NULL,
#'   col = colorRampPalette(c('blue', 'yellow')) (50))
plotMS = function(
    ms,
    X = as.numeric(colnames(ms)),
    Y = as.numeric(rownames(ms)),
    quantiles = c(.5, .8, .9),
    colorTheme = 'bw',
    col = NULL,
    logWarpX = NULL,
    logWarpY = NULL,
    main = NULL,
    xlab = 'Hz',
    ylab = '1/kHz',
    xlim = NULL,
    ylim = NULL,
    audio = NULL,
    extraY = TRUE,
    ...
) {
  if (!is.null(col)) colorTheme = NULL
  if (!is.null(colorTheme)) {
    color.palette = switchColorTheme(colorTheme)
  } else {
    color.palette = NULL
  }
  if (is.null(col)) col = color.palette(30)
  if (is.null(xlim)) xlim = range(X)
  if (is.null(ylim)) ylim = range(Y)
  if (is.null(main) && !is.null(audio$filename_noExt)) {
    if (audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
  }

  # plot with filled.contour.mod
  # ms = zeroOne(ms)
  X1 = X
  Y1 = Y
  if (is.numeric(logWarpX) || is.numeric(logWarpY)) {
    if (is.numeric(logWarpX)) {
      X1 = pseudoLog(X, sigma = logWarpX[1], base = logWarpX[2])
      # lab_x = pretty(X, n = 9, min.n = 7)
      # at_x = pseudoLog(lab_x, sigma = logWarpX[1], base = logWarpX[2])
      at_x = pretty(X1, n = 7)
      lab_x = round(pseudoLog_undo(at_x, sigma = logWarpX[1], base = logWarpX[2]))
      if (!is.null(xlim)) xlim = pseudoLog(xlim, sigma = logWarpX[1], base = logWarpX[2])
    } else {
      lab_x = at_x = formatC(pretty(X))  # formatC prevents extra decimals after as.character(pretty(...))
    }

    if (is.numeric(logWarpY)) {
      Y1 = pseudoLog(Y, sigma = logWarpY[1], base = logWarpY[2])
      lab_y = formatC(pretty(Y))
      at_y = pseudoLog(lab_y, sigma = logWarpY[1], base = logWarpY[2])
      if (!is.null(ylim)) ylim = pseudoLog(ylim, sigma = logWarpY[1], base = logWarpY[2])
    } else {
      lab_y = at_y = formatC(pretty(Y))
    }
    filled.contour.mod(X1, Y1, t(ms),
                       col = col,
                       xlab = xlab, ylab = ylab,
                       bty = 'n',
                       main = main, xaxt = 'n', yaxt = 'n',
                       xlim = xlim, ylim = ylim,
                       axisX = FALSE, axisY = FALSE,
                       y_Hz = FALSE,
                       ...)

    # add nicely labeled axes
    axis(1, at = at_x, labels = lab_x, ...)  # xpd = TRUE to extend beyond the plot
    axis(2, at = at_y, labels = lab_y, ...)
    if (extraY) {
      lbl_Hz_pos = pretty(Y1)
      lbl_Hz_pos = lbl_Hz_pos[lbl_Hz_pos > 0]
      lbls_Hz = round(1000 / lbl_Hz_pos)
      axis(4, at = lbl_Hz_pos, labels = lbls_Hz, ...)
    }
  } else {
    # no log-warping
    filled.contour.mod(X1, Y1, t(ms),
                       col = col,
                       xlab = xlab, ylab = ylab,
                       bty = 'n',
                       main = main,
                       xlim = xlim, ylim = ylim,
                       y_Hz = FALSE,
                       ...)
    if (extraY) {
      py = pretty(Y1); py = py[py > 0]
      lbls = round(1000 / py)
      lbl_pos = 1000 / lbls
      axis(4, at = lbl_pos, labels = lbls, ...)
    }
  }
  abline(v = 0, lty = 3)

  # add contours
  if (is.numeric(quantiles)) {
    # qntls = quantile(out_aggreg, probs = quantiles)  # could try HDI instead
    qntls = pDistr(as.numeric(ms), quantiles = quantiles)
    par(new = TRUE)
    contour(x = X1, y = Y1, z = t(ms),
            levels = qntls, labels = quantiles * 100,
            xaxs = 'i', yaxs = 'i',
            axes = FALSE, frame.plot = FALSE,
            xlim = xlim, ylim = ylim,
            ...)
    par(new = FALSE)
  }
}


#' Spectrogram to modulation spectrum and back
#'
#' \code{specToMS()} takes a spectrogram (complex or magnitude) and returns its
#' 2D Fourier transform - the modulation spectrum (MS). \code{msToSpec()} is the
#' inverse: it takes a full, complex modulation spectrum and reconstructs the
#' magnitude spectrogram.
#'
#' Both functions assume that the input carries valid labels: a spectrogram must
#' have frequency labels (kHz) in its rownames and time labels (ms) in its
#' colnames, and a modulation spectrum must have spectral modulation labels in
#' its rownames and temporal modulation labels in its colnames. This is the case
#' for objects produced by soundgen functions (e.g. \code{\link{spectrogram}},
#' \code{\link{stft_simple}}, \code{\link{modulationSpectrum}}), and it also
#' ensures that zero-padding is reflected correctly in the labels.
#'
#' @seealso \code{\link{modulationSpectrum}}, \code{\link{plotMS}}
#'
#' @param spec target spectrogram (numeric matrix, frequency in rows, time in
#'   columns), with frequency labels (kHz) in rownames and time labels (ms) in
#'   colnames
#' @param ms centered modulation spectrum (complex matrix, spectral modulation
#'   in rows, temporal modulation in columns), as returned by \code{specToMS()}
#' @param center if TRUE, the MS is centered with DC in the middle
#'
#' @return \code{specToMS()} returns a list with \code{$ms} (the uncentered MS)
#'   and \code{$ms_centered} (the centered MS, or NULL if \code{center = FALSE}).
#'   Both are complex matrices of the same dimension as \code{spec}, with
#'   spectral modulation (FM) in rows and temporal modulation (AM) in columns.
#'   \code{msToSpec()} returns a magnitude spectrogram (real matrix, frequency in
#'   rows, time in columns).
#'
#' @export
#' @examples
#' s = soundgen(sylLen = 200, amFreq = 25, amDep = 50, pitch = 250,
#'              addSilence = 0, samplingRate = 16000)
#' spec = spectrogram(s, samplingRate = 16000, windowLength = 25, step = 5,
#'   plot = FALSE)
#' image(t(spec))
#' ms = specToMS(spec)
#' plotMS(log(Mod(ms$ms_centered)), quantiles = NULL)
#' # round-trip: reconstruct the magnitude spectrogram
#' spec_new = msToSpec(ms$ms_centered)
#' image(t(spec_new))
specToMS = function(spec, center = TRUE) {
  # minimal validity checks: labels are assumed valid (from soundgen functions)
  if (is.null(rownames(spec)) || is.null(colnames(spec)) ||
      nrow(spec) < 2 || ncol(spec) < 2) {
    stop('spec must have rownames and colnames and at least 2 rows/columns')
  }
  nr = nrow(spec)
  nc = ncol(spec)

  # 2D FFT (unnormalized, matching R's fft convention)
  ms = fft(spec)

  # AM labels from time colnames (ms)
  step = diff(as.numeric(colnames(spec)[1:2]))  # ms
  # FM labels from frequency rownames (kHz)
  bin_width_kHz = diff(as.numeric(rownames(spec)[1:2]))  # kHz (assuming spec is STFT)
  if (!is.finite(step) || step == 0 ||
      !is.finite(bin_width_kHz) || bin_width_kHz == 0) {
    stop('spec must have evenly spaced numeric colnames (time) and rownames (frequency)')
  }

  samplingRate_am = 1000 / step  # Hz
  bin_width_am = samplingRate_am / nc  # Hz
  names_am = (0:(nc - 1)) * bin_width_am

  bin_width = bin_width_kHz * 1000  # Hz
  samplingRate_fm = 1000 / bin_width  # bins per kHz
  bin_width_fm = samplingRate_fm / nr  # cycles/kHz
  names_fm = (0:(nr - 1)) * bin_width_fm

  rownames(ms) = names_fm
  colnames(ms) = names_am

  ms_centered = if (center) centerMS(ms) else NULL
  list(ms = ms, ms_centered = ms_centered)
}


#' @rdname specToMS
#' @export
msToSpec = function(ms) {
  # minimal validity checks: labels are assumed valid (from soundgen functions)
  if (is.null(rownames(ms)) || is.null(colnames(ms)) ||
      nrow(ms) < 2 || ncol(ms) < 2) {
    stop('ms must have rownames and colnames and at least 2 rows/columns')
  }
  nr = nrow(ms)
  nc = ncol(ms)

  # Inverse FFT (divide by nr*nc because R's fft is unnormalized).
  # Taking Mod means it doesn't matter whether the ms is centered or not.
  s1 = Mod(fft(ms, inverse = TRUE) / (nr * nc))

  # AM labels -> time stamps (ms)
  bin_width_am = diff(as.numeric(colnames(ms))[1:2])  # Hz
  # FM labels -> window length (ms)
  bin_width_fm = diff(as.numeric(rownames(ms))[1:2])  # cycles/kHz
  if (!is.finite(bin_width_am) || bin_width_am == 0 ||
      !is.finite(bin_width_fm) || bin_width_fm == 0) {
    stop('ms must have evenly spaced numeric colnames (AM) and rownames (FM)')
  }

  samplingRate_am = bin_width_am * nc  # Hz
  step = 1000 / samplingRate_am  # ms
  windowLength = bin_width_fm * nr  # ms

  colnames(s1) = windowLength / 2 + (0:(ncol(s1) - 1)) * step
  bin_width = 1 / windowLength  # kHz
  rownames(s1) = (0:(nrow(s1) - 1)) * bin_width

  s1
}


#' Spectrogram to modulation spectrum 1D
#'
#' Takes a spectrogram and returns the spectrum of each channel. The input can
#' be an ordinary STFT spectrogram or an auditory spectrogram (a signal
#' convolved with a bank of bandpass filters). The difference from
#' \code{\link{specToMS}} is that, instead of taking a two-dimensional transform
#' of the spectrogram, here the spectra are calculated independently for each
#' frequency bin.
#'
#' @param spec input spectrogram (numeric matrix with frequency in rows and time
#'   in columns)
#' @param samplingRate for auditory spectrogram, the sampling rate of input
#'   audio; for STFT spectrograms, the number of STFT frames per second
#' @param windowLength,step determine the resolution of modulation spectra (both
#'   in ms); this only has an effect if \code{specMethod = "meanSpectrum"}
#' @param specMethod calls either \code{\link{meanSpectrum}} (faster) or
#'   \code{\link{spectrum}} (higher resolution)
#'
#' @return A modulation spectrum - a matrix of real values, with center
#'   frequencies of original filters in rows (kHz) and modulation frequencies in
#'   columns (Hz).
#'
#' @export
#' @examples
#' data(speechEx, package = 'soundgen')
#'
#' # auditory spectrogram
#' as = audSpectrogram(speechEx, from = 0, to = 1,
#'   envelope = 'hil', nFilters = 24, step = NULL, plot = FALSE)
#' ms = specToMS_1D(as$filterbank_env, samplingRate = speechEx@samp.rate)
#' plotMS(log(ms+.01), logWarpX = c(10, 2), quantiles = NULL, ylab = 'kHz')
#'
#' # ordinary STFT spectrogram
#' sp = spectrogram(speechEx, from = 0, to = 1, windowLength = 15, step = 0.5,
#'   output = 'original', plot = FALSE)
#' ms2 = specToMS_1D(sp, 1000 / 0.5)  # 1000/0.5 frames per s
#' plotMS(log(ms2+.01), quantiles = NULL, ylab = 'kHz')
#'
#' \dontrun{
#' # full resolution
#' ms_spec = specToMS_1D(as$filterbank_env, samplingRate = speechEx@samp.rate,
#'   specMethod = 'spectrum')
#' plotMS(log(ms_spec+.01), logWarpX = c(10, 2), quantile = NULL, ylab = 'kHz')
#' }
specToMS_1D = function(spec,
                       samplingRate,
                       windowLength = 250,
                       step = windowLength / 2,
                       specMethod = c('meanSpectrum', 'spectrum')) {
  specMethod = match.arg(specMethod)
  ## Extract the modulation spectrum as the spectrum of each channel
  nFilters = nrow(spec)
  if (nFilters < 1) return(NA)
  ms_list = vector('list', nFilters)
  if (specMethod == 'meanSpectrum') {
    # if we use meanSpectrum, wl should be at least 2 * longest modulation period
    for (i in 1:nFilters) {
      si = meanSpectrum(spec[i, ], samplingRate,
                        windowLength = windowLength,
                        step = step, plot = FALSE)
      ms_list[[i]] = si$ampl
      # plot(si, type = 'l', log = 'x')
    }
  } else if (specMethod == 'spectrum') {
    for (i in 1:nFilters) {
      si = spectrum(spec[i, ], samplingRate, plot = FALSE)
      ms_list[[i]] = si[, 2]
      # plot(si[, 1] * 1000, si[, 2], type = 'l', log = 'x')
    }
  }
  ms = do.call(rbind, ms_list)
  colnames(ms) = si$freq * 1000
  rownames(ms) = rownames(spec)
  # plotMS(ms, logWarpX = c(10, 2), quantile = NULL, ylab = '')
  ms
}



#' Calculate prop or fluctuation
#'
#' Calculates psychoacoustic roughness or slow fluctuation strength as the
#' proportion of a modulation spectrum that falls within a specified range of
#' temporal modulation frequencies - both as a single total value and per
#' frequency band. Roughness or fluctuation can be calculated either as the
#' simple proportion of the spectrum within \code{modRange}, or as a weighted
#' version using a lognormal weighting function centered at \code{modMean} with
#' spread \code{modSD}. Recommended weighting function for measuring roughness:
#' \code{modMean = 100, modSD = 8} (100 Hz ± 8 semitones); for measuring
#' low-frequency fluctuation: \code{modMean = 4, modSD = 12} (4 Hz ± 12
#' semitones). Because the result is a proportion (numerator and denominator are
#' summed over the same modulation-frequency columns), positive and negative
#' temporal modulation frequencies do not need to be folded: any mirroring
#' cancels out.
#'
#' @param m numeric matrix of non-negative values with column names giving
#'   temporal modulation frequencies (Hz). Row names, if present, are used as
#'   the frequency-band labels in the output.
#' @param modRange,modMean,modSD like "roughRange, roughMean, roughSD" or
#'   "fluctRange, fluctMean, fluctSD" in modulationSpectrum()
#' @param plot if TRUE, plots a one-sided AM spectrum with the selected
#'   frequency range and weighting function
#'
#' @return A list with two components:
#' \itemize{
#'   \item \code{prop}: the proportion of MS in the target range of modulation
#'   frequencies, in percent (NA if no valid \code{modRange} or weighting
#'   parameters, empty input, or no energy in the spectrum). \item
#'   \code{details}: a data.frame with one row per frequency band and columns
#'   \code{freq} (band label), \code{prop} (band-wise contribution to total
#'   proportion, \%), and \code{amp} (mean band amplitude, normalized to the
#'   maximum band).
#' }
#'
#' @seealso \code{\link{modulationSpectrum}}
#'
#' @keywords internal
#' @examples
#' s = soundgen(jitterDep = 2, amFreq = 80, amDep = 70, addSilence = 0)
#' m = modulationSpectrum(s, 16000, amRes = NULL, plot = FALSE)$detailed$original
#'
#' ## Ex. 1: simple proportion within modRange
#' r = soundgen:::getRoughness(m, modRange = c(30, Inf), plot = TRUE)
#' r = soundgen:::getRoughness(m, modRange = c(30, 150), plot = TRUE)
#' plot(r$details$freq, r$details$prop, type = 'b')
#' r$prop == sum(r$details$prop)
#'
#' ## Ex. 2: lognormal weighting function instead of modRange
#' # narrow
#' soundgen:::getRoughness(m, modRange = NULL, modMean = 75,
#'   modSD = 3, plot = TRUE)
#' # broad
#' soundgen:::getRoughness(m, modRange = NULL, modMean = 75,
#'   modSD = 10, plot = TRUE)
#' # truncated at modRange
#' soundgen:::getRoughness(m, modRange = c(30, 200), modMean = 75,
#'   modSD = 10, plot = TRUE)
#' # NB: a very large modSD means we essentially revert to a simple proportion
#'
#' ## Ex. 3: measure low-frequency AM fluctuation instead of prop
#' # (just use a different weighting function)
#' s2 = sinpi(2*440*(1:16000)/16000) * (1 - cospi(2*5*(1:16000)/16000))
#' osc(s2, 16000)
#' m2 = modulationSpectrum(s2, 16000, amRes = NULL, plot = FALSE)$detailed$original
#' fl = soundgen:::getRoughness(m2, modRange = c(0.25, 30), modMean = 4,
#'   modSD = 12, plot = TRUE)
#' fl$prop  # in this case, a measure of slow fluctuation
getRoughness = function(m,
                        modRange = NULL,
                        modMean = 100,
                        modSD = 8,
                        plot = FALSE) {
  out = list(prop = NA, details = NULL)

  # minimal checks: empty / invalid input
  if (is.null(m) || !is.matrix(m) || nrow(m) < 1 || ncol(m) < 1) return(out)
  freqs = as.numeric(colnames(m))
  if (all(is.na(freqs)) || any(!is.finite(freqs))) return(out)

  # validate that prop is actually defined
  has_range = !is.null(modRange) && length(modRange) == 2 &&
    modRange[1] < modRange[2]
  has_weights = !is.null(modMean) && !is.null(modSD) &&
    is.finite(modMean) && is.finite(modSD) &&
    modMean > 0 && modSD > 0
  if (!has_range && !has_weights) return(out)

  # no energy -> undefined
  total = sum(m, na.rm = TRUE)
  if (!is.finite(total) || total <= 0) return(out)

  # use absolute temporal modulation frequencies; no folding needed because
  # prop is a proportion (pos/neg sides cancel out)
  freqs = abs(freqs)
  nr = nrow(m)
  prop = rep(NA, nr)

  if (has_weights) {
    # weigh AM frequency components ~human perception of prop
    log_freqs = log2(freqs)
    weights = dnorm(log_freqs, mean = log2(modMean), sd = modSD / 12)
    if (has_range) {
      weights[freqs < modRange[1] | freqs > modRange[2]] = 0
    }
    # plot(freqs, weights, type = 'l', xlim = modRange)
    wmax = max(weights, na.rm = TRUE)
    if (!is.finite(wmax) || wmax <= 0) return(out)
    weights = matrix(rep(weights / wmax, nr), nrow = nr, byrow = TRUE)
    prop = rowSums(m * weights, na.rm = TRUE)
  } else {
    mod_cols = which(freqs > modRange[1] & freqs < modRange[2])
    if (length(mod_cols) > 0) {
      prop = rowSums(m[, mod_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # per-band amplitude labels, normalized to the max band
  amp = rowMeans(m, na.rm = TRUE)
  amax = max(amp, na.rm = TRUE)
  amp = if (is.finite(amax) && amax > 0) amp / amax else rep(NA, nr)

  details = data.frame(
    freq = if (!is.null(rownames(m))) as.numeric(rownames(m)) else NA,
    prop = as.numeric(prop) / total * 100,
    amp = amp
  )

  if (all(is.na(details$prop))) {
    total_prop = NA
  } else {
    total_prop = sum(details$prop, na.rm = TRUE)
  }

  # optional diagnostic plot (temporal-modulation domain, where modRange
  # and the weighting function are defined)
  if (plot) {
    # one-sided AM function for display: sum across frequency bands and
    # aggregate mirrored +/- temporal modulation frequencies
    amp_col = colSums(m, na.rm = TRUE)
    bin = median(abs(diff(freqs)), na.rm = TRUE)
    if (!is.finite(bin)) bin = 0.04
    amp_folded = foldMS(data.frame(freq = freqs, amp = amp_col), tol = bin / 4)

    # reference y-range for scaling the weight curve
    y_max = max(amp_folded$amp, na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max = 1

    plot(amp_folded$freq, amp_folded$amp, type = 'l',
         xlab = 'Temporal modulation, Hz', ylab = 'Modulation amplitude')

    # modRange as vertical lines
    if (has_range) {
      rr = modRange[is.finite(modRange)]
      if (length(rr) > 0) abline(v = modRange, col = 'blue', lty = 2)
    }

    # weighting function as a curve, scaled to the plotted y-range
    if (has_weights) {
      f_grid = seq(min(amp_folded$freq), max(amp_folded$freq), length.out = 200)
      w = dnorm(log2(pmax(f_grid, .Machine$double.eps)),
                mean = log2(modMean), sd = modSD / 12)
      if (has_range) w[f_grid < modRange[1] | f_grid > modRange[2]] = 0
      w_max = max(w, na.rm = TRUE)
      if (is.finite(w_max) && w_max > 0) {
        lines(f_grid, w / w_max * y_max, col = 'blue', lwd = 2)
      }
    }
  }

  list(prop = total_prop, details = details)
}


#' Fold a modulation spectrum
#' @param am data.frame(freq, amp) = colSums(ms)
#' @noRd
foldMS = function(am, tol = 0.01) {
  am = am[order(am$freq), ]
  # vectorized folding: average adjacent points that are closer than tol
  # (needed b/c each point is doubled in pos/neg frequencies)
  group = cumsum(c(TRUE, diff(am$freq) > tol))
  freq_sum = rowsum(am$freq, group)
  amp_sum = rowsum(am$amp, group)
  n_obs = rowsum(rep(1, nrow(am)), group)
  am_folded = data.frame(freq = as.numeric(freq_sum / n_obs),
                         amp = as.numeric(amp_sum / n_obs))
  am_folded = am_folded[is.finite(am_folded$freq) & is.finite(am_folded$amp), ]
  am_folded = am_folded[order(am_folded$freq), ]
  am_folded
}


#' Center / uncenter MS
#'
#' \code{centerMS()} reorders a modulation spectrum so that the DC component
#' is in the middle (equivalent to fftshift); \code{centerMS(inverse = TRUE)}
#' restores the original DFT order (equivalent to ifftshift). The two
#' operations are exact inverses of each other for both even and odd
#' dimensions. Row/column names are updated accordingly: centering derives
#' signed modulation frequencies from the original 0-based labels, and
#' uncentering restores the 0-based labels.
#' @param ms modulation spectrum: a matrix at least 2 x 2
#' @param addNames if TRUE, row/column names are updated (the input must have
#'   numeric rownames and colnames; otherwise names are left as is)
#' @param inverse if FALSE (default), center the MS (DC in the middle); if
#'   TRUE, undo the centering (restore the original DFT order)
#' @noRd
#' @examples
#' spec = spectrogram(soundgen(), 16000, plot = FALSE)
#' ms = soundgen:::specToMS(spec)
#' image(abs(ms$ms))
#' image(abs(ms$ms_centered))
#' image(abs(soundgen:::centerMS(ms$ms)))
#' centerMS(matrix(1, nrow = 2, ncol = 1))  # error
#' centerMS(matrix(1, nrow = 2, ncol = 2))  # OK
#'
#' # Round trip is exact for both odd and even dimensions:
#' m = matrix(1:30, nrow = 5, ncol = 6)
#' rownames(m) = 0:4; colnames(m) = 0:5
#' identical(centerMS(centerMS(m), inverse = TRUE), m)  # TRUE (odd x even)
#' m = matrix(1:36, nrow = 6, ncol = 6)
#' rownames(m) = 0:5; colnames(m) = 0:5
#' identical(centerMS(centerMS(m), inverse = TRUE), m)  # TRUE (even x even)
centerMS = function(ms, addNames = TRUE, inverse = FALSE) {
  nr = nrow(ms)
  nc = ncol(ms)
  if (nr < 2 || nc < 2)
    stop('ms must be a matrix of at least 2 x 2')

  names_fm = suppressWarnings(as.numeric(rownames(ms)))
  names_am = suppressWarnings(as.numeric(colnames(ms)))
  update_names = addNames &&
    length(names_fm) == nr && all(is.finite(names_fm)) &&
    length(names_am) == nc && all(is.finite(names_am))

  idx_fm = centerMS_idx(nr, inverse)
  idx_am = centerMS_idx(nc, inverse)

  ms_out = ms[idx_fm, idx_am, drop = FALSE]
  if (update_names) {
    rownames(ms_out) = centerMS_names(nr, names_fm, inverse)
    colnames(ms_out) = centerMS_names(nc, names_am, inverse)
  }
  ms_out
}

#' Permutation indices for one dimension:
#' fftshift for centering, ifftshift for uncentering.
#' NB: for even n the two coincide; for odd n they differ!
#' @noRd
centerMS_idx = function(n, inverse) {
  if (n %% 2 == 0) {  # even
    i = n %/% 2
    return(c((i + 1):n, 1:i))
  }
  i = n %/% 2
  if (inverse) {  # odd, uncentering: ifftshift
    c((i + 1):n, 1:i)
  } else {  # odd, centering: fftshift
    c((i + 2):n, 1:(i + 1))
  }
}

#' New labels for one dimension
#' @noRd
centerMS_names = function(n, names_in, inverse) {
  if (inverse) {
    # restore the original 0-based labels
    width = median(diff(sort(unique(names_in))))
    return((0:(n - 1)) * width)
  }
  if (n %% 2 == 0) {  # even
    i = n %/% 2
    # NB: 2:i would be c(2, 1) for n = 2, hence the explicit check
    neg = if (i >= 2) rev(-names_in[2:i]) else numeric(0)
    c(-names_in[i + 1], neg, names_in[1:i])
  } else {  # odd
    i = n %/% 2
    c(rev(-names_in[2:(i + 1)]), names_in[1:(i + 1)])
  }
}
