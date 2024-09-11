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
#' @param X,Y rownames and colnames of \code{ms}, respectively
#' @param audio (internal) a list of audio attributes
#' @param extraY if TRUE, another Y-axis is plotted on the right showing 1000/Y
#' @export
#' @examples
#' ms1 = modulationSpectrum(runif(4000), samplingRate = 16000, plot = TRUE)
#' plotMS(ms1$processed)  # identical to above
#'
#' # compare two modulation spectra
#' ms2 = modulationSpectrum(soundgen(sylLen = 100, addSilence = 0),
#'                          samplingRate = 16000)
#' # ensure the two matrices have the same dimensions
#' ms2_resized = soundgen:::interpolMatrix(ms2$original,
#'   nr = nrow(ms1$original), nc = ncol(ms1$original))
#' # plot the difference
#' plotMS(log(ms1$original / ms2_resized), quantile = NULL,
#'   col = colorRampPalette(c('blue', 'yellow')) (50))
plotMS = function(
    ms,
    X = NULL,
    Y = NULL,
    quantiles = c(.5, .8, .9),
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
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
  if (is.null(X)) X = as.numeric(colnames(ms))
  if (is.null(Y)) Y = as.numeric(rownames(ms))
  if (is.null(xlim)) xlim = range(X)
  if (is.null(ylim)) ylim = range(Y)
  if (is.null(main) & !is.null(audio$filename_noExt)) {
    if (audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
  }

  # plot with filled.contour.mod
  ms = zeroOne(ms)
  X1 = X
  Y1 = Y
  if (is.numeric(logWarpX) | is.numeric(logWarpY)) {
    if (is.numeric(logWarpX)) {
      X1 = pseudoLog(X, sigma = logWarpX[1], base = logWarpX[2])
      # lab_x = pretty(X, n = 9, min.n = 7)
      # at_x = pseudoLog(lab_x, sigma = logWarpX[1], base = logWarpX[2])
      at_x = pretty(X1, n = 7)
      lab_x = round(pseudoLog_undo(at_x, sigma = logWarpX[1], base = logWarpX[2]))
      if (!is.null(xlim)) xlim = pseudoLog(xlim, sigma = logWarpX[1], base = logWarpX[2])
    } else {
      lab_x = at_x = pretty(X)
    }

    if (is.numeric(logWarpY)) {
      Y1 = pseudoLog(Y, sigma = logWarpY[1], base = logWarpY[2])
      lab_y = pretty(Y)
      at_y = pseudoLog(lab_y, sigma = logWarpY[2], base = logWarpY[2])
      if (!is.null(ylim)) ylim = pseudoLog(ylim, sigma = logWarpX[1], base = logWarpX[2])
    } else {
      lab_y = at_y = pretty(Y)
    }
    filled.contour.mod(X1, Y1, t(ms),
                       levels = seq(0, 1, length = 100),
                       color.palette = color.palette,
                       col = col,
                       xlab = xlab, ylab = ylab,
                       bty = 'n',
                       main = main, xaxt = 'n', yaxt = 'n',
                       xlim = xlim, ylim = ylim,
                       ...)

    # add nicely labeled axes
    axis(1, at = at_x, labels = lab_x, ...)  # xpd = TRUE to extend beyond the plot
    axis(2, at = at_y, labels = lab_y, ...)

    lbl_Hz_pos = pretty(Y1)
    lbls_Hz = round(1000 / lbl_Hz_pos)
    if (extraY) axis(4, at = lbl_Hz_pos, labels = lbls_Hz, ...)
  } else {
    # no log-warping
    filled.contour.mod(X1, Y1, t(ms),
                       levels = seq(0, 1, length = 100),
                       color.palette = color.palette,
                       col = col,
                       xlab = xlab, ylab = ylab,
                       bty = 'n',
                       main = main,
                       xlim = xlim, ylim = ylim,
                       ...)
    lbls = round(1000 / pretty(Y1))
    lbl_pos = 1000 / lbls
    if (extraY) axis(4, at = lbl_pos, labels = lbls, ...)
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


#' Get amplitude modulation
#'
#' Internal soundgen function
#'
#' Helper function for calculating amplitude modulation based on the modulation
#' spectrum. Algorithm: averages AM across all FM bins in the positive half of
#' the modulation spectrum and looks for a peak in the specified AM frequency
#' range.
#' @param m numeric matrix of non-negative values with colnames giving temporal
#'   modulation frequency
#' @inheritParams modulationSpectrum
#' @param amRes controls the width of window over which we look for local maxima
#' @return Returns a list with the frequency (Hz) and depth of amplitude
#'   modulation (dB relative to global max, normally at 0 Hz).
#' @keywords internal
getAM = function(m,
                 amRange = c(10, 100),
                 amRes = NULL) {
  if (is.null(amRes)) amRes = 0
  colNames = abs(as.numeric(colnames(m)))
  out = list(amMsFreq = NA, amMsPurity = NA)
  # image(t(log(m)))

  # fold around 0 and average AM across all FM bins
  am = data.frame(freq = abs(colNames), amp = colSums(m))
  am = am[order(am$freq), ]
  # plot(am, type = 'l')

  # average folded AM function cross pos/neg AM frequencies
  # (b/c each point is doubled)
  am_sm = am
  i = 1
  while(i < nrow(am_sm)) {
    if (abs(am_sm$freq[i] - am_sm$freq[i + 1]) < .1) {
      am_sm$amp[i] = (am_sm$amp[i] + am_sm$amp[i + 1]) / 2
      am_sm$amp[i + 1] = NA
      i = i + 2
    } else {
      i = i + 1
    }
  }
  am_sm = na.omit(am_sm)
  # plot(am, type = 'l')
  # lines(am_sm, type = 'l', col = 'blue')

  # find local maxima within amRange (note that we don't just throw away the
  # rest of ms to improve the precision of smoothed AM function)
  am_smRan = am_sm[am_sm$freq >= amRange[1] & am_sm$freq <= amRange[2], ]
  # plot(am_smRan, type = 'l')
  wl = max(3, round(amRes / (colNames[2] - colNames[1])))
  # eg if amRes = 10, we look for a local maximum within ±5 Hz
  temp = zoo::rollapply(
    zoo::as.zoo(am_smRan$amp),
    width = wl,  # width 10 Hz, ie ±5 Hz
    align = 'center',
    function(x) {
      middle = ceiling(length(x) / 2)
      return(which.max(x) == middle)
    })
  idx = zoo::index(temp)[zoo::coredata(temp)]

  if (length(idx) > 0) {
    peaks = am_smRan[idx, ]
    peaks = peaks[which.max(peaks$amp), ]
    if (nrow(peaks) > 0) {
      out$amMsFreq = peaks$freq
      out$amMsPurity = log10(peaks$amp / max(am_sm$amp)) * 20
    }
  }
  return(out)
}


#' Log-warp a modulation spectrum
#'
#' Internal soundgen function
#'
#' Log-warps a modulation spectrum along time dimension
#'
#' @param x a modulation spectrum: rows = FM, cols = AM
#' @keywords internal
#' @examples
#' a = matrix(1:44, ncol = 11)
#' colnames(a) = -5:5
#' soundgen:::logWarpMS(a, logWarp = 2)
logWarpMS = function(x, logWarp) {
  X = as.numeric(colnames(x))
  neg_col = which(X < 0)
  zero_col = which(X == 0)
  pos_col = which(X > 0)
  m_left = logMatrix(x[, rev(neg_col)], base = logWarp)
  # NB: flip the left half!
  m_right = logMatrix(x[, pos_col], base = logWarp)
  x_transf = cbind(m_left[, ncol(m_left):1], x[, zero_col, drop = FALSE], m_right)
  return(x_transf)
}


#' Calculate roughness from modulation spectrum
#'
#' Internal soundgen function
#'
#' Helper function for calculating roughness - the proportion of energy /
#' amplitude in the roughness range
#' @param m numeric matrix of non-negative values with colnames giving temporal
#'   modulation frequency
#' @inheritParams modulationSpectrum
#' @return Returns roughness in percent.
#' @keywords internal
#' @examples
#' m = modulationSpectrum(soundgen(jitterDep = 2, addSilence = 0),
#'   samplingRate = 16000)$original
#' # proportion within roughRange
#' plot(soundgen:::getRough(m, roughRange = c(30, Inf))[, 1:2])
#' plot(soundgen:::getRough(m, roughRange = c(30, 150))[, 1:2])
#'
#' # lognormal weighting function instead of roughRange
#' plot(soundgen:::getRough(m, roughRange = NULL,
#'   roughMean = 75, roughSD = 1)[, 1:2])  # narrow
#' plot(soundgen:::getRough(m, roughRange = NULL,
#'   roughMean = 75, roughSD = 5000)[, 1:2])  # very broad
#'
#' # lognormal weighting function truncated at roughRange
#' plot(soundgen:::getRough(m, roughRange = c(30, 150),
#'   roughMean = 75, roughSD = 3)[, 1:2])
#' plot(soundgen:::getRough(m, roughRange = c(30, 150),
#'   roughMean = 75, roughSD = 5000)[, 1:2])
#' # approaches proportion in roughRange as SD --> Inf
#'
#' # a nice plot weighting by amplitude
#' r2 = soundgen:::getRough(m)
#' plot(r2$freq, r2$rough, cex = r2$amp ^ 2 + .25)
#' sum(r2$rough)  # simple sum across all bands
#' sum(r2$rough * r2$amp / sum(r2$amp))  # amplitude-weighted mean
#' # log-amplitude-weighted mean
#' sum(r2$rough * log(r2$amp+1e-6) / sum(log(r2$amp+1e-6)))
getRough = function(m,
                    roughRange = c(30, 150),
                    roughMean = NULL,
                    roughSD = NULL) {
  freqs = as.numeric(colnames(m))
  if (any(freqs < 0)) {
    # fold around 0
    ord = order(abs(freqs))
    m = m[, ord, drop = FALSE]
    freqs = abs(freqs[ord])
  }
  nr = nrow(m)
  # roughness = rep(NA, nr)
  # rowsums_m = rowSums(m)
  # image(t(m))

  roughness = rep(NA, nrow(m))
  if (!is.null(roughMean) & !is.null(roughSD)) {
    # calculate a weighted mean over the entire range of modulation frequencies
    # weigh AM frequency components ~corresponding to human perception of roughness
    log_freqs = log2(freqs)
    weights = dnorm(log_freqs, mean = log2(roughMean), sd = roughSD/12)
    if (!is.null(roughRange))
      weights[which(freqs < roughRange[1] | freqs > roughRange[2])] = 0
    weights = matrix(rep(weights / max(weights), nr), nrow = nr, byrow = TRUE)
    if (FALSE) {
      cs = zeroOne(colSums(m))
      plot(freqs, cs, type = 'l', ylim = c(0, 1))
      lines(freqs, weights[1, ], type = 'l', col = 'blue')
      lines(freqs, cs * weights[1, ], type = 'l', col = 'red')
    }
    roughness = rowSums(m * weights) # / rowsums_m * 100
  } else if (!is.null(roughRange)) {
    rough_cols = which(freqs > roughRange[1] & freqs < roughRange[2])
    if (length(rough_cols) > 0)
      roughness = rowSums(m[, rough_cols, drop = FALSE]) # / rowsums_m * 100
  }

  # set amps + labels per channel, if available
  amp = as.numeric(rowMeans(m))
  amp = amp / max(amp)
  return(data.frame(
    roughness = as.numeric(roughness) / sum(m) * 100,
    freq = if (!is.null(rownames(m))) as.numeric(rownames(m)) else NA,
    amp = amp
  ) [, c(2, 1, 3)])
}


#' Spectrogram to modulation spectrum
#'
#' Takes a spectrogram (either complex or magnitude) and returns a MS with
#' proper row and column labels.
#' @return Returns a MS - matrix of complex values of the same dimension as
#'   spec, with AM in rows and FM in columns.
#' @param spec target spectrogram (numeric matrix, frequency in rows, time in
#'   columns)
#' @inheritParams spectrogram
#' @export
#' @examples
#' s = soundgen(sylLen = 500, amFreq = 25, amDep = 50,
#'              pitch = 250, samplingRate = 16000)
#' spec = spectrogram(s, samplingRate = 16000, windowLength = 25, step = 5, plot = FALSE)
#' ms = specToMS(spec)
#' image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)),
#'       z = t(log(abs(ms))), xlab = 'Amplitude modulation, Hz',
#'       ylab = 'Frequency modulation, cycles/kHz')
#' abline(h = 0, lty = 3); abline(v = 0, lty = 3)
specToMS = function(spec, windowLength = NULL, step = NULL) {
  if ((is.null(colnames(spec)) & is.null(step)) |
      (is.null(rownames(spec)) & is.null(windowLength))) {
    addNames = FALSE
    message(paste("If spec doesn't have rownames/colnames,",
                  "you have to specify STFT step and samplingRate,",
                  "otherwise AM and FM stamps can't be",
                  "added to the modulation spectrum"))
  } else {
    addNames = TRUE
  }

  # Center to flip the spectrum and have 0 frequencies in the middle
  # (see spec.fft function in "spectral" package)
  spec_centered = spec * (-1)^(row(spec) + col(spec))  # *checkerboard of ±1

  # 2D fft
  ms = fft(spec_centered, inverse = FALSE) / length(spec_centered)

  # Add labels
  if (!is.null(colnames(spec)) | !is.null(step)) {
    if (is.null(step)) step = diff(as.numeric(colnames(spec)[1:2]))
    # AM
    nc = ncol(ms)
    bin_width = 1000 / step / nc
    colnames(ms) = ((0:(nc - 1)) - nc / 2) * bin_width
  }

  if (!is.null(rownames(spec)) | !is.null(windowLength)) {
    # FM
    nr = nrow(ms)
    if (is.null(windowLength)) {
      samplingRate = (max(abs(as.numeric(rownames(spec)))) +  # middle of top bin
                        min(abs(as.numeric(rownames(spec))))) *  # bin/2
        1000 * 2
      windowLength = nr * 2 / (samplingRate / 1000)
    }
    max_fm = windowLength / 2
    rownames(ms) = seq(-max_fm, max_fm, length.out = nr)
  }

  return(ms)
}


#' Modulation spectrum to spectrogram
#'
#' Takes a complex MS and transforms it to a complex spectrogram with proper row
#' (frequency) and column (time) labels.
#' @return Returns a spectrogram - a numeric matrix of complex numbers of
#'   the same dimensions as ms.
#' @param ms target modulation spectrum (matrix of complex numbers)
#' @inheritParams spectrogram
#' @export
#' @examples
#' s = soundgen(sylLen = 250, amFreq = 25, amDep = 50,
#'              pitch = 250, samplingRate = 16000)
#' spec = spectrogram(s, samplingRate = 16000, windowLength = 25, step = 5)
#' ms = specToMS(spec)
#' image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)),
#'       z = t(log(abs(ms))), xlab = 'Amplitude modulation, Hz',
#'       ylab = 'Frequency modulation, cycles/kHz')
#' spec_new = msToSpec(ms)
#' image(x = as.numeric(colnames(spec_new)), y = as.numeric(rownames(spec_new)),
#'       z = t(log(abs(spec_new))), xlab = 'Time, ms',
#'       ylab = 'Frequency, kHz')
msToSpec = function(ms, windowLength = NULL, step = NULL) {
  addNames = TRUE
  if ((is.null(colnames(ms)) & is.null(step)) |
      (is.null(rownames(ms)) & is.null(windowLength))) {
    addNames = FALSE
    message(paste("If ms doesn't have rownames/colnames,",
                  "you have to specify windowLength and step,",
                  "otherwise frequency and time stamps can't be",
                  "added to the spectrogram"))
  }

  # Inverse FFT
  s1 = fft(ms, inverse = TRUE) / length(ms)

  # Undo centering
  s2 = s1 / (-1)^(row(s1) + col(s1))

  # Add rownames & colnames
  if (addNames) {
    if (is.null(step)) {
      max_am = abs(as.numeric(colnames(ms)[1]))
      step = 1000 / 2 / max_am
    }
    if (is.null(windowLength)) {
      max_fm = max(abs(as.numeric(rownames(ms))))
      windowLength = max_fm * 2
    }
    # From the def in spectrogram():
    windowLength_points = nrow(s2) * 2
    samplingRate = windowLength_points / windowLength * 1000
    # frequency stamps
    rownames(s2) = (0:(nrow(s2) - 1)) * samplingRate / windowLength_points / 1000
    # time stamps
    colnames(s2) = windowLength / 2 + (0:(ncol(s2) - 1)) * step
  }
  return(s2)
}


#' Spectrogram to modulation spectrum 1D
#'
#' Takes a spectrogram and returns a MS - that is, the spectrum of each channel. The input can be an ordinary
#' STFT spectrogram or an auditory spectrogram (a signal convolved with a bank
#' of bandpass filters). The difference from \code{\link{specToMS}} is that,
#' instead of taking a two-dimensional transform of the spectrogram, here the
#' spectra are calculated independently for each frequency bin.
#'
#' @param fb input spectrogram (numeric matrix with frequency in rows and time
#'   in columns)
#' @param samplingRate for auditory spectrogram, the sampling rate of input
#'   audio; for STFT spectrograms, the number of STFT frames per second
#' @param windowLength,step determine the resolution of modulation spectra (both
#'   in ms)
#' @param method calls either \code{\link[seewave]{meanspec}} or
#'   \code{\link[seewave]{spec}}
#'
#' @return Returns a MS - a matrix of real values, with center frequencies of
#'   original filters in rows and modulation frequencies in columns.

#' @export
#' @examples
#' data(sheep, package = 'seewave')
#'
#' # auditory spectrogram
#' as = audSpectrogram(sheep, filterType = 'butterworth',
#'   nFilters = 24, plot = FALSE)
#' fb = t(do.call(cbind, as$filterbank_env))
#' rownames(fb) = names(as$filterbank_env)
#' ms = soundgen:::specToMS_1D(fb, sheep@samp.rate)
#' plotMS(log(ms+.01), logWarpX = c(10, 2), quantile = NULL, ylab = 'kHz')
#'
#' # ordinary STFT spectrogram
#' sp = spectrogram(sheep, windowLength = 15, step = 0.5,
#'   output = 'original', plot = FALSE)
#' ms2 = soundgen:::specToMS_1D(sp, 1000 / 0.5)  # 1000/0.5 frames per s
#' plotMS(log(ms2+.01), quantile = NULL, ylab = 'kHz')
#' \dontrun{
#' ms_spec = soundgen:::specToMS_1D(fb, sheep@samp.rate, method = 'spec')
#' plotMS(log(ms_spec+.01), logWarpX = c(10, 2), quantile = NULL, ylab = 'kHz')
#' }
specToMS_1D = function(fb,
                       samplingRate,
                       windowLength = 250,
                       step = windowLength / 2,
                       method = c('spec', 'meanspec')[2]) {
  ## Extract the modulation spectrum as the spectrum of each channel
  nFilters = nrow(fb)
  ms_list = vector('list', nFilters)
  if (method == 'meanspec') {
    # if we use meanspec, wl should be at least 2 * longest modulation period
    # (wl must also be an even number)
    wl = 2 * floor(min(ncol(fb) / 2, windowLength / 1000 * samplingRate / 2))
    # 2 * round(min(ncol(fb) / 2, 2^ceiling(log2(audio$samplingRate / lowestFreq) + 1)) / 2)
    ovlp = 100 * (1 - step / windowLength)
    for (i in 1:nFilters) {
      si = seewave::meanspec(fb[i, ], f = samplingRate, norm = FALSE,
                             wl = wl, ovlp = ovlp, plot = FALSE)
      ms_list[[i]] = si[, 2]
      # plot(si[, 1] * 1000, si[, 2], type = 'l', log = 'x')
    }
  } else if (method == 'spec') {
    for (i in 1:nFilters) {
      si = seewave::spec(fb[i, ], f = samplingRate, norm = FALSE, plot = FALSE)
      ms_list[[i]] = si[, 2]
      # plot(si[, 1] * 1000, si[, 2], type = 'l', log = 'x')
    }
  }
  ms = do.call(rbind, ms_list)
  colnames(ms) = si[, 1] * 1000
  rownames(ms) = rownames(fb)
  # plotMS(ms, logWarpX = c(10, 2), quantile = NULL, ylab = '')
  return(ms)
}
