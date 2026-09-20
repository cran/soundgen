### UTILITIES FOR LOW-LEVEL MATH ###

#' Find peaks
#'
#' A bare-bones, very fast function to find local maxima (peaks) in a numeric
#' vector. NAs are set to the minimum value of input and effectively ignored.
#' The first and last values can also be peaks. Plateaus are NOT treated as
#' peaks.
#'
#' @param x numeric vector
#' @param wl rolling window over which we look for maxima: central value ±
#'   floor(wl/2), eg ±1 if wl=3; forced to be at least 3
#' @param thres required absolute value of each peak
#' @param thresRel required absolute excess over the window mean excluding the
#'   focal point
#' @param dropFirstLast if TRUE, the first and last value cannot be peaks (only
#'   two-sided maxima accepted)
#' @param plot if TRUE, plots the input vector with peaks marked.
#' @return A vector of indices of local maxima.
#' @export
#' @examples
#' x = c(5, 2, 3, 3, 2, 7, 3, 2, 5, 5, 1, 0, 2, NA)
#' findPeaks(x, wl = 3, plot = TRUE)
#' findPeaks(x, wl = 3, thres = 2, plot = TRUE)
#' findPeaks(x, wl = 3, thres = 100, plot = TRUE)
#' findPeaks(x, wl = 4, plot = TRUE)
#' findPeaks(x, wl = 6, plot = TRUE)
#' findPeaks(x, wl = 3, thresRel = 2, plot = TRUE)
#' findPeaks(x, wl = 3, thresRel = 4, plot = TRUE)
#' findPeaks(x, wl = 3, thres = 6, thresRel = 2, plot = TRUE)
#' findPeaks(x, dropFirstLast = TRUE)
findPeaks = function(x,
                     wl = 3,
                     thres = NULL,
                     thresRel = NULL,
                     dropFirstLast = FALSE,
                     plot = FALSE) {
  if (wl < 3) {
    wl = 3
    warning('wl must be >=3; resetting to 3')
  }
  if (length(x) == 0 || all(is.na(x))) return(numeric(0))
  min_x = min(x, na.rm = TRUE)
  xm = x
  xm[is.na(xm)] = min_x  # easy way to ignore all NAs
  if (!is.null(thres) && !any(xm > thres)) {
    # nothing to do
    idx_peaks = numeric(0)
  } else {
    len = length(xm)
    halfwl = floor(wl / 2)
    if (halfwl == 1 && is.null(thresRel)) {
      # pad with min_x to accept the first and last values as peaks
      x1 = c(min_x, xm, min_x)
      if (is.null(thres) && is.null(thresRel)) {
        # no thresholds
        idx_peaks = which(diff(sign(diff(x1))) == -2)
      } else {
        # only absolute threshold
        idx = which(diff(sign(diff(x1))) == -2)
        idx_peaks = idx[which(xm[idx] > thres)]
      }
    } else if (halfwl > 1 || !is.null(thresRel)) {
      # in which case just diff(sign(x)) is not enough
      peaks = rep(FALSE, len)
      if (is.null(thres) && is.null(thresRel)) {
        # no thresholds
        for (i in 1:len) {
          frx = max(1, (i - halfwl)):min(len, (i + halfwl))
          frx_wo_i = frx[frx != i]
          peaks[i] = !any(xm[frx_wo_i] >= xm[i])
        }
      } else if (!is.null(thres) & is.null(thresRel)) {
        # only absolute threshold
        for (i in 1:len) {
          frx = max(1, (i - halfwl)):min(len, (i + halfwl))
          frx_wo_i = frx[frx != i]
          peaks[i] = xm[i] > thres && !any(xm[frx_wo_i] >= xm[i])
        }
      } else if (is.null(thres) & !is.null(thresRel)) {
        # only relative threshold
        for (i in 1:len) {
          frx = max(1, (i - halfwl)):min(len, (i + halfwl))
          frx_wo_i = frx[frx != i]
          peaks[i] = ((xm[i] - mean(xm[frx_wo_i])) > thresRel) &&
            !any(xm[frx_wo_i] >= xm[i])
        }
      } else {
        # both absolute and relative thresholds
        for (i in 1:len) {
          frx = max(1, (i - halfwl)):min(len, (i + halfwl))
          frx_wo_i = frx[frx != i]
          peaks[i] = (xm[i] > thres) &&
            ((xm[i] - mean(xm[frx_wo_i])) > thresRel) &&
            !any(xm[frx_wo_i] >= xm[i])
        }
      }
      idx_peaks = which(peaks)
    }
  }

  # drop the first and last values if they are among peaks
  if (dropFirstLast)
    idx_peaks = idx_peaks[which(!idx_peaks %in% c(1, length(x)))]

  if (plot) {
    plot(x, type = 'b')
    points(idx_peaks, x[idx_peaks], pch = 16, col = 'blue')
  }
  idx_peaks
}


#' Entropy
#'
#' Returns Wiener or Shannon entropy of an input vector such as the spectrum of
#' a sound. Non-positive input values are converted to a small positive number
#' (convertNonPositive). If all elements are zero, returns NA.
#'
#' @param x vector of positive floats
#' @param type 'shannon' for Shannon (information) entropy, 'wiener' for Wiener
#'   entropy
#' @param normalize if TRUE, Shannon entropy is normalized by the length of
#'   input vector to range from 0 to 1. It has no effect on Wiener entropy
#' @param convertNonPositive occasional non-positive values are converted to
#'   \code{convertNonPositive} (if all inputs are non-positive, entropy = NA)
#' @return The entropy of input vector as a single number.
#' @export
#' @examples
#' # Here are four simplified power spectra, each with 9 frequency bins:
#' s = list(
#'   onePeak = c(rep(0, 4), 1, rep(0, 4)),         # a single peak in spectrum
#'   harmonics = c(0, 0, 1, 0, 0, .75, 0, 0, .5),  # perfectly periodic, with 3 harmonics
#'   silent = rep(0, 9),                           # a silent frame
#'   noise = rep(1, 9)                             # white noise
#' )
#'
#' # Wiener entropy is ~0 for periodic, NA for silent, 1 for white noise
#' lapply(s, function(x) round(getEntropy(x), 2))
#'
#' # Shannon entropy is ~0 for periodic with a single harmonic, moderate for
#' # periodic with multiple harmonics, NA for silent, highest for white noise
#' lapply(s, function(x) round(getEntropy(x, type = 'shannon'), 2))
#'
#' # Normalized Shannon entropy - same but forced to be 0 to 1
#' lapply(s, function(x) round(getEntropy(x,
#'   type = 'shannon', normalize = TRUE), 2))
getEntropy = function(x,
                      type = c('wiener', 'shannon'),
                      normalize = FALSE,
                      convertNonPositive = 1e-10) {
  type = match.arg(type)
  if (any(!is.finite(x)) || all(x <= 0)) return(NA)
  len = length(x)
  if (len < 2) return(NA)
  x[x <= 0] = convertNonPositive  # otherwise log0 gives NaN
  sum_x = sum(x)
  if (sum_x < .Machine$double.eps) return (NA)  # empty frames

  if (type == 'wiener') {
    geom_mean = exp(mean(log(x)))
    ar_mean = sum_x / len
    entropy = geom_mean / ar_mean
  } else if (type == 'shannon') {
    p = x / sum_x
    if (normalize) {
      entropy = -sum(p * log2(p) / log2(len))
    } else {  # unnormalized
      entropy = -sum(p * log2(p))
    }
  } else {
    stop('Implemented entropy types: "shannon" or "wiener"')
  }
  entropy
}


#' Mix signals at specific SNR
#'
#' Adds two partly overlapping vectors, such as two waveforms or "signal" and
#' "noise", at specific RMS or loudness levels. The location at which vector 2
#' ("noise") is pasted is defined by \code{insertionPoint}. Algorithm: both
#' vectors are padded with zeros to match in length, rescaled, added, and
#' re-normalized. All NA's are converted to 0. \code{addVectors} is the former
#' name from soundgen 2.x, preserved for back-compatibility.
#'
#' @param signal,noise numeric vectors
#' @param insertionPoint the index of element in vector 1 at which vector 2 will
#'   be inserted (any integer, can also be negative)
#' @param SNR signal-to-noise ratio, dB
#' @param type rescale \code{noise} to target SNR relative to \code{signal}
#'   based on  root mean square amplitude ('rms' or 'RMS', default), peak
#'   amplitude ('peak'), or mean subjective loudness in sone ('loudness')
#' @param loudness_pars a list of parameters passed to \code{\link{getLoudness}}
#'   if \code{type = 'loudness'}
#' @param normalize if TRUE, the output is normalized to range from -1 to +1
#' @return A numeric vector.
#' @export
#' @examples
#' v1 = 1:6
#' v2 = rep(100, 3)
#' addVectors(v1, v2, insertionPoint = 5, normalize = FALSE)
#' addVectors(v1, v2, insertionPoint = -4, normalize = FALSE)
#' addVectors(v1, rep(100, 15), insertionPoint = -4, normalize = FALSE)
#' # note the asymmetry: insertionPoint refers to the first arg
#' addVectors(v1, v2, insertionPoint = -4, normalize = FALSE)
#'
#' v3 = rep(100, 15)
#' addVectors(v1, v3, insertionPoint = -4, normalize = FALSE)
#' addVectors(v2, v3, insertionPoint = 7, normalize = FALSE)
#' addVectors(1:6, 3:6, insertionPoint = 3, normalize = FALSE)
#'
#' # add noise to a sound at a particular SNR
#' signal = rep(c(rep(0, 500), cospi(2 * 440 * (1:1000) / 16000)), 2)
#' noise = rnorm(length(signal))
#' osc(mixAtSNR(signal, noise, SNR = 6), samplingRate = 16000)
#' osc(mixAtSNR(signal, noise, SNR = 6, type = 'peak'), samplingRate = 16000)
#' osc(mixAtSNR(signal, noise, SNR = 6, type = 'loudness',
#'   loudness_pars = list(samplingRate = 16000, SPL = 80)), samplingRate = 16000)
#' osc(mixAtSNR(signal, noise, SNR = 12), samplingRate = 16000)
#' osc(mixAtSNR(signal, noise, SNR = 0), samplingRate = 16000)
#'
#' # no rescaling (SNR = NULL)
#' osc(mixAtSNR(signal, noise, SNR = NULL), samplingRate = 16000)
#' osc(mixAtSNR(signal, noise/4, SNR = NULL), samplingRate = 16000)
mixAtSNR = function(signal, noise,
                    insertionPoint = 1L,
                    normalize = TRUE,
                    SNR = NULL,
                    type = c('rms', 'RMS', 'peak', 'loudness'),
                    loudness_pars = list()) {
  type = match.arg(type, choices = c('rms', 'RMS', 'peak', 'loudness'))
  type = tolower(type)

  # Handle zero-length vectors immediately
  if (!is.numeric(signal)) stop(paste('Non-numeric signal:', head(signal)))
  if (!is.numeric(noise)) stop(paste('Non-numeric noise:', head(noise)))
  l1 = length(signal)
  l2 = length(noise)
  if (l1 == 0 && l2 == 0) return(numeric(0))
  if (l1 == 0) return(if (normalize && max(abs(noise)) > 0)
    noise / max(abs(noise)) else noise)
  if (l2 == 0) return(if (normalize && max(abs(signal)) > 0)
    signal / max(abs(signal)) else signal)
  signal[is.na(signal)] = 0
  noise[is.na(noise)] = 0
  insertionPoint = round(insertionPoint)

  # rescale
  if (!is.null(SNR) && is.finite(SNR)) {
    rescale = TRUE
    if (type == 'peak') {
      a_signal = max(abs(signal))
      a_noise = max(abs(noise))
    } else if (type == 'rms') {
      a_signal = sqrt(mean(signal^2))
      a_noise = sqrt(mean(noise^2))
    } else if (type == 'loudness') {
      if (is.null(loudness_pars$samplingRate))
        stop('if method = "loudness", samplingRate must be defined in loudness_pars')
      a_signal = do.call(getLoudness, c(list(signal),
                                        loudness_pars))$summary$loudness_mean
      a_noise = do.call(getLoudness, c(list(noise),
                                       loudness_pars))$summary$loudness_mean
    }
    if (a_noise > 0)
      noise = noise * (a_signal / a_noise / 10^(SNR/20))
  } else {
    rescale = FALSE
  }

  # swap signal/noise if inserting noise before signal
  if (insertionPoint < 1) {
    # negative indices - noise starts before signal: simply swap signal/noise
    temp = signal
    signal = noise
    noise = temp
    insertionPoint = 2 - insertionPoint
    if (rescale) SNR = -SNR
    # swap l1 and l2 as well
    l_temp = l1
    l1 = l2
    l2 = l_temp
  }

  # add
  if (insertionPoint == 1) {
    # left-aligned already
    if (l1 == l2) {
      out = signal + noise
    } else if (l1 > l2) {
      out = c(signal[1:l2] + noise, signal[(l2 + 1):l1])
    } else {
      out = c(signal + noise[1:l1], noise[(l1 + 1):l2])
    }
  } else if (insertionPoint > l1) {
    # noise starts after the end of signal
    out = c(signal, rep(0, insertionPoint - l1 - 1), noise)
  } else {
    # insertionPoint inside signal
    a = l1 - insertionPoint + 1
    if (a == l2) {
      # signal and noise end at the same spot
      out = c(
        signal[1:(insertionPoint - 1)],
        signal[insertionPoint:(insertionPoint + l2 - 1)] + noise)
    } else if (a > l2) {
      # noise fully contained within signal
      b = insertionPoint + l2
      out = c(
        signal[1:(insertionPoint - 1)],
        signal[insertionPoint:(b - 1)] + noise,
        signal[b:l1]
      )
    } else {
      # signal and noise overlap only partially
      out = c(
        signal[1:(insertionPoint - 1)],
        signal[insertionPoint:l1] + noise[1:a],
        noise[(a + 1):l2]
      )
    }
  }

  if (normalize) {
    m = max(abs(out))
    if (m > 0) out = out / m
  }
  out
}

#' @rdname mixAtSNR
#' @export
addVectors = mixAtSNR



#' Convert from (0, 1) to dB (Boersma 1993)
#' @noRd
#' @param x a vector of floats between 0 and 1 (exclusive, i.e. these are ratios)
#' @examples
#' soundgen:::zeroOne_to_dB(c(.1, .5, .75, .9, .95, .99, .999, .9999, 1, 15, -2))
zeroOne_to_dB = function(x) {
  x[x <= 0 | x >= 1] = NA
  10 * log10(x / (1 - x))
}


#' Normalize 0 to 1
#'
#' Edge cases: returns 0 if all inputs are 0, 1 if all inputs are the same (any
#' value other than 0).
#' @noRd
#' @param x numeric vector or matrix
#' @param xmin,xmax min and max (to save time if already known)
#' @examples
#' soundgen:::zeroOne(rnorm(5))
#' soundgen:::zeroOne(0:5)
#' soundgen:::zeroOne(c(0, 0, 0))  # 0 0 0
#' soundgen:::zeroOne(c(5, 5, 5))  # 1 1 1
zeroOne = function(x, xmin = NULL, xmax = NULL) {
  if (all(is.na(x))) return(x)
  if (is.null(xmin) || is.null(xmax)) ran = range(x, na.rm = TRUE, finite = TRUE)
  if (is.null(xmin)) xmin = ran[1]
  if (is.null(xmax)) xmax = ran[2]
  if (xmin == xmax) {
    if (xmin == 0) return(x) else return(x / xmax)
  }
  (x - xmin) / (xmax - xmin)
}


#' log01
#' Normalizes, log-transforms, and re-normalizes an input vector, so it ranges
#' from 0 to 1
#' @param v numeric vector
#' @noRd
#' @examples
#' plot(soundgen:::log01(exp(1:10)), type = 'b')
#' hist(soundgen:::log01(exp(rnorm(100))))
log01 = function(v) {
  v = log(v - min(v) + 1)
  zeroOne(v)
}


#' Random draw from a truncated normal distribution
#'
#' \code{rnorm_truncated} generates random numbers from a normal distribution
#' using rnorm(), but forced to remain within the specified low/high bounds. All
#' proposals outside the boundaries (exclusive) are discarded, and the sampling
#' is repeated until there are enough values within the specified range. Fully
#' vectorized. Note: "truncnorm::truncnorm" is much faster, but it only accepts
#' static low/high boundaries. \code{rnorm_truncated2} is a simplified version
#' of \code{rnorm_truncated}, in which values outside the bounds are simply
#' reset to the low/high bounds. The shape of the resulting distribution is no
#' longer Gaussian, but this is obviously much faster. Unlike in
#' \code{rnorm_truncated}, "low" and "high" should be scalars, not vectors (ie
#' static boundaries).
#'
#' @param n the number of values to return
#' @param mean the mean of the normal distribution from which values are
#'   generated (vector of length 1 or n)
#' @param sd the standard deviation of the normal distribution from which values
#'   are generated (vector of length 1 or n)
#' @param low,high exclusive lower and upper bounds ((vectors of length 1 or n))
#' @param roundToInteger boolean vector of length 1 or n. If TRUE, the
#'   corresponding value is rounded to the nearest integer.
#' @return A vector of length n.
#' @keywords internal
#' @examples
#' soundgen:::rnorm_truncated(n = 3, mean = 10, sd = 5, low = 7, high = NULL,
#'   roundToInteger = c(TRUE, FALSE, FALSE))
#' soundgen:::rnorm_truncated(n = 9, mean = c(10, 50, 100), sd = c(5, 0, 20),
#'   roundToInteger = TRUE) # vectorized
#' # in case of conflicts between mean and bounds, either adjust the mean:
#' soundgen:::rnorm_truncated(n = 3, mean = 10, sd = .1,
#'   low = c(15, 0, 0), high = c(100, 100, 8), invalidArgAction = 'adjust')
#' #... or ignore the boundaries
#' soundgen:::rnorm_truncated(n = 3, mean = 10, sd = .1,
#'   low = c(15, 0, 0), high = c(100, 100, 8), invalidArgAction = 'ignore')
#'
#' hist(soundgen:::rnorm_truncated2(n = 100, mean = 10, sd = 5, low = 7, high = NULL,
#'   roundToInteger = c(TRUE, FALSE, FALSE)))
#' hist(soundgen:::rnorm_truncated2(n = 100, mean = c(10, 50, 100), sd = c(5, 0, 20),
#'   roundToInteger = TRUE)) # vectorized
#' # in case of conflicts between mean and bounds, either sample at random
#' # between the boundaries...
#' hist(soundgen:::rnorm_truncated2(n = 100, mean = 10, sd = .1,
#'   low = 10, high = 15, invalidArgAction = 'adjust'))
#' #... or ignore the boundaries
#' hist(soundgen:::rnorm_truncated2(n = 100, mean = 10, sd = .1,
#'   low = 15, high = 100, invalidArgAction = 'ignore'))
#' soundgen:::rnorm_truncated2(n = 6, mean = c(0, 0, 0, 0, 0, 3),
#'   sd = .05, low = 0, high = 6)
rnorm_truncated = function(
    n = 1,
    mean = 0,
    sd = 1,
    low = NULL,
    high = NULL,
    roundToInteger = FALSE,
    invalidArgAction = 'adjust') {
  len_mean = length(mean)
  if (len_mean == 1) {
    mean = rep(mean, n)
  } else {
    mean = approx(mean, n = n)$y
  }

  if (any(!is.finite(sd))) {
    sd = abs(mean) / 10
  } else {
    len_sd = length(sd)
    if (len_sd < n) {
      if (len_sd == 1) {
        sd = rep(sd, n)
      } else {
        sd = approx(sd, n = n)$y
      }
    }
  }
  sd[sd < 0] = 0

  if (sum(sd) == 0) {
    out = mean
    out[roundToInteger] = round(out[roundToInteger], 0)
    return(out)
  }

  if (is.null(low) && is.null(high)) {
    out = rnorm(n, mean, sd)
    out[roundToInteger] = round(out[roundToInteger], 0)
    return(out)
  }

  if (is.null(low)) low = rep(-Inf, n)
  if (is.null(high)) high = rep(Inf, n)
  if (length(low) == 1) low = rep(low, n)
  if (length(high) == 1) high = rep(high, n)


  if (any((mean > high | mean < low), na.rm = TRUE)) {
    warning(paste('Some of the specified means are outside the low/high bounds!',
                  'Mean =', paste(head(mean, 3), collapse = ', '), '...',
                  'Low =', paste(head(low, 3), collapse = ', '), '...',
                  'High = ', paste(head(high, 3), collapse = ', '),  '...'))
    if (invalidArgAction == 'abort') {
      stop('Aborting rnorm_truncated()')
    } else if (invalidArgAction == 'adjust') {
      mean[mean < low] = low[mean < low]
      mean[mean > high] = high[mean > high]
    } else if (invalidArgAction == 'ignore') {
      low = rep(-Inf, n)
      high = rep(Inf, n)
    }
  }

  if (any(low >= high, na.rm = TRUE))
    stop("low must be strictly less than high")
  if (any(sd == 0 & (mean < low | mean > high), na.rm = TRUE))
    stop("Cannot generate values: sd is 0 and mean is outside bounds.")
  out = rnorm(n, mean, sd)
  for (i in seq_len(n)) {
    while (out[i] < low[i] || out[i] > high[i]) {
      out[i] = rnorm(1, mean[i], sd[i]) # repeat until a suitable value is generated
    }
  }
  out[roundToInteger] = round(out[roundToInteger], 0)
  out
}


#' @rdname rnorm_truncated
#' @keywords internal
rnorm_truncated2 = function(
    n = 1,
    mean = 0,
    sd = 1,
    low = NULL,
    high = NULL,
    roundToInteger = FALSE,
    invalidArgAction = 'adjust') {
  len_mean = length(mean)
  len_sd = length(sd)

  if (len_mean != n)
    mean = if (len_mean == 1) rep(mean, n) else approx(mean, n = n)$y
  if (len_sd != n)
    sd = if (len_sd == 1) rep(sd, n) else approx(sd, n = n)$y
  sd[sd < 0] = 0
  out = rnorm(n, mean, sd)

  if (invalidArgAction != 'ignore' && (!is.null(low) || !is.null(high))) {
    if (is.null(low)) low = -1e6
    if (is.null(high)) high = 1e6
    if (invalidArgAction == 'abort') {
      if (any((mean > high | mean < low), na.rm = TRUE)) {
        stop('Some of the specified means are outside the low/high bounds; aborting...')
      }
    } else if (invalidArgAction == 'adjust') {
      out[out <= low] = low
      out[out >= high] = high
    }
  }
  out[roundToInteger] = round(out[roundToInteger], 0)
  out
}


#' Modified mode
#' Internal helper function for spectral (~BaNa) pitch tracker. NOT quite the
#' same as simply mode(x).
#' @param x numeric vector
#' @noRd
#' @examples
#' soundgen:::Mode(c(1, 2, 3))  # if every element is unique, return the smallest
#' soundgen:::Mode(c(1, 2, 2, 3))
Mode = function(x) {
  x = sort(x)
  ux = unique(x)
  if (length(ux) < length(x)) {
    return(ux[which.max(tabulate(match(x, ux)))])
  } else {
    # if every element is unique, return the smallest
    return(x[1])
  }
}


#' Random walk
#'
#' Generates a random walk with flexible control over its range, trend, and
#' smoothness. It works by calling stats::rnorm at each step and taking a
#' cumulative sum of the generated values. Smoothness is controlled by initially
#' generating a shorter random walk and upsampling.
#' @param len an integer specifying the required length of random walk. If len
#'   is 1, returns a single draw from a gamma distribution with mean=1 and
#'   sd=rw_range
#' @param rw_range the upper bound of the generated random walk (the lower bound
#'   is set to 0)
#' @param rw_smoothing specifies the amount of smoothing, basically the number
#'   of points used to construct the rw as a proportion of len, from 0 (no
#'   smoothing) to 1 (maximum smoothing to a straight line)
#' @param method specifies the method of smoothing: either linear interpolation
#'   ('linear', see stats::approx) or cubic splines ('spline', see
#'   stats::spline)
#' @param trend mean of generated normal distribution (vectors are also
#'   acceptable, as long as their length is an integer multiple of len). If
#'   positive, the random walk has an overall upwards trend (good values are
#'   between 0 and 0.5 or -0.5). Trend = c(1,-1) gives a roughly bell-shaped rw
#'   with an upward and a downward curve. Larger absolute values of trend
#'   produce less and less random behavior
#' @return A numeric vector of length len and range from 0 to rw_range.
#' @keywords internal
#' @examples
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = 0))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = .2))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = .95))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = .99))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = 1))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 15,
#'   rw_smoothing = .2, trend = c(.1, -.1)))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 15,
#'   rw_smoothing = .2, trend = c(15, -1)))
getRandomWalk = function(len,
                         rw_range = 1,
                         rw_smoothing = .2,
                         method = c('linear', 'spline')[2],
                         trend = 0) {
  if (len < 2)
    return(rgamma(1, 1 / rw_range ^ 2, 1 / rw_range ^ 2))
  rw_smoothing[rw_smoothing < 0] = 0
  rw_smoothing[rw_smoothing > 1] = 1

  # generate a random walk (rw) of length depending on rw_smoothing,
  # then linear extrapolation to len
  # n = floor(max(2, 2 ^ (1 / rw_smoothing)))
  # n = 2 + (len - 1) / (1 + exp(10 * (rw_smoothing - .1)))
  n = max(2, len * (1 - rw_smoothing))
  if (length(trend) > 1) {
    n = round(n / 2, 0) * 2 # force to be even
    trend_short = rep(trend, each = n / length(trend))
    # for this to work, length(trend) must be a multiple of n.
    # In practice, specify trend of length 2
  } else {
    trend_short = trend
  }

  if (n > len) {
    rw_long = cumsum(rnorm(len, trend_short)) # just a rw of length /len/
  } else {
    # get a shorter sequence and extrapolate, thus achieving
    # more or less smoothing
    rw_short = cumsum(rnorm(n, trend_short)) # plot(rw_short, type = 'l')
    if (method == 'linear') {
      rw_long = approx(rw_short, n = len)$y
    } else if (method == 'spline') {
      rw_long = spline(rw_short, n = len)$y
    }
  } # plot (rw_long, type = 'l')

  # normalize
  rw_normalized = rw_long - min(rw_long)
  rw_normalized / max(abs(rw_normalized)) * rw_range
}


#' Discrete random walk
#'
#' Takes a continuous random walk and converts it to continuous epochs of
#' repeated values 0/1/2, each at least minLength points long. 0/1/2 correspond
#' to different noise regimes: 0 = no noise, 1 = subharmonics, 2 = subharmonics
#' and jitter/shimmer.
#' @param rw a random walk generated by \code{\link{getRandomWalk}} (expected
#'   range 0 to 100)
#' @param nonlinBalance a number between 0 to 100: 0 = returns all zeros;
#'   100 = returns all twos
#' @param minLength the mimimum length of each epoch
#' @param q1,q2 cutoff points for transitioning from regime 0 to 1 (q1) or from
#'   regime 1 to 2 (q2). See noiseThresholdsDict for defaults
#' @param plot if TRUE, plots the random walk underlying nonlinear regimes
#' @return A vector of integers (0/1/2) of the same length as rw.
#' @keywords internal
#' @examples
#' rw = soundgen:::getRandomWalk(len = 100, rw_range = 100, rw_smoothing = .2)
#' r = soundgen:::getIntegerRandomWalk(rw, nonlinBalance = 75,
#'                          minLength = 10, plot = TRUE)
#' r = soundgen:::getIntegerRandomWalk(rw, nonlinBalance = 15,
#'                          q1 = 30, q2 = 70,
#'                          minLength = 10, plot = TRUE)
getIntegerRandomWalk = function(rw,
                                nonlinBalance = 50,
                                minLength = 50,
                                q1 = NULL,
                                q2 = NULL,
                                plot = FALSE) {
  len = length(rw)

  # calculate thresholds for different noise regimes
  if (is.null(q1)) {
    q1 = noiseThresholdsDict$q1[nonlinBalance + 1]
    # +1 b/c the rows indices in noiseThresholdsDict start from 0, not 1
  }
  if (is.null(q2)) {
    q2 = noiseThresholdsDict$q2[nonlinBalance + 1]
  }

  if (nonlinBalance == 0) {
    rw_bin = rep(0, len)
  } else if (nonlinBalance == 100) {
    rw_bin = rep(2, len)
  } else {
    # convert continuous rw to discrete epochs based on q1 and q2 thresholds
    rw_bin = rep(0, len)
    rw_bin[which(rw > q1)] = 1
    rw_bin[which(rw > q2)] = 2   # plot (rw_bin, ylim=c(0,2))

    # make sure each epoch is long enough
    rw_bin = clumper(rw_bin, minLength = minLength)
  }

  if (plot) {
    rw_bin_100 = rw_bin
    rw_bin_100[rw_bin_100 == 1] = q1
    rw_bin_100[rw_bin_100 == 2] = q2

    plot(rw, ylim = c(0, 110), type = 'l', lwd = 1,
         xlab = 'Time', ylab = 'Latent non-linearity', main = 'Random walk')
    points(rw_bin_100, type = 'l', lwd = 4, col = 'blue')
    abline(h = q1, lty = 3, lwd = 2, col = 'red')
    text(x = 1, y = q1 + 2, labels = 'subh', pos = 4)
    abline(h = q2, lty = 3, lwd = 2, col = 'red')
    text(x = 1, y = q2 + 2, labels = 'subh + jitter', pos = 4)
  }
  rw_bin
}


#' Resize vector to required length
#' Adjusts a vector to match the required length by either trimming one or both
#' ends or padding them with zeros.
#' @param myseq input vector
#' @param len target length
#' @param padDir specifies the affected side. For padding, it is the side on
#'   which new elements will be added. For trimming, this is the side that will
#'   be trimmed. Defaults to 'central'
#' @param padWith if the vector needs to be padded to match the required length,
#'   what should it be padded with? Defaults to 0
#' @return Returns the modified vector of the required length.
#' @noRd
#' @examples
#' soundgen:::matchLengths(1:3, len = 5)
#' soundgen:::matchLengths(1:3, len = 15, padWith = NA)
#' soundgen:::matchLengths(3:7, len = 3)
#' # trimmed on the left
#' soundgen:::matchLengths(3:7, len = 3, padDir = 'left')
#' # padded with zeros on the left
#' soundgen:::matchLengths(3:7, len = 10, padDir = 'left')
#' #' # trimmed on the right
#' soundgen:::matchLengths(3:7, len = 3, padDir = 'right')
#' # padded with zeros on the right
#' soundgen:::matchLengths(3:7, len = 10, padDir = 'right')
matchLengths = function(myseq,
                        len,
                        padDir = c('left', 'right', 'central')[3],
                        padWith = 0) {
  len_seq = length(myseq)
  if (len_seq == len) return(myseq)

  if (padDir == 'central') {
    if (len_seq < len) {
      half_len = (len - len_seq) / 2
      myseq = c(rep(padWith, floor(half_len)),
                myseq,
                rep(padWith, ceiling(half_len)))
    } else {
      half_chop = (len_seq - len) / 2
      left = seq_len(floor(half_chop))
      right = (len_seq - ceiling(half_chop) + 1) : len_seq
      myseq = myseq[-c(left, right)]
    }
  } else if (padDir == 'left') {
    if (len_seq > len) {
      myseq = myseq[(len_seq - len + 1):len_seq]
    } else {
      myseq = c(rep(padWith, (len - len_seq)), myseq)
    }
  } else if (padDir == 'right') {
    if (len_seq > len) {
      myseq = myseq[seq_len(len)]
    } else {
      myseq = c(myseq, rep(padWith, (len - len_seq)))
    }
  }
  myseq
}


#' Clump a sequence into large segments
#'
#' \code{clumper} makes sure each homogeneous segment in a sequence is at least
#' minLength long. Called by getIntegerRandomWalk(), addSubh(), naiveBayes(),
#' etc. Algorithm: find the epochs shorter than minLength, merge max 1/4 of them
#' with the largest neighbor, and repeat recursively until all epochs are at
#' least minLength long. minLength can be a vector, in which case it is assumed
#' to change over time.
#'
#' @param x a vector: anything that can be converted into an integer to call
#'   diff(): factors, integers, characters, booleans
#' @param minLength the minimum length of a segment (interger or vector)
#' @return Returns the original sequence x transformed to homogeneous segments
#'   of required length, with the original class (e.g. character or factor).
#' @keywords internal
#' @examples
#' s = c(1,3,2,2,2,0,0,4,4,1,1,1,1,1,3,3)
#' soundgen:::clumper(s, 2)
#' soundgen:::clumper(s, 3)
#' soundgen:::clumper(1:5, 10)
#' soundgen:::clumper(c('a','a','a','b','b','c','c','c','a','c'), 3)
#' soundgen:::clumper(x = c(1,2,1,2,1,1,1,1,3,1), minLength = c(1, 1, 1, 3))
#' soundgen:::clumper(as.factor(c('A','B','B','C')), 2)
#'
#' ## clumper vs clumper2
#' # clumper2 is faster for wiggly inputs
#' s = rpois(1000, 2)
#' plot(s, type = 'l')
#' plot(soundgen:::clumper(s, minLength = c(2, 3, 10)), type = 'l')
#' plot(soundgen:::clumper2(s, minLength = c(2, 3, 10)), type = 'l')
#' system.time(for (i in 1:10) soundgen:::clumper(s, minLength = c(2, 3, 10)))
#' system.time(for (i in 1:10) soundgen:::clumper2(s, minLength = c(2, 3, 10)))
#'
#' # clumper1 is faster for step-like inputs
#' s2 = approx(c(2,4,3,2,0), n = 1001, method = 'constant')$y[1:1000]
#' plot(s2, type = 'l')
#' plot(soundgen:::clumper(s2, minLength = c(2, 3, 10)), type = 'l')
#' plot(soundgen:::clumper2(s2, minLength = c(2, 3, 10)), type = 'l')
#' system.time(for (i in 1:10) soundgen:::clumper(s2, minLength = c(2, 3, 10)))
#' system.time(for (i in 1:10) soundgen:::clumper2(s2, minLength = c(2, 3, 10)))
clumper = function(x, minLength, n = length(x)) {
  # print('calling self')
  mode_orig = mode(x)
  minLength = round(minLength)   # just in case it's not all integers
  if (max(minLength) < 2) return(x)
  n = length(x)
  if (n <= minLength[1]) {
    # just repeat the most common element
    tbx = table(x)
    mode_x = names(tbx)[which.max(tbx)]
    if (is.factor(x)) {
      x = factor(rep(mode_x, n), levels = levels(x))
    } else if (is.numeric(x)) {
      x = rep(as.numeric(mode_x), n)
    } else {
      x = rep(mode_x, n)
    }
    return(x)
  }

  # find indices of changes in the composition of x (epoch boundaries)
  if (inherits(x, 'character')) {
    x_int = as.integer(as.factor(x))
  } else {
    x_int = as.integer(x)
  }
  idx_change = which(diff(as.numeric(x_int)) != 0)
  if (length(idx_change) < 1) {
    return(x)
  }
  dur_epochs = diff(unique(c(0, idx_change, n)))
  n_epochs = length(dur_epochs)
  if (length(minLength) == 1) {
    minLength_ups = minLength
  } else {
    minLength_ups = round(approx(minLength, n = n_epochs)$y)
  }
  short_epochs = which(dur_epochs < minLength_ups)
  short_epochs = short_epochs[order(dur_epochs[short_epochs])]
  nse = length(short_epochs)

  # limit the number of epochs that are to be modified per iteration - here set
  # to max 1/4 of the total n_epochs (starting with the shortest - this is
  # crucial!)
  n_max = max(2, ceiling(n_epochs / 4))
  if (nse > n_max) {
    short_epochs = order(dur_epochs)[1:n_max]
    nse = n_max
  }
  if (nse > 0) {
    idx = c(1, idx_change + 1)
    for (e in short_epochs) {
      # calculate the indices within vector x corresponding to this short epoch
      if (e < n_epochs) {
        idx_epoch = idx[e]:(idx[e + 1] - 1)
      } else {
        idx_epoch = idx[e]:n
      }

      # merge the short epoch with the longer neighbor (ie replace original
      # elements with those of the longest adjacent epoch)
      dur_left = ifelse(e > 1, dur_epochs[e - 1], 0)
      dur_right = ifelse(e < n_epochs, dur_epochs[e + 1], 0)
      if (dur_left >= dur_right) {
        x[idx_epoch] = x[idx_epoch[1] - 1]
      } else if (dur_left < dur_right) {
        x[idx_epoch] = x[tail(idx_epoch, 1) + 1]
      }
    }
    # call clumper() recursively until no short epochs are left
    x = clumper(x, minLength, n = n)  # just to avoid recalculating n every time
  }
  x
}


#' @rdname clumper
#' @keywords internal
clumper2 = function(x, minLength, n = length(x)) {
  mode_orig = mode(x)
  # minLength = round(minLength)   # just in case it's not all integers
  if (max(minLength) < 2) return(x)
  n = length(x)
  if (n <= minLength[1]) {
    # just repeat the most common element
    tbx = table(x)
    mode_x = names(tbx)[which.max(tbx)]
    if (is.factor(x)) {
      x = factor(rep(mode_x, n), levels = levels(x))
    } else if (is.numeric(x)) {
      x = rep(as.numeric(mode_x), n)
    } else {
      x = rep(mode_x, n)
    }
    return(x)
  }

  if (length(minLength) == 1) {
    minLength_ups = rep(minLength, n)
  } else {
    if (length(minLength) != n) {
      minLength_ups = round(approx(minLength, n = n)$y)
    } else {
      minLength_ups = minLength
    }
  }

  counter = 1
  for (i in 2:n) {
    if (x[i] == x[i - 1]) {
      counter = counter + 1
    } else {
      if (counter < minLength_ups[i]) {
        x[i] = x[i - 1]
        counter = counter + 1
      } else {
        counter = 1
      }
    }
  }
  x
}


#' Get sigmoid filter
#' Produces a filter for amplitude modulation ranging from clicks to
#' approximately a sine wave to reversed clicks (small episodes of silence). The
#' filter is made from concatenated sigmoids and their mirror reflections.
#' @return Returns a vector of length \code{len} and range from 0 to 1.
#' @param len the length of output vector
#' @param samplingRate the sampling rate of the output vector, Hz
#' @param freq the frequency of amplitude modulation, Hz (numeric vector)
#' @param shape 0 = ~sine, -1 = clicks, +1 = notches (NB: vice versa in soundgen
#'   because AM is added as 1-sigmoid); numeric vector of length 1 or the same
#'   length as \code{freq}
#' @noRd
#' @examples
#' par(mfrow = c(3, 2))
#' for (shape in c(0, -.1, .1, .5, -1, 1)) {
#'   s = soundgen:::getSigmoid(shape = shape, len = 1000,
#'     samplingRate = 500, freq = 2)
#'   plot(s, type = 'l', main = paste('shape =', shape), xlab = '', ylab = '')
#' }
#' par(mfrow = c(1, 1))
getSigmoid = function(len,
                      samplingRate = 16000,
                      freq = 5,
                      shape = 0) {
  # print(c(len, freq))
  len_freq = length(freq)
  if (len_freq > 1 || length(shape) > 1) {
    # get preliminary frequency contour to estimate how many cycles are needed
    if (len_freq != len) {
      if (len_freq == 1) {
        freqContour_prelim = rep(freq, 100)
      } else {
        freqContour_prelim = approx(freq, n = 100)$y
      }
    } else {
      freqContour_prelim = freq
    }
    freqContour_prelim = pmax(0.001, freqContour_prelim)
    # plot(freqContour_prelim, type = 'l')
    n = ceiling(len / min(samplingRate / freqContour_prelim))

    # get actual contours
    if (len_freq == 1) {
      freqContour = rep(freq, n)
    } else {
      freqContour = spline(freq, n = n)$y
    }
    freqContour[freqContour < 0.001] = 0.001
    if (length(shape) == 1) {
      shapeContour = rep(shape, n)
    } else {
      shapeContour = spline(shape, n = n)$y
    }

    # set up par vectors
    from = -exp(-shapeContour)
    to = exp(shapeContour)
    slope = exp(abs(shapeContour)) * 5  # close to sine

    # create one cycle at a time
    out = vector('numeric', len)
    i = 1
    idx = 1
    while (TRUE) {
      len_i = max(2, round(samplingRate / freqContour[i] / 2) * 2)
      a = seq(from = from[i], to = to[i], length.out = len_i / 2)
      b = 1 / (1 + exp(-a * slope[i]))
      b = zeroOne(b)
      new_cycle = c(b, rev(b)) # plot(new_cycle, type = 'l')
      idx_end = idx + len_i - 1
      if (idx_end <= len) {
        out[idx:idx_end] = new_cycle
      } else {
        idx_end = len
        out[idx:idx_end] = new_cycle[1:(idx_end - idx + 1)]
        break
      }
      i = ifelse(i + 1 > n, n, i + 1)  # repeat the last value if we run out of cycles
      idx = idx + len_i
    }
    if (length(out) > len) out = out[1:len]
    # can happen if the last cycle ends exactly at n
  } else {
    # if pars are static, we can take a shortcut (~50 times faster)
    freq = max(0.001, freq)
    from = -exp(-shape)
    to = exp(shape)
    slope = exp(abs(shape)) * 5  # close to sine
    half_cycle = max(2, round(samplingRate / freq / 2))
    a = seq(from = from, to = to, length.out = half_cycle)
    b = 1 / (1 + exp(-a * slope))
    b = zeroOne(b)  # plot(b, type = 'l')
    out = rep(c(b, rev(b)), length.out = len)
  }
  # plot(out, type = 'l')
  out
}


#' sampleModif
#'
#' Same as \code{\link[base]{sample}}, but without defaulting to x = 1:x if
#' length(x) = 1. See
#' https://stackoverflow.com/questions/7547758/using-sample-with-sample-space-size-1
#' @param x vector
#' @param ... other arguments passed to \code{sample}
#' @noRd
#' @examples
#' soundgen:::sampleModif(x = 3, n = 1)
#' # never returns 1 or 2: cf. sample(x = 3, size = 1)
sampleModif = function(x, ...) x[sample.int(length(x), ...)]


#' Proportion of total
#'
#' Calculates values in the input distribution that contain particular
#' proportions of its sum.
#' @param x numeric vector of non-negative real numbers
#' @param quantiles thresholds of the cumulative distribution
#' @noRd
#' @examples
#' x = rnorm(100)
#' hist(x)
#' v = soundgen:::pDistr(x, quantiles = c(.5, .8, .9))
#' sum(x[x > v['0.5']]) / sum(x)
#' sum(x[x > v['0.9']]) / sum(x)
pDistr = function(x, quantiles) {
  if (length(x) == 0 || any(!is.finite(x))) return(NA)
  floor = min(x)
  x = x - floor  # must be non-negative
  a1 = rev(sort(x))  # plot(a1, type = 'l')
  a2 = cumsum(a1)  # plot(a2, type = 'l')
  total = sum(x)
  out = a1[findInterval(total * quantiles, a2) ]
  # same as for (q in seq_along(quantiles)) out[q] = a1[which(a2 > (total * quantiles[q]))[1]]
  names(out) = quantiles
  out + floor
}


#' Parabolic peak interpolation
#' Takes a spectral peak and two adjacent points, fits a parabola through them,
#' and thus estimates true peak location relative to the discrete peak. The
#' points are assumed to be equidistant. See
#' https://ccrma.stanford.edu/~jos/sasp/Quadratic_Interpolation_Spectral_Peaks.html
#' @param threePoints the amplitudes of three adjacent and equidistant points
#'   (beta is the peak), ideally spectrum on a dB scale, obtained with a
#'   Gaussian window
#' @param plot if TRUE, plot the points and the fit parabola
#' @return Returns a list: $p = the correction coefficient in bins (idx_beta + p
#'   gives the true peak) in the range [-.5, .5], $ampl_p = the amplitude of the
#'   true peak on the same scale as input.
#' @noRd
#' @examples
#' soundgen:::parabPeakInterpol(c(-1, 0, -4), plot = TRUE)
#' soundgen:::parabPeakInterpol(c(1, 3, 5), plot = TRUE)
#' soundgen:::parabPeakInterpol(c(3, 3, 3))
parabPeakInterpol = function(threePoints, plot = FALSE) {
  if (any(is.na(threePoints)) || length(threePoints) < 3) {
    stop(paste('invalid input:', threePoints))
  }
  alpha = threePoints[1]
  beta = threePoints[2]
  gamma = threePoints[3]
  if (beta < alpha ||
      beta < gamma ||
      (alpha == beta && beta == gamma)) {
    # the central point is not a peak
    return(list(p = 0, ampl_p = beta))
  }

  a = (alpha - 2 * beta + gamma) / 2
  if (a == 0) return(list(p = 0, ampl_p = beta))
  p = (alpha - gamma) / 4 / a
  ampl_p = beta - (alpha - gamma) * p / 4
  if (plot) {
    b = beta - a * p ^ 2
    x = seq(-2, 2, length.out = 100)
    parab = a * (x - p) ^ 2 + b
    plot(-1:1, c(alpha, beta, gamma), type = 'p',
         xlim = c(-2, 2), ylim = c(gamma, ampl_p),
         xlab = 'Bin', ylab = 'Ampl')
    points(x, parab, type = 'l', col = 'blue')
  }
  list(p = p, ampl_p = ampl_p)
}


#' Principal argument
#' Recalculates the phase of complex numbers to be within the interval from -pi to pi.
#' @param x real number representing phase angle
#' @noRd
princarg = function(x) (x + pi) %% (2 * pi) - pi


#' Pseudolog
#' From function pseudo_log_trans() in the "scales" package.
#' @param x numeric vector to transform
#' @param sigma scaling factor for the linear part
#' @param base approximate logarithm base used
#' @noRd
#' @examples
#' a = -30:30
#' plot(a, soundgen:::pseudoLog(a, sigma = 1, base = 2))
#' plot(a, soundgen:::pseudoLog(a, sigma = 5, base = 2))
#' plot(a, soundgen:::pseudoLog(a, sigma = .1, base = 2))
pseudoLog = function(x, sigma, base) {
  asinh(x/(2 * sigma))/log(base)
}


#' Undo pseudolog
#' From function pseudo_log_trans() in the "scales" package.
#' @noRd
pseudoLog_undo = function(x, sigma, base) {
  2 * sigma * sinh(x * log(base))
}


#' Find the elbow of a screeplot or similar
#'
#' Adapted from
#' https://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
#' Algorithm: draw a straight line between the two endpoints and find the point
#' furthest from this line.
#'
#' @param d dataframe containing x and y coordinates of the points
#' @noRd
#' @examples
#' y = c(10, 11, 8, 4, 2, 1.5, 1, 0.7, .5, .4, .3)
#' soundgen:::findElbow(data.frame(x = seq_along(y), y = y), plot = TRUE)
findElbow = function(d, plot = FALSE) {
  d = na.omit(d)
  n = nrow(d)
  if (n < 2) return(NA)

  # draw a straight line between the endpoints
  x1n = d$x[c(1, n)]
  y1n = d$y[c(1, n)]

  # calculate distances from points to line
  # https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
  if (diff(x1n) == 0) {
    distances = abs(d$x - x1n[1])
  } else {
    slope = diff(y1n) / diff(x1n)
    intercept = y1n[1] - slope * x1n[1]
    distances = abs(slope * d$x - d$y + intercept) / sqrt(slope^2 + 1)
  }
  out = which.max(distances)

  if (plot) {
    plot(d, type = 'b')
    abline(intercept, slope, col = 'blue', lty = 2)
    # segment to the closest point on the line (again from wiki)
    a = -slope; b = 1; c = -intercept
    x0 = d$x[out]; y0 = d$y[out]
    x1 = (b * (b * x0 - a * y0) - a * c) / (a^2 + b^2)
    y1 = (a * (-b * x0 + a * y0) - b * c) / (a^2 + b^2)
    segments(x0, y0, x1, y1, lty = 3)
    # NB: the angle only looks straight if the plot is square!
  }
  out
}


#' Logistic
#' @param x numeric vector
#' @noRd
logistic = function(x) 1 / (1 + exp(-x))


#' Logit
#' @param x numeric vector
#' @noRd
logit = function(x) log(x / (1 - x))


#' Sinc
#' @noRd
#' @examples
#' x = seq(-5, 5, .01); plot(x, soundgen:::sinc(x))
sinc = function(x) {
  out = sinpi(x) / (pi * x)
  out[x == 0] = 1
  out
}


#' Trim leading and trailing NAs
#' Slightly slower solution: \code{a[!cumprod(is.na(a)) &
#' rev(!cumprod(is.na(rev(a))))]}. See
#' https://stackoverflow.com/questions/42759027/remove-leading-and-trailing-na
#' @param x numeric vector
#' @noRd
#' @examples
#' soundgen:::na.trim(c(NA, NA, 1:10, NA, 21:25, NA))
na.trim = function(x) {
  idx_notNA = which(!is.na(x))
  if (length(idx_notNA) > 0) {
    x[min(idx_notNA):max(idx_notNA)]
  } else {
    x
  }

}


#' Normalized half-Gaussian window
#'
#' Helper function for creating normalized half-Gaussian density distributions
#' that sum to one. Called by getSurprisal_matrix() and getSurprisal_vector().
#' @param n output length
#' @param q lower bound in SD units
#' @noRd
#' @examples
#' soundgen:::normHalfGaus(1)
#' soundgen:::normHalfGaus(2)
#' soundgen:::normHalfGaus(5)
#' plot(soundgen:::normHalfGaus(150), type = 'l')
normHalfGaus = function(n, q = -3) {
  if (n == 1) {
    win = 1
  } else if (n == 2) {
    win = rep(0.5, 2)
  } else {
    win = dnorm(seq(q, 0, length.out = n))
    win = win / sum(win)
  }
  win
}


#' Log-transform values above floor
#' Normalized x to [0, ...) --> sets everything below thres to thres -->
#' log-transforms --> again normalizes to [0, ...).
#' @param x numeric vector with or without NAs
#' @param takeLog if TRUE, log-transforms x
#' @param floor everything below floor after normalizing by -min(x) is set to
#'   zero
#' @param dynamicRange dynamic range (dB) relative to the scale of x; if
#'   specified, overrides floor
#' @noRd
#' @examples
#' x = c(NA, -3, -2.999, -2.5, 0, 1)
#' soundgen:::floor_log(x, takeLog = FALSE)
#' soundgen:::floor_log(x, takeLog = FALSE, thres = 0.01)
#' soundgen:::floor_log(x, thres = 0.01)
#' soundgen:::floor_log(x, dynamicRange = 6)
#' soundgen:::floor_log(x, dynamicRange = 60)
#' soundgen:::floor_log(x, dynamicRange = 120)
floor_log = function(x, takeLog = TRUE, thres = NULL, dynamicRange = NULL) {
  if (all(is.na(x))) return(x)
  ran = range(x, na.rm = TRUE)
  if (any(!is.finite(ran))) return(x)
  x = x - ran[1]
  if (!is.null(dynamicRange) && is.null(thres))
    thres = ran[2] / 10^(dynamicRange / 20)
  apply_thres = !is.null(thres) && length(thres) == 1 && is.finite(thres)
  if (apply_thres) {
    x = pmax(x, thres)
  } else {
    thres = 0
  }
  if (takeLog) {
    if (apply_thres) log(x) - log(thres)
    else (log(x))
  } else {
    if (apply_thres) x - thres
    else x
  }
}
