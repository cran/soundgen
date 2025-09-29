### UTILITIES FOR LOW-LEVEL MATH ###

#' Report time
#'
#' Provides a nicely formatted "estimated time left" in loops plus a summary
#' upon completion.
#' @param i current iteration
#' @param time_start time when the loop started running
#' @param nIter total number of iterations
#' @param reportEvery report progress every n iterations
#' @param jobs vector of length \code{nIter} specifying the relative difficulty
#'   of each iteration. If not NULL, estimated time left takes into account
#'   whether the jobs ahead will take more or less time than the jobs already
#'   completed
#' @param prefix a string to print before "Done...", eg "Chain 1: "
#' @export
#' @examples
#' time_start = proc.time()
#' nIter = 100
#' for (i in 1:nIter) {
#'   Sys.sleep(i ^ 1.02 / 10000)
#'   reportTime(i, time_start, nIter,
#'     jobs = (1:100) ^ 1.02, prefix = 'Chain 1: ')
#' }
#' \dontrun{
#' # Unknown number of iterations:
#' time_start = proc.time()
#' for (i in 1:20) {
#'   Sys.sleep(i ^ 2 / 10000)
#'   reportTime(i = i, time_start = time_start,
#'   jobs = (1:20) ^ 2, reportEvery = 5)
#' }
#'
#' # when analyzing a bunch of audio files, their size is a good estimate
#' # of how long each will take to process
#' time_start = proc.time()
#' filenames = list.files('~/Downloads/temp', pattern = "*.wav|.mp3",
#'   full.names = TRUE)
#' filesizes = file.info(filenames)$size
#' for (i in seq_along(filenames)) {
#'   # ...do what you have to do with each file...
#'   reportTime(i = i, time_start = time_start, nIter = length(filenames),
#'              jobs = filesizes)
#' }
#' }
reportTime = function(
    i,
    time_start,
    nIter = NULL,
    reportEvery = NULL,
    jobs = NULL,
    prefix = ''
) {
  time_diff = as.numeric((proc.time() - time_start)[3])
  if (is.null(reportEvery))
    reportEvery = ifelse(is.null(nIter),
                         1,
                         max(1, 10 ^ (floor(log10(nIter)) - 1)))
  if (is.null(nIter)) {
    # number of iter unknown, so we just report time elapsed
    if (i %% reportEvery == 0) {
      print(paste0(prefix, 'Completed ', i, ' iterations in ',
                   convert_sec_to_hms(time_diff)))
    }
  } else {
    # we know how many iter, so we also report time left
    if (i == nIter) {
      time_total = convert_sec_to_hms(time_diff)
      print(paste0(prefix, 'Completed ', i, ' iterations in ', time_total, '.'))
    } else {
      if (i %% reportEvery == 0 || i == 1) {
        if (is.null(jobs)) {
          # simply count iterations
          time_left = time_diff / i * (nIter - i)
        } else {
          # take into account the expected time for each iteration
          speed = time_diff / sum(jobs[1:i])
          time_left = speed * sum(jobs[min((i + 1), nIter):nIter])
        }
        time_left_hms = convert_sec_to_hms(time_left)
        print(paste0(prefix, 'Done ', i, ' / ', nIter, '; Estimated time left: ', time_left_hms))
      }
    }
  }
}


#' Print time
#'
#' Internal soundgen function.
#'
#' Converts time in seconds to time in y m d h min s for pretty printing.
#' @param time_s time (s)
#' @param digits number of digits to preserve for s (1-60 s)
#' @return Returns a character string like "1 h 20 min 3 s"
#' @keywords internal
#' @examples
#' time_s = c(.0001, .01, .33, .8, 2.135, 5.4, 12, 250, 3721, 10000,
#'            150000, 365 * 24 * 3600 + 35 * 24 * 3600 + 3721)
#' soundgen:::convert_sec_to_hms(time_s)
#' soundgen:::convert_sec_to_hms(time_s, 2)
convert_sec_to_hms = function(time_s, digits = 0) {
  if (!any(time_s > 1)) {
    output = paste(round(time_s * 1000), 'ms')
  } else {
    len = length(time_s)
    output = vector('character', len)
    for (i in 1:len) {
      # years = time_s %/% 31536000
      # years_string = ifelse(years > 0, paste(years, 'y '), '')
      #
      # months = time_s %/% 2592000 - years * 12
      # months_string = ifelse(months > 0, paste(months, 'm '), '')
      days_string = hours_string = minutes_string = seconds_string = ms_string = ''
      days = time_s[i] %/% 86400
      if (days > 0) days_string = paste(days, 'd ')

      hours = time_s[i] %/% 3600 - days * 24
      if (hours > 0) hours_string = paste(hours, 'h ')

      if (days == 0) {
        minutes = time_s[i] %/% 60 - days * 1440 - hours * 60
        if (minutes > 0) minutes_string = paste(minutes, 'min ')

        if (hours == 0) {
          seconds = time_s[i] - days * 86400 - hours * 3600 - minutes * 60
          seconds_floor = floor(seconds)
          if (seconds_floor > 0) seconds_string = paste(round(seconds, digits), 's ')

          if (minutes == 0 & seconds_floor == 0) {
            ms = (time_s[i] %% 1) * 1000
            if (ms > 0) ms_string = paste(ms, 'ms')
          }
        }
      }
      output[i] = paste0(days_string, hours_string,
                         minutes_string, seconds_string, ms_string)
    }
  }
  trimws(output)
}


#' Find peaks
#'
#' A bare-bones, very fast function to find local maxima (peaks) in a numeric
#' vector.
#'
#' @seealso \code{\link{findInflections}}
#'
#' @param x numeric vector
#' @param wl rolling window over which we look for maxima: central value ±
#'   floor(wl/2), eg ±1 if wl=3
#' @param thres required absolute value of each peak
#' @return Returns a vector with indices of local maxima
#' @export
#' @examples
#' x = rnorm(100)
#' findPeaks(x, wl = 3)
#' findPeaks(x, wl = 3, thres = 1)
#' findPeaks(x, wl = 5)
#' idx = findPeaks(x, wl = 5, thres = 1)
#' plot(x, type = 'b'); abline(h = 1, lty = 3)
#' points(idx, x[idx], col = 'blue', pch = 8)
findPeaks = function(x, wl = 3, thres = NULL) {
  halfwl = floor(wl / 2)
  if (halfwl == 1) {
    if (is.null(thres)) {
      which(diff(diff(x) > 0) == -1) + 1
    } else {
      idx = which(diff(diff(x) > 0) == -1) + 1
      idx[which(x[idx] > thres)]
    }
  } else if (halfwl > 1) {
    if (is.null(thres)) {
      which(vapply(
        (halfwl + 1):(length(x) - halfwl), function(i) {
          frx = x[(i - halfwl):(i + halfwl)]
          x[i] == max(frx)
        }, logical(1))) + halfwl
    } else {
      which(vapply(
        (halfwl + 1):(length(x) - halfwl), function(i) {
          frx = x[(i - halfwl):(i + halfwl)]
          x[i] > thres && x[i] == max(frx)
        }, logical(1))) + halfwl
    }
  } else {
    numeric(0)
  }
}


#' Convert to dB
#'
#' Internal soundgen function.
#' @param x a vector of floats between 0 and 1 (exclusive, i.e. these are ratios)
#' @keywords internal
#' @examples
#' soundgen:::to_dB(c(.1, .5, .75, .9, .95, .99, .999, .9999))
to_dB = function(x) {
  10 * log10(x / (1 - x))
}


#' Normalize 0 to 1
#'
#' Internal soundgen function
#'
#' Normalized input vector to range from 0 to 1
#' @param x numeric vector or matrix
#' @param na.rm if TRUE, remove NA's when calculating min/max for normalization
#' @param xmin,xmax min and max (to save time if already known)
#' @keywords internal
zeroOne = function(x, na.rm = FALSE, xmin = NULL, xmax = NULL) {
  if (is.null(xmin) | is.null(xmax)) ran = range(x, na.rm = TRUE, finite = TRUE)
  if (is.null(xmin)) xmin = ran[1] # min(x, na.rm = na.rm)
  if (is.null(xmax)) xmax = ran[2] # max(x, na.rm = na.rm)
  (x - xmin) / (xmax - xmin)
}


#' log01
#'
#' Internal soundgen function
#'
#' Normalizes, log-transforms, and re-normalizes an input vector, so it ranges
#' from 0 to 1
#' @param v numeric vector
#' @keywords internal
#' @examples
#' soundgen:::log01(exp(1:10))
log01 = function(v) {
  v = v - min(v) + 1
  v = log(v)
  zeroOne(v)
}


#' Entropy
#'
#' Returns Weiner or Shannon entropy of an input vector such as the spectrum of
#' a sound. Non-positive input values are converted to a small positive number
#' (convertNonPositive). If all elements are zero, returns NA.
#'
#' @param x vector of positive floats
#' @param type 'shannon' for Shannon (information) entropy, 'weiner' for Weiner
#'   entropy
#' @param normalize if TRUE, Shannon entropy is normalized by the length of
#'   input vector to range from 0 to 1. It has no affect on Weiner entropy
#' @param convertNonPositive all non-positive values are converted to
#'   \code{convertNonPositive}
#' @export
#' @examples
#' # Here are four simplified power spectra, each with 9 frequency bins:
#' s = list(
#'   c(rep(0, 4), 1, rep(0, 4)),       # a single peak in spectrum
#'   c(0, 0, 1, 0, 0, .75, 0, 0, .5),  # perfectly periodic, with 3 harmonics
#'   rep(0, 9),                        # a silent frame
#'   rep(1, 9)                         # white noise
#' )
#'
#' # Weiner entropy is ~0 for periodic, NA for silent, 1 for white noise
#' sapply(s, function(x) round(getEntropy(x), 2))
#'
#' # Shannon entropy is ~0 for periodic with a single harmonic, moderate for
#' # periodic with multiple harmonics, NA for silent, highest for white noise
#' sapply(s, function(x) round(getEntropy(x, type = 'shannon'), 2))
#'
#' # Normalized Shannon entropy - same but forced to be 0 to 1
#' sapply(s, function(x) round(getEntropy(x,
#'   type = 'shannon', normalize = TRUE), 2))
getEntropy = function(x,
                      type = c('weiner', 'shannon')[1],
                      normalize = FALSE,
                      convertNonPositive = 1e-10) {
  sum_x = sum(x)
  if (sum_x == 0) return (NA)  # empty frames
  x[x <= 0] = convertNonPositive  # otherwise log0 gives NaN
  if (type == 'weiner') {
    geom_mean = exp(mean(log(x)))
    ar_mean = sum_x / length(x)
    entropy = geom_mean / ar_mean
  } else if (type == 'shannon') {
    p = x / sum_x
    if (normalize) {
      entropy = -sum(p * log2(p) / log(length(p)) * log(2))
    } else {  # unnormalized
      entropy = -sum(p * log2(p))
    }
  } else {
    stop('Implemented entropy types: "shannon" or "weiner"')
  }
  entropy
}


#' Random draw from a truncated normal distribution
#'
#' \code{rnorm_truncated} generates random numbers from a normal distribution
#' using rnorm(), but forced to remain within the specified low/high bounds. All
#' proposals outside the boundaries (exclusive) are discarded, and the sampling
#' is repeated until there are enough values within the specified range. Fully
#' vectorized. Note: "truncnorm::truncnorm" is much faster, but it only accepts
#' static low/high boundaries.
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
rnorm_truncated = function(
    n = 1,
    mean = 0,
    sd = 1,
    low = NULL,
    high = NULL,
    roundToInteger = FALSE,
    invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  len_mean = length(mean)
  if (len_mean == 1) {
    mean = rep(mean, n)
  } else {
    mean = approx(mean, n = n)$y
  }

  if (any(!is.finite(sd))) {
    sd = mean / 10
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

  if (is.null(low) & is.null(high)) {
    out = rnorm(n, mean, sd)
    out[roundToInteger] = round(out[roundToInteger], 0)
    return(out)
  }

  if (is.null(low)) low = rep(-Inf, n)
  if (is.null(high)) high = rep(Inf, n)
  if (length(low) == 1) low = rep(low, n)
  if (length(high) == 1) high = rep(high, n)


  if (any(mean > high | mean < low)) {
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

  out = rnorm(n, mean, sd)
  out[roundToInteger] = round(out[roundToInteger], 0)
  for (i in seq_len(n)) {
    while (out[i] < low[i] | out[i] > high[i]) {
      out[i] = rnorm(1, mean[i], sd[i]) # repeat until a suitable value is generated
      out[roundToInteger] = round(out[roundToInteger], 0)
    }
  }
  out
}


#' Random draw from a truncated normal distribution
#'
#' A simplified version of \code{rnorm_truncated}, in which values outside the
#' bounds are simply reset to the low/high bounds. The shape of the resulting
#' distribution is no longer Gaussian, but this is obviously much faster. Unlike
#' in \code{rnorm_truncated}, "low" and "high" should be scalars, not vectors
#' (ie static boundaries).
#'
#' @param n the number of values to return
#' @param mean the mean of the normal distribution from which values are
#'   generated (vector of length 1 or n)
#' @param sd the standard deviation of the normal distribution from which values
#'   are generated (vector of length 1 or n)
#' @param low,high exclusive lower and upper bounds (both of length 1)
#' @param roundToInteger boolean vector of length 1 or n. If TRUE, the
#'   corresponding value is rounded to the nearest integer.
#' @return A vector of length n.
#' @keywords internal
#' @examples
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
rnorm_truncated2 = function(
    n = 1,
    mean = 0,
    sd = 1,
    low = NULL,
    high = NULL,
    roundToInteger = FALSE,
    invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  len_mean = length(mean)
  len_sd = length(sd)

  if (len_mean != n)
    mean = if (len_mean == 1) rep(mean, n) else approx(mean, n = n)$y
  if (len_sd != n)
    sd = if (len_sd == 1) rep(sd, n) else approx(sd, n = n)$y
  sd[sd < 0] = 0
  out = rnorm(n, mean, sd)

  if (invalidArgAction != 'ignore' && (!is.null(low) | !is.null(high))) {
    if (is.null(low)) low = -1e6
    if (is.null(high)) high = 1e6
    if (invalidArgAction == 'abort') {
      if (any(mean > high | mean < low)) {
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
#'
#' Internal soundgen function
#'
#' Internal helper function for spectral (~BaNa) pitch tracker. NOT quite the
#' same as simply mode(x).
#' @param x numeric vector
#' @keywords internal
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
#' @return Returns a numeric vector of length len and range from 0 to rw_range.
#' @export
#' @examples
#' plot(getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = 0))
#' plot(getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = .2))
#' plot(getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = .95))
#' plot(getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = .99))
#' plot(getRandomWalk(len = 1000, rw_range = 5, rw_smoothing = 1))
#' plot(getRandomWalk(len = 1000, rw_range = 15,
#'   rw_smoothing = .2, trend = c(.1, -.1)))
#' plot(getRandomWalk(len = 1000, rw_range = 15,
#'   rw_smoothing = .2, trend = c(15, -1)))
getRandomWalk = function(len,
                         rw_range = 1,
                         rw_smoothing = .2,
                         method = c('linear', 'spline')[2],
                         trend = 0) {
  if (len < 2)
    return(rgamma(1, 1 / rw_range ^ 2, 1 / rw_range ^ 2))

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
#' @return Returns a vector of integers (0/1/2) of the same length as rw.
#' @export
#' @examples
#' rw = getRandomWalk(len = 100, rw_range = 100, rw_smoothing = .2)
#' r = getIntegerRandomWalk(rw, nonlinBalance = 75,
#'                          minLength = 10, plot = TRUE)
#' r = getIntegerRandomWalk(rw, nonlinBalance = 15,
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
    lines(x = c(0, 100), y = c(q1, q1), lty = 3, lwd = 2, col = 'red')
    text(x = 0, y = q1 + 2, labels = 'subh', pos = 4)
    lines(x = c(0, 100), y = c(q2, q2), lty = 3, lwd = 2, col = 'red')
    text(x = 0, y = q2 + 2, labels = 'subh + jitter', pos = 4)
  }
  rw_bin
}


#' Resize vector to required length
#'
#' Internal soundgen function.
#'
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
#' @keywords internal
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
      myseq = myseq[1:len]
    } else {
      myseq = c(myseq, rep(padWith, (len - len_seq)))
    }
  }
  myseq
}


#' Match number of columns
#'
#' Internal soundgen function
#'
#' Adds columns of new values (eg zeros or NAs) to a matrix, so that the new
#' number of columns = \code{len}
#' @inheritParams matchLengths
#' @param matrix_short input matrix
#' @param nCol the required number of columns
#' @param padWith the value to pad with, normally \code{0} or \code{NA}
#' @keywords internal
#' @examples
#' a = matrix(1:9, nrow = 3)
#' soundgen:::matchColumns(a, nCol = 6, padWith = NA, padDir = 'central')
#' soundgen:::matchColumns(a, nCol = 6, padWith = 0, padDir = 'central')
#' soundgen:::matchColumns(a, nCol = 6, padWith = NA, padDir = 'left')
#' soundgen:::matchColumns(a, nCol = 6, padWith = 'a', padDir = 'right')
#' soundgen:::matchColumns(a, nCol = 2)
matchColumns = function (matrix_short,
                         nCol,
                         padWith = 0,
                         padDir = 'central',
                         interpol = c("approx", "spline")[1]) {
  if (ncol(matrix_short) > nCol) {
    # downsample
    new = interpolMatrix(matrix_short, nc = nCol, interpol = interpol)
  } else {
    # pad with 0 / NA / etc
    if (is.null(colnames(matrix_short))) {
      col_short = 1:ncol(matrix_short)
    } else {
      col_short = colnames(matrix_short)
    }
    col_long = matchLengths(col_short, nCol,
                            padDir = padDir, padWith = NA)
    new = matrix(padWith,
                 nrow = nrow(matrix_short),
                 ncol = length(col_long))
    colnames(new) = col_long
    # paste the old matrix where it belongs
    new[, !is.na(colnames(new))] = matrix_short
  }
  new
}


#' Add overlapping vectors
#'
#' Adds two partly overlapping vectors, such as two waveforms, to produce a
#' longer vector. The location at which vector 2 is pasted is defined by
#' insertionPoint. Algorithm: both vectors are padded with zeros to match in
#' length and then added. All NA's are converted to 0.
#'
#' @seealso \code{\link{soundgen}}
#'
#' @param v1,v2 numeric vectors
#' @param insertionPoint the index of element in vector 1 at which vector 2 will
#'   be inserted (any integer, can also be negative)
#' @param normalize if TRUE, the output is normalized to range from -1 to +1
#' @export
#' @examples
#' v1 = 1:6
#' v2 = rep(100, 3)
#' addVectors(v1, v2, insertionPoint = 5, normalize = FALSE)
#' addVectors(v1, v2, insertionPoint = -4, normalize = FALSE)
#' addVectors(v1, rep(100, 15), insertionPoint = -4, normalize = FALSE)
#' # note the asymmetry: insertionPoint refers to the first arg
#' addVectors(v2, v1, insertionPoint = -4, normalize = FALSE)
#'
#' v3 = rep(100, 15)
#' addVectors(v1, v3, insertionPoint = -4, normalize = FALSE)
#' addVectors(v2, v3, insertionPoint = 7, normalize = FALSE)
#' addVectors(1:6, 3:6, insertionPoint = 3, normalize = FALSE)
addVectors = function(v1, v2, insertionPoint = 1L, normalize = TRUE) {
  if (!is.numeric(v1)) stop(paste('Non-numeric v1:', head(v1)))
  if (!is.numeric(v2)) stop(paste('Non-numeric v2:', head(v2)))
  # v1[is.na(v1)] = 0
  # v2[is.na(v2)] = 0
  insertionPoint = round(insertionPoint)

  if (insertionPoint < 1) {
    # negative indices - v2 starts before v1: simply swap v1/v2
    temp = v1
    v1 = v2
    v2 = temp
    if (insertionPoint == 0) {
      insertionPoint = 2
    } else {
      insertionPoint = 1 - insertionPoint
    }
  }
  l1 = length(v1)
  l2 = length(v2)


  if (insertionPoint == 1) {
    # left-aligned already
    if (l1 == l2) {
      out = v1 + v2
    } else if (l1 > l2) {
      out = c(v1[1:l2] + v2, v1[(l2 + 1):l1])
    } else {
      out = c(v1 + v2[1:l1], v2[(l1 + 1):l2])
    }
  } else if (insertionPoint > l1) {
    # v2 starts after the end of v1
    out = c(v1, rep(0, insertionPoint - l1 - 1), v2)
  } else {
    # insertionPoint inside v1
    a = l1 - insertionPoint + 1
    if (a == l2) {
      # v1 and v2 end at the same spot
      out = c(
        v1[1:(insertionPoint - 1)],
        v1[insertionPoint:(insertionPoint + l2 - 1)] + v2)
    } else if (a > l2) {
      # v2 fully contained within v1
      b = insertionPoint + l2
      out = c(
        v1[1:(insertionPoint - 1)],
        v1[insertionPoint:(b - 1)] + v2,
        v1[b:l1]
      )
    } else {
      # v1 and v2 overlap only partially
      out = c(
        v1[1:(insertionPoint - 1)],
        v1[insertionPoint:l1] + v2[1:a],
        v2[(a + 1):l2]
      )
    }
  }

  if (normalize) out = out / max(abs(out))
  # if (any(is.na(out))) browser()
  out
}


#' Clump a sequence into large segments
#'
#' Internal soundgen function.
#'
#' \code{clumper} makes sure each homogeneous segment in a sequence is at least
#' minLength long. Called by getIntegerRandomWalk(), addSubh(), naiveBayes(),
#' etc. Algorithm: find the epochs shorter than minLength, merge max 1/4 of them
#' with the largest neighbor, and repeat recursively until all epochs are at
#' least minLength long. minLength can be a vector, in which case it is assumed
#' to change over time.
#' @keywords internal
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
clumper = function(x, minLength, n = length(x)) {
  mode_orig = mode(x)
  minLength = round(minLength)   # just in case it's not all integers
  if (max(minLength) < 2) return(x)
  n = length(x)
  if (n <= minLength[1]) {
    # just repeat the most common element
    tbx = table(x)
    mode_x = names(tbx)[which.max(tbx)]
    x = rep(mode_x, n)
    mode(x) = mode_orig
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


#' Get sigmoid filter
#'
#' Internal soundgen function.
#'
#' Produces a filter for amplitude modulation ranging from clicks to
#' approximately a sine wave to reversed clicks (small episodes of silence). The
#' filter is made from concatenated sigmoids and their mirror reflections.
#' @return Returns a vector of length \code{len} and range from 0 to 1
#' @param len the length of output vector
#' @param samplingRate the sampling rate of the output vector, Hz
#' @param freq the frequency of amplitude modulation, Hz (numeric vector)
#' @param shape 0 = ~sine, -1 = clicks, +1 = notches (NB: vice versa in
#'   soundgen!); numeric vector of length 1 or the same length as \code{freq}
#' @param spikiness amplifies the effect of the "shape" parameter;
#'   numeric vector of length 1 or the same length as \code{freq}
#' @keywords internal
#' @examples
#' par(mfrow = c(3, 2))
#' for (shape in c(0, -.1, .1, .5, -1, 1)) {
#'   s = soundgen:::getSigmoid(shape = shape, len = 1000, samplingRate = 500, freq = 2)
#'   plot(s, type = 'l', main = paste('shape =', shape), xlab = '', ylab = '')
#' }
#' par(mfrow = c(1, 1))
#'
#' par(mfrow = c(3, 2))
#' for (shape in c(0, -.1, .1, .5, -1, 1)) {
#'   s = soundgen:::getSigmoid(shape = shape, len = 1000, samplingRate = 500, freq = 2,
#'     spikiness = 3)
#'   plot(s, type = 'l', main = paste('shape =', shape), xlab = '', ylab = '')
#' }
#' par(mfrow = c(1, 1))
getSigmoid = function(len,
                      samplingRate = 16000,
                      freq = 5,
                      shape = 0,
                      spikiness = 1) {
  # print(c(len, freq))
  len_freq = length(freq)
  if (len_freq > 1 | length(shape) > 1 | length(spikiness) > 1) {
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
    # plot(freqContour_prelim, type = 'l')
    n = ceiling(len / samplingRate / mean(1 / freqContour_prelim))

    # get actual contours
    freqContour = spline(freq, n = n)$y
    freqContour[freqContour < 0.001] = 0.001
    shapeContour = spline(shape, n = n)$y
    spikinessContour = spline(spikiness, n = n)$y

    # set up par vectors
    from = -exp(-shapeContour * spikinessContour)
    to = exp(shapeContour * spikinessContour)
    slope = exp(abs(shapeContour)) * 5  # close to sine

    # create one cycle at a time
    out = vector('numeric', len)
    i = 1
    idx = 1
    while (TRUE) {
      len_i = round(samplingRate / freqContour[i] / 2) * 2
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
    if (length(out) > len) out = out[seq_len(len)]
    # can happen if the last cycle ends exactly at n
  } else {
    # if pars are static, we can take a shortcut (~50 times faster)
    from = -exp(-shape * spikiness)
    to = exp(shape * spikiness)
    slope = exp(abs(shape)) * 5  # close to sine
    a = seq(from = from, to = to, length.out = samplingRate / freq / 2)
    b = 1 / (1 + exp(-a * slope))
    b = zeroOne(b)  # plot(b, type = 'l')
    out = rep(c(b, rev(b)), length.out = len)
  }
  # plot(out, type = 'l')
  out
}


#' Report CI
#'
#' Internal soundgen function
#'
#' Takes a numeric vector or matrix with three elements / columns: fit, lower
#' quantile from a CI, and upper quantile from a CI. For each row, it prints the
#' result as fit and CI in square brackets
#' @param n numeric vector or matrix
#' @param digits number of decimal points to preserve
#' @keywords internal
#' @examples
#' n = rnorm(100)
#' soundgen:::reportCI(quantile(n, probs = c(.5, .025, .975)))
#'
#' a = data.frame(fit = c(3, 5, 7),
#'                lwr = c(1, 4, 6.5),
#'                upr = c(5, 6, 7.1))
#' soundgen:::reportCI(a, 1)
#' soundgen:::reportCI(a, 1, ' cm')
#' soundgen:::reportCI(a, 1, '%, 95% CI')
reportCI = function(n, digits = 2, suffix = NULL) {
  if (is.data.frame(n)) n = as.matrix(n)
  n = round(n, digits)
  if (is.matrix(n)) {
    out = matrix(NA, nrow = nrow(n))
    rownames(out) = rownames(n)
    for (i in 1:nrow(n)) {
      out[i, ] = reportCI(n[i, ], digits = digits, suffix = suffix)
    }
    out
  } else {
    paste0(n[1], suffix, ' [', n[2], ', ', n[3], ']')
  }
}


#' Interpolate matrix
#'
#' Internal soundgen function
#'
#' Performs a chosen type of interpolation (bilinear or spline) across both rows
#' and columns of a matrix, in effect up- or downsampling a matrix to required
#' dimensions. Rownames and colnames are also interpolated as needed.
#' @param m input matrix of numeric values
#' @param nr,nc target dimensions
#' @param interpol interpolation method ('approx' for linear, 'spline' for
#'   spline)
#' @keywords internal
#' @examples
#' m = matrix(1:12 + rnorm(12, 0, .2), nrow = 3)
#' rownames(m) = 1:3; colnames(m) = 1:4
#' soundgen:::interpolMatrix(m)  # just returns the original
#' soundgen:::interpolMatrix(m, nr = 10, nc = 7)
#' soundgen:::interpolMatrix(m, nr = 10, nc = 7, interpol = 'spline')
#' soundgen:::interpolMatrix(m, nr = 2, nc = 7)
#' soundgen:::interpolMatrix(m, nr = 2, nc = 3)
#'
#' # input matrices can have a single row/column
#' soundgen:::interpolMatrix(matrix(1:5, nrow = 1), nc = 9)
#' soundgen:::interpolMatrix(matrix(1:5, ncol = 1), nr = 5, nc = 3)
interpolMatrix = function(m,
                          nr = NULL,
                          nc = NULL,
                          interpol = c("approx", "spline")[1]) {
  if (!is.matrix(m)) {
    m = as.matrix(m)
    warning('non-matrix input m: converting to matrix')
  }
  nr0 = nrow(m)
  nc0 = ncol(m)
  if (is.null(nr)) nr = nr0
  if (is.null(nc)) nc = nc0
  if (nr == nr0 & nc == nc0) return(m)
  # if (nr < 2) stop('nr must be >1')
  # if (nc < 2) stop('nc must be >1')
  isComplex = is.complex(m[1, 1])

  # # Downsample rows if necessary
  # if (nr0 > nr) {
  #   m = m[seq(1, nr0, length.out = nr),, drop = FALSE]
  # }
  #
  # # Downsample columns if necessary
  # if (nc0 > nc) {
  #   m = m[, seq(1, nc0, length.out = nc), drop = FALSE]
  # }

  # Interpolate rows if necessary
  if (nr0 != nr) {
    if (nr0 == 1) {
      temp = matrix(rep(m, nr), nrow = nr, byrow = TRUE)
    } else {
      temp = matrix(1, nrow = nr, ncol = nc0)
      for (c in 1:nc0) {
        if (isComplex) {
          # approx doesn't work with complex numbers properly, so we treat the
          # Re and Im parts separately
          temp_re = approx(x = Re(m[, c]), n = nr)$y
          temp_im = approx(x = Im(m[, c]), n = nr)$y
          temp[, c] = complex(real = temp_re, imaginary = temp_im)
        } else {
          temp[, c] = do.call(interpol, list(x = m[, c], n = nr))$y
        }
      }
    }
    if (!is.null(rownames(m))) {
      rnms = as.numeric(rownames(m))
      rownames(temp) = do.call(interpol, list(x = rnms, n = nr))$y
    }
  } else {
    temp = m
    rownames(temp) = rownames(m)
  }
  colnames(temp) = colnames(m)

  # Interpolate columns if necessary
  if (nc0 != nc) {
    if (nc0 == 1) {
      out = matrix(rep(temp[, 1], nc), ncol = nc, byrow = FALSE)
    } else {
      out = matrix(1, nrow = nr, ncol = nc)
      for (r in 1:nr) {
        if (isComplex) {
          temp_re = approx(x = Re(temp[r, ]), n = nc)$y
          temp_im = approx(x = Im(temp[r, ]), n = nc)$y
          out[r, ] = complex(real = temp_re, imaginary = temp_im)
        } else {
          out[r, ] = do.call(interpol, list(x = temp[r, ], n = nc))$y
        }
      }
    }
    if (!is.null(colnames(m))) {
      cnms = as.numeric(colnames(m))
      try_colnames = try(do.call(interpol, list(x = cnms, n = nc))$y,
                         silent = TRUE)
      if (!inherits(try_colnames, 'try-error')) {
        colnames(out) = try_colnames
      }
    }
  } else {
    out = temp
    colnames(out) = colnames(temp)
  }
  rownames(out) = rownames(temp)
  out
}


#' sampleModif
#'
#' Internal soundgen function
#'
#' Same as \code{\link[base]{sample}}, but without defaulting to x = 1:x if
#' length(x) = 1. See
#' https://stackoverflow.com/questions/7547758/using-sample-with-sample-space-size-1
#' @param x vector
#' @param ... other arguments passed to \code{sample}
#' @keywords internal
#' @examples
#' soundgen:::sampleModif(x = 3, n = 1)
#' # never returns 1 or 2: cf. sample(x = 3, size = 1)
sampleModif = function(x, ...) x[sample.int(length(x), ...)]


#' Gaussian smoothing in 2D
#'
#' Takes a matrix of numeric values and smoothes it by convolution with a
#' symmetric Gaussian window function.
#'
#' @seealso \code{\link{modulationSpectrum}}
#'
#' @return Returns a numeric matrix of the same dimensions as input.
#' @param m input matrix (numeric, on any scale, doesn't have to be square)
#' @param kernelSize the size of the Gaussian kernel, in points
#' @param kernelSD the SD of the Gaussian kernel relative to its size (.5 = the
#'   edge is two SD's away)
#' @param action 'blur' = kernel-weighted average, 'unblur' = subtract
#'   kernel-weighted average
#' @param plotKernel if TRUE, plots the kernel
#' @export
#' @examples
#' s = spectrogram(soundgen(), samplingRate = 16000, windowLength = 10,
#'   output = 'original', plot = FALSE)
#' s = log(s + .001)
#' # image(s)
#' s1 = gaussianSmooth2D(s, kernelSize = 5, plotKernel = TRUE)
#' # image(s1)
#'
#' \dontrun{
#' # more smoothing in time than in frequency
#' s2 = gaussianSmooth2D(s, kernelSize = c(5, 15))
#' image(s2)
#'
#' # vice versa - more smoothing in frequency
#' s3 = gaussianSmooth2D(s, kernelSize = c(25, 3))
#' image(s3)
#'
#' # sharpen the image by deconvolution with the kernel
#' s4 = gaussianSmooth2D(s1, kernelSize = 5, action = 'unblur')
#' image(s4)
#'
#' s5 = gaussianSmooth2D(s, kernelSize = c(15, 1), action = 'unblur')
#' image(s5)
#' }
gaussianSmooth2D = function(m,
                            kernelSize = 5,
                            kernelSD = .5,
                            action = c('blur', 'unblur')[1],
                            plotKernel = FALSE) {
  if (length(kernelSize) == 1) kernelSize = c(kernelSize, kernelSize)
  nr = nrow(m)
  nc = ncol(m)
  if (max(kernelSize) < 2) return(m)
  if (kernelSize[1] >= (nr / 2)) {
    kernelSize[1] = ceiling(nr / 2) - 1
  }
  if (kernelSize[2] >= (nc / 2)) {
    kernelSize[2] = ceiling(nc / 2) - 1
  }
  if (kernelSize[1] %% 2 == 0) kernelSize[1] = kernelSize[1] + 1  # make uneven
  if (kernelSize[2] %% 2 == 0) kernelSize[2] = kernelSize[2] + 1  # make uneven
  if (max(kernelSize) < 2) return(m)

  # set up 2D Gaussian filter
  kernel = getCheckerboardKernel(
    size = kernelSize,
    kernelSD = kernelSD,
    checker = FALSE,
    plot = plotKernel)
  kernel = kernel / sum(kernel)  # convert to pdf

  ## pad matrix with size / 2 zeros, so that we can correlate it with the
  #  kernel starting from the very edge
  m_padded = matrix(0,
                    nrow = nr + kernelSize[1],
                    ncol = nc + kernelSize[2])
  # lower left corner in the padded matrix where we'll paste the original m
  idx_row = round((kernelSize[1] + 1) / 2)
  idx_col = round((kernelSize[2] + 1) / 2)
  # paste original. Now we have a padded matrix
  m_padded[idx_row:(idx_row + nr - 1), idx_col:(idx_col + nc - 1)] = m

  # kernel smoothing / deconvolution
  out = matrix(NA, nrow = nr, ncol = nc)
  if (action == 'blur') {
    for (i in 1:nr) {
      for (j in 1:nc) {
        out[i, j] = sum(
          # m_padded[i:(i + 2 * idx_row - 2), j:(j + 2 * idx_col - 2)] * kernel
          .subset(m_padded, i:(i + 2 * idx_row - 2), j:(j + 2 * idx_col - 2)) * kernel
        )
      }
    }
  } else if (action == 'unblur') {
    for (i in 1:nr) {
      for (j in 1:nc) {
        out[i, j] = .subset2(m, i, j) - sum(
          # m_padded[i:(i + 2 * idx_row - 2), j:(j + 2 * idx_col - 2)] * kernel
          .subset(m_padded, i:(i + 2 * idx_row - 2), j:(j + 2 * idx_col - 2)) * kernel
        )
      }
    }
  }
  if (!is.null(rownames(m))) rownames(out) = rownames(m)
  if (!is.null(colnames(m))) colnames(out) = colnames(m)
  out
}


#' Proportion of total
#'
#' Internal soundgen function.
#'
#' Calculates the values in the input distribution that contain particular
#' proportions of the sum of all values in the input distribution.
#' @param x numeric vector of non-negative real numbers
#' @param quantiles quantiles of the cumulative distribution
#' @keywords internal
#' @examples
#' x = rnorm(100)
#' x = x - min(x)  # must be non-negative
#' hist(x)
#' v = soundgen:::pDistr(x, quantiles = c(.5, .8, .9))
#' sum(x[x > v['0.5']]) / sum(x)
#' sum(x[x > v['0.9']]) / sum(x)
pDistr = function(x, quantiles) {
  a1 = rev(sort(x))  # plot(a1, type = 'l')
  a2 = cumsum(a1)  # plot(a2, type = 'l')
  total = sum(x)
  out = rep(NA, length(quantiles))
  names(out) = quantiles
  for (q in seq_along(quantiles)) {
    out[q] = a1[which(a2 > (total * quantiles[q]))[1]]
  }
  out
}


#' Log-warp matrix
#'
#' Internal soundgen function.
#'
#' Log-warps a matrix, as if log-transforming plot axes.
#'
#' @param m a matrix of numeric values of any dimensions (not necessarily
#'   square)
#' @param base the base of logarithm
#' @keywords internal
#' @examples
#' m = matrix(1:90, nrow = 10)
#' colnames(m) = 1:9
#' soundgen:::logMatrix(m, base = 2)
#' soundgen:::logMatrix(m, base = 10)
#'
#' soundgen:::logMatrix(m = matrix(1:9, nrow = 1), base = 2)
#'
#' \dontrun{
#' s = spectrogram(soundgen(), 16000, output = 'original')
#' image(log(t(soundgen:::logMatrix(s, base = 2))))
#' }
logMatrix = function(m, base = 2) {
  # the key is to make a sequence of time locations within each row/column for
  # interpolation: (1:nrow(m)) ^ base (followed by normalization, so this index
  # will range from 1 to nrow(m) / ncol(m))
  if (ncol(m) > 1) {
    idx_row = (1:ncol(m)) ^ base - 1
    idx_row = idx_row / max(idx_row)
    idx_row = idx_row * (ncol(m) - 1) + 1
    # interpolate rows at these time points
    m1 = t(apply(m, 1, function(x) approx(x, xout = idx_row)$y))
  } else {
    m1 = m
  }

  # same for columns
  if (nrow(m) > 1) {
    idx_col = (1:nrow(m)) ^ base - 1
    idx_col = idx_col / max(idx_col)
    idx_col = idx_col * (nrow(m) - 1) + 1
    # interpolate columns at these time points
    m2 = apply(m1, 2, function(x) approx(x, xout = idx_col)$y)
  } else {
    m2 = m1
  }

  # interpolate row and column names
  # (assuming numeric values, as when called from modulationSpectrum())
  if (!is.null(colnames(m)) & ncol(m) > 1) {
    colnames(m2) = approx(as.numeric(colnames(m)), xout = idx_row)$y
  }
  if (!is.null(rownames(m)) & nrow(m) > 1) {
    rownames(m2) = approx(as.numeric(rownames(m)), xout = idx_col)$y
  }
  m2
}


#' Switch color theme
#'
#' Internal soundgen function
#' @param colorTheme string like 'bw', 'seewave', or function name
#' @keywords internal
#' @examples
#' soundgen:::switchColorTheme('bw')
#' soundgen:::switchColorTheme('seewave')
#'
#' cols_matlab = soundgen:::switchColorTheme('matlab') (100)
#' plot(1:100, seq(0, 1, length.out = 100), type = 'n')
#' for (i in 1:100) {
#'   rect(i - 1, 0, i, 1, col = cols_matlab[i], border = NA)
#' }
switchColorTheme = function(colorTheme) {
  if (is.null(colorTheme)) {
    return(NULL)
  }  else if (colorTheme == 'bw') {
    color.palette = function(x) gray(seq(from = 1, to = 0, length = x))
  } else if (colorTheme == 'seewave') {
    color.palette = seewave::spectro.colors
  } else if (colorTheme == 'matlab') {
    color.palette = jet.col
  } else {
    colFun = match.fun(colorTheme)
    color.palette = function(x) rev(colFun(x))
  }
  color.palette
}


#' Matlab colors
#'
#' Internal soundgen function for generating a Matlab-like palette
#' (=plot3D::jet.col).
#' @param n number of colors
#' @param alpha transparency
#' @keywords internal
jet.col = function(n = 100, alpha = 1) {
  red = c(0, 0, 0, 255, 255, 128)
  green = c(0, 0, 255, 255, 0, 0)
  blue = c(143, 255, 255, 0, 0, 0)
  x.from = c(0, seq(0.125, 1, by = 0.25), 1)
  x.to = seq(0, 1, length.out = n)
  expand = function(col) approx(x = x.from, y = col, xout = x.to)$y
  rgb(expand(red), expand(green), expand(blue), maxColorValue = 255,
      alpha = alpha * 255)
}


#' Parabolic peak interpolation
#'
#' Internal soundgen function
#'
#' Takes a spectral peak and two adjacent points, fits a parabola through them,
#' and thus estimates true peak location relative to the discrete peak. See
#' https://ccrma.stanford.edu/~jos/sasp/Quadratic_Interpolation_Spectral_Peaks.html
#' @param threePoints the amplitudes of three adjacent points (beta is the
#'   peak), ideally spectrum on a dB scale, obtained with a Gaussian window
#' @param plot if TRUE, plot the points and the fit parabola
#' @return Returns a list: $p = the correction coefficient in bins (idx_beta + p
#'   gives the true peak), $ampl_p = the amplitude of the true peak
#' @keywords internal
#' @examples
#' soundgen:::parabPeakInterpol(c(-1, 0, -4), plot = TRUE)
parabPeakInterpol = function(threePoints, plot = FALSE) {
  if (any(is.na(threePoints)) | length(threePoints) < 3) {
    stop(paste('invalid input:', threePoints))
  }
  if (threePoints[2] < threePoints[1] |
      threePoints[2] < threePoints[3]) {
    # the central point is not a peak
    return(list(p = 0, ampl_p = 0))
  }
  alpha = threePoints[1]
  beta = threePoints[2]
  gamma = threePoints[3]
  a = (alpha - 2 * beta + gamma) / 2
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


#' Identify and play
#'
#' Internal soundgen function. NB: even built-in examples of identify() not
#' working in R 4.4.1 (points not identified).
#'
#' A wrapper around \code{identify()} intended to play the sound corresponding
#' to a clicked point.
#' @param x,y plot coordinates
#' @param data dataframe from which x & y are taken, also containing a column
#'   called "file"
#' @param audioFolder path to audio files
#' @param to play only the first ... s
#' @param plot if TRUE, plots the index of clicked points
#' @param pch symbol for marking clicked points
#' @param ... other arguments passed to \code{identify()}
#' @keywords internal
#' @examples
#' \dontrun{
#' msf = modulationSpectrum('~/Downloads/temp', plot = FALSE)
#'
#' # Method 1: provide path to folder, leave data = NULL
#' plot(msf$summary$amMsFreq_median, msf$summary$amMsDep_median)
#' soundgen:::identifyAndPlay(msf$summary$amFreq_median, msf$summary$amDep_median,
#'   audioFolder = '~/Downloads/temp',
#'   to = 2,
#'  plot = TRUE,
#'  pch = 19)
#'
#' # Method 2:
#' x = msf$summary$amMsFreq_median
#' y = msf$summary$amMsDep_median
#' plot(x, y)
#' soundgen:::identifyAndPlay(x, y, data = msf$summary,
#'   audioFolder = '~/Downloads/temp',
#'   to = 2,
#'   plot = FALSE,
#'   pch = 8)
#' }
identifyAndPlay = function(
    x,
    y = NULL,
    data = NULL,
    audioFolder,
    to = 5,
    plot = FALSE,
    pch = 19,
    ...) {
  n = length(x)
  if (is.null(data)) {
    data = data.frame(
      file = list.files(audioFolder, full.names = TRUE)
    )
  } else {
    data = data.frame(
      file = paste0(audioFolder, '/', data$file)
    )
  }
  xy = xy.coords(x, y)
  x = xy$x
  y = xy$y
  sel = rep(FALSE, n)
  answers = numeric(0)
  while(sum(sel) < n) {
    ans = identify(x[!sel], y[!sel], labels = which(!sel),
                   n = 1, plot = plot, ...)
    if(!length(ans)) break
    ans = which(!sel)[ans]
    answers = c(answers, ans)
    points(x[ans], y[ans], pch = pch)
    ## play the selected point
    try(playme(data$file[ans], to = to))
  }
  ## return indices of selected points
  return(data$file[answers])
}


#' Warp matrix
#'
#' Internal soundgen function
#'
#' Warps or scales each column of a matrix (normally a spectrogram).
#' @keywords internal
#' @param m matrix (rows = frequency bins, columns = time)
#' @param scaleFactor 1 = no change, >1 = raise formants
#' @param interpol interpolation method
#' @examples
#' a = matrix(1:12, nrow = 4)
#' a
#' soundgen:::warpMatrix(a, 1.5, 'approx')
#' soundgen:::warpMatrix(a, 1/1.5, 'spline')
warpMatrix = function(m, scaleFactor, interpol = c('approx', 'spline')[1]) {
  scaleFactor = getSmoothContour(scaleFactor, len = ncol(m))
  n1 = nrow(m)
  m_warped = m
  for (i in 1:ncol(m)) {
    if (scaleFactor[i] > 1) {
      # "stretch" the vector (eg spectrum of a frame)
      n2 = round(n1 / scaleFactor[i])
      m_warped[, i] = do.call(interpol, list(x = m[1:n2, i], n = n1))$y
    } else if (scaleFactor[i] < 1) {
      # "shrink" the vector and pad it with the last obs to the original length
      n2 = round(n1 * scaleFactor[i])
      padding = rep(m[n1, i], n1 - n2)  # or 0
      m_warped[, i] = c(
        do.call(interpol, list(x = m_warped[, i],
                               xout = seq(1, n1, length.out = n2),
                               n = n1))$y,
        padding
      )
    }
    # plot(m_warped[, i], type = 'l')
    # plot(abs(m[, i]), type = 'l', xlim = c(1, max(n1, n2)))
    # points(m_warped[, i], type = 'l', col = 'blue')
  }
  m_warped
}


#' Principal argument
#'
#' Internal soundgen function.
#'
#' Recalculates the phase of complex numbers to be within the interval from -pi to pi.
#' @param x real number representing phase angle
#' @keywords internal
princarg = function(x) (x + pi) %% (2 * pi) - pi


#' Split vector into chunks
#'
#' Internal soundgen function.
#'
#' Takes a numeric vector x and splits it into n chunks. This is the fastest
#' splitting algorithm from
#' https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
#' @param x numeric vector to split
#' @param n number of chunks
#' @return Returns a list of length \code{n} containing the chunks
#' @keywords internal
#' @examples
#' # prepare chunks of iterator to run in parallel on several cores
#' chunks = soundgen:::splitIntoChunks(1:21, 4)
#' chunks
splitIntoChunks = function(x, n) {
  len = length(x)
  len_chunk = ceiling(len / n)
  mapply(function(a, b) (x[a:b]),
         seq.int(from = 1, to = len, by = len_chunk),
         pmin(seq.int(from = 1, to = len, by = len_chunk) + (len_chunk - 1), len),
         SIMPLIFY = FALSE)
}


#' rbind_fill
#'
#' Internal soundgen function
#'
#' Fills missing columns with NAs, then rbinds - handy in case one has extra
#' columns. Used in formant_app(), pitch_app()
#' @param df1,df2 two dataframes with partly matching columns
#' @keywords internal
rbind_fill = function(df1, df2) {
  if (!is.list(df1) || nrow(df1) == 0) return(df2)
  if (!is.list(df2) || nrow(df2) == 0) return(df1)
  df1[setdiff(names(df2), names(df1))] = NA
  df2[setdiff(names(df1), names(df2))] = NA
  rbind(df1, df2)
}


#' Pseudolog
#'
#' Internal soundgen function
#'
#' From function pseudo_log_trans() in the "scales" package.
#'
#' @seealso \code{\link{pseudoLog_undo}}
#'
#' @param x numeric vector to transform
#' @param sigma scaling factor for the linear part
#' @param base approximate logarithm base used
#' @keywords internal
#' @examples
#' a = -30:30
#' plot(a, soundgen:::pseudoLog(a, sigma = 1, base = 2))
#' plot(a, soundgen:::pseudoLog(a, sigma = 5, base = 2))
#' plot(a, soundgen:::pseudoLog(a, sigma = .1, base = 2))
pseudoLog = function(x, sigma, base) {
  asinh(x/(2 * sigma))/log(base)
}


#' Undo pseudolog
#'
#' Internal soundgen function
#'
#' From function pseudo_log_trans() in the "scales" package.
#'
#' @seealso \code{\link{pseudoLog}}
#'
#' @param x numeric vector to transform
#' @param sigma scaling factor for the linear part
#' @param base approximate logarithm base used
#' @keywords internal
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
#' @keywords internal
#' @examples
#' y = c(10, 11, 8, 4, 2, 1.5, 1, 0.7, .5, .4, .3)
#' soundgen:::findElbow(data.frame(x = seq_along(y), y = y), plot = TRUE)
findElbow = function(d, plot = FALSE) {
  d = na.omit(d)
  n = nrow(d)

  # draw a straight line between the endpoints
  endpoints = d[c(1, n), ]
  fit = lm(endpoints$y ~ endpoints$x)

  # calculate distances from points to line
  # https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
  coefs = coef(fit)
  distances = abs(coefs[2] * d$x - d$y + coefs[1]) / sqrt(coefs[2]^2 + 1)
  out = which.max(distances)

  if (plot) {
    plot(d, type = 'b')
    abline(coefs, col = 'blue', lty = 2)
    # segment to the closest point on the line (again from wiki)
    a = -coefs[2]; b = 1; c = -coefs[1]
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
#' @keywords internal
logistic = function(x) 1 / (1 + exp(-x))


#' Logit
#' @param x numeric vector
#' @keywords internal
logit = function(x) log(x / (1 - x))


#' Sinc
#' @param x numeric vector
#' @keywords internal
#' @examples
#' x = seq(-5, 5, .01); plot(x, soundgen:::sinc(x))
sinc = function(x) {
  out = sin(pi * x) / (pi * x)
  out[x == 0] = 1
  out
}


#' Average matrices
#'
#' Internal soundgen function.
#'
#' Takes a list of matrices (normally modulation spectra), interpolates them to
#' have the same size, and then reduces them (eg takes the average).
#' @param mat_list a list of matrices to aggregate (eg spectrograms or
#'   modulation spectra)
#' @param rFun, cFun functions used to determine the number of rows and columns
#'   in the result
#' @param reduceFun function used to aggregate
#' @keywords internal
#' @examples
#' mat_list = list(
#'   matrix(1:30, nrow = 5),
#'   matrix(80:17, nrow = 8)
#' )
#' soundgen:::averageMatrices(mat_list)
#' soundgen:::averageMatrices(mat_list, cFun = 'max', reduceFun = '*')
averageMatrices = function(mat_list,
                           rFun = 'max',
                           cFun = 'median',
                           reduceFun = '+') {
  # normally same samplingRate, but in case not, upsample frequency resolution
  nr = round(do.call(rFun, list(unlist(lapply(mat_list, nrow)))))
  # take typical ncol (depends on sound dur)
  nc = round(do.call(cFun, list(unlist(lapply(mat_list, ncol)))))
  mat_list_sameDim = lapply(mat_list, function(x) interpolMatrix(x, nr = nr, nc = nc))
  Reduce(reduceFun, mat_list_sameDim) / length(mat_list)
}


#' Trim leading and trailing NAs
#'
#' Internal soundgen function. Nearly 10 times faster than zoo::na.trim.
#' Slightly slower solution: a[!cumprod(is.na(a)) &
#' rev(!cumprod(is.na(rev(a))))]. See
#' https://stackoverflow.com/questions/42759027/remove-leading-and-trailing-na
#'
#' @param x numeric vector
#' @keywords internal
#' @examples
#' soundgen:::na.trim(c(NA, NA, 1:10, NA, 21:25, NA))
na.trim = function(x) {
  idx_notNA = which(!is.na(x))
  x[min(idx_notNA):max(idx_notNA)]
}
