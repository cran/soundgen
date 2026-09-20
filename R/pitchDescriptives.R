#' Pitch descriptives
#'
#' Provides common descriptives of time series such as pitch contours, including
#' measures of average / range / variability / slope / inflections etc. Several
#' degrees of smoothing can be applied consecutively. The summaries are produced
#' on the original and log-transformed scales, so this is meant to be used on
#' frequency-related variables in Hz.
#' @param x input: numeric vector, a list of time stamps and values in rows, a
#'   dataframe with one row per file and time/pitch values stored as characters
#'   (as exported by \code{\link{pitch_app}}), or path to csv file containing
#'   the output of \code{\link{pitch_app}} or \code{\link{analyze}}
#' @param step distance between values in s (only needed if input is a vector)
#' @param timeUnit if NULL (default), guesses "ms" if step > 1 and "s"
#'   otherwise; specify "s" or "ms" explicitly to override
#' @param smoothBW a vector of bandwidths (Hz) for consecutive smoothing of
#'   input using \code{\link{pitchSmoothPraat}}; NA = no smoothing
#' @param inflThres minimum difference (in semitones) between consecutive
#'   extrema to consider them inflections; to apply a different threshold at
#'   each smoothing level, provide \code{inflThres} as a vector of the same
#'   length as \code{smoothBW}; NA = no threshold
#' @param ref reference value for transforming Hz to semitones, defaults to
#'   C0 (16.3516 Hz)
#' @param summaryFun summary function(s) to apply to the syllable descriptives
#'   (not to the pitch contours themselves)
#' @param ptvStep,ptvTime,ptvFreq the instantaneous proportion of time
#'   vocalizing (PTV) is calculated by producing a binary (sound on/off) contour
#'   with a step of ptvStep ms and convolving it with a half-Gaussian filter
#'   with SD = ptvTime s ($ptv_conv) and by low-pass filtering it over ptvFreq
#'   Hz ($ptv_lowpass)
#' @param extraSummaryFun additional summary function(s) applied to pitch
#'   contours themselves (not to extracted pitch descriptives) that take a
#'   numeric vector with some NAs and return a single number, eg c('myFun1',
#'   'myFun2')
#' @param plot if TRUE, plots the inflections for manual verification

#' @return A list with three elements: \code{summary} (a dataframe with columns
#'   containing summaries of one or multiple inputs, one input per row),
#'   \code{syllables} (a dataframe or list of dataframes with syllable timings,
#'   where a syllable is a contiguous non-NA run of the pitch contour), and
#'   \code{ptv} (a dataframe or list of dataframes with the instantaneous PTV
#'   contour). The descriptives in \code{summary} are as follows:
#'   \describe{\item{duration}{total duration, s} \item{durDefined}{duration
#'   after omitting leading and trailing NAs} \item{propDefined}{percentage of
#'   input with non-NA value, eg percentage of voiced frames if the input is
#'   pitch} \item{start, start_oct, end, end_oct}{the first and last values on
#'   the original scale and in octaves above C0 (16.3516 Hz)} \item{mean,
#'   median, max, min}{average and extreme values on the original scale}
#'   \item{mean_oct, median_oct, min_oct, max_oct}{same in octaves above C0}
#'   \item{time_max, time_min}{the location of minimum and maximum relative to
#'   durDefined, 0 to 1} \item{range, range_sem, sd, sd_sem}{range and standard
#'   deviation on the original scale and in semitones} \item{CV}{coefficient of
#'   variation = sd/mean (provided for historical reasons)} \item{meanSlope,
#'   meanSlope_sem}{mean slope in Hz/s or semitones/s (NB: does not depend on
#'   duration or missing values)} \item{meanAbsSlope, meanAbsSlope_sem}{mean
#'   absolute slope (modulus, ie rising and falling sections no longer cancel
#'   out)} \item{maxAbsSlope, maxAbsSlope_sem}{the steepest slope}}
#' @export
#' @examples
#' x = c(NA, NA, 405, 441, 459, 459, 460, 462, 462, 458, 458, 445, 458, 451,
#' 444, 444, 430, 416, 409, 403, 403, 389, 375, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, 183, 677, 677, 846, 883, 886, 924, 938, 883, 946, 846, 911, 826, 826,
#' 788, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 307,
#' 307, 368, 377, 383, 383, 383, 380, 377, 377, 377, 374, 374, 375, 375, 375,
#' 375, 368, 371, 374, 375, 361, 375, 389, 375, 375, 375, 375, 375, 314, 169,
#' NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 238, 285, 361, 374, 375, 375,
#' 375, 375, 375, 389, 403, 389, 389, 375, 375, 389, 375, 348, 361, 375, 348,
#' 348, 361, 348, 342, 361, 361, 361, 365, 365, 361, 966, 966, 966, 959, 959,
#' 946, 1021, 1021, 1026, 1086, 1131, 1131, 1146, 1130, 1172, 1240, 1172, 1117,
#' 1103, 1026, 1026, 966, 919, 946, 882, 832, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA)
#' plot(x, type = 'b')
#' ci95 = function(x) diff(quantile(na.omit(x), probs = c(.025, .975)))
#' pd = pitchDescriptives(
#'   x, step = .025,
#'   smoothBW = c(NA, 10, 1),   # original + smoothed at 10 Hz and 1 Hz
#'   inflThres = c(NA, .2, .2), # different for each level of smoothing
#'   extraSummaryFun = 'ci95',  # user-defined, here 95% coverage interval
#'   plot = TRUE
#' )
#' pd
#'
#' \dontrun{
#' # a single file
#' data(speechEx, package = 'soundgen')
#' a = analyze(speechEx)
#' pd1 = pitchDescriptives(a$detailed[, c('time', 'pitch')],
#'                         inflThres = NA, plot = TRUE)
#' pd2 = pitchDescriptives(a$detailed[, c('time', 'pitch')],
#'                         inflThres = c(0.1, 0.1, .5), plot = TRUE)
#'
#' # multiple files returned by analyze()
#' an = analyze('~/Downloads/temp')
#' pd = pitchDescriptives(an$detailed)
#' pd
#' }
pitchDescriptives = function(x,
                             step = NULL,
                             timeUnit = NULL,
                             smoothBW = c(NA, 10, 1),
                             inflThres = .2,
                             summaryFun = c('mean', 'sd'),
                             extraSummaryFun = c(),
                             ref = 16.3516,
                             ptvStep = NULL,
                             ptvTime = 0.5,
                             ptvFreq = 1,
                             plot = FALSE) {
  empty_result = list(summary = data.frame(), syllables = list(), ptv = list())

  orig = ''
  if (is.character(x)) {
    if (length(x) != 1) stop('If x is a character, it must be a single file path')
    orig = x
    if (file.exists(x)) {
      x = try(read.csv(x), silent = TRUE)
      if (inherits(x, 'try-error'))
        stop('Input not recognized')
    } else {
      stop('File not found: ', x)
    }
  }

  if (is.list(x) && !is.null(x$time) && !is.null(x$pitch)) {
    if (is.numeric(x$time)) {
      data = suppressWarnings(list(list(file = basename(orig),
                                        time = x$time,
                                        pitch = as.numeric(x$pitch))))
    } else if (is.character(x$time)) {
      nr = nrow(x)
      if (nr == 0) return(empty_result)
      data = vector('list', nr)
      for (i in seq_len(nr)) {
        data[[i]] = list(
          file = x$file[i],
          time = suppressWarnings(as.numeric(unlist(strsplit(x$time[i], ',')))),
          pitch = suppressWarnings(as.numeric(unlist(strsplit(x$pitch[i], ','))))
        )
      }
    } else {
      stop('time column must be numeric or character')
    }
  } else if (is.list(x)) {
    n = length(x)
    if (n == 0) return(empty_result)
    data = vector('list', n)
    for (i in seq_len(n)) {
      data[[i]] = list(
        file = names(x)[i],
        time = x[[i]]$time,
        pitch = x[[i]]$pitch
      )
    }
  } else if (is.numeric(x)) {
    if (length(x) < 2) return(empty_result)
    if (is.null(step))
      stop('If x is a numeric vector, step must be provided')
    data = list(list(
      file = '',
      time = step * (seq_along(x) - .5),
      pitch = x
    ))
  } else {
    stop('Input not recognized')
  }

  # attempt to auto-detect time unit
  if (is.null(timeUnit)) {
    step = data[[1]]$time[2] - data[[1]]$time[1]
    if (step < 1) {
      timeUnit = 's'
      message('Setting timeUnit to "s"... Specify timeUnit = "ms" to override')
    } else {
      timeUnit = 'ms'
      message('Setting timeUnit to "ms"... Specify timeUnit = "s" to override')
    }
  }

  len_data = length(data)
  summary_list = vector('list', len_data)
  syllables = ptv = vector('list', len_data)
  time_start = proc.time()

  for (i in seq_len(len_data)) {
    data_i = data[[i]]
    if (timeUnit == 'ms') data_i$time = data_i$time / 1000
    res = do.call(
      .pitchDescriptives,
      list(time = data_i$time,
           pitch = data_i$pitch,
           smoothBW = smoothBW,
           inflThres = inflThres,
           summaryFun = summaryFun,
           extraSummaryFun = extraSummaryFun,
           ref = ref,
           ptvStep = ptvStep,
           ptvTime = ptvTime,
           ptvFreq = ptvFreq,
           plot = plot,
           main = data_i$file)
    )
    summary_list[[i]] = res$summary
    syllables[[i]] = res$syllables
    ptv[[i]] = res$ptv
    if (!is.null(data_i$file)) {
      summary_list[[i]]$file = if (is.null(data_i$file)) NA else data_i$file
      names(syllables)[i] = names(ptv)[i] = data_i$file
    }
    reportTime(i = i, nIter = len_data, time_start = time_start)
    if (plot && i < len_data)
      invisible(readline(prompt="Press [enter] to continue"))
  }

  summary = do.call(rbind, summary_list)

  if (len_data == 1) {
    syllables = syllables[[1]]
    ptv = ptv[[1]]
  }
  return(list(summary = summary, syllables = syllables, ptv = ptv))
}


#' Pitch descriptives per file
#' @noRd
.pitchDescriptives = function(time,
                              pitch,
                              smoothBW,
                              inflThres,
                              summaryFun = c('mean', 'sd'),
                              extraSummaryFun = c(),
                              ref = 16.3516,
                              ptvStep = NULL,
                              ptvTime = 0.5,
                              ptvFreq = 1,
                              plot = FALSE,
                              main = '') {
  len = length(time)
  if (len < 2) {
    time = c(0, 1)
    pitch = c(NA, NA)
    len = 2
  }
  if (length(pitch) != len) stop('time and pitch must have the same length')

  step = time[2] - time[1]
  samplingRate = 1 / step
  if (length(inflThres) == 1) {
    inflThres = rep(inflThres[1], length(smoothBW))
  } else if (length(inflThres) != length(smoothBW)) {
    stop('inflThres should be of length 1 or the same length as smoothBW')
  }

  out = data.frame(file = NA)

  smooth_labels = ifelse(is.na(smoothBW), 'raw', as.character(smoothBW))
  keep = !duplicated(smoothBW)
  smoothBW = smoothBW[keep]
  inflThres = inflThres[keep]
  smooth_labels = smooth_labels[keep]

  if (length(smoothBW) > 0 && plot) {
    op = par('mfrow')
    par(mfrow = c(length(smoothBW), 1))
    on.exit(par(mfrow = op))
  }

  for (i in seq_along(smoothBW)) {
    if (is.finite(smoothBW[i]) && any(!is.na(pitch))) {
      pitch_sm = pitchSmoothPraat(pitch, bandwidth = smoothBW[i],
                                  samplingRate = samplingRate)
    } else {
      pitch_sm = pitch
    }

    if (is.na(smoothBW[i])) {
      smoothing_text = 'No smoothing'
    } else {
      smoothing_text = paste('Smoothed at', smoothBW[i], 'Hz')
    }

    out_i = timeSeriesSummary(pitch_sm,
                              step = step,
                              inflThres = inflThres[i],
                              extraSummaryFun = extraSummaryFun,
                              ref = ref,
                              plot = plot,
                              main = paste(main, smoothing_text))
    if (smooth_labels[i] != 'raw' || i > 1) {
      if (i == 1) {
        colnames(out_i)[4:ncol(out_i)] = paste0(
          colnames(out_i)[4:ncol(out_i)], '_', smooth_labels[i]
        )
      } else {
        out_i = out_i[, 4:ncol(out_i), drop = FALSE]
        colnames(out_i) = paste0(colnames(out_i), '_', smooth_labels[i])
      }
    }
    out = cbind(out, out_i)
  }

  # get syllable descriptives
  if (is.null(ptvStep)) ptvStep = step * 1000
  pitch_noNA = pitch
  pitch_noNA[is.na(pitch_noNA)] = 0
  duration = max(time, na.rm = TRUE) - min(time, na.rm = TRUE) + step

  syllables = findSyllables(
    pitch_noNA, step = step * 1000,
    windowLength = 0, threshold = 1e-12, shortestSyl = 0, shortestPause = 0)

  ptv = getPTV(syllables,
               duration = duration,
               ptvStep = ptvStep, ptvTime = ptvTime, ptvFreq = ptvFreq)

  if (!is.null(ptv$syllables) &&
      nrow(ptv$syllables) > 0 &&
      any(!is.na(ptv$syllables$start))) {
    ptv$syllables[, c('start_idx', 'end_idx')] = NULL
    sum_syl = summarizeAnalyze(
      ptv$syllables[, c('sylLen', 'pauseLen', 'sylRate', 'ptv'), drop = FALSE],
      summaryFun = summaryFun,
      var_noSummary = NULL)
  } else {
    ptv$syllables = data.frame()
    sum_syl = summarizeAnalyze(
      data.frame(sylLen = numeric(0), pauseLen = numeric(0),
                 sylRate = numeric(0), ptv = numeric(0)),
      summaryFun = summaryFun,
      var_noSummary = NULL)
  }
  out = cbind(out, sum_syl)

  return(list(summary = out, syllables = ptv$syllables, ptv = ptv$ptv))
}


#' Time series summary
#'
#' A helper function called by .pitchDescriptives for each smoothing level.
#' @param x numeric vector
#' @param step time step in s
#' @inheritParams pitchDescriptives
#' @noRd
timeSeriesSummary = function(x,
                             step,
                             inflThres = NULL,
                             extraSummaryFun = c(),
                             ref = 16.3516,
                             plot = FALSE,
                             main = '') {
  len = length(x)
  out = data.frame(duration = step * len)
  vars = c('durDefined', 'propDefined',
           'start', 'start_oct', 'end', 'end_oct',
           'mean', 'mean_oct', 'median', 'median_oct',
           'min', 'min_oct', 'time_min',
           'max', 'max_oct', 'time_max',
           'range', 'range_sem', 'sd', 'sd_sem', 'CV',
           'meanSlope', 'meanSlope_sem', 'meanAbsSlope', 'meanAbsSlope_sem',
           'maxAbsSlope', 'maxAbsSlope_sem', 'inflex')
  out[, vars] = NA

  lu = length(extraSummaryFun)
  if (lu > 0) {
    out[, extraSummaryFun] = NA
  }

  # transform to semitones, drop NAs
  x_noNA = as.numeric(na.omit(x))
  x_sem = HzToSemitones(x, ref = ref)
  x_sem[!is.finite(x_sem)] = NA
  x_sem_noNA = as.numeric(na.omit(x_sem))
  ran_not_NA = range(which(!is.na(x)))
  if (length(x_noNA) < 1) return(out)

  # user-defined function(x)
  if (lu > 0) {
    for (f in seq_len(lu)) {
      temp = try(do.call(extraSummaryFun[f], list(x)), silent = TRUE)
      if (!inherits(temp, 'try-error') && length(temp) == 1 && is.numeric(temp)) {
        out[, extraSummaryFun[f]] = temp
      }
    }
  }

  # basic descriptives
  out$durDefined = (diff(ran_not_NA) + 1) * step
  out$propDefined = sum(!is.na(x)) / len * 100
  out$start = x_noNA[1]
  out$end = tail(x_noNA, n = 1)
  out$mean = mean(x_noNA)
  out$median = median(x_noNA)

  # min, max
  idx_min = which.min(x)
  if (length(idx_min) > 0) {
    out$min = x[idx_min]
    if (diff(ran_not_NA) == 0) {
      out$time_min = 0.5
    } else {
      out$time_min = (idx_min - ran_not_NA[1]) / diff(ran_not_NA)
    }
  }
  idx_max = which.max(x)
  if (length(idx_max) > 0) {
    out$max = x[idx_max]
    if (diff(ran_not_NA) == 0) {
      out$time_max = 0.5
    } else {
      out$time_max = (idx_max - ran_not_NA[1]) / diff(ran_not_NA)
    }
  }

  # convert mean-related vars from Hz to octaves above C0
  vars_to_oct = c('start', 'end', 'mean', 'median', 'min', 'max')
  for (v in vars_to_oct) {
    out[, paste0(v, '_oct')] = HzToSemitones(out[, v], ref = 16.3516) / 12
  }

  # range, sd
  out$range = out$max - out$min
  out$range_sem = diff(range(x_sem_noNA))
  out$sd = sd(x_noNA)
  out$sd_sem = sd(x_sem_noNA)
  out$CV = out$sd / out$mean

  # slope
  pd = as.numeric(na.omit(diff(x))) / step
  pd_abs = abs(pd)
  pd_sem = as.numeric(na.omit(diff(x_sem))) / step
  pd_sem_abs = abs(pd_sem)

  if (length(pd) > 0) {
    out$meanSlope = mean(pd)
    out$meanSlope_sem = mean(pd_sem)
    out$meanAbsSlope = mean(pd_abs)
    out$meanAbsSlope_sem = mean(pd_sem_abs)
    out$maxAbsSlope = max(pd_abs)
    out$maxAbsSlope_sem = max(pd_sem_abs)
  }

  # inflections
  infl = findInflections(x_sem, thres = inflThres, ref = ref,
                         step = step, plot = plot, main = main)
  n_inflections = length(infl)
  out$inflex = n_inflections / out$durDefined
  return(out)
}


#' Find inflections
#'
#' Finds inflections in discrete time series such as pitch contours. When there
#' are no missing values and no thresholds, this can be accomplished with a fast
#' one-liner like \code{which(diff(diff(x) > 0) != 0) + 1}. Missing values are
#' interpolated by repeating the first and last non-missing values at the head
#' and tail, respectively, and by linear interpolation in the middle. Setting a
#' threshold means that small "wiggling" no longer counts. To use an analogy
#' with ocean waves, smoothing (low-pass filtering) removes the ripples and only
#' leaves the slow roll, while thresholding preserves only waves that are
#' sufficiently high, whatever their period.
#'
#' @seealso \code{\link{findPeaks}}
#' @param x numeric vector with or without NAs, expected to be in semitones for correct plotting
#' @param thres minimum vertical distance between two extrema for them to count
#'   as two independent inflections
#' @param step distance between values in s (only needed for plotting)
#' @param plot if TRUE, produces a simple plot
#' @param main plot title
#' @param ref reference value for converting semitones back to Hz for plotting
#' @return A vector of indices giving the location of inflections.
#' @noRd
#' @examples
#' x = sin(2 * pi * (1:100) / 15) * seq(1, 5, length.out = 100)
#' idx_na = c(1:4, 6, 7, 14, 25, 30:36, 39, 40, 42, 45:50,
#'            57, 59, 62, 66, 71:79, 98)
#' x[idx_na] = NA
#' soundgen:::findInflections(x, plot = TRUE)
#' soundgen:::findInflections(x, thres = 5, plot = TRUE)
#'
#' \dontrun{
#' for (i in 1:10) {
#'   temp = soundgen:::getRandomWalk(len = runif(1, 10, 100), rw_range = 10,
#'                                   rw_smoothing = runif(1, 0, 1))
#'   soundgen:::findInflections(temp, thres = 1, plot = TRUE)
#'   invisible(readline(prompt="Press [enter] to continue"))
#' }
#' }
findInflections = function(x,
                           thres = NULL,
                           step = NULL,
                           plot = FALSE,
                           main = '',
                           ref = 16.3516) {
  if (length(x) == 0) return(numeric(0))

  # remove leading/trailing NAs
  orig = x  # for plotting
  r = rle(is.na(x))
  x = na.trim(x)
  if (length(x) == 0) return(numeric(0))

  if (r$values[1]) {
    shift = r$lengths[1]
  } else {
    shift = 0
  }

  len = length(x)
  if (len < 3) return(numeric(0))
  if (any(is.na(x))) {
    xInt = approx(x, n = len, na.rm = TRUE)$y
  } else {
    xInt = x
  }
  extrema = which(diff(diff(xInt) > 0) != 0) + 1

  # threshold
  if (!is.null(thres) && is.finite(thres) && thres > 0 && length(extrema) > 0) {
    for (iter in 1:100) {
      de = abs(diff(c(xInt[1], xInt[extrema])))
      idx_keep = which(de >= thres)
      new_extrema = extrema[idx_keep]

      # get rid of "staircase effects"
      de = c(xInt[1], xInt[new_extrema], xInt[len])
      new_extrema = new_extrema[which(diff(diff(de) > 0) != 0)]

      # check last extremum
      le = length(new_extrema)
      if (le > 0) {
        d_last = abs(tail(xInt, 1) - xInt[tail(new_extrema, 1)])
        if (d_last < thres) new_extrema = new_extrema[-le]
      }

      if (identical(extrema, new_extrema)) break
      extrema = new_extrema
      if (length(extrema) == 0) break
    }
  }
  extrema = extrema + shift

  if (plot) {
    if (!is.null(step)) {
      time = ((seq_along(orig)) - 0.5) * step
      xlab = 'Time, s'
      xaxt = 'n'
    } else {
      xlab = ''
      xaxt = 's'
    }

    # reconstruct full length vectors for plotting
    orig_plot = semitonesToHz(orig, ref = ref)
    xInt_plot = c(rep(NA, shift), xInt, rep(NA, length(orig) - shift - len))
    xInt_plot = semitonesToHz(xInt_plot, ref = ref)

    plot(xInt_plot, type = 'b', col = 'gray70', main = main,
         pch = 16, cex = .5, ylab = 'Hz', xlab = xlab, xaxt = xaxt)
    points(orig_plot, pch = 16, type = 'b')
    le = length(extrema)
    if (le > 0) {
      points(extrema, orig_plot[extrema], col = 'blue', pch = 18)
      usr = par('usr')
      for (i in seq_len(le))
        segments(x0 = extrema[i], y0 = usr[3], y1 = orig_plot[extrema[i]],
                 lty = 1, lwd = .25, col = 'black')
    }
    if (!is.null(step)) {
      pt = pretty(time)
      axis(1, at = pt / step + .5, labels = pt)
    }
  }
  extrema
}
