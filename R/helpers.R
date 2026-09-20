### SIMPLE HELPER FUNCTIONS ###

#' Report CI
#'
#' A simple function for formatting and printing estimates with their confidence
#' intervals or similar statistics that follow a "X [Y, Z]" format. Takes a
#' numeric vector or matrix with three elements / columns: estimate, lower quantile
#' from a CI, and upper quantile from a CI. For each row, it prints the result
#' as "estimate [CI]".
#' @param n numeric vector or matrix-like object
#' @param digits number of decimal points to preserve
#' @param suffix a string to print between the first value and the values in []
#' @return Does not return anything, just prints the estimate and CI.
#' @export
#' @examples
#' n = rnorm(100)
#' reportCI(quantile(n, probs = c(.5, .025, .975)))
#'
#' a = data.frame(fit = c(3, 5, 7),
#'                lwr = c(1, 4, 6.5),
#'                upr = c(5, 6, 7.1))
#' reportCI(a, 1)
#' reportCI(a, 1, ' cm')
#' reportCI(a, 1, '%, 95% CI')
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
#' @return Does not return anything, just prints the time left.
#' @export
#' @examples
#' time_start = proc.time()
#' nIter = 100
#' for (i in 1:nIter) {
#'   Sys.sleep(i ^ 1.02 / 10000)
#'   reportTime(i, time_start, nIter,
#'     jobs = (1:100) ^ 1.02, prefix = 'Chain 1: ')
#' }
#'
#' # Unknown number of iterations:
#' time_start = proc.time()
#' for (i in 1:20) {
#'   Sys.sleep(i ^ 2 / 10000)
#'   reportTime(i = i, time_start = time_start,
#'   jobs = (1:20) ^ 2, reportEvery = 5)
#' }
#'
#' \dontrun{
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
  if (is.null(reportEvery)) {
    reportEvery = ifelse(is.null(nIter),
                         1,
                         max(1, 10 ^ (floor(log10(nIter)) - 1)))
  } else if (isFALSE(reportEvery)) {
    return(invisible())
  }
  if (is.null(nIter)) {
    # number of iter unknown, so we just report time elapsed
    if (i %% reportEvery == 0) {
      cat(paste0(prefix, 'Completed ', i, ' iterations in ',
                 convert_sec_to_hms(time_diff)), "\n")
    }
  } else {
    # we know how many iter, so we also report time left
    if (i == nIter) {
      time_total = convert_sec_to_hms(time_diff)
      cat(paste0(prefix, 'Completed ', i, ' iterations in ', time_total, '.'), "\n")
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
        cat(paste0(prefix, 'Done ', i, ' / ', nIter,
                   '; Estimated time left: ', time_left_hms), "\n")
      }
    }
  }
  flush.console()   # to ensure immediate display when calling during parallel processing
}


#' Print time
#' Converts time in seconds to time in y m d h min s for pretty printing.
#' @param time_s time (s)
#' @param digits number of digits to preserve for s (1-60 s)
#' @return A character string like "1 h 20 min 3 s".
#' @noRd
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

    days = time_s %/% 86400
    hours = floor((time_s %% 86400) / 3600)
    minutes = floor((time_s %% 3600) / 60)
    seconds = time_s %% 60
    ms = round((time_s %% 1) * 1000, 1)

    for (i in 1:len) {
      days_string = hours_string = minutes_string = seconds_string = ms_string = ''
      if (days[i] > 0) days_string = paste(days[i], 'd ')
      if (hours[i] > 0) hours_string = paste(hours[i], 'h ')
      if (days[i] == 0) {
        if (minutes[i] > 0) minutes_string = paste(minutes[i], 'min ')
        if (hours[i] == 0) {
          seconds_floor = floor(seconds[i])
          if (seconds_floor > 0) seconds_string = paste(round(seconds[i], digits), 's ')
          if (minutes[i] == 0 && seconds_floor == 0) {
            if (ms[i] > 0) ms_string = paste(ms[i], 'ms')
          }
        }
      }
      output[i] = paste0(days_string, hours_string,
                         minutes_string, seconds_string, ms_string)
    }
  }
  trimws(output)
}


#' Switch color theme
#' @param colorTheme string like 'bw', 'seewave', or function name
#' @noRd
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
    color.palette = seewave.col
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
#' @noRd
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


#' Seewave colors
#'
#' Internal soundgen function for generating a seewave-like palette, as in
#' \code{\link[seewave]{spectro.colors}}.
#' @param n number of colors
#' @noRd
seewave.col = function(n) {
  n = as.integer(n[1])
  if (n > 0) {
    j = k = n %/% 3
    i = n - j - k
    c(
      if (i > 0) hsv(h = seq(from = 31/60, to = 43/60, length = i),
                     s = seq(0, 1, length = i)),
      if (j > 0) hsv(h = seq(from = 21/60, to = 9/60, length = j),
                     v = seq(0.5, 0.8, length = j)),
      if (k > 0) hsv(h = seq(from = 8/60, to = 1/60, length = k),
                     s = seq(from = 0.5, to = 1, length = k), v = 1))
  }
  else character(0)
}


#' Exhaustive checks of wl / step / overlap for analyze-like functions.
#' @param audio we only really need audio$dur and audio$samplingRate
#' @noRd
validateWlOvlp = function(audio, windowLength, step, overlap) {
  # windowLength must be a positive finite scalar, in ms, not exceeding half
  # the sound duration
  if (is.null(audio$duration) || !is.finite(audio$duration)) audio$duration = Inf
  if (is.null(audio$samplingRate)) stop('sampling rate not speficied')
  if (length(windowLength) != 1 ||
      !is.numeric(windowLength) ||
      !is.finite(windowLength) ||
      windowLength <= 0 ||
      windowLength > (audio$duration / 2 * 1000)) {
    min_wl_ms = 4 * 1000 / audio$samplingRate
    windowLength = min(50, round(audio$duration / 2 * 1000))
    if (!is.finite(windowLength) || windowLength < min_wl_ms) {
      windowLength = min_wl_ms
    }
    warning(paste0(
      '"windowLength" must be between 0 and half the sound duration (in ms);
            resetting to ', windowLength, ' ms')
    )
  }

  if (is.null(step)) {
    if (is.null(overlap)) {
      stop('Need to specify either step or overlap')
    } else {
      if (length(overlap) != 1 ||
          !is.numeric(overlap) ||
          !is.finite(overlap) ||
          overlap < 0 ||
          overlap >= 100) {
        stop('overlap must be a single number between 0 and 100')
      }
      step = windowLength * (1 - overlap / 100)
    }
  }

  if (length(step) != 1 ||
      !is.numeric(step) ||
      !is.finite(step) ||
      step <= 0 ||
      step > (audio$duration * 1000)) {
    step = windowLength / 2
    warning('"step" must be between 0 and sound_duration ms;
            defaulting to windowLength / 2')
  }

  if (step > windowLength)
    warning(paste('"step" should normally not be larger than "windowLength" ms:',
                  'you are skipping parts of the sound!'))

  wl = max(4, round(windowLength / 1000 * audio$samplingRate))
  # to speed up FFT, could use nextn(wl)
  step_points = max(1, round(step / 1000 * audio$samplingRate))
  step = step_points * 1000 / audio$samplingRate
  # ≠ the original step b/c of limited time resolution
  overlap = max(0, 100 * (1 - step_points / wl))

  return(list(windowLength = windowLength, wl = wl,
              step_points = step_points, step = step, overlap = overlap))
}


#' Split vector into chunks
#'
#' Takes a numeric vector x and splits it into n chunks. Adapted from
#' parallel::splitIndices. See also
#' https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
#' @param x the length of input vector
#' @param n number of chunks
#' @return Returns a list of length \code{n} containing the chunks
#' @noRd
#' @examples
#' # prepare chunks of iterator to run in parallel on several cores
#' soundgen:::splitIntoChunks(7, 3)
#' soundgen:::splitIntoChunks(21, 4)
#' soundgen:::splitIntoChunks(3, 1)
#' soundgen:::splitIntoChunks(21, 8)
splitIntoChunks = function(x, n) {
  n = min(n, x)
  i = seq_len(x)
  len_chunk = ceiling(x / n)
  fuzz = min((x - 1L) / 1000, 0.4 * x / n)
  breaks = seq(1 - fuzz, x + fuzz, length.out = n + 1L)
  structure(split(i, cut(i, breaks)), names = NULL)
}
