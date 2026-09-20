#' Resample a vector
#'
#' Changes the sampling rate of a sound or another time series without aliasing.
#' Similar in spirit and speed to \link[signal]{resample} in the signal package,
#' but also works for short vectors and preserves NAs approximately after
#' resampling. Use resample() for interpolation when the points are evenly
#' spaced, when there are missing values, or when we care about aliasing. When
#' the points are unevenly spaced, use \link{interpolate} instead, but then
#' there should not be any missing values.
#'
#' Algorithm: to downsample, applies a low-pass filter, then decimates with
#' \code{spline}; to upsample, performs a specified type of interpolation
#' (defaults to monotone Fritsch‑Carlson splines, see \link{interpolate}), then
#' applies a low-pass filter. NAs can be interpolated or preserved in the
#' output. The length of output is determined, in order of precedence, by
#' \code{len / mult / samplingRate_new}. For simple vector operations, this is
#' very similar to approx, but the leading and trailing NAs are also preserved
#' when \code{na.rm = FALSE} (see examples).
#'
#' @seealso \link{interpolate} \link{interpolateNA}
#'
#' @inheritParams .roxygen_defaults
#' @param mult multiplier of sampling rate: new sampling rate = old sampling
#'   rate x mult, so 1 = no effect, >1 = upsample, <1 = downsample
#' @param len if specified, overrides mult and samplingRate_new and simply
#'   returns a vector of length \code{len}
#' @param samplingRate_new an alternative to \code{mult} provided that the old
#'   \code{samplingRate is known} (NB: \code{mult} takes precedence)
#' @param lowPass if TRUE, applies a low-pass filter before decimating or after
#'   upsampling to avoid aliasing
#' @param interpol the method of interpolation (only used for upsampling,
#'   not downsampling); see \link{interpolate} for details
#' @param na.rm if TRUE, NAs are linearly interpolated before resampling; if
#'   FALSE they are preserved in the interpolated output in (approximately) the
#'   same position
#'
#' @return A numeric vector.
#' @export
#' @examples
#' ## Example 1: a short vector with NAs
#' x = c(NA, 1, 2, 3, NA, NA, 6, 9, 8, NA)
#'
#' # downsample
#' print(resample(x, len = 5))  # NAs are propagated
#' print(resample(x, len = 5, na.rm = TRUE))  # NAs are interpolated
#' print(resample(x, mult = 1/2, na.rm = TRUE, plot = TRUE)) # same
#'
#' # upsample
#' resample(x, mult = 3.5, lowPass = FALSE, plot = TRUE)  # just approx
#' resample(x, mult = 3.5, lowPass = TRUE, plot = TRUE) # low-pass + approx
#' resample(x, mult = 3.5, lowPass = FALSE, na.rm = TRUE, plot = TRUE)
#'
#' # change the method of interpolation
#' resample(x, mult = 15, lowPass = FALSE, interpol = 'pchip', plot = TRUE)
#' resample(x, mult = 15, lowPass = FALSE,
#'   interpol = interpol_loess(span = .6), plot = TRUE)
#'
#' ## Example 2: a sound
#' silence = rep(0, 10)
#' samplingRate = 1000
#' fr = seq(100, 300, length.out = 400)
#' x = c(silence, sin(cumsum(fr) * 2 * pi / samplingRate), silence)
#' spectrogram(x, samplingRate)
#'
#' # downsample
#' x1 = resample(x, mult = 1 / 2.5)
#' spectrogram(x1, samplingRate / 2.5)  # no aliasing
#' # cf:
#' x1bad = resample(x, mult = 1 / 2.5, lowPass = FALSE)
#' spectrogram(x1bad, samplingRate / 2.5)  # aliasing
#'
#' # upsample
#' x2 = resample(x, mult = 3)
#' spectrogram(x2, samplingRate * 3)  # nothing above the old Nyquist
#' # cf:
#' x2bad = resample(x, mult = 3, lowPass = FALSE)
#' spectrogram(x2bad, samplingRate * 3)  # high-frequency artifacts
#'
#' \dontrun{
#' # Example 3: resample all audio files in a folder to 8000 Hz
#' resample('~/Downloads/temp', saveAudio = TRUE,
#'          samplingRate_new = 8000, savePlots = TRUE)
#' }
resample = function(
    x,
    samplingRate = NULL,
    samplingRate_new = NULL,
    mult = NULL,
    len = NULL,
    lowPass = TRUE,
    interpol = 'splineFC',
    na.rm = FALSE,
    reportEvery = NULL,
    cores = 1,
    saveAudio = FALSE,
    plot = FALSE,
    savePlots = FALSE,
    embed = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  # check mult / samplingRate_new
  if (is.null(mult) && is.null(len)) {
    if (is.null(samplingRate_new))
      stop('Please specify len, mult or samplingRate_new')
  }
  if (is.null(samplingRate)) samplingRate = 1  # just to avoid warnings

  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'savePlots', 'embed', 'saveAudio')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    funToCall = '.resample',
                    suffix = 'resample',
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units),
                  changesAudio = saveAudio, embed = embed))

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Resample per sound
#' @param audio a list returned by \code{readAudio}
#' @noRd
.resample = function(
    audio,
    mult = NULL,
    len = NULL,
    samplingRate_new = NULL,
    lowPass = TRUE,
    interpol = 'splineFC',
    na.rm = FALSE,
    nPoints = 1,
    plot = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  # calculate length
  x = audio$sound
  n1 = length(x)
  if (n1 == 0) return(NA)
  if (is.null(len)) {
    if (is.null(mult)) mult = samplingRate_new / audio$samplingRate
    n2 = round(n1 * mult)
  } else {
    n2 = len
    mult = n2 / n1
  }

  if (!is.finite(mult) || mult <= 0 || n2 < 1) {
    warning('mult must be a real positive number and produce output >=1 samples long')
    out = x
  } else if (abs(mult - 1) < .Machine$double.eps^0.5) {
    out = x
  } else {
    # prepare to deal with NAs
    idx_na = which(is.na(x))
    n_na = length(idx_na)
    any_na = n_na > 0
    if (any_na) {
      if (n_na == n1) return(rep(NA, n2))
      x_noNA = interpolateNA(x, idx_na = idx_na, nPoints = nPoints)
    } else {
      x_noNA = x
    }
    if (!any(diff(x_noNA) != 0)) {
      out = rep(x_noNA[1], n2)
    } else {
      # up- or downsample
      if (mult < 1) {
        # downsample (we don't care what the sampling rate is, just set to an
        # arbitrary number like 1000)
        if (lowPass) x_noNA = .bandpass(
          list(sound = x_noNA, samplingRate = 1000),
          upr = 1000 * mult / 2 * .9,
          bw = 1000 * mult / 2 / 10)
        # NB: nice to avoid a sharp dropoff at Nyquist, ramp over ~10% of Nyquist
        out = spline(x_noNA, n = n2)$y
        # NB: approx introduces artifacts; simple decimation is ~10 times faster,
        # but also introduces artifacts
        # out = x_noNA[seq(1, n1, length.out = n2)]
      } else if (mult > 1) {
        # upsample
        out = interpolate(x = 1:n1, y = x_noNA, xout = seq(1, n1, length.out = n2),
                          method = interpol)
        if (lowPass) out = .bandpass(
          list(sound = out, samplingRate = 1000),
          upr = 1000 / 2 / mult * .9,
          bw = 1000 / 2 / mult / 10)
      }
    }

    # put NAs back in
    if (!na.rm && any_na) {
      # NA interval detection
      d = diff(is.na(x))
      beg = which(d == 1) + 1
      if (is.na(x[1])) beg = c(1, beg)
      end = which(d == -1) + 1
      if (is.na(x[n1])) end = c(end, n1 + 1)

      # inclusive bounds of each NA episode
      a = beg  # start index (1-based)
      b = end - 1  # end index (inclusive)

      # scale positions to new length n2
      start_new = round((a - 1) / n1 * n2) + 1
      len_new = round((b - a + 1) / n1 * n2)
      # number of NA samples in resampled vector

      # keep only intervals that will actually contribute
      keep = len_new > 0 & start_new <= n2
      start_new = start_new[keep]
      len_new = len_new[keep]

      if (length(start_new) > 0) {
        # vectorised expansion: generate all target indices
        idx = rep(start_new, times = len_new) + sequence(len_new) - 1
        idx = idx[idx > 0 & idx <= n2]  # just in case
        out[idx] = NA
      }
    }
  }

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    if (is.null(audio$filename_noExt) || audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
    plot(seq(0, 1, length.out = n1), x, type = 'p',
         ylim = range(c(x, out), na.rm = TRUE),
         xlab = 'Relative position', ylab = '', main = main)
    points(x = seq(0, 1, length.out = n2), y = out,
           type = 'b', col = 'red', pch = 3)
  }

  # save audio
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    audio$samplingRate = audio$samplingRate * mult
    writeAudio(out, audio = audio, filename = filename)
  }

  out
}
