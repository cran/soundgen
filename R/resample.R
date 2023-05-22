#' Resample a vector
#'
#' Changes the sampling rate without introducing artefacts like aliasing.
#' Algorithm: to downsample, applies a low-pass filter, then decimates with
#' \code{approx}; to upsample, performs linear interpolation with \code{approx},
#' then applies a low-pass filter. NAs can be interpolated or preserved in the
#' output. The length of output is determined, in order of precedence, by
#' \code{len / mult / samplingRate_new}. For simple vector operations, this is
#' very similar to approx, but the leading and trailing NAs are also preserved
#' (see examples).
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param mult multiplier of sampling rate: new sampling rate = old sampling
#'   rate x mult, so 1 = no effect, >1 = upsample, <1 = downsample
#' @param len if specified, overrides mult and samplingRate_new and simply
#'   returns a vector of length \code{len}
#' @param samplingRate_new an alternative to \code{mult} provided that the old
#'   \code{samplingRate is know} (NB: \code{mult} takes precedence)
#' @param lowPass if TRUE, applies a low-pass filter before decimating or after
#'   upsampling to avoid aliasing
#' @param na.rm if TRUE, NAs are interpolated, otherwise they are preserved in
#'   the output
#' @export
#' @examples
#' ## Example 1: a short vector with NAs
#' x = c(NA, 1, 2, 3, NA, NA, 6, 7, 8, NA)
#'
#' # upsample
#' resample(x, mult = 3.5, lowPass = FALSE, plot = TRUE)  # just approx
#' resample(x, mult = 3.5, lowPass = TRUE, plot = TRUE) # low-pass + approx
#' resample(x, mult = 3.5, lowPass = FALSE, na.rm = TRUE, plot = TRUE)
#'
#' # downsample
#' resample(x, mult = 0.5, lowPass = TRUE, plot = TRUE)
#' resample(x, mult = 0.5, na.rm = TRUE, plot = TRUE)
#' resample(x, len = 5, na.rm = TRUE, plot = TRUE) # same
#'
#' # The most important TIP: use resample() for audio files and the internal
#' # soundgen:::.resample(list(sound = ...)) for simple vector operations because
#' # it's >1000 times faster. For example:
#' soundgen:::.resample(list(sound = x), mult = 3.5, lowPass = FALSE)
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
#' spectrogram(x2bad, samplingRate * 3)  # high-frequency artefacts
#'
#' \dontrun{
#' # Example 3: resample all audio files in a folder to 8000 Hz
#' resample('~/Downloads/temp', saveAudio = '~/Downloads/temp/sr8000/',
#'          samplingRate_new = 8000, savePlots = '~/Downloads/temp/sr8000/')
#' }
resample = function(x,
                    samplingRate = NULL,
                    samplingRate_new = NULL,
                    mult = NULL,
                    len = NULL,
                    lowPass = TRUE,
                    na.rm = FALSE,
                    reportEvery = NULL,
                    cores = 1,
                    saveAudio = NULL,
                    plot = FALSE,
                    savePlots = NULL,
                    width = 900,
                    height = 500,
                    units = 'px',
                    res = NA,
                    ...) {
  # check mult / samplingRate_new
  if (is.null(mult) & is.null(len)) {
    if (is.null(samplingRate_new))
      stop('Please specify len, mult or samplingRate_new')
  }
  if (is.null(samplingRate)) samplingRate = 1  # just to avoid warnings

  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'savePlots',
    'saveAudio', 'width', 'height', 'units')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = '.resample',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "resample", width = paste0(width, units)))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Resample per sound
#'
#' Internal soundgen function
#'
#' @param audio a list returned by \code{readAudio}
#' @inheritParams intplNA
#' @inheritParams resample
#' @inheritParams segment
#' @keywords internal
.resample = function(audio,
                     mult = NULL,
                     len = NULL,
                     samplingRate_new = NULL,
                     lowPass = TRUE,
                     na.rm = FALSE,
                     nPoints = 1,
                     plot = FALSE,
                     width = 900,
                     height = 500,
                     units = 'px',
                     res = NA,
                     ...) {
  # calculate length
  x = audio$sound
  n1 = length(x)
  if (is.null(len)) {
    if (is.null(mult)) mult = samplingRate_new / audio$samplingRate
    n2 = round(n1 * mult)
  } else {
    n2 = len
    mult = n2 / n1
  }
  if (mult == 1) return(x)

  # prepare to deal with NAs
  idx_na = which(is.na(x))
  n_na = length(idx_na)
  any_na = n_na > 0
  if (any_na) {
    if (n_na == n1) return(rep(NA, n2))
    x_noNA = intplNA(x, idx_na = idx_na, nPoints = nPoints)
  } else {
    x_noNA = x
  }
  if (!any(diff(x_noNA) != 0)) {
    out = rep(x_noNA[1], n2)
  } else {
    # up- or downsample
    if (!is.finite(mult) | mult < 0) {
      stop('mult must be a real positive number')
    } else if (mult == 1 | length(idx_na) == n1) {
      # nothing to do
      out = x
    } else if (mult < 1) {
      # downsample
      if (lowPass) x_noNA = .bandpass(
        list(sound = x_noNA, samplingRate = 1000),
        upr = 1000 / 2 * mult)
      out = approx(x_noNA, n = n2)$y
    } else if (mult > 1) {
      # upsample
      out = approx(x_noNA, n = n2)$y
      if (lowPass) out = .bandpass(
        list(sound = out, samplingRate = 1000),
        upr = 1000 / 2 / mult)
    }
  }

  # put NAs back in
  if (!na.rm & any_na) {
    # find NA positions in the new sound
    d = diff(is.na(x))  # 1 = beginning of NA episode, -1 = end of NA episode
    beg = which(d == 1) + 1
    if (is.na(x[1])) beg = c(1, beg)
    end = which(d == -1) + 1
    if (is.na(x[n1]) & !n1 %in% end) end = c(end, n1 + 1)
    time_stamps = (0:n1) / n1  # the beg of each frame plus one extra
    na_pos = data.frame(beg = time_stamps[beg], end = time_stamps[end])
    na_idx = numeric(0)
    for (i in 1:length(beg)) {
      idx_start = round(time_stamps[beg[i]] * n2) + 1
      n_na = round((na_pos$end[i] - na_pos$beg[i]) * n2)
      if (n_na > 0) na_idx = c(na_idx, idx_start:(min(n2, (idx_start + n_na - 1))))
    }
    na_idx = unique(na_idx[na_idx > 0 & na_idx <= n2])
    out[na_idx] = NA  # fill in NAs in the new vector
  }

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_resample.png"),
        width = width, height = height, units = units, res = res)
  }
  if (!exists('time_stamps')) time_stamps = seq(0, 1, length.out = n1)
  if (plot) {
    if (audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
    plot(seq(0, 1, length.out = n1), x, type = 'p', ylim = range(c(x, out), na.rm = TRUE),
         xlab = 'Relative position', ylab = '', main = main)
    points(x = seq(0, 1, length.out = n2), y = out,
           type = 'b', col = 'red', pch = 3)
    if (is.character(audio$savePlots)) dev.off()
  }

  if (!is.null(audio$saveAudio)) {
    if (!dir.exists(audio$saveAudio)) dir.create(audio$saveAudio)
    audio$samplingRate = audio$samplingRate * mult
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(out, audio = audio, filename = filename)
  }

  return(out)
}

