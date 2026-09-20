#' Oscillogram
#'
#' Plots the oscillogram (waveform) of a sound on a linear or logarithmic scale
#' (in dB). To get a dB scale, centers and normalizes the sound, then takes a
#' logarithm of the positive part and a flipped negative part, which is
#' analogous to "Waveform (dB)" view in Audacity. For more plotting options,
#' check the "oscillo" function in the seewave package.
#'
#' @inheritParams .roxygen_defaults
#' @param dB if TRUE, plots on a dB instead of linear scale
#' @param returnWave if TRUE, returns a log-transformed waveform as a numeric vector
#' @param xlab,ylab axis labels
#' @param main plot title
#' @param ylim override default amplitude scale for non-centered sounds
#' @param bty box type (see \code{?par})
#' @param midline if TRUE, draws a line at 0 dB
#' @param maxPoints the maximum number of points to plot (speeds up the plotting
#'   of long audio files, but beware of aliasing artifacts)
#' @param ... Other graphical parameters passed on to \code{plot()}
#'
#' @return If \code{returnWave = TRUE}, returns the waveform as a numeric
#'   vector on the original or dB scale; otherwise, does not return anything.
#' @export
#' @examples
#' sound = sin(1:2000/10) * approx(c(1, .01, .5), n = 2000)$y
#'
#' # Oscillogram on a linear scale without bells and whistles, just base R
#' plot(sound, type = 'l')
#'
#' # Oscillogram options with soundgen
#' osc(sound)             # linear
#' osc(sound, dB = TRUE)  # dB
#'
#' # For numeric vectors, indicate samplingRate and scale (max amplitude)
#' osc(sound, samplingRate = 1000, scale = 100, dB = TRUE)
#'
#' # Embellish and customize the plot
#' o = osc(sound, samplingRate = 1000, dB = TRUE, midline = FALSE,
#'         main = 'My waveform', col = 'blue', returnWave = TRUE)
#' abline(h = -80, col = 'orange', lty = 3)
#' o[1:10]  # the waveform in dB
#'
#' # Wave object
#' data(speechEx, package = 'soundgen')
#' osc(speechEx, from = 2, to = 2.5, dB = TRUE)
#'
#' # for long files, reduce the resolution to plot quickly (careful: if the
#' # resolution is too low, antialiasing may cause artifacts)
#' osc(speechEx, dB = TRUE, maxPoints = 2500)
#'
#' \dontrun{
#' # saves oscillograms of all audio files in a folder
#' osc('~/Downloads/temp', savePlots = TRUE)
#' }
osc = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    dynamicRange = 80,
    dB = FALSE,
    returnWave = FALSE,
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    ylim = NULL,
    bty = 'n',
    midline = TRUE,
    maxPoints = 10000,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'reportEvery', 'cores', 'savePlots', 'embed')]

  # call .osc
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.osc',
    suffix = 'osc',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))
  if (returnWave) {
    if (pa$input$n == 1) pa$result = pa$result[[1]]
    invisible(pa$result)
  }
}


#' Oscillogram per sound
#'
#' Internal soundgen function called by \code{\link{osc}}.
#' @inheritParams osc
#' @noRd
.osc = function(
    audio,
    dynamicRange = 80,
    dB = FALSE,
    returnWave = FALSE,
    plot = TRUE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    ylim = NULL,
    bty = 'n',
    midline = TRUE,
    maxPoints = 10000,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # get original range
  rs = range(audio$sound, na.rm = TRUE)
  d = diff(rs)
  if (!is.null(audio$scale)) {
    mult = d / 2 / audio$scale
    m = audio$scale
  } else {
    mult = 1  # assume max amplitude
    m = max(abs(rs))
  }
  if (is.null(audio$timeShift)) audio$timeShift = 0

  if (dB) {
    # center and normalize to range from -1 to +1, unless it is quieter than maxAmpl
    ms = median(audio$sound)
    audio$sound = audio$sound - ms
    rs = rs - ms
    maxs = max(abs(rs))
    if (maxs > 0) {
      audio$sound = audio$sound / maxs * mult
    } else {
      audio$sound = rep(-dynamicRange, audio$ls)
    }

    # treat smaller values as 0 (beyond dynamic range)
    floor = 10^(-dynamicRange / 20)
    pos = audio$sound >= floor
    neg = audio$sound <= -floor
    zero = !pos & !neg

    # log-transform
    audio$sound[pos] = 20 * log10(audio$sound[pos])
    audio$sound[neg] = -20 * log10(-audio$sound[neg]) - 2 * dynamicRange
    audio$sound[zero] = -dynamicRange
    midline_pos = -dynamicRange
  } else {
    if (rs[1] <= 0 && rs[2] >= 0) {
      midline_pos = 0
    } else {
      midline_pos = median(rs)
    }
  }

  # plot
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    if (is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }

    # For long files, downsample before plotting
    if (!is.null(maxPoints) && maxPoints < audio$ls) {
      myseq = seq(1, audio$ls, length.out = maxPoints)
      audio$sound = audio$sound[myseq]
      # Get time stamps
      time = myseq / audio$samplingRate * 1000 + audio$timeShift
        if (is.null(xlab)) xlab = 'Time'
    } else {
      # Get time stamps
      time = (1:audio$ls) / audio$samplingRate * 1000 + audio$timeShift
        if (is.null(xlab)) xlab = 'Time'
    }
    if (is.null(ylab)) if (dB) ylab = 'dB' else ylab = ''
    if (is.null(ylim)) if (dB) ylim = c(-2 * dynamicRange, 0) else ylim = c(-m, m)

    # plot
    plot(time, audio$sound, type = 'l', main = main, xlab = xlab, ylab = ylab,
         bty = bty, xaxt = 'n', yaxt = 'n', ylim = ylim, ...)
    time_location = axTicks(1)
    if (!is.null(audio$samplingRate)) {
      time_labels = convert_sec_to_hms(time_location / 1000, 3)
    } else {
      time_labels = time_location
    }
    axis(side = 1, at = time_location, labels = time_labels)
    if (dB) {
      axis(side = 2, at = seq(-dynamicRange, 0, by = 10))
    } else {
      axis(side = 2)
    }
    if (midline) abline(h = midline_pos, lty = 2, col = 'gray70')
  }
  if (returnWave) return(audio$sound)
}
