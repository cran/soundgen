#' Phasegram
#'
#' Produces a phasegram of a sound or another time series, which is a collection
#' of Poincare sections cut through phase portraits of consecutive frames. The x
#' axis is time, just as in a spectrogram, the y axis is a slice through the
#' phase portrait, and the color shows the density of trajectories at each point
#' of the phase portrait.
#'
#' Algorithm: the input sound is normalized to [-1, 1] and divided into
#' consecutive frames \code{windowLength} ms long without multiplying by any
#' windowing function (unlike in STFT). For each frame, a phase portrait is
#' obtained by time-shifting the frame by \code{timeLag} ms. A Poincare section
#' is taken through the phase portrait (currently at a fixed angle, namely the
#' default in \code{\link[nonlinearTseries]{poincareMap}}), giving the
#' intersection points of trajectories with this bisecting line. The density of
#' intersections is estimated with a smoothing kernel of bandwidth \code{bw} (as
#' an alternative to using histogram bins). The density distributions per frame
#' are stacked together into a phasegram (output: "orig"). The ranges of phase
#' portraits depend on the amplitude of signal in each frame. The resulting
#' phasegram can optionally be rasterized to smooth it for plotting (output:
#' "rasterized").
#'
#' @inheritParams spectrogram
#' @inheritParams nonlinStats
#' @param windowLength the length of each frame analyzed separately (ms)
#' @param step time step between consecutive frames (ms)
#' @param timeLag time lag between the original and time-shifted version of each
#'   frame that together represent the phase portrait (ms). Defaults to the
#'   number of steps beyond which the mutual information function reaches its
#'   minimum or, if that fails, the steps until mutual information experiences
#'   the first exponential decay - see \code{\link[nonlinearTseries]{timeLag}}
#' @param theilerWindow time lag between two points that are considered locally
#'   independent and can be treated as neighbors in the reconstructed phase
#'   space. defaults to the first minimum or, if unavailable, the first zero of
#'   the autocorrelation function (or, failing that, to \code{timeLag * 2})
#' @param nonlinStats nonlinear statistics to report: "ed" = the optimal number
#'   of embedding dimensions, "d2" = correlation dimension D2, "ml" = maximum
#'   Lyapunov exponent, "sur" = the results of surrogate data testing for
#'   stochasticity. These are calculated using the functionality of the package
#'   nonlinearTseries, which is seriously slow, so the default is just to get
#'   the phasegram itself
#' @param bw standard deviation of the smoothing kernel, as in
#'   \code{\link[stats]{density}}
#' @param bins the number of bins along the Y axis after rasterizing (has no
#'   effect if \code{rasterize = FALSE})
#' @param rasterize if FALSE, only plots and returns Poincare sections on the
#'   original scale (most graphical parameters will then have no effect); if
#'   TRUE, rasterizes the phasegram matrix and plots it with more graphical
#'   parameters
#'@param xlab,ylab,main graphical parameters passed to
#'  soundgen:::filled.contour.mod (if \code{rasterize = TRUE}) or plot (if
#'  \code{rasterize = FALSE})
#'@param ... other graphical parameters passed to soundgen:::filled.contour.mod
#'  (if \code{rasterize = TRUE})  or plot (if \code{rasterize = FALSE})
#'
#' @references \itemize{
#'   \item Herbst, C. T., Herzel, H., Švec, J. G., Wyman, M. T., & Fitch, W.
#'   T. (2013). Visualization of system dynamics using phasegrams. Journal of
#'   the Royal Society Interface, 10(85), 20130288.
#'   \item Huffaker, R., Huffaker, R. G., Bittelli, M., & Rosa, R. (2017). Nonlinear time series analysis with R. Oxford University Press.
#' }
#'
#' @return Returns a list of three components: "orig" = the full phasegram;
#'   "rasterized" = a rasterized version. For both, $time is the middle of each
#'   frame (ms), $x is the coordinate along a Poincare section (since the audio
#'   is normalized, the scale is [-1, 1]), and $y is the density of
#'   intersections of system trajectories with the Poincare section. The third
#'   component is $descriptives, which gives the result of nonlinear analysis
#'   per frame. Currently implemented: shannon = Shannon entropy of Poincare
#'   sections, nPeaks = log-number of peaks in the density distribution of
#'   Poincare sections, ml = maximum Lyapunov exponent (positive values suggest
#'   chaos), ed = optimal number of embedding dimensions (shows the complexity
#'   of the reconstructed attractor), d2 = correlation dimension, sur =
#'   probability of stochasticity according to surrogate data testing (0 =
#'   deterministic, 1 = stochastic).
#'
#' @export
#' @examples
#' target = soundgen(sylLen = 300, pitch = c(350, 420, 420, 410, 340) * 3,
#'   subDep = c(0, 0, 60, 50, 0, 0) / 2, addSilence = 0, plot = TRUE)
#' # Nonlinear statistics are also returned (slow - disable by setting
#' # nonlinStats = NULL if these are not needed)
#' ph = phasegram(target, 16000, nonlinStats = NULL)
#'
#' \dontrun{
#' ph = phasegram(target, 16000, windowLength = 20, step = 20,
#'   rasterize = TRUE, bw = .01, bins = 150)
#' ph$descriptives
#'
#' # Unfortunately, phasegrams are greatly affected by noise. Compare:
#' target2 = soundgen(sylLen = 300, pitch = c(350, 420, 420, 410, 340) * 3,
#'   subDep = c(0, 0, 60, 50, 0, 0)/2, noise = -10, addSilence = 0, plot = TRUE)
#' ph2 = phasegram(target2, 16000)
#'
#' s2 = soundgen(sylLen = 3000, addSilence = 0, temperature = 1e-6,
#'   pitch = c(380, 550, 500, 220), subDep = c(0, 0, 40, 0, 0, 0, 0, 0),
#'   amDep = c(0, 0, 0, 0, 80, 0, 0, 0), amFreq = 80,
#'   jitterDep = c(0, 0, 0, 0, 0, 3))
#' spectrogram(s2, 16000, yScale = 'bark')
#' phasegram(s2, 16000, windowLength = 10, nonlinStats = NULL, bw = .001)
#' phasegram(s2, 16000, windowLength = 10, nonlinStats = NULL, bw = .02)
#' }
phasegram = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    windowLength = 10,
    step = windowLength / 2,
    timeLag = NULL,
    theilerWindow = NULL,
    nonlinStats = c('ed', 'd2', 'ml', 'sur'),
    pars_ed = list(max.embedding.dim = 15),
    pars_d2 = list(min.embedding.dim = 2,
                   min.radius = 1e-3,
                   n.points.radius = 20),
    pars_ml = list(min.embedding.dim = 2,
                   radius = 0.001),
    pars_sur = list(FUN = nonlinearTseries::timeAsymmetry,
                    K = 1),
    bw = .01,
    bins = 5 / bw,
    reportEvery = NULL,
    cores = 1,
    rasterize = FALSE,
    plot = TRUE,
    savePlots = NULL,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    xlab = 'Time',
    ylab = '',
    main = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'pars_ed', 'pars_d2', 'pars_ml',
    'pars_sur', 'reportEvery', 'cores', 'savePlots')]
  myPars$pars_ed = pars_ed
  myPars$pars_d2 = pars_d2
  myPars$pars_ml = pars_ml
  myPars$pars_sur = pars_sur

  # call .phasegram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.phasegram',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "phasegram", width = paste0(width, units)))
  }

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}

#' Phasegram per sound
#'
#' Internal soundgen function called by \code{\link{phasegram}}.
#' @inheritParams phasegram
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.phasegram = function(
    audio,
    windowLength = 10,
    step = windowLength / 2,
    timeLag = NULL,
    theilerWindow = NULL,
    nonlinStats = c('ed', 'd2', 'ml', 'sur'),
    pars_ed = list(max.embedding.dim = 15),
    pars_d2 = list(min.embedding.dim = 2,
                   min.radius = 1e-3,
                   n.points.radius = 20),
    pars_ml = list(min.embedding.dim = 2,
                   radius = 0.001),
    pars_sur = list(FUN = nonlinearTseries::timeAsymmetry,
                    K = 1),
    bw = .01,
    bins = 5 / bw,
    plot = TRUE,
    rasterize = FALSE,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    xlab = 'Time',
    ylab = '',
    main = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  len = length(audio$sound)
  windowLength_points = windowLength * audio$samplingRate / 1000
  step_points = step * audio$samplingRate / 1000
  audio$sound = audio$sound / max(abs(audio$sound))
  sds = sd(audio$sound)
  if (is.na(sds) || !sds > 0) return(NA)

  # choose a suitable time shift t as the period at which the autocorrelation
  # function first crosses 0. Because sounds often start with silence or some other unrepresentative artifacts, we grab a sample in the middle
  if (is.null(timeLag)) {
    t = suppressWarnings(try(
      nonlinearTseries::timeLag(audio$sound,
                                technique = 'ami',
                                selection.method = 'first.minimum',
                                do.plot = FALSE),
      silent = TRUE))
    if (inherits(t, 'try-error') || !is.numeric(t)) {
      t = suppressWarnings(try(
        nonlinearTseries::timeLag(audio$sound,
                                  technique = 'ami',
                                  selection.method = 'first.e.decay',
                                  do.plot = FALSE),
        silent = TRUE))
      if (inherits(t, 'try-error') || !is.numeric(t)) {
        timeLag = 1
        warning('Please set timeLag manually; defaulting to 1 point')
      }
    } else {
      timeLag = round(t / audio$samplingRate * 1000, 1)
      message(paste('Setting timeLag to', timeLag, 'ms'))
    }
  } else {
    t = round(audio$samplingRate * timeLag / 1000)
  }
  if (t > (windowLength_points / 2)) {
    warning('timeLag is too large, resetting to 1 point')
    t = 1
  }
  if (is.null(pars_d2$max.radius)) pars_d2$max.radius = max(abs(audio$sound)) * 2

  # theiler window - only needed if we calculate d2 or ml
  if (is.null(theilerWindow) & ('ed' %in% nonlinStats |
                                'ml' %in% nonlinStats)) {
    theiler.window = suppressWarnings(try(
      nonlinearTseries::timeLag(audio$sound,
                                technique = 'acf',
                                selection.method = 'first.minimum',
                                do.plot = FALSE),
      silent = TRUE))
    if (inherits(theiler.window, 'try-error') || !is.numeric(theiler.window)) {
      theiler.window = suppressWarnings(try(
        nonlinearTseries::timeLag(audio$sound,
                                  technique = 'acf',
                                  selection.method = 'first.zero',
                                  do.plot = FALSE),
        silent = TRUE))
      if (inherits(theiler.window, 'try-error') || !is.numeric(theiler.window)) {
        theiler.window = t * 2
        warning('Failed to determine theiler.window automatically; defaulting to t * 2')
      }
    }
  } else if (!is.null(theilerWindow)) {
    theiler.window = round(theilerWindow / audio$samplingRate * 1000, 1)
  } else {
    theiler.window = t * 2
  }
  if (is.null(pars_d2$theiler.window)) pars_d2$theiler.window = theiler.window
  if (is.null(pars_ml$theiler.window)) pars_ml$theiler.window = theiler.window

  # for each frame
  n_frames_max = floor(len / step_points)
  frame_starts = 1 + step_points * (0:n_frames_max)
  frame_starts = frame_starts[frame_starts < (len - windowLength_points + 1)]
  n_frames = length(frame_starts)
  out_pg = out_stats = out_ns = vector('list', n_frames)
  for (f in seq_len(n_frames)) {
    ## grab a frame (without windowing)
    frame = audio$sound[frame_starts[f]:(frame_starts[f] + windowLength_points - 1)]
    # plot(frame, type = 'l')
    flat = (sd(frame) == 0)

    ## take a Poincare section through the phase portrait (~0.5 ms/frame)
    # choose the best angle - just default normal vector for now
    if (FALSE) {
      autocor = acf(frame, lag.max = length(frame) / 2, plot = TRUE)
      t = which(autocor$acf < 0)[1]
      pp = data.frame(
        orig = frame[1:(windowLength_points - t)],
        shifted = frame[(1 + t):windowLength_points]
      )
      plot(pp, type = 'l')
    }

    if (!flat) {
      pc = suppressMessages(suppressWarnings(try(
        nonlinearTseries::poincareMap(frame, time.lag = t, embedding.dim = 2),
        silent = TRUE
      )))
      # plot(pc$pm)
      # hist(pc$pm[, 1], breaks = 100)
      # h = hist(pc$pm[, 1], breaks = breaks, plot = FALSE)
      # d = data.frame(x = h$mids, y = h$counts / max(h$counts))
      # try kernel smoothing instead of a histogram
      # plot(density(pc$pm[, 1], bw = .005))  # bw = 'SJ-ste'
      if (!inherits(pc, 'try-error')) {
        pm = pc$pm[, 1]
        if (length(pm) > 1) {
          # NB: the audio is normalized, so pm is always on the same scale of [-1,
          # 1]. Thus, bw can be specified in absolute units
          d = density(pm, bw = bw)
          out_pg[[f]] = data.frame(
            time = frame_starts[f] + windowLength_points / 2,
            x = d$x, y = d$y)
        }
      } else {
        out_pg[[f]] = data.frame(
          time = frame_starts[f] + windowLength_points / 2,
          x = NA, y = NA)
      }

      # stats per frame derived from the Poincare section
      out_stats[[f]] = data.frame(time = out_pg[[f]]$time[1],
                                  shannon = NA, nPeaks = NA)

      if (exists('d')) {
        # Entropy and nPeaks in poincare sections
        out_stats[[f]]$shannon = getEntropy(d$y, type = 'shannon', normalize = TRUE)

        # normalize nPeaks - at most, every other point can be a peak; log2(x+1)
        # means it will range from 0 to 1
        out_stats[[f]]$nPeaks = log2(1 + length(which(diff(diff(d$y) > 0) == -1)) /
                                       ceiling(length(d$y) / 2))
      }


      ## Other nonlinear stats per frame
      if (!is.null(nonlinStats)) out_ns[[f]] = nonlinStats(
        frame, t = t, pars_ed = pars_ed, pars_d2 = pars_d2,
        pars_ml = pars_ml, pars_sur = pars_sur, nonlinStats = nonlinStats)
    }
    # end of for-loop processing each frame
  }

  # rbind the phasegrams and stats into dataframes
  pg = do.call('rbind', out_pg)
  # head(pg)
  pg$time = (pg$time / audio$samplingRate + audio$timeShift) * 1000
  pg$y = pg$y / max(pg$y, na.rm = TRUE)
  # plot(pg$time, pg$x, col = rgb(0, 0, 0, pg$y), pch = 16, cex = .5)

  descriptives = do.call('rbind', out_stats)
  descriptives$time = (descriptives$time / audio$samplingRate + audio$timeShift) * 1000
  # head(descriptives)
  if (!is.null(nonlinStats)) {
    ns = as.data.frame(do.call('rbind', out_ns))
    for (c in 1:ncol(ns)) ns[, c] = as.numeric(ns[, c])
    # (some strange formatting issue, hence this workaround)
    descriptives = cbind(descriptives, ns)
  }

  # plotting
  Z = NULL
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_phasegram.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    # prepare for plotting
    if (!is.null(col)) colorTheme = NULL
    if (!is.null(colorTheme)) {
      color.palette = switchColorTheme(colorTheme)
    } else {
      color.palette = NULL
    }
    if (is.null(xlab)) xlab = ''
    if (is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }

    pg_plot = na.omit(pg)
    if (rasterize) {
      # An irregular time-frequency grid is hard to plot, so we rasterize it
      ly = length(unique(pg_plot$time))
      pg_plot$ix = findInterval(pg_plot$x, seq(min(pg_plot$x), max(pg_plot$x),
                                               length.out = bins))
      pg_plot$itime = findInterval(pg_plot$time, sort(unique(pg_plot$time)))
      Z = matrix(min(pg_plot$y), nrow = ly, ncol = bins)
      for (i in 1:nrow(pg_plot))
        Z[pg_plot$itime[i], pg_plot$ix[i]] =
        Z[pg_plot$itime[i], pg_plot$ix[i]] + pg_plot$y[i]
      for (i in 1:nrow(Z)) Z[i, ] = Z[i, ] / max(Z[i, ])
      # xn = as.numeric(unique(pg$ix))
      # xn = xn - median(xn)
      # xn = xn / max(abs(xn))
      # colnames(Z) = xn
      rownames(Z) = sort(unique(pg_plot$time))

      try(do.call('filled.contour.mod', list(
        x = as.numeric(rownames(Z)), z = Z,
        yaxt = 'n',
        color.palette = color.palette,
        col = col,
        main = main,
        xlab = xlab, ylab = ylab,
        ...)))
    } else {
      # if (is.null(col)) col = color.palette(30)
      # pl = pg[ph$orig$y > .05, ]
      try(plot(pg_plot$time, pg_plot$x,
               col = rgb(0, 0, 0, pg_plot$y),
               # col = col[as.numeric(cut(pg_plot$y, breaks = length(col) - 1))],
               pch = 16, cex = .5,
               main = main,
               xlab = xlab, ylab = ylab,
               ...))
    }

  }
  if (is.character(audio$savePlots)) {
    dev.off()
  }

  invisible(list(orig = pg, rasterized = Z, descriptives = descriptives))
}


#' Nonlinear statistics
#'
#' Estimates the optimal number of embedding dimensions (ed), correlation
#' dimension D2 (d2), maximum Lyapunov exponent (ml), and the results of
#' surrogate data testing for stochasticity (sur) using the functionality of the
#' package nonlinearTseries. This is basically just a wrapper that puts all
#' these functions together - convenient for frame-by-frame analysis, eg by
#' \code{\link{phasegram}}.
#'
#' @param x numeric vector such as a sound or analysis frame
#' @param t time lag in points. Defaults to the number of steps beyond which the
#'   mutual information function reaches its minimum - see
#'   \code{\link[nonlinearTseries]{timeLag}}
#' @param pars_ed a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{estimateEmbeddingDim}}
#' @param pars_d2 a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{corrDim}}
#' @param pars_ml a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{maxLyapunov}}
#' @param pars_sur a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{surrogateTest}}
#' @keywords internal
#'
#' @examples
#' x = sin((1:200) / 5) + rnorm(200, 0, .5)
#' plot(x, type = 'l')
#' soundgen:::nonlinStats(x)
nonlinStats = function(
    x,
    t = NULL,
    pars_ed = list(time.lag = t,
                   max.embedding.dim = 15),
    pars_d2 = list(time.lag = t,
                   min.embedding.dim = 2,
                   min.radius = 1e-3,
                   max.radius = max(abs(x)) * 2,
                   n.points.radius = 20,
                   theiler.window = t * 2),
    pars_ml = list(time.lag = t,
                   min.embedding.dim = 2,
                   radius = 0.001,
                   theiler.window = t * 2),
    pars_sur = list(FUN = nonlinearTseries::timeAsymmetry,
                    K = 1),
    nonlinStats = c('ed', 'd2', 'ml', 'sur')
) {
  out = list(t = t, ed = NA, d2 = NA, ml = NA, sur = NA)
  sds = sd(x)
  if (is.na(sds) || !sds > 0) return(out)

  # time lag
  if (is.null(t)) {
    t = suppressWarnings(try(
      nonlinearTseries::timeLag(x,
                                technique = 'ami',
                                selection.method = 'first.minimum',
                                do.plot = FALSE),
      silent = TRUE))
    if (inherits(t, 'try-error') || !is.numeric(t)) {
      t = suppressWarnings(try(
        nonlinearTseries::timeLag(x,
                                  technique = 'ami',
                                  selection.method = 'first.e.decay',
                                  do.plot = FALSE),
        silent = TRUE))
      if (inherits(t, 'try-error') || !is.numeric(t)) {
        t = 1
        warning('Failed to determine t automatically; defaulting to 1 point')
      }
    }
  }
  out$t = t

  # the optimal number of embedding dimensions (~140 ms/frame)
  if ('ed' %in% nonlinStats) {
    ed = suppressWarnings(try(
      do.call(nonlinearTseries::estimateEmbeddingDim, c(list(
        time.series = x, do.plot = FALSE),
        pars_ed)),
      silent = TRUE))
    if (!inherits(ed, 'try-error') && is.finite(ed)) out$ed = ed
    if (is.null(ed) || !is.finite(ed)) ed = 2
  } else {
    ed = 2
  }

  # correlation dimension (~5.7 ms/frame)
  if ('d2' %in% nonlinStats) {
    if (is.null(pars_d2$max.embedding.dim))
      pars_d2$max.embedding.dim = min(15, ed * 2)
    cd = suppressWarnings(try(
      do.call(nonlinearTseries::corrDim, c(list(
        time.series = x, do.plot = FALSE),
        pars_d2)),
      silent = TRUE))
    if (!inherits(cd, 'try-error')) {
      d2_temp = suppressWarnings(try(
        nonlinearTseries::estimate(cd),
        silent = TRUE))
      if (!inherits(d2_temp, 'try-error'))
        out$d2 = d2_temp
    }
  }

  # maximum Lyapunov exponent (~10 ms/frame)
  if ('ml' %in% nonlinStats) {
    if (is.null(pars_ml$max.embedding.dim))
      pars_ml$max.embedding.dim = pars_d2$max.embedding.dim
    ml = suppressWarnings(try(
      do.call(nonlinearTseries::maxLyapunov, c(list(
        time.series = x, do.plot = FALSE), pars_ml)),
      silent = TRUE))
    # plot(ml)
    ml_est = suppressWarnings(try(
      nonlinearTseries::estimate(ml, do.plot = FALSE), # might need better defaults
      silent = TRUE))
    if (!inherits(ml_est, 'try-error'))
      out$ml = ml_est
  }

  # surrogate testing for stochasticity
  if ('sur' %in% nonlinStats) {
    st = suppressWarnings(try(
      do.call(nonlinearTseries::surrogateTest, c(list(
        time.series = x, verbose = FALSE, do.plot = FALSE), pars_sur)),
      silent = TRUE))
    if (!inherits(st, 'try-error'))
      out$sur = 1 - max(c(
        mean(st$data.statistic > st$surrogates.statistics),
        mean(st$data.statistic < st$surrogates.statistics)))
    # 0 = deterministic (scrambling ruins time symmetry)
    # 1 = stochastic (scrambling the phase has little effect on time symmetry)
  }
  out
}
