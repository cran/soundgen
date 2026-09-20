#' Phasegram
#'
#' Produces a phasegram of a sound or another time series, which is a collection
#' of Poincare sections cut through phase portraits of consecutive frames. The x
#' axis is time, just as in a spectrogram, the y axis is a slice through the
#' phase portrait, and the color shows the density of trajectories at each point
#' of the phase portrait.
#'
#' Algorithm: the input sound is normalized to \code{[-1, 1]} and divided into
#' consecutive frames \code{windowLength} ms long without multiplying by any
#' windowing function (unlike in STFT). For each frame, a phase portrait is
#' obtained by time-shifting the frame by \code{timeLag} ms. A Poincare section
#' is taken through the phase portrait (currently at a fixed angle, namely the
#' default in \code{\link[nonlinearTseries]{poincareMap}}), giving the
#' intersection points of trajectories with this bisecting line. The density of
#' intersections is estimated with a smoothing kernel of bandwidth \code{bw}
#' (as an alternative to using histogram bins). The density distributions per
#' frame are stacked together into a phasegram (output: \code{orig}). The
#' density values in \code{orig} are normalized by the global maximum across all
#' frames. The resulting phasegram can optionally be rasterized to smooth it for
#' plotting (output: \code{rasterized}); the rasterized matrix is additionally
#' normalized row by row for display.
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams spectrogram
#' @param timeLag time lag between the original and time-shifted version of each
#'   frame that together represent the phase portrait (ms). Defaults to the
#'   number of steps beyond which the mutual information function reaches its
#'   minimum or, if that fails, the steps until mutual information experiences
#'   the first exponential decay - see \code{\link[nonlinearTseries]{timeLag}}.
#'   If automatic estimation fails, defaults to 1 sample
#' @param theilerWindow time lag between two points that are considered locally
#'   independent and can be treated as neighbors in the reconstructed phase
#'   space (ms). Converted internally to samples. Defaults to the first minimum
#'   or, if unavailable, the first zero of the autocorrelation function (or,
#'   failing that, to \code{timeLag * 2})
#' @param nonlinStats nonlinear statistics to report: "ed" = the optimal number
#'   of embedding dimensions, "d2" = correlation dimension D2, "ml" = maximum
#'   Lyapunov exponent, "sur" = the results of surrogate data testing for
#'   stochasticity. These are calculated using the functionality of the package
#'   nonlinearTseries, which can be slow. Set to \code{NULL} or
#'   \code{character(0)} to calculate only the phasegram and basic descriptives.
#'   The default is to compute all available nonlinear statistics
#' @param ed_pars a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{estimateEmbeddingDim}}. If
#'   \code{ed_pars$time.lag} is \code{NULL}, it is set to the estimated time
#'   lag in samples
#' @param d2_pars a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{corrDim}}. If \code{d2_pars$time.lag},
#'   \code{d2_pars$max.radius}, or \code{d2_pars$theiler.window} are
#'   \code{NULL}, they are filled in automatically
#' @param ml_pars a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{maxLyapunov}}. If
#'   \code{ml_pars$time.lag} or \code{ml_pars$theiler.window} are \code{NULL},
#'   they are filled in automatically
#' @param sur_pars a list of control parameters passed to
#'   \code{\link[nonlinearTseries]{surrogateTest}}
#' @param bw standard deviation of the smoothing kernel, as in
#'   \code{\link[stats]{density}}. Must be a single positive finite number
#' @param bins the number of bins along the Y axis after rasterizing (has no
#'   effect if \code{rasterize = FALSE}). Coerced to an integer of at least 2
#' @param rasterize if FALSE, only plots and returns Poincare sections on the
#'   original scale (most graphical parameters will then have no effect); if
#'   TRUE, rasterizes the phasegram matrix and plots it with more graphical
#'   parameters. The rasterized matrix is returned even if \code{plot = FALSE}
#' @param xlab,ylab,main graphical parameters passed to
#'   soundgen:::filled.contour.mod (if \code{rasterize = TRUE}) or plot (if
#'   \code{rasterize = FALSE})
#' @param ... other graphical parameters passed to soundgen:::filled.contour.mod
#'   (if \code{rasterize = TRUE}) or plot (if \code{rasterize = FALSE})
#'
#' @references \itemize{
#'   \item Herbst, C. T., Herzel, H., Švec, J. G., Wyman, M. T., & Fitch, W.
#'   T. (2013). Visualization of system dynamics using phasegrams. Journal of
#'   the Royal Society Interface, 10(85), 20130288.
#'   \item Huffaker, R., Huffaker, R. G., Bittelli, M., & Rosa, R. (2017).
#'   Nonlinear time series analysis with R. Oxford University Press.
#' }
#'
#' @return For a single input, a list of three components:
#'   \describe{
#'     \item{orig}{the full phasegram as a data frame. \code{$time} is the
#'       middle of each frame (ms), \code{$x} is the coordinate along the
#'       Poincare section (approximately on the normalized audio scale), and
#'       \code{$y} is the density of intersections of system trajectories with
#'       the Poincare section, normalized by the global maximum across all
#'       frames. Failed or flat frames are represented by NA rows}
#'     \item{rasterized}{the rasterized phasegram as a numeric matrix, or
#'       \code{NULL} if \code{rasterize = FALSE}. Rows correspond to time
#'       frames and columns correspond to bins along the Poincare-section
#'       coordinate. Values are row-normalized for display, so each non-empty
#'       row has a maximum of 1. If no valid Poincare intersections are found,
#'       a zero-valued matrix is returned}
#'     \item{descriptives}{per-frame descriptives as a data frame. Always
#'       included are \code{time} (ms), \code{shannon} = normalized Shannon
#'       entropy of Poincare sections, and \code{nPeaks} = log-normalized
#'       number of peaks in the density distribution of Poincare sections. If
#'       requested via \code{nonlinStats}, also includes \code{ed} = optimal
#'       number of embedding dimensions, \code{d2} = correlation dimension,
#'       \code{ml} = maximum Lyapunov exponent (positive values suggest chaos),
#'       and \code{sur} = stochasticity index from surrogate data testing,
#'       rescaled to approximately 0 = deterministic and 1 = stochastic}
#'   }
#'
#'   For multiple inputs, a list of such per-input results is returned.
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
#'   subDep = c(0, 0, 60, 50, 0, 0), noise = -30, jitterDep = .4,
#'   rolloff = -5, addSilence = 0, plot = TRUE)
#' ph2 = phasegram(target2, 16000, nonlinStats = NULL)
#'
#' # low-pass filtering may help a bit
#' target2_lowpass = bandpass(target2, 16000, upr = 2500)
#' phasegram(target2_lowpass, 16000, nonlinStats = NULL)
#'
#' s2 = soundgen(sylLen = 3000, addSilence = 0, temperature = 1e-6,
#'   pitch = c(380, 550, 500, 220), subDep = c(0, 0, 40, 0, 0, 0, 0, 0),
#'   amDep = c(0, 0, 0, 0, 80, 0, 0, 0), amFreq = 80,
#'   jitterDep = c(0, 0, 0, 0, 0, 3), plot = TRUE, yScale = 'bark')
#' phasegram(s2, 16000, windowLength = 10, nonlinStats = NULL, bw = .001)
#' phasegram(s2, 16000, windowLength = 10, nonlinStats = NULL, bw = .02)
#' }
phasegram = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    windowLength = 10,
    step = NULL,
    overlap = 50,
    timeLag = NULL,
    theilerWindow = NULL,
    nonlinStats = c('ed', 'd2', 'ml', 'sur'),
    ed_pars = list(max.embedding.dim = 15),
    d2_pars = list(min.embedding.dim = 2,
                   min.radius = 1e-3,
                   n.points.radius = 20),
    ml_pars = list(min.embedding.dim = 2,
                   radius = 0.001),
    sur_pars = list(FUN = nonlinearTseries::timeAsymmetry,
                    K = 20),
    bw = .01,
    bins = 5 / bw,
    reportEvery = NULL,
    cores = 1,
    rasterize = FALSE,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    colorTheme = 'bw',
    col = NULL,
    xlab = 'Time',
    ylab = '',
    main = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  # check if package "nonlinearTseries" is available
  if (!requireNamespace("nonlinearTseries", quietly = TRUE))
    stop('Please install nonlinearTseries to run this function: ',
         '`install.packages("nonlinearTseries")`')

  # allow nonlinStats = NULL to disable nonlinear stats
  if (is.null(nonlinStats) || length(nonlinStats) == 0 || all(is.na(nonlinStats))) {
    nonlinStats = NULL
  } else {
    nonlinStats = match.arg(nonlinStats, c('ed', 'd2', 'ml', 'sur'), several.ok = TRUE)
    nonlinStats = unique(nonlinStats)
  }

  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'reportEvery',
    'cores', 'savePlots', 'embed')]
  if (missing(bins)) myPars$bins = NULL  # in case the default 5/bw is invalid

  # call .phasegram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.phasegram',
    suffix = 'phasegram',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Phasegram per sound
#' @noRd
.phasegram = function(
    audio,
    windowLength = 10,
    step = NULL,
    overlap = 50,
    timeLag = NULL,
    theilerWindow = NULL,
    nonlinStats = c('ed', 'd2', 'ml', 'sur'),
    ed_pars = list(max.embedding.dim = 15),
    d2_pars = list(min.embedding.dim = 2,
                   min.radius = 1e-3,
                   n.points.radius = 20),
    ml_pars = list(min.embedding.dim = 2,
                   radius = 0.001),
    sur_pars = list(FUN = nonlinearTseries::timeAsymmetry,
                    K = 20),
    bw = .01,
    bins = 5 / bw,
    plot = TRUE,
    rasterize = FALSE,
    colorTheme = 'bw',
    col = NULL,
    xlab = 'Time',
    ylab = '',
    main = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  # basic input validation
  if (is.null(audio$sound) || !is.numeric(audio$sound)) {
    stop('No numeric sound found in audio')
  }
  if (any(!is.finite(audio$sound))) {
    stop('The input contains non-finite values')
  }

  len = length(audio$sound)
  if (len < 4) stop('The sound is too short to analyze')

  # bandwidth should be a positive finite number
  if (length(bw) != 1 || !is.numeric(bw) || !is.finite(bw) || bw <= 0) {
    warning('"bw" must be a single positive finite number; resetting to 0.01')
    bw = .01
  }

  # avoid forcing the default bins promise if bw was invalid
  if (missing(bins)) {
    bins = max(1, 5 / bw)
  }
  if (length(bins) != 1 || !is.numeric(bins) || !is.finite(bins) || bins < 1) {
    warning('"bins" must be a single positive finite number; resetting to 500')
    bins = 500
  }
  bins_int = suppressWarnings(as.integer(round(bins)))
  if (!is.finite(bins_int) || bins_int < 2) {
    warning('"bins" is too small or too large; resetting to 500')
    bins_int = 500L
  }
  bins = bins_int

  val = validateWlOvlp(audio, windowLength, step, overlap)
  wl = val$wl
  step = val$step
  step_points = val$step_points

  mas = max(abs(audio$sound))
  if (is.finite(mas) && mas > 0) {
    audio$sound = audio$sound / mas
  } else {
    stop('Nothing to do: the input is silent')
  }

  # helper for checking estimated / supplied lags
  lag_is_valid = function(lag) {
    !inherits(lag, 'try-error') && is.numeric(lag) && length(lag) == 1 &&
      is.finite(lag) && lag >= 1
  }

  # choose a suitable time shift t as the period at which the autocorrelation
  # function first crosses 0. Because sounds often start with silence or some other
  # unrepresentative artifacts, we grab a sample in the middle
  if (is.null(timeLag)) {
    t = suppressWarnings(try(
      nonlinearTseries::timeLag(audio$sound,
                                technique = 'ami',
                                selection.method = 'first.minimum',
                                do.plot = FALSE),
      silent = TRUE))
    if (lag_is_valid(t)) {
      timeLag = round(t / audio$samplingRate * 1000, 1)
      message(paste('Setting timeLag to', timeLag, 'ms'))
    } else {
      t = suppressWarnings(try(
        nonlinearTseries::timeLag(audio$sound,
                                  technique = 'ami',
                                  selection.method = 'first.e.decay',
                                  do.plot = FALSE),
        silent = TRUE))
      if (lag_is_valid(t)) {
        timeLag = round(t / audio$samplingRate * 1000, 1)
        message(paste('Setting timeLag to', timeLag, 'ms'))
      } else {
        t = 1
        timeLag = round(1000 / audio$samplingRate, 1)
        warning('Please set timeLag manually; defaulting to 1 point')
      }
    }
  } else {
    if (length(timeLag) != 1 || !is.numeric(timeLag) ||
        !is.finite(timeLag) || timeLag <= 0) {
      warning('"timeLag" must be a single positive finite number of ms; resetting to 1 point')
      t = 1
      timeLag = round(1000 / audio$samplingRate, 1)
    } else {
      t = round(audio$samplingRate * timeLag / 1000)
      if (!is.finite(t) || t < 1) {
        warning('timeLag is too small; resetting to 1 point')
        t = 1
        timeLag = round(1000 / audio$samplingRate, 1)
      }
    }
  }

  t = max(1L, as.integer(round(t)))
  if (t > (wl / 2)) {
    warning('timeLag is too large, resetting to 1 point')
    t = 1L
    timeLag = round(1000 / audio$samplingRate, 1)
  }

  # parameter lists should be lists
  if (is.null(ed_pars)) ed_pars = list()
  if (is.null(d2_pars)) d2_pars = list()
  if (is.null(ml_pars)) ml_pars = list()
  if (is.null(sur_pars)) sur_pars = list()
  ed_pars = as.list(ed_pars)
  d2_pars = as.list(d2_pars)
  ml_pars = as.list(ml_pars)
  sur_pars = as.list(sur_pars)
  if (is.null(d2_pars$max.radius)) d2_pars$max.radius = max(abs(audio$sound)) * 2

  # theiler window - only needed if we calculate ed, d2, or ml
  # user-facing theilerWindow is in ms; nonlinearTseries expects samples
  if (!is.null(nonlinStats)) {
    if (is.null(theilerWindow) && any(c('ed', 'd2', 'ml') %in% nonlinStats)) {
      theiler.window = suppressWarnings(try(
        nonlinearTseries::timeLag(audio$sound,
                                  technique = 'acf',
                                  selection.method = 'first.minimum',
                                  do.plot = FALSE),
        silent = TRUE))
      if (!lag_is_valid(theiler.window)) {
        theiler.window = suppressWarnings(try(
          nonlinearTseries::timeLag(audio$sound,
                                    technique = 'acf',
                                    selection.method = 'first.zero',
                                    do.plot = FALSE),
          silent = TRUE))
      }
      if (!lag_is_valid(theiler.window)) {
        theiler.window = t * 2
        warning('Failed to determine theiler.window automatically; defaulting to t * 2')
      }
    } else if (!is.null(theilerWindow)) {
      if (length(theilerWindow) == 1 && is.numeric(theilerWindow) &&
          is.finite(theilerWindow) && theilerWindow > 0) {
        theiler.window = round(audio$samplingRate * theilerWindow / 1000)
      } else {
        warning('"theilerWindow" must be a single positive finite number of ms; defaulting to timeLag * 2')
        theiler.window = t * 2
      }
    } else {
      theiler.window = t * 2
    }

    theiler.window = max(1L, as.integer(round(theiler.window)))
    if (theiler.window >= wl) theiler.window = max(1L, wl - 1L)

    if (is.null(d2_pars$theiler.window)) d2_pars$theiler.window = theiler.window
    if (is.null(ml_pars$theiler.window)) ml_pars$theiler.window = theiler.window
  }

  # for each frame
  last_start = len - wl + 1
  if (last_start < 1) {
    stop('windowLength is longer than the selected sound segment')
  }
  frame_starts = seq(1, last_start, by = step_points)
  n_frames = length(frame_starts)
  if (n_frames == 0) {
    stop('No frames to analyze')
  }

  frame_times = frame_starts + wl / 2
  time_shift = if (is.null(audio$timeShift)) 0 else audio$timeShift
  frame_times_ms = (frame_times / audio$samplingRate + time_shift) * 1000

  out_pg = out_stats = out_ns = vector('list', n_frames)

  for (f in seq_len(n_frames)) {
    d = NULL
    frame_time = frame_times[f]
    frame = audio$sound[frame_starts[f]:(frame_starts[f] + wl - 1)]

    # default NA outputs preserve the frame even if processing fails
    out_pg[[f]] = data.frame(
      time = frame_time,
      x = NA_real_,
      y = NA_real_
    )
    out_stats[[f]] = data.frame(
      time = frame_time,
      shannon = NA_real_,
      nPeaks = NA_real_
    )
    if (!is.null(nonlinStats)) {
      out_ns[[f]] = list(
        ed = NA_real_,
        d2 = NA_real_,
        ml = NA_real_,
        sur = NA_real_
      )
    }

    frame_sd = suppressWarnings(sd(frame))
    flat = length(frame) < 4 || !is.finite(frame_sd) || frame_sd <= .Machine$double.eps
    if (flat) next

    ## take a Poincare section through the phase portrait
    pc = suppressMessages(suppressWarnings(try(
      nonlinearTseries::poincareMap(frame, time.lag = t, embedding.dim = 2),
      silent = TRUE
    )))
    if (inherits(pc, 'try-error') || is.null(pc$pm)) next

    pm = pc$pm
    if (is.matrix(pm) || is.data.frame(pm)) {
      pm = pm[, 1]
    }
    pm = pm[is.finite(pm)]
    if (length(pm) < 2) next

    # kernel smoothing instead of a histogram
    d = suppressWarnings(try(
      density(pm, bw = bw),
      silent = TRUE
    ))
    if (inherits(d, 'try-error') || is.null(d$x) || is.null(d$y) ||
        length(d$x) < 2 || any(!is.finite(d$x)) || any(!is.finite(d$y))) {
      next
    }

    # NB: the audio is normalized, so pm is approximately on the same scale
    out_pg[[f]] = data.frame(
      time = frame_time,
      x = d$x,
      y = d$y
    )

    # stats per frame derived from the Poincare section
    entropy_temp = suppressWarnings(try(
      getEntropy(d$y, type = 'shannon', normalize = TRUE),
      silent = TRUE
    ))
    if (!inherits(entropy_temp, 'try-error') &&
        length(entropy_temp) == 1 && is.finite(entropy_temp)) {
      out_stats[[f]]$shannon = entropy_temp
    }

    # normalize nPeaks - at most, every other point can be a peak; log2(x+1)
    # means it will range from 0 to 1
    if (length(d$y) >= 3) {
      n_peaks = sum(diff(diff(d$y) > 0) == -1, na.rm = TRUE)
    } else {
      n_peaks = 0
    }
    out_stats[[f]]$nPeaks = log2(1 + n_peaks / ceiling(length(d$y) / 2))

    ## Other nonlinear stats per frame
    if (!is.null(nonlinStats)) {
      ns_temp = suppressWarnings(try(
        .nonlinStats(
          frame,
          t = t,
          ed_pars = ed_pars,
          d2_pars = d2_pars,
          ml_pars = ml_pars,
          sur_pars = sur_pars,
          nonlinStats = nonlinStats
        ),
        silent = TRUE
      ))
      if (!inherits(ns_temp, 'try-error') && is.list(ns_temp)) {
        ns_temp$t = NULL
        out_ns[[f]] = ns_temp
      }
    }
    # end of for-loop processing each frame
  }

  # rbind the phasegrams and stats into dataframes
  pg = do.call('rbind', out_pg)
  if (is.null(pg) || nrow(pg) == 0) {
    pg = data.frame(time = numeric(0), x = numeric(0), y = numeric(0))
  } else {
    pg$time = (pg$time / audio$samplingRate + time_shift) * 1000
    max_y = max(pg$y, na.rm = TRUE)
    if (is.finite(max_y) && max_y > 0) {
      pg$y = pg$y / max_y
    }
  }

  descriptives = do.call('rbind', out_stats)
  if (is.null(descriptives) || nrow(descriptives) == 0) {
    descriptives = data.frame(
      time = numeric(0),
      shannon = numeric(0),
      nPeaks = numeric(0)
    )
  } else {
    descriptives$time = (descriptives$time / audio$samplingRate + time_shift) * 1000
  }

  if (!is.null(nonlinStats)) {
    ns = do.call('rbind', out_ns)
    if (is.null(ns) || nrow(ns) == 0) {
      ns = data.frame(
        ed = numeric(0),
        d2 = numeric(0),
        ml = numeric(0),
        sur = numeric(0)
      )
    } else {
      ns = as.data.frame(ns, stringsAsFactors = FALSE)
      if (ncol(ns) > 0) {
        for (i in seq_len(ncol(ns))) {
          ns[, i] = suppressWarnings(as.numeric(as.character(ns[, i])))
        }
      }
      if (nrow(ns) == nrow(descriptives)) {
        descriptives = cbind(descriptives, ns)
      }
    }
  }

  # rasterization
  # rasterization is computed even if plot = FALSE
  Z = NULL
  x_grid = seq(0, 1, length.out = bins)
  if (rasterize) {
    time_grid = frame_times_ms
    ly = length(time_grid)
    Z = matrix(0, nrow = ly, ncol = bins)

    pg_plot = pg[stats::complete.cases(pg), , drop = FALSE]
    if (nrow(pg_plot) > 0) {
      min_x = min(pg_plot$x, na.rm = TRUE)
      max_x = max(pg_plot$x, na.rm = TRUE)

      if (is.finite(min_x) && is.finite(max_x)) {
        if (max_x <= min_x) {
          pad = max(bw, 0.01, abs(min_x) * 0.01)
          min_x = min_x - pad
          max_x = max_x + pad
        }

        # bin boundaries and bin centers along the Poincare coordinate
        x_bins = seq(min_x, max_x, length.out = bins + 1)
        x_grid = (x_bins[-1] + x_bins[-length(x_bins)]) / 2

        # bin boundaries along time
        if (ly > 1) {
          time_bins = seq(min(time_grid), max(time_grid), length.out = ly + 1)
        } else {
          half_step = max(0.5, step / 2)
          time_bins = c(time_grid - half_step, time_grid + half_step)
        }

        df = pg_plot
        # make ix & itime factors; otherwise, xtabs drops empty bins
        df$ix = factor(
          findInterval(df$x, x_bins, all.inside = TRUE),
          levels = seq_len(bins)
        )
        df$itime = factor(
          findInterval(df$time, time_bins, all.inside = TRUE),
          levels = seq_len(ly)
        )

        # time (rows) x Poincare coordinate (columns)
        Z = xtabs(y ~ itime + ix, data = df)
        Z = as.matrix(Z)
        rownames(Z) = time_grid
        colnames(Z) = x_grid

        # row-normalize for display
        row_max = apply(Z, 1, max, na.rm = TRUE)
        for (i in seq_len(nrow(Z))) {
          if (is.finite(row_max[i]) && row_max[i] > 0) {
            Z[i, ] = Z[i, ] / row_max[i]
          } else {
            Z[i, ] = 0
          }
        }
      }
    } else {
      rownames(Z) = time_grid
      colnames(Z) = x_grid
    }
  }

  # plotting
  filename_noExt = if (is.null(audio$filename_noExt)) 'sound' else audio$filename_noExt
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }

  if (plot) {
    # prepare for plotting
    if (!is.null(col)) colorTheme = NULL
    if (!is.null(colorTheme)) {
      color.palette = switchColorTheme(colorTheme)
    } else {
      color.palette = NULL
    }
    if (is.null(color.palette)) {
      color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE)
    }

    if (is.null(xlab)) xlab = ''
    if (is.null(main)) {
      if (filename_noExt == 'sound') {
        main = ''
      } else {
        main = filename_noExt
      }
    }

    pg_plot = pg[stats::complete.cases(pg), , drop = FALSE]

    if (rasterize && !is.null(Z)) {
      if (is.null(col)) col = color.palette(30)

      dot_pars = list(...)
      explicit = c('x', 'y', 'z', 'yaxt', 'yScale', 'y_Hz',
                   'color.palette', 'col', 'main', 'xlab', 'ylab')
      for (nm in explicit) dot_pars[[nm]] = NULL

      try(do.call('filled.contour.mod', c(list(
        x = as.numeric(rownames(Z)),
        y = x_grid,
        z = Z,
        yaxt = 'n',
        yScale = 'orig',
        y_Hz = FALSE,
        color.palette = color.palette,
        col = col,
        main = main,
        xlab = xlab,
        ylab = ylab),
        dot_pars)))
    } else {
      dot_pars = list(...)
      explicit = c('x', 'y', 'col', 'pch', 'cex', 'main', 'xlab', 'ylab')
      for (nm in explicit) dot_pars[[nm]] = NULL

      if (nrow(pg_plot) > 0) {
        try(do.call('plot', c(list(
          x = pg_plot$time,
          y = pg_plot$x,
          col = rgb(0, 0, 0, pg_plot$y),
          pch = 16,
          cex = .5,
          main = main,
          xlab = xlab,
          ylab = ylab),
          dot_pars)))
      } else {
        xlim = range(descriptives$time, na.rm = TRUE)
        if (!is.finite(xlim[1]) || !is.finite(xlim[2])) xlim = c(0, 1)
        try(do.call('plot', c(list(
          x = 0,
          y = 0,
          type = 'n',
          xlim = xlim,
          ylim = c(-1, 1),
          main = main,
          xlab = xlab,
          ylab = ylab),
          dot_pars)))
      }
    }
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
#' @param nonlinStats nonlinear statistics to report. If NULL or character(0),
#'   no nonlinear statistics are calculated
#' @noRd
.nonlinStats = function(
    x,
    t = NULL,
    ed_pars = list(max.embedding.dim = 15),
    d2_pars = list(min.embedding.dim = 2,
                   min.radius = 1e-3,
                   n.points.radius = 20),
    ml_pars = list(min.embedding.dim = 2,
                   radius = 0.001),
    sur_pars = list(FUN = nonlinearTseries::timeAsymmetry,
                    K = 20),
    nonlinStats = c('ed', 'd2', 'ml', 'sur')
) {
  out = list(t = t, ed = NA_real_, d2 = NA_real_, ml = NA_real_, sur = NA_real_)

  if (is.null(nonlinStats) || length(nonlinStats) == 0) return(out)
  nonlinStats = match.arg(nonlinStats, c('ed', 'd2', 'ml', 'sur'), several.ok = TRUE)
  nonlinStats = unique(nonlinStats)

  # check if package "nonlinearTseries" is available
  if (!requireNamespace("nonlinearTseries", quietly = TRUE))
    stop('Please install nonlinearTseries to run this function: ',
         '`install.packages("nonlinearTseries")`')

  if (missing(x) || !is.numeric(x) || length(x) < 4 || any(!is.finite(x))) return(out)
  sds = sd(x)
  if (!is.finite(sds) || sds <= .Machine$double.eps) return(out)

  lag_is_valid = function(lag) {
    !inherits(lag, 'try-error') && is.numeric(lag) && length(lag) == 1 &&
      is.finite(lag) && lag >= 1
  }

  # time lag
  if (is.null(t)) {
    t = suppressWarnings(try(
      nonlinearTseries::timeLag(x,
                                technique = 'ami',
                                selection.method = 'first.minimum',
                                do.plot = FALSE),
      silent = TRUE))
    if (!lag_is_valid(t)) {
      t = suppressWarnings(try(
        nonlinearTseries::timeLag(x,
                                  technique = 'ami',
                                  selection.method = 'first.e.decay',
                                  do.plot = FALSE),
        silent = TRUE))
    }
    if (!lag_is_valid(t)) {
      t = 1
      warning('Failed to determine t automatically; defaulting to 1 point')
    }
  } else {
    if (length(t) != 1 || !is.numeric(t) || !is.finite(t) || t < 1) {
      warning('"t" must be a single positive finite number of samples; resetting to 1 point')
      t = 1
    }
  }
  t = max(1L, as.integer(round(t)))
  if (t >= length(x)) t = max(1L, length(x) - 1L)
  out$t = t

  # fill in defaults that depend on t
  if (is.null(ed_pars)) ed_pars = list()
  if (is.null(d2_pars)) d2_pars = list()
  if (is.null(ml_pars)) ml_pars = list()
  ed_pars = as.list(ed_pars)
  d2_pars = as.list(d2_pars)
  ml_pars = as.list(ml_pars)

  if (is.null(ed_pars$time.lag)) ed_pars$time.lag = t
  if (is.null(d2_pars$time.lag)) d2_pars$time.lag = t
  if (is.null(ml_pars$time.lag)) ml_pars$time.lag = t
  if (is.null(d2_pars$theiler.window)) d2_pars$theiler.window = t * 2
  if (is.null(ml_pars$theiler.window)) ml_pars$theiler.window = t * 2
  if (is.null(d2_pars$max.radius)) d2_pars$max.radius = max(abs(x)) * 2

  # ensure positive integer-like lags / windows
  positive_int = function(value, default_value) {
    if (length(value) == 1 && is.numeric(value) && is.finite(value) && value >= 1) {
      return(max(1L, as.integer(round(value))))
    }
    default_value
  }

  ed_pars$time.lag = positive_int(ed_pars$time.lag, t)
  d2_pars$time.lag = positive_int(d2_pars$time.lag, t)
  ml_pars$time.lag = positive_int(ml_pars$time.lag, t)
  d2_pars$theiler.window = positive_int(d2_pars$theiler.window, t * 2)
  ml_pars$theiler.window = positive_int(ml_pars$theiler.window, t * 2)

  max_theiler_window = max(1L, length(x) - 1L)
  d2_pars$theiler.window = min(d2_pars$theiler.window, max_theiler_window)
  ml_pars$theiler.window = min(ml_pars$theiler.window, max_theiler_window)

  # the highest embedding dim that can be supported by the length of x and t
  max_possible_dim = max(2, floor((length(x) - 1) / t) + 1)

  if (is.null(ed_pars$max.embedding.dim)) ed_pars$max.embedding.dim = 15
  ed_pars$max.embedding.dim = positive_int(ed_pars$max.embedding.dim, 15)
  ed_pars$max.embedding.dim = max(2, min(ed_pars$max.embedding.dim, max_possible_dim))

  # the optimal number of embedding dimensions (~140 ms/frame)
  ed = 2
  if ('ed' %in% nonlinStats) {
    ed_temp = suppressWarnings(try(
      do.call(nonlinearTseries::estimateEmbeddingDim, c(list(
        time.series = x, do.plot = FALSE),
        ed_pars)),
      silent = TRUE))
    if (!inherits(ed_temp, 'try-error') && length(ed_temp) == 1 && is.finite(ed_temp)) {
      out$ed = ed_temp
      ed = ed_temp
    }
  }

  default_max_embedding_dim = min(15, max(2, ed * 2), max_possible_dim)

  # correlation dimension (~5.7 ms/frame)
  if ('d2' %in% nonlinStats) {
    if (is.null(d2_pars$max.embedding.dim))
      d2_pars$max.embedding.dim = default_max_embedding_dim
    cd = suppressWarnings(try(
      do.call(nonlinearTseries::corrDim, c(list(
        time.series = x, do.plot = FALSE),
        d2_pars)),
      silent = TRUE))
    if (!inherits(cd, 'try-error')) {
      d2_temp = suppressWarnings(try(
        nonlinearTseries::estimate(cd),
        silent = TRUE))
      if (!inherits(d2_temp, 'try-error') && length(d2_temp) == 1 && is.finite(d2_temp))
        out$d2 = d2_temp
    }
  }

  # maximum Lyapunov exponent (~10 ms/frame)
  if ('ml' %in% nonlinStats) {
    if (is.null(ml_pars$max.embedding.dim))
      ml_pars$max.embedding.dim = default_max_embedding_dim
    ml = suppressWarnings(try(
      do.call(nonlinearTseries::maxLyapunov, c(list(
        time.series = x, do.plot = FALSE), ml_pars)),
      silent = TRUE))
    # plot(ml)
    if (!inherits(ml, 'try-error')) {
      ml_est = suppressWarnings(try(
        nonlinearTseries::estimate(ml, do.plot = FALSE), # might need better defaults
        silent = TRUE))
      if (!inherits(ml_est, 'try-error') && length(ml_est) == 1 && is.finite(ml_est))
        out$ml = ml_est
    }
  }

  # surrogate testing for stochasticity
  if ('sur' %in% nonlinStats) {
    if (is.null(sur_pars)) sur_pars = list()
    sur_pars = as.list(sur_pars)
    if (is.null(sur_pars$K)) sur_pars$K = 20

    st = suppressWarnings(try(
      do.call(nonlinearTseries::surrogateTest, c(list(
        time.series = x, verbose = FALSE, do.plot = FALSE), sur_pars)),
      silent = TRUE))
    if (!inherits(st, 'try-error')) {
      greater = suppressWarnings(try(
        mean(st$data.statistic > st$surrogates.statistics),
        silent = TRUE))
      less = suppressWarnings(try(
        mean(st$data.statistic < st$surrogates.statistics),
        silent = TRUE))
      if (!inherits(greater, 'try-error') && !inherits(less, 'try-error') &&
          is.finite(greater) && is.finite(less)) {
        extremeness = max(c(greater, less))
        # 0 = deterministic (scrambling ruins time symmetry)
        # 1 = stochastic (scrambling the phase has little effect on time symmetry)
        out$sur = max(0, min(1, 2 * (1 - extremeness)))
      }
    }
  }

  out
}
