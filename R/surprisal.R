#' Get surprisal
#'
#' Tracks the (un)predictability of spectral changes in a sound over time,
#' returning a continuous contour of "surprisal". This is an attempt to track
#' auditory salience over time - that is, to identify parts of a sound that are
#' likely to involuntarily attract the listeners' attention. The functions
#' returns surprisal proper (`$surprisal`) and its product with increases in
#' loudness (`$surprisalLoudness`). Because getSurprisal() is slow and
#' experimental, it is not called by analyze().
#'
#' Algorithm: we start with an auditory spectrogram produced by applying a bank
#' of bandpass filters to the signal, by default with central frequencies
#' equally spaced on the bark scale (see \code{\link{audSpectrogram}}). For each
#' frequency channel, a sliding window is analyzed to compare the actually
#' observed final value with its expected value. There are many ways to
#' extrapolate / predict time series and thus perform this comparison such as
#' autocorrelation (method = 'acf') or nonlinear prediction (method = 'np'). The
#' resulting per-channel surprisal contours are aggregated by taking their mean
#' weighted by the average amplitude of each frequency channel across the
#' analysis window. Because increases in loudness are known to be important
#' predictors of auditory salience, loudness per frame is also returned, as well
#' as the square root of the product of its derivative and surprisal.
#'
#' @return Returns a list with $detailed per-frame and $summary per-file results
#'   (see \code{\link{analyze}} for more information). Three measures are
#'   reported: \code{loudness} (in sone, as per \code{\link{getLoudness}}), the
#'   first derivative of loudness with respect to time (\code{dLoudness}),
#'   \code{surprisal} (non-negative), and \code{suprisalLoudness} (geometric
#'   mean of surprisal and dLoudness, treating negative values of dLoudnessas
#'   zero).
#'
#' @inheritParams audSpectrogram
#' @inheritParams analyze
#' @param winSurp surprisal analysis window, ms
#' @param method acf = change in maximum autocorrelation after adding the final
#'   point, np = nonlinear prediction (see \code{\link{nonlinPred}})
#' @param plot if TRUE, plots the auditory spectrogram and the
#'   \code{suprisalLoudness} contour
#' @export
#' @examples
#' # A quick example
#' s = soundgen(nSyl = 2, sylLen = 50, pauseLen = 25, addSilence = 15)
#' surp = getSurprisal(s, samplingRate = 16000)
#' surp
#'
#' \dontrun{
#' # A more meaningful example
#' sound = soundgen(nSyl = 5, sylLen = 150,
#'   pauseLen = c(50, 50, 50, 130), pitch = c(200, 150),
#'   noise = list(time = c(-300, 200), value = -20), plot = TRUE)
#' # playme(sound)
#' surp = getSurprisal(sound, samplingRate = 16000,
#'   yScale = 'bark', method = 'acf')
#' surp = getSurprisal(sound, samplingRate = 16000,
#'   yScale = 'bark', method = 'np')  # very slow
#'
#' # plot "pure" surprisal, without weighting by loudness
#' spectrogram(sound, 16000, extraContour = surp$detailed$surprisal /
#'   max(surp$detailed$surprisal, na.rm = TRUE) * 8000)
#'
#' # NB: surprisalLoudness contour is also log-transformed if yScale = 'log',
#' # so zeros become NAs
#' surp = getSurprisal(sound, samplingRate = 16000, yScale = 'log')
#'
#' # add bells and whistles
#' surp = getSurprisal(sound, samplingRate = 16000,
#'   yScale = 'mel',
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   brightness = -.1,  # reduce brightness
#'   # colorTheme = 'heat.colors',  # pick color theme...
#'   col = rev(hcl.colors(30, palette = 'Viridis')),  # ...or specify the colors
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   ylim = c(0, 5),  # always in kHz
#'   main = 'Audiogram with surprisal contour', # title
#'   extraContour = list(col = 'blue', lty = 2, lwd = 2)
#'   # + axis labels, etc
#' )
#'
#' surp = getSurprisal('~/Downloads/temp/', savePlots = '~/Downloads/temp/surp')
#' surp$summary
#' }
getSurprisal = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    step = 20,
    winSurp = 2000,
    method = c('acf', 'np')[1],
    yScale = c('bark', 'mel', 'log')[1],
    nFilters = 64,
    dynamicRange = 80,
    minFreq = 20,
    maxFreq = samplingRate / 2,
    summaryFun = 'mean',
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = NULL,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    contrast = .2,
    brightness = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  # match args
  myPars = as.list(environment())
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'reportEvery', 'cores', 'summaryFun', 'savePlots')]

  # call .getSurprisal
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.getSurprisal',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "surprisal", width = paste0(width, units)))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          data.frame(loudness = pa$result[[i]]$loudness,
                     surprisal = pa$result[[i]]$surprisal,
                     surprisalLoudness = pa$result[[i]]$surprisalLoudness),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]] [1, ]
        filler[1, ] = NA
      } else {
        stop('Failed to analyze any input')
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = cbind(data.frame(file = pa$input$filenames_base),
                      do.call('rbind', temp))
  } else {
    mysum_all = NULL
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(list(
    detailed = pa$result,
    summary = mysum_all
  ))
}


#' Get surprisal per sound
#'
#' Internal soundgen function called by \code{\link{getSurprisal}}.
#' @inheritParams getSurprisal
#' @keywords internal
.getSurprisal = function(
    audio,
    step,
    winSurp,
    method = c('acf', 'np')[1],
    yScale = c('bark', 'mel', 'log')[1],
    nFilters = 64,
    dynamicRange = 80,
    minFreq = 20,
    maxFreq = audio$samplingRate / 2,
    plot = TRUE,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    contrast = .2,
    brightness = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  if (is.null(maxFreq) | length(maxFreq) < 1) maxFreq = audio$samplingRate / 2
  # sp = getMelSpec(audio$sound, samplingRate = audio$samplingRate,
  #                 windowLength = windowLength, step = step,
  #                 maxFreq = maxFreq, specPars = specPars, plot = FALSE)
  # pad with winSurp of silence
  # silence = rep(0, audio$samplingRate * winSurp / 1000)
  # audio$sound = c(silence, audio$sound, silence)
  # audio$duration = audio$duration + winSurp * 2 / 1000
  # audio$ls = length(audio$sound)
  # env = getEnv(audio$sound, windowLength_points = 10, method = 'rms')
  # thres = 10 ^ (-dynamicRange / 20) * audio$scale
  # audio$sound[env < thres] = 0

  # get auditory spectrogram
  sp_list = .audSpectrogram(
    audio[names(audio) != 'savePlots'],
    step = step, nFilters = nFilters,
    yScale = yScale, dynamicRange = dynamicRange,
    minFreq = minFreq, maxFreq = maxFreq, plot = FALSE)
  sp = sp_list$audSpec_processed
  if (is.null(step)) step = 1000 / audio$samplingRate

  # set quiet sections below dynamicRange to zero
  thres = 10 ^ (-dynamicRange / 20)
  sp[sp < thres] = 0

  # get surprisal
  surprisal = getSurprisal_matrix(sp, win = floor(winSurp / step), method = method)
  # we don't care about negative surprisal
  surprisal[surprisal < 0] = 0

  # get loudness
  loud = .getLoudness(
    audio[which(names(audio) != 'savePlots')],  # otherwise saves plot
    step = step, plot = FALSE)$loudness
  # make sure surprisal and loudness are the same length
  # (initially they should be close, but probably not identical)
  len_surp = length(surprisal)
  loud[is.na(loud)] = 0
  if (length(loud) != len_surp) {
    loud = .resample(list(sound = loud), len = len_surp, lowPass = FALSE)
  }

  # multiply surprisal by time derivative of loudness
  loud_norm = loud / max(loud, na.rm = TRUE)
  dLoud = diff(c(0, loud_norm))
  surprisalLoudness = surprisal * dLoud # (surprisal + dLoud) / 2
  surprisalLoudness[surprisalLoudness < 0] = 0
  surprisalLoudness = sqrt(surprisalLoudness)

  # plotting
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_surprisal.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    if (!exists('main') || is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }
    sl_norm = surprisalLoudness / max(surprisalLoudness, na.rm = TRUE) * maxFreq
    plotSpec(
      X = as.numeric(colnames(sp)),  # time
      Y = as.numeric(rownames(sp)),  # freq
      Z = t(sp_list$audSpec_processed),
      audio = audio, internal = NULL, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim,
      yScale = yScale,
      contrast = contrast, brightness = brightness,
      maxPoints = maxPoints, colorTheme = colorTheme, col = col,
      extraContour = c(list(x = sl_norm), extraContour),
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      width = width, height = height,
      units = units, res = res,
      ...
    )
    if (is.character(audio$savePlots)) dev.off()
  }
  out = data.frame(
    surprisal = surprisal,
    loudness = loud,
    dLoudness = dLoud,
    surprisalLoudness = surprisalLoudness)
  invisible(out)
}


#' Get surprisal per matrix
#'
#' Internal soundgen function called by \code{\link{getSurprisal}}.
#' @param x input matrix such as a spectrogram (columns = time, rows =
#'   frequency)
#' @param win length of analysis window
#' @inheritParams getSurprisal
#' @keywords internal
getSurprisal_matrix = function(x,
                               win,
                               method = c('acf', 'np')[1]) {
  # image(t(log(x)))
  nc = ncol(x)  # time
  nr = nrow(x)  # freq bins
  surprisal = rep(NA, nc)
  for (c in 2:nc) {  # for each time point
    idx_i = max(1, c - win + 1):c
    win_i = x[, idx_i, drop = FALSE]
    if (c < win) {
      # pad the matrix with 0s on the left to have win columns
      nColsToAdd = win - c
      win_i = cbind(matrix(0, ncol = win - c, nrow = nr), win_i)
      # noise = matrix(runif(win * nr, 0, .001), nrow = nr)
      # win_i = cbind(noise[, 1:nColsToAdd], win_i)
    }
    # image(t(log(win_i)))
    surp_i = rep(NA, nr)
    for (r in 1:nr) {  # for each freq bin
      surp_i[r] = getSurprisal_vector(as.numeric(win_i[r, ]), method = method)
    }
    weights = as.numeric(rowSums(win_i))
    weights = weights / sum(weights)
    surprisal[c] = sum(surp_i * weights, na.rm = TRUE)
  }
  # plot(surprisal, type = 'b')
  return(surprisal)
}


#' Get surprisal per vector
#'
#' Internal soundgen function called by \code{\link{getSurprisal}}. Estimates
#' the unexpectedness or "surprisal" of the last element of input vector.
#' @param x numeric vector representing the time sequence of interest, eg
#'   amplitudes in a frequency bin over multiple STFT frames
#' @inheritParams getSurprisal
#' @keywords internal
#' @examples
#' x = c(rep(1, 3), rep(0, 4), rep(1, 3), rep(0, 4), rep(1, 3), 0, 0)
#' soundgen:::getSurprisal_vector(x)
#' soundgen:::getSurprisal_vector(c(x, 1))
#' soundgen:::getSurprisal_vector(c(x, 13))
#'
#' soundgen:::getSurprisal_vector(x, method = 'np')
#' soundgen:::getSurprisal_vector(c(x, 1), method = 'np')
#' soundgen:::getSurprisal_vector(c(x, 13), method = 'np')
getSurprisal_vector = function(x, method = c('acf', 'np')[1]) {
  ran_x = diff(range(x))
  if (ran_x == 0) return(0)
  # plot(x, type = 'b')
  len = length(x)
  x1 = x[1:(len - 1)]
  ran_x1 = diff(range(x1))
  if (ran_x1 == 0) {
    # completely stationary until the analyzed point
    surprisal = abs(x[len] - x[1]) / x[1]
    if (!is.finite(surprisal)) {
      out = 1
    } else if (surprisal < 1) {
      out = surprisal / 2
    } else {
      out = 1 / (1 + exp(1 - surprisal))
    }
    return(out)
  }

  if (method == 'acf') {
    autocor = as.numeric(acf(x1, lag.max = len - 2, plot = FALSE)$acf)[-1]
    # plot(autocor, type = 'b')
    # find the highest peak to avoid getting best_lag = 1 all the time
    peaks = which(diff(sign(diff(autocor))) == -2) + 1
    if (length(peaks) > 0) {
      best_lag = peaks[which.max(autocor[peaks])]
    } else {
      best_lag = which.max(autocor)
    }
    best_acf = suppressWarnings(
      cor(x1, c(x1[(best_lag+1):(len - 1)], rep(0, best_lag)))
    )

    # check acf at the best lag for the time series with the next point
    best_next_point = suppressWarnings(
      cor(x, c(x[(best_lag+1):len], rep(0, best_lag)))
    )
    out = (best_acf - best_next_point) / 2

    # rescale from [-2, 2] to [-1, 1] * len
    # * len to compensate for diminishing effects of single-point changes on acf
    # as window length increases
  } else if (method == 'np') {
    # predict the last point and get residual

    pr = try(nonlinPred(x1, nPoints = 1), silent = TRUE)
    if (inherits(pr, 'try-error')) pr = NA
    surprisal = abs(x[len] - pr) / ran_x1

    # rescale from [0, Inf) to [0, 1)
    if (FALSE) {
      a = c(seq(0, 1, .01), seq(1.1, 10, .1))
      for (i in 1:length(a)) {
        if (a[i] < 1) {
          b[i] = a[i] / 2
        } else {
          b[i] = 1 / (1 + exp(1 - a[i]))
        }
      }
      plot(a, b, log = 'x', type = 'l')
    }
    if (!is.finite(surprisal)) {
      if (is.finite(pr)) {
        out = 1
      } else {
        out = NA
      }
    } else if (surprisal < 1) {
      out = surprisal / 2
    } else {
      out = 1 / (1 + exp(1 - surprisal))
    }
  } else {
    stop('method not recognized')
  }
  return(out)
}
