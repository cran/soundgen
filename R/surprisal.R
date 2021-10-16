#' Get surprisal
#'
#' Tracks the (un)predictability of spectral changes in a sound over time,
#' returning a continuous contour of "surprisal". This is an attempt to track
#' auditory salience over time - that is, to identify parts of a sound that are
#' likely to involuntarily attract the listeners' attention. The functions
#' returns surprisal proper (`$surprisal`) and its product with increases in
#' loudness (`$surprisalLoudness`). Because getSurprisal() is slow and
#' experimental, it is not called by analyzed().
#'
#' Algorithm: we start with an auditory spectrogram produced by applying a bank
#' of bandpass filters to the signal, by default with central frequencies
#' equally spaced on the bark scale (see \code{\link{audSpectrogram}}). For each
#' frequency channel, a sliding window is analyzed to compare the actually
#' observed final value with its expected value. There are many ways to
#' extrapolate / predict time series and thus perform this comparison. Here, we
#' calculate the autocorrelation function of the window without the final point,
#' find its peak (i.e., the delay that produces the highest autocorrelation),
#' calculate autocorrelation of the window with the final point at this
#' "optimal" delay, and compare these two correlations. In effect, we estimate
#' how far the final point in our window deviates from the dominant oscillation
#' frequency or "fundamental frequency" of the time series, which in this case
#' represents the changes in amplitude in the same frequency channel over time.
#' The resulting per-channel surprisal contours are aggregated by taking their
#' mean weighted by the average amplitude of each frequency channel across the
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
#' surp = getSurprisal(sound, samplingRate = 16000, yScale = 'bark')
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
#'   colorTheme = 'heat.colors',  # pick color theme
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   ylim = c(0, 5),  # always in kHz
#'   main = 'Audiogram with surprisal contour' # title
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
  yScale = c('bark', 'mel', 'log')[1],
  nFilters = 64,
  dynamicRange = 80,
  minFreq = 20,
  maxFreq = samplingRate / 2,
  summaryFun = 'mean',
  reportEvery = NULL,
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
    'reportEvery', 'summaryFun', 'savePlots')]

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
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_surprisal.html'),
      plotFiles = paste0(pa$input$filenames_noExt, "_surprisal.png"),
      audioFiles = if (savePlots == '') pa$input$filenames_base else pa$input$filenames,
      width = paste0(width, units))
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
  surprisal = getSurprisal_matrix(sp, win = floor(winSurp / step))
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
    loud = resample(loud, len = len_surp, lowPass = FALSE)
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
      maxPoints = maxPoints, colorTheme = colorTheme,
      extraContour = sl_norm,
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
#' @keywords internal
getSurprisal_matrix = function(x, win) {
  # image(t(log(x)))
  nc = ncol(x)  # time
  nr = nrow(x)  # freq bins
  surprisal = rep(NA, nc)
  for (c in 2:nc) {  # for each time point
    idx_i = max(1, c - win + 1):c
    win_i = x[, idx_i]
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
      surp_i[r] = getSurprisal_vector(as.numeric(win_i[r, ]))
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
#' @keywords internal
#' @examples
#' x = c(rep(1, 3), rep(0, 4), rep(1, 3), rep(0, 4), rep(1, 3), 0, 0)
#' soundgen:::getSurprisal_vector(x)
#' soundgen:::getSurprisal_vector(c(x, 1))
#' soundgen:::getSurprisal_vector(c(x, 13))
getSurprisal_vector = function(x) {
  if (diff(range(x)) == 0) return(0)
  # plot(x, type = 'b')
  len = length(x)
  x1 = x[1:(len - 1)]
  # a = as.numeric(acf(x1, lag.max = len / 2, plot = FALSE)$acf)[-1]
  # plot(a, type = 'l')

  # calculate acf manually for speed and compatibility (we compare it later
  # with one more cor to estimate how well the next point fits in) - problems
  # with dealing with all-zero series, I don't know how acf does this, so
  # let's just stick to acf even if it's a bit slower
  # lag = 1:floor(len / 2)
  # len_lag = length(lag)
  # autocor = numeric(len_lag)
  # for (lag in 1:len_lag) {
  #   idx = (1 + lag) : len
  #   # n = len - lag
  #   autocor[lag] = cor(x1[idx], x1[idx - lag], use = 'complete.obs') # * (n - 1) / n
  # }
  # plot(a, type = 'l', ylim = c(-1, 1))
  # points(autocor, type = 'l', col = 'blue')
  # plot(autocor, type = 'b')
  if (diff(range(x1)) == 0) {
    # completely stationary until the analyzed point
    # out = abs(x[len] - x[len - 1]) # / max(tail(x, 2))
    # out = NA
    # or (but this tends to create a much higher peak at .5 after complete
    # silence vs near-silence)
    best_lag = 1
    best_acf = 1
  } else {
    autocor = as.numeric(acf(x1, lag.max = len - 2, plot = FALSE)$acf)[-1]
    # plot(autocor, type = 'b')
    # take the absolute max of autocor function
    # best_lag = which.max(autocor)
    # ...or find the highest peak to avoid getting best_lag = 1 all the time
    peaks = which(diff(sign(diff(autocor))) == -2) + 1
    if (length(peaks) > 0) {
      best_lag = peaks[which.max(autocor[peaks])]
    } else {
      best_lag = which.max(autocor)
    }
    best_acf = autocor[best_lag]
  }

  # check acf at the best lag for the time series with the next point
  # idx = (2 + best_lag) : (len + 1)
  # autocor_next_point = cor(x[idx], x[idx - best_lag], use = 'complete.obs')
  autocor_next_point = tail(as.numeric(acf(x, lag.max = best_lag,
                                           plot = FALSE)$acf), 1)
  # out = max(0, (best_acf - autocor_next_point) / 2)
  # rescale from [-2, 2] to [-1, 1] * len
  # * len to compensate for diminishing effects of single-point changes on acf
  # as window length increases
  out = (best_acf - autocor_next_point) / 2 # * len

  # # or: predict the last point and get residual
  # idx = (best_lag + 1) : (len - 1)
  # xc = x[idx]
  # xp = x[idx - best_lag]
  # # plot(xp, xc)
  # # cor(xc, xp, use = 'complete.obs')  # basically best_acf
  # mod = lm(xc ~ xp)
  # # summary(mod)
  # pred = as.numeric(predict(mod, newdata = data.frame(xp = x[len - best_lag])))
  # out = abs(x[len] - pred)

  return(out)
}
