#' Get surprisal
#'
#' Tracks the (un)predictability of spectral changes in a sound over time,
#' returning a continuous contour of "surprisal". This is an attempt to track
#' auditory salience over time - that is, to identify parts of a sound that are
#' likely to involuntarily attract the listeners' attention. The function
#' returns a proxy for surprisal (`$surprisal`) and its product with increases
#' in estimated subjective loudness (`$surprisalLoudness`). Because
#' getSurprisal() is slow and experimental, it is not called by analyze().
#'
#' Algorithm: the sound is transformed into an RMS amplitude envelope, a
#' standard STFT spectrogram, or an an auditory spectrogram produced by applying
#' a bank of bandpass filters to the signal (see \code{\link{audSpectrogram}}).
#' Using just the envelope is very fast, but then we discard all spectral
#' information. Auditory spectrograms are perceptually more valid than STFT
#' spectrograms and a bit faster because we don't get so many redundant
#' high-frequency bands. For each frequency channel, a sliding window is
#' analyzed to compare the actually observed final value with its expected
#' value. There are many ways to extrapolate / predict time series and thus
#' perform this comparison. The two implemented here are autocorrelation (method
#' = 'acf') or nonlinear prediction (method = 'np'). The resulting per-channel
#' surprisal contours are aggregated by taking their mean weighted by the
#' maximum amplitude of each frequency channel across the analysis window.
#' Because increases in loudness are known to be important predictors of
#' auditory salience, loudness per frame is also returned, as well as the
#' product of its positive changes and surprisal.
#'
#' @return Returns a list with $detailed per-frame and $summary per-file results
#'   (see \code{\link{analyze}} for more information). Three measures are
#'   reported: \code{loudness} (in sone, as per \code{\link{getLoudness}}), the
#'   first derivative of loudness with respect to time (\code{dLoudness}),
#'   \code{surprisal}, and \code{suprisalLoudness} product of surprisal and
#'   dLoudness, treating negative values of dLoudness as zero.
#'
#' @inheritParams audSpectrogram
#' @inheritParams analyze
#' @param winSurp surprisal analysis window, ms (Inf = from sound onset to each
#'   point)
#' @param input \code{audSpec} = auditory spectrogram
#'   (\code{\link{audSpectrogram}}, speed ~= 0.4x), \code{spec} = ordinary STFT
#'   spectrogram (\code{\link{spectrogram}}, speed ~= 0.25x), \code{env} =
#'   analytic envelope (\code{\link{getRMS}}, speed ~= 33x)
#' @param audSpec_pars,spec_pars,env_pars a list of parameters passed to
#'   \code{\link{audSpectrogram}} (if input = 'audSpec'),
#'   \code{\link{spectrogram}} (if input = 'spec'), or \code{\link{getRMS}} (if
#'   input = 'env')
#' @param method acf = change in maximum autocorrelation after adding the final
#'   point, np = nonlinear prediction (see \code{\link{nonlinPred}} - works but
#'   is VERY slow)
#' @param sameLagAllFreqs (only for method = 'acf') if TRUE, the best_lag is
#'   calculated by averaging the ACFs of all channels, and the same best_lag is
#'   used to calculate the surprisal in each frequency channel (we expect the
#'   same "rhythm" for all frequencies); if FALSE, the best_lag is calculated
#'   separately for each frequency channel (we can track different "rhythms" at
#'   different frequencies)
#' @param weightByAmpl if TRUE, ACFs and surprisal are weighted by max amplitude
#'   per frequency channel
#' @param rescale if TRUE, surprisal is normalized from (-Inf, Inf) to [-1, 1]
#' @param plot if TRUE, plots the auditory spectrogram and the
#'   \code{suprisalLoudness} contour
#' @param whatToPlot "surprisal" = pure surprisal, "surprisalLoudness" =
#'   surprisal x increase in subjective loudness
#' @export
#' @examples
#' # A quick example
#' s = soundgen(nSyl = 2, sylLen = 50, pauseLen = 25, addSilence = 15)
#' surp = getSurprisal(s, samplingRate = 16000)
#' surp
#'
#' \dontrun{
#' # A couple of more meaningful examples
#'
#' ## Example 1: a temporal deviant
#' s0 = soundgen(nSyl = 8, sylLen = 150,
#'               pauseLen = c(rep(200, 7), 450), pitch = c(200, 150),
#'               temperature = 1e-6, plot = FALSE)
#' sound = c(rep(0, 4000),
#'           addVectors(rnorm(16000 * 3.5, 0, .02), s0, insertionPoint = 4000),
#'           rep(0, 4000))
#' spectrogram(sound, 16000, yScale = 'ERB')
#'
#' # long window  (Inf = from the beginning)
#' surp = getSurprisal(sound, 16000, winSurp = Inf)
#'
#' # just use the amplitude envelope instead of an auditory spectrogram
#' surp = getSurprisal(sound, 16000, winSurp = Inf, input = 'env')
#'
#' # increase spectral and temporal resolution (slow)
#' surp = getSurprisal(sound, 16000, winSurp = 2000,
#'   audSpec_pars = list(nFilters = 128, step = 10,
#'   yScale = 'bark', bandwidth = 1/12))
#'
#' # weight by increase in loudness instead of "pure" surprisal
#' spectrogram(sound, 16000, extraContour = surp$detailed$surprisalLoudness /
#'   max(surp$detailed$surprisalLoudness, na.rm = TRUE) * 8000)
#' # or just
#' getSurprisal(sound, 16000, whatToPlot = 'surprisalLoudness')
#'
#' par(mfrow = c(3, 1))
#' plot(surp$detailed$surprisal, type = 'l', xlab = '',
#'   ylab = '', main = 'surprisal')
#' abline(h = 0, lty = 2)
#' plot(surp$detailed$dLoudness, type = 'l', xlab = '',
#'   ylab = '', main = 'd-loudness')
#' abline(h = 0, lty = 2)
#' plot(surp$detailed$surprisalLoudness, type = 'l', xlab = '',
#'   ylab = '', main = 'surprisal * d-loudness')
#' par(mfrow = c(1, 1))
#'
#' # short window = amnesia (every event is equally surprising)
#' getSurprisal(sound, 16000, winSurp = 250)
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
#' ## Example 2: a spectral deviant
#' s1 = soundgen(
#'   nSyl = 11, sylLen = 150, invalidArgAction = 'ignore',
#'   formants = NULL, lipRad = 0,  # so all syls have the same envelope
#'   pauseLen = 90, pitch = c(200, 150), rolloff = -20,
#'   pitchGlobal = c(rep(0, 5), 18, rep(0, 5)),
#'   temperature = .01, plot = TRUE, windowLength = 35, yScale = 'ERB')
#' surp = getSurprisal(s1, 16000, winSurp = 1500)
#' surp = getSurprisal(s1, 16000, winSurp = 1500,
#'   input = 'env')  # doesn't work - need spectral info
#'
#' s2 = soundgen(
#'   nSyl = 11, sylLen = 150, invalidArgAction = 'ignore',
#'   formants = NULL, lipRad = 0,  # so all syls have the same envelope
#'   pauseLen = 90, pitch = c(200, 150),  rolloff = -20,
#'   pitchGlobal = c(rep(18, 5), 0, rep(18, 5)),
#'   temperature = .01, plot = TRUE, windowLength = 35, yScale = 'ERB')
#' surp = getSurprisal(s2, 16000, winSurp = 1500)
#'
#' ## Example 3: different rhythms in different frequency bins
#' s6_1 = soundgen(nSyl = 23, sylLen = 100, pauseLen = 50, pitch = 1200,
#'   rolloffExact = 1, invalidArgAction = 'ignore', plot = TRUE)
#' s6_2 = soundgen(nSyl = 10, sylLen = 250, pauseLen = 100, pitch = 400,
#'   rolloffExact = 1, invalidArgAction = 'ignore', plot = TRUE)
#' s6_3 = soundgen(nSyl = 5, sylLen = 400, pauseLen = 200, pitch = 3400,
#'   rolloffExact = 1, invalidArgAction = 'ignore', plot = TRUE)
#' s6 = addVectors(s6_1, s6_2)
#' s6 = addVectors(s6, s6_3)
#'
#' surp = getSurprisal(s6, 16000, winSurp = Inf, sameLagAllFreqs = TRUE,
#'   audSpec_pars = list(nFilters = 32))
#' surp = getSurprisal(s6, 16000, winSurp = Inf, sameLagAllFreqs = FALSE,
#'   audSpec_pars = list(nFilters = 32))  # learns all 3 rhythms
#'
#' ## Example 4: different time scales
#' s8 = soundgen(nSyl = 4, sylLen = 75, pauseLen = 50)
#' s8 = rep(c(s8, rep(0, 2000)), 8)
#' getSurprisal(s8, 16000, input = 'env', winSurp = Inf)
#' # ACF picks up first the fast rhythm, then after a few cycles switches to
#' # the slow rhythm
#'
#' # analyze all sounds in a folder
#' surp = getSurprisal('~/Downloads/temp/', savePlots = '~/Downloads/temp/surp')
#' surp$summary
#' }
getSurprisal = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    winSurp = 2000,
    input = c('audSpec', 'spec', 'env')[1],
    audSpec_pars = list(filterType = 'butterworth', nFilters = 32,
                        step = 20, yScale = 'bark'),
    spec_pars = list(windowLength = 20, step = 20),
    env_pars = list(windowLength = 40, step = 20),
    method = c('acf', 'np')[1],
    sameLagAllFreqs = TRUE,
    weightByAmpl = TRUE,
    rescale = FALSE,
    summaryFun = 'mean',
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    whatToPlot = c('surprisal', 'surprisalLoudness')[1],
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
  # fill in defaults
  if (is.null(audSpec_pars$filterType)) audSpec_pars$filterType = 'butterworth'
  if (is.null(audSpec_pars$nFilters)) audSpec_pars$nFilters = 64
  if (is.null(audSpec_pars$step)) audSpec_pars$step = 20
  if (is.null(audSpec_pars$yScale)) audSpec_pars$yScale = 'ERB'
  if (audSpec_pars$nFilters == 1) input = 'env'

  # match args
  myPars = as.list(environment())
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'reportEvery', 'cores', 'summaryFun', 'savePlots', 'audSpec_pars', 'spec_pars')]
  myPars$audSpec_pars = audSpec_pars
  myPars$spec_pars = spec_pars

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
    for (i in seq_len(pa$input$n)) {
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
    winSurp,
    input = c('audSpec', 'spec', 'env')[1],
    audSpec_pars = list(filterType = 'butterworth', nFilters = 32,
                        step = 20, yScale = 'bark'),
    spec_pars = list(windowLength = c(5, 40), step = NULL),
    env_pars = list(windowLength = 40, step = 20),
    method = c('acf', 'np')[1],
    sameLagAllFreqs = TRUE,
    weightByAmpl = TRUE,
    rescale = FALSE,
    plot = TRUE,
    whatToPlot = c('surprisal', 'surprisalLoudness')[1],
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
  if (is.null(audSpec_pars$maxFreq)) {
    maxFreq = audio$samplingRate / 2
  } else {
    maxFreq = audSpec_pars$maxFreq
  }
  if (is.null(step)) step = 1000 / audio$samplingRate else step = audSpec_pars$step
  if (!is.finite(winSurp)) winSurp = length(audio$sound) / audio$samplingRate * 1000
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

  # extract the features to analyze
  if (input == 'env') {
    # # analytic amplitude envelope
    # smooth_win = step / 1000 * audio$samplingRate
    # env = seewave::env(audio$sound, f = audio$samplingRate, envt = 'hil',
    #                    msmooth = c(smooth_win, 50), plot = FALSE)
    env = do.call(.getRMS, c(env_pars, list(audio = audio, plot = FALSE)))
    # plot(env, type = 'l')
    sp = matrix(env, nrow = 1)
  } else if (input == 'spec') {
    # STFT spectrogram
    sp = do.call(.spectrogram, c(spec_pars, list(
      audio = audio[names(audio) != 'savePlots'], plot = FALSE,
      output = 'processed')))
  } else if (input == 'audSpec') {
    # auditory spectrogram
    sp_list = do.call(.audSpectrogram, c(audSpec_pars, list(
      audio = audio[names(audio) != 'savePlots'], plot = FALSE)))
    sp = sp_list$audSpec_processed
  } else {
    stop('input type not recognized')
  }

  # # set quiet sections below dynamicRange to zero
  # thres = 10 ^ (-dynamicRange / 20)
  # sp[sp < thres] = 0

  # get surprisal
  surprisal = getSurprisal_matrix(
    sp, win = floor(winSurp / step), method = method,
    sameLagAllFreqs = sameLagAllFreqs, weightByAmpl = weightByAmpl,
    rescale = rescale)
  # we don't care about negative surprisal
  # surprisal[surprisal < 0] = 0

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
  dLoud_rect = dLoud
  dLoud_rect[dLoud_rect < 0] = 0
  surprisal_rect = surprisal
  surprisal_rect[surprisal_rect < 0 ] = 0
  surprisalLoudness = surprisal_rect * dLoud_rect # (surprisal + dLoud) / 2
  # surprisalLoudness[surprisalLoudness < 0] = 0
  # surprisalLoudness = sqrt(surprisalLoudness)

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
    if (input == 'env') {
      sl_norm = get(whatToPlot) / max(abs(get(whatToPlot)), na.rm = TRUE) *
                                        audio$scale
      time_stamps = seq(0, audio$duration * 1000, length.out = length(sl_norm))
      .osc(audio, main = '', ...)
      points(time_stamps, sl_norm, type = 'l', col = 'green')
      # layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(1, 1))
      # par(mar = c(mar[1:2], 0, mar[4]), xaxt = 's', yaxt = 's')
      # .osc(audio, main = '', ...)
      # par(mar = c(0, mar[2:4]), xaxt = 'n', yaxt = 's')
      # plot(get(whatToPlot), type = 'l', xlab = 'Points',
      #      ylab = whatToPlot, ...)
    } else {
      # sl_norm = surprisalLoudness / max(surprisalLoudness, na.rm = TRUE) * maxFreq
      sl_norm = get(whatToPlot) / max(get(whatToPlot), na.rm = TRUE) * maxFreq
      sl_norm[sl_norm < 0] = 0  # don't plot negatives over the specrogram
      plotSpec(
        X = as.numeric(colnames(sp)),  # time
        Y = as.numeric(rownames(sp)),  # freq
        Z = t(sp), # if (input == 'audSpec') t(sp) else (log(t(sp + 1e-6))),
        audio = audio, internal = NULL,
        osc = osc, heights = heights, ylim = ylim,
        yScale = audSpec_pars$yScale,
        maxPoints = maxPoints, colorTheme = colorTheme, col = col,
        extraContour = c(list(x = sl_norm, warp = FALSE), extraContour),
        xlab = xlab, ylab = ylab, xaxp = xaxp,
        mar = mar, main = main, grid = grid,
        width = width, height = height,
        units = units, res = res,
        ...
      )
    }

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
getSurprisal_matrix = function(
    x,
    win,
    method = c('acf', 'np')[1],
    sameLagAllFreqs = TRUE,
    weightByAmpl = TRUE,
    rescale = FALSE) {
  # image(t(x))
  nc = ncol(x)  # time
  nr = nrow(x)  # freq bins
  surprisal = rep(NA, nc)
  for (c in 2:nc) {  # for each time point
    idx_i = max(1, c - win + 1):c
    win_i = x[, idx_i, drop = FALSE]
    # image(t(win_i))
    weights = apply(win_i, 1, max)
    weights = weights / sum(weights)
    surprisal_per_freq_bin = rep(NA, nr)

    if (method == 'acf') {
      # by default, we determine best_lag separately for each frequency bin
      best_lag = NULL
      if (sameLagAllFreqs) {
        # determine the best lag taking into account the ACFs of all frequency bins
        # extract ACF per bin
        len = ncol(win_i)
        autocor_matrix = matrix(NA, nrow = nr, ncol = len - 2)
        win_i_wo_last = win_i[, seq_len(len - 1), drop = FALSE]
        for (r in seq_len(nr)) {  # for each freq bin
          autocor_matrix[r, ] = as.numeric(acf(
            win_i_wo_last[r, ], lag.max = len - 2, plot = FALSE)$acf)[-1]
        }

        # average the ACFs across frequency bins
        if (weightByAmpl) {
          # weight by max amplitude per bin
          autocor = colSums(sweep(autocor_matrix, MARGIN = 1, weights, `*`), na.rm = TRUE)
        } else {
          # just simple mean
          autocor = colMeans(autocor_matrix)
        }
        # plot(autocor, type = 'b')

        # find the highest peak of average ACF to avoid getting best_lag = 1 all the time
        peaks = which(diff(sign(diff(autocor))) == -2) + 1
        if (length(peaks) > 0) {
          best_lag = peaks[which.max(autocor[peaks])]
        } else {
          best_lag = which.max(autocor)
        }
      }
      if (length(best_lag) != 1 || !is.finite(best_lag)) best_lag = NULL

      # calculate surprisal per bin as change in ACF at best_lag
      # (the same lag for all frequency bins)
      for (r in seq_len(nr)) {
        surprisal_per_freq_bin[r] = getSurprisal_vector(
          win_i[r, ], method = 'acf', best_lag = best_lag)
        # plot(surprisal_per_freq_bin, type = 'l')
      }
    } else if (method == 'np') {
      for (r in seq_len(nr)) {  # for each freq bin
        surprisal_per_freq_bin[r] = getSurprisal_vector(win_i[r, ], method = 'np')
      }
    }

    # calculate overall surprisal of the last point in the analysis window as the
    # mean surprisal across frequency bins
    if (weightByAmpl) {
      # weight by the max amplitude of each bin
      surprisal[c] = sum(surprisal_per_freq_bin * weights, na.rm = TRUE)
    } else {
      # just simple mean
      surprisal[c] = mean(surprisal_per_freq_bin, na.rm = TRUE)
    }
  }
  # plot(surprisal, type = 'b')

  # rescale surprisal from (-Inf, Inf) to [-1, 1]
  if (rescale) {
    # idx_pos = which(surprisal > 0)
    # surprisal[idx_pos] = surprisal[idx_pos] / (surprisal[idx_pos] + 1)
    # # a = c(seq(0, 1, .01), seq(1.1, 10, .1)); plot(a, a / (a + 1), log = 'x', type = 'l')
    # idx_neg = which(surprisal < 0)
    # surprisal[idx_neg] = -surprisal[idx_neg] / (surprisal[idx_neg] - 1)
    # # a = seq(-25, 0, .01); plot(a, -a / (a - 1), type = 'l')

    # or just logistic (-1, 1)
    surprisal = 1 - 2 / (exp(surprisal) + 1)
    # a = seq(-5, 5, .02); plot(a, 1 - 2 / (exp(a) + 1), type = 'l')
  }
  surprisal
}


#' Get surprisal per vector
#'
#' Internal soundgen function called by \code{\link{getSurprisal}}.
#' Estimates the unexpectedness or "surprisal" of the last element of input
#' vector.
#' @param x numeric vector representing the time sequence of interest, eg
#'   amplitudes in a frequency bin over multiple STFT frames
#' @param best_lag (only for method = 'acf') if specified, we don't calculate
#'   the ACF but simply compare autocorrelation at best_lag with vs without the
#'   final point
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
getSurprisal_vector = function(x, method = c('acf', 'np')[1], best_lag = NULL) {
  ran_x = diff(range(x))
  if (ran_x == 0) return(0)
  # plot(x, type = 'b')
  len = length(x)
  x1 = x[seq_len(len - 1)]
  first = .subset(x, 1)
  last = .subset(x, len)
  ran_x1 = diff(range(x1))
  if (ran_x1 == 0) {
    # completely stationary until the analyzed point
    if (first == 0) {
      surprisal = 1  # becomes 1/2 after surprisal / (surprisal + 1) normalization
    } else {
      surprisal = abs((last - first) / (last + first))
    }
  } else if (method == 'acf') {
    if (TRUE) {
      # non-stationary --> autocorrelation
      # center, as in acf()
      x = x - mean(x, na.rm = TRUE)
      x1 = x1 - mean(x1, na.rm = TRUE)
      if (is.null(best_lag)) {
        autocor = as.numeric(acf(x1, lag.max = len - 2, plot = FALSE)$acf)[-1]
        # plot(autocor, type = 'b')
        # find the highest peak to avoid getting best_lag = 1 all the time
        peaks = which(diff(sign(diff(autocor))) == -2) + 1
        if (length(peaks) > 0) {
          best_lag = peaks[which.max(autocor[peaks])]
        } else {
          best_lag = which.max(autocor)
        }
      }

      best_acf = suppressWarnings(
        # cor(x1, c(x1[(best_lag+1):(len - 1)], rep(0, best_lag)))
        cor(c(x1, rep(0, best_lag)), c(rep(0, best_lag), x1))
      )
      if (is.na(best_acf)) best_acf = 0

      # check acf at the best lag for the time series with the next point
      # (centered and zero-padded to get exactly the same values of autocor as
      # in acf, but this way we don't need to recalculate the entire ACF for the
      # last point, just a single value)
      best_next_point = suppressWarnings(
        # cor(x, c(x[(best_lag+1):len], rep(0, best_lag)))
        cor(c(x, rep(0, best_lag)), c(rep(0, best_lag), x))
      )
      if (is.na(best_next_point)) best_next_point = 0

      # rescale from [-2, 2] to [-1, 1] * len
      # * len to compensate for diminishing effects of single-point changes on acf
      # as window length increases (matter b/c we compare these values with the
      # stationary ones calculated above w/o acf, simply as abs(last-first)/first)
      # * abs(best_acf) to make a change more surprising if highly regular until now
      surprisal = (best_acf - best_next_point) * len * abs(best_acf)

      # or KL divergence, but then need non-negatives to reinterpret autocor
      # ~as probability
      # surprisal = best_acf * (log(best_acf) - log(best_next_point)) * len

      # or just how different the observed next point is from the last point at
      # best_lag (doesn't really seem to work)
      # obs = .subset(x, len)
      # expt = .subset(x, len - best_lag)
      # surprisal = abs((obs - expt) / (obs + expt)) # * best_acf
    } else {
      # a possible alternative - compare all peaks, not just one
      # (so not limited to 1 lag; doesn't seem to work; also tried just summing
      # the entire ACFs)
      autocor = as.numeric(acf(x1, lag.max = len - 2, plot = FALSE)$acf)[-1]
      # plot(autocor, type = 'b')
      peaks = which(diff(sign(diff(autocor))) == -2) + 1
      autocor_next = as.numeric(acf(x, lag.max = len - 2, plot = FALSE)$acf)[-1]
      # plot(autocor_next, type = 'b')
      peaks_next = which(diff(sign(diff(autocor_next))) == -2) + 1
      surprisal = (mean(autocor[peaks]) - mean(autocor_next[peaks_next])) * len
    }
  } else if (method == 'np') {
    # non-stationary --> nonlinear prediction
    # predict the last point and get residual
    pr = try(nonlinPred(x1, nPoints = 1), silent = TRUE)
    if (inherits(pr, 'try-error')) pr = NA
    surprisal = abs(last - pr) / ran_x1
    # or -log(p) - "proper" surprisal, but again we have to convert the prediction error into a prob
    # surprisal = -log(dnorm(pr, last, sd(x1)))
    # or -log(prob_error):
    # surprisal = -log(dnorm(abs(last - pr), 0, ran_x1))
    # assuming pred errors are ~gaussian with a large sd to avoid getting density > 0
    # (doesn't work as well as just simple abs prediction error / ran_x1)
    if (!is.finite(surprisal)) {
      if (is.finite(pr)) {
        surprisal = 1
      } else {
        surprisal = NA
      }
    }
  } else if (method == 'gam') {
    # slow and doesn't make much sense - a large k makes it follow periodic
    # trends, but then we get overfitting as well - basically, not enough data
    # for GAM
    # d = data.frame(time = seq_len(len - 1), value = x1)  # plot(d, type = 'b')
    # mod_gam = mgcv::gam(value ~ s(time, bs="cr"), data = d)
    # # plot(mod_gam)
    # pr = try(as.numeric(predict(mod_gam, newdata = data.frame(time = len))))
    # if (inherits(pr, 'try-error')) pr = NA
    # surprisal = abs(last - pr) / ran_x1
    surprisal = NA
  } else {
    stop('method not recognized')
  }

  # put on the same scale as change after completely static values above, namely [0, 1)
  if (!is.finite(surprisal)) {
    NA
  } else {
    surprisal
  }
}
