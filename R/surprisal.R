#' Get surprisal
#'
#' Tracks the unpredictability of spectro-temporal changes in a sound over time,
#' returning continuous contours of Shannon surprisal (\code{$info}), Bayesian
#' surprise (\code{$kl} for Kullback-Leibler divergence), and
#' autocorrelation-based surprisal (\code{$surprisal}). This is an attempt to
#' track auditory salience over time - that is, to identify parts of a sound
#' that are likely to involuntarily attract the listeners' attention.
#'
#' Algorithm: the sound is transformed into some spectrogram-like representation
#' (e.g., an auditory spectrogram, a mel-warped STFT spectrogram, etc.) or an
#' RMS amplitude envelope. Using just the envelope is very fast, but then we
#' discard all spectral information. For each frequency channel, a sliding
#' window is analyzed to compare the actually observed final value with its
#' expected value. There are many ways to extrapolate / predict time series and
#' thus perform this comparison. The resulting per-channel surprisal contours
#' are aggregated by taking their mean - optionally, weighted by the maximum
#' amplitude of each frequency channel across the analysis window. Because
#' increases in loudness are known to be important predictors of auditory
#' salience, loudness per frame is also returned, as well as the product of its
#' positive changes and surprisal.
#'
#' @return Returns a list with surprisal statistics per frame ($detailed) and
#'   per file ($summary). Calculated measures:
#'   \describe{\item{loudness}{subjective loudness in sone, as per
#'   \code{\link{getLoudness}}} \item{surprisal}{surprisal calculated as the
#'   change in autocorrelation or as the nonlinear prediction error}
#'   \item{surprisalLoudness}{the product of surprisal and the first derivative
#'   of loudness with respect to time, treating negative values of dLoudness as
#'   zero} \item{info}{Shannon information calculated as -log(p), where p =
#'   density of Gaussian distribution at the next observation}
#'   \item{infoW}{windowed Shannon information: same as "info", but after
#'   applying a half-Gaussian taper that prioritizes more recent observations}
#'   \item{kl}{Bayesian log-surprisal: Kullback–Leibler divergence between the
#'   Gaussian distributions per frequency channel before vs. after observing the
#'   next datapoint} \item{klW}{windowed Bayesian log-surprisal}} Under
#'   \code{$detailed}, the function also returns several "_mat" objects that
#'   give the same statistics with a separate value for each time-frequency bin,
#'   as well as the best lag used to calculate autocorrelation (see examples).
#'
#' @inheritParams audSpectrogram
#' @inheritParams analyze
#' @param winSurp surprisal analysis window, ms (Inf = from sound onset)
#' @param input \code{audSpec} = auditory spectrogram
#'   (\code{\link{audSpectrogram}}, speed with default settings ~= 1x),
#'   \code{spectrogram} = STFT spectrogram with (\code{\link{spectrogram}},
#'   speed ~= 0.25x), \code{pspec} = STFT power spectrogram with
#'   (\code{\link[tuneR]{melfcc}}, speed ~= 0.2x), \code{melspec} = STFT
#'   mel-spectrogram with (\code{\link[tuneR]{melfcc}}, speed ~= 0.45x),
#'   \code{env} = analytic envelope (\code{\link{getRMS}}, speed ~= 27x) Any
#'   custom spectrogram-like matrix of features (time in columns labeled in s,
#'   features in rows) is also accepted (see examples)
#' @param takeLog if TRUE, the input is log-transformed prior to calculating
#'   surprisal. Negative values are treated as in \code{\link{audSpectrogram}} -
#'   note that the chosen dynamic range affects this normalization (the default
#'   is 80 dB). If \code{input = audSpec} or \code{input = spectrogram}, there
#'   can be other internal preprocessing like modifying contrast based on
#'   \code{audSpec_pars} or \code{spec_pars}
#' @param audSpec_pars,spec_pars,melfcc_pars,env_pars a list of parameters
#'   passed to \code{\link{audSpectrogram}} (if input = 'audSpec'),
#'   \code{\link{spectrogram}} (if input = 'spectrogram'),
#'   \code{\link[tuneR]{melfcc}} (if input = 'melspec' or 'pspec'), or
#'   \code{\link{getRMS}} (if input = 'env')
#' @param method (for $surprisal only, has no effect on $info and $kl)
#'   \code{acf} = change in maximum autocorrelation after adding the final
#'   point; \code{np} = nonlinear prediction (see \code{\link{nonlinPred}} -
#'   works but is VERY slow); \code{none} = do not calculate $surprisal to save
#'   time and only return $info and $kl
#' @param sameLagAllFreqs (only for method = 'acf') if TRUE, the bestLag is
#'   calculated by averaging the ACFs of all channels, and the same bestLag is
#'   used to calculate the surprisal in each frequency channel (we expect the
#'   same "rhythm" for all frequencies); if FALSE, the bestLag is calculated
#'   separately for each frequency channel (we can track different "rhythms" at
#'   different frequencies)
#' @param weightByAmpl if TRUE, ACFs and surprisal are weighted by max amplitude
#'   per frequency channel
#' @param weightByPrecision if TRUE, surprisal is weighted by the current
#'   autocorrelation, so deviations from a previous pattern are more surprising
#'   if this pattern is strong
#' @param onlyPeakAutocor if TRUE, only peaks of ACFs are considered (so bestLag
#'   can never be 1, and the first change after a string of static values
#'   results in surprisal = NA)
#' @param rescale if TRUE, surprisal is normalized from \code{(-Inf, Inf)} to
#'   \code{[-1, 1]}
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
#' # A couple of more meaningful examples
#'
#' ## Example 1: a temporal deviant
#' s0 = soundgen(nSyl = 8, sylLen = 150,
#'               pauseLen = c(rep(200, 7), 450), pitch = c(200, 150),
#'               temperature = .05, plot = FALSE)
#' sound = c(rep(0, 4000),
#'           addVectors(rnorm(16000 * 3.5, 0, .02), s0, insertionPoint = 4000),
#'           rep(0, 4000))
#' spectrogram(sound, 16000, yScale = 'ERB')
#'
#' # long window  (Inf = from the beginning)
#' surp = getSurprisal(sound, 16000, winSurp = Inf)
#' # Which frequency-time bins are surprising?
#' filled.contour(x = as.numeric(colnames(surp$detailed$surprisal_mat)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$surprisal_mat)),
#'                z = t(surp$detailed$surprisal_mat),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#' hist(surp$detailed$bestLag, xlab = 'Period, s')
#' abline(v = .35, lty = 3, lwd = 3, col = 'blue')  # true period = 350 ms
#' filled.contour(x = as.numeric(colnames(surp$detailed$bestLag)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$bestLag)),
#'                z = t(surp$detailed$bestLag),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#'
#' # just use the amplitude envelope instead of an auditory spectrogram
#' surp = getSurprisal(sound, 16000, winSurp = Inf, input = 'env')
#'
#' # increase spectral and temporal resolution (very slow)
#' surp = getSurprisal(sound, 16000, winSurp = 2000,
#'   audSpec_pars = list(nFilters = 50, step = 10,
#'   yScale = 'bark', bandwidth = 1/4))
#'
#' # weight by increase in loudness
#' spectrogram(sound, 16000, extraContour = surp$detailed$surprisalLoudness /
#'   max(surp$detailed$surprisalLoudness, na.rm = TRUE) * 8000)
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
#'   pauseLen = 90, pitch = c(1000, 750), rolloff = -20,
#'   pitchGlobal = c(rep(0, 5), 18, rep(0, 5)),
#'   temperature = .01, pitchCeiling = 7000,
#'   plot = TRUE, windowLength = 35)
#' surp = getSurprisal(s1, 16000, winSurp = 1500)
#' filled.contour(x = as.numeric(colnames(surp$detailed$surprisal_mat)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$surprisal_mat)),
#'                z = t(surp$detailed$surprisal_mat),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#' # deviant surprising both at 1 kHz (expected tone omitted) and at the new freq
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
#' filled.contour(x = as.numeric(colnames(surp$detailed$surprisal_mat)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$surprisal_mat)),
#'                z = t(surp$detailed$surprisal_mat),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#'
#' ## Example 4: different time scales
#' s8 = soundgen(nSyl = 4, sylLen = 75, pauseLen = 50)
#' s8 = rep(c(s8, rep(0, 2000)), 8)
#' getSurprisal(s8, 16000, input = 'env', winSurp = Inf)
#' # ACF picks up first the fast rhythm, then after a few cycles switches to
#' # the slow rhythm
#'
#' # Custom input: produce a nice spectrogram first, then feed it into ssm()
#' sp = spectrogram(s0, 16000, windowLength = 10, step = 10, contrast = .3,
#'   output = 'processed')  # return the modified spectrogram
#' colnames(sp) = as.numeric(colnames(sp)) / 1000  # convert ms to s
#' getSurprisal(s0, 16000, input = sp, takeLog = FALSE)
#'
#' # Custom input: use acoustic features returned by analyze()
#' an = analyze(s0, 16000, windowLength = 20)
#' input_an = t(an$detailed[, 4:ncol(an$detailed)]) # or select pitch, HNR, ...
#' input_an = t(apply(input_an, 1, scale))  # z-transform all variables
#' input_an[is.na(input_an)] = 0  # get rid of NAs
#' colnames(input_an) = an$detailed$time / 1000  # time stamps in s
#' rownames(input_an) = 1:nrow(input_an)
#' image(t(input_an))  # not a spectrogram, just a feature matrix
#' getSurprisal(s0, 16000, input = input_an, takeLog = FALSE)
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
    input = c('audSpec', 'env', 'melspec', 'spectrogram', 'pspec')[1],
    takeLog = TRUE,
    audSpec_pars = list(nFilters = 8, step = 15, minFreq = 60),
    spec_pars = list(windowLength = 20, step = 20),
    env_pars = list(windowLength = 40, step = 20),
    melfcc_pars = list(windowLength = 20, step = 20, maxfreq = NULL, nbands = NULL),
    method = c('acf', 'np')[1],
    sameLagAllFreqs = FALSE,
    weightByAmpl = TRUE,
    weightByPrecision = TRUE,
    onlyPeakAutocor = TRUE,
    rescale = FALSE,
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
                     surprisalLoudness = pa$result[[i]]$surprisalLoudness,
                     info = pa$result[[i]]$info,
                     infoW = pa$result[[i]]$infoW,
                     kl = pa$result[[i]]$kl,
                     klW = pa$result[[i]]$klW),
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
    input = c('audSpec', 'env', 'spectrogram', 'pspec', 'melspec')[1],
    takeLog = TRUE,
    audSpec_pars = list(filterType = 'butterworth', nFilters = 32,
                        step = 20, yScale = 'bark'),
    spec_pars = list(windowLength = c(5, 40), step = NULL),
    env_pars = list(windowLength = 40, step = 20),
    melfcc_pars = list(windowLength = 25, step = 5, maxfreq = NULL, nbands = NULL),
    method = c('acf', 'np')[1],
    sameLagAllFreqs = FALSE,
    weightByAmpl = TRUE,
    weightByPrecision = TRUE,
    onlyPeakAutocor = TRUE,
    rescale = FALSE,
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
  if (is.matrix(input)) {
    # custom input to getSurprisal() - use as is
    sp = as.matrix(input)
    colnames(sp) = as.numeric(colnames(sp)) * 1000  # need time in ms here
    step = diff(as.numeric(colnames(sp))[1:2])  # step in ms
    frame_points = round(audio$samplingRate * step)
    input = 'custom'
  } else {
    if (input == 'env') {
      env = do.call(.getRMS, c(env_pars, list(audio = audio, plot = FALSE)))
      # # or analytic amplitude envelope
      # smooth_win = step / 1000 * audio$samplingRate
      # env = seewave::env(audio$sound, f = audio$samplingRate, envt = 'hil',
      #                    msmooth = c(smooth_win, 50), plot = FALSE)
      # plot(env, type = 'l')
      sp = matrix(env, nrow = 1)
    } else if (input == 'audSpec') {
      # auditory spectrogram
      sp_list = do.call(.audSpectrogram, c(audSpec_pars, list(
        audio = audio[names(audio) != 'savePlots'], plot = FALSE)))
      if (takeLog) {
        sp = sp_list$audSpec_processed
      } else {
        sp = sp_list$audSpec
      }
    } else if (input == 'spectrogram') {
      sp = do.call(.spectrogram, c(spec_pars, list(
        audio = audio[names(audio) != 'savePlots'], plot = FALSE,
        output = if (takeLog) 'processed' else 'original')))
    } else if (input %in% c('pspec', 'melspec')) {
      if (is.null(melfcc_pars$windowLength)) melfcc_pars$windowLength = 25
      if (is.null(melfcc_pars$step)) melfcc_pars$step = 5
      if (!is.numeric(melfcc_pars$windowLength) | melfcc_pars$windowLength <= 0 |
          melfcc_pars$windowLength > (audio$duration / 2 * 1000)) {
        melfcc_pars$windowLength = min(50, round(audio$duration / 2 * 1000))
        warning(paste0(
          '"windowLength" must be between 0 and half the sound duration (in ms);
            resetting to ', melfcc_pars$windowLength, ' ms')
        )
      }
      if (is.null(melfcc_pars$step))
        melfcc_pars$step = melfcc_pars$windowLength / 4
      if (is.null(melfcc_pars$nbands)) {
        melfcc_pars$nbands = round(100 * melfcc_pars$windowLength / 20)
      }
      windowLength_points = floor(melfcc_pars$windowLength / 1000 *
                                    audio$samplingRate / 2) * 2
      if (is.null(melfcc_pars$maxfreq)) {
        melfcc_pars$maxfreq = floor(audio$samplingRate / 2)  # Nyquist
      }

      sound = tuneR::Wave(left = audio$sound, samp.rate = audio$samplingRate, bit = 16)
      mel = do.call(tuneR::melfcc, c(
        melfcc_pars[which(!names(melfcc_pars) %in% c('windowLength', 'step'))],
        list(
          samples = sound,
          wintime = melfcc_pars$windowLength / 1000,
          hoptime = melfcc_pars$step / 1000,
          spec_out = TRUE,
          numcep = 12
        )))
      if (input == 'pspec') {
        sp = t(mel$pspectrum)  # cols = time, rows = freq
        rownames(sp) = seq(0, audio$samplingRate / 2, length.out = nrow(sp)) / 1000
      } else if (input == 'melspec') {
        sp = t(mel$aspectrum)  # cols = time, rows = freq
        rownames(sp) = otherToHz(
          seq(0, HzToOther(audio$samplingRate / 2, "mel"),
              length.out = nrow(sp)), "mel") / 1000
      }
      colnames(sp) = seq(audio$timeShift, audio$duration,
                         length.out = ncol(sp)) * 1000

    } else {
      stop('input type not recognized')
    }
  }
  if (takeLog & !input %in% c('audSpec', 'spectrogram')) {
    # audSpec and spectrogram do log-transform internally
    sp = sp - min(sp, na.rm = TRUE)
    sp = log(sp + min(sp[sp > 0], na.rm = TRUE))
  }
  # image(t(sp))

  # # set quiet sections below dynamicRange to zero
  # thres = 10 ^ (-dynamicRange / 20)
  # sp[sp < thres] = 0

  # get surprisal
  surprisal_list = getSurprisal_matrix(
    sp,
    win = floor(winSurp / step),
    method = method,
    sameLagAllFreqs = sameLagAllFreqs,
    weightByAmpl = weightByAmpl,
    weightByPrecision = weightByPrecision,
    onlyPeakAutocor = onlyPeakAutocor,
    rescale = rescale)
  surprisal = surprisal_list$surprisal

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
      sl_norm = surprisal / max(abs(surprisal), na.rm = TRUE) * audio$scale
      time_stamps = seq(0, audio$duration * 1000, length.out = length(sl_norm))
      .osc(audio, main = '', ...)
      points(time_stamps, sl_norm, type = 'l', col = 'green')
      # layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(1, 1))
      # par(mar = c(mar[1:2], 0, mar[4]), xaxt = 's', yaxt = 's')
      # .osc(audio, main = '', ...)
      # par(mar = c(0, mar[2:4]), xaxt = 'n', yaxt = 's')
      # plot(surprisal, type = 'l', xlab = 'Points',
      #      ylab = 'Surprisal', ...)
    } else {
      # sl_norm = surprisalLoudness / max(surprisalLoudness, na.rm = TRUE) * maxFreq
      sl_norm = zeroOne(surprisal) * maxFreq
      if (!any(!is.na(sl_norm))) sl_norm = surprisal  # eg if all 0's
      sl_norm[sl_norm < 0] = 0  # don't plot negatives over the specrogram
      if (input == 'melspec') {
        yScale = 'mel'
      } else if (input == 'audSpec') {
        yScale = audSpec_pars$yScale
      } else {
        yScale = 'linear'
      }
      plotSpec(
        X = as.numeric(colnames(sp)),  # time
        Y = as.numeric(rownames(sp)),  # freq
        Z = t(sp), # if (input == 'audSpec') t(sp) else (log(t(sp + 1e-6))),
        audio = audio, internal = NULL,
        osc = osc, heights = heights, ylim = ylim,
        yScale = yScale,
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
  out = list(
    surprisal = surprisal,
    loudness = loud,
    dLoudness = dLoud,
    surprisalLoudness = surprisalLoudness,
    surprisal_mat = surprisal_list$surprisal_mat,
    bestLag_mat = surprisal_list$bestLag * step / 1000,
    info = surprisal_list$info,  # colMeans(surprisal_list$info_mat, na.rm = TRUE),
    info_mat = surprisal_list$info_mat,
    infoW = surprisal_list$infoW,  # colMeans(surprisal_list$infoW_mat, na.rm = TRUE),
    infoW_mat = surprisal_list$infoW_mat,
    kl = surprisal_list$kl,  # colMeans(surprisal_list$kl_mat, na.rm = TRUE),
    kl_mat = surprisal_list$kl_mat,
    klW = surprisal_list$klW,  # colMeans(surprisal_list$klW_mat, na.rm = TRUE),
    klW_mat = surprisal_list$klW_mat,
    spectrogram = sp)
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
    weightByPrecision = TRUE,
    onlyPeakAutocor = FALSE,
    rescale = FALSE) {
  # image(t(x))
  nc = ncol(x)  # time
  nr = nrow(x)  # freq bins
  surprisal = info = infoW = kl = klW = rep(NA, nc)
  surprisal_mat = bestLag_mat = info_mat = infoW_mat = kl_mat = klW_mat = x
  surprisal_mat[] = bestLag_mat[] = info_mat[] = infoW_mat[] = kl_mat[] = klW_mat[] = NA

  for (c in 2:nc) {  # for each time point
    idx_i = max(1, c - win + 1):c
    win_i = x[, idx_i, drop = FALSE]
    # # pad with zeros if shorter than target "win", so all the inputs passed to
    # # getSurprisal_vector() will have the same length - don't; leads to strange beh
    # if (!is.na(padWith) && padWith == 0 && ncol(win_i) < win) {
    #   win_i = cbind(
    #     matrix(0, nrow = nr, ncol = win - ncol(win_i)),
    #     win_i
    #   )
    # }
    # image(t(win_i))
    weights = apply(win_i, 1, max)
    sw = sum(weights)
    if (sw != 0) {
      weights = weights / sum(weights)
    } else {
      weights = rep(1, nr)
    }
    bestLag = NULL

    if (method == 'acf') {
      # by default, we determine bestLag separately for each frequency bin
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
          autocor = colMeans(autocor_matrix, na.rm = TRUE)
        }
        # autocor = colMeans(autocor_matrix, na.rm = TRUE)
        # plot(autocor, type = 'b')

        # find the highest peak of average ACF to avoid getting bestLag = 1 all the time
        peaks = which(diff(sign(diff(autocor))) == -2) + 1
        if (length(peaks) > 0) {
          bestLag = peaks[which.max(autocor[peaks])]
        } else {
          if (onlyPeakAutocor) {
            bestLag = NA
          } else {
            bestLag = which.max(autocor)
          }
        }
        if (length(bestLag) != 1 || !is.finite(bestLag)) bestLag = NA # NULL
      }
    }

    # calculate surprisal per bin as change in ACF at bestLag
    # (the same lag for all frequency bins)
    for (r in seq_len(nr)) {
      s_r = getSurprisal_vector(
        win_i[r, ], method = method,
        bestLag = bestLag,
        weightByPrecision = weightByPrecision,
        onlyPeakAutocor = onlyPeakAutocor
      )
      surprisal_mat[r, c] = s_r$surprisal
      bestLag_mat[r, c] = s_r$bestLag
      info_mat[r, c] = s_r$info
      infoW_mat[r, c] = s_r$infoW
      kl_mat[r, c] = s_r$kl
      klW_mat[r, c] = s_r$klW
    }
    # plot(surprisal_mat[, c], type = 'l')
    # plot(info_mat[, c], type = 'l')
  }
  # image(t(surprisal_mat))

  # calculate overall surprisal of the last point in the analysis window as the
  # mean surprisal across frequency bins
  if (weightByAmpl) {
    # weight by the max amplitude of each bin
    surprisal = colSums(sweep(surprisal_mat, MARGIN = 1, weights, `*`), na.rm = TRUE)
    info = colSums(sweep(info_mat, MARGIN = 1, weights, `*`), na.rm = TRUE)
    infoW = colSums(sweep(infoW_mat, MARGIN = 1, weights, `*`), na.rm = TRUE)
    kl = colSums(sweep(kl_mat, MARGIN = 1, weights, `*`), na.rm = TRUE)
    klW = colSums(sweep(klW_mat, MARGIN = 1, weights, `*`), na.rm = TRUE)
  } else {
    # just simple mean
    surprisal = colMeans(surprisal_mat, na.rm = TRUE)
    info = colMeans(info_mat, na.rm = TRUE)
    infoW = colMeans(infoW_mat, na.rm = TRUE)
    kl = colMeans(kl_mat, na.rm = TRUE)
    klW = colMeans(klW_mat, na.rm = TRUE)
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
  list(
    surprisal = surprisal, surprisal_mat = surprisal_mat, bestLag = bestLag_mat,
    info = info, info_mat = info_mat,
    infoW = infoW, infoW_mat = infoW_mat,
    kl = kl, kl_mat = kl_mat,
    klW = klW, klW_mat = klW_mat)
}


#' Get surprisal per vector
#'
#' Internal soundgen function called by \code{\link{getSurprisal}}.
#' Estimates the unexpectedness or "surprisal" of the last element of input
#' vector.
#' @param x numeric vector representing the time sequence of interest, eg
#'   amplitudes in a frequency bin over multiple STFT frames
#' @param bestLag (only for method = 'acf') if specified, we don't calculate
#'   the ACF but simply compare autocorrelation at bestLag with vs without the
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
getSurprisal_vector = function(
    x,
    method = c('acf', 'np', 'none')[1],
    bestLag = NULL,
    weightByPrecision = TRUE,
    onlyPeakAutocor = FALSE) {
  ran_x = diff(range(x))
  if (ran_x == 0) return(list(surprisal = 0, bestLag = NA,
                              info = NA, infoW = NA, kl = NA, klW = NA))
  # plot(x, type = 'b')
  len = length(x)
  x1 = x[-len]
  first = .subset(x, 1)
  last = .subset(x, len)
  ran_x1 = diff(range(x1))
  if (ran_x1 == 0) {
    # completely stationary until the analyzed point
    info = infoW = kl = klW = bestLag = surprisal = NA
    if (!onlyPeakAutocor) {
      if (first == 0) {
        surprisal = 1
      } else {
        surprisal = abs((last - first) / (last + first))
      }
    }
  } else {
    ## calculate Shannon information (doesn't depend on len)
    mean_x1 = mean(x1, na.rm = TRUE)
    sd_x1 = sd(x1, na.rm = TRUE)
    prob_x1 = dnorm(last, mean_x1, sd_x1) / dnorm(mean_x1, mean_x1, sd_x1)
    info = -log(max(1e-12, prob_x1))

    # or add half-Gaussian filter of "forgetfulness"
    win = dnorm(seq(-3, 0, length.out = len - 1))
    win = win / sum(win)
    # plot(win)
    mean_x1w = sum(x1 * win)  # weighted mean
    sd_x1w = sqrt(sum((x1 - mean_x1)^2 * win))  # weighted SD
    prob_x1w = dnorm(last, mean_x1w, sd_x1w) / dnorm(mean_x1w, mean_x1w, sd_x1w)
    infoW = -log(max(1e-12, prob_x1w))

    ## calculate Kullback-Leibler (KL) divergence between two Gaussian distributions
    # (from rodriguez-hidalgo_2018_bayesian-log-surprise, p. 6, but with log)

    # NB: kl DOES depend on len, so need to add 2 * log(len)
    # if (FALSE) {
    #   # correction for analysis window length
    #   a = rnorm(500)
    #   out = data.frame(mult = c(1 / (10:1), 1:50))
    #   for (i in 1:nrow(out)) {
    #     if (out$mult[i] < 1) {
    #       temp = approx(a, n = length(a) * out$mult[i])$y
    #     } else {
    #       temp = rep(a, out$mult[i]) #  approx(a, n = 20 * out$mult[i])$y
    #     }
    #     surp_i = getSurprisal_vector(c(temp, 3))
    #     out$kl[i] = surp_i$kl
    #     out$info[i] = surp_i$info
    #   }
    #   plot(out$mult, out$info, type = 'b')  # doesn't depend on len
    #   plot(out$mult, out$kl, type = 'b')  # declines logarithmically with len without correction
    #   plot(out$mult, out$kl + 2 * log(out$mult), type = 'b')
    #
    #   out$log_mult = log(out$mult)
    #   summary(lm(kl ~ log_mult, out))  # -2.1
    #   summary(lm(kl ~ log_mult, out[out$mult > 1, ]))  # -2
    # }
    var_x1 = sd_x1^2
    mean_x = mean(x, na.rm = TRUE)
    var_x = var(x, na.rm = TRUE)
    kl = log((mean_x - mean_x1)^2 / 2 / var_x1 +
               (var_x / var_x1 - 1 - log(var_x / var_x1)) / 2) + 2 * log(len)

    # KL with a half-Gaussian filter of "forgetfulness"
    var_x1w = sd_x1w^2
    # win_x = c(0, win)
    # lazy way - to avoid recalculating the entire win of length "len" instead of "len-1"
    win_x = dnorm(seq(-3, 0, length.out = len))
    win_x = win_x / sum(win_x)
    mean_xw = sum(x * win_x, na.rm = TRUE)  # weighted mean
    var_xw = sum((x - mean_x)^2 * win_x)  # weighted var
    klW = log((mean_xw - mean_x1w)^2 / 2 / var_x1w +
                (var_xw / var_x1w - 1 - log(var_xw / var_x1w)) / 2) + 2 * log(len)

    # calculate surprisal
    if (method == 'acf') {
      if (TRUE) {
        # non-stationary --> autocorrelation
        # center, as in acf()
        x = x - mean(x, na.rm = TRUE)
        x1 = x1 - mean(x1, na.rm = TRUE)
        if (is.null(bestLag)) {
          autocor = as.numeric(acf(x1, lag.max = len - 2, plot = FALSE)$acf)[-1]
          # plot(autocor, type = 'b')
          # if (FALSE) {
          #   # apply a Gaussian window to the ACF (has no effect on the result)
          #   win = dnorm(seq(0, 3, length.out = len - 2))
          #   autocor = autocor * win
          # }
          # find the highest peak to avoid getting bestLag = 1 all the time
          peaks = which(diff(sign(diff(autocor))) == -2) + 1
          if (length(peaks) > 0) {
            bestLag = peaks[which.max(autocor[peaks])]
          } else {
            if (onlyPeakAutocor) {
              bestLag = NA
            } else {
              bestLag = which.max(autocor)
            }
          }
        }

        if (is.na(bestLag)) {
          surprisal = NA
        } else {
          best_acf = suppressWarnings(
            # cor(x1, c(x1[(bestLag+1):(len - 1)], rep(0, bestLag)))
            cor(c(x1, rep(0, bestLag)), c(rep(0, bestLag), x1))
          )
          if (is.na(best_acf)) best_acf = 0

          # check acf at the best lag for the time series with the next point
          # (centered and zero-padded to get exactly the same values of autocor as
          # in acf, but this way we don't need to recalculate the entire ACF for the
          # last point, just a single value)
          best_next_point = suppressWarnings(
            # cor(x, c(x[(bestLag+1):len], rep(0, bestLag)))
            cor(c(x, rep(0, bestLag)), c(rep(0, bestLag), x))
          )
          if (is.na(best_next_point)) best_next_point = 0

          # rescale from [-2, 2] to [-1, 1] * len
          # * len to compensate for diminishing effects of single-point changes on acf
          # as window length increases (matter b/c we compare these values with the
          # stationary ones calculated above w/o acf, simply as abs(last-first)/first)
          # * abs(best_acf) to make a change more surprising if highly regular until now
          if (weightByPrecision) {
            surprisal = (best_acf - best_next_point) * len * abs(best_acf)
          } else {
            surprisal = (best_acf - best_next_point) * len
          }

          # or KL divergence, but then need non-negatives to reinterpret autocor
          # ~as probability
          # surprisal = best_acf * (log(best_acf) - log(best_next_point)) * len

          # or just how different the observed next point is from the last point at
          # bestLag (doesn't really seem to work)
          # obs = .subset(x, len)
          # expt = .subset(x, len - bestLag)
          # surprisal = abs((obs - expt) / (obs + expt)) # * best_acf
        }
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
      bestLag = NA
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
    } else if (method %in% c('none', 'gam')) {
      # slow and doesn't make much sense - a large k makes it follow periodic
      # trends, but then we get overfitting as well - basically, not enough data
      # for GAM
      # d = data.frame(time = seq_len(len - 1), value = x1)  # plot(d, type = 'b')
      # mod_gam = mgcv::gam(value ~ s(time, bs="cr"), data = d)
      # # plot(mod_gam)
      # pr = try(as.numeric(predict(mod_gam, newdata = data.frame(time = len))))
      # if (inherits(pr, 'try-error')) pr = NA
      # surprisal = abs(last - pr) / ran_x1
      surprisal = bestLag = NA
    } else {
      stop('method not recognized')
    }
  }

  if (!is.finite(surprisal)) surprisal = NA
  if (!is.finite(info)) info = NA
  if (!is.finite(infoW)) infoW = NA
  if (!is.finite(kl)) kl = NA
  list(surprisal = surprisal, bestLag = bestLag,
       info = info, infoW = infoW, kl = kl, klW = klW)
}
