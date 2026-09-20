#' Get surprisal
#'
#' Tracks the unpredictability of spectro-temporal changes in a sound over time,
#' returning continuous contours of Shannon surprisal (\code{$info}), Bayesian
#' surprise (\code{$kl} for Kullback-Leibler divergence), and
#' autocorrelation-based surprisal (\code{$surprisal}). This is an attempt to
#' track auditory salience over time - that is, to identify parts of a sound
#' that are likely to involuntarily attract the listener's attention.
#'
#' Algorithm: the sound is transformed into some spectrogram-like representation
#' (e.g., an auditory spectrogram, a mel-warped STFT spectrogram, etc.) or an
#' RMS amplitude envelope. Using just the envelope is very fast, but then we
#' discard all spectral information. For each frequency channel, a sliding
#' window is analyzed to compare the actually observed final value with its
#' expected value. The resulting per-channel surprisal contours are aggregated
#' by taking their mean - optionally, weighted by the maximum amplitude of each
#' frequency channel across the analysis window. Because increases in loudness
#' are known to be important predictors of auditory salience, loudness per frame
#' is also returned, as well as the product of its positive changes and
#' surprisal.
#'
#' @references \itemize{
#'   \item Anikin, A. (2026) Measuring surprisal in sound sequences. Behavior
#'   Research Methods. doi: 10.3758/s13428-026-03153-3.
#' }
#'
#' @return A list with two top-level elements: \code{$detailed} and
#'   \code{$summary}.
#'
#'   \code{$detailed} contains per-frame statistics selected with the
#'   \code{output} argument. If multiple sounds are analyzed, \code{$detailed}
#'   is a list of per-sound lists.
#'
#'   \code{$summary} contains per-file summaries of a fixed set of contours:
#'   \code{loudness}, \code{surprisal}, \code{surprisalLoudness}, \code{info},
#'   \code{infoW}, \code{kl}, and \code{klW}. These are summarized even if not
#'   all of them are included in \code{output}. If \code{summaryFun} is
#'   \code{NULL}, \code{$summary} is \code{NULL}.
#'
#'   Available measures:
#'   \describe{
#'     \item{surprisal}{Aggregated surprisal contour: change in autocorrelation.
#'       Values are averaged across frequency channels, optionally weighted by
#'       channel amplitude. Positive values mean "an unexpected change", and
#'       negative values mean "a change that confirms the expectations, making
#'       signal periodicity more certain." If \code{rescale = TRUE}, the
#'       aggregated contour is transformed with \code{tanh(surprisal / 2)} to
#'       approximately \code{(-1, 1)}.}
#'
#'     \item{loudness}{Subjective loudness in sone, as per
#'       \code{\link{getLoudness}}, resampled to match the number of surprisal
#'       frames.}
#'
#'     \item{dLoudness}{First temporal derivative of max-normalized loudness:
#'       \code{diff(c(0, loudness / max(loudness)))}.}
#'
#'     \item{surprisalLoudness}{Product of the positive parts of surprisal and
#'       dLoudness: \code{max(surprisal, 0) * max(dLoudness, 0)}. This contour
#'       emphasizes surprising events that coincide with increases in
#'       loudness. If \code{rescale = TRUE}, this uses the rescaled surprisal
#'       contour.}
#'
#'     \item{surprisal_mat}{Matrix of per-channel surprisal values before
#'       aggregation across frequency channels (frequency channels in rows,
#'       time frames in columns). Column names are time in ms.}
#'
#'     \item{bestLag_mat}{Matrix of the autocorrelation lag used to calculate
#'       ACF-surprisal in each time-frequency bin, in seconds. \code{NA} where
#'       no lag was available or applicable, static analysis windows, or when
#'       \code{onlyPeakAutocor = TRUE} and no ACF peak was found. If
#'       \code{sameLagAllFreqs = TRUE}, the same lag is used for all non-static
#'       frequency channels; static channels still return \code{NA}.}
#'
#'     \item{info}{Shannon surprisal contour, calculated as \code{-log(rho)},
#'       where \code{rho} is the Gaussian density at the next observation
#'       normalized by the maximum Gaussian density. Values are capped at
#'       \code{-log(minProb)}.}
#'
#'     \item{info_mat}{Matrix of per-channel Shannon surprisal values
#'       corresponding to \code{info}.}
#'
#'     \item{infoW}{Windowed Shannon surprisal: same as \code{info}, but using
#'       weighted means and standard deviations from a half-Gaussian taper that
#'       prioritizes more recent observations.}
#'
#'     \item{infoW_mat}{Matrix of per-channel windowed Shannon surprisal values
#'       corresponding to \code{infoW}.}
#'
#'     \item{kl}{Bayesian log-surprisal: natural logarithm of the
#'       Kullback-Leibler divergence between the Gaussian distributions before
#'       and after observing the next data point, with a window-length
#'       correction added as \code{2 * log(n)}. To avoid \code{-Inf}, the KL
#'       divergence is floored at \code{minProb} before taking the logarithm.
#'       Values can therefore be negative.}
#'
#'     \item{kl_mat}{Matrix of per-channel Bayesian log-surprisal values
#'       corresponding to \code{kl}.}
#'
#'     \item{klW}{Windowed Bayesian log-surprisal: same as \code{kl}, but using
#'       weighted means and variances from a half-Gaussian taper that
#'       prioritizes more recent observations. The full window length \code{n}
#'       is still used for the \code{2 * log(n)} correction.}
#'
#'     \item{klW_mat}{Matrix of per-channel windowed Bayesian log-surprisal
#'       values corresponding to \code{klW}.}
#'
#'     \item{spectrogram}{The spectrogram-like feature matrix actually analyzed
#'       (frequency channels or features in rows, time frames in columns),
#'       after any requested preprocessing such as log-transformation. Column
#'       names are time in ms. If \code{specFun = 'env'}, this is a one-row
#'       matrix containing the RMS envelope, possibly log-transformed if
#'       \code{logSpec = TRUE}.}
#'   }
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams spectrogram
#' @param winSurp surprisal analysis window, ms. \code{Inf} means "from sound
#'   onset"; windows shorter than 3 frames produce \code{NA}
#' @param logSpec if TRUE, the output of \code{specFun} is log-transformed prior
#'   to calculating surprisal, offsetting as needed to avoid non-positive values
#' @param specFun the function used to extract a spectrogram-like feature
#'   matrix. Can be a string or a custom function that takes audio (numeric
#'   vector) as the first argument and returns a spectrogram-like matrix with
#'   time in columns and features in rows (see examples). A precomputed
#'   spectrogram-like matrix is also accepted (features in rows, time in
#'   columns [ms], numeric rownames for plotting). Supported strings:
#'   \describe{
#'     \item{\code{\link{stft_simple}}}{
#'       \code{'stft'} / \code{STFT} / \code{'stft_simple'} (amplitude spectrogram)
#'       Parameters in \code{specFun_pars}: \code{samplingRate} (optional if
#'       frequency and time labels are not needed), \code{wl} (samples),
#'       \code{step} (samples), \code{wn}, \code{zp}, \code{padWithSilence}.
#'     }
#'     \item{\code{\link{spectrogram}}}{
#'       \code{'spectrogram'} (amplitude spectrogram - a wrapper around
#'       stft_simple with more options).
#'       Parameters in \code{specFun_pars}: see \code{\link{spectrogram}}.
#'     }
#'     \item{\code{\link[tuneR:powspec]{powspec}}}{
#'       \code{'powerspec'} (power spectrogram with tuneR).
#'       Parameters in \code{specFun_pars}: \code{wintime} (s) or
#'       \code{windowLength} (ms), \code{steptime} (s) or \code{step} (ms),
#'       \code{dither}.
#'     }
#'     \item{\code{\link[tuneR:melfcc]{melfcc}}}{
#'       \code{'melspec'} (mel-spectrogram), \code{'mfcc'} / \code{'melfcc'} (MFCCs).
#'       Parameters in \code{specFun_pars}: \code{windowLength} (ms), \code{step} (ms),
#'       \code{nbands}, \code{maxfreq} (Hz), \code{MFCC} (integer vector).
#'     }
#'     \item{\code{\link{audSpectrogram}}}{
#'       \code{'audSpectrogram'} / \code{'audSpec'} (auditory spectrogram).
#'       Parameters in \code{specFun_pars}: see \code{\link{audSpectrogram}}.
#'     }
#'     \item{\code{\link{getRMS}}}{
#'       \code{'getRMS'} / \code{'rms'} (RMS amplitude envelope).
#'       Parameters in \code{specFun_pars}: see \code{\link{getRMS}}.
#'     }
#'     \item{\code{\link{getEnv}}}{
#'       \code{'getEnv'} / \code{'env'} (various smoothed envelopes: RMS,
#'       analytical, peak, etc.).
#'       Parameters in \code{specFun_pars}: see \code{\link{getEnv}}.
#'     }
#' }
#' @param specFun_pars a list of parameters passed to \code{specFun}. Defaults
#'   for audSpectrogram, \code{list(yScale = 'ERB', nFilters_oct = 6, step = 15,
#'   minFreq = 60)}; for melspec, \code{windowLength = 20, step = 20, maxfreq =
#'   NULL, nbands = 128, MFCC = 2:13}; for spectrogram, \code{windowLength = 20,
#'   step = 20}; for env, \code{windowLength = 40, step = 20} converted to
#'   samples; for rms, \code{windowLength = 40, step = 20}. For convenience when
#'   using melspec, \code{windowLength} and \code{step} in ms are converted to
#'   \code{wintime} and \code{steptime} in seconds if no explicit values in
#'   seconds are supplied.
#' @param method affects \code{$surprisal} and \code{$bestLag_mat} only; has
#'   no effect on \code{$info} and \code{$kl}. \code{acf} = change in
#'   autocorrelation at the previously best lag after adding the final point;
#'   \code{none} = do not calculate \code{$surprisal}; \code{$surprisal} and
#'   \code{$bestLag_mat} are then \code{NA}.
#' @param sameLagAllFreqs only for \code{method = 'acf'}. If TRUE, the
#'   bestLag is calculated by averaging the ACFs of all channels, and the same
#'   bestLag is used to calculate the surprisal in each frequency channel (we
#'   expect the same "rhythm" for all frequencies). If FALSE, the bestLag is
#'   calculated separately for each frequency channel (we can track different
#'   "rhythms" at different frequencies).
#' @param weightByAmpl f TRUE, ACF averaging (when
#'   \code{sameLagAllFreqs = TRUE}) and aggregation of all per-channel contours
#'   (\code{surprisal}, \code{info}, \code{infoW}, \code{kl}, \code{klW}) are
#'   weighted by the maximum non-negative value per frequency channel in the
#'   current analysis window. For log-transformed, processed, or custom feature
#'   matrices, these weights are feature maxima, not necessarily physical
#'   amplitude.
#' @param weightByPrecision if TRUE, ACF-based surprisal is weighted by the
#'   current autocorrelation, so deviations from a previous pattern are more
#'   surprising if this pattern is strong.
#' @param onlyPeakAutocor if TRUE, only peaks of ACFs are considered (so
#'   bestLag can never be 1, and the first change after a string of static
#'   values results in surprisal = NA).
#' @param rescale if TRUE, aggregated surprisal is normalized from
#'   \code{(-Inf, Inf)} to approximately \code{(-1, 1)} using
#'   \code{tanh(surprisal / 2)}. \code{surprisal_mat} is unaffected.
#' @param minProb minimum probability used to cap Shannon surprisal:
#'   \code{info} and \code{infoW} cannot exceed \code{-log(minProb)}. Also
#'   used as a lower bound for the KL divergence before logging in \code{kl}
#'   and \code{klW}.
#' @param summaryFun functions used to summarize each acoustic characteristic,
#'   eg \code{c('mean', 'sd')}; user-defined functions are fine (see examples);
#'   NAs are omitted automatically for mean/median/sd/min/max/range/sum,
#'   otherwise take care of NAs yourself
#' @param plot If TRUE, plots the feature matrix and the surprisal contour.
#'   For \code{specFun = 'env'}, the surprisal contour is overlaid on an
#'   oscillogram.
#' @param output what to return, options: 'surprisal', 'loudness', 'dLoudness',
#'   'surprisalLoudness', 'surprisal_mat', 'bestLag_mat', 'info', 'info_mat',
#'   'infoW', 'infoW_mat', 'kl', 'kl_mat', 'klW', 'klW_mat', 'spectrogram',
#'   'all' (see the Return section)
#' @param ... other graphical parameters
#'
#' @export
#'
#' @examples
#' # A quick example
#' data('speechEx', package = 'soundgen')
#' surp = getSurprisal(speechEx, from = 0.5, to = 1)
#' surp
#'
#' \dontrun{
#' # A few more meaningful examples
#'
#' ## Example 1: a temporal deviant
#' s0 = soundgen(nSyl = 8, sylLen = 150,
#'               pauseLen = c(rep(200, 7), 450), pitch = c(200, 150),
#'               temperature = .05, plot = FALSE)
#' sound = c(rep(0, 4000),
#'           addVectors(rnorm(16000 * 3.5, 0, .02), s0, insertionPoint = 4000),
#'           rep(0, 200))
#' spectrogram(sound, 16000, yScale = 'ERB')
#'
#' # long window (Inf = from the beginning)
#' surp = getSurprisal(sound, 16000, winSurp = Inf, output = 'all')
#' plot(sound, type = 'l')
#' surp_cont = surp$detailed$surprisal
#' lines(seq(0, length(sound), length.out = length(surp_cont)),
#'   surp_cont / max(surp_cont, na.rm = TRUE), col = 'blue', lwd = 2)
#'
#' # Which frequency-time bins are surprising?
#' filled.contour(x = as.numeric(colnames(surp$detailed$surprisal_mat)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$surprisal_mat)),
#'                z = t(surp$detailed$surprisal_mat),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#' # Best lag (periodicity) over time
#' hist(surp$detailed$bestLag_mat, xlab = 'Period, s')
#' abline(v = .35, lty = 3, lwd = 3, col = 'blue')  # true period = 350 ms
#'
#' # just use the amplitude envelope instead of an auditory spectrogram
#' surp = getSurprisal(sound, 16000, winSurp = Inf, specFun = 'env')
#'
#' # increase spectral and temporal resolution (can be slow)
#' surp = getSurprisal(sound, 16000, winSurp = 2000,
#'   specFun_pars = list(nFilters = 50, step = 10,
#'   yScale = 'bark', bandwidth = 1/4), output = 'all')
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
#' # short window = amnesia (every new sound is surprising)
#' getSurprisal(sound, 16000, winSurp = 300)
#'
#' # add bells and whistles
#' surp = getSurprisal(sound, samplingRate = 16000,
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
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
#' surp = getSurprisal(s1, 16000, winSurp = 1500, output = 'all')
#' filled.contour(x = as.numeric(colnames(surp$detailed$surprisal_mat)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$surprisal_mat)),
#'                z = t(surp$detailed$surprisal_mat),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#' # deviant surprising both at 1 kHz (expected tone omitted) and at the new freq
#' surp = getSurprisal(s1, 16000, winSurp = 1500,
#'   specFun = 'env')  # doesn't work - need spectral info
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
#'   specFun_pars = list(nFilters = 32), output = 'all')
#' surp = getSurprisal(s6, 16000, winSurp = Inf, sameLagAllFreqs = FALSE,
#'   specFun_pars = list(nFilters = 32), output = 'all')  # learns all 3 rhythms
#' filled.contour(x = as.numeric(colnames(surp$detailed$surprisal_mat)) / 1000,
#'                y = as.numeric(rownames(surp$detailed$surprisal_mat)),
#'                z = t(surp$detailed$surprisal_mat),
#'                xlab = 'Time, s',
#'                ylab = 'Frequency, kHz')
#'
#' ## Example 4: different time scales
#' s8 = soundgen(nSyl = 4, sylLen = 75, pauseLen = 50)
#' s8 = rep(c(s8, rep(0, 2000)), 8)
#' getSurprisal(s8, 16000, specFun = 'env', winSurp = Inf)
#' # ACF picks up first the fast rhythm, then after a few cycles switches to
#' # the slow rhythm
#'
#' # Custom input: produce a nice spectrogram first, then use it as input
#' sp = spectrogram(s0, 16000, windowLength = 10, step = 10, contrast = .3,
#'   output = 'processed')  # return the modified spectrogram
#' colnames(sp) = as.numeric(colnames(sp)) / 1000  # convert ms to s
#' getSurprisal(s0, 16000, specFun = sp, logSpec = FALSE)
#'
#' # Custom input: use acoustic features returned by analyze()
#' an = analyze(sound, 16000, windowLength = 20, novelty = NULL)
#' feature_mat = t(an$detailed[, 4:ncol(an$detailed)]) # or select pitch, HNR, ...
#' feature_mat = t(apply(feature_mat, 1, scale))  # z-transform all variables
#' feature_mat[is.na(feature_mat)] = 0  # get rid of NAs
#' colnames(feature_mat) = an$detailed$time  # time stamps in ms
#' rownames(feature_mat) = 1:nrow(feature_mat)
#' image(t(feature_mat))  # not a spectrogram, just a feature matrix
#' getSurprisal(sound, 16000, specFun = feature_mat, logSpec = FALSE)
#'
#' # analyze all sounds in a folder
#' surp = getSurprisal('~/Downloads/temp/', savePlots = TRUE)
#' surp$summary
#' }
getSurprisal = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    winSurp = 2000,
    specFun = 'audSpec',
    specFun_pars = list(),
    logSpec = TRUE,
    method = c('acf', 'none'),
    sameLagAllFreqs = FALSE,
    weightByAmpl = TRUE,
    weightByPrecision = TRUE,
    onlyPeakAutocor = TRUE,
    rescale = FALSE,
    minProb = 1e-12,
    summaryFun = 'mean',
    output = c('surprisal', 'info', 'kl'),
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    osc = c('linear', 'dB', 'none'),
    heights = c(3, 1),
    ylim = NULL,
    maxPoints = c(1e5, 5e5),
    colorTheme = 'bw',
    col = NULL,
    extraContour = list(col = 'blue', lwd = 3, lty = 1),
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
  # deprecated parameters
  call_arg_names = names(match.call())
  if (any(c('input', 'melfcc_pars', 'audSpec_pars', 'env_pars') %in%
          call_arg_names))
    stop(paste('Changes in soundgen >3.0: "input" is deprecated and replaced by "specFun";',
               '"melfcc_pars", "audSpec_pars", "env_pars" are now passed in "specFun_pars"'))
  rm('call_arg_names')

  # match args
  if (is.character(specFun)) specFun = specFun[1]
  osc = match.arg(osc)
  method = match.arg(method)
  output = match.arg(output, c(
    'surprisal', 'loudness', 'dLoudness', 'surprisalLoudness', 'surprisal_mat',
    'bestLag_mat', 'info', 'info_mat', 'infoW', 'infoW_mat', 'kl', 'kl_mat',
    'klW', 'klW_mat', 'spectrogram', 'all'), several.ok = TRUE)
  if ('all' %in% output) output = c(
    'surprisal', 'loudness', 'dLoudness',  'surprisalLoudness', 'surprisal_mat',
    'bestLag_mat', 'info', 'info_mat', 'infoW', 'infoW_mat', 'kl', 'kl_mat',
    'klW', 'klW_mat', 'spectrogram')

  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'output',
    'reportEvery', 'cores', 'summaryFun', 'savePlots', 'embed')]
  myPars$yScale = NULL  # must be specified via specFun_pars

  # call .getSurprisal
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = .getSurprisal,
    suffix = 'getSurprisal',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  if (pa$input$n == 0) stop('Failed to analyze any input')

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
      if (!pa$input$failed[i]) {
        res = pa$result[[i]]
        temp[[i]] = summarizeAnalyze(
          data.frame(surprisal = res$surprisal,
                     loudness = res$loudness,
                     surprisalLoudness = res$surprisalLoudness,
                     info = res$info,
                     infoW = res$infoW,
                     kl = res$kl,
                     klW = res$klW),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]][1, ]
        filler[1, ] = NA
      } else {
        # All files failed: generate a safe NA filler
        filler = try(summarizeAnalyze(
          data.frame(surprisal = NA, loudness = NA, surprisalLoudness = NA,
                     info = NA, infoW = NA, kl = NA, klW = NA),
          summaryFun = summaryFun,
          var_noSummary = NULL))
        if (inherits(filler, 'try-error')) {
          filler = data.frame(matrix(NA, nrow = 1, ncol = 7 * length(summaryFun)))
        } else {
          filler[1, ] = NA
        }
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = try(cbind(data.frame(file = pa$input$filenames_base),
                          rbind_fill_list(temp)))
    if (inherits(mysum_all, 'try-error')) mysum_all = NULL
  } else {
    mysum_all = NULL
  }

  # prepare detailed output
  detailed = vector('list', pa$input$n)
  names(detailed) = pa$input$filenames_base
  failed_template = as.list(rep(NA, length(output)))
  names(failed_template) = output

  for (i in seq_len(pa$input$n)) {
    if (pa$input$failed[i]) {
      detailed[[i]] = failed_template
    } else {
      detailed[[i]] = pa$result[[i]][output]
    }
  }
  if (pa$input$n == 1) detailed = detailed[[1]]

  invisible(list(
    detailed = detailed,
    summary = mysum_all
  ))
}


#' Get surprisal per sound
#' @noRd
.getSurprisal = function(
    audio,
    winSurp,
    specFun = 'audSpec',
    specFun_pars = list(),
    logSpec = TRUE,
    method = 'acf',
    sameLagAllFreqs = FALSE,
    weightByAmpl = TRUE,
    weightByPrecision = TRUE,
    onlyPeakAutocor = TRUE,
    rescale = FALSE,
    minProb = 1e-12,
    plot = TRUE,
    osc = 'linear',
    heights = c(3, 1),
    ylim = NULL,
    maxPoints = c(1e5, 5e5),
    colorTheme = 'bw',
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
  if (all(audio$sound == 0)) stop('nothing to do: the input is silent')
  maxFreq = audio$samplingRate / 2
  if (!is.finite(winSurp)) winSurp = length(audio$sound) / audio$samplingRate * 1000
  if (is.null(specFun_pars)) specFun_pars = list()
  if (is.character(specFun)) specFun = specFun[1]

  # extract the features to analyze
  if (is.matrix(specFun)) {
    # custom input to getSurprisal() - use as is
    sp = as.matrix(specFun)
    # we need time stamps to be in ms, so let's double-check and convert if need be
    if (is.null(colnames(sp))) {
      colnames(sp) = seq(0, audio$duration * 1000,
                         length.out = ncol(sp)) + audio$timeShift * 1000
    } else {
      cols_num = as.numeric(colnames(sp))
      ran = tail(cols_num, 1)
      if (!is.finite(ran) ||  # weird non-numeric time labels
          ran < (audio$duration * 2))  # probably in s, not ms
        colnames(sp) = cols_num * 1000
    }

  } else {
    # If only one auditory filter is requested, fall back to the envelope
    if (is.character(specFun) &&
        specFun %in% c('audSpec', 'audSpectrogram') &&
        isTRUE(specFun_pars$nFilters == 1)) {
      specFun = 'env'
      if (!is.null(specFun_pars$step))
        specFun_pars$step = round(specFun_pars$step / 1000 * audio$samplingRate)
    }

    # getSurprisal-specific defaults
    if (is.character(specFun)) {
      if (specFun %in% c('audSpec', 'audSpectrogram')) {
        if (is.null(specFun_pars$yScale)) specFun_pars$yScale = 'ERB'
        if (is.null(specFun_pars$nFilters_oct)) specFun_pars$nFilters_oct = 6
        if (is.null(specFun_pars$step)) specFun_pars$step = 15
        if (is.null(specFun_pars$minFreq)) specFun_pars$minFreq = 60
      } else if (specFun == 'spectrogram') {
        if (is.null(specFun_pars$windowLength)) specFun_pars$windowLength = 20
        if (is.null(specFun_pars$step)) specFun_pars$step = 20
      } else if (specFun %in% c('rms', 'RMS', 'getRMS')) {
        if (is.null(specFun_pars$windowLength)) specFun_pars$windowLength = 40
        if (is.null(specFun_pars$step)) specFun_pars$step = 20
      } else if (specFun %in% c('env', 'getEnv')) {
        if (is.null(specFun_pars$wl))
          specFun_pars$wl = round(40 * audio$samplingRate / 1000)
        if (is.null(specFun_pars$step))
          specFun_pars$step = round(20 * audio$samplingRate / 1000)
        if (is.null(specFun_pars$upsample))
          specFun_pars$upsample = FALSE
      } else if (specFun %in% c('melspec', 'mfcc', 'melfcc')) {
        if (is.null(specFun_pars$windowLength)) specFun_pars$windowLength = 20
        if (is.null(specFun_pars$step)) specFun_pars$step = 20
        if (is.null(specFun_pars$nbands)) specFun_pars$nbands = 128
        if (specFun %in% c('mfcc', 'melfcc') &&
            is.null(specFun_pars$MFCC)) {
          specFun_pars$MFCC = 2:13
        }
      } else if (specFun %in% c('powerspec', 'powspec')) {
        # allow ms-style arguments for convenience, but tuneR::powspec uses s
        if (is.null(specFun_pars$wintime) &&
            !is.null(specFun_pars$windowLength)) {
          specFun_pars$wintime = specFun_pars$windowLength / 1000
        }
        if (is.null(specFun_pars$steptime) &&
            !is.null(specFun_pars$step)) {
          specFun_pars$steptime = specFun_pars$step / 1000
        }
        if (is.null(specFun_pars$wintime)) specFun_pars$wintime = 0.02
        if (is.null(specFun_pars$steptime)) specFun_pars$steptime = 0.02
        specFun_pars$windowLength = NULL
        specFun_pars$step = NULL
      }
    }

    sp = getSpec(audio,
                 specFun = specFun,
                 specFun_pars = specFun_pars)
  }

  if (ncol(sp) >= 2) {
    step = diff(as.numeric(colnames(sp)[1:2]))
  } else {
    step = audio$duration * 1000
  }

  if (logSpec) sp = floor_log(sp, dynamicRange = 90)
  # image(t(sp))

  if (length(step) != 1 || !is.finite(step) || step <= 0) {
    stop('Could not determine a valid time step from the input feature matrix')
  }


  # get surprisal
  surprisal_list = getSurprisal_matrix(
    sp,
    win = floor(winSurp / step),
    method = method,
    sameLagAllFreqs = sameLagAllFreqs,
    weightByAmpl = weightByAmpl,
    weightByPrecision = weightByPrecision,
    onlyPeakAutocor = onlyPeakAutocor,
    rescale = rescale,
    minProb = minProb)
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
  max_loud = max(loud, na.rm = TRUE)
  loud_norm = if (max_loud > 0) loud / max_loud else rep(0, length(loud))
  dLoud = diff(c(0, loud_norm))
  dLoud_rect = dLoud
  dLoud_rect[dLoud_rect < 0] = 0
  surprisal_rect = surprisal
  surprisal_rect[surprisal_rect < 0 ] = 0
  surprisalLoudness = surprisal_rect * dLoud_rect # (surprisal + dLoud) / 2
  # surprisalLoudness[surprisalLoudness < 0] = 0
  # surprisalLoudness = sqrt(surprisalLoudness)

  # plotting
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
    if (is.character(specFun) &&
        specFun %in% c('env', 'getEnv', 'rms', 'RMS', 'getRMS')) {
      .osc(audio[which(names(audio) != 'savePlots')], main = main, dB = (osc == 'dB'),
           maxPoints = maxPoints[1], xlab = xlab, ylab = ylab, ...)
      max_abs_surp = max(abs(surprisal), na.rm = TRUE)
      if (is.finite(max_abs_surp) && max_abs_surp > 0) {
        sl_norm = surprisal / max_abs_surp * audio$scale
        time_stamps = seq(0, audio$duration * 1000, length.out = length(sl_norm))
        do.call(points, c(list(x = time_stamps, y = sl_norm, type = 'l'), extraContour))
      }
    } else {
      sl_norm = zeroOne(surprisal) * maxFreq
      # if (is.null(ylim)) ylim = c(0, maxFreq / 1000)
      if (!any(!is.na(sl_norm))) sl_norm = surprisal  # eg if all 0's
      yScale = if (!is.null(specFun_pars$yScale)) specFun_pars$yScale else
        if (isTRUE(specFun == 'melspec')) 'mel' else 'linear'
      plotSpec(
        X = as.numeric(colnames(sp)),  # time
        Y = as.numeric(rownames(sp)),  # freq
        Z = sp, # if (specFun == 'audSpec') t(sp) else (log(t(sp + 1e-6))),
        audio = audio[which(names(audio) != 'savePlots')],
        internal = NULL,
        osc = osc, heights = heights, ylim = ylim,
        yScale = yScale,
        maxPoints = maxPoints, colorTheme = colorTheme, col = col,
        extraContour = c(list(x = sl_norm, warp = FALSE), extraContour),
        xlab = xlab, ylab = ylab, xaxp = xaxp,
        mar = mar, main = main, grid = grid,
        ...
      )
    }
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
#' @param x input matrix such as a spectrogram (columns = time, rows =
#'   frequency)
#' @param win length of analysis window
#' @inheritParams getSurprisal
#' @noRd
getSurprisal_matrix = function(
    x,
    win,
    method = 'acf',
    sameLagAllFreqs = TRUE,
    weightByAmpl = TRUE,
    weightByPrecision = TRUE,
    onlyPeakAutocor = FALSE,
    rescale = FALSE,
    minProb = 1e-12){
  # image(t(x))
  nc = ncol(x)  # time
  nr = nrow(x)  # freq bins
  surprisal = info = infoW = kl = klW = rep(NA, nc)
  surprisal_mat = bestLag_mat = info_mat = infoW_mat = kl_mat = klW_mat =
    weights_mat = matrix(NA, nrow = nr, ncol = nc)
  rownames(surprisal_mat) = rownames(bestLag_mat) = rownames(info_mat) =
    rownames(infoW_mat) = rownames(kl_mat) = rownames(klW_mat) =
    rownames(weights_mat) = rownames(bestLag_mat) = rownames(x)
  colnames(surprisal_mat) = colnames(bestLag_mat) = colnames(info_mat) =
    colnames(infoW_mat) = colnames(kl_mat) = colnames(klW_mat) =
    colnames(weights_mat) =  colnames(bestLag_mat) = colnames(x)
  if (nr == 0 || nc < 2) return(list(
    surprisal = surprisal, surprisal_mat = surprisal_mat, bestLag = bestLag_mat,
    info = info, info_mat = info_mat, infoW = infoW, infoW_mat = infoW_mat,
    kl = kl, kl_mat = kl_mat, klW = klW, klW_mat = klW_mat)
  )

  for (c in 2:nc) {  # for each time point
    idx_i = max(1, c - win + 1):c
    if (length(idx_i) < 3) next
    win_i = x[, idx_i, drop = FALSE]
    len = ncol(win_i)
    win_i_wo_last = win_i[, 1:(len - 1), drop = FALSE]

    # calculate weights based max values per channel, ignoring negative values
    weights = pmax(0, apply(win_i_wo_last, 1, max, na.rm = TRUE))
    weights[!is.finite(weights)] = 0
    sw = sum(weights)
    if (sw != 0) {
      weights = weights / sw
    } else {
      weights = rep(1/nr, nr)
    }
    weights_mat[, c] = weights
    bestLag = NULL

    if (method == 'acf') {
      # by default, we determine bestLag separately for each frequency bin
      if (sameLagAllFreqs) {
        # determine the best lag taking into account the ACFs of all frequency bins
        # extract ACF per bin
        autocor_matrix = matrix(NA, nrow = nr, ncol = len - 2)
        for (r in 1:nr) {  # for each freq bin
          # autocor_matrix[r, ] = as.numeric(acf(
          #   win_i_wo_last[r, ], lag.max = len - 2, plot = FALSE)$acf)[-1]
          # faster method of calculating ACF via FFT
          x_r = win_i_wo_last[r, ]
          acm = try(acf_fft(x_r - mean(x_r)), silent = TRUE)
          if (!inherits(acm, 'try-error') && length(acm) >= len - 1)
            autocor_matrix[r, ] = acm[2:(len - 1)]
        }

        # average the ACFs across frequency bins
        if (weightByAmpl) {
          # weight by max amplitude per bin
          idx_NA = apply(autocor_matrix, 2, function(x) all(is.na(x)))
          autocor = colSums(sweep(autocor_matrix, MARGIN = 1, weights, `*`), na.rm = TRUE)
          autocor[idx_NA] = NA
        } else {
          # just simple mean
          autocor = colMeans(autocor_matrix, na.rm = TRUE)
        }
        # plot(autocor, type = 'b')

        # find the highest peak of average ACF to avoid getting bestLag = 1 all the time
        if (isTRUE(any(autocor != 0))) {
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
    }

    # calculate surprisal per bin as change in ACF at bestLag
    # (the same lag for all frequency bins)
    # pre-calculate half-Gaussian windows to avoid doing it in the loop
    win_x = normHalfGaus(ncol(win_i))
    win_x1 = normHalfGaus(ncol(win_i) - 1)
    for (r in 1:nr) {
      s_r = getSurprisal_vector(
        win_i[r, ], method = method,
        bestLag = bestLag,
        weightByPrecision = weightByPrecision,
        onlyPeakAutocor = onlyPeakAutocor,
        win_x = win_x,
        win_x1 = win_x1,
        minProb = minProb
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
    # weight by the max amplitude of each bin, setting undefined columns to NA
    # (colSums sets them to 0 - so we'd get surprisal = 0 in frames with no data)
    weights_NA = as.integer(which(apply(weights_mat, 2, function(x) all(is.na(x)))))

    surprisal = colSums(surprisal_mat * weights_mat, na.rm = TRUE)
    surp_NA = unique(c(weights_NA,
                       which(apply(surprisal_mat, 2, function(x) all(is.na(x))))
    ))
    surprisal[surp_NA] = NA

    info = colSums(info_mat * weights_mat, na.rm = TRUE)
    info_NA = unique(c(weights_NA,
                       which(apply(info_mat, 2, function(x) all(is.na(x))))
    ))
    info[info_NA] = NA

    infoW = colSums(infoW_mat * weights_mat, na.rm = TRUE)
    infoW_NA = unique(c(weights_NA,
                        which(apply(infoW_mat, 2, function(x) all(is.na(x))))
    ))
    infoW[infoW_NA] = NA

    kl = colSums(kl_mat * weights_mat, na.rm = TRUE)
    kl_NA = unique(c(weights_NA,
                     which(apply(kl_mat, 2, function(x) all(is.na(x))))
    ))
    kl[kl_NA] = NA

    klW = colSums(klW_mat * weights_mat, na.rm = TRUE)
    klW_NA = unique(c(weights_NA,
                      which(apply(klW_mat, 2, function(x) all(is.na(x))))
    ))
    klW[klW_NA] = NA
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
  if (rescale) surprisal = tanh(surprisal / 2)
  # same as: surprisal = 1 - 2 / (exp(surprisal) + 1)
  # a = seq(-5, 5, .02); plot(a, 1 - 2 / (exp(a) + 1), type = 'l')

  list(
    surprisal = surprisal, surprisal_mat = surprisal_mat, bestLag = bestLag_mat,
    info = info, info_mat = info_mat, infoW = infoW, infoW_mat = infoW_mat,
    kl = kl, kl_mat = kl_mat, klW = klW, klW_mat = klW_mat)
}


#' Get surprisal per vector
#'
#' Estimates the unexpectedness or "surprisal" of the last element of input
#' vector.
#' @param x numeric vector representing the time sequence of interest, eg
#'   amplitudes in a frequency bin over multiple STFT frames
#' @param bestLag (only for method = 'acf') if specified, we don't calculate
#'   the ACF but simply compare autocorrelation at bestLag with vs without the
#'   final point
#' @param win_x,win_x1 half-Gaussian windows passed from getSurprisal_matrix() to
#'   avoid recalculating them each time getSurprisal_vector() is called
#' @param minProb minimum probability used to cap Shannon surprisal: \code{info}
#'   and \code{infoW} cannot exceed \code{-log(minProb)} (natural logarithm)
#' @return A list with scalar values: \code{surprisal}, \code{bestLag},
#'   \code{info}, \code{infoW}, \code{kl}, and \code{klW}. Non-finite values
#'   are returned as \code{NA}.
#' @noRd
#' @examples
#' x = c(rep(1, 3), rep(0, 4), rep(1, 3), rep(0, 4), rep(1, 3), 0, 0)
#' soundgen:::getSurprisal_vector(x)
#' soundgen:::getSurprisal_vector(c(x, 1))
#' soundgen:::getSurprisal_vector(c(x, 13))
getSurprisal_vector = function(
    x,
    method = 'acf',
    bestLag = NULL,
    weightByPrecision = TRUE,
    onlyPeakAutocor = FALSE,
    win_x = NULL,
    win_x1 = NULL,
    minProb = 1e-12) {
  out_NA = list(surprisal = NA, bestLag = NA,
                info = NA, infoW = NA, kl = NA, klW = NA)

  # validate input
  if (missing(x) || is.null(x) || !is.numeric(x) || is.object(x)) return(out_NA)
  x = as.numeric(x)
  len = length(x)
  if (len < 2 || any(!is.finite(x))) return(out_NA)
  if (is.null(minProb) || !is.finite(minProb) || minProb <= 0 ||
      minProb > 1) minProb = 1e-12
  infoCap = -log(minProb)
  if (!is.null(bestLag)) bestLag = as.integer(round(bestLag))

  # initialize outputs
  surprisal = info = infoW = kl = klW = NA
  ran_x = diff(range(x))
  if (!is.finite(ran_x)) return(out_NA)
  if (ran_x == 0) return(out_NA)
  # plot(x, type = 'b')
  x1 = x[-len]
  first = x[1]
  last = x[len]
  ran_x1 = diff(range(x1))
  if (!is.finite(ran_x1)) return(out_NA)
  if (ran_x1 == 0) {
    # completely stationary until the analyzed point
    info = infoW = kl = klW = bestLag = surprisal = NA
    if (!onlyPeakAutocor && method != 'none') {
      if (first == 0) {
        surprisal = 1
      } else {
        # stable version of abs(last - first) / (abs(first) + abs(last))
        m = max(abs(first), abs(last))
        if (!is.finite(m) || m == 0) {
          surprisal = 1
        } else {
          surprisal = abs(last / m - first / m) /
            (abs(first / m) + abs(last / m))
          if (is.finite(surprisal)) {
            surprisal = min(1, max(0, surprisal))
          }
        }
      }
    }
  } else {
    ## calculate Shannon information (doesn't depend on len)
    mean_x1 = mean(x1, na.rm = TRUE)
    sd_x1 = sqrt(mean((x1 - mean_x1)^2, na.rm = TRUE))
    # prob_x1 = dnorm(last, mean_x1, sd_x1) / dnorm(mean_x1, mean_x1, sd_x1)
    # info = -log(max(1e-12, prob_x1))
    # identical, but numerically stable and faster:
    z = (last - mean_x1) / sd_x1  # NB: sd_x1 can't be 0 b/c we check ran_x1 == 0
    info = if (is.finite(z)) min(0.5 * z^2, infoCap) else infoCap

    # or add half-Gaussian filter of "forgetfulness"
    if (is.null(win_x1) || length(win_x1) != len - 1 ||
        !is.numeric(win_x1) || is.object(win_x1) ||
        any(!is.finite(win_x1)) || any(win_x1 < 0)) {
      win_x1 = normHalfGaus(len - 1)
    }
    sum_win_x1 = sum(win_x1)
    if (!is.finite(sum_win_x1) || sum_win_x1 <= 0) {
      win_x1 = rep(1 / (len - 1), len - 1)
    } else {
      win_x1 = win_x1 / sum_win_x1
    }
    # plot(win_x1)
    mean_x1w = sum(x1 * win_x1)  # weighted mean
    sd_x1w = sqrt(sum((x1 - mean_x1w)^2 * win_x1))  # weighted SD
    # prob_x1w = dnorm(last, mean_x1w, sd_x1w) / dnorm(mean_x1w, mean_x1w, sd_x1w)
    # infoW = -log(max(1e-12, prob_x1w))
    zW = (last - mean_x1w) / sd_x1w
    infoW = if (is.finite(zW)) min(0.5 * zW^2, infoCap) else infoCap

    ## calculate Kullback-Leibler (KL) divergence between two Gaussian distributions
    # (from rodriguez-hidalgo_2018_bayesian-log-surprise, p. 6, but with log)
    # NB: kl DOES depend on len, so need to add 2 * log(len)
    var_x1 = sd_x1^2
    mean_x = mean(x, na.rm = TRUE)
    var_x = mean((x - mean_x)^2, na.rm = TRUE)
    var_ratio = var_x / var_x1
    kl_arg = (mean_x - mean_x1)^2 / 2 / var_x1 +
      (var_ratio - 1 - log(var_ratio)) / 2
    if (is.finite(kl_arg)) {
      if (kl_arg < minProb) kl_arg = minProb
      kl = log(kl_arg) + 2 * log(len)
    }

    # KL with a half-Gaussian filter of "forgetfulness"
    var_x1w = sd_x1w^2
    if (is.null(win_x) || length(win_x) != len ||
        !is.numeric(win_x) || is.object(win_x) ||
        any(!is.finite(win_x)) || any(win_x < 0)) {
      win_x = normHalfGaus(len)
    }
    sum_win_x = sum(win_x)
    if (!is.finite(sum_win_x) || sum_win_x <= 0) {
      win_x = rep(1 / len, len)
    } else {
      win_x = win_x / sum_win_x
    }
    mean_xw = sum(x * win_x, na.rm = TRUE)  # weighted mean
    var_xw = sum((x - mean_xw)^2 * win_x)  # weighted var
    var_ratioW = var_xw / var_x1w
    klW_arg = (mean_xw - mean_x1w)^2 / 2 / var_x1w +
      (var_ratioW - 1 - log(var_ratioW)) / 2
    if (is.finite(klW_arg)) {
      if (klW_arg < minProb) klW_arg = minProb
      klW = log(klW_arg) + 2 * log(len)
    }

    # calculate surprisal
    if (method == 'acf') {
      if (len > 2) {
        # non-stationary --> autocorrelation
        # center, as in acf()
        x = x - mean_x
        x1 = x1 - mean_x1
        if (is.null(bestLag) || length(bestLag) == 0) {
          # autocor = as.numeric(acf(x1, lag.max = len - 2, plot = FALSE)$acf)[-1]
          # faster method of calculating ACF via FFT
          autocor_full = try(acf_fft(x1), silent = TRUE)
          if (inherits(autocor_full, 'try-error')) {
            autocor_full = NA
          }
          if (length(autocor_full) >= len - 1) {
            autocor = autocor_full[2:(len - 1)]
          } else {
            autocor = rep(NA, len - 2)
          }

          # find the highest peak to avoid getting bestLag = 1 all the time
          # (plateaus are not treated as peaks, the first value is never a peak)
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
        if (length(bestLag) < 1 || is.na(bestLag)) {
          surprisal = NA
        } else {
          best_acf = suppressWarnings(
            cor(c(x1, rep(0, bestLag)), c(rep(0, bestLag), x1))
          )
          if (length(best_acf) != 1 || !is.finite(best_acf)) best_acf = 0

          # check acf at the best lag for the time series with the next point
          # (centered and zero-padded to get exactly the same values of autocor as
          # in acf, but this way we don't need to recalculate the entire ACF for the
          # last point, just a single value)
          best_next_point = suppressWarnings(
            cor(c(x, rep(0, bestLag)), c(rep(0, bestLag), x))
          )
          if (length(best_next_point) != 1 || !is.finite(best_next_point)) best_next_point = 0

          # rescale
          # * len to compensate for diminishing effects of single-point changes on acf
          # as window length increases (matter b/c we compare these values with the
          # stationary ones calculated above w/o acf, simply as abs(last-first)/first)
          # * abs(best_acf) to make a change more surprising if highly regular until now
          if (weightByPrecision) {
            surprisal = (best_acf - best_next_point) * len * abs(best_acf)
          } else {
            surprisal = (best_acf - best_next_point) * len
          }
        }
      } else {
        surprisal = NA
        bestLag = NA
      }
    } else {
      surprisal = bestLag = NA
    }
  }

  if (!is.finite(surprisal)) surprisal = NA
  if (!is.finite(info)) info = NA
  if (!is.finite(infoW)) infoW = NA
  if (!is.finite(kl)) kl = NA
  if (!is.finite(klW)) klW = NA
  list(surprisal = surprisal, bestLag = bestLag,
       info = info, infoW = infoW, kl = kl, klW = klW)
}
