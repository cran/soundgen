#' Detect NLP
#'
#' (Experimental) A function for automatically detecting and annotating
#' nonlinear vocal phenomena (NLP). Algorithm: analyze the audio using
#' \code{\link{analyze}} and \code{\link{phasegram}}, then use the extracted
#' frame-by-frame descriptives to classify each frame as having no NLP ("none"),
#' subharmonics ("sh"), sibebands / amplitude modulation ("sb"), or
#' deterministic chaos ("chaos"). The classification is performed by a
#' \code{\link{naiveBayes}} algorithm adapted to autocorrelated time series and
#' pretrained on a manually annotated corpus of vocalizations. Whenever
#' possible, check and correct pitch tracks prior to running the algorithm. See
#' \code{\link{naiveBayes}} for tips on using adaptive priors and "clumpering"
#' to account for the fact that NLP typically occur in continuous segments
#' spanning multiple frames.
#'
#' @return Returns a dataframe with frame-by-frame descriptives, posterior
#'   probabilities of each NLP type per frame, and the tentative classification
#'   (the NLP type with the highest posterior probability, possibly corrected by
#'   clumpering). The time step is equal to the larger of the steps passed to
#'   analyze() and phasegram().
#'
#' @inheritParams analyze
#' @inheritParams findJumps
#' @param predictors variables to include in NLP classification. The default is
#'   to include all 7 variables in the training corpus. NA values are fine (they
#'   do not cause the entire frame to be dropped as long as at least one
#'   variable is measured).
#' @param thresProb minimum probability of NLP for the frame to be classified as
#'   non-"none", which is good for reducing false alarms (<1/nClasses means just
#'   go for the highest probability)
#' @param unvoicedToNone if TRUE, frames treated as unvoiced are set to "none"
#'   (mostly makes sense with manual pitch tracking)
#' @param train training corpus, namely the result of running
#'   \code{\link{naiveBayes_train}} on audio with known NLP episodes. Currently
#'   implemented: soundgen::detectNLP_training_nonv = manually annotated human
#'   nonverbal vocalizations, soundgen::detectNLP_training_synth = synthetic,
#'   soundgen()-generated sounds with various NLP. To train your own, run
#'   \code{detectNLP} on a collection of recordings, provide ground truth
#'   classification of NLP per frame (normally this would be converted from NLP
#'   annotations), and run \code{\link{naiveBayes_train}}.
#' @param pars_analyze arguments passed to \code{\link{analyze}}. NB: drop
#'   everything unnecessary to speed up the process, e.g. nFormants = 0,
#'   loudness = NULL, etc. If you have manual pitch contours, pass them as
#'   \code{pitchManual = ...}. Make sure the "silence" threshold is appropriate,
#'   and ideally normalize the audio (silent frames are automatically assigned
#'   to "none")
#' @param pars_phasegram arguments passed to \code{\link{phasegram}}. NB: only
#'   \code{d2} and {nPeaks} are used for NLP detection because they proved
#'   effective in the training corpus; other nonlinear statistics are not
#'   calculated to save time.
#' @param pars_naiveBayes arguments passed to \code{\link{naiveBayes}}. It is
#'   strongly recommended to use some clumpering, with \code{wlClumper} given as
#'   frames (multiple by \code{step} to get the corresponding minumum duration
#'   of an NLP segment in ms), and/or dynamic priors.
#' @param plot if TRUE, produces a spectrogram with annotated NLP regimes
#' @param main,xlab,ylab,... graphical parameters passed to
#'   \code{\link{spectrogram}}
#'
#' @return Returns a list of datasets, one per input file, with acoustic
#'   descriptives per frame (returned by \code{analyze} and \code{phasegram}),
#'   probabilities of each NLP type per frame, and the putative classification
#'   of NLP per frame.
#' @export
#' @examples
#'
#' \dontrun{
#' target = soundgen(sylLen = 1600, addSilence = 0, temperature = 1e-6,
#'   pitch = c(380, 550, 500, 220), subDep = c(0, 0, 40, 0, 0, 0, 0, 0),
#'   amDep = c(0, 0, 0, 0, 80, 0, 0, 0), amFreq = 80,
#'   noise = c(-10, rep(-40, 5)),
#'   jitterDep = c(0, 0, 0, 0, 0, 3))
#'
#' # classifier trained on manually annotated recordings of human nonverbal
#' # vocalizations
#' nlp = detectNLP(target, 16000, plot = TRUE, ylim = c(0, 4))
#'
#' # classifier trained on synthetic, soundgen()-generated sounds
#' nlp = detectNLP(target, 16000, train = soundgen::detectNLP_training_synth,
#'                 plot = TRUE, ylim = c(0, 4))
#' head(nlp[, c('time', 'pr')])
#' table(nlp$pr)
#' plot(nlp$amEnvDep, type = 'l')
#' plot(nlp$subDep, type = 'l')
#' plot(nlp$entropy, type = 'l')
#' plot(nlp$none, type = 'l')
#' points(nlp$sb, type = 'l', col = 'blue')
#' points(nlp$sh, type = 'l', col = 'green')
#' points(nlp$chaos, type = 'l', col = 'red')
#'
#' # detection of pitch jumps
#' s1 = soundgen(sylLen = 1200, temperature = .001, pitch = list(
#'   time = c(0, 350, 351, 890, 891, 1200),
#'   value = c(140, 230, 460, 330, 220, 200)))
#' playme(s1, 16000)
#' detectNLP(s1, 16000, plot = TRUE, ylim = c(0, 3))
#'
#' # process all files in a folder
#' nlp = detectNLP('/home/allgoodguys/Downloads/temp260/',
#'   pitchManual = soundgen::pitchContour, cores = 4, plot = TRUE,
#'   savePlots = '', ylim = c(0, 3))
#' }
detectNLP = function(
    x,
    samplingRate = NULL,
    predictors = c('nPeaks', 'd2', 'subDep', 'amEnvDep',
                   'entropy', 'HNR', 'CPP', 'roughness'),
    thresProb = 0.4,
    unvoicedToNone = FALSE,
    train = soundgen::detectNLP_training_nonv,
    scale = NULL,
    from = NULL,
    to = NULL,
    pitchManual = NULL,
    pars_analyze = list(windowLength = 50,
                        roughness = list(windowLength = 15, step = 3)),
    pars_phasegram = list(nonlinStats = 'd2'),
    pars_naiveBayes = list(prior = 'static',
                           wlClumper = 3),
    jumpThres = 14,
    jumpWindow = 100,
    reportEvery = NULL,
    cores = 1,
    plot = FALSE,
    savePlots = NULL,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # match args
  myPars = c(as.list(environment()), list(...))
  if (is.null(pars_naiveBayes$prior)) pars_naiveBayes$prior = 'static'
  if (is.null(pars_naiveBayes$wlClumper)) pars_naiveBayes$wlClumper = 3
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'savePlots', 'reportEvery', 'cores', 'summaryFun', 'pitchManual',
    'pars_analyze', 'pars_phasegram', 'pars_naiveBayes')]
  if (!is.null(pitchManual))
    myPars$pitchManual_list = formatPitchManual(pitchManual)
  myPars$pars_analyze = pars_analyze
  myPars$pars_phasegram = pars_phasegram
  myPars$pars_naiveBayes = pars_naiveBayes

  avail_preds = names(train)
  avail_preds = avail_preds[1:(length(avail_preds) - 1)]
  bad_preds = predictors[which(!predictors %in% avail_preds)]
  if (length(bad_preds) > 0) {
    predictors = predictors[which(predictors %in% avail_preds)]
    warning(paste(
      'Predictors {', paste(bad_preds, collapse = ', '),
      '} are not available in the training corpus.',
      'The available predictors are {',
      paste(avail_preds, collapse = ', '), '}'))
  }

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    scale = scale,
                    from = from,
                    to = to,
                    funToCall = '.detectNLP',
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "nlp", width = paste0(width, units)))
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Detect NLP per sound
#'
#' Internal soundgen function called by \code{\link{detectNLP}}.
#' @param audio a list returned by \code{readAudio}
#' @inheritParams getRMS
#' @keywords internal
.detectNLP = function(
    audio,
    predictors = c('nPeaks', 'd2', 'subDep', 'amEnvDep',
                   'entropy', 'HNR', 'CPP', 'roughness'),
    thresProb = 0.4,
    unvoicedToNone = FALSE,
    train = soundgen::detectNLP_training_nonv,
    scale = NULL,
    from = NULL,
    to = NULL,
    pitchManual_list = NULL,
    pars_analyze = list(windowLength = 50,
                        roughness = list(windowLength = 15, step = 3),
                        plot = FALSE),
    pars_phasegram = list(nonlinStats = 'd2'),
    pars_naiveBayes = list(prior = 'static',
                           wlClumper = 3),
    jumpThres = 14,
    jumpWindow = 100,
    plot = FALSE,
    savePlots = NULL,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    type = 'b',
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  an = do.call(.analyze, c(pars_analyze, list(
    audio = audio[which(names(audio) != 'savePlots')],
    from = from, to = to, novelty = NULL, loudness = NULL,
    nFormants = 0, pitchManual_list = pitchManual_list, plot = FALSE)))
  an_df = an[, which(colnames(an) %in%
                       c('time', 'pitch', predictors)), drop = FALSE]

  # call .phasegram
  if (is.null(pars_phasegram$windowLength)) {
    # take 5 * typical f0 period
    mp = 1000 / median(an$pitch, na.rm = TRUE)
    if (is.finite(mp)) {
      pars_phasegram$windowLength = 5 * mp
    } else {
      pars_phasegram$windowLength = 20
    }
  }
  extras = predictors[which(predictors %in% c('ed', 'd2', 'ml', 'sur'))]
  pars_phasegram$nonlinStats = unique(c(pars_phasegram$nonlinStats, extras))
  ph = do.call(.phasegram, c(pars_phasegram, list(
    audio = audio[which(names(audio) != 'savePlots')],
    from = from, to = to, plot = FALSE)))
  cols_preds = which(colnames(ph$descriptives) %in% predictors)
  ph_df = ph$descriptives[, cols_preds, drop = FALSE]

  # downsample the longer of an / ph to the same time resolution
  nr_an = nrow(an_df)
  nr_ph = nrow(ph_df)
  if (nr_an > nr_ph) {
    an_df = an_df[seq(1, nr_an, length.out = nr_ph), , drop = FALSE]
  } else if (nr_an < nr_ph) {
    ph_df = ph_df[seq(1, nr_ph, length.out = nr_an), , drop = FALSE]
  }

  # merge the two datasets
  df = cbind(an_df, ph_df)

  # detect pitch jumps
  if (!is.null(pars_analyze$step)) {
    step = pars_analyze$step
  } else {
    step = df$time[2] - df$time[1]
  }
  df$pitchJumps = findJumps(
    pitch = df$pitch, step = step,
    jumpThres = jumpThres, jumpWindow = jumpWindow)

  # # drop variables with nothing but NAs
  # all_na = which(apply(df, 2, function(x) !any(!is.na(x))))
  # if (length(all_na) > 0) df = df[, -as.numeric(all_na)]

  # call naiveBayes() w/o clumpering (done after setting unvoiced to "none")
  myf = formula(paste('nlp ~', paste(predictors, collapse = '+')))
  nb = do.call(naiveBayes, c(
    pars_naiveBayes[which(names(pars_naiveBayes) != 'wlClumper')],
    list(formula = myf, train = train, test = df, wlClumper = 0)))
  df[, c('none', 'sb', 'sh', 'chaos', 'pr')] =
    nb[, c('nlpnone', 'nlpsb', 'nlpsh', 'nlpchaos', 'pr')]

  # set frames with sub-threshold NLP probability to "none"
  max_nlp_prob = apply(df[, c('sb', 'sh', 'chaos')], 1, max)
  df$pr[which(max_nlp_prob < thresProb)] = 'none'
  # same for very quiet frames and unvoiced frames
  if (nr_an != nrow(df))
    an = an[seq(1, nrow(an), length.out = nrow(df)),
            c('ampl_noSilence', 'voiced')]
  df$pr[which(is.na(an$ampl_noSilence))] = 'none'
  if (unvoicedToNone)
    df$pr[which(!an$voiced)] = 'none'
  # run clumper()
  if (!is.null(pars_naiveBayes$wlClumper) && pars_naiveBayes$wlClumper > 1)
    df$pr = clumper(df$pr, minLength = pars_naiveBayes$wlClumper)

  # plotting
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_nlp.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    if (is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }
    cols = list(none = rgb(1, 1, 1, 0),
                chaos = rgb(1, 0, 0, .5),
                sb = rgb(0, 0, 1, .5),
                sh = rgb(0, 1, 0, .5))
    halfstep_s = step / 2 / 1000
    time_s = df$time / 1000
    pr_str = as.character(df$pr)
    if (is.null(pars_analyze$windowLength))
      pars_analyze$windowLength = 50
    .spectrogram(audio[which(names(audio) != 'savePlots')], osc = FALSE,
                 windowLength = pars_analyze$windowLength,
                 main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
    points(time_s, df$pitch / 1000, type = 'l', col = 'blue')
    if (!is.null(ylim)) {
      rect_top = ylim[2] / 10
    } else {
      rect_top = audio$samplingRate / 2 / 1000 / 10
    }

    # plot pitch jumps
    idx_pj = which(df$pitchJumps)
    if (length(idx_pj) > 0) {
      for (i in idx_pj) {
        arrows(x0 = time_s[i] - halfstep_s, x1 = time_s[i] - halfstep_s,
               y0 = 0, y1 = df$pitch[i] / 1000, lwd = 2, length = .05)
      }
    }

    # plot other NLP
    idx_nlp = which(pr_str != 'none')
    if (length(idx_nlp) > 0) {
      for (i in idx_nlp) {
        rect(xleft = time_s[i] - halfstep_s,
             xright = time_s[i] + halfstep_s,
             ybottom = 0, ytop = rect_top, border = NA, col = cols[[pr_str[i]]])
        if ((i == 1 || pr_str[i] != pr_str[i - 1]) & pr_str[i] != 'none')
          text(x = time_s[i], y = rect_top/2, labels = pr_str[i], cex = 1.5, adj = 0)
      }
    }
    if (is.character(audio$savePlots)) dev.off()
  }
  return(df)
}


#' Find frequency jumps
#'
#' This function flags frames with apparent pith jumps (frequency jumps, voice
#' breaks), defined as relatively large and sudden changes in voice pitch or
#' some other frequency measure (peak frequency, a formant frequency, etc). It
#' is called by \code{\link{detectNLP}}. Algorithm: a frame is considered to
#' contain a frequency jump if the absolute slope at this frame exceeds the
#' average slope over ±\code{jumpWindow} around it by more than
#' \code{jumpThres}. Note that the slope is considered per second rather than
#' per time step - that is, taking into account the sampling rate of the
#' frequency track. Thus, it's not just the change from frame to frame that
#' defines what is considered a jump, but a change that differs from the trend
#' in the surrounding frames (see examples). If several consecutive frames
#' contain apparent jumps, only the greatest of them is preserved.
#'
#' @param pitch vector of frequencies per frame, Hz
#' @param step time step between frames, ms
#' @param jumpThres frames in which pitch changes by \code{jumpThres} octaves/s
#'   more than in the surrounding frames are classified as containing "pitch
#'   jumps". Note that this is the rate of frequency change PER SECOND, not from
#'   one frame to the next
#' @param jumpWindow the window for calculating the median pitch slope around
#'   the analyzed frame, ms
#' @param plot if TRUE, plots the pitch contour with putative frequency jumps
#'   marked by arrows
#' @param xlab,ylab,... graphical parameters passed to \code{plot}
#'
#' @return Returns a boolean vector of the same length as \code{pitch}, where
#'   TRUE values correspond to frames with detected pitch jumps.
#'
#' @export
#' @examples
#' pitch = getSmoothContour(anchors = list(
#'   time = c(0, 350, 351, 890, 891, 1200),
#'   value = c(140, 230, 460, 330, 220, 200)), len = 40)
#' step = 25
#' pj = findJumps(pitch, step, plot = TRUE)
#'
#' # convert frame indices to time in ms
#' step = 25
#' which(pj) * step
#' # or consider pj's to occur midway between the two frames
#' which(pj) * step - step / 2
#'
#' # even very rapid changes are not considered jumps if they match
#' # the surrounding trend
#' pitch = getSmoothContour(anchors = list(
#'   time = c(0, 350, 351, 700),
#'   value = c(340, 710, 850, 1200)), len = 20)
#' findJumps(pitch, step, plot = TRUE)
#' diff(HzToSemitones(pitch)) * (1000 / step) / 12
#' # the slope at frame 10 (10.4 oct/s) exceeds the jumpThres (8 oct/s), but not
#' # 10.4 minus the average slope around frame 10 (~3 oct/s, so 10 - 3 < 8)
findJumps = function(pitch,
                     step,
                     jumpThres = 8,
                     jumpWindow = 80,
                     plot = FALSE,
                     xlab = 'Time, ms',
                     ylab = 'f0, Hz',
                     ...) {
  wl_frames = round(jumpWindow / step)  # slope averaged over ... frames
  thres_per_frame = jumpThres * step / 1000  # convert thres from st/s to st/frame
  pitch_oct = log2(pitch)
  nFrames = length(pitch)

  # calculate slope, average slope around each frame, and their difference
  relativeSlope = rep(NA, nFrames)
  diff_pitch = c(NA, diff(pitch_oct))
  for (i in 2:nFrames) {
    if (!is.na(pitch_oct[i])) {
      # mean absolute pitch slope around frame i
      idx_left = max(1, i - wl_frames) : (i - 1)
      idx_right = (i + 1) : (min(nFrames, i + wl_frames))
      slope_around = c(diff_pitch[idx_left], diff_pitch[idx_right])
      slope_i = pitch_oct[i] - pitch_oct[i - 1]
      relativeSlope[i] = abs(slope_i - median(slope_around))
      # NB: no na.rm = TRUE to ensure the surrounding areas must be voiced,
      # otherwise doesn't count as a jump
    }
  }
  # plot(pitch, type = 'b')
  # plot(relativeSlope, type = 'b')
  pitchJumps = (abs(relativeSlope) > thres_per_frame)
  pitchJumps[is.na(pitchJumps)] = FALSE

  # avoid two consecutive pitch jumps
  for (i in 2:nFrames) {
    if (pitchJumps[i] & pitchJumps[i - 1]) {
      if (relativeSlope[i] > relativeSlope[i - 1]) {
        pitchJumps[i - 1] = FALSE
      } else {
        pitchJumps[i] = FALSE
      }
    }
  }

  if (plot) {
    plot(step * (1:length(pitch)), pitch, type = 'b',
         pch = 16, xlab = xlab, ylab = ylab)
    pj_idx = which(pitchJumps)
    pj_times = pj_idx * step - step / 2
    if (length(pj_idx) > 0) {
      for (i in 1:length(pj_idx))
        arrows(x0 = pj_times[i], x1 = pj_times[i],
               y0 = 0, y1 = pitch[pj_idx[i]],
               lwd = 2, length = .05, col = 'blue')
    }
  }
  return(pitchJumps)
}
