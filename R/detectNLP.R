#' Detect NLP
#'
#' (Experimental) A function for automatically detecting and annotating
#' nonlinear vocal phenomena (NLP). Algorithm: analyze the audio using
#' \code{\link{analyze}} and \code{\link{phasegram}}, then use the extracted
#' frame-by-frame descriptives to classify each frame as having no NLP ("none"),
#' subharmonics ("sh"), sidebands / amplitude modulation ("sb"), or
#' deterministic chaos ("chaos"). The classification is performed by a
#' \code{\link{naiveBayes}} algorithm adapted to autocorrelated time series and
#' pretrained on a manually annotated corpus of vocalizations. Whenever
#' possible, check and correct pitch tracks prior to running the algorithm. See
#' \code{\link{naiveBayes}} for tips on using adaptive priors and clumping
#' to account for the fact that NLP typically occur in continuous segments
#' spanning multiple frames.
#'
#' @inheritParams .roxygen_defaults
#' @param predictors variables to include in NLP classification. If NULL
#'   (default), uses the standard set of predictors if present in the training
#'   corpus: d2, subDep, amEnvDep, amMsPurity, entropyW, entropySh, HNR, CPP,
#'   roughness. If specified, uses all predictors as long as they are available
#'   in the training corpus. NA values are fine (they do not cause the entire
#'   frame to be dropped as long as at least one variable is measured).
#' @param thresProb minimum probability of NLP for the frame to be classified as
#'   non-"none", which is good for reducing false alarms (<1/nClasses means just
#'   go for the highest probability)
#' @param voicelessToNone if TRUE, frames treated as voiceless are set to "none"
#'   (mostly makes sense with manual pitch tracking)
#' @param train training corpus, namely the result of running
#'   \code{\link{naiveBayes_train}} on audio with known NLP episodes. Currently
#'   implemented: soundgen::detectNLP_training_nonv = manually annotated human
#'   nonverbal vocalizations, soundgen::detectNLP_training_synth = synthetic,
#'   soundgen()-generated sounds with various NLP. To train your own, run
#'   \code{detectNLP} on a collection of recordings, provide ground truth
#'   classification of NLP per frame (normally this would be converted from NLP
#'   annotations), and run \code{\link{naiveBayes_train}}.
#' @param pitchManual manually corrected pitch contour. For a single sound,
#'   provide a numeric vector of any length. For multiple sounds, provide a
#'   dataframe with columns "file" and "pitch" (or path to a csv file) as
#'   returned by \code{\link{pitch_app}}, ideally with the same windowLength and
#'   step as in current call to analyze. A named list with pitch vectors per
#'   file is also accepted - e.g., as returned by \code{\link{pitch_app}}
#' @param analyze_pars arguments passed to \code{\link{analyze}}. NB: drop
#'   everything unnecessary to speed up the process, e.g. nFormants = 0,
#'   loudness = NULL, etc. If you have manual pitch contours, pass them as
#'   \code{pitchManual = ...}. Make sure the "silence" threshold is appropriate,
#'   and ideally normalize the audio (silent frames are automatically assigned
#'   to "none")
#' @param phasegram_pars arguments passed to \code{\link{phasegram}}. NB: only
#'   phasegram descriptives requested here are used for NLP detection; other
#'   nonlinear statistics are not calculated to save time.
#' @param naiveBayes_pars arguments passed to \code{\link{naiveBayes}}. It is
#'   strongly recommended to use some clumping, with \code{wlClumper} given as
#'   frames (multiply by \code{step} to get the corresponding minimum duration
#'   of an NLP segment in ms), and/or dynamic priors.
#' @param jumpThres frames in which pitch changes by \code{jumpThres} octaves/s
#'   more than in the surrounding frames are classified as containing "pitch
#'   jumps". Note that this is the rate of frequency change PER SECOND, not from
#'   one frame to the next
#' @param jumpWindow the window for calculating the median pitch slope around
#'   the analyzed frame, ms
#' @param plot if TRUE, produces a spectrogram with annotated NLP regimes
#' @param ylim frequency range to plot, kHz
#' @param main,xlab,ylab,... graphical parameters passed to
#'   \code{\link{spectrogram}}
#'
#' @return A list of datasets, one per input file, with acoustic descriptives
#'   per frame (returned by \code{analyze} and \code{phasegram}), probabilities
#'   of each NLP type per frame, and the putative classification of NLP per
#'   frame. The time step is equal to the larger of the steps passed to
#'   analyze() and phasegram().
#' @export
#' @examples
#'
#' \dontrun{
#' target = soundgen(sylLen = 2000, addSilence = 0, temperature = 1e-2,
#'   pitch = c(380, 550, 500, 220), subDep = c(0, 0, 40, 0, 0, 0, 0, 0),
#'   amDep = c(0, 0, 0, 0, 80, 0, 0, 0), amFreq = 80,
#'   noise = c(-10, rep(-40, 5)),
#'   jitterDep = c(0, 0, 0, 0, 0, 3),
#'   plot = TRUE, play = TRUE)
#'
#' # classifier trained on manually annotated recordings of human nonverbal
#' # vocalizations
#' nlp = detectNLP(target, 16000,
#'   plot = TRUE, ylim = c(0, 4))
#'
#' # classifier trained on synthetic, soundgen()-generated sounds
#' nlp = detectNLP(target, 16000,
#'   train = soundgen::detectNLP_training_synth,
#'   plot = TRUE, ylim = c(0, 4))
#' head(nlp[, c('time', 'pr')])
#' table(nlp$pr)
#' plot(nlp$amEnvDep, type = 'l')
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
#' nlp1 = detectNLP(s1, 16000, plot = TRUE, ylim = c(0, 3))
#' }
detectNLP = function(
    x,
    samplingRate = NULL,
    predictors = NULL,
    thresProb = 0.4,
    voicelessToNone = FALSE,
    train = soundgen::detectNLP_training_nonv,
    scale = NULL,
    from = NULL,
    to = NULL,
    pitchManual = NULL,
    analyze_pars = list(
      windowLength = 50,
      roughness = list(
        msType = '1D',
        specMethod = 'spectrum',
        specFun_pars = list(windowLength = 25, step = 2),
        amRes = NULL, # a single value for the entire sound
        roughRange = NULL,
        roughMean = 100,
        roughSD = 8),
      amRange = c(20, 150)),
    phasegram_pars = list(nonlinStats = 'd2'),
    naiveBayes_pars = list(prior = 'static',
                           wlClumper = 3),
    jumpThres = 14,
    jumpWindow = 100,
    reportEvery = NULL,
    cores = 1,
    plot = FALSE,
    savePlots = FALSE,
    embed = FALSE,
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
  # predictors available in the training corpus
  if (is.data.frame(train)) {
    avail_preds = setdiff(colnames(train), 'nlp')
  } else {
    avail_preds = setdiff(names(train), 'table')
  }

  if (length(avail_preds) == 0) {
    stop('the training corpus contains no usable predictors')
  }

  # standard predictor set, used when predictors = NULL
  preferred_preds = c('amEnvDep', 'amEnvPurity', 'amMsPurity', 'CPP',
                      'entropySh', 'harmEnergy', 'harmHeight', 'HNR', 'subDep')

  if (is.null(predictors)) {
    predictors = intersect(preferred_preds, avail_preds)
    if (length(predictors) == 0) {
      predictors = avail_preds
    }
  } else {
    predictors = match.arg(predictors, choices = avail_preds,
                           several.ok = TRUE)
  }

  if (length(predictors) == 0) {
    stop('no usable predictors were selected')
  }

  myPars = c(as.list(environment()), list(...))
  if (is.null(naiveBayes_pars$prior)) naiveBayes_pars$prior = 'static'
  if (is.null(naiveBayes_pars$wlClumper)) naiveBayes_pars$wlClumper = 3

  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'savePlots', 'embed', 'reportEvery', 'cores', 'summaryFun',
    'pitchManual',
    'avail_preds', 'preferred_preds')]
  if (!is.null(pitchManual))
    myPars$pitchManual_list = formatPitchManual(pitchManual)

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    scale = scale,
                    from = from,
                    to = to,
                    funToCall = '.detectNLP',
                    suffix = 'detectNLP',
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Detect NLP per sound
#'
#' Internal soundgen function called by \code{\link{detectNLP}}.
#' @param audio a list returned by \code{readAudio}
#' @noRd
.detectNLP = function(
    audio,
    predictors = NULL,
    thresProb = 0.4,
    voicelessToNone = FALSE,
    train = soundgen::detectNLP_training_nonv,
    scale = NULL,
    from = NULL,
    to = NULL,
    pitchManual_list = NULL,
    analyze_pars = list(windowLength = 50,
                        roughness = list(specFun_pars = list(windowLength = 15, step = 3)),
                        plot = FALSE),
    phasegram_pars = list(nonlinStats = 'd2'),
    naiveBayes_pars = list(prior = 'static',
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
  # predictors available in the training corpus
  if (is.data.frame(train)) {
    avail_preds = setdiff(colnames(train), 'nlp')
  } else {
    avail_preds = setdiff(names(train), 'table')
  }

  if (length(avail_preds) == 0) {
    stop('the training corpus contains no usable predictors')
  }

  preferred_preds = c('d2', 'subDep', 'amEnvDep', 'amMsPurity',
                      'entropyW', 'HNR', 'CPP', 'roughness')

  if (is.null(predictors)) {
    predictors = intersect(preferred_preds, avail_preds)
    if (length(predictors) == 0) {
      predictors = avail_preds
    }
  } else {
    predictors = predictors[predictors %in% avail_preds]
  }

  if (length(predictors) == 0) {
    stop('no usable predictors were selected')
  }

  # call .analyze
  analyze_pars = analyze_pars[!names(analyze_pars) %in%
                                c('audio', 'from', 'to', 'novelty', 'loudness', 'nFormants',
                                  'pitchManual_list', 'plot')]

  an = do.call(.analyze, c(analyze_pars, list(
    audio = audio, from = from, to = to, novelty = NULL, loudness = NULL,
    nFormants = 0, pitchManual_list = pitchManual_list, plot = FALSE)))

  an_df = an[, which(colnames(an) %in%
                       c('time', 'pitch', predictors)), drop = FALSE]

  # call .phasegram
  if (is.null(phasegram_pars$windowLength)) {
    medPitch = if ('pitch' %in% colnames(an)) {
      median(an$pitch, na.rm = TRUE)
    } else {
      NA
    }

    if (is.finite(medPitch) && medPitch > 0) {
      wl = 5 * 1000 / medPitch
    } else {
      wl = 20
    }

    if (!is.finite(wl) || wl <= 0) wl = 20
    phasegram_pars$windowLength = wl
  }

  extras = predictors[which(predictors %in% c('ed', 'd2', 'ml', 'sur'))]
  if (length(extras) == 0) {
    ph_df = an_df[, FALSE, drop = FALSE]
  } else {
    phasegram_pars$nonlinStats = unique(c(phasegram_pars$nonlinStats, extras))
    phasegram_pars = phasegram_pars[!names(phasegram_pars) %in%
                                      c('audio', 'from', 'to', 'plot')]
    ph = do.call(.phasegram, c(phasegram_pars, list(
      audio = audio[which(names(audio) != 'savePlots')],
      from = from, to = to, plot = FALSE)))
    cols_preds = which(colnames(ph$descriptives) %in% predictors)
    ph_df = ph$descriptives[, cols_preds, drop = FALSE]
  }

  # downsample the longer of an / ph to the same time resolution
  nr_an = nrow(an_df)
  nr_ph = nrow(ph_df)

  if (nr_an == 0 || nr_ph == 0) {
    df = data.frame(time = numeric(0), pitch = numeric(0),
                    pitchJumps = logical(0), pr = character(0))
    return(df)
  }

  if (nr_an > nr_ph) {
    idx_an = round(seq(1, nr_an, length.out = nr_ph))
    idx_an = pmin(pmax(idx_an, 1), nr_an)
    an_df = an_df[idx_an, , drop = FALSE]
  } else if (nr_an < nr_ph) {
    idx_ph = round(seq(1, nr_ph, length.out = nr_an))
    idx_ph = pmin(pmax(idx_ph, 1), nr_ph)
    ph_df = ph_df[idx_ph, , drop = FALSE]
  }

  # merge the two datasets
  df = cbind(an_df, ph_df)

  if (!'pitch' %in% colnames(df)) {
    df$pitch = NA_real_
  }

  preds_in_df = intersect(predictors, colnames(df))
  if (length(preds_in_df) == 0) {
    stop('none of the requested predictors were extracted from this audio')
  }

  # detect pitch jumps
  step = if (nrow(df) >= 2 && 'time' %in% colnames(df)) {
    df$time[2] - df$time[1]
  } else {
    analyze_pars$step
  }

  df$pitchJumps = findJumps(
    pitch = df$pitch, step = step,
    jumpThres = jumpThres, jumpWindow = jumpWindow)

  # call naiveBayes() w/o clumping (done after setting voiceless to "none")
  myf = formula(paste('nlp ~', paste(preds_in_df, collapse = '+')))

  nb_pars = naiveBayes_pars
  nb_pars = nb_pars[!names(nb_pars) %in%
                      c('formula', 'train', 'test', 'wlClumper')]

  nb = do.call(naiveBayes, c(
    nb_pars,
    list(formula = myf, train = train, test = df, wlClumper = 0)))

  df[, c('none', 'sb', 'sh', 'chaos', 'pr')] =
    nb[, c('nlpnone', 'nlpsb', 'nlpsh', 'nlpchaos', 'pr')]

  # invalid or missing predictions are treated as no NLP
  df$pr = as.character(df$pr)
  df$pr[is.na(df$pr)] = 'none'

  # set frames with sub-threshold NLP probability to "none"
  nlp_cols = c('sb', 'sh', 'chaos')
  max_nlp_prob = suppressWarnings(
    apply(df[, nlp_cols], 1, max, na.rm = TRUE))
  max_nlp_prob[!is.finite(max_nlp_prob)] = 0

  if (!is.null(thresProb) && is.finite(thresProb)) {
    df$pr[which(max_nlp_prob < thresProb)] = 'none'
  }

  # same for very quiet frames and voiceless frames
  an_cols = intersect(c('ampl_noSilence', 'voiced'), colnames(an))

  if (nr_an != nrow(df)) {
    if (length(an_cols) > 0 && nrow(an) > 0) {
      idx_an2 = round(seq(1, nrow(an), length.out = nrow(df)))
      idx_an2 = pmin(pmax(idx_an2, 1), nrow(an))
      an = an[idx_an2, an_cols, drop = FALSE]
    }
  } else {
    if (length(an_cols) > 0) {
      an = an[, an_cols, drop = FALSE]
    }
  }

  if ('ampl_noSilence' %in% colnames(an)) {
    df$pr[which(is.na(an$ampl_noSilence))] = 'none'
  }

  if (voicelessToNone && 'voiced' %in% colnames(an)) {
    df$pr[which(!an$voiced)] = 'none'
  }

  df$pr[is.na(df$pr)] = 'none'

  # run clumper()
  wlClumper = naiveBayes_pars$wlClumper
  if (!is.null(wlClumper) && length(wlClumper) == 1 &&
      is.finite(wlClumper) && wlClumper > 1 && nrow(df) > 0) {
    df$pr = clumper(df$pr, minLength = wlClumper)
    df$pr = as.character(df$pr)
    df$pr[is.na(df$pr)] = 'none'
  }

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

    cols = list(none = rgb(1, 1, 1, 0),
                chaos = rgb(1, 0, 0, .5),
                sb = rgb(0, 0, 1, .5),
                sh = rgb(0, 1, 0, .5))

    if (!is.finite(step) || step <= 0) step = 0
    halfstep_s = step / 2 / 1000
    time_s = df$time / 1000
    pr_str = as.character(df$pr)
    pr_str[is.na(pr_str)] = 'none'

    if (is.null(analyze_pars$windowLength) ||
        !is.finite(analyze_pars$windowLength) ||
        analyze_pars$windowLength <= 0) {
      analyze_pars$windowLength = 50
    }

    .spectrogram(audio[which(!names(audio) == 'savePlots')], osc = 'none',
                 windowLength = analyze_pars$windowLength, plot = TRUE,
                 main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
    points(time_s, df$pitch / 1000, type = 'l', col = 'blue')

    if (!is.null(ylim) && length(ylim) >= 2 && is.finite(ylim[2])) {
      rect_top = ylim[2] / 10
    } else {
      rect_top = audio$samplingRate / 2 / 1000 / 10
    }
    if (!is.finite(rect_top) || rect_top <= 0) rect_top = 1

    # plot pitch jumps
    idx_pj = which(df$pitchJumps)
    if (length(idx_pj) > 0) {
      for (i in idx_pj) {
        if (is.finite(df$pitch[i])) {
          arrows(x0 = time_s[i] - halfstep_s, x1 = time_s[i] - halfstep_s,
                 y0 = 0, y1 = df$pitch[i] / 1000, lwd = 2, length = .05)
        }
      }
    }

    # plot other NLP
    idx_nlp = which(pr_str != 'none')
    if (length(idx_nlp) > 0) {
      for (i in idx_nlp) {
        if (!pr_str[i] %in% names(cols)) next

        rect(xleft = time_s[i] - halfstep_s,
             xright = time_s[i] + halfstep_s,
             ybottom = 0, ytop = rect_top, border = NA,
             col = cols[[pr_str[i]]])

        if ((i == 1 || pr_str[i] != pr_str[i - 1]) && pr_str[i] != 'none') {
          text(x = time_s[i], y = rect_top / 2, labels = pr_str[i],
               cex = 1.5, adj = 0)
        }
      }
    }
  }

  return(df)
}


#' Find frequency jumps
#'
#' This function flags frames with apparent pitch jumps (frequency jumps, voice
#' breaks), defined as relatively large and sudden changes in voice pitch or
#' some other frequency measure (peak frequency, a formant frequency, etc). It
#' is called by \code{\link{detectNLP}}. Algorithm: a frame is considered to
#' contain a frequency jump if the absolute slope at this frame exceeds the
#' median slope over ±\code{jumpWindow} around it by more than
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
#' @return A boolean vector of the same length as \code{pitch}, where TRUE
#'   values correspond to frames with detected pitch jumps.
#'
#' @noRd
#' @examples
#' pitch = soundgen:::getSmoothContour(anchors = list(
#'   time = c(0, 350, 351, 890, 891, 1200),
#'   value = c(140, 230, 460, 330, 220, 200)), len = 40, interpol = 'approx')
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
#' pitch = soundgen:::getSmoothContour(anchors = list(
#'   time = c(0, 350, 370, 700),
#'   value = c(340, 710, 850, 1200)), len = 20, interpol = 'approx')
#' findJumps(pitch, step, plot = TRUE)
#' diff(HzToOther(pitch, 'semitones')) * (1000 / step) / 12
#' # the slope at frame 10 (11 oct/s) exceeds the jumpThres (8 oct/s), but not
#' # 11 minus the median slope around frame 10 (~4 oct/s, so 11 - 4 < 8)
findJumps = function(pitch,
                     step,
                     jumpThres = 8,
                     jumpWindow = 80,
                     plot = FALSE,
                     xlab = 'Time, ms',
                     ylab = 'f0, Hz',
                     ...) {
  nFrames = length(pitch)

  if (nFrames < 2) {
    return(rep(FALSE, nFrames))
  }

  if (!is.finite(step) || step <= 0) {
    return(rep(FALSE, nFrames))
  }

  if (!is.finite(jumpWindow) || jumpWindow <= 0) {
    jumpWindow = 0
  }

  wl_frames = max(1, round(jumpWindow / step))  # slope averaged over ... frames
  thres_per_frame = jumpThres * step / 1000     # convert thres from oct/s to oct/frame

  if (!is.finite(thres_per_frame) || thres_per_frame <= 0) {
    return(rep(FALSE, nFrames))
  }

  pitch_oct = log2(pitch)
  diff_pitch = c(0, diff(pitch_oct))
  relativeSlope = rep(0, nFrames)

  # calculate slope, median slope around each frame, and their difference
  for (i in 2:nFrames) {
    if (is.na(pitch_oct[i]) || is.na(pitch_oct[i - 1])) next

    idx_left = max(1, i - wl_frames):(i - 1)

    if (i < nFrames) {
      idx_right = (i + 1):min(nFrames, i + wl_frames)
    } else {
      idx_right = integer(0)
    }

    slope_around = c(diff_pitch[idx_left], diff_pitch[idx_right])
    if (length(slope_around) == 0) next

    # NB: no na.rm = TRUE to ensure the surrounding areas must be voiced,
    # otherwise doesn't count as a jump
    med_slope = median(slope_around)
    if (is.na(med_slope)) next

    slope_i = pitch_oct[i] - pitch_oct[i - 1]
    if (is.na(slope_i)) next

    rel_i = abs(slope_i - med_slope)
    if (!is.finite(rel_i)) next

    relativeSlope[i] = rel_i
  }

  pitchJumps = rep(FALSE, nFrames)
  idx_valid = which(!is.na(relativeSlope))
  pitchJumps[idx_valid] = relativeSlope[idx_valid] > thres_per_frame

  # avoid two consecutive pitch jumps
  for (i in 2:nFrames) {
    if (pitchJumps[i] && pitchJumps[i - 1]) {
      if (relativeSlope[i] > relativeSlope[i - 1]) {
        pitchJumps[i - 1] = FALSE
      } else {
        pitchJumps[i] = FALSE
      }
    }
  }

  if (plot) {
    plot(step * (seq_along(pitch)), pitch, type = 'b',
         pch = 16, xlab = xlab, ylab = ylab)
    pj_idx = which(pitchJumps)
    pj_times = pj_idx * step - step / 2

    if (length(pj_idx) > 0) {
      for (i in seq_along(pj_idx)) {
        if (is.finite(pitch[pj_idx[i]])) {
          arrows(x0 = pj_times[i], x1 = pj_times[i],
                 y0 = 0, y1 = pitch[pj_idx[i]],
                 lwd = 2, length = .05, col = 'blue')
        }
      }
    }
  }

  return(pitchJumps)
}
