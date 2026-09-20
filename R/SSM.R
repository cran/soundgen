#' Self-similarity matrix
#'
#' Calculates the self-similarity matrix and novelty vector of a sound. The
#' self-similarity matrix is produced by comparing all pairs of frames in
#' the input sound. Novelty is calculated by convolving the self-similarity
#' matrix with a tapered checkerboard kernel. The positive lobes of the kernel
#' represent coherence (self-similarity within the regions on either side of the
#' center point) and the negative lobes anti-coherence (cross-similarity
#' between these two regions). Since novelty is the dot product of the
#' checkerboard kernel with the SSM, it is high when the two regions are
#' self-similar (internally consistent) but different from each other.
#'
#' @seealso \code{\link{spectrogram}} \code{\link{modulationSpectrum}}
#'   \code{\link{segment}}
#'
#' @references \itemize{
#'   \item Foote, J. (1999, October). Visualizing music and
#'   audio using self-similarity. In Proceedings of the seventh ACM
#'   international conference on Multimedia (Part 1) (pp. 77-80). ACM.
#'   \item
#'   Foote, J. (2000). Automatic audio segmentation using a measure of audio
#'   novelty. In Multimedia and Expo, 2000. ICME 2000. 2000 IEEE International
#'   Conference on (Vol. 1, pp. 452-455). IEEE.
#' }
#'
#' @inheritParams .roxygen_defaults
#' @param specFun the function used to extract a spectrogram-like feature matrix.
#'   Can be a string or a custom function that takes audio (numeric vector) as the
#'   first argument and returns a spectrogram-like matrix with time in columns
#'   and features in rows (see examples). A precomputed matrix is also accepted
#'   (features in rows, time [ms] in columns, numeric rownames for plotting).
#'   Supported strings:
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
#'       \code{'getEnv'} / \code{'env'} ("various smoothed envelopes: RMS,
#'       analytical, peak, etc.). Parameters in \code{specFun_pars}: see
#'       \code{\link{getEnv}}.
#'     }
#' }
#' @param specFun_pars a list of parameters passed to \code{specFun}. Defaults
#'   for \code{specFun = "melspec": list(windowLength = 125, step = 25, nbands =
#'   50)}; defaults for \code{specFun = "audSpec": list(nFilters = 16, step =
#'   10)}
#' @param logSpec if TRUE, the input is log-transformed prior to calculating
#'   self-similarity
#' @param normalize if TRUE, the spectrum of each STFT frame (or each column of
#'   feature matrix) is normalized to the same range prior to calculating
#'   self-similarity
#' @param simil method for comparing frames: "cosine" = cosine similarity, "cor"
#'   = Pearson's correlation
#' @param kernelLen length of checkerboard kernel for calculating novelty, ms
#'   (larger values favor global, slow vs. local, fast novelty)
#' @param kernelSD SD of checkerboard kernel evaluated over [-1, 1]: for
#'   ex., if kernelSD = 0.5, the kernel spans approximately ±2 SDs
#' @param padWith how to treat edges when calculating novelty: NA = pad with NA
#'   (ignores edges in correlation), 0 = pad with zeros
#' @param ssmWin window for averaging SSM, frames (has a smoothing effect and
#'   speeds up the processing)
#' @param output what to include in \code{$detailed} (drop "ssm" to save memory
#'   when analyzing a lot of files); options: 'ssm', 'novelty', 'all'
#' @param plot if TRUE, plots the SSM
#' @param heights relative sizes of the SSM and spectrogram/novelty plot
#' @param main plot title
#' @param specPars graphical parameters passed to \code{filled.contour.mod} and
#'   affecting the \code{\link{spectrogram}}
#' @param ssmPars graphical parameters passed to \code{filled.contour.mod} and
#'   affecting the plot of SSM
#' @param noveltyPars graphical parameters passed to
#'   \code{\link[graphics]{lines}} and affecting the novelty contour
#' @param summaryFun functions used to summarize novelty, eg \code{c('mean',
#'   'sd')}; user-defined functions are fine (see examples); NAs are omitted
#'   automatically for mean/median/sd/min/max/range/sum, otherwise take care of
#'   NAs yourself
#'
#' @return A list with two top-level elements: \code{$detailed} and
#'   \code{$summary}.
#'
#'   \code{$detailed} contains per-file results selected with the \code{output}
#'   argument. If multiple sounds are analyzed, \code{$detailed} is a named list
#'   of per-sound lists. If a single sound is analyzed, it is simplified to a
#'   single list. Each list may contain:
#'   \describe{
#'     \item{ssm}{self-similarity matrix}
#'     \item{novelty}{novelty vector}
#'   }
#'
#'   \code{$summary} is a dataframe with summaries of novelty per sound (one
#'   row per file), or \code{NULL} if \code{summaryFun = NULL}.
#'
#' @export
#' @examples
#' sound = c(soundgen(),
#'           soundgen(nSyl = 4, sylLen = 50, pauseLen = 70,
#'           formants = NA, pitch = c(500, 330)))
#' # playme(sound)
#' # detailed, local features (captures each syllable)
#' s1 = ssm(sound, samplingRate = 16000, kernelLen = 100)
#' # more global features (captures the transition b/w the two sounds)
#' s2 = ssm(sound, samplingRate = 16000, kernelLen = 400)
#'
#' s2$summary
#' s2$detailed$novelty  # novelty contour
#' \dontrun{
#' ssm(sound, samplingRate = 16000,
#'     specFun = 'mfcc', simil = 'cor', normalize = TRUE,
#'     ssmWin = 10,  # speed up the processing
#'     kernelLen = 300,  # global features
#'     specPars = list(colorTheme = 'seewave'),
#'     ssmPars = list(col = rainbow(100)),
#'     noveltyPars = list(type = 'l', lty = 3, lwd = 2))
#'
#' # Custom input: produce a nice spectrogram first, then feed it into ssm()
#' sp = spectrogram(sound, 16000, windowLength = c(5, 40), contrast = .3,
#'   output = 'processed')  # return the modified spectrogram
#' ssm(sound, 16000, kernelLen = 400, specFun = sp)
#'
#' # Custom input: use acoustic features returned by analyze()
#' an = analyze(sound, 16000, windowLength = 20, novelty = NULL)
#' feature_mat = t(an$detailed[, 4:ncol(an$detailed)]) # or select pitch, HNR, ...
#' feature_mat = t(apply(feature_mat, 1, scale))  # z-transform all variables
#' feature_mat[is.na(feature_mat)] = 0  # get rid of NAs
#' colnames(feature_mat) = an$detailed$time  # time stamps in ms
#' rownames(feature_mat) = 1:nrow(feature_mat)
#' image(t(feature_mat))  # not a spectrogram, just a feature matrix
#' ssm(sound, 16000, kernelLen = 500, specFun = feature_mat, logSpec = FALSE,
#'   specPars = list(ylab = 'Feature'))
#' }
ssm = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    specFun = 'melspec',
    specFun_pars = list(),
    logSpec = FALSE,
    normalize = FALSE,
    simil = c('cosine', 'cor'),
    kernelLen = 1000,
    kernelSD = .5,
    padWith = 0,
    ssmWin = 1,
    summaryFun = c('mean', 'sd'),
    output = c('ssm', 'novelty'),
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    main = NULL,
    heights = c(2, 1),
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    specPars = list(
      colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
      xlab = 'Time'
    ),
    ssmPars = list(
      colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
      xlab = 'Time',
      ylab = 'Time'
    ),
    noveltyPars = list(
      type = 'l',
      col = 'black',
      lwd = 1
    )) {
  # deprecated parameters
  call_arg_names = names(match.call())
  if (any(c('input', 'melfcc_pars', 'MFCC', 'audSpec_pars') %in%
          call_arg_names))
    stop(paste('Changes in soundgen >3.0: "input" is deprecated and replaced by "specFun";',
               '"melfcc_pars", "MFCC", "audSpec_pars" are now passed in "specFun_pars"'))
  rm('call_arg_names')

  if (is.character(specFun)) specFun = specFun[1]
  output = match.arg(output, c('ssm', 'novelty', 'all'), several.ok = TRUE)
  if ('all' %in% output) output = c('ssm', 'novelty')
  simil = match.arg(simil)

  ## Prepare a list of arguments to pass to .ssm()
  myPars = as.list(environment())
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'savePlots', 'embed', 'reportEvery',
    'cores', 'summaryFun', 'output', 'specPars', 'ssmPars', 'noveltyPars')]
  myPars$specPars = specPars
  myPars$ssmPars = ssmPars
  myPars$noveltyPars = noveltyPars

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = .ssm,
    suffix = 'ssm',
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  if (pa$input$n == 0) stop('Failed to analyze any input')

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))

  # prepare summary
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
      if (!pa$input$failed[i]) {
        res = pa$result[[i]]
        temp[[i]] = try(summarizeAnalyze(
          data.frame(novelty = res$novelty),
          summaryFun = summaryFun,
          var_noSummary = NULL), silent = TRUE)
        if (inherits(temp[[i]], 'try-error')) temp[[i]] = NULL
      }
    }
    # Identify failed files (either processAudio failed or summarizeAnalyze failed)
    idx_failed = which(pa$input$failed)
    for (i in seq_len(pa$input$n)) {
      if (is.null(temp[[i]])) idx_failed = c(idx_failed, i)
    }
    idx_failed = unique(idx_failed)
    idx_ok = which(!seq_len(pa$input$n) %in% idx_failed)

    if (length(idx_failed) > 0) {
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]][1, , drop = FALSE]
        filler[1, ] = NA
      } else {
        # All files failed: generate a safe NA filler
        filler = try(summarizeAnalyze(
          data.frame(novelty = NA),
          summaryFun = summaryFun,
          var_noSummary = NULL), silent = TRUE)
        if (inherits(filler, 'try-error')) {
          filler = data.frame(matrix(NA, nrow = 1, ncol = length(summaryFun)))
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


#' SSM per sound
#' @param audio a list returned by \code{readAudio}
#' @noRd
.ssm = function(
    audio,
    specFun = 'melspec',
    specFun_pars = list(),
    logSpec = FALSE,
    normalize = FALSE,
    simil = 'cosine',
    kernelLen = 100,
    kernelSD = .5,
    padWith = 0,
    ssmWin = 1,
    output = c('ssm', 'novelty', 'summary'),
    plot = FALSE,
    main = NULL,
    heights = c(2, 1),
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    specPars = list(
      colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
      xlab = 'Time, s',
      ylab = 'kHz'
    ),
    ssmPars = list(
      colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
      xlab = 'Time, s',
      ylab = 'Time, s'
    ),
    noveltyPars = list(
      type = 'l',
      col = 'black',
      lwd = 1
    )) {
  nyquist = audio$samplingRate / 2
  if (is.null(specFun_pars)) specFun_pars = list()
  if (is.character(specFun)) specFun = specFun[1]

  if (is.matrix(specFun) || is.data.frame(specFun)) {
    # custom input to SSM - use as is
    target_spec = as.matrix(specFun)
  } else {
    # ssm()-specific defaults for common spectrogram extractors
    if (is.character(specFun)) {
      if (specFun %in% c('melspec', 'mfcc', 'melfcc')) {
        if (is.null(specFun_pars$windowLength)) specFun_pars$windowLength = 125
        if (is.null(specFun_pars$step)) specFun_pars$step = 25
        if (is.null(specFun_pars$nbands)) specFun_pars$nbands = 50
        if (is.null(specFun_pars$MFCC)) specFun_pars$MFCC = 2:13
      } else if (specFun %in% c('audSpectrogram', 'audSpec')) {
        if (is.null(specFun_pars$nFilters)) specFun_pars$nFilters = 16
        if (is.null(specFun_pars$step)) specFun_pars$step = 10
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
      specFun_pars$windowLength = NULL
      specFun_pars$step = NULL
    }

    target_spec = getSpec(audio,
                          specFun = specFun,
                          specFun_pars = specFun_pars)
  }

  # estimate the time step from the extracted representation
  if (ncol(target_spec) >= 2) {
    if (is.null(colnames(target_spec))) {
      step = audio$duration / max(1, ncol(target_spec) - 1)
    } else {
      step = diff(as.numeric(colnames(target_spec))[1:2])
    }
  } else {
    step = audio$duration
  }
  if (length(step) != 1 || !is.finite(step) || step <= 0) {
    step = audio$duration / max(1, ncol(target_spec))
  }
  frame_points = max(1, round(audio$samplingRate * step / 1000), na.rm = TRUE)

  if (logSpec) target_spec = floor_log(target_spec, dynamicRange = 90)
  # image(t(target_spec))

  ## compute self-similarity matrix
  # kernel size in frames, guaranteed to be even
  kernelSize = max(4, round(kernelLen * audio$samplingRate / 1000 /
                              frame_points / 2) * 2)
  s = selfsim(
    m = target_spec,
    normalize = normalize,
    simil = simil,
    ssmWin = ssmWin
  )
  # s = zeroOne(s^2)  # hist(s)
  # image(s)

  ## compute novelty
  novelty = getNovelty(ssm = s, kernelSize = kernelSize,
                       kernelSD = kernelSD, padWith = padWith)

  ## PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    if (is.null(main)) {
      if (audio$filename_base == 'sound') {
        main = ''
      } else {
        main = audio$filename_base
      }
    }

    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    on.exit(par(op), add = TRUE)
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
    par(mar = c(5.1, 4.1, 0, 2.1),
        xaxt = 's',
        yaxt = 's')

    # spectrogram
    yScale = if (!is.null(specFun_pars$yScale)) specFun_pars$yScale else
      if (isTRUE(specFun == 'melspec')) 'mel' else 'linear'

    if (isTRUE(specFun %in% c('audSpec', 'audSpectrogram'))) {
      # spec = zeroOne(t(log(target_spec + 1e-4)))
      spec = t(target_spec)
      specPars1 = list(
        colorTheme = 'seewave',
        xlab = 'Time, s',
        ylab = 'kHz'
      )
      specPars1[names(specPars)] = specPars
      specPars1$color.palette = switchColorTheme(specPars1$colorTheme)
      specPars1[['colorTheme']] = NULL
      do.call(filled.contour.mod, c(list(
        x = as.numeric(rownames(spec)),
        y = as.numeric(colnames(spec)),
        z = spec,
        yScale = if (is.null(specFun_pars$yScale)) 'ERB' else specFun_pars$yScale
      ), specPars1))
    } else {
      spec = t(target_spec)
      specPars1 = list(
        colorTheme = 'seewave',
        xlab = 'Time, s',
        ylab = ''
      )
      specPars1[names(specPars)] = specPars
      specPars1$color.palette = switchColorTheme(specPars1$colorTheme)
      specPars1[['colorTheme']] = NULL

      do.call(filled.contour.mod, c(list(
        x = as.numeric(rownames(spec)),
        y = as.numeric(colnames(spec)),
        z = spec,
        yScale = yScale
      ), specPars1
      ))
    }

    # novelty
    noveltyPars1 = list(
      type = 'l',
      col = 'black',
      lwd = 1
    )
    noveltyPars1[names(noveltyPars)] = noveltyPars

    # rescale novelty contour to ylim of the newly created spectrogram
    ranges = par("usr")
    xlim = ranges[1:2]
    ylim = ranges[3:4]
    do.call(lines, c(list(
      x = seq(ranges[1], ranges[2], length.out = length(novelty)),
      y = ranges[3] + zeroOne(novelty) * (ranges[4] - ranges[3]) * .95
    ), noveltyPars1
    ))
    axis(side = 1, labels = TRUE)
    par(mar = c(0, 4.1, 2.1, 2.1),
        xaxt = 'n',
        yaxt = 's')
    # xlab = ''

    # SSM
    ssmPars1 = list(
      levels = seq(0, 1, length = 30),
      colorTheme = 'seewave',
      xlab = 'Time, s',
      ylab = 'Time, s',
      main = main
    )
    ssmPars1[names(ssmPars)] = ssmPars
    ssmPars1$color.palette = switchColorTheme(ssmPars1$colorTheme)
    ssmPars1[['colorTheme']] = NULL
    timestamps_ssm = seq(0, audio$duration, length.out = nrow(s))
    do.call(filled.contour.mod, c(list(
      x = timestamps_ssm,
      y = timestamps_ssm,
      z = s,
      y_Hz = FALSE
    ), ssmPars1
    ))
  }

  out = list(ssm = s, novelty = novelty)
  invisible(out[which(names(out) %in% output)])
}


#' Compute self-similarity
#' @param m input matrix such as a spectrogram
#' @inheritParams ssm
#' @param ssmWin the length of window for averaging self-similarity, frames
#' @return A square self-similarity matrix.
#' @noRd
#' @examples
#' m = matrix(rnorm(40), nrow = 5)
#' soundgen:::selfsim(m)
selfsim = function(m,
                   normalize = FALSE,
                   simil = 'cosine',
                   ssmWin = 1) {
  nc = ncol(m)
  if (ssmWin > floor(nc / 2)) {
    ssmWin = floor(nc / 2)
    warning(paste('"ssmWin" must be smaller than half the number of frames',
                  'resetting to', floor(nc / 2)))
  }
  if (ssmWin %% 2 == 0) {
    ssmWin = max(ceiling(ssmWin / 2) * 2 - 1, 1)
  }

  # normalize input by column, if needed
  if (normalize) {
    m = apply(m, 2, zeroOne)
    m[is.na(m)] = 0
  }

  # window start indices
  winIdx = unique(round(seq(1, nc - ssmWin + 1,
                            length.out = ceiling(nc / ssmWin))))
  numWins = length(winIdx)

  # build blocks matrix (each column = one flattened window)
  nr = nrow(m)
  blockLen = nr * ssmWin
  blocks = matrix(0, nrow = blockLen, ncol = numWins)
  for (k in seq_len(numWins)) {
    blocks[, k] = as.vector(m[, winIdx[k]:(winIdx[k] + ssmWin - 1), drop = FALSE])
  }

  # compute full similarity matrix via vectorized operations
  if (simil == 'cosine') {
    colNorms = sqrt(colSums(blocks^2))
    # mark zero-norm columns (will yield 0 similarity)
    zeroCols = which(colNorms == 0)
    colNorms[colNorms == 0] = 1  # avoid division by zero
    blocks_unit = blocks / rep(colNorms, each = blockLen)
    out = crossprod(blocks_unit)  # numWins x numWins
    if (length(zeroCols) > 0) {
      out[zeroCols, ] = 0
      out[, zeroCols] = 0
    }
  } else {
    # Pearson correlation; cor() is already vectorized over columns
    out = cor(blocks)
    out[is.na(out)] = 0  # constant columns produce NA
  }
  diag(out) = 1
  rownames(out) = colnames(out) = winIdx
  zeroOne(out)
}


#' SSM novelty
#' Calculates novelty in a self-similarity matrix. Called by \code{\link{ssm}}.
#' @param ssm self-similarity matrix, as produced by \code{\link{selfsim}}
#' @param kernelSize the size of Gaussian kernel (points, even number >=2)
#' @param kernelSD the SD of Gaussian kernel
#' @return A numeric vector of length \code{nrow(ssm)}.
#' @noRd
getNovelty = function(ssm,
                      kernelSize,
                      kernelSD,
                      padWith = 0) {
  kernelSize = kernelSize + (kernelSize %% 2)  # ensure even
  kernel = getCheckerboardKernel(size = kernelSize, kernelSD = kernelSD)
  N = nrow(ssm)
  K = kernelSize
  halfK = K / 2

  # Pad the SSM so the kernel can slide from the very edge
  ssm_padded = matrix(padWith, nrow = N + K, ncol = N + K)
  ssm_padded[(halfK + 1):(N + halfK), (halfK + 1):(N + halfK)] = ssm

  # Indices of diagonal elements within a K x K block (column-major order)
  diag_idx = seq(1L, K * K, by = K + 1L)
  off_diag_idx = setdiff(seq_len(K * K), diag_idx)
  n_off = length(off_diag_idx)  # K^2 - K

  # Kernel vector, off-diagonal only
  kernel_vec = as.vector(kernel)[off_diag_idx]

  ## Rare path: NAs present (padWith = NA or NAs in ssm): fall back to the
  ## original loop
  if (is.na(padWith) || anyNA(ssm)) {
    novelty = rep(0, N)
    for (p in seq_len(N)) {
      mat_p = ssm_padded[p:(p + K - 1), p:(p + K - 1)]
      diag(mat_p) = NA
      novelty[p] = suppressWarnings(
        cor(as.vector(mat_p), as.vector(kernel),
            use = 'pairwise.complete.obs'))
    }
    novelty[is.na(novelty)] = 0
    return(novelty)
  }

  ## Fast vectorized path (no NAs): extract all N diagonal blocks at once.
  # For block p, element (r,c) lives at ssm_padded[p+r-1, p+c-1].
  row_off = rep(seq_len(K), times = K)   # row offset within block
  col_off = rep(seq_len(K), each = K)   # col offset within block
  all_rows = outer(seq_len(N), row_off - 1L, `+`)  # N x K^2
  all_cols = outer(seq_len(N), col_off - 1L, `+`)  # N x K^2
  X = matrix(
    ssm_padded[cbind(as.vector(all_rows), as.vector(all_cols))],
    nrow = N, ncol = K * K
  )[, off_diag_idx]  # N x n_off (drop diagonal elements)

  # Pearson correlation of each row of X with the fixed kernel_vec:
  #   cor(x, y) = (x_c . y_c) / sqrt( ss_x * ss_y )
  y_c = kernel_vec - mean(kernel_vec)
  ss_y = sum(y_c^2)                        # scalar (constant)
  x_c = X - rowMeans(X)                   # center each row; R recycles
  ss_x = rowSums(x_c^2)                    # length N
  numerator = as.vector(x_c %*% y_c)       # N dot products in one BLAS call
  denom = sqrt(ss_x * ss_y)
  novelty = numerator / denom
  novelty[is.na(novelty) | denom == 0] = 0
  return(novelty)
}
