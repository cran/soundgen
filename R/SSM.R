#' Self-similarity matrix
#'
#' Calculates the self-similarity matrix and novelty vector of a sound. The
#' self-similarity matrix is produced by cross-correlating different segments of
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
#' @inheritParams spectrogram
#' @inheritParams analyze
#' @param sparse if TRUE, the entire SSM is not calculated, but only the central
#'   region needed to extract the novelty contour (speeds up the processing)
#' @param input the spectral representation used to calculate the SSM: "audSpec"
#'   = auditory spectrogram returned by \code{\link{audSpectrogram}}, "mfcc" =
#'   Mel-Frequency Cepstral coefficients, "melspec" = Mel-transformed STFT
#'   spectrogram, "spec" = STFT power spectrogram (all three returned by
#'   \code{\link[tuneR]{melfcc}}). Any custom spectrogram-like matrix of
#'   features (time in columns labeled in s, features in rows) is also accepted
#'   (see examples)
#' @param takeLog if TRUE, the input is log-transformed prior to calculating
#'   self-similarity
#' @param MFCC which mel-frequency cepstral coefficients to use; defaults to
#'   \code{2:13}
#' @param melfcc_pars a list of parameters passed to \code{\link[tuneR]{melfcc}}
#' @param audSpec_pars a list of parameters passed to
#'   \code{\link{audSpectrogram}} (if input = 'audSpec')
#' @param norm if TRUE, the spectrum of each STFT frame is normalized
#' @param simil method for comparing frames: "cosine" = cosine similarity, "cor"
#'   = Pearson's correlation
#' @param kernelLen length of checkerboard kernel for calculating novelty, ms
#'   (larger values favor global, slow vs. local, fast novelty)
#' @param kernelSD SD of checkerboard kernel for calculating novelty
#' @param padWith how to treat edges when calculating novelty: NA = treat sound
#'   before and after the recording as unknown, 0 = treat it as silence
#' @param ssmWin window for averaging SSM, frames (has a smoothing effect and
#'   speeds up the processing)
#' @param output what to return (drop "ssm" to save memory when analyzing a lot
#'   of files)
#' @param plot if TRUE, plots the SSM
#' @param heights relative sizes of the SSM and spectrogram/novelty plot
#' @param main plot title
#' @param specPars graphical parameters passed to \code{filled.contour.mod} and
#'   affecting the \code{\link{spectrogram}}
#' @param ssmPars graphical parameters passed to \code{filled.contour.mod} and
#'   affecting the plot of SSM
#' @param noveltyPars graphical parameters passed to
#'   \code{\link[graphics]{lines}} and affecting the novelty contour
#' @return Returns a list of two components: $ssm contains the self-similarity
#'   matrix, and $novelty contains the novelty vector.
#' @export
#' @examples
#' sound = c(soundgen(),
#'           soundgen(nSyl = 4, sylLen = 50, pauseLen = 70,
#'           formants = NA, pitch = c(500, 330)))
#' # playme(sound)
#' # detailed, local features (captures each syllable)
#' s1 = ssm(sound, samplingRate = 16000, kernelLen = 100,
#'          sparse = TRUE)  # much faster with 'sparse'
#' # more global features (captures the transition b/w the two sounds)
#' s2 = ssm(sound, samplingRate = 16000, kernelLen = 400, sparse = TRUE)
#'
#' s2$summary
#' s2$novelty  # novelty contour
#' \dontrun{
#' ssm(sound, samplingRate = 16000,
#'     input = 'mfcc', simil = 'cor', norm = TRUE,
#'     ssmWin = 10,  # speed up the processing
#'     kernelLen = 300,  # global features
#'     specPars = list(colorTheme = 'seewave'),
#'     ssmPars = list(col = rainbow(100)),
#'     noveltyPars = list(type = 'l', lty = 3, lwd = 2))
#'
#' # Custom input: produce a nice spectrogram first, then feed it into ssm()
#' sp = spectrogram(sound, 16000, windowLength = c(5, 40), contrast = .3,
#'   output = 'processed')  # return the modified spectrogram
#' colnames(sp) = as.numeric(colnames(sp)) / 1000  # convert ms to s
#' ssm(sound, 16000, kernelLen = 400, input = sp)
#'
#' # Custom input: use acoustic features returned by analyze()
#' an = analyze(sound, 16000, windowLength = 20, novelty = NULL)
#' input_an = t(an$detailed[, 4:ncol(an$detailed)]) # or select pitch, HNR, ...
#' input_an = t(apply(input_an, 1, scale))  # z-transform all variables
#' input_an[is.na(input_an)] = 0  # get rid of NAs
#' colnames(input_an) = an$detailed$time / 1000  # time stamps in s
#' rownames(input_an) = 1:nrow(input_an)
#' image(t(input_an))  # not a spectrogram, just a feature matrix
#' ssm(sound, 16000, kernelLen = 500, input = input_an, takeLog = FALSE,
#'   specPars = list(ylab = 'Feature'))
#' }
ssm = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    sparse = FALSE,
    input = c('melspec', 'mfcc', 'spec', 'audSpec')[1],
    melfcc_pars = list(windowLength = 125, step = 25, nbands = 50),
    MFCC = 2:13,
    audSpec_pars = list(nFilters = 16, step = 10),
    takeLog = FALSE,
    norm = FALSE,
    simil = c('cosine', 'cor')[1],
    kernelLen = 1000,
    kernelSD = .5,
    padWith = 0,
    ssmWin = NULL,
    summaryFun = c('mean', 'sd'),
    output = c('ssm', 'novelty', 'summary'),
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = NULL,
    main = NULL,
    heights = c(2, 1),
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    specPars = list(
      colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
      xlab = 'Time, s'
    ),
    ssmPars = list(
      colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
      xlab = 'Time, s',
      ylab = 'Time, s'
    ),
    noveltyPars = list(
      type = 'b',
      pch = 16,
      col = 'black',
      lwd = 3
    )) {
  ## Prepare a list of arguments to pass to .ssm()
  myPars = as.list(environment())
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'savePlots', 'reportEvery', 'cores',
    'summaryFun', 'specPars', 'ssmPars', 'noveltyPars', 'ssmWin',
    'melfcc_pars', 'audSpec_pars')]
  myPars$specPars = specPars
  myPars$ssmPars = ssmPars
  myPars$noveltyPars = noveltyPars
  myPars$melfcc_pars = melfcc_pars
  myPars$audSpec_pars = audSpec_pars
  myPars$win = ssmWin

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.ssm',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "ssm", width = paste0(width, units)))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          data.frame(novelty = pa$result[[i]]$novelty),
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

  if (pa$input$n == 1) {
    # unlist
    ssm = pa$result[[1]]$ssm
    novelty = pa$result[[1]]$novelty
  } else {
    ssm = lapply(pa$result, function(x) x[['ssm']])
    novelty = lapply(pa$result, function(x) x[['novelty']])
  }
  out = list(ssm = ssm, novelty = novelty, summary = mysum_all)
  invisible(out[which(names(out) %in% output)])
}


#' SSM per sound
#'
#' Internal soundgen function.
#' @inheritParams ssm
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.ssm = function(
    audio,
    sparse = FALSE,
    input = c('melspec', 'mfcc', 'spec', 'audSpec')[1],
    melfcc_pars = list(windowLength = 125, step = 25, nbands = 50),
    MFCC = 2:13,
    audSpec_pars = list(nFilters = 16, step = 10),
    takeLog = FALSE,
    norm = FALSE,
    simil = c('cosine', 'cor')[1],
    kernelLen = 100,
    kernelSD = .5,
    padWith = 0,
    win = 1,
    output = c('ssm', 'novelty', 'summary'),
    plot = TRUE,
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
      type = 'b',
      pch = 16,
      col = 'black',
      lwd = 3
    )) {
  nyquist = audio$samplingRate / 2
  if (is.matrix(input)) {
    # custom input to SSM - use as is
    target_spec = as.matrix(input)
    step = diff(as.numeric(colnames(target_spec))[1:2])
    frame_points = round(audio$samplingRate * step)
    input = 'custom'
  } else {
    ## compute mel-filtered spectrum and MFCCs
    if (input == 'audSpec') {
      # call audSpectrogram()
      if (is.null(audSpec_pars$nFilters)) audSpec_pars$nFilters = 32
      if (is.null(audSpec_pars$step)) audSpec_pars$step = 20
      frame_points = round(audio$samplingRate * audSpec_pars$step / 1000)

      target_spec = do.call(.audSpectrogram, c(audSpec_pars, list(
        audio = audio,
        plot = FALSE
      )))$audSpec  # cols = time, rows = freq
      colnames(target_spec) = as.numeric(colnames(target_spec)) / 1000  # ms to s
    } else if (input %in% c('mfcc', 'spec', 'melspec')) {
      # call tuneR::melfcc()
      if (is.null(melfcc_pars$windowLength)) melfcc_pars$windowLength = 25
      if (is.null(melfcc_pars$step)) melfcc_pars$step = 5
      frame_points = round(audio$samplingRate * melfcc_pars$step / 1000)
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
          numcep = max(MFCC)
        )))
      if (input == 'mfcc') {
        # the first cepstrum presumably makes no sense with amplitude normalization
        # (?), and it overestimates the similarity of different frames
        target_spec = t(mel$cepstra)[MFCC, ]
        target_spec[is.na(target_spec)] = 0  # MFCC are NaN for silent frames
        colnames(target_spec) = seq(audio$timeShift, audio$duration,
                                    length.out = ncol(target_spec))
        rownames(target_spec) = 1:nrow(target_spec)
      } else if (input == 'melspec') {
        target_spec = t(mel$aspectrum)  # cols = time, rows = freq
        colnames(target_spec) = seq(audio$timeShift, audio$duration,
                                    length.out = ncol(target_spec))
        rownames(target_spec) = otherToHz(
          seq(0, HzToOther(nyquist, "mel"),
              length.out = nrow(target_spec)), "mel") / 1000
      } else if (input == 'spec') {
        target_spec = t(mel$pspectrum)  # cols = time, rows = freq
        colnames(target_spec) = seq(audio$timeShift, audio$duration,
                                    length.out = ncol(target_spec))
        rownames(target_spec) = seq(0, nyquist,
                                    length.out = nrow(target_spec)) / 1000
      }
    }
  }
  if (takeLog) {
    target_spec = target_spec - min(target_spec, na.rm = TRUE)
    target_spec = log(target_spec + min(target_spec[target_spec > 0], na.rm = TRUE))
  }
  # image(t(target_spec))

  ## compute self-similarity matrix
  # kernel size in frames, guaranteed to be even
  kernelSize = max(4, round(kernelLen * audio$samplingRate / 1000 /
                              frame_points / 2) * 2)
  s = selfsim(
    m = target_spec,
    norm = norm,
    simil = simil,
    win = win,
    sparse = sparse,
    kernelSize = kernelSize
  )
  # s = zeroOne(s^2)  # hist(s)
  # image(s)

  ## compute novelty
  novelty = getNovelty(ssm = s, kernelSize = kernelSize,
                       kernelSD = kernelSD, padWith = padWith)

  ## PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_ssm.png"),
        width = width, height = height, units = units, res = res)
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
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
    par(mar = c(5.1, 4.1, 0, 2.1),
        xaxt = 's',
        yaxt = 's')

    # spectrogram
    if (input == 'audSpec') {
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
        yScale = if (is.null(audSpec_pars$yScale)) 'bark' else audSpec_pars$yScale
      ), specPars1))
      specPars1$xlim = range(as.numeric(rownames(spec)))
      specPars1$ylim = range(as.numeric(colnames(spec)))
    } else {
      # # log-transform and normalize spectrogram
      # if (input == 'melspec') {
      #   spec = log(zeroOne(mel$aspectrum) + 1e-4)  # dynamic range ~ 80 dB or 1e-4
      # } else {
      #   spec = log(zeroOne(mel$pspectrum) + 1e-4)
      # }
      # spec = zeroOne(spec)
      spec = t(target_spec)
      specPars1 = list(
        colorTheme = 'seewave',
        xlab = 'Time, s',
        ylab = if (input %in% c('custom', 'mfcc')) input  else 'kHz'
      )
      specPars1[names(specPars)] = specPars
      specPars1$color.palette = switchColorTheme(specPars1$colorTheme)
      specPars1[['colorTheme']] = NULL
      do.call(filled.contour.mod, c(list(
        x = as.numeric(rownames(spec)),
        y = as.numeric(colnames(spec)),
        z = spec,
        yScale = if (input == 'melspec') 'mel' else 'linear'
      ), specPars1
      ))
    }
    specPars1$xlim = c(audio$timeShift, audio$duration)
    specPars1$ylim = range(as.numeric(rownames(target_spec)))
    if (input == 'melspec')
      specPars1$ylim = HzToOther(specPars1$ylim * 1000, 'mel')

    # novelty
    noveltyPars1 = list(
      type = 'b',
      pch = 16,
      col = 'black',
      lwd = 3
    )
    noveltyPars1[names(noveltyPars)] = noveltyPars
    do.call(lines, c(list(
      x = seq(specPars1$xlim[1], specPars1$xlim[2], length.out = length(novelty)),
      y = zeroOne(novelty) * specPars1$ylim[2] * .95
    ), noveltyPars1
    ))
    axis(side = 1, labels = TRUE)
    par(mar = c(0, 4.1, 2.1, 2.1),
        xaxt = 'n',
        yaxt = 's')
    xlab = ''

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
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
    if (is.character(audio$savePlots)) dev.off()
  }

  out = list(ssm = s, novelty = novelty)
  invisible(out[which(names(out) %in% output)])
}


#' Compute self-similarity
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{ssm}}.
#' @param m input matrix such as a spectrogram
#' @inheritParams ssm
#' @param win the length of window for averaging self-similarity, frames
#' @return Returns a square self-similarity matrix.
#' @keywords internal
#' @examples
#' m = matrix(rnorm(40), nrow = 5)
#' soundgen:::selfsim(m, sparse = TRUE, kernelSize = 2)
selfsim = function(m,
                   norm = FALSE,
                   simil = c('cosine', 'cor')[1],
                   win = 1,
                   sparse = FALSE,
                   kernelSize = NULL) {
  nc = ncol(m)
  if (win > floor(nc / 2)) {
    win = floor(nc / 2)
    warning(paste('"win" must be smaller than half the number of frames',
                  'resetting to', floor(nc / 2)))
  }
  if (win %% 2 == 0) {
    win = max(ceiling(win / 2) * 2 - 1, 1)
  } # win must be odd

  # normalize input by column, if needed
  if (norm) {
    m = apply(m, 2, zeroOne, na.rm = TRUE)
    m[is.na(m)] = 0
  }

  # calculate windows for averaging self-similarity
  winIdx = unique(round(seq(1, nc - win + 1, length.out = ceiling(nc / win))))
  numWins = length(winIdx)

  # calculate the lower triangle of self-similarity matrix
  out = matrix(NA, nrow = numWins, ncol = numWins)
  rownames(out) = colnames(out) = winIdx
  if (!sparse) j_idx = seq_len(numWins)
  for (i in seq_along(winIdx)) {
    if (sparse) {
      j_idx = max(1, i - kernelSize) : max(1, (i - 1))
    } else {
      j_idx = 1:max(1, (i - 1))
    }
    for (j in j_idx) {
      mi = as.vector(m[, winIdx[i]:(winIdx[i] + win - 1)])
      mj = as.vector(m[, winIdx[j]:(winIdx[j] + win - 1)])
      if (any(mi != 0) && any(mj != 0)) {
        if (simil == 'cosine') {
          # http://stackoverflow.com/questions/6597005/cosine-similarity-between-two-vectors-in-language-r
          out[i, j] = crossprod(mi, mj) / sqrt(crossprod(mi) * crossprod(mj))
        } else if (simil == 'cor') {
          out[i, j] = cor(mi, mj)
        }
      } else {
        # if at least one is a vector of zeros, set result to 0 (otherwise NA)
        out[i, j] = 0
      }
    }
  }
  # fill up the upper triangle as well
  diag(out) = 1
  out1 = t(out)
  out1[lower.tri(out1)] = out[lower.tri(out)]
  # isSymmetric(out1)
  # image(out1)
  zeroOne(t(out1), na.rm = TRUE)
}


#' Checkerboard kernel
#'
#' Internal soundgen function.
#'
#' Prepares a square matrix \code{size x size} specifying a gaussian kernel for
#' measuring novelty of self-similarity matrices. Called by
#' \code{\link{getNovelty}}
#' @param size kernel size (points), preferably an even number
#' @param kernel_mean,kernelSD mean and SD of the gaussian kernel
#' @param plot if TRUE, shows a perspective plot of the kernel
#' @param checker if TRUE, inverts two quadrants
#' @return Returns a square matrix with \code{size} rows and columns.
#' @keywords internal
#' @examples
#' kernel = soundgen:::getCheckerboardKernel(size = 64, kernelSD = 0.1, plot = TRUE)
#' dim(kernel)
#' kernel = soundgen:::getCheckerboardKernel(size = 19, kernelSD = .5,
#'   checker = FALSE, plot = TRUE)
#' kernel = soundgen:::getCheckerboardKernel(size = c(9, 45), kernelSD = .5,
#'   checker = FALSE, plot = TRUE)
#' kernel = soundgen:::getCheckerboardKernel(size = c(9, 45), kernelSD = .5,
#'   checker = TRUE, plot = TRUE)
getCheckerboardKernel = function(size,
                                 kernel_mean = 0,
                                 kernelSD = 0.5,
                                 plot = FALSE,
                                 checker = TRUE) {
  if (length(size) == 1) {
    x = y = seq(-1, 1, length.out = size)
    size = c(size, size)
  } else if (length(size) == 2) {
    x = seq(-1, 1, length.out = size[1])
    y = seq(-1, 1, length.out = size[2])
  } else {
    stop('size must be of length 1 or 2')
  }

  kernelSD = kernelSD  # just to get rid of the "unused arg" warning in CMD check :-)
  if (max(size) < 50) {
    # faster than mvtnorm::dmvnorm for small kernels
    kernel = matrix(NA, ncol = size[2], nrow = size[1])
    for (i in seq_len(nrow(kernel))) {
      for (j in seq_len(ncol(kernel))) {
        kernel[i, j] = dnorm(x[i], mean = kernel_mean, sd = kernelSD) *
          dnorm(y[j], mean = kernel_mean, sd = kernelSD)
      }
    }
  } else {
    # this is faster for large kernels
    sigma = diag(2) * kernelSD
    kernel_long = expand.grid(x1 = x, x2 = y)
    kernel_long$dd = mvtnorm::dmvnorm(x = kernel_long,
                                      mean = c(kernel_mean, kernel_mean),
                                      sigma = sigma)
    kernel = matrix(kernel_long$dd, nrow = size[1])
    # kernel[1:5, 1:5]
  }

  if (checker) {
    fl_row = floor(size[1] / 2)
    fl_col = floor(size[2] / 2)
    cl_row = ceiling(size[1] / 2)
    cl_col = ceiling(size[2] / 2)
    # quadrant 0 to 3 o'clock
    kernel[seq_len(fl_row), (cl_col + 1):size[2]] = -kernel[seq_len(fl_row), (cl_col + 1):size[2]]
    # quadrant 6 to 9 o'clock
    kernel[(cl_row + 1):size[1], seq_len(cl_col)] = -kernel[(cl_row + 1):size[1], seq_len(cl_col)]
  }
  kernel = kernel / max(kernel)

  if (plot) {
    persp(
      kernel,
      theta = -20,
      phi = 25,
      # zlim = c(-1, 4),
      ticktype = 'detailed'
    )
  }
  kernel
}


#' SSM novelty
#'
#' Internal soundgen function.
#'
#' Calculates novelty in a self-similarity matrix. Called by \code{\link{ssm}}.
#' @param ssm self-similarity matrix, as produced by \code{\link{selfsim}}
#' @param kernelSize the size of gausisan kernel (points)
#' @param kernelSD the SD of gaussian kernel
#' @param normalize if TRUE, normalizes so that max = 1
#' @return Returns a numeric vector of length \code{nrow(ssm)}
#' @keywords internal
getNovelty = function(ssm,
                      kernelSize,
                      kernelSD,
                      padWith = 0,
                      normalize = TRUE) {
  kernel = getCheckerboardKernel(size = kernelSize, kernelSD = kernelSD)
  ## pad matrix with size / 2 zeros, so that we can correlate it with the
  #  kernel starting from the very edge
  ssm_padded = matrix(padWith,
                      nrow = nrow(ssm) + kernelSize,
                      ncol = nrow(ssm) + kernelSize)
  halfK = kernelSize / 2
  # indices in the padded matrix where we'll paste the original ssm
  idx = c(halfK + 1, nrow(ssm_padded) - halfK)
  # paste original. Now we have a padded ssm
  ssm_padded[idx[1]:idx[2], idx[1]:idx[2]] = ssm

  ## get novelty
  novelty = rep(0, nrow(ssm))
  # for each point on the main diagonal, novelty = correlation between the
  # checkerboard kernel and the ssm. See Badawy, "Audio novelty-based
  # segmentation of music concerts"
  for (i in idx[1]:idx[2]) {
    n = (i - halfK):(i + halfK - 1)
    # suppress warnings, b/c otherwise cor complains of sd = 0 for silent segments
    mat_i = ssm_padded[n, n]
    diag(mat_i) = NA
    novelty[i - halfK] =  suppressWarnings(
      cor(as.vector(mat_i), as.vector(kernel), use = 'pairwise.complete.obs')
    )
  }
  novelty[is.na(novelty)] = 0
  novelty
}
