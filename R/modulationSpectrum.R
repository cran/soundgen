### MODULATION SPECTRUM

#' Modulation spectrum
#'
#' Produces a modulation spectrum of waveform(s) or audio file(s), with temporal
#' modulation along the X axis (Hz) and spectral modulation (1/KHz) along the Y
#' axis. A good visual analogy is decomposing the spectrogram into a sum of
#' ripples of various frequencies and directions. Roughness is calculated as the
#' proportion of energy / amplitude of the modulation spectrum within
#' \code{roughRange} of temporal modulation frequencies. The frequency of
#' amplitude modulation (amMsFreq, Hz) is calculated as the highest peak in the
#' smoothed AM function, and its purity (amMsPurity, dB) as the ratio of this
#' peak to the median AM over \code{amRange}. For relatively short and steady
#' sounds, set \code{amRes = NULL} and analyze the entire sound. For longer
#' sounds and when roughness or AM vary over time, set \code{amRes} to get
#' multiple measurements over time (see examples).
#'
#' Algorithm: prepare a spectrogram, take its logarithm (if \code{logSpec =
#' TRUE}), center, perform a 2D Fourier transform (see also
#' spectral::spec.fft()), take the upper half of the resulting symmetric matrix,
#' and raise it to \code{power}. The result is returned as \code{$original}. For
#' plotting purposes, the modulation matrix can be smoothed with Gaussian blur
#' (see \code{\link{gaussianSmooth2D}}) and log-warped (if \code{logWarp} is a
#' positive number). This processed modulation spectrum is returned as
#' \code{$processed}. If the audio is long enough, multiple windows are
#' analyzed, resulting in a vector of roughness values. For multiple inputs,
#' such as a list of waveforms or path to a folder with audio files, the
#' ensemble of modulation spectra can be interpolated to the same spectral and
#' temporal resolution and averaged (if \code{averageMS}).
#'
#' @seealso \code{\link{plotMS}} \code{\link{spectrogram}} \code{\link{analyze}}
#'
#' @references \itemize{
#'   \item Singh, N. C., & Theunissen, F. E. (2003). Modulation spectra of
#'   natural sounds and ethological theories of auditory processing. The Journal
#'   of the Acoustical Society of America, 114(6), 3394-3411.
#' }
#' @inheritParams spectrogram
#' @inheritParams analyze
#' @param roughRange the range of temporal modulation frequencies that
#'   constitute the "roughness" zone, Hz
#' @param amRange the range of temporal modulation frequencies that we are
#'   interested in as "amplitude modulation" (AM), Hz
#' @param amRes target resolution of amplitude modulation, Hz. If \code{NULL},
#'   the entire sound is analyzed at once, resulting in a single roughness value
#'   (unless it is longer than \code{maxDur}, in which case it is analyzed in
#'   chunks \code{maxDur} s long). If \code{amRes} is set, roughness is
#'   calculated for windows \code{~1000/amRes} ms long (but at least 3 STFT
#'   frames). \code{amRes} also affects the amount of smoothing when calculating
#'   \code{amMsFreq} and \code{amMsPurity}
#' @param maxDur sounds longer than \code{maxDur} s are split into fragments,
#'   and the modulation spectra of all fragments are averaged
#' @param logSpec if TRUE, the spectrogram is log-transformed prior to taking 2D
#'   FFT
#' @param power raise modulation spectrum to this power (eg power = 2 for ^2, or
#'   "power spectrum")
#' @param normalize if TRUE, the modulation spectrum of each analyzed fragment
#'   \code{maxDur} in duration is separately normalized to have max = 1
#' @param returnMS if FALSE, only roughness is returned (much faster)
#' @param returnComplex if TRUE, returns a complex modulation spectrum (without
#'   normalization and warping)
#' @param averageMS if TRUE, the modulation spectra of all inputs are averaged
#'   into a single output; if FALSE, a separate MS is returned for each input
#' @param plot if TRUE, plots the modulation spectrum of each sound
#' @param savePlots if a valid path is specified, a plot is saved in this folder
#'   (defaults to NA)
#' @param logWarpX,logWarpY numeric vector of length 2: c(sigma, base) of
#'   pseudolog-warping the modulation spectrum, as in function
#'   pseudo_log_trans() from the "scales" package
#' @param quantiles labeled contour values, \% (e.g., "50" marks regions that
#'   contain 50\% of the sum total of the entire modulation spectrum)
#' @param kernelSize the size of Gaussian kernel used for smoothing (1 = no
#'   smoothing)
#' @param kernelSD the SD of Gaussian kernel used for smoothing, relative to its
#' size
#' @param xlab,ylab,main,xlim,ylim graphical parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param ... other graphical parameters passed on to \code{filled.contour.mod}
#'   and \code{\link[graphics]{contour}} (see \code{\link{spectrogram}})
#' @return Returns a list with the following components:
#' \itemize{
#' \item \code{$original} modulation spectrum prior to blurring and log-warping,
#' but after squaring if \code{power = TRUE}, a matrix of nonnegative values.
#' Rownames are spectral modulation frequencies (cycles/KHz), and colnames are
#' temporal modulation frequencies (Hz).
#' \item \code{$processed} modulation spectrum after blurring and log-warping
#' \item \code{$complex} untransformed complex modulation spectrum (returned
#' only if returnComplex = TRUE)
#' \item \code{$roughness} proportion of energy / amplitude of the modulation
#' spectrum within \code{roughRange} of temporal modulation frequencies, \% - a
#' vector if amRes is numeric and the sound is long enough, a single number
#' otherwise
#' \item \code{$amMsFreq} frequency of the highest peak, within \code{amRange}, of
#' the folded AM function (average AM across all FM bins for both negative and
#' positive AM frequencies), where a peak is a local maximum over \code{amRes}
#' Hz. Like \code{roughness}, \code{amMsFreq} and \code{amMsPurity} can be single
#' numbers or vectors, depending on whether the sound is analyzed as a whole or
#' in chunks
#' \item \code{$amMsPurity} ratio of the peak at amMsFreq to the median AM over
#' \code{amRange}, dB
#' \item \code{$summary} dataframe with summaries of roughness, amMsFreq, and
#' amMsPurity
#' }
#' @export
#' @examples
#' # White noise
#' ms = modulationSpectrum(runif(16000), samplingRate = 16000,
#'   logSpec = FALSE, power = TRUE,
#'   amRes = NULL)  # analyze the entire sound, giving a single roughness value
#' str(ms)
#'
#' # Harmonic sound
#' s = soundgen(amFreq = 25, amDep = 50)
#' ms = modulationSpectrum(s, samplingRate = 16000, amRes = NULL)
#' ms[c('roughness', 'amMsFreq', 'amMsPurity')]  # a single value for each
#' ms1 = modulationSpectrum(s, samplingRate = 16000, amRes = 5)
#' ms1[c('roughness', 'amMsFreq', 'amMsPurity')]
#' # measured over time (low values of amRes mean more precision, so we analyze
#' # longer segments and get fewer values per sound)
#'
#' # Embellish
#' ms = modulationSpectrum(s, samplingRate = 16000,
#'   xlab = 'Temporal modulation, Hz', ylab = 'Spectral modulation, 1/KHz',
#'   colorTheme = 'heat.colors', main = 'Modulation spectrum', lty = 3)
#'
#' \dontrun{
#' # A long sound with varying AM and a bit of chaos at the end
#' s_long = soundgen(sylLen = 3500, pitch = c(250, 320, 280),
#'                   amFreq = c(30, 55), amDep = c(20, 60, 40),
#'                   jitterDep = c(0, 0, 2))
#' playme(s_long)
#' ms = modulationSpectrum(s_long, 16000)
#' # plot AM over time
#' plot(x = seq(1, 1500, length.out = length(ms$amMsFreq)), y = ms$amMsFreq,
#'      cex = 10^(ms$amMsPurity/20) * 10, xlab = 'Time, ms', ylab = 'AM frequency, Hz')
#' # plot roughness over time
#' spectrogram(s_long, 16000, ylim = c(0, 4),
#'   extraContour = list(ms$roughness / max(ms$roughness) * 4000, col = 'blue'))
#'
#' # As with spectrograms, there is a tradeoff in time-frequency resolution
#' s = soundgen(pitch = 500, amFreq = 50, amDep = 100,
#'              samplingRate = 44100, plot = TRUE)
#' # playme(s, samplingRate = 44100)
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 50, step = 50, amRes = NULL)  # poor temporal resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 5, step = 1, amRes = NULL)  # poor frequency resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 15, step = 3, amRes = NULL)  # a reasonable compromise
#'
#' # customize the plot
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 15, overlap = 80, amRes = NULL,
#'   kernelSize = 17,  # more smoothing
#'   xlim = c(-70, 70), ylim = c(0, 4),  # zoom in on the central region
#'   quantiles = c(.25, .5, .8),  # customize contour lines
#'   col = rev(rainbow(100)),  # alternative palette
#'   power = 2)                   # ^2
#' # Note the peaks at FM = 2/KHz (from "pitch = 500") and AM = 50 Hz (from
#' # "amFreq = 50")
#'
#' # Input can be a wav/mp3 file
#' ms = modulationSpectrum('~/Downloads/temp/16002_Faking_It_Large_clear.wav')
#'
#' # Input can be path to folder with audio files. Each file is processed
#' # separately, and the output can contain an MS per file...
#' ms1 = modulationSpectrum('~/Downloads/temp', kernelSize = 11,
#'                          plot = FALSE, averageMS = FALSE)
#' ms1$summary
#' names(ms1$original)  # a separate MS per file
#' # ...or a single MS can be calculated:
#' ms2 = modulationSpectrum('~/Downloads/temp', kernelSize = 11,
#'                          plot = FALSE, averageMS = TRUE)
#' plotMS(ms2$original)
#' ms2$summary
#'
#' # Input can also be a list of waveforms (numeric vectors)
#' ss = vector('list', 10)
#' for (i in 1:length(ss)) {
#'   ss[[i]] = soundgen(sylLen = runif(1, 100, 1000), temperature = .4,
#'     pitch = runif(3, 400, 600))
#' }
#' # lapply(ss, playme)
#' # MS of the first sound
#' ms1 = modulationSpectrum(ss[[1]], samplingRate = 16000, scale = 1)
#' # average MS of all 10 sounds
#' ms2 = modulationSpectrum(ss, samplingRate = 16000, scale = 1, averageMS = TRUE, plot = FALSE)
#' plotMS(ms2$original)
#'
#' # A sound with ~3 syllables per second and only downsweeps in F0 contour
#' s = soundgen(nSyl = 8, sylLen = 200, pauseLen = 100, pitch = c(300, 200))
#' # playme(s)
#' ms = modulationSpectrum(s, samplingRate = 16000, maxDur = .5,
#'   xlim = c(-25, 25), colorTheme = 'seewave',
#'   power = 2)
#' # note the asymmetry b/c of downsweeps
#'
#' # "power = 2" returns squared modulation spectrum - note that this affects
#' # the roughness measure!
#' ms$roughness
#' # compare:
#' modulationSpectrum(s, samplingRate = 16000, maxDur = .5,
#'   xlim = c(-25, 25), colorTheme = 'seewave',
#'   power = 1)$roughness  # much higher roughness
#'
#' # Plotting with or without log-warping the modulation spectrum:
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000, plot = TRUE)
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'   logWarpX = c(2, 2), plot = TRUE)
#'
#' # logWarp and kernelSize have no effect on roughness
#' # because it is calculated before these transforms:
#' modulationSpectrum(s, samplingRate = 16000, logWarpX = c(1, 10))$roughness
#' modulationSpectrum(s, samplingRate = 16000, logWarpX = NA)$roughness
#' modulationSpectrum(s, samplingRate = 16000, kernelSize = 17)$roughness
#'
#' # Log-transform the spectrogram prior to 2D FFT (affects roughness):
#' modulationSpectrum(s, samplingRate = 16000, logSpec = FALSE)$roughness
#' modulationSpectrum(s, samplingRate = 16000, logSpec = TRUE)$roughness
#'
#' # Complex modulation spectrum with phase preserved
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'                         returnComplex = TRUE)
#' plotMS(abs(ms$complex))  # note the symmetry
#' # compare:
#' plotMS(ms$original)
#' }
modulationSpectrum = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    amRes = 5,
    maxDur = 5,
    logSpec = FALSE,
    windowLength = 15,
    step = NULL,
    overlap = 80,
    wn = 'hanning',
    zp = 0,
    power = 1,
    normalize = TRUE,
    roughRange = c(30, 150),
    amRange = c(10, 200),
    returnMS = TRUE,
    returnComplex = FALSE,
    summaryFun = c('mean', 'median', 'sd'),
    averageMS = FALSE,
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = NULL,
    logWarpX = NULL,
    logWarpY = NULL,
    quantiles = c(.5, .8, .9),
    kernelSize = 5,
    kernelSD = .5,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    main = NULL,
    xlab = 'Hz',
    ylab = '1/KHz',
    xlim = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  ## Prepare a list of arguments to pass to .modulationSpectrum()
  myPars = c(as.list(environment()), list(...))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'savePlots',
    'reportEvery', 'cores', 'summaryFun', 'averageMS')]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.modulationSpectrum',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "MS", width = paste0(width, units)))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          data.frame(roughness = pa$result[[i]]$roughness,
                     amMsFreq = pa$result[[i]]$amMsFreq,
                     amMsPurity = pa$result[[i]]$amMsPurity),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
    idx_ok = which(!pa$input$failed)
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
  for (i in idx_failed) pa$result[[i]] = list(
    original = NULL, processed = NULL, complex = NULL,
    roughness = NULL, amMsFreq = NULL, amMsPurity = NULL
  )
  original = processed = complex = roughness = amMsFreq = amMsPurity = NULL
  # (otherwise note about no visible binding)
  out_prep = c('original', 'processed', 'complex', 'roughness', 'amMsFreq', 'amMsPurity')
  if (pa$input$n == 1) {
    # unlist
    for (op in out_prep) assign(noquote(op), pa$result[[1]] [[op]])
  } else {
    for (op in out_prep) assign(noquote(op), lapply(pa$result, function(x) x[[op]]))
  }

  # average MS across sounds
  op_prep2 = c('original', 'processed')  # roughness is not a matrix
  if (returnComplex) {
    op_prep2 = c(op_prep2, 'complex')
  } else {
    complex = NULL
  }
  if (averageMS & pa$input$n > 1) {
    for (op in op_prep2) {
      assign(
        noquote(op),
        averageMatrices(
          get(op) [idx_ok],
          rFun = 'max',   # normally same samplingRate, but if not, upsample frequency resolution
          cFun = 'median',   # typical ncol (depends on sound dur)
          reduceFun = '+')  # internally divides by length, so actually becomes mean
      )
    }
  }

  invisible(list(original = original,
                 processed = processed,
                 complex = complex,
                 roughness = roughness,
                 amMsFreq = amMsFreq,
                 amMsPurity = amMsPurity,
                 summary = mysum_all))
}


#' Modulation spectrum per sound
#'
#' Internal soundgen function.
#' @inheritParams modulationSpectrum
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.modulationSpectrum = function(
    audio,
    amRes = 5,
    maxDur = 5,
    logSpec = FALSE,
    windowLength = 15,
    step = NULL,
    overlap = 80,
    wn = 'hanning',
    zp = 0,
    power = 1,
    normalize = TRUE,
    roughRange = c(30, 150),
    amRange = c(10, 200),
    returnMS = TRUE,
    returnComplex = FALSE,
    plot = TRUE,
    savePlots = NULL,
    logWarpX = NULL,
    logWarpY = NULL,
    quantiles = c(.5, .8, .9),
    kernelSize = 5,
    kernelSD = .5,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    main = NULL,
    xlab = 'Hz',
    ylab = '1/KHz',
    xlim = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # Re-set windowLength, step, and overlap so as to ensure that
  # windowLength_points and step_points are not fractions
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  step_points = round(step / 1000 * audio$samplingRate)
  step = step_points / audio$samplingRate * 1000
  windowLength_points = round(windowLength / 1000 * audio$samplingRate)
  windowLength = windowLength_points / audio$samplingRate * 1000
  overlap = 100 * (1 - step_points / windowLength_points)

  max_am = 1000 / step / 2
  if (max_am < roughRange[1]) {
    warning(paste(
      'roughRange outside the analyzed range of temporal modulation frequencies;',
      'increase overlap / decrease step to improve temporal resolution,',
      'or else look for roughness in a lower range'))
  }
  if (is.numeric(amRes)) {
    nFrames = max(3, ceiling(max_am / amRes * 2))  # min 3 spectrograms frames
  } else {
    nFrames = NULL
  }

  if (is.numeric(nFrames)) {
    # split the input sound into chunks nFrames long
    chunk_ms = windowLength + step * (nFrames - 1)
    splitInto = max(1, ceiling(audio$duration * 1000 / chunk_ms))
    if (chunk_ms > (audio$duration * 1000)) {
      message(paste('The sound is too short to be analyzed with amRes =', amRes,
                    'Hz. Roughness is probably not measured correctly'))
      chunk_ms = audio$duration
    }
  } else {
    # split only those sounds that exceed maxDur
    splitInto = max(1, ceiling(audio$duration / maxDur))
    # so, for ex., if 2.1 times longer than maxDur, split into three
  }
  if (splitInto > 1) {
    myseq = floor(seq(1, length(audio$sound), length.out = splitInto + 1))
    myInput = vector('list', splitInto)
    for (i in 1:splitInto) {
      idx = myseq[i]:(myseq[i + 1])
      myInput[[i]] = audio$sound[idx]
    }
  } else {
    myInput = list(audio$sound)
  }

  # extract modulation spectrum per fragment
  out = vector('list', splitInto)
  if (returnComplex) {
    out_complex = out
  } else {
    out_aggreg_complex = NULL
  }
  for (i in 1:splitInto) {
    ms_i = modulationSpectrumFragment(
      myInput[[i]],
      samplingRate = audio$samplingRate,
      windowLength = windowLength,
      windowLength_points = windowLength_points,
      step = step,
      step_points = step_points,
      wn = wn,
      zp = zp,
      logSpec = logSpec,
      power = power,
      normalize = normalize)
    out[[i]] = ms_i$ms_half

    if (returnComplex) {
      out_complex[[i]] = ms_i$ms_complex
    }
  }
  keep = which(unlist(lapply(out, function(x) !is.null(x))))
  out = out[keep]
  if (length(out) < 1) {
    warning('The sound is too short or windowLength too long. Need at least 3 frames')
    return(list('original' = NA,
                'processed' = NA,
                'complex' = NA,
                'roughness' = NA,
                'amMsFreq' = NA,
                'amMsPurity' = NA))
  }

  # extract a measure of roughness
  roughness = unlist(lapply(out, function(x)
    getRough(x, roughRange, colNames = as.numeric(colnames(out[[1]])))))

  # detect systematic amplitude modulation at the same frequency
  am_list = lapply(out, function(x)
    getAM(x, amRange = amRange, amRes = amRes))
  amMsFreq = unlist(lapply(am_list, function(x) x$amMsFreq))
  amMsPurity = unlist(lapply(am_list, function(x) x$amMsPurity))

  # average modulation spectra across all sounds
  if (!returnMS) {
    result = list('original' = NULL,
                  'processed' = NULL,
                  'complex' = NULL,
                  'roughness' = roughness,
                  'amMsFreq' = amMsFreq,
                  'amMsPurity' = amMsPurity)
  } else {
    out_aggreg = averageMatrices(
      out,
      rFun = 'max',   # normally same samplingRate, but if not, upsample frequency resolution
      cFun = 'median',  # typical ncol (depends on sound dur)
      reduceFun = '+')  # internally divides by length, so actually becomes mean
    X = as.numeric(colnames(out_aggreg))
    Y = as.numeric(rownames(out_aggreg))
    # image(X, Y, t(log(out_aggreg)))

    # prepare a separate summary of the complex ms
    if (returnComplex) {
      out_aggreg_complex = averageMatrices(
        out_complex,
        rFun = 'max',
        cFun = 'median',
        reduceFun = '+')
      # image(t(log(abs(out_aggreg_complex))))
    }

    # smoothing / blurring
    out_transf = gaussianSmooth2D(out_aggreg,
                                  kernelSize = kernelSize,
                                  kernelSD = kernelSD)
    result = list('original' = out_aggreg,
                  'processed' = out_transf,
                  'complex' = out_aggreg_complex,
                  'roughness' = roughness,
                  'amMsFreq' = amMsFreq,
                  'amMsPurity' = amMsPurity)
  }  # end of if (returnMS)


  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_MS.png"),
        width = width, height = height, units = units, res = res)
  }

  if (plot) {
    plotMS(ms = out_transf,
           X = X,
           Y = Y,
           audio = audio,
           colorTheme = colorTheme,
           col = col,
           logWarpX = logWarpX,
           logWarpY = logWarpY,
           xlab = xlab, ylab = ylab,
           main = main,
           xlim = xlim, ylim = ylim,
           quantiles = quantiles,
           ...
    )
    if (is.character(audio$savePlots)) dev.off()
  }

  invisible(result)
}


#' Plot modulation spectrum
#'
#' Plots a single modulation spectrum returned by
#' \code{\link{modulationSpectrum}}. The result is the same as the plot produced
#' by \code{\link{modulationSpectrum}}, but calling \code{plotMS} is handy for
#' processed modulation spectra - for instance, for plotting the difference
#' between the modulation spectra of two sounds or groups of sounds.
#'
#' @inheritParams modulationSpectrum
#' @param ms modulation spectrum - a matrix with temporal modulation in columns
#'   and spectral modulation in rows, as returned by
#'   \code{\link{modulationSpectrum}}
#' @param X,Y rownames and colnames of \code{ms}, respectively
#' @param audio (internal) a list of audio attributes
#' @export
#' @examples
#' ms1 = modulationSpectrum(runif(4000), samplingRate = 16000, plot = TRUE)
#' plotMS(ms1$processed)  # identical to above
#'
#' # compare two modulation spectra
#' ms2 = modulationSpectrum(soundgen(sylLen = 100, addSilence = 0),
#'                          samplingRate = 16000)
#' # ensure the two matrices have the same dimensions
#' ms2_resized = soundgen:::interpolMatrix(ms2$original,
#'   nr = nrow(ms1$original), nc = ncol(ms1$original))
#' # plot the difference
#' plotMS(log(ms1$original / ms2_resized), quantile = NULL,
#'   col = colorRampPalette(c('blue', 'yellow')) (50))
plotMS = function(
    ms,
    X = NULL,
    Y = NULL,
    quantiles = c(.5, .8, .9),
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
    col = NULL,
    logWarpX = NULL,
    logWarpY = NULL,
    main = NULL,
    xlab = 'Hz',
    ylab = '1/KHz',
    xlim = NULL,
    ylim = NULL,
    audio = NULL,
    ...
) {
  if (!is.null(col)) colorTheme = NULL
  if (!is.null(colorTheme)) {
    color.palette = switchColorTheme(colorTheme)
  } else {
    color.palette = NULL
  }
  if (is.null(X)) X = as.numeric(colnames(ms))
  if (is.null(Y)) Y = as.numeric(rownames(ms))
  if (is.null(xlim)) xlim = c(X[1], -X[1])
  if (is.null(ylim)) ylim = range(Y)
  if (is.null(main) & !is.null(audio$filename_noExt)) {
    if (audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
  }

  # plot with filled.contour.mod
  ms = zeroOne(ms)
  X1 = X
  Y1 = Y
  if (is.numeric(logWarpX) | is.numeric(logWarpY)) {
    if (is.numeric(logWarpX)) {
      X1 = pseudoLog(X, sigma = logWarpX[1], base = logWarpX[2])
      # lab_x = pretty(X, n = 9, min.n = 7)
      # at_x = pseudoLog(lab_x, sigma = logWarpX[1], base = logWarpX[2])
      at_x = pretty(X1, n = 7)
      lab_x = round(pseudoLog_undo(at_x, sigma = logWarpX[1], base = logWarpX[2]))
    } else {
      lab_x = at_x = pretty(X)
    }

    if (is.numeric(logWarpY)) {
      Y1 = pseudoLog(Y, sigma = logWarpY[1], base = logWarpY[2])
      lab_y = pretty(Y)
      at_y = pseudoLog(lab_y, sigma = logWarpY[2], base = logWarpY[2])
    } else {
      lab_y = at_y = pretty(Y)
    }
    filled.contour.mod(X1, Y1, t(ms),
                       levels = seq(0, 1, length = 100),
                       color.palette = color.palette,
                       col = col,
                       xlab = xlab, ylab = ylab,
                       bty = 'n',
                       main = main, xaxt = 'n', yaxt = 'n',
                       # xlim = xlim, ylim = ylim,
                       ...)

    # add nicely labeled axes
    axis(1, at = at_x, labels = lab_x, ...)  # xpd = TRUE to extend beyond the plot
    axis(2, at = at_y, labels = lab_y, ...)

    lbl_Hz_pos = pretty(Y1)
    lbls_Hz = round(1000 / lbl_Hz_pos)
    axis(4, at = lbl_Hz_pos, labels = lbls_Hz, ...)
  } else {
    filled.contour.mod(X1, Y1, t(ms),
                       levels = seq(0, 1, length = 100),
                       color.palette = color.palette,
                       col = col,
                       xlab = xlab, ylab = ylab,
                       bty = 'n',
                       main = main,
                       # xlim = xlim, ylim = ylim,
                       ...)
    lbls = round(1000 / pretty(Y1))
    lbl_pos = 1000 / lbls
    axis(4, at = lbl_pos, labels = lbls, ...)
  }
  abline(v = 0, lty = 3)

  # add contours
  if (is.numeric(quantiles)) {
    # qntls = quantile(out_aggreg, probs = quantiles)  # could try HDI instead
    qntls = pDistr(as.numeric(ms), quantiles = quantiles)
    par(new = TRUE)
    contour(x = X1, y = Y1, z = t(ms),
            levels = qntls, labels = quantiles * 100,
            xaxs = 'i', yaxs = 'i',
            axes = FALSE, frame.plot = FALSE,
            # xlim = xlim, ylim = ylim,
            ...)
    par(new = FALSE)
  }
}


#' Modulation spectrum per fragment
#'
#' Internal soundgen function.
#' @inheritParams modulationSpectrum
#' @param sound numeric vector
#' @keywords internal
#' @examples
#' s = soundgen(amFreq = 25, amDep = 100)
#' ms = soundgen:::modulationSpectrumFragment(s, 16000,
#'   windowLength = 50, windowLength_points = .05 * 16000,
#'   step = 5, step_points = .005 * 16000)
#' image(as.numeric(colnames(ms$ms_half)), as.numeric(rownames(ms$ms_half)),
#'       t(log(ms$ms_half)))
modulationSpectrumFragment = function(sound,
                                      samplingRate,
                                      windowLength,
                                      windowLength_points,
                                      step,
                                      step_points,
                                      wn = 'hanning',
                                      zp = 0,
                                      logSpec = FALSE,
                                      power = 1,
                                      normalize = TRUE) {
  # Calling stdft is ~80 times faster than going through spectrogram (!)
  step_seq = seq(1, length(sound) + 1 - windowLength_points, step_points)
  # print(length(step_seq))
  if (length(step_seq) < 3) return(NULL)
  s1 = seewave::stdft(
    wave = as.matrix(sound),
    wn = wn,
    wl = windowLength_points,
    f = samplingRate,
    zp = zp,
    step = step_seq,
    scale = FALSE,
    norm = FALSE,
    complex = FALSE
  )
  # image(t(log(s1))); s1[1:3, 1:3]; dim(s1); range(s1)

  # log-transform amplitudes
  if (logSpec) {
    positives = which(s1 > 0)
    nonpositives = which(s1 <= 0)
    s1[positives] = log(s1[positives])
    if (length(positives) > 0 & length(nonpositives) > 0) {
      s1[nonpositives] = min(s1[positives])
    }
    s1 = s1 - min(s1) + 1e-16  # positive
    # image(t(log(s1)))
  }

  # 2D fft
  ms_complex = specToMS(s1, windowLength = windowLength, step = step)
  ms = abs(ms_complex)
  # image(as.numeric(colnames(ms)), as.numeric(rownames(ms)), t(log(ms)))
  symAxis = floor(nrow(ms) / 2) + 1
  # ms[(symAxis - 2) : (symAxis + 2), 1:2]
  ms_half = ms[symAxis:nrow(ms),, drop = FALSE]  # take only the upper half (always even)

  # power
  if (is.numeric(power) && power != 1) ms_half = ms_half ^ power

  # normalize
  if (normalize && any(s1 != 0)) {
    ms_half = ms_half - min(ms_half)
    ms_half = ms_half / max(ms_half)
  }
  # image(x = as.numeric(colnames(ms_half)), z = t(log(ms_half)))
  return(list(
    ms_half = ms_half,
    ms_complex = ms_complex
  ))
}


#' Get roughness
#'
#' Internal soundgen function
#'
#' Helper function for calculating roughness - the proportion of energy /
#' amplitude in the roughness range
#' @param m numeric matrix of non-negative values with colnames giving temporal
#'   modulation frequency
#' @inheritParams modulationSpectrum
#' @param colNames numeric vector with frequencies corresponding to columns
#' @return Returns roughness in percent.
#' @keywords internal
#' @examples
#' m = modulationSpectrum(soundgen(), samplingRate = 16000)$original
#' soundgen:::getRough(m, roughRange = c(30, Inf))
getRough = function(m, roughRange, colNames = NULL) {
  if (is.null(colNames)) colNames = abs(as.numeric(colnames(m)))
  rough_cols = which(colNames > roughRange[1] &
                       colNames < roughRange[2])
  if (length(rough_cols) > 0) {
    roughness = sum(m[, rough_cols]) / sum(m) * 100
  } else {
    roughness = 0
  }
  # # alternatively / in addition, could try gaussian filter when calculating roughness
  # ampl_filter = dnorm(colNames, mean = roughMean, sd = roughSD)
  # ampl_filter = ampl_filter / sum(ampl_filter)  # to pdf
  # # plot(ampl_filter, type = 'l')
  # roughness = sum(colSums(m) * ampl_filter) / sum(m) * 100
  return(roughness)
}


#' Average matrices
#'
#' Internal soundgen function.
#'
#' Takes a list of matrices (normally modulation spectra), interpolates them to
#' have the same size, and then reduces them (eg takes the average).
#' @param mat_list a list of matrices to aggregate (eg spectrograms or
#'   modulation spectra)
#' @param rFun, cFun functions used to determine the number of rows and columns
#'   in the result
#' @param reduceFun function used to aggregate
#' @keywords internal
#' @examples
#' mat_list = list(
#'   matrix(1:30, nrow = 5),
#'   matrix(80:17, nrow = 8)
#' )
#' soundgen:::averageMatrices(mat_list)
#' soundgen:::averageMatrices(mat_list, cFun = 'max', reduceFun = '*')
averageMatrices = function(mat_list,
                           rFun = 'max',
                           cFun = 'median',
                           reduceFun = '+') {
  # normally same samplingRate, but in case not, upsample frequency resolution
  nr = round(do.call(rFun, list(unlist(lapply(mat_list, nrow)))))
  # take typical ncol (depends on sound dur)
  nc = round(do.call(cFun, list(unlist(lapply(mat_list, ncol)))))
  mat_list_sameDim = lapply(mat_list, function(x) interpolMatrix(x, nr = nr, nc = nc))
  agg = Reduce(reduceFun, mat_list_sameDim) / length(mat_list)
  return(agg)
}


#' Get amplitude modulation
#'
#' Internal soundgen function
#'
#' Helper function for calculating amplitude modulation based on the modulation
#' spectrum. Algorithm: averages AM across all FM bins in the positive half of
#' the modulation spectrum and looks for a peak in the specified AM frequency
#' range.
#' @param m numeric matrix of non-negative values with colnames giving temporal
#'   modulation frequency
#' @inheritParams modulationSpectrum
#' @param amRes controls the width of window over which we look for local maxima
#' @return Returns a list with the frequency (Hz) and depth of amplitude
#'   modulation (dB relative to global max, normally at 0 Hz).
#' @keywords internal
getAM = function(m,
                 amRange = c(10, 100),
                 amRes = NULL) {
  if (is.null(amRes)) amRes = 0
  colNames = abs(as.numeric(colnames(m)))
  out = list(amMsFreq = NA, amMsPurity = NA)
  # image(t(log(m)))

  # fold around 0 and average AM across all FM bins
  am = data.frame(freq = abs(colNames), amp = colSums(m))
  am = am[order(am$freq), ]
  # plot(am, type = 'l')

  # average folded AM function cross pos/neg AM frequencies
  # (b/c each point is doubled)
  am_sm = am
  i = 1
  while(i < nrow(am_sm)) {
    if (abs(am_sm$freq[i] - am_sm$freq[i + 1]) < .1) {
      am_sm$amp[i] = (am_sm$amp[i] + am_sm$amp[i + 1]) / 2
      am_sm$amp[i + 1] = NA
      i = i + 2
    } else {
      i = i + 1
    }
  }
  am_sm = na.omit(am_sm)
  # plot(am, type = 'l')
  # lines(am_sm, type = 'l', col = 'blue')

  # find local maxima within amRange (note that we don't just throw away the
  # rest of ms to improve the precision of smoothed AM function)
  am_smRan = am_sm[am_sm$freq >= amRange[1] & am_sm$freq <= amRange[2], ]
  # plot(am_smRan, type = 'l')
  wl = max(3, round(amRes / (colNames[2] - colNames[1])))
  # eg if amRes = 10, we look for a local maximum within ±5 Hz
  temp = zoo::rollapply(
    zoo::as.zoo(am_smRan$amp),
    width = wl,  # width 10 Hz, ie ±5 Hz
    align = 'center',
    function(x) {
      middle = ceiling(length(x) / 2)
      return(which.max(x) == middle)
    })
  idx = zoo::index(temp)[zoo::coredata(temp)]

  if (length(idx) > 0) {
    peaks = am_smRan[idx, ]
    peaks = peaks[which.max(peaks$amp), ]
    if (nrow(peaks) > 0) {
      out$amMsFreq = peaks$freq
      out$amMsPurity = log10(peaks$amp / max(am_sm$amp)) * 20
    }
  }
  return(out)
}


#' Log-warp a modulation spectrum
#'
#' Internal soundgen function
#'
#' Log-warps a modulation spectrum along time dimension
#'
#' @param x a modulation spectrum: rows = FM, cols = AM
#' @keywords internal
#' @examples
#' a = matrix(1:44, ncol = 11)
#' colnames(a) = -5:5
#' soundgen:::logWarpMS(a, logWarp = 2)
logWarpMS = function(x, logWarp) {
  X = as.numeric(colnames(x))
  neg_col = which(X < 0)
  zero_col = which(X == 0)
  pos_col = which(X > 0)
  m_left = logMatrix(x[, rev(neg_col)], base = logWarp)
  # NB: flip the left half!
  m_right = logMatrix(x[, pos_col], base = logWarp)
  x_transf = cbind(m_left[, ncol(m_left):1], x[, zero_col, drop = FALSE], m_right)
  return(x_transf)
}
