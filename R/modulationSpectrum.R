### MODULATION SPECTRUM

#' Modulation spectrum
#'
#' Produces a modulation spectrum of waveform(s) or audio file(s). It begins
#' with some spectrogram-like time-frequency representation and analyzes the
#' modulation of the envelope in each frequency band. if \code{specSource =
#' 'audSpec'}, the sound is passed through a bank of bandpass filters with
#' \code{\link{audSpectrogram}}. If \code{specSource = 'STFT'}, we begin with an
#' ordinary spectrogram produced with a Short-Time Fourier Transform. If
#' \code{msType = '2D'}, the modulation spectrum is a 2D Fourier transform of
#' the spectrogram-like representation, with temporal modulation along the X
#' axis and spectral modulation along the Y axis. A good visual analogy is
#' decomposing the spectrogram into a sum of ripples of various frequencies and
#' directions. If \code{msType = '1D'}, the modulation spectrum is a matrix
#' containing 1D Fourier transforms of each frequency band in the spectrogram,
#' so the result again has modulation frequencies along the X axis, but the Y
#' axis now shows the frequency of each analyzed band. Roughness is calculated
#' as the proportion of the modulation spectrum within \code{roughRange} of
#' temporal modulation frequencies or some weighted version thereof. The
#' frequency of amplitude modulation (amMsFreq, Hz) is calculated as the highest
#' peak in the smoothed AM function, and its purity (amMsPurity, dB) as the
#' ratio of this peak to the median AM over \code{amRange}. For relatively short
#' and steady sounds, set \code{amRes = NULL} and analyze the entire sound. For
#' longer sounds and when roughness or AM vary over time, set \code{amRes} to
#' get multiple measurements over time (see examples). For multiple inputs, such
#' as a list of waveforms or path to a folder with audio files, the ensemble of
#' modulation spectra can be interpolated to the same spectral and temporal
#' resolution and averaged (if \code{averageMS = TRUE}).
#'
#' @seealso \code{\link{plotMS}} \code{\link{spectrogram}}
#'   \code{\link{audSpectrogram}} \code{\link{analyze}}
#'
#' @references \itemize{
#'   \item Singh, N. C., & Theunissen, F. E. (2003). Modulation spectra of
#'   natural sounds and ethological theories of auditory processing. The Journal
#'   of the Acoustical Society of America, 114(6), 3394-3411.
#'   \item Anikin, A. (2025) Acoustic estimation of voice roughness. Attention,
#'   Perception, & Psychophysics 87: 1771–1787.
#' }
#' @inheritParams spectrogram
#' @inheritParams analyze
#' @param msType '2D' = two-dimensional Fourier transform of a spectrogram; '1D'
#'   = separately calculated spectrum of each frequency band
#' @param specSource 'STFT' = Short-Time Fourier Transform; 'audSpec' = a bank
#'   of bandpass filters (see \code{\link{audSpectrogram}})
#' @param windowLength,step,wn,zp parameters for extracting a spectrogram if
#'   \code{specType = 'STFT'}. Window length and step are specified in ms (see
#'   \code{\link{spectrogram}}). If \code{specType = 'audSpec'}, these settings
#'   have no effect
#' @param audSpec_pars parameters for extracting an auditory spectrogram if
#'   \code{specType = 'audSpec'}. If \code{specType = 'STFT'}, these settings
#'   have no effect
#' @param roughRange the range of temporal modulation frequencies that
#'   constitute the "roughness" zone, Hz
#' @param roughMean,roughSD the mean (Hz) and standard deviation (semitones) of
#'   a lognormal distribution used to weight roughness estimates. If either is
#'   null, roughness is calculated simply as the proportion of spectrum within
#'   \code{roughRange}. If both \code{roughMean} and \code{roughRange} are
#'   defined, weights outside \code{roughRange} are set to 0; a very large SD (a
#'   flat weighting function) gives the same result as just \code{roughRange}
#'   without any weighting (see examples)
#' @param roughMinFreq frequencies below roughMinFreq (Hz) are ignored when
#'   calculating roughness (ie the estimated roughness increases if we disregard
#'   very low-frequency modulation, which is often strong)
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
#' @param specMethod the function to call when calculating the spectrum of each
#'   frequency band (only used when \code{msType = '1D'}); 'meanspec' is faster
#'   and less noisy, whereas 'spec' produces higher resolution
#' @param logSpec if TRUE, the spectrogram is log-transformed prior to taking 2D
#'   FFT
#' @param logMPS if TRUE, the modulation spectrum is log-transformed prior to
#'   calculating roughness
#' @param power raise modulation spectrum to this power (eg power = 2 for ^2, or
#'   "power spectrum")
#' @param normalize if TRUE, the modulation spectrum of each analyzed fragment
#'   \code{maxDur} in duration is separately normalized to have max = 1
#' @param returnMS if FALSE, only roughness is returned (much faster). Careful
#'   with exporting the modulation spectra of a lot of sounds at once as this
#'   requires a lot of RAM
#' @param returnComplex if TRUE, returns a complex modulation spectrum (without
#'   normalization and warping)
#' @param averageMS if TRUE, the modulation spectra of all inputs are averaged
#'   into a single output; if FALSE, a separate MS is returned for each input
#' @param plot if TRUE, plots the modulation spectrum of each sound (see
#'   \code{\link{plotMS}})
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
#' \item \code{$summary} dataframe with summaries of roughness, amMsFreq, and
#' amMsPurity
#' \item \code{$original} modulation spectrum prior to blurring and log-warping,
#' but after squaring if \code{power = TRUE}, a matrix of nonnegative values.
#' Colnames are temporal modulation frequencies (Hz). Rownames are spectral
#' modulation frequencies (cycles/kHz) if \code{msType = '2D'} and frequencies
#' of filters or spectrograms bands (kHz) if \code{msType = '1D'}.
#' \item \code{$original_list} a list of modulation spectra for each analyzed
#' fragment (is \code{amRes} is not NULL)
#' \item \code{$modulation_spectrogram} a spectrogram-like representation showing
#' how the modulation spectrum changes over time
#' \item \code{$processed} modulation spectrum after blurring and log-warping
#' \item \code{$complex} untransformed complex modulation spectrum (returned
#' only if returnComplex = TRUE)
#' \item \code{$roughness} proportion of the modulation spectrum within
#' \code{roughRange} of temporal modulation frequencies or a weighted average
#' thereof if \code{roughMean} and \code{roughSD} are defined, \% - a vector if
#' amRes is numeric and the sound is long enough, otherwise a single number
#' \item \code{$roughness_list} a list containing frequencies, amplitudes, and
#' roughness values for each analyzed frequency band (1D) or frequency
#' modulation band (2D)
#' \item \code{roughness_spectrogram} a spectrogram showing roughness instead of
#' amplitude per time-frequency bin
#' \item \code{$amMsFreq} frequency of the highest peak, within \code{amRange}, of
#' the folded AM function (average AM across all FM bins for both negative and
#' positive AM frequencies), where a peak is a local maximum over \code{amRes}
#' Hz. Like \code{roughness}, \code{amMsFreq} and \code{amMsPurity} can be single
#' numbers or vectors, depending on whether the sound is analyzed as a whole or
#' in chunks
#' \item \code{$amMsPurity} ratio of the peak at amMsFreq to the median AM over
#' \code{amRange}, dB
#' \item \code{$ampl} Room Mean Square amplitude of each analyzed fragment
#' }
#' @export
#' @examples
#' # White noise
#' ms = modulationSpectrum(rnorm(16000), samplingRate = 16000,
#'   logSpec = FALSE, power = TRUE,
#'   amRes = NULL)  # analyze the entire sound, giving a single roughness value
#' str(ms)
#'
#' # Harmonic sound
#' s = soundgen(pitch = 440, amFreq = 100, amDep = 50)
#' ms = modulationSpectrum(s, samplingRate = 16000, amRes = NULL)
#' ms[c('roughness', 'amMsFreq', 'amMsPurity')]  # a single value for each
#' ms1 = modulationSpectrum(s, samplingRate = 16000, amRes = 5)
#' ms1[c('roughness', 'amMsFreq', 'amMsPurity')]
#' # measured over time (low values of amRes mean more precision, so we analyze
#' # longer segments and get fewer values per sound)
#'
#' # Embellish
#' ms = modulationSpectrum(s, samplingRate = 16000, logMPS = TRUE,
#'   xlab = 'Temporal modulation, Hz', ylab = 'Spectral modulation, 1/kHz',
#'   colorTheme = 'matlab', main = 'Modulation spectrum', lty = 3)
#'
#' # Plot a modulation spectrogram (the peak at 100 Hz shows AM)
#' spectrogram(s, 16000, specManual = ms$modulation_spectrogram,
#'   ylab = 'Modulation frequency, kHz', main = 'Modulation spectrogram')
#'
#' # Plot a roughness spectrogram
#' spectrogram(s, 16000, specManual = ms$roughness_spectrogram, yScale = 'ERB',
#'   main = 'Roughness spectrogram')
#'
#' # 1D instead of 2D
#' modulationSpectrum(s, 16000, msType = '1D', quantiles = NULL,
#'   col = soundgen:::jet.col(50))
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
#'   extraContour = list(x = ms$roughness / max(ms$roughness) * 4000, col = 'blue'))
#'
#' # As with spectrograms, there is a tradeoff in time-frequency resolution
#' s = soundgen(pitch = 500, amFreq = 50, amDep = 100, sylLen = 500,
#'              samplingRate = 44100, plot = TRUE)
#' # playme(s, samplingRate = 44100)
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 50, step = 50, amRes = NULL)  # poor temporal resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 5, step = 1, amRes = NULL)  # poor frequency resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 15, step = 3, amRes = NULL)  # a reasonable compromise
#'
#' # Start with an auditory spectrogram instead of STFT
#' modulationSpectrum(s, 44100, specSource = 'audSpec', xlim = c(-100, 100))
#' modulationSpectrum(s, 44100, specSource = 'audSpec',
#'   logWarpX = c(10, 2), xlim = c(-500, 500),
#'   audSpec_pars = list(nFilters = 32, filterType = 'gammatone', bandwidth = NULL))
#'
#' # customize the plot
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 15, step = 2, amRes = NULL,
#'   kernelSize = 17,  # more smoothing
#'   xlim = c(-70, 70), ylim = c(0, 4),  # zoom in on the central region
#'   quantiles = c(.25, .5, .8),  # customize contour lines
#'   col = rev(rainbow(100)),  # alternative palette
#'   logWarpX = c(10, 2),  # pseudo-log transform
#'   power = 2)                   # ^2
#' # Note the peaks at FM = 2/kHz (from "pitch = 500") and AM = 50 Hz (from
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
#' for (i in seq_along(ss)) {
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
#' # Use a lognormal weighting function to calculate roughness
#' # (instead of just % in roughRange)
#' modulationSpectrum(s, 16000, roughRange = NULL,
#'   roughMean = 75, roughSD = 3)$roughness
#' modulationSpectrum(s, 16000, roughRange = NULL,
#'   roughMean = 100, roughSD = 12)$roughness
#' # truncate weights outside roughRange
#' modulationSpectrum(s, 16000, roughRange = c(30, 150),
#'   roughMean = 100, roughSD = 1000)$roughness  # very large SD
#' modulationSpectrum(s, 16000, roughRange = c(30, 150),
#'   roughMean = NULL)$roughness  # same as above b/c SD --> Inf
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
    msType = c('1D', '2D')[2],
    specSource = c('STFT', 'audSpec')[1],
    windowLength = 15,
    step = 1,
    wn = 'hanning',
    zp = 0,
    audSpec_pars = list(filterType = 'butterworth',
                        nFilters = 32,
                        bandwidth = 1/24,
                        yScale = 'bark',
                        dynamicRange = 120),
    amRes = 5,
    maxDur = 5,
    specMethod = c('spec', 'meanspec')[2],
    logSpec = FALSE,
    logMPS = FALSE,
    power = 1,
    normalize = TRUE,
    roughRange = c(30, 150),
    roughMean = NULL,
    roughSD = NULL,
    roughMinFreq = 1,
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
    ylab = NULL,
    xlim = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # default labels for y-axis
  if (is.null(ylab)) {
    if (specSource == 'STFT') {
      ylab = ifelse(msType == '2D', '1/kHz', 'kHz')
    } else {
      if (msType == '2D') {
        ylab = if (is.null(audSpec_pars$yScale)) '' else paste0('1/', audSpec_pars$yScale)
      } else {
        ylab = 'kHz'
      }
    }
  }

  ## Prepare a list of arguments to pass to .modulationSpectrum()
  myPars = c(as.list(environment()), list(...))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'savePlots',
    'reportEvery', 'cores', 'summaryFun', 'averageMS', 'audSpec_pars')]
  myPars$audSpec_pars = audSpec_pars

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
  original = original_list = processed = complex = roughness = roughness_list =
    modulation_spectrogram = roughness_spectrogram =
    amMsFreq = amMsPurity = ampl = NULL
  # (otherwise note about no visible binding)
  out_prep = c('original', 'original_list', 'modulation_spectrogram',
               'processed', 'complex', 'roughness', 'roughness_spectrogram',
               'roughness_list', 'amMsFreq', 'amMsPurity', 'ampl')
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
                 original_list = original_list,
                 modulation_spectrogram = modulation_spectrogram,
                 processed = processed,
                 complex = complex,
                 roughness = roughness,
                 roughness_list = roughness_list,
                 roughness_spectrogram = roughness_spectrogram,
                 amMsFreq = amMsFreq,
                 amMsPurity = amMsPurity,
                 ampl = ampl,
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
    specSource = c('STFT', 'audSpec')[1],
    windowLength = 15,
    step = 1,
    wn = 'hanning',
    zp = 0,
    audSpec_pars = list(filterType = 'butterworth',
                        nFilters = 32,
                        bandwidth = 1/24,
                        yScale = 'bark',
                        dynamicRange = 120),
    msType = c('1D', '2D')[2],
    amRes = 5,
    maxDur = 5,
    specMethod = c('spec', 'meanspec')[2],
    logSpec = FALSE,
    logMPS = FALSE,
    power = 1,
    normalize = TRUE,
    roughRange = c(30, 150),
    roughMean = NULL,
    roughSD = NULL,
    roughMinFreq = 1,
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
    ylab = '1/kHz',
    xlim = NULL,
    ylim = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # Re-set windowLength and step  to ensure that
  # windowLength_points and step_points are not fractions
  # f (is.null(step)) step = windowLength * (1 - overlap / 100)
  step_points = round(step / 1000 * audio$samplingRate)
  step = step_points / audio$samplingRate * 1000
  windowLength_points = round(windowLength / 1000 * audio$samplingRate)
  windowLength = windowLength_points / audio$samplingRate * 1000
  lowestFreq = if (is.null(roughRange)) 5 else min(5, roughRange[1])

  max_am = 1000 / step / 2
  if (!is.null(roughRange) && max_am < roughRange[1]) {
    warning(paste(
      'roughRange outside the analyzed range of temporal modulation frequencies;',
      'decrease step to improve temporal resolution ',
      'or look for roughness in a lower range'))
  }
  if (is.numeric(amRes)) {
    if (specSource == 'STFT') {
      nFrames = max(3, ceiling(max_am / amRes * 2))  # min 3 spectrograms frames
      # split the input sound into chunks nFrames long
      chunk_ms = windowLength + step * (nFrames - 1)
      if (amRes < (1/audio$duration)) {
        message(paste('The sound is too short to be analyzed with amRes =', amRes,
                      'Hz. Actual amRes ~= ', round(1/audio$duration, 2)))
        splitInto = 1
      } else {
        splitInto = max(1, ceiling(audio$duration * 1000 / chunk_ms))
      }
    } else {
      # split into as many chunks as needed (each chunk's dur = 1/amRes s)
      splitInto = max(1, round(audio$duration * amRes))
    }
  } else {
    # split only those sounds that exceed maxDur
    splitInto = max(1, ceiling(audio$duration / maxDur))
    # so, for ex., if 2.1 times longer than maxDur, split into three
  }

  if (splitInto > 1) {
    myseq = floor(seq(1, audio$ls, length.out = splitInto + 1))
    midpoints = myseq[1:splitInto] + (myseq[2] - myseq[1]) / 2
    myInput = vector('list', splitInto)
    for (i in 1:splitInto) {
      idx = myseq[i]:(myseq[i + 1])
      myInput[[i]] = audio$sound[idx]
    }
  } else {
    myInput = list(audio$sound)
  }

  # extract modulation spectrum per fragment
  ampl = rep(NA, splitInto)
  out = vector('list', splitInto)
  if (splitInto > 1) names(out) = midpoints / audio$ls * audio$dur
  if (returnComplex) {
    out_complex = out
  } else {
    out_aggreg_complex = NULL
  }
  for (i in 1:splitInto) {
    ms_i = modulationSpectrumFragment(
      myInput[[i]],
      samplingRate = audio$samplingRate,
      specSource = specSource,
      audSpec_pars = audSpec_pars,
      msType = msType,
      windowLength = windowLength,
      windowLength_points = windowLength_points,
      step = step,
      step_points = step_points,
      lowestFreq = lowestFreq,
      wn = wn,
      zp = zp,
      specMethod = specMethod,
      logSpec = logSpec,
      logMPS = logMPS,
      power = power,
      normalize = normalize)
    out[[i]] = ms_i$ms_half
    ampl[i] = ms_i$ampl

    if (returnComplex) {
      out_complex[[i]] = ms_i$ms_complex
    }
  }
  keep = which(unlist(lapply(out, function(x) !is.null(x))))
  out = out[keep]
  if (length(out) < 1) {
    warning('The sound is too short or windowLength too long. Need at least 3 frames')
    return(list('original' = NA,
                'original_list' = NA,
                'modulation_spectrogram' = NA,
                'processed' = NA,
                'complex' = NA,
                'roughness' = NA,
                'roughness_list' = NA,
                'roughness_spectrogram' = NA,
                'amMsFreq' = NA,
                'amMsPurity' = NA,
                'ampl' = NA))
  }

  # standardize the size of matrices for ease of processing
  if (length(out) > 1) {
    ncs = unlist(lapply(out, ncol))
    nc_max = max(ncs)
    for (i in seq_along(out)) {
      if (ncol(out[[i]]) != nc_max)
        out[[i]] = interpolMatrix(out[[i]], nc = nc_max)
    }
  }

  # concatenate freq-averaged modulation spectra into a modulation spectrogram
  if (length(out) < 2) {
    mg = NA
  } else {
    mg = do.call(cbind, lapply(out, colSums))
    colnames(mg) = as.numeric(names(out)) * 1000 # time in ms
    rownames(mg) = as.numeric(colnames(out[[1]])) / 1000 # modulation frequency in kHz
  }

  # extract a measure of roughness
  drop_any = !is.null(roughMinFreq) && is.finite(roughMinFreq) && roughMinFreq > 0
  if (drop_any) {
    am_drop = which(abs(as.numeric(colnames(out[[1]]))) < roughMinFreq)
    if (!length(am_drop) > 0) drop_any = FALSE
  }
  if (drop_any) {
    roughness_list = lapply(out, function(x)
      getRough(x[, -am_drop, drop = FALSE], roughRange = roughRange,
               roughMean = roughMean, roughSD = roughSD))
  } else {
    roughness_list = lapply(out, function(x)
      getRough(x, roughRange = roughRange,
               roughMean = roughMean, roughSD = roughSD))
  }
  roughness = unlist(lapply(roughness_list, function(x) sum(x$roughness, na.rm = TRUE)))

  # concatenate roughness per frequency bin over time into a roughness spectrogram
  rg = do.call(cbind, lapply(roughness_list, function(x) x$roughness))
  colnames(rg) = as.numeric(names(out)) * 1000  # time in ms
  rownames(rg) = rownames(out[[1]])  # frequency in kHz

  # detect systematic amplitude modulation at the same frequency
  am_list = lapply(out, function(x)
    getAM(x, amRange = amRange, amRes = amRes))
  amMsFreq = unlist(lapply(am_list, function(x) x$amMsFreq))
  amMsPurity = unlist(lapply(am_list, function(x) x$amMsPurity))

  # average modulation spectra across all sounds
  if (!returnMS) {
    result = list('original' = NULL,
                  'original_list' = NULL,
                  'modulation_spectrogram' = NULL,
                  'processed' = NULL,
                  'complex' = NULL,
                  'roughness' = roughness,
                  'roughness_list' = roughness_list,
                  'roughness_spectrogram' = NULL,
                  'amMsFreq' = amMsFreq,
                  'amMsPurity' = amMsPurity,
                  'ampl' = ampl / audio$scale)
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
                  'original_list' = out,
                  'modulation_spectrogram' = mg,
                  'processed' = out_transf,
                  'complex' = out_aggreg_complex,
                  'roughness' = roughness,
                  'roughness_list' = roughness_list,
                  'roughness_spectrogram' = rg,
                  'amMsFreq' = amMsFreq,
                  'amMsPurity' = amMsPurity,
                  'ampl' = ampl / audio$scale)
  }  # end of if (returnMS)


  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_MS.png"),
        width = width, height = height, units = units, res = res)
  }

  if (plot) {
    if (nrow(out_transf) > 1) {
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
             extraY = (msType == '2D' && specSource == 'STFT'),
             ...)
    } else {
      plot(X, out_transf, type = 'l', xlab = xlab, ylab = ylab,
           main = main, xlim = xlim, ylim = ylim, ...)
    }
    if (is.character(audio$savePlots)) dev.off()
  }

  invisible(result)
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
#' plotMS(ms$ms_half)
#' image(as.numeric(colnames(ms$ms_half)), as.numeric(rownames(ms$ms_half)),
#'       t(log(ms$ms_half)))
modulationSpectrumFragment = function(
    sound,
    samplingRate,
    specSource = 'STFT',
    audSpec_pars = NULL,
    msType = c('2D', '1D')[1],
    windowLength,
    windowLength_points,
    step,
    step_points,
    lowestFreq,
    wn = 'hanning',
    zp = 0,
    specMethod = c('spec', 'meanspec')[2],
    logSpec = FALSE,
    logMPS = FALSE,
    power = 1,
    normalize = TRUE) {
  if (specSource == 'audSpec') {
    if (!is.null(audSpec_pars$nFilters) && audSpec_pars$nFilters == 1) {
      # just take Hilbert envelope of the original sound
      s1 = matrix(Mod(seewave::hilbert(
        sound,
        f = samplingRate,  # not actually needed
        fftw = FALSE)), nrow = 1)
      msType = '1D'  # can't do 2D with a single filter
    } else {
      audSpec = do.call(audSpectrogram, c(audSpec_pars, list(
        x = sound, samplingRate = samplingRate, plot = FALSE)))
      s1 = t(do.call(cbind, audSpec$filterbank_env))
      rownames(s1) = rownames(audSpec$audSpec)
    }
  } else if (specSource == 'STFT') {
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
    )  # rows = freqs, cols = time
    rownames(s1) = seq(0, samplingRate / 2, length.out = nrow(s1))
  }
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
    # image(t(s1))
  }

  # spectrogram to modulation spectrum
  if (msType == '2D') {
    # 2D FFT
    if (specSource == 'STFT') {
      ms_complex = specToMS(s1, windowLength = windowLength, step = step)
    } else {
      # audSpec - need to set frequency labels
      ms_complex = suppressMessages(
        specToMS(s1, windowLength = NULL, step = 1000/samplingRate))
      rownames(ms_complex) = seq(-1, 1, length.out = nrow(ms_complex))
    }
    if (logMPS) {
      ms = log(abs(ms_complex))
    } else {
      ms = abs(ms_complex)
    }
    symAxis = floor(nrow(ms) / 2) + 1
    # ms[(symAxis - 2) : (symAxis + 2), 1:2]
    ms_half = ms[symAxis:nrow(ms),, drop = FALSE]  # take only the upper half (always even)
  } else if (msType == '1D') {
    # 1D FFT
    sr_s1 = ifelse(specSource == 'STFT', 1000/step, samplingRate)
    ms_complex = NA
    ms_half = specToMS_1D(s1, windowLength = 1000 / lowestFreq,
                          samplingRate = sr_s1, method = specMethod)
    if (logMPS) ms_half = log(ms_half)
  }

  # power
  if (is.numeric(power) && power != 1) ms_half = ms_half ^ power

  # normalize
  if (normalize && diff(range(ms_half)) != 0) {
    ms_half = ms_half - min(ms_half)  # can be negative if logMPS = TRUE
    ms_half = ms_half / max(ms_half) # or sum(ms_half)
  }
  # image(x = as.numeric(colnames(ms_half)), z = t(log(ms_half)))
  # plotMS(log(ms_half + 1e-6), quantiles = NULL, colorTheme = 'matlab', logWarpX = c(10, 2))
  list(
    ms_half = ms_half,
    ms_complex = ms_complex,
    ampl = sqrt(mean(sound^2))
  )
}
