### MODULATION SPECTRUM

#' Modulation spectrum
#'
#' Produces a modulation spectrum of waveform(s) or audio file(s). It begins
#' with a spectrogram-like time-frequency representation and analyzes the
#' modulation of the envelope in each frequency band. If \code{specFun =
#' 'audSpec'}, the sound is passed through a bank of bandpass filters with
#' \code{\link{audSpectrogram}}. If \code{specFun = 'STFT'}, we begin with an
#' ordinary spectrogram produced with a Short-Time Fourier Transform. If
#' \code{msType = '2D'}, the modulation spectrum is a 2D Fourier transform of
#' the spectrogram-like representation, with temporal modulation along the X
#' axis and spectral modulation along the Y axis. A good visual analogy is
#' decomposing the spectrogram into a sum of ripples of various frequencies and
#' directions. If \code{msType = '1D'}, the modulation spectrum is a matrix
#' containing 1D Fourier transforms of each frequency band in the spectrogram,
#' so the result again has modulation frequencies along the X axis, but the Y
#' axis now shows the frequency of each analyzed band.
#'
#' To calculate roughness and fluctuation depth, the symmetric modulation
#' spectrum is folded by averaging its ±AM halves. Roughness is calculated as
#' the proportion of the modulation spectrum within \code{roughRange} of
#' temporal modulation frequencies or as a weighted version thereof. Fluctuation
#' is calculated in the same manner as roughness, but in a lower frequency range
#' of ~4 Hz (0.25 to 30 Hz). The frequency of amplitude modulation
#' (\code{amMsFreq}, Hz) is calculated as the highest peak of the folded AM
#' function within \code{amRange}, and its purity (\code{amMsPurity}, dB) as the
#' ratio of this peak to the average of the folded AM function around the peak
#' within amRange. For relatively short and steady sounds, set \code{amRes =
#' NULL} and analyze the entire sound. For longer sounds and when roughness or
#' AM vary over time, set \code{amRes} to get multiple measurements over time,
#' but inspect the MS visually to make sure the relevant modulation frequencies
#' are resolved. For multiple inputs, such as a list of waveforms or a path to a
#' folder with audio files, the ensemble of modulation spectra can be
#' interpolated to the same spectral and temporal resolution and averaged if
#' \code{averageMS = TRUE}.
#'
#' The full STFT or auditory spectrogram is computed once and then sliced into
#' temporal chunks. Roughness, fluctuation strength, \code{amMsFreq}, and
#' \code{amMsPurity} are calculated from the per-fragment modulation spectra
#' before Gaussian smoothing and before any log-warping used for plotting.
#' Log-warps specified via \code{logWarpX} and \code{logWarpY} affect the plot
#' only; the returned matrices are not log-warped.
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
#'
#' @inheritParams .roxygen_defaults
#' @param msType '2D' = two-dimensional Fourier transform of a spectrogram;
#'   '1D' = separately calculated spectrum of each frequency band
#' @param specFun 'STFT' or 'stft' = ordinary \code{\link{spectrogram}},
#'   'audSpec' = auditory spectrogram with \code{\link{audSpectrogram}}, 'env' =
#'   amplitude envelope with \code{\link{getEnv}}
#' @param specFun_pars a list of parameters passed to \code{specFun}. Defaults:
#'   STFT \code{list(windowLength = 15, overlap = 90, wn = 'hanning', zp =
#'   NULL)} (window length and step are specified in ms), audSpec
#'   \code{list(yScale = 'ERB', step = 1, envelope = 'hil', nFilters = 32)}, env
#'   \code{list()}. Whatever your \code{specFun}, careful with choosing the time
#'   step as it determines the temporal modulation resolution
#' @param roughRange the range of temporal modulation frequencies that
#'   constitute the "roughness" zone, Hz. If \code{NULL}, roughness can still
#'   be calculated using \code{roughMean} and \code{roughSD}
#' @param roughMean,roughSD the mean (Hz) and standard deviation (semitones) of
#'   a lognormal distribution used to weight roughness estimates. Weighted
#'   roughness requires both \code{roughMean} and \code{roughSD}. If either is
#'   \code{NULL}, weighting is disabled and roughness is calculated using
#'   \code{roughRange}, if supplied. If neither a valid \code{roughRange} nor
#'   complete weighting parameters are supplied, roughness is undefined and is
#'   currently reported as 0. If both \code{roughMean}/\code{roughSD} and
#'   \code{roughRange} are defined, weights outside \code{roughRange} are set
#'   to 0; a very large SD, i.e. a flat weighting function, gives approximately
#'   the same result as using \code{roughRange} without weighting
#' @param roughMinFreq frequencies below \code{roughMinFreq} (Hz) are ignored
#'   when calculating roughness and fluctuation, i.e. the estimated roughness
#'   and fluctuation strength increase if we disregard ~static components near
#'   AM = 0, which are often strong
#' @param fluctRange,fluctMean,fluctSD same as \code{roughRange, roughMean,
#'   roughSD}, but for calculating low-frequency fluctuations centered around 4
#'   Hz (~0.25 to 30 Hz). Ensure that temporal windows are long enough to
#'   analyze slow fluctuations: e.g., use \code{specMethod = 'spectrum'} (not
#'   'meanSpectrum') if doing 1D MS with \code{msType = '1D'}, avoid splitting
#'   the sound into short chunks - set \code{amRes = NULL}, etc.
#' @param amRange the range of temporal modulation frequencies in which we look
#'   for systematic amplitude modulation, Hz
#' @param amRes approximate frequency resolution of amplitude modulation, Hz;
#'   equivalently, the number of independent MS / roughness / fluctuation values
#'   extracted per second of audio. Thus, larger \code{amRes} means more chunks
#'   (better temporal resolution of roughness), but each chunk is smaller (worse
#'   frequency resolution); lower \code{amRes} means fewer chunks, but better
#'   frequency resolution in each chunk. If \code{NULL}, the entire sound is
#'   analyzed at once, unless it is longer than \code{maxDur}, in which case it
#'   is analyzed in chunks \code{maxDur} s long. If \code{amRes} is set, it
#'   overrides \code{maxDur} and the sound is analyzed in approximately equal
#'   chunks of about \code{1000 / amRes} ms. For STFT-based spectrograms, each
#'   chunk contains at least three time frames whenever possible; if the
#'   requested resolution cannot be achieved, the effective resolution is lower.
#'   \code{amRes} also affects the width of the window over which local AM peaks
#'   are evaluated
#' @param maxDur sounds longer than \code{maxDur} s are split into fragments,
#'   and the modulation spectra of all fragments are averaged. This parameter
#'   is used only when \code{amRes = NULL}
#' @param specMethod the function to call when calculating the spectrum of each
#'   frequency band, only used when \code{msType = '1D'};
#'   \code{\link{meanSpectrum}} is faster and less noisy, whereas
#'   \code{\link{spectrum}} gives higher resolution
#' @param logSpec if \code{TRUE}, the spectrogram-like representation is
#'   log-transformed prior to estimating the modulation spectrum. This applies
#'   to both 2D and 1D modulation spectra. A small constant is added to avoid
#'   non-finite values
#' @param logMPS if \code{TRUE}, the modulation spectrum is log-transformed
#'   prior to applying \code{power}, normalization, roughness calculation, and
#'   AM estimation. A small constant is added to avoid non-finite values
#' @param power numeric exponent applied to the modulation spectrum, e.g.
#'   \code{power = 2} for a squared modulation spectrum. This affects roughness
#'   and AM measures
#' @param normalize if \code{TRUE}, the modulation spectrum of each analyzed
#'   fragment is min-max normalized to the range [0, 1], unless all values are
#'   identical
#' @param returnMS if \code{FALSE}, only the summary measures are returned and
#'   plotting is disabled. This is faster and uses less memory. Careful with
#'   exporting the modulation spectra of many sounds at once, as this can
#'   require a lot of RAM
#' @param returnComplex if \code{TRUE} and \code{msType = '2D'}, returns the
#'   complex modulation spectrum prior to log transformation, power, and
#'   normalization. Ignored for \code{msType = '1D'}
#' @param summaryFun summary functions used to summarize per-fragment roughness,
#'   \code{amMsFreq}, and \code{amMsPurity} across time; see
#'   \code{\link{analyze}}
#' @param output what to return, options: 'original', 'original_list',
#'   'modulation_spectrogram', 'processed', 'complex', 'roughness',
#'   'roughness_spectrogram', 'roughness_list', 'fluctuation',
#'   'fluctuation_list', 'fluctuation_spectrogram', 'amMsFreq', 'amMsPurity',
#'   'ampl', 'all' (see the Return section)
#' @param averageMS if \code{TRUE}, the modulation spectra of all successful
#'   inputs are averaged into a single output matrix; if \code{FALSE}, a
#'   separate MS is returned for each input. Note that summary statistics are
#'   still returned per input unless summarized separately
#' @param plot if \code{TRUE}, plots the modulation spectrum of each sound (see
#'   \code{\link{plotMS}}). Plotting requires \code{returnMS = TRUE}
#' @param logWarpX,logWarpY numeric vector of length 2: \code{c(sigma, base)}
#'   for pseudolog-warping the modulation spectrum in the plot, as in
#'   \code{pseudo_log_trans()} from the \pkg{scales} package. These arguments
#'   affect plotting only; the returned matrices are not log-warped
#' @param quantiles numeric vector of cumulative contours in (0, 1). For
#'   example, \code{0.5} marks a region that contains 50\% of the total
#'   modulation-spectrum amplitude. Set to \code{NULL} to omit contour lines
#' @param kernelSize the size of the Gaussian kernel used for smoothing the
#'   modulation spectrum prior to plotting; 1 means no smoothing
#' @param kernelSD the SD of the Gaussian kernel used for smoothing, relative to
#'   its size
#' @param colorTheme plot color theme
#' @param col custom color palette; if supplied, overrides \code{colorTheme}
#' @param main,xlab,ylab,xlim,ylim graphical parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param ... other graphical parameters passed on to
#'   \code{filled.contour.mod} and \code{\link[graphics]{contour}} (see
#'   \code{\link{spectrogram}})
#'
#' @return A list with three top-level elements: \code{$summary},
#'   \code{$detailed}, and \code{$aggregated}.
#'
#'   \code{$summary} is a dataframe summarizing roughness, \code{amMsFreq},
#'   and \code{amMsPurity} for each input (one row per file), or \code{NULL}
#'   if \code{summaryFun = NULL}.
#'
#'   \code{$detailed} contains detailed statistics per sound. If multiple
#'   sounds are analyzed, it is a named list of per-sound lists. If a single
#'   sound is analyzed, it is simplified to a single list. Components are
#'   selected via the \code{output} argument and may include:
#'   \describe{
#'     \item{original}{averaged modulation spectrum across fragments,
#'       after \code{logMPS}, \code{power}, and \code{normalize} if requested, but
#'       before Gaussian smoothing and plotting transforms. Colnames are temporal
#'       modulation frequencies (Hz). Rownames are spectral modulation frequencies
#'       if \code{msType = '2D'} and frequencies of filters or spectrogram bands if
#'       \code{msType = '1D'}.}
#'     \item{original_list}{list of per-fragment modulation spectra.}
#'     \item{modulation_spectrogram}{a spectrogram-like representation
#'       showing how the frequency-averaged modulation spectrum changes over time;
#'       \code{NA} if there is only one fragment.}
#'     \item{processed}{modulation spectrum after Gaussian smoothing. This
#'       matrix is not log-warped; log-warping is applied only during plotting.}
#'     \item{complex}{complex modulation spectrum, returned only if
#'       \code{returnComplex = TRUE}, \code{returnMS = TRUE}, and
#'       \code{msType = '2D'}.}
#'     \item{roughness}{proportion of the modulation spectrum within
#'       \code{roughRange}, or a weighted version thereof if \code{roughMean} and
#'       \code{roughSD} are supplied, in percent. This is a vector if the sound is
#'       analyzed in multiple fragments, otherwise a single number.}
#'     \item{roughness_list}{a list containing frequencies, band-wise
#'       roughness values, and band amplitudes for each fragment.}
#'     \item{roughness_spectrogram}{a spectrogram-like matrix showing
#'       roughness per frequency band and per fragment.}
#'     \item{fluctuation}{proportion of the modulation spectrum within
#'       \code{fluctRange}, or a weighted version thereof if \code{fluctMean} and
#'       \code{fluctSD} are supplied, in percent. This is a vector if the sound is
#'       analyzed in multiple fragments, otherwise a single number.}
#'     \item{fluctuation_list}{a list containing frequencies, band-wise
#'       fluctuation values, and band amplitudes for each fragment.}
#'     \item{fluctuation_spectrogram}{a spectrogram-like matrix showing
#'       fluctuation strength per frequency band and per fragment.}
#'     \item{amMsFreq}{frequency of the highest AM peak within
#'       \code{amRange}. Like \code{roughness}, this can be a single number or a
#'       vector, depending on whether the sound is analyzed as a whole or in chunks.}
#'     \item{amMsPurity}{ratio of the AM peak at \code{amMsFreq} to the
#'       mean amplitude of the other folded AM values within \code{amRange}, in dB.}
#'     \item{ampl}{Root Mean Square amplitude of each analyzed fragment,
#'       divided by the internal audio scaling factor if present.}
#'   }
#'
#'   \code{$aggregated} is a list containing the \code{original},
#'   \code{processed}, and (if requested) \code{complex} modulation spectra
#'   averaged across all successfully analyzed sounds. Only present if
#'   \code{averageMS = TRUE} and multiple sounds are analyzed.
#'
#' @export
#' @examples
#' s = soundgen(pitch = 440, amFreq = 100, amDep = 50)
#' ms = modulationSpectrum(s, samplingRate = 16000, amRes = NULL)
#' ms$detailed[c('roughness', 'amMsFreq', 'amMsPurity')]  # a single value for each
#' ms1 = modulationSpectrum(s, samplingRate = 16000, amRes = 5)
#' ms1$detailed[c('roughness', 'amMsFreq', 'amMsPurity')]
#' # measured over time (low values of amRes mean more precision, so we analyze
#' # longer segments and get fewer values per sound)
#'
#' # Embellish
#' ms = modulationSpectrum(s, samplingRate = 16000, logMPS = TRUE,
#'   xlab = 'Temporal modulation, Hz', ylab = 'Spectral modulation, 1/kHz',
#'   colorTheme = 'matlab', main = 'Modulation spectrum', lty = 3)
#'
#' # Plot a modulation spectrogram (the peak at 100 Hz shows AM)
#' spectrogram(s, 16000, specManual = ms$detailed$modulation_spectrogram,
#'   ylab = 'Modulation frequency, kHz', main = 'Modulation spectrogram')
#'
#' # Plot a roughness spectrogram
#' spectrogram(s, 16000, specManual = ms$detailed$roughness_spectrogram,
#'   yScale = 'ERB', main = 'Roughness spectrogram')
#'
#' # 1D instead of 2D
#' modulationSpectrum(s, 16000, msType = '1D', quantiles = NULL,
#'   col = soundgen:::jet.col(50), logWarpX = c(10, 2))
#'
#' \dontrun{
#' # A long sound with varying AM and a bit of chaos at the end
#' s_long = soundgen(sylLen = 3500, pitch = c(250, 320, 280),
#'                   amFreq = c(30, 55), amDep = c(20, 60, 40),
#'                   jitterDep = c(0, 0, 2), plot = TRUE, yScale = 'ERB')
#' playme(s_long)
#' ms = modulationSpectrum(s_long, 16000)
#' # plot AM over time
#' plot(names(ms$detailed$amMsFreq), ms$detailed$amMsFreq,
#'   cex = ms$detailed$amMsPurity/5,
#'   xlab = 'Time, ms', ylab = 'AM frequency, Hz')
#' # plot roughness over time
#' spectrogram(s_long, 16000, ylim = c(0, 4),
#'   extraContour = list(x = ms$detailed$roughness / max(ms$detailed$roughness) * 4000, col = 'blue'))
#'
#'
#' # As with spectrograms, there is a tradeoff in time-frequency resolution
#' s = soundgen(pitch = 500, amFreq = 50, amDep = 100, sylLen = 500,
#'              samplingRate = 44100, plot = TRUE)
#' # playme(s, samplingRate = 44100)
#' ms = modulationSpectrum(s, samplingRate = 44100, amRes = NULL,
#'   specFun_pars = list(windowLength = 50, step = 50)) # poor temporal resolution
#' ms = modulationSpectrum(s, samplingRate = 44100, amRes = NULL,
#'   specFun_pars = list(windowLength = 5, step = 1))  # poor frequency resolution
#' ms = modulationSpectrum(s, samplingRate = 44100, amRes = NULL,
#'   specFun_pars = list(windowLength = 15, step = 3))  # a reasonable compromise
#'
#' # Start with an auditory spectrogram instead of STFT
#' modulationSpectrum(s, 44100, specFun = 'audSpec', xlim = c(-100, 100))
#' modulationSpectrum(s, 44100, specFun = 'audSpec',
#'   logWarpX = c(10, 2), xlim = c(-500, 500),
#'   specFun_pars = list(nFilters = 32, filterType = 'gammatone', bandwidth = NULL))
#'
#' # customize the plot
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   amRes = NULL,
#'   kernelSize = 17,  # more smoothing
#'   xlim = c(-70, 70), ylim = c(0, 4),  # zoom in on the central region
#'   quantiles = c(.25, .5, .8),  # customize contour lines
#'   col = rev(rainbow(100)),  # alternative palette
#'   logWarpX = c(10, 2),  # pseudo-log transform
#'   power = 2)                   # ^2
#' # Note the peaks at FM = 2/kHz (from "pitch = 500") and AM = 50 Hz (from
#' # "amFreq = 50")
#'
#' # Input can be path to folder with audio files. Each file is processed
#' # separately, and the output can contain an MS per file...
#' ms1 = modulationSpectrum('~/Downloads/temp', kernelSize = 11,
#'                          plot = FALSE, averageMS = FALSE)
#' ms1$summary
#' names(ms1$detailed)  # separate MS and other descriptives per file
#' # ...or a single MS can be calculated by averaging across all files:
#' ms2 = modulationSpectrum('~/Downloads/temp', kernelSize = 11,
#'                          plot = FALSE, averageMS = TRUE)
#' plotMS(ms2$aggregated$original)
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
#' ms$detailed$roughness
#' # compare:
#' modulationSpectrum(s, samplingRate = 16000, maxDur = .5,
#'   xlim = c(-25, 25), colorTheme = 'seewave',
#'   power = 1)$detailed$roughness  # much higher roughness
#'
#' # Plotting with or without log-warping the modulation spectrum:
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000, plot = TRUE)
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'   logWarpX = c(2, 2), plot = TRUE)
#'
#' # logWarp and kernelSize have no effect on roughness
#' # because it is calculated before these transforms:
#' modulationSpectrum(s, samplingRate = 16000, logWarpX = c(1, 10))$detailed$roughness
#' modulationSpectrum(s, samplingRate = 16000, logWarpX = NA)$detailed$roughness
#' modulationSpectrum(s, samplingRate = 16000, kernelSize = 17)$detailed$roughness
#'
#' # Log-transform the spectrogram prior to 2D FFT (affects roughness):
#' modulationSpectrum(s, samplingRate = 16000, logSpec = FALSE)$detailed$roughness
#' modulationSpectrum(s, samplingRate = 16000, logSpec = TRUE)$detailed$roughness
#'
#' # Use a lognormal weighting function to calculate roughness
#' # (instead of just % in roughRange)
#' modulationSpectrum(s, 16000, roughRange = NULL,
#'   roughMean = 75, roughSD = 3)$detailed$roughness
#' modulationSpectrum(s, 16000, roughRange = NULL,
#'   roughMean = 100, roughSD = 12)$detailed$roughness
#' # truncate weights outside roughRange
#' modulationSpectrum(s, 16000, roughRange = c(30, 150),
#'   roughMean = 100, roughSD = 1000)$detailed$roughness  # very large SD
#' modulationSpectrum(s, 16000, roughRange = c(30, 150),
#'   roughMean = NULL)$detailed$roughness  # same as above b/c SD --> Inf
#'
#' # Complex modulation spectrum with phase preserved
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'                         returnComplex = TRUE)
#' plotMS(log(abs(ms$detailed$complex + 1e-8)))  # note the symmetry
#' # compare:
#' plotMS(ms$detailed$original)
#' }
modulationSpectrum = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    msType = c('2D', '1D'),
    specFun = 'STFT',
    specFun_pars = list(),
    amRes = 5,
    maxDur = 5,
    specMethod = c('meanSpectrum', 'spectrum'),
    logSpec = FALSE,
    logMPS = FALSE,
    power = 1,
    normalize = TRUE,
    roughRange = NULL,
    roughMean = 100,
    roughSD = 8,
    roughMinFreq = 1,
    amRange = c(10, 200),
    fluctRange = c(0.25, 30),
    fluctMean = 4,
    fluctSD = 12,
    returnMS = TRUE,
    returnComplex = FALSE,
    summaryFun = c('mean', 'sd'),
    output = 'all',
    averageMS = FALSE,
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    logWarpX = NULL,
    logWarpY = NULL,
    quantiles = c(.5, .8, .9),
    kernelSize = 5,
    kernelSD = .5,
    colorTheme = 'bw',
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
  specFun = match.arg(specFun, choices = c('stft', 'STFT', 'audSpec', 'env'))
  if (specFun %in% c('STFT', 'stft_simple')) specFun = 'stft'
  # (the default is "STFT" just for compatibility with soundgen 2.x)
  specMethod = match.arg(specMethod)
  msType = match.arg(msType)
  all_outputs = c('original', 'original_list', 'modulation_spectrogram',
                  'processed', 'complex', 'roughness', 'roughness_spectrogram',
                  'roughness_list', 'fluctuation', 'fluctuation_list',
                  'fluctuation_spectrogram', 'amMsFreq', 'amMsPurity', 'ampl')
  output = match.arg(output, c(all_outputs, 'all'), several.ok = TRUE)
  if ('all' %in% output) output = all_outputs
  rm('all_outputs')

  # deprecated parameters
  call_arg_names = names(match.call())
  if (any(c('specSource', 'audSpec_pars', 'windowLength',
            'overlap', 'step', 'wn', 'zp') %in% call_arg_names))
    stop(paste('Changes in soundgen >3.0: "specSource" is deprecated',
               'and replaced by "specFun". Arguments "audSpec_pars", "windowLength"',
               '"overlap", "step", "wn", "zp" are now passed in "specFun_pars"'))
  rm('call_arg_names')

  # default labels for y-axis
  if (is.null(ylab)) {
    if (specFun == 'stft') {
      ylab = ifelse(msType == '2D', '1/kHz', 'kHz')
    } else if (specFun == 'audSpec' && msType == '2D') {
      yScale = if (is.null(specFun_pars$yScale)) 'ERB' else specFun_pars$yScale
      ylab = paste0('1/', if (yScale == 'log') 'oct' else yScale)
      rm('yScale')
    } else {
      ylab = 'kHz'
    }
  }

  if (msType == '1D' && returnComplex) {
    returnComplex = FALSE
    warning('returnComplex can only be TRUE if msType = "2D"; resetting returnComplex to FALSE')
  }

  ## Prepare a list of arguments to pass to .modulationSpectrum()
  myPars = c(as.list(environment()), list(...))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'savePlots', 'embed',
    'reportEvery', 'cores', 'summaryFun', 'averageMS', 'specFun_pars', 'output')]
  myPars$specFun_pars = specFun_pars

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.modulationSpectrum',
    suffix = 'modulationSpectrum',
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
          data.frame(roughness = res$roughness,
                     fluctuation = res$fluctuation,
                     amMsFreq = res$amMsFreq,
                     amMsPurity = res$amMsPurity),
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
          data.frame(roughness = NA, fluctuation = NA,
                     amMsFreq = NA, amMsPurity = NA),
          summaryFun = summaryFun,
          var_noSummary = NULL))
        if (inherits(filler, 'try-error')) {
          filler = data.frame(matrix(NA, nrow = 1, ncol = 3 * length(summaryFun)))
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

  # average MS across sounds
  op_prep2 = c('original', 'processed')
  if (returnComplex) op_prep2 = c(op_prep2, 'complex')

  aggreg = list()
  if (averageMS && pa$input$n > 1) {
    idx_ok = which(!pa$input$failed)
    if (length(idx_ok) > 0) {
      for (op in op_prep2) {
        mats = lapply(pa$result[idx_ok], function(x) x[[op]])
        # Filter out NAs or non-matrices (e.g. from empty_result for short files)
        mats = mats[vapply(mats, is.matrix, logical(1))]
        if (length(mats) > 0) {
          aggreg[[op]] = try(reduceMatrices(
            mats,
            rFun = 'max',
            cFun = 'median',
            reduceFun = '+') / length(mats))
          if (inherits(aggreg[[op]], 'try-error')) aggreg[[op]] = NULL
        }
      }
    }
  }
  if (length(aggreg) == 0) aggreg = NULL

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
    summary = mysum_all,
    aggregated = aggreg
  ))
}


#' Modulation spectrum per sound
#' @noRd
.modulationSpectrum = function(
    audio,
    specFun = 'stft',
    specFun_pars = list(),
    msType = '2D',
    amRes = 5,
    maxDur = 5,
    specMethod = 'meanSpectrum',
    logSpec = FALSE,
    logMPS = FALSE,
    power = 1,
    normalize = TRUE,
    roughRange = NULL,
    roughMean = 100,
    roughSD = 8,
    roughMinFreq = 1,
    amRange = c(10, 200),
    fluctRange = c(0.25, 30),
    fluctMean = 4,
    fluctSD = 12,
    returnMS = TRUE,
    returnComplex = FALSE,
    plot = FALSE,
    logWarpX = NULL,
    logWarpY = NULL,
    quantiles = c(.5, .8, .9),
    kernelSize = 5,
    kernelSD = .5,
    colorTheme = 'bw',
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
  lowestFreq = if (is.null(roughRange)) 5 else min(5, roughRange[1])
  if (!is.finite(lowestFreq) || lowestFreq <= 0) lowestFreq = 5

  # hard-coded defaults for the two methods
  step = NULL
  if (specFun == 'stft') {
    def = list(windowLength = 15, overlap = 90, wn = 'hanning', zp = NULL)
    if (is.list(specFun_pars) && length(specFun_pars) > 0)
      def[names(specFun_pars)] = specFun_pars
    specFun_pars = def
    val = validateWlOvlp(audio, specFun_pars$windowLength,
                         specFun_pars$step, specFun_pars$overlap)
    specFun_pars$wl = val$wl
    specFun_pars$step = val$step_points
    if (is.null(specFun_pars$zp)) specFun_pars$zp = nextn(specFun_pars$wl)
    specFun_pars = specFun_pars[-which(
      !names(specFun_pars) %in% c('wl', 'step', 'wn', 'zp', 'padWithSilence'))]
    step = val$step
    windowLength = val$windowLength
  } else if (specFun == 'audSpec') {
    def = list(yScale = 'ERB', step = 1, envelope = 'hil', nFilters = 32)
    if (is.list(specFun_pars) && length(specFun_pars) > 0)
      def[names(specFun_pars)] = specFun_pars
    specFun_pars = def
    if (specFun_pars$nFilters == 1) specFun = 'hil'
    step = specFun_pars$step
  }

  # a standard return value for failed / too short sounds
  empty_result = list('original' = NA,
                      'original_list' = NA,
                      'modulation_spectrogram' = NA,
                      'processed' = NA,
                      'complex' = NA,
                      'roughness' = NA,
                      'roughness_list' = NA,
                      'roughness_spectrogram' = NA,
                      'fluctuation' = NA,
                      'fluctuation_list' = NA,
                      'fluctuation_spectrogram' = NA,
                      'amMsFreq' = NA,
                      'amMsPurity' = NA,
                      'ampl' = NA)

  # calculate the entire spectrogram, regardless of whether we are splitting in chunks
  if (specFun == 'hil') {
    # just take Hilbert envelope of the original sound
    s1 = matrix(hilbert_approx(audio$sound)$envelope, nrow = 1)
    rownames(s1) = 1L
  } else {
    s1 = getSpec(audio, specFun = specFun, specFun_pars = specFun_pars)
  }

  if (specFun %in% c('hil', 'env', 'rms', 'RMS', 'getRMS'))
    msType = '1D' # can't do 2D with a single feature
  if (!is.matrix(s1)) s1 = as.matrix(s1)
  if (nrow(s1) < 1 || ncol(s1) < 3) {
    warning('The sound is too short or windowLength too long. Need at least 3 frames')
    return(empty_result)
  }

  if (is.null(step)) {
    step = diff(as.numeric(colnames(s1)[1:2]))
    if (is.null(step) || !is.finite(step))
      stop('Failed to determine step from specFun_pars or the generated matrix')
  }

  max_am = 1000 / step / 2
  if (!is.null(roughRange) && max_am < roughRange[1]) {
    warning(paste(
      'roughRange outside the analyzed range of temporal modulation frequencies;',
      'decrease step to improve temporal resolution ',
      'or look for roughness in a lower range'))
  }

  # make sure we have time stamps for slicing and for modulation frequency labels
  if (is.null(colnames(s1))) {
    default_step = if (specFun == 'stft') step else 1000 / audio$samplingRate
    colnames(s1) = (0:(ncol(s1) - 1)) * default_step
  }
  if (is.null(rownames(s1))) {
    rownames(s1) = as.character(1:nrow(s1))
  }

  # estimate the temporal sampling interval of the spectrogram-like representation
  time_ms = as.numeric(colnames(s1))
  if (length(time_ms) > 1) {
    step_s1 = median(diff(time_ms))
  } else {
    step_s1 = if (specFun == 'stft') step else 1000 / audio$samplingRate
  }
  if (!is.finite(step_s1) || step_s1 <= 0) {
    step_s1 = if (specFun == 'stft') step else 1000 / audio$samplingRate
  }

  # floor and log-transform the spectrogram
  s1 = floor_log(s1, takeLog = logSpec, dynamicRange = 120)
  # image(t(s1))

  # complex MS only makes sense for 2D transforms
  if (returnComplex && msType == '1D') {
    returnComplex = FALSE
    warning('returnComplex is only meaningful for msType = "2D"; ignoring returnComplex')
  }

  # process the spectrogram in one go or in chunks
  if (!is.null(amRes) && (!is.numeric(amRes) || length(amRes) != 1 ||
                          !is.finite(amRes) || amRes <= 0)) {
    amRes = NULL
  }
  if (is.null(amRes) && (is.null(maxDur) || !is.finite(maxDur) || maxDur <= 0)) {
    maxDur = 5
  }

  if (is.numeric(amRes)) {
    if (specFun == 'stft') {
      nFrames = max(3, ceiling(max_am / amRes * 2))  # min 3 spectrograms frames
      # split the input sound into chunks nFrames long
      chunk_ms = windowLength + step * (nFrames - 1)
      if (amRes < (1 / audio$duration)) {
        message(paste('The sound is too short to be analyzed with amRes =', amRes,
                      'Hz. Actual amRes ~= ', round(1 / audio$duration, 2)))
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

  # cannot have more fragments than samples / columns, and need at least 3
  # columns per fragment
  splitInto = max(1, min(splitInto, audio$ls, ncol(s1)))
  if (ncol(s1) >= 3) {
    splitInto = max(1, min(splitInto, floor(ncol(s1) / 3)))
  }

  if (splitInto > 1) {
    myseq_audio = floor(seq(1, audio$ls, length.out = splitInto + 1))
    myseq_cols = floor(seq(1, ncol(s1), length.out = splitInto + 1))
    midpoints = rep(NA, splitInto)
    chunk_audio = vector('list', splitInto)
    chunk_cols = vector('list', splitInto)

    for (i in 1:splitInto) {
      # audio chunk (for RMS)
      audio_start = myseq_audio[i]
      audio_end = if (i == splitInto) audio$ls else myseq_audio[i + 1] - 1
      if (audio_end < audio_start) audio_end = audio_start
      idx_audio = audio_start:audio_end
      chunk_audio[[i]] = audio$sound[idx_audio]
      midpoints[i] = (idx_audio[1] + idx_audio[length(idx_audio)]) / 2

      # spectrogram chunk
      col_start = myseq_cols[i]
      col_end = if (i == splitInto) ncol(s1) else myseq_cols[i + 1] - 1
      if (col_end < col_start) col_end = col_start
      chunk_cols[[i]] = col_start:col_end
    }
  } else {
    midpoints = (1 + audio$ls) / 2
    chunk_audio = list(audio$sound)
    chunk_cols = list(1:ncol(s1))
  }

  # extract modulation spectrum per fragment
  ampl = rep(NA, splitInto)
  out = vector('list', splitInto)
  if (audio$ls > 0) {
    names(out) = midpoints / audio$ls * audio$dur
  } else {
    names(out) = as.character(1:splitInto)
  }

  out_complex = if (returnComplex) out else NULL
  out_aggreg_complex = NULL

  for (i in 1:splitInto) {
    # take a slice of the already-computed spectrogram
    spec = s1[, chunk_cols[[i]], drop = FALSE]

    # spectrogram to modulation spectrum
    if (msType == '2D') {
      # 2D FFT
      if (specFun == 'stft') {
        ms_complex = specToMS(spec)$ms_centered
      } else if (specFun == 'audSpec') {
        # audSpec
        ms_complex = specToMS(spec)$ms_centered

        # Recalculate FM labels in cycles/yScale instead of cycles/kHz
        yScale = specFun_pars$yScale
        freq_kHz = as.numeric(rownames(spec))
        freq_yscale = HzToOther(freq_kHz * 1000, scale = yScale)
        delta_yscale = median(diff(freq_yscale))

        if (is.finite(delta_yscale) && delta_yscale > 0) {
          samplingRate_fm_yscale = 1 / delta_yscale
          bin_width_fm_yscale = samplingRate_fm_yscale / nrow(spec)
          names_fm_yscale = (0:(nrow(spec) - 1)) * bin_width_fm_yscale

          # use centerMS() on a dummy matrix to apply the exact same
          # centering logic to the new yScale labels
          dummy = matrix(0, nrow = nrow(spec), ncol = 2)
          rownames(dummy) = names_fm_yscale
          rownames(ms_complex) = rownames(centerMS(dummy))
        }
      }
      ms = floor_log(abs(ms_complex), takeLog = logMPS, dynamicRange = 120)
      symAxis = nrow(ms) %/% 2 + 1
      ms_half = ms[symAxis:nrow(ms), , drop = FALSE]
    } else if (msType == '1D') {
      # 1D FFT
      sr_s1 = ifelse(specFun == 'stft', 1000 / step, 1000 / step_s1)
      ms_complex = NA
      ms_half = specToMS_1D(spec, samplingRate = sr_s1,
                            windowLength = 1000 / lowestFreq,
                            specMethod = specMethod)
      ms_half = floor_log(ms_half, takeLog = logMPS, dynamicRange = 120)
    } else {
      stop("msType must be either '2D' or '1D'")
    }

    # power
    if (is.numeric(power) && power != 1) ms_half = ms_half ^ power

    # normalize
    if (normalize && diff(range(ms_half)) != 0) {
      ms_half = ms_half - min(ms_half)  # can be negative if logMPS = TRUE
      ms_half = ms_half / max(ms_half) # or sum(ms_half)
    }

    out[[i]] = ms_half
    ampl[i] = if (length(chunk_audio[[i]]) > 0) sqrt(mean(chunk_audio[[i]] ^ 2)) else NA

    if (returnComplex) {
      out_complex[[i]] = ms_complex
    }
  }

  keep = which(unlist(lapply(out, function(x) !is.null(x))))
  out = out[keep]
  ampl = ampl[keep]
  if (returnComplex) out_complex = out_complex[keep]

  if (length(out) < 1) {
    warning('The sound is too short or windowLength too long. Need at least 3 frames')
    return(empty_result)
  }

  # standardize the size of matrices for ease of processing
  if (length(out) > 1) {
    ncs = unlist(lapply(out, ncol))
    nc_max = max(ncs)
    for (i in seq_along(out)) {
      if (ncol(out[[i]]) != nc_max)
        out[[i]] = interpolMatrix(out[[i]], nc = nc_max)  # upsample, never decimate
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

  # extract a measure of roughness and fluctuation strength
  drop_any = !is.null(roughMinFreq) && is.finite(roughMinFreq) && roughMinFreq > 0
  if (drop_any) {
    am_drop = which(abs(as.numeric(colnames(out[[1]]))) < roughMinFreq)
    if (length(am_drop) == 0 || length(am_drop) >= ncol(out[[1]])) {
      drop_any = FALSE
    }
  }
  if (drop_any) {
    roughness_list = lapply(out, function(x)
      getRoughness(x[, -am_drop, drop = FALSE], modRange = roughRange,
                   modMean = roughMean, modSD = roughSD))
    fluct_list = lapply(out, function(x)
      getRoughness(x[, -am_drop, drop = FALSE], modRange = fluctRange,
                   modMean = fluctMean, modSD = fluctSD))
  } else {
    roughness_list = lapply(out, function(x)
      getRoughness(x, modRange = roughRange,
                   modMean = roughMean, modSD = roughSD))
    fluct_list = lapply(out, function(x)
      getRoughness(x, modRange = fluctRange,
                   modMean = fluctMean, modSD = fluctSD))
  }
  roughness = unlist(lapply(roughness_list, function(x) x$prop))
  fluct = unlist(lapply(fluct_list, function(x) x$prop))

  # concatenate roughness and fluctuation per frequency bin over time into a
  # roughness/fluct spectrogram
  rg = do.call(cbind, lapply(roughness_list, function(x) x$details$prop))
  colnames(rg) = as.numeric(names(out)) * 1000  # time in ms
  rownames(rg) = rownames(out[[1]])  # frequency in kHz

  fg = do.call(cbind, lapply(fluct_list, function(x) x$details$prop))
  colnames(fg) = as.numeric(names(out)) * 1000  # time in ms
  rownames(fg) = rownames(out[[1]])  # frequency in kHz

  # detect systematic amplitude modulation at the same frequency
  am_list = lapply(out, function(x)
    getAM_ms(x, amRange = amRange, amRes = amRes))
  amMsFreq = unlist(lapply(am_list, function(x) x$amMsFreq))
  amMsPurity = unlist(lapply(am_list, function(x) x$amMsPurity))

  # scale RMS amplitude back if the sound was normalized internally
  ampl_out = ampl
  if (!is.null(audio$scale) && is.finite(audio$scale) && audio$scale != 0) {
    ampl_out = ampl / audio$scale
  }

  # average modulation spectra across all fragments
  if (!returnMS) {
    plot = FALSE  # can't plot w/o producing modulation spectra
    audio$savePlots = FALSE
    result = list('original' = NULL,
                  'original_list' = NULL,
                  'modulation_spectrogram' = NULL,
                  'processed' = NULL,
                  'complex' = NULL,
                  'roughness' = roughness,
                  'roughness_list' = roughness_list,
                  'roughness_spectrogram' = NULL,
                  'fluctuation' = fluct,
                  'fluctuation_list' = fluct_list,
                  'fluctuation_spectrogram' = NULL,
                  'amMsFreq' = amMsFreq,
                  'amMsPurity' = amMsPurity,
                  'ampl' = ampl_out)
  } else {
    out_aggreg = reduceMatrices(
      out,
      rFun = 'max',   # normally same samplingRate, but if not, upsample frequency resolution
      cFun = 'median',  # typical ncol (depends on sound dur)
      reduceFun = '+') / length(out)  # mean
    X = as.numeric(colnames(out_aggreg))
    Y = as.numeric(rownames(out_aggreg))
    # image(X, Y, t(log(out_aggreg)))

    # prepare a separate summary of the complex ms
    if (returnComplex) {
      out_aggreg_complex = reduceMatrices(
        out_complex,
        rFun = 'max',
        cFun = 'median',
        reduceFun = '+') / length(out_complex)
      # image(t(log(abs(out_aggreg_complex))))
    }

    # smoothing / blurring
    if (msType == '2D') {
      out_transf = gaussianSmooth2D(out_aggreg,
                                    kernelSize = kernelSize,
                                    kernelSD = kernelSD)
    } else {
      out_transf = out_aggreg
    }

    result = list('original' = out_aggreg,
                  'original_list' = out,
                  'modulation_spectrogram' = mg,
                  'processed' = out_transf,
                  'complex' = out_aggreg_complex,
                  'roughness' = roughness,
                  'roughness_list' = roughness_list,
                  'roughness_spectrogram' = rg,
                  'fluctuation' = fluct,
                  'fluctuation_list' = fluct_list,
                  'fluctuation_spectrogram' = fg,
                  'amMsFreq' = amMsFreq,
                  'amMsPurity' = amMsPurity,
                  'ampl' = ampl_out)
  }  # end of if (returnMS)


  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
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
             extraY = (msType == '2D' && specFun == 'stft'),
             ...)
    } else {
      plot(X, as.numeric(out_transf), type = 'l', xlab = xlab, ylab = ylab,
           main = main, xlim = xlim, ylim = ylim, ...)
    }
  }

  invisible(result)
}
