#' Auditory spectrogram
#'
#' Produces an auditory spectrogram by convolving the sound with a bank of
#' bandpass filters. The main difference from STFT is that we don't window the
#' signal and de facto get variable temporal resolution in different frequency
#' channels, as with a wavelet transform. The key settings are
#' \code{filterType}, \code{nFilters}, and \code{yScale}, which determine the
#' type, number, and spacing of the filters, respectively. Gammatone filters
#' were designed as a simple approximation of human perception - see
#' \code{\link[seewave]{gammatone}} and Slaney 1993 "An Efficient Implementation
#' of the Patterson–Holdsworth Auditory Filter Bank". Butterworth or Chebyshev
#' filters are not meant to model perception, but can be useful for quickly
#' plotting a sound.
#'
#' @inheritParams spectrogram
#' @param step step, ms (determines time resolution of the plot, but not of the
#'   returned envelopes per channel). step = NULL means no downsampling at all
#'   (ncol of output = length of input audio)
#' @param filterType "butterworth" = Butterworth filter
#'   \code{\link[signal]{butter}}, "chebyshev" = Chebyshev filter
#'   \code{\link[signal]{butter}}, "gammatone" =
#'   \code{\link[seewave]{gammatone}}
#' @param nFilters the number of filters between \code{minFreq} and
#'   \code{maxFreq} (determines frequency resolution, while \code{yScale}
#'   determines the location of center frequencies)
#' @param nFilters_oct an alternative way to specify frequency resolution: the
#'   number of filters per octave
#' @param filterOrder filter order (defaults to 4 for gammatones, 3 otherwise)
#' @param bandwidth filter bandwidth, octaves. If NULL, defaults to ERB
#'   bandwidths as in \code{\link[seewave]{gammatone}}
#' @param bandwidthMult a scaling factor for all bandwidths (1 = no effect)
#' @param minFreq,maxFreq the range of frequencies to analyze. If the
#'   spectrogram looks empty, try increasing minFreq - the lowest filters are
#'   prone to returning very large values
#' @param minBandwidth minimum filter bandwidth, Hz (otherwise filters may
#'   become too narrow when nFilters is high; has no effect if filterType =
#'   'gammatone')
#' @param output a list of measures to return. Defaults to everything, but this
#'   takes a lot of RAM, so shorten to what's needed if analyzing many files at
#'   once
#' @param plotFilters if TRUE, plots the filters as central frequencies ±
#'   bandwidth/2
#' @param yScale determines the location of center frequencies of the filters
#'
#' @return Returns a list for each analyzed file, including:
#'   \describe{\item{audSpec}{auditory spectrogram with frequencies in rows and
#'   time in columns} \item{audSpec_processed}{same but rescaled for plotting}
#'   \item{filterbank}{raw output of the filters} \item{roughness}{roughness per
#'   channel (as many as \code{nFilters})}}
#'
#' @export
#' @examples
#' # synthesize a sound with gradually increasing hissing noise
#' sound = soundgen(sylLen = 200, temperature = 0.001,
#'   noise = list(time = c(0, 350), value = c(-40, 0)),
#'   formantsNoise = list(f1 = list(freq = 5000, width = 10000)),
#'   addSilence = 25)
#' # playme(sound, samplingRate = 16000)
#'
#' # auditory spectrogram
#' as = audSpectrogram(sound, samplingRate = 16000, nFilters = 48)
#' dim(as$audSpec)
#'
#' # compare to FFT-based spectrogram with similar time and frequency resolution
#' fs = spectrogram(sound, samplingRate = 16000, yScale = 'bark',
#'                  windowLength = 5, step = 1)
#' dim(fs)
#'
#' \dontrun{
#' # add bells and whistles
#' audSpectrogram(sound, samplingRate = 16000,
#'   filterType = 'butterworth',
#'   nFilters = 128,
#'   yScale = 'ERB',
#'   bandwidth = 1/6,
#'   dynamicRange = 150,
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   contrast = .4,  # increase contrast
#'   brightness = -.2,  # reduce brightness
#'   # colorTheme = 'heat.colors',  # pick color theme...
#'   col = hcl.colors(100, palette = 'Plasma'),  # ...or specify the colors
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   grid = 5,  # to customize, add manually with graphics::grid()
#'   ylim = c(0.05, 8),  # always in kHz
#'   main = 'My auditory spectrogram' # title
#'   # + axis labels, etc
#' )
#'
#' # NB: frequency resolution is controlled by both nFilters and bandwidth
#' audSpectrogram(sound, 16000, nFilters = 15, bandwidth = 1/2)
#' audSpectrogram(sound, 16000, nFilters = 15, bandwidth = 1/10)
#' audSpectrogram(sound, 16000, nFilters = 100, bandwidth = 1/2)
#' audSpectrogram(sound, 16000, nFilters = 100, bandwidth = 1/10)
#' audSpectrogram(sound, 16000, nFilters_oct = 5, bandwidth = 1/10)
#'
#' # remove the oscillogram
#' audSpectrogram(sound, samplingRate = 16000, osc = 'none')
#'
#' # save auditory spectrograms of all audio files in a folder
#' audSpectrogram('~/Downloads/temp',
#'   savePlots = '~/Downloads/temp/audSpec', cores = 4)
#' }
audSpectrogram = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    step = 1,
    dynamicRange = 80,
    filterType = c('butterworth', 'chebyshev', 'gammatone')[1],
    nFilters = 128,
    nFilters_oct = NULL,
    filterOrder = if (filterType == 'gammatone') 4 else 3,
    bandwidth = NULL,
    bandwidthMult = 1,
    minFreq = 20,
    maxFreq = samplingRate / 2,
    minBandwidth = 10,
    output = c('audSpec', 'audSpec_processed', 'filterbank', 'filterbank_env', 'roughness'),
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = NULL,
    plotFilters = FALSE,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    yScale = c('bark', 'mel', 'ERB', 'log')[1],
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
    ...
) {
  if (!yScale %in% c('bark', 'mel', 'ERB', 'log')) {
    stop('Valid yScale values: bark, mel, ERB, log')
  }
  if (!filterType %in% c('butterworth', 'chebyshev', 'gammatone')) {
    stop('Valid filterType values: butterworth, chebyshev, gammatone')
  }

  # match args
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'reportEvery', 'cores', 'savePlots')]

  # call .audSpectrogram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.audSpectrogram',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "audSpectrogram", width = paste0(width, units)))
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Auditory spectrogram per sound
#'
#' Internal soundgen function.
#' @inheritParams audSpectrogram
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.audSpectrogram = function(
    audio,
    step = 1,
    dynamicRange = 80,
    filterType = c('butterworth', 'chebyshev', 'gammatone')[1],
    nFilters = 128,
    nFilters_oct = NULL,
    filterOrder = if(filterType == 'gammatone') 4 else 3,
    bandwidth = NULL,
    bandwidthMult = 1,
    minFreq = 20,
    maxFreq = audio$samplingRate / 2,
    minBandwidth = 10,
    output = c('audSpec', 'audSpec_processed',
               'filterbank', 'filterbank_env', 'roughness'),
    plot = TRUE,
    plotFilters = FALSE,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    yScale = 'bark',
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
    ...
) {
  nyquist = audio$samplingRate / 2
  if (is.null(maxFreq) || length(maxFreq) < 1) {
    maxFreq = nyquist
  } else {
    maxFreq = min(maxFreq, nyquist)
  }
  if (!is.null(step)) {
    len = max(1, round(audio$duration * 1000 / step))
    step = audio$duration * 1000 / len  # avoid rounding error
  } else {
    len = NULL
    step = 1000 / audio$samplingRate
  }
  if (!is.null(nFilters_oct))
    nFilters = round(log2(maxFreq/minFreq) * nFilters_oct)

  # set up filters (if "bandwidth" is NULL, non-overlapping)
  if (yScale == 'log') {
    cf = 2^(seq(log2(minFreq), log2(maxFreq), length.out = nFilters))
  } else if (yScale == 'bark') {
    cf = tuneR::bark2hz(seq(tuneR::hz2bark(minFreq), tuneR::hz2bark(maxFreq),
                            length.out = nFilters))
  } else if (yScale == 'mel') {
    cf = tuneR::mel2hz(seq(hz2mel(minFreq), hz2mel(maxFreq), length.out = nFilters))
  } else if (yScale == 'ERB') {
    cf = ERBToHz(seq(HzToERB(minFreq), HzToERB(maxFreq), length.out = nFilters))
  }
  filters = data.frame(cf = cf)
  if (is.null(bandwidth)) {
    # default bandwidths from Slaney 1993
    filters$bandwidth = 24.7 * (4.37 * filters$cf/1000 + 1) * bandwidthMult
  } else {
    # user-specified bandwidths in octaves
    # (cf + half_bw_Hz) / (cf - half_bw_Hz) = 2^bw_oct  - solving for bw gives:
    #  half_bw_Hz = cf * (2^bw_oct - 1) / (2^bw_oct + 1)
    filters$bandwidth = 2 * cf * (2^bandwidth - 1) / (2^bandwidth + 1) * bandwidthMult
  }
  filters$from = filters$cf - filters$bandwidth/2
  filters$to = filters$cf + filters$bandwidth/2

  if (filterType != 'gammatone') {
    # make sure the bandwidth in Hz (!) is always wide enough to avoid crashing
    idx_narrow = which(filters$bandwidth < minBandwidth)
    if (length(idx_narrow) > 0) {
      half_bw_hz = ceiling(minBandwidth / 2)
      filters$from[idx_narrow] = filters$cf[idx_narrow] - half_bw_hz
      filters$to[idx_narrow] = filters$cf[idx_narrow] + half_bw_hz
    }
    filters$from[filters$from < 0] = 0
    filters$to[filters$to > nyquist] = nyquist
  }
  # summary(filters)

  # plot the filters
  if (plotFilters) {
    y = 1 + 5/1000 * (1:nFilters - nrow(filters) / 2)
    plot(filters$cf, rep(1, nFilters), type = 'n', log = 'x',
         ylim = range(y), bty = 'n', ylab = '', xlab = 'Center frequency, Hz')
    for (i in 1:nrow(filters)) {
      y_i = 1 + 5/1000 * (i - nrow(filters) / 2) # rnorm(1, 1, .01)
      segments(x0 = filters$from[i], x1 = filters$to[i], y0 = y[i], y1 = y[i])
    }
  }

  # bandpass filter the signal (seewave::ffilter goes via stdft - not suitable)
  fb = fb_env = sp = vector('list', nFilters)
  for (i in 1:nFilters) {
    if (filterType == 'butterworth') {
      btord = signal::FilterOfOrder(
        n = filterOrder,
        Wc = c(filters$from[i], filters$to[i]) / nyquist,
        type = 'pass'
      )
      bt = signal::butter(btord)
      fb[[i]] = signal::filter(filt = bt, x = audio$sound)
    } else if (filterType == 'chebyshev') {
      btord = signal::FilterOfOrder(
        n = filterOrder,
        Wc = c(filters$from[i], filters$to[i]) / nyquist,
        type = 'pass'
      )
      ch = signal::cheby1(btord, Rp = 12)
      fb[[i]] = signal::filter(filt = ch, x = audio$sound)
    } else if (filterType == 'gammatone') {
      d = min(10/filters$cf[i],  # 10 fundamental periods of cf
              audio$duration / 2)  # but max half the dur of our audio
      t = seq(0, d, length.out = audio$samplingRate * d)
      filter_i = t^(filterOrder - 1) *
        exp(-2 * pi * filters$bandwidth[i] * t) *
        cos(2 * pi * filters$cf[i] * t)
      # filter_i = seewave::gammatone(
      #   f = audio$samplingRate, d = d, cfreq = filters$cf[i])
      # plot(filter_i, type = 'l')
      audio_filt = stats::convolve(audio$sound, filter_i, type = 'filter')
      fb[[i]] = matchLengths(audio_filt, audio$ls)
    }

    # osc(fb[[i]] / max(fb[[i]]))
    fb_env_i = try(
      Mod(seewave::hilbert(
        fb[[i]],
        f = 1,  # not actually needed
        fftw = FALSE)))
    if (!inherits(fb_env_i, 'try-error') && !any(!is.finite(fb_env_i))) {
      fb_env[[i]] = fb_env_i
      # seewave::env(fb[[i]], f = audio$samplingRate,
      #              envt = 'hil',
      #              # msmooth = c(10, 0),
      #              plot = FALSE)
      # plot(fb_env, type = 'l')
      if (!is.null(len)) {
        # downsample
        # fb_env_i = .resample(list(sound = fb_env_i), len = len, lowPass = FALSE)
        fb_env_i = approx(fb_env_i, n = len)$y
      }
      sp[[i]] = matrix(fb_env_i, nrow = 1)
    } else {
      sp[[i]] = matrix(0, nrow = 1, ncol = len)
    }
  }

  if ('audSpec' %in% output | 'audSpec_processed' %in% output) {
    audSpec = do.call(rbind, sp)
    audSpec[is.na(audSpec)] = 0
    rownames(audSpec) = names(fb) = names(fb_env) = filters$cf / 1000
    colnames(audSpec) = step / 2 + step * (0 : (ncol(audSpec) - 1)) +
      audio$timeShift * 1000
    # or: seq(0, audio$duration, length.out = ncol(audSpec)) * 1000

    # rescale etc in order to return $audSpectrogram_processed
    Z1 = t(audSpec)
    # set to zero under dynamic range
    threshold = max(Z1) / 10^(dynamicRange/20)
    Z1[Z1 < threshold] = 0
    # re-normalize
    positives = which(Z1 > 0)
    nonpositives = which(Z1 <= 0)
    Z1[positives] = log(Z1[positives])
    if (length(positives) > 0 & length(nonpositives) > 0) {
      Z1[nonpositives] = min(Z1[positives])
    }
    Z1 = Z1 - min(Z1)

    # contrast & brightness
    contrast_exp = exp(3 * contrast)
    brightness_exp = exp(3 * brightness)
    # visualization: plot(exp(3 * seq(-1, 1, by = .01)), type = 'l')
    if (contrast_exp != 1) {
      Z1 = Z1 ^ contrast_exp
    }
    if (any(Z1 != 0)) Z1 = Z1 / max(Z1)
    if (brightness_exp != 1) {
      Z1 = Z1 / brightness_exp
    }
    if (brightness_exp < 1) {
      Z1[Z1 > 1] = 1 # otherwise values >1 are shown as white instead of black
    }
  } else {
    audSpec = Z1 = NA
  }

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots,
                          audio$filename_noExt,
                          "_audSpectrogram.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    if (is.null(ylim)) ylim = c(minFreq, maxFreq) / 1000
    plotSpec(
      X = as.numeric(colnames(audSpec)),  # time
      Y = as.numeric(rownames(audSpec)),  # freq
      Z = Z1,
      audio = audio, internal = NULL, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim, yScale = yScale,
      contrast = contrast, brightness = brightness,
      maxPoints = maxPoints, colorTheme = colorTheme, col = col,
      extraContour = extraContour,
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      width = width, height = height,
      units = units, res = res,
      ...
    )
    if (is.character(audio$savePlots)) {
      dev.off()
    }
  }

  return(list(
    audSpec = audSpec,
    audSpec_processed = t(Z1),
    filterbank = fb,
    filterbank_env = fb_env
  )[output])
}
