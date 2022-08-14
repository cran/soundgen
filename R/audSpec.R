#' Auditory spectrogram
#'
#' Produces an auditory spectrogram by extracting a bank of bandpass filters
#' (work in progress). While tuneR::audspec is based on FFT, here we convolve
#' the sound with a bank of filters. The main difference is that we don't window
#' the signal and de facto get variable temporal resolution in different
#' frequency channels, as with a wavelet transform. The filters are currently
#' third-order Butterworth bandpass filters implemented in
#' \code{\link[signal]{butter}}.
#'
#' @inheritParams spectrogram
#' @param nFilters the number of filters (determines frequency resolution)
#' @param step step, ms (determines time resolution). step = NULL means no
#'   downsampling at all (ncol of output = length of input audio)
#' @param minFreq,maxFreq the range of frequencies to analyze
#' @param minBandwidth minimum filter bandwidth, Hz (otherwise filters may
#'   become too narrow when nFilters is high)
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
#'   yScale = 'ERB',
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   brightness = -.1,  # reduce brightness
#'   colorTheme = 'heat.colors',  # pick color theme
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   grid = 5,  # to customize, add manually with graphics::grid()
#'   ylim = c(0.1, 5),  # always in kHz
#'   main = 'My auditory spectrogram' # title
#'   # + axis labels, etc
#' )
#'
#' # change dynamic range
#' audSpectrogram(sound, samplingRate = 16000, dynamicRange = 40)
#' audSpectrogram(sound, samplingRate = 16000, dynamicRange = 120)
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
  nFilters = 128,
  minFreq = 20,
  maxFreq = samplingRate / 2,
  minBandwidth = 10,
  reportEvery = NULL,
  cores = 1,
  plot = TRUE,
  savePlots = NULL,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('bark', 'mel', 'ERB', 'log')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
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
  nFilters,
  minFreq = 20,
  maxFreq = audio$samplingRate / 2,
  minBandwidth = 1,
  plot = TRUE,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = 'bark',
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
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
  if (is.null(maxFreq) || length(maxFreq) < 1) maxFreq = audio$samplingRate / 2
  if (!is.null(step)) {
    len = max(1, round(audio$duration * 1000 / step))
    step = audio$duration * 1000 / len  # avoid rounding error
  } else {
    len = NULL
    step = 1000 / audio$samplingRate
  }
  nyquist = audio$samplingRate / 2

  # set up filters
  if (yScale == 'log') {
    filter_width = (HzToSemitones(maxFreq) - HzToSemitones(minFreq)) / nFilters
    halfFW = filter_width / 2
    cf_semitones = seq(
      HzToSemitones(minFreq),
      HzToSemitones(min(maxFreq, nyquist / 2^(filter_width / 2 / 12))) - 1,
      length.out = nFilters
    )
    filters = data.frame(
      cf = semitonesToHz(cf_semitones),
      from = semitonesToHz(cf_semitones - halfFW),
      to = semitonesToHz(cf_semitones + halfFW)
    )
  } else if (yScale == 'bark') {
    filter_width = (tuneR::hz2bark(maxFreq) - tuneR::hz2bark(minFreq)) / nFilters
    halfFW = filter_width / 2
    cf_bark = seq(
      tuneR::hz2bark(minFreq + tuneR::bark2hz(halfFW)),
      tuneR::hz2bark(min(maxFreq, nyquist)) - 1,
      length.out = nFilters
    )
    filters = data.frame(
      cf = tuneR::bark2hz(cf_bark),
      from = tuneR::bark2hz(cf_bark - halfFW),
      to = tuneR::bark2hz(cf_bark + halfFW)
    )
  } else if (yScale == 'mel') {
    filter_width = (hz2mel(maxFreq) - hz2mel(minFreq)) / nFilters
    halfFW = filter_width / 2
    cf_mel = seq(
      hz2mel(minFreq + tuneR::mel2hz(halfFW)),
      hz2mel(min(maxFreq, nyquist)) - 1,
      length.out = nFilters
    )
    filters = data.frame(
      cf = tuneR::mel2hz(cf_mel),
      from = tuneR::mel2hz(cf_mel - halfFW),
      to = tuneR::mel2hz(cf_mel + halfFW)
    )
  } else if (yScale == 'ERB') {
    filter_width = (HzToERB(maxFreq) - HzToERB(minFreq)) / nFilters
    halfFW = filter_width / 2
    cf_erb = seq(
      HzToERB(minFreq + ERBToHz(halfFW)),
      HzToERB(min(maxFreq, nyquist)) - 1,
      length.out = nFilters
    )
    filters = data.frame(
      cf = ERBToHz(cf_erb),
      from = ERBToHz(cf_erb - halfFW),
      to = ERBToHz(cf_erb + halfFW)
    )
  }

  # make sure the bandwidth in Hz (!) is always wide enough to avoid crashing
  bw_hz = filters$to - filters$from
  idx_narrow = which(bw_hz < minBandwidth)
  if (length(idx_narrow) > 0) {
    half_bw_hz = ceiling(minBandwidth / 2)
    filters$from[idx_narrow] = filters$cf[idx_narrow] - half_bw_hz
    filters$to[idx_narrow] = filters$cf[idx_narrow] + half_bw_hz
  }
  # summary(filters)

  # bandpass filter the signal. seewave::ffilter goes via stdft, so it's not suitable
  fb = sp = vector('list', nFilters)
  for (i in 1:nFilters) {
    btord = signal::FilterOfOrder(
      n = 3,
      Wc = c(filters$from[i], filters$to[i]) / nyquist,
      type = 'pass'
    )
    bt = signal::butter(btord)
    fb[[i]] = signal::filter(filt = bt, x = audio$sound)
    # osc(fb[[i]] / max(fb[[i]]))
    fb_env = seewave::env(fb[[i]], f = audio$samplingRate,
                          envt = 'hil',
                          # msmooth = c(10, 0),
                          plot = F)
    # plot(fb_env, type = 'l')
    if (!is.null(len)) {
      # downsample
      fb_env = .resample(list(sound = fb_env), len = len, lowPass = FALSE)
    }
    sp[[i]] = matrix(fb_env, nrow = 1)
  }
  audSpec = do.call(rbind, sp)
  audSpec[is.na(audSpec)] = 0
  rownames(audSpec) = filters$cf / 1000
  colnames(audSpec) = step / 2 + step * (0 : (ncol(audSpec) - 1))
  # or: seq(0, audio$duration, length.out = ncol(audSpec)) * 1000

  # rescale etc in order to return $audSpectorgram_processed
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
      maxPoints = maxPoints, colorTheme = colorTheme,
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
    filterbank = fb
  ))
}
