#' Bandpass/stop filters
#'
#' Filtering in the frequency domain with FFT-iFFT: low-pass, high-pass,
#' bandpass, and bandstop filters. Similar to \code{\link[seewave]{ffilter}},
#' but here we use FFT instead of STFT - that is, the entire sound is processed
#' at once. This works best for relatively short sounds (seconds), but gives us
#' maximum precision (e.g., for precise notch filtering) and doesn't affect the
#' attack and decay. NAs are accepted and can be interpolated or preserved in
#' the output. Because we don't do STFT, arbitrarily short vectors are also fine
#' as input - for example, we can apply a low-pass filter prior to decimation
#' when changing the sampling rate without aliasing. Note that, unlike
#' \code{\link{pitchSmoothPraat}}, \code{bandpass} applies an abrupt cutoff
#' instead of a smooth gaussian filter.
#'
#' Algorithm: fill in NAs with constant interpolation at the edges and linear
#' interpolation in the middle; perform FFT; set the frequency ranges to be
#' filtered out to 0; perform inverse FFT; set to the original scale; put the
#' NAs back in.
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param lwr,upr cutoff frequencies, Hz. Specifying just lwr gives a high-pass
#'   filter, just upr low-pass filter with action = 'pass' (or vice versa with
#'   action = 'stop'). Specifying both lwr and upr a bandpass/bandstop filter,
#'   depending on 'action'
#' @param action "pass" = preserve the selected frequency range (bandpass),
#'   "stop" = remove the selected frequency range (bandstop)
#' @param dB a positive number giving the strength of effect in dB (defaults to
#'   Inf - complete removal of selected frequencies)
#' @param na.rm if TRUE, NAs are interpolated, otherwise they are preserved in
#'   the output
#' @param normalize if TRUE, resets the output to the original scale (otherwise
#'   filtering often reduces the amplitude)
#' @param saveAudio full path to the folder in which to save the processed audio
#' @export
#' @examples
#' # Filter white noise
#' s1 = fade(c(runif(2000, -1, 1)), samplingRate = 16000)
#' bandpass(s1, 16000, upr = 2000, plot = TRUE)    # low-pass
#' bandpass(s1, 16000, lwr = 2000, dB = 40, plot = TRUE)  # high-pass by 40 dB
#' bandpass(s1, 16000, lwr = 1000, upr = 1100, action = 'stop', plot = TRUE) # bandstop
#' s2 = bandpass(s1, 16000, lwr = 2000, upr = 2100, plot = TRUE) # bandpass
#' # playme(rep(s2, 5))
#' spectrogram(s2, 16000)  # more accurate than plotting the spectrum with plot = TRUE
#'
#' # a short vector with some NAs
#' x = rnorm(150, 10) + 3 * sin((1:50) / 5)
#' x[sample(1:length(x), 50)] = NA
#' plot(x, type = 'l')
#' points(bandpass(x, samplingRate = 100, upr = 10), type = 'l', col = 'blue')
#'
#' \dontrun{
#' # precise notch filtering is possible, even in low frequencies
#' whiteNoise = runif(8000, -1, 1)
#' s3 = bandpass(whiteNoise, 16000, lwr = 30, upr = 40,
#'               plot = TRUE, xlim = c(0, 500))
#' playme(rep(s3, 5))
#' spectrogram(s3, 16000, windowLength = 150, yScale = 'log')
#'
#' # compare the same with STFT
#' s4 = seewave::ffilter(whiteNoise, f = 16000, from = 30, to = 40)
#' spectrogram(s4, 16000, windowLength = 150, yScale = 'log')
#' # (note: works better as wl approaches length(s4))
#'
#' # high-pass all audio files in a folder
#' bandpass('~/Downloads/temp', saveAudio = '~/Downloads/temp/hp2000/',
#'          lwr = 2000, savePlots = '~/Downloads/temp/hp2000/')
#' }
bandpass = function(
    x,
    samplingRate = NULL,
    lwr = NULL,
    upr = NULL,
    action = c('pass', 'stop')[1],
    dB = Inf,
    na.rm = TRUE,
    from = NULL,
    to = NULL,
    normalize = FALSE,
    reportEvery = NULL,
    cores = 1,
    saveAudio = NULL,
    plot = FALSE,
    savePlots = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'savePlots', 'saveAudio',
    'width', 'height', 'units')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = '.bandpass',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "bandpass", width = paste0(width, units)))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Bandpass filter per sound
#'
#' Internal soundgen function
#'
#' @param audio a list returned by \code{readAudio}
#' @inheritParams bandpass
#' @inheritParams segment
#' @keywords internal
.bandpass = function(audio,
                     lwr = NULL,
                     upr = NULL,
                     action = c('pass', 'stop')[1],
                     dB = Inf,
                     na.rm = TRUE,
                     normalize = FALSE,
                     plot = FALSE,
                     width = 900,
                     height = 500,
                     units = 'px',
                     res = NA,
                     ...) {
  if ((is.null(lwr) & is.null(upr)) | (!is.numeric(lwr) & !is.numeric(upr)))
    stop('Nothing to do: specify lwr and/or upr')
  if ((is.numeric(lwr) & is.numeric(upr)) && lwr > upr) {
    lwr = upr
    upr = lwr
    warning('Found lwr >= upr; swapping them')
  }
  x = audio$sound
  if (!any(is.finite(x))) return(x)
  len = length(x)
  mean_x = mean(x, na.rm = TRUE)
  ran_x = range(x, na.rm = TRUE)
  x = x - mean_x  # center to avoid a weird fade-out
  if (len > 10000) {
    # for long files, pad with 0 to the nearest power of 2 - much faster fft
    pad_with = ifelse(prod(ran_x) < 0, 0, mean(x))
    target_len = 2 ^ ceiling(log2(len))
    n_zeros = target_len - len
    x = c(x, rep(pad_with, n_zeros))
    len = length(x)
  } else {
    n_zeros = 0
  }
  half_len = len %/% 2

  # interpolate NAs
  idx_na = which(is.na(x))
  n_na = length(idx_na)
  if (n_na > 0) x = intplNA(x, idx_na = idx_na)

  # get spectrum
  sp = stats::fft(x)

  # select frequency range
  if (!is.null(lwr)) {
    idx_lwr = max(1, round(half_len / (audio$samplingRate / lwr) * 2))
  } else {
    idx_lwr = 1
  }

  if (!is.null(upr)) {
    idx_upr = min(half_len, round(half_len / (audio$samplingRate / upr) * 2))
  } else {
    idx_upr = half_len
  }

  # half-filter of 1/0
  if (is.finite(dB)) {
    m = 10 ^ (-abs(dB) / 20)
  } else {
    m = 0
  }
  if (action == 'pass') {
    filter = rep(m, half_len)
    filter[idx_lwr:idx_upr] = 1
  } else {
    filter = rep(1, half_len)
    filter[idx_lwr:idx_upr] = m
  }
  # plot(filter)

  # mirror image of filter to cover the whole spectrum
  even = len %% 2 == 0
  if (even) {
    filter_full = c(filter, rev(filter))
  } else {
    filter_full = c(filter, filter[half_len], rev(filter))
  }
  filter_full[c(1, len)] = 1  # why?
  # plot(filter_full)

  # inverse fft
  x_new = Re(fft(sp * filter_full, inverse = TRUE)) / len

  # trim extra zeros and fade-out (10 ms)
  if (n_zeros > 0) {
    x_new = .fade(
      list(sound = x_new[1:(len - n_zeros)],
           samplingRate = audio$samplingRate),
      fadeIn_points = 0,
      fadeOut_points = min(half_len, audio$samplingRate * .01))
  }

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_bandpass.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    bin_width = audio$samplingRate / 2 / len
    freq = seq(bin_width / 2,
               audio$samplingRate / 2 - bin_width / 2,
               length.out = half_len)
    sp_dB = 20 * log10(abs(sp[1:half_len]))
    sp_dB = sp_dB - max(sp_dB)
    if (!exists('ylim')) ylim = c(-120, 0)
    if (!exists('xlab')) xlab = 'Hz'
    if (!exists('ylab')) ylab = 'dB'
    plot(freq, sp_dB, type = 'l', ylim = ylim, xlab = xlab, ylab = ylab, ...)

    sp_new = stats::fft(x_new - min(x_new))
    sp_new_dB = 20 * log10(abs(sp_new[1:half_len]))
    sp_new_dB = sp_new_dB - max(sp_new_dB)
    points(freq, sp_new_dB, type = 'l', col = 'blue')
    if (is.character(audio$savePlots)) dev.off()
  }

  # back to original scale
  if (normalize) x_new = x_new / max(abs(x_new)) * audio$scale_used
  x_new = x_new + mean_x

  if (!is.null(audio$saveAudio)) {
    if (!dir.exists(audio$saveAudio)) dir.create(audio$saveAudio)
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(x_new, audio = audio, filename = filename)
  }

  # put NAs back in
  if (!na.rm && n_na > 0) x_new[idx_na] = NA
  invisible(x_new)
}


#' Pitch smoothing as in Praat
#'
#' Smoothes an intonation (pitch) contour with a low-pass filter, as in Praat
#' (http://www.fon.hum.uva.nl/praat/). Algorithm: interpolates missing values
#' (unvoiced frames), performs FFT to obtain the spectrum, multiplies by a
#' Gaussian filter, performs an inverse FFT, and fills the missing values back
#' in. The \code{bandwidth} parameter is about half the cutoff frequency (ie
#' some frequencies will still be present up to ~2 * bandwidth)
#'
#' @seealso \code{\link{analyze}}
#'
#' @param pitch numeric vector of pitch values (NA = unvoiced)
#' @param bandwidth the bandwidth of low-pass filter, Hz (high = less smoothing,
#'   close to zero = more smoothing)
#' @param samplingRate the number of pitch values per second
#' @param plot if TRUE, plots the original and smoothed pitch contours
#' @export
#' @examples
#' pitch = c(NA, NA, 405, 441, 459, 459, 460, 462, 462, 458, 458, 445, 458, 451,
#' 444, 444, 430, 416, 409, 403, 403, 389, 375, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, 183, 677, 677, 846, 883, 886, 924, 938, 883, 946, 846, 911, 826, 826,
#' 788, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 307,
#' 307, 368, 377, 383, 383, 383, 380, 377, 377, 377, 374, 374, 375, 375, 375,
#' 375, 368, 371, 374, 375, 361, 375, 389, 375, 375, 375, 375, 375, 314, 169,
#' NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 238, 285, 361, 374, 375, 375,
#' 375, 375, 375, 389, 403, 389, 389, 375, 375, 389, 375, 348, 361, 375, 348,
#' 348, 361, 348, 342, 361, 361, 361, 365, 365, 361, 966, 966, 966, 959, 959,
#' 946, 1021, 1021, 1026, 1086, 1131, 1131, 1146, 1130, 1172, 1240, 1172, 1117,
#' 1103, 1026, 1026, 966, 919, 946, 882, 832, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA)
#' pitchSmoothPraat(pitch, bandwidth = 10, samplingRate = 40, plot = TRUE)
#' pitchSmoothPraat(pitch, bandwidth = 2, samplingRate = 40, plot = TRUE)
pitchSmoothPraat = function(pitch,
                            bandwidth,
                            samplingRate,
                            plot = FALSE) {
  # make positive
  m = min(pitch, na.rm = TRUE)
  pitch = pitch - m
  idx_unv = which(is.na(pitch))
  len = length(pitch)
  bin_width = samplingRate / 2 / len
  half_len = len %/% 2
  even = len %% 2 == 0

  # interpolate NAs
  pitch1 = intplNA(pitch, idx_na = idx_unv)

  # get spectrum
  sp = stats::fft(pitch1)
  freq = seq(bin_width / 2,
             samplingRate / 2 - bin_width / 2,
             length.out = half_len)
  # plot(freq, abs(sp[1:half_len]) / max(abs(sp[1:half_len])), type = 'l')

  # gaussian filter
  filter = exp(-(freq/bandwidth)^2)  # NB: a bit different from dnorm()
  # points(freq, filter, type = 'l', col = 'blue')
  if (even) {
    filter = c(filter, rev(filter))
  } else {
    filter = c(filter, filter[half_len], rev(filter))
  }

  # inverse fft
  pitch2 = abs(fft(sp * filter, inverse = TRUE)) / len
  pitch2[idx_unv] = NA
  pitch2 = pitch2 + m  # back to the original scale

  if (plot) {
    plot(pitch + m)
    points(pitch2, type = 'l', col = 'red')
  }
  return(pitch2)
}



