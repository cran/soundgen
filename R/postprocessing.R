#' Fade
#'
#' Applies fade-in and/or fade-out of variable length, shape, and steepness. The
#' resulting effect softens the attack and release of a waveform.
#'
#' @seealso \code{\link{crossFade}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param fadeIn,fadeOut length of segments for fading in and out, ms (0 = no
#'   fade)
#' @param fadeIn_points,fadeOut_points length of segments for fading in and out,
#'   points (if specified, override \code{fadeIn/fadeOut})
#' @param shape controls the type of fade function: 'lin' = linear, 'exp' =
#'   exponential, 'log' = logarithmic, 'cos' = cosine, 'logistic' = logistic
#'   S-curve
#' @param steepness scaling factor regulating the steepness of fading curves
#'   (except for shapes 'lin' and 'cos'): 0 = linear, >1 = steeper than default
#' @param plot if TRUE, produces an oscillogram of the waveform after fading
#' @return Returns a numeric vector of the same length as input
#' @export
#' @examples
#' #' # Fading a real sound: say we want fast attack and slow release
#' s = soundgen(attack = 0, windowLength = 10,
#'              sylLen = 500, addSilence = 0)
#' # playme(s)
#' s1 = fade(s, fadeIn = 40, fadeOut = 350,
#'           samplingRate = 16000, shape = 'cos', plot = TRUE)
#' # playme(s1)
#'
#' # Illustration of fade shapes
#' x = runif(5000, min = -1, max = 1)  # make sure to zero-center input!!!
#' # plot(x, type = 'l')
#' y = fade(x, fadeIn_points = 1000, fadeOut_points = 0, plot = TRUE)
#' y = fade(x, fadeIn_points = 1000, fadeOut_points = 1500,
#'          shape = 'exp', steepness = 1, plot = TRUE)
#' y = fade(x, fadeIn_points = 1500, fadeOut_points = 500,
#'          shape = 'log', steepness = 1, plot = TRUE)
#' y = fade(x, fadeIn_points = 1500, fadeOut_points = 500,
#'          shape = 'log', steepness = 3, plot = TRUE)
#' y = fade(x, fadeIn_points = 1500, fadeOut_points = 1500,
#'          shape = 'cos', plot = TRUE)
#' y = fade(x, fadeIn_points = 1500, fadeOut_points = 1500,
#'          shape = 'logistic', steepness = 1, plot = TRUE)
#' y = fade(x, fadeIn_points = 1500, fadeOut_points = 1500,
#'          shape = 'logistic', steepness = 3, plot = TRUE)
#' y = fade(x, fadeIn_points = 1500, fadeOut_points = 1500,
#'          shape = 'gaussian', steepness = 1.5, plot = TRUE)
#'
#' \dontrun{
#'   fade('~/Downloads/temp', fadeIn = 500, fadeOut = 500, savePlots = '')
#' }
fade = function(
    x,
    fadeIn = 50,
    fadeOut = 50,
    fadeIn_points = NULL,
    fadeOut_points = NULL,
    samplingRate = NULL,
    scale = NULL,
    shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')[1],
    steepness = 1,
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
    'x', 'samplingRate', 'scale', 'reportEvery', 'cores',
    'savePlots', 'saveAudio')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    scale = scale,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = '.fade',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "fade", width = paste0(width, units)))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Fade per sound
#'
#' Internal soundgen function
#'
#' @param audio a list returned by \code{readAudio}
#' @inheritParams fade
#' @inheritParams segment
#' @keywords internal
.fade = function(
    audio,
    fadeIn = 1000,
    fadeOut = 1000,
    fadeIn_points = NULL,
    fadeOut_points = NULL,
    samplingRate = NULL,
    shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')[1],
    steepness = 1,
    plot = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...) {
  if ((!is.numeric(fadeIn) | fadeIn < 1) &
      (!is.numeric(fadeOut) | fadeOut < 1)) {
    return(audio$sound)
  }

  valid_shapes = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')
  if (!shape %in% valid_shapes) {
    warning(paste(
      shape, 'is not a valid shape, defaulting to "lin".',
      'Implemented shapes: "', paste(valid_shapes, collapse = ", "), '"'
    ))
    shape = 'lin'
  }

  if (steepness < 0) {
    steepness = 1
    warning('steepness must be non-negative; resetting to 1')
  } else if (steepness == 0) {
    shape = 'lin'
  }

  # convert fade from ms to points
  len = length(audio$sound)
  if (is.null(fadeIn_points))
    fadeIn_points = round(fadeIn * audio$samplingRate / 1000)
  if (is.null(fadeOut_points))
    fadeOut_points = round(fadeOut * audio$samplingRate / 1000)

  # make sure fadeIn/Out region is no longer than the entire sound
  fadeIn_points = round(min(len, fadeIn_points))
  fadeOut_points = round(min(len, fadeOut_points))

  # prepare a fade-in/out curve of requisite length
  time_in = seq(0, 1, length.out = fadeIn_points)
  time_out = seq(1, 0, length.out = fadeOut_points)
  if (shape == 'lin') {
    fi = time_in
    fo = time_out
  } else if (shape == 'exp') {
    m = exp(steepness * 3)  # to avoid taking min/max within zeroOne
    fi = zeroOne(exp(time_in * steepness * 3), xmin = 1, xmax = m)
    fo = zeroOne(exp(time_out * steepness * 3), xmin = 1, xmax = m)
  } else if (shape == 'log') {
    m = exp(steepness * 3)
    fi = 1 - rev(zeroOne(exp(time_in * steepness * 3), xmin = 1, xmax = m))
    fo = 1 - rev(zeroOne(exp(time_out * steepness * 3), xmin = 1, xmax = m))
  } else if (shape == 'cos') {
    fi = (1 - cos(time_in * pi)) / 2
    fo = (1 - cos(time_out * pi)) / 2
  } else if (shape == 'logistic') {
    xmin = 1 - 1 / (1 + exp(6 * steepness * (0 - .5)))
    xmax = 1 - 1 / (1 + exp(6 * steepness * (1 - .5)))
    fi = zeroOne(1 - 1 / (1 + exp(6 * steepness * (time_in - .5))),
                 xmin = xmin, xmax = xmax)
    fo = zeroOne(1 - 1 / (1 + exp(6 * steepness * (time_out - .5))),
                 xmin = xmin, xmax = xmax)
  } else if (shape == 'gaussian') {
    xmin = dnorm(x = -1, mean = 0, sd = .4 / steepness)
    xmax = dnorm(x = 0, mean = 0, sd = .4 / steepness)
    fi = zeroOne(dnorm(x = -rev(time_in), mean = 0, sd = .4 / steepness),
                 xmin = xmin, xmax = xmax)
    fo = rev(zeroOne(dnorm(x = time_out, mean = 0, sd = .4 / steepness),
                     xmin = xmin, xmax = xmax))
  }
  # plot(fi, type = 'l', xlim = c(1, max(fadeIn, fadeOut)))
  # points(fo, type = 'l', col = 'red')

  # apply the fade
  if (fadeIn_points > 1) {
    idx_in = seq_len(fadeIn_points)
    audio$sound[idx_in] = audio$sound[idx_in] * fi
  }
  if (fadeOut_points > 0) {
    idx_out = (len - fadeOut_points + 1):len
    audio$sound[idx_out] = audio$sound[idx_out] * fo
  }

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_fade.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    .osc(audio[!names(audio) %in% c('savePlots')], maxPoints = Inf)
    # plot(audio$sound, type = 'l', xlab = '', ylab = '')
    abline(v = fadeIn_points / audio$samplingRate * 1000, col = 'blue')
    abline(v = (len - fadeOut_points)  / audio$samplingRate * 1000, col = 'blue')
    if (is.character(audio$savePlots)) dev.off()
  }
  if (is.character(audio$saveAudio)) {
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(audio$sound, audio = audio, filename = filename)
  }
  audio$sound
}


#' Join two waveforms by cross-fading
#'
#' \code{crossFade} joins two input vectors (waveforms) by cross-fading. First
#' it truncates both input vectors, so that \code{ampl1} ends with a zero
#' crossing and \code{ampl2} starts with a zero crossing, both on an upward
#' portion of the soundwave. Then it cross-fades both vectors linearly with an
#' overlap of crossLen or crossLenPoints. If the input vectors are too short for
#' the specified length of cross-faded region, the two vectors are concatenated
#' at zero crossings instead of cross-fading. Soundgen uses \code{crossFade} for
#' gluing together epochs with different regimes of pitch effects (see the
#' vignette on sound generation), but it can also be useful for joining two
#' separately generated sounds without audible artifacts.
#'
#' @seealso \code{\link{fade}}
#'
#' @param ampl1,ampl2 two numeric vectors (waveforms) to be joined
#' @param crossLenPoints (optional) the length of overlap in points
#' @param crossLen the length of overlap in ms (overrides crossLenPoints)
#' @param samplingRate the sampling rate of input vectors, Hz (needed only if
#'   crossLen is given in ms rather than points)
#' @inheritParams fade
#' @export
#' @return Returns a numeric vector.
#' @examples
#' sound1 = sin(1:100 / 9)
#' sound2 = sin(7:107 / 3)
#' plot(c(sound1, sound2), type = 'b')
#' # an ugly discontinuity at 100 that will make an audible click
#'
#' sound = crossFade(sound1, sound2, crossLenPoints = 5)
#' plot(sound, type = 'b') # a nice, smooth transition
#' length(sound) # but note that cross-fading costs us ~60 points
#' #  because of trimming to zero crossings and then overlapping
#'
#' \dontrun{
#' # Actual sounds, alternative shapes of fade-in/out
#' sound3 = soundgen(formants = 'a', pitch = 200,
#'                   addSilence = 0, attackLen = c(50, 0))
#' sound4 = soundgen(formants = 'u', pitch = 200,
#'                   addSilence = 0, attackLen = c(0, 50))
#'
#' # simple concatenation (with a click)
#' playme(c(sound3, sound4), 16000)
#'
#' # concatentation from zc to zc (no click, but a rough transition)
#' playme(crossFade(sound3, sound4, crossLen = 0), 16000)
#'
#' # linear crossFade over 35 ms - brief, but smooth
#' playme(crossFade(sound3, sound4, crossLen = 35, samplingRate = 16000), 16000)
#'
#' # s-shaped cross-fade over 300 ms (shortens the sound by ~300 ms)
#' playme(crossFade(sound3, sound4, samplingRate = 16000,
#'                  crossLen = 300, shape = 'cos'), 16000)
#' }
crossFade = function(
    ampl1,
    ampl2,
    crossLenPoints = 240,
    crossLen = NULL,
    samplingRate = NULL,
    shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')[1],
    steepness = 1) {
  # cut to the nearest zero crossings
  zc1 = findZeroCrossing(ampl1, location = length(ampl1))
  if (!is.na(zc1)) {
    # up to the last point before the last zero-crossing in sound 1 on the
    # upward curve + one extra zero (to have a nice, smooth transition line:
    # last negative in s1 - zero - first positive in s2)
    ampl1 = c(ampl1[seq_len(zc1)], 0)
  }
  zc2 = findZeroCrossing(ampl2, location = 1)
  if (!is.na(zc2)) {
    ampl2 = ampl2[(zc2 + 1):length(ampl2)]
    # from the first positive point on the upward curve. Note the +1 - next
    # point after the first zero crossing in s2
  }

  # check whether there is enough data to cross-fade. Note that ampl1 or ampl2
  # may even become shorter than crossLenPoints after we shortened them to the
  # nearest zero crossing
  if (is.null(crossLenPoints) |
      (!is.null(crossLen) & !is.null(samplingRate))) {
    crossLenPoints = floor(crossLen * samplingRate / 1000)
  }
  crossLenPoints = min(crossLenPoints, length(ampl1) - 1, length(ampl2) - 1)

  # concatenate or cross-fade
  if (crossLenPoints < 2) {
    # for segments that are too short,
    # just concatenate from zero crossing to zero crossing
    ampl = c(ampl1, ampl2)
  } else {
    # for segments that are long enough, cross-fade properly
    # multipl = seq(0, 1, length.out = crossLenPoints)
    multipl = .fade(list(sound = rep(1, crossLenPoints), samplingRate = samplingRate),
                    fadeIn_points = crossLenPoints,
                    fadeOut_points = 0,
                    shape = shape,
                    steepness = steepness)
    idx1 = length(ampl1) - crossLenPoints
    cross = rev(multipl) * ampl1[(idx1 + 1):length(ampl1)] +
      multipl * ampl2[seq_len(crossLenPoints)]
    ampl = c(ampl1[seq_len(idx1)],
             cross,
             ampl2[(crossLenPoints + 1):length(ampl2)])
  }
  return(ampl)
}


#' Flat spectrum
#'
#' Flattens the spectrum of a sound by smoothing in the frequency domain. Can be
#' used for removing formants without modifying pitch contour or voice quality
#' (the balance of harmonic and noise components), followed by the addition of a
#' new spectral envelope (cf. \code{\link{transplantFormants}}). Algorithm:
#' makes a spectrogram, flattens the real part of the smoothed spectrum of each
#' STFT frame, and transforms back into time domain with inverse STFT (see also
#' \code{\link{addFormants}}).
#'
#' @seealso \code{\link{addFormants}} \code{\link{transplantFormants}}
#'
#' @return Returns a numeric vector with the same sampling rate as the input.
#' @inheritParams spectrogram
#' @inheritParams addAM
#' @param freqWindow the width of smoothing window, Hz. Defaults to median
#'   pitch estimated by \code{\link{analyze}}
#' @export
#' @examples
#' sound_aii = soundgen(formants = 'aii')
#' # playme(sound_aii, 16000)
#' seewave::meanspec(sound_aii, f = 16000, dB = 'max0')
#'
#' sound_flat = flatSpectrum(sound_aii, freqWindow = 150, samplingRate = 16000)
#' # playme(sound_flat, 16000)
#' seewave::meanspec(sound_flat, f = 16000, dB = 'max0')
#' # harmonics are still there, but formants are gone and can be replaced
#'
#' \dontrun{
#' # Now let's make a sheep say "aii"
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' playme(sheep)
#' sheep_flat = flatSpectrum(sheep)
#' playme(sheep_flat, sheep@samp.rate)
#' seewave::spec(sheep_flat, f = sheep@samp.rate, dB = 'max0')
#'
#' # So far we have a sheep bleating with a flat spectrum;
#' # now let's add new formants
#' sheep_aii = addFormants(sheep_flat,
#'   samplingRate = sheep@samp.rate,
#'   formants = 'aii',
#'   lipRad = -3)  # negative lipRad to counter unnatural flat source
#' playme(sheep_aii, sheep@samp.rate)
#' spectrogram(sheep_aii, sheep@samp.rate)
#' seewave::spec(sheep_aii, f = sheep@samp.rate, dB = 'max0')
#' }
flatSpectrum = function(x,
                        samplingRate = NULL,
                        freqWindow = NULL,
                        dynamicRange = 80,
                        windowLength = 50,
                        step = NULL,
                        overlap = 90,
                        wn = 'gaussian',
                        zp = 0,
                        play = FALSE,
                        saveAudio = NULL,
                        reportEvery = NULL,
                        cores = 1) {
  # match args
  myPars = c(as.list(environment()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    funToCall = '.flatSpectrum',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Flat spectrum per sound
#'
#' Internal soundgen function, see \code{\link{flatSpectrum}}.
#'
#' @inheritParams flatSpectrum
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.flatSpectrum = function(audio,
                         freqWindow = NULL,
                         samplingRate = NULL,
                         dynamicRange = 80,
                         windowLength = 50,
                         step = NULL,
                         overlap = 90,
                         wn = 'gaussian',
                         zp = 0,
                         play = FALSE,
                         saveAudio = NULL) {
  # default freqWindow = f0
  if (!is.numeric(freqWindow)) {
    a = analyze(audio$sound, audio$samplingRate, plot = FALSE)
    freqWindow = median(a$detailed$pitch, na.rm = TRUE)
  }

  # get a spectrogram of the original sound
  spec = .spectrogram(audio,
                      dynamicRange = dynamicRange,
                      windowLength = windowLength,
                      step = step,
                      overlap = overlap,
                      wn = wn,
                      zp = zp,
                      output = 'complex',
                      padWithSilence = FALSE,
                      plot = FALSE)

  # calculate the width of smoothing window in bins
  freqRange_kHz = diff(range(as.numeric(rownames(spec))))
  freqBin_Hz = freqRange_kHz * 1000 / nrow(spec)
  freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
  if (freqWindow_bins < 3) {
    message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz * 3)))
    freqWindow_bins = 3
  }

  # modify the spectrogram
  for (i in seq_len(ncol(spec))) {
    abs_s = abs(spec[, i])
    sc = max(abs_s)
    cor_coef = .flatEnv(list(sound = abs_s,
                             samplingRate = 1,  # sr not really needed
                             scale = sc,
                             scale_used = sc),
                        method = 'peak',
                        dynamicRange = dynamicRange,
                        windowLength_points = freqWindow_bins) / abs_s
    # plot(cor_coef, type = 'b')
    # spec[, i] = complex(real = Re(spec[, i]) * cor_coef,
    #                     imaginary = Im(spec[, i]))
    spec[, i] = spec[, i] * cor_coef
    # plot(abs(spec[, i]), type = 'b')
  }

  # recreate an audio from the modified spectrogram
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  sound_new = as.numeric(
    seewave::istft(
      spec,
      f = audio$samplingRate,
      ovlp = overlap,
      wl = windowLength_points,
      output = "matrix"
    )
  )
  sound_new = sound_new / max(abs(range(sound_new))) * audio$scale_used
  if (play) playme(sound_new, audio$samplingRate)
  if (is.character(audio$saveAudio)) {
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(sound_new, audio = audio, filename = filename)
  }
  # spectrogram(sound_new, audio$samplingRate)
  sound_new
}


#' Reverb & echo
#'
#' Adds reverberation and/or echo to a sound. Algorithm for reverb: adds
#' time-shifted copies of the signal weighted by a decay function, which is
#' analogous to convoluting the input with a parametric model of some
#' hypothetical impulse response function. In simple terms: we specify how much
#' and when the sound rebounds back (as from a wall) and add these time-shifted
#' copies to the original, optionally with some spectral filtering.
#' @inheritParams spectrogram
#' @inheritParams addAM
#' @param echoDelay the delay at which the echo appears, ms
#' @param echoLevel the rate at which the echo weakens at each repetition, dB
#' @param reverbDelay the time of maximum reverb density, ms
#' @param reverbSpread standard deviation of reverb spread around time
#'   \code{reverbDelay}, ms
#' @param reverbLevel the maximum amplitude of reverb, dB below input
#' @param reverbDensity the number of echos or "voices" added
#' @param reverbType so far only "gaussian" has been implemented
#' @param filter (optional) a spectral filter to apply to the created reverb and
#'   echo (see \code{addFormants} for acceptable formats)
#' @param dynamicRange the precision with which the reverb and echo are
#'   calculated, dB
#' @param output "audio" = returns just the processed audio, "detailed" =
#'   returns a list with reverb window, the added reverb/echo, etc.
#' @export
#' @examples
#' s = soundgen()
#' s_rev = reverb(s, 16000)
#' # playme(s_rev)
#'
#' \dontrun{
#' # double echo, no reverb
#' s1 = reverb(s, samplingRate = 16000, reverbLevel = NULL,
#'             echoDelay = c(250, 800), echoLevel = c(-15, -25))
#' # playme(s1)
#' # spectrogram(s1, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # only reverb (indoors)
#' s2 = reverb(s, samplingRate = 16000, echoDelay = NULL,
#'             reverbDelay = 70, reverbSpread = 130,
#'             reverbLevel = -20, reverbDensity = 20)
#' # playme(s2)
#' # spectrogram(s2, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # reverb (caves)
#' s3 = reverb(s, samplingRate = 16000, echoDelay = NULL,
#'             reverbDelay = 600, reverbSpread = 1500,
#'             reverbLevel = -10, reverbDensity = 100)
#' # playme(s3)
#' # spectrogram(s3, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # both echo and reverb with high frequencies emphasized
#' s4 = reverb(s, samplingRate = 16000,
#'             echoDelay = 250, echoLevel = -20,
#'             reverbDelay = 70, reverbSpread = 120,
#'             reverbLevel = -25, reverbDensity = 50,
#'             filter = list(formants = NULL, lipRad = 3))
#' # playme(s4)
#' # spectrogram(s4, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # add reverb to a recording
#' s5 = reverb('~/Downloads/temp260/ut_fear_57-m-tone.wav',
#'             echoDelay = 850, echoLevel = -40)
#' # playme(s5, 44100)
#'
#' # add reverb to all files in a folder, save the result
#' reverb('~/Downloads/temp2', saveAudio = '~/Downloads/temp2/rvb')
#' }
reverb = function(x,
                  samplingRate = NULL,
                  echoDelay = 200,
                  echoLevel = -20,
                  reverbDelay = 70,
                  reverbSpread = 130,
                  reverbLevel = -25,
                  reverbDensity = 50,
                  reverbType = 'gaussian',
                  filter = list(),
                  dynamicRange = 80,
                  output = c('audio', 'detailed')[1],
                  play = FALSE,
                  reportEvery = NULL,
                  cores = 1,
                  saveAudio = NULL) {
  # match args
  myPars = c(as.list(environment()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    funToCall = '.reverb',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  if (output == 'audio') result = result$audio
  invisible(result)
}


#' Add reverb to a sound
#'
#' Internal soundgen function, see \code{\link{reverb}}.
#'
#' @inheritParams reverb
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.reverb = function(audio,
                   echoDelay = 200,
                   echoLevel = -20,
                   reverbDelay = 70,
                   reverbSpread = 130,
                   reverbLevel = -25,
                   reverbDensity = 50,
                   reverbType = 'gaussian',
                   filter = list(),
                   dynamicRange = 80,
                   output = c('audio', 'detailed')[1],
                   play = FALSE) {
  win = rvb = echo = effect = numeric(0)
  ## reverb
  addRvb = FALSE
  if (is.numeric(reverbDelay) & is.numeric(reverbLevel)) {
    addRvb = TRUE
    if (reverbType == 'gaussian') {
      # Gaussian over 3 SD
      rvb_len_ms = reverbDelay + 3 * reverbSpread
      nFr_rvb = round(rvb_len_ms * audio$samplingRate / 1000)
      win_mean = reverbDelay * audio$samplingRate / 1000
      win_sd = reverbSpread * audio$samplingRate / 1000
      win = dnorm(seq_len(nFr_rvb), mean = win_mean, sd = win_sd)
      max_win = dnorm(win_mean, win_mean, win_sd)  # density at mean
      # win = win / max(win) * dynamicRange - dynamicRange + reverbLevel
      win = win / max_win * 120 - 120 + reverbLevel
      # 120 dB is used to set up the slope of decay, otherwise it would depend
      # on dynamicRange
      # "discretize" the win - only keep a few delay points
      idx_keep = sort(sample(seq_len(nFr_rvb), size = min(nFr_rvb, reverbDensity)))
      # only keep as much of win as exceeds dynamicRange (don't bother to add
      # very quiet reverb)
      idx_keep = idx_keep[win[idx_keep] > (-dynamicRange)]
      win = win[seq_len(.subset2(idx_keep, length(idx_keep)))]
      if (length(idx_keep) == 0) {
        # nothing to do
        addRvb = FALSE
      } else {
        nFr_rvb = tail(idx_keep, 1)
        win[idx_keep] = 10 ^ (win[idx_keep] / 20)
        win[-idx_keep] = 0
      }
      # plot(win, type = 'l')
    } else {
      stop("Only reverbType = 'gaussian' has been implemented so far")
      # exponential decay: halves (-6 dB) every reverbDelay
      n_halves = min(4, round((dynamicRange + reverbLevel) / 6))
      rvb_len_ms = n_halves * reverbDelay
      nFr_rvb = ceiling(rvb_len_ms * audio$samplingRate / 1000)
      len_halflife = ceiling(reverbDelay * audio$samplingRate / 1000)
      reverbLevel_lin = 10 ^ (reverbLevel / 20)
      win = 2 ^ (-(seq_len(nFr_rvb)) / len_halflife) * reverbLevel_lin
      # plot(win, type = 'l')

      # add some noise
      if (FALSE) {
        # rvb_wiggle = getRandomWalk(len = nFr_rvb,
        #   rw_range = reverbSpread * reverbLevel_lin * 2) - reverbSpread * reverbLevel_lin
        lw = rvb_len_ms / 2  # relatively smooth wiggle (1 value per 2 ms)
        # sd_w = seq(reverbSpread, reverbSpread / n_halves, length.out = lw) * win[1]
        rvb_wiggle = rnorm(lw, mean = 0, sd = sd_w)
        rvb_wiggle = approx(rvb_wiggle, n = nFr_rvb)$y
        # plot(rvb_wiggle, type = 'l')
        win = win + rvb_wiggle
        # plot(win, type = 'l')
      }
    }
  }
  # create reverb by adding up time-shifted signal weighted by decay window
  if (addRvb) {
    rvb = rep(0, audio$ls + nFr_rvb - 1)
    for (i in idx_keep) {
      idx_i = i:(i + audio$ls - 1)
      rvb[idx_i] = rvb[idx_i] + audio$sound * win[i]
    }
    # playme(rvb)
    # spectrogram(rvb, 16000, ylim = c(0, 4), osc = TRUE)
    # rvb = rvb_pad[1:len]  # range(rvb)
  } else {
    rvb = 0
  }

  ## echo
  if (all(is.numeric(echoLevel)) &&
      any(echoLevel > (-dynamicRange)) &&
      any(echoDelay > 0)) {
    le = length(echoDelay)
    if (le > 1 & length(echoLevel) == 1) echoLevel = rep(echoLevel, le)
    echo = NULL
    for (e in seq_len(le)) {
      nFr_echo = dynamicRange / (-echoLevel[e])
      step_echo = ceiling(echoDelay[e] * audio$samplingRate / 1000)
      echo_e = rep(0, audio$ls + nFr_echo * step_echo - 1)
      for (i in seq_len(nFr_echo)) {
        idx_start = i * step_echo
        idx_i = idx_start:(idx_start + audio$ls - 1)
        echo_e[idx_i] = echo_e[idx_i] + audio$sound * 10 ^ (echoLevel[e] * i / 20)
      }
      if (is.null(echo)) {
        echo = echo_e
      } else {
        echo = addVectors(echo, echo_e, normalize = FALSE)
      }
    }
    # playme(echo)
  } else {
    echo = 0
  }

  effect = addVectors(rvb, echo, normalize = FALSE)
  if (length(filter) > 1) {
    scale = max(effect)
    rvb = do.call(.addFormants, c(list(
      audio = list(
        sound = effect,
        samplingRate = audio$samplingRate
      ),
      normalize = 'orig'),
      filter)
    ) * scale
    # playme(effect, audio$samplingRate)
    # spectrogram(effect, audio$samplingRate, ylim = c(0, 4), osc = TRUE)
  }

  out = addVectors(audio$sound, effect) * audio$scale_used
  # playme(out, audio$samplingRate)
  # spectrogram(out, audio$samplingRate, ylim = c(0, 4), osc = TRUE)

  if (play) playme(out, audio$samplingRate)
  if (is.character(audio$saveAudio)) {
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(out, audio = audio, filename = filename)
  }

  list(
    rvb_win = win,
    rvb = rvb,
    echo = echo,
    effect = effect,
    audio = out
  )
}

